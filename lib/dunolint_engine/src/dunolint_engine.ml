(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*                                                                               *)
(*  This file is part of Dunolint.                                               *)
(*                                                                               *)
(*  Dunolint is free software; you can redistribute it and/or modify it          *)
(*  under the terms of the GNU Lesser General Public License as published by     *)
(*  the Free Software Foundation either version 3 of the License, or any later   *)
(*  version, with the LGPL-3.0 Linking Exception.                                *)
(*                                                                               *)
(*  Dunolint is distributed in the hope that it will be useful, but WITHOUT      *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*                                                                               *)
(*  You should have received a copy of the GNU Lesser General Public License     *)
(*  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
(*********************************************************************************)

module Git_pager = Dunolint_vendor_git_pager.Git_pager
module Prompt = Dunolint_vendor_prompt.Prompt

let src = Logs.Src.create "dunolint" ~doc:"dunolint"

module Config = Config

module Edited_file = struct
  (* Edited files are indexed by their path relative to the root_path provided
     during the creation of the engine [t]. *)
  type t =
    { path : Relative_path.t
    ; original_contents : string (* Empty if the file is new *)
    ; mutable new_contents : string
    }
end

type 'a env = 'a
  constraint
    'a =
    < process_mgr : _ Eio.Process.mgr
    ; fs : _ Eio.Path.t
    ; cwd : _ Eio.Path.t
    ; stdin : _ Eio.Flow.source
    ; stdout : _ Eio.Flow.sink
    ; .. >
    as
    'a

type env_packed = Env : 'a env -> env_packed [@@unboxed]

type t =
  { config : Config.t
  ; edited_files : Edited_file.t Hashtbl.M(Relative_path).t
  ; root_path : Eio.Fs.dir_ty Eio.Path.t
  ; process_mgr : [ `Generic ] Eio.Process.mgr_ty Eio.Process.mgr
  ; stdout : Eio.Flow.sink_ty Eio.Flow.sink
  ; env : env_packed
  }

let create ~env ~config =
  { config
  ; edited_files = Hashtbl.create (module Relative_path)
  ; root_path = (Eio.Stdenv.fs env :> Eio.Fs.dir_ty Eio.Path.t)
  ; process_mgr =
      (Eio.Stdenv.process_mgr env :> [ `Generic ] Eio.Process.mgr_ty Eio.Process.mgr)
  ; stdout = (Eio.Stdenv.stdout env :> Eio.Flow.sink_ty Eio.Flow.sink)
  ; env = Env env
  }
;;

let lint_file ?autoformat_file ?create_file ?rewrite_file t ~path =
  let eio_path = Eio.Path.(t.root_path / (path |> Relative_path.to_string)) in
  Pp_log.info ~src (fun () ->
    Pp.O.[ Pp.text "Linting file " ++ Pp_tty.path (module Relative_path) path ]);
  let edited_file = Hashtbl.find t.edited_files path in
  let file_exists = Eio.Path.is_file eio_path in
  match
    if file_exists || Option.is_some edited_file
    then (
      let previous_contents =
        match edited_file with
        | Some edited_file -> edited_file.new_contents
        | None -> Eio.Path.load eio_path
      in
      let new_contents =
        match rewrite_file with
        | None -> previous_contents
        | Some rewrite_file -> rewrite_file ~previous_contents
      in
      Some (previous_contents, new_contents))
    else (
      match create_file with
      | Some create_file ->
        let new_contents = create_file () in
        Some ("", new_contents)
      | None -> None)
  with
  | None -> ()
  | Some (previous_contents, new_contents) ->
    let new_contents =
      match autoformat_file with
      | None -> new_contents
      | Some fmt -> fmt ~new_contents
    in
    if (not file_exists) || not (String.equal previous_contents new_contents)
    then (
      match edited_file with
      | Some edited_file -> edited_file.new_contents <- new_contents
      | None ->
        Hashtbl.set
          t.edited_files
          ~key:path
          ~data:{ Edited_file.path; original_contents = previous_contents; new_contents })
;;

let format_dune_file t ~new_contents =
  Eio.Process.parse_out
    t.process_mgr
    Eio.Buf_read.take_all
    ~stdin:(Eio.Flow.string_source new_contents)
    [ "dune"; "format-dune-file" ]
;;

let lint_dune_file ?with_linter t ~(path : Relative_path.t) ~f =
  lint_file
    t
    ~path
    ?create_file:None
    ~rewrite_file:(fun ~previous_contents ->
      match Dune_linter.create ~path ~original_contents:previous_contents with
      | Error { loc; message } ->
        Err.error ~loc [ Pp.textf "%s" message ];
        previous_contents
      | Ok linter ->
        Dune_linter.visit linter ~f;
        Option.iter with_linter ~f:(fun f -> f linter);
        Dune_linter.contents linter)
    ~autoformat_file:(fun ~new_contents -> format_dune_file t ~new_contents)
;;

let lint_dune_project_file ?with_linter t ~(path : Relative_path.t) ~f =
  lint_file
    t
    ~path
    ?create_file:None
    ~rewrite_file:(fun ~previous_contents ->
      match Dune_project_linter.create ~path ~original_contents:previous_contents with
      | Error { loc; message } ->
        Err.error ~loc [ Pp.textf "%s" message ];
        previous_contents
      | Ok linter ->
        Dune_project_linter.visit linter ~f;
        Option.iter with_linter ~f:(fun f -> f linter);
        Dune_project_linter.contents linter)
    ~autoformat_file:(fun ~new_contents -> format_dune_file t ~new_contents)
;;

let materialize t =
  let (Env env) = t.env in
  let running_mode = Config.running_mode t.config in
  let edited_files =
    Hashtbl.to_alist t.edited_files
    |> List.sort ~compare:(fun (p1, _) (p2, _) -> Relative_path.compare p1 p2)
    |> List.map ~f:snd
  in
  let exception Quit in
  try
    List.iteri edited_files ~f:(fun i { path; original_contents; new_contents } ->
      if i > 0 then Eio_writer.print_newline ~env;
      let path = path |> Relative_path.to_string in
      let eio_path = Eio.Path.(t.root_path / path) in
      let parent_dir =
        Eio.Path.split eio_path
        |> Option.map ~f:fst
        |> Option.filter ~f:(fun parent_dir -> not (String.is_empty (snd parent_dir)))
      in
      let with_flow flow =
        let delayed_mkdir =
          Option.bind parent_dir ~f:(fun parent_dir ->
            let parent_path = parent_dir |> snd |> Fpath.v in
            match Eio.Path.kind ~follow:true parent_dir with
            | `Symbolic_link -> assert false
            | `Directory -> None
            | `Not_found ->
              Eio_writer.writef
                flow
                "%s `mkdir -p %s`\n"
                (match running_mode with
                 | Dry_run -> "dry-run: Would run"
                 | Check -> "check: Would run"
                 | Interactive -> "Would run"
                 | Force_yes -> "Running")
                (parent_path |> Fpath.to_dir_path |> Fpath.to_string);
              Some (fun () -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o777 parent_dir)
            | ( `Unknown
              | `Fifo
              | `Character_special
              | `Block_device
              | `Regular_file
              | `Socket ) as unexpected_kind ->
              Err.error
                Pp.O.
                  [ Pp.text "Parent directory "
                    ++ Pp_tty.path (module String) (parent_dir |> snd)
                    ++ Pp.text " is not a directory: "
                    ++ Pp_tty.id
                         (module String)
                         (Stdlib.Format.asprintf
                            "%a"
                            Eio.File.Stat.pp_kind
                            unexpected_kind)
                  ];
              None)
        in
        Eio_writer.writef
          flow
          "%s file %S:\n"
          (match running_mode with
           | Dry_run -> "dry-run: Would edit"
           | Check -> "check: Would edit"
           | Interactive -> "Would edit"
           | Force_yes -> "Editing")
          path;
        Eio_writer.write_line
          flow
          (if Err.am_running_test ()
           then Expect_test_patdiff.patdiff original_contents new_contents ~context:3
           else
             Patdiff.Patdiff_core.patdiff
               ~context:6
               ~prev:{ name = path; text = original_contents }
               ~next:{ name = path; text = new_contents }
               ());
        delayed_mkdir
      in
      let delayed_mkdir =
        match running_mode with
        | Dry_run | Check | Force_yes -> Eio_writer.with_flow t.stdout with_flow
        | Interactive ->
          let mkdir = ref None in
          Git_pager.run
            ~env
            ~cwd:(Eio.Stdenv.cwd env :> Eio.Fs.dir_ty Eio.Path.t)
            ~f:(fun pager ->
              let flow = Git_pager.write_end pager in
              let res = Eio_writer.with_flow flow with_flow in
              mkdir := res);
          !mkdir
      in
      let do_it =
        match running_mode with
        | Dry_run | Check -> false
        | Force_yes -> true
        | Interactive ->
          Eio_writer.print_newline ~env;
          (match
             Prompt.ask
               ~env
               ~prompt:"Accept diff"
               ~choices:
                 [ Prompt.Choice.create
                     'N'
                     `No
                     ~help:"No - skip diff and go to the next one (default)."
                 ; Prompt.Choice.create 'y' `Yes ~help:"Yes - save diff and continue."
                 ; Prompt.Choice.create
                     'q'
                     `Quit
                     ~help:"Quit - do not accept diff and exit."
                 ]
           with
           | `Yes -> true
           | `No -> false
           | `Quit -> raise Quit)
      in
      if do_it
      then (
        Option.iter delayed_mkdir ~f:(fun f -> f ());
        Eio.Path.save ~create:(`Or_truncate 0o666) eio_path new_contents))
  with
  | Quit -> ()
;;

module Visitor_decision = struct
  type t =
    | Break
    | Continue
    | Skip_subtree
end

let visit ?below (t : t) ~f =
  let root_path =
    match below with
    | None -> t.root_path
    | Some below ->
      (match Relative_path.to_string below with
       | "./" -> t.root_path
       | path ->
         let path = String.chop_suffix path ~suffix:"/" |> Option.value ~default:path in
         Eio.Path.(t.root_path / path))
  in
  let rec visit = function
    | [] -> ()
    | [] :: tl -> visit tl
    | (parent_dir :: tl) :: rest ->
      let entries = Eio.Path.read_dir parent_dir in
      let subdirectories, files, _ =
        entries
        |> List.partition3_map ~f:(fun entry ->
          match Eio.Path.kind ~follow:false Eio.Path.(parent_dir / entry) with
          | `Directory -> `Fst entry
          | `Regular_file -> `Snd entry
          | `Not_found
          | `Unknown
          | `Fifo
          | `Character_special
          | `Block_device
          | `Symbolic_link
          | `Socket -> `Trd ())
      in
      let parent_dir_as_fpath =
        match parent_dir |> snd with
        | "" -> Relative_path.v "./"
        | path -> path |> Relative_path.v |> Relative_path.to_dir_path
      in
      Pp_log.debug ~src (fun () ->
        Pp.O.
          [ Pp.text "Visiting directory "
            ++ Pp_tty.path (module Relative_path) parent_dir_as_fpath
          ]);
      (match
         (f ~parent_dir:parent_dir_as_fpath ~subdirectories ~files : Visitor_decision.t)
       with
       | Break -> ()
       | Continue ->
         visit
           (List.map subdirectories ~f:(fun subdirectory ->
              Eio.Path.(parent_dir / subdirectory))
            :: tl
            :: rest)
       | Skip_subtree ->
         Pp_log.info ~src (fun () ->
           Pp.O.
             [ Pp.text "Skipping children of directory "
               ++ Pp_tty.path (module Relative_path) parent_dir_as_fpath
             ]);
         visit (tl :: rest))
  in
  visit [ [ root_path ] ]
;;

let run ~env ~config f =
  let t = create ~env ~config in
  let result = f t in
  materialize t;
  let () =
    match Config.running_mode config with
    | Dry_run | Force_yes | Interactive -> ()
    | Check ->
      if not (Hashtbl.is_empty t.edited_files)
      then
        Err.error
          [ Pp.text "Linting check failed: Exiting with unaddressed linting errors." ]
  in
  result
;;
