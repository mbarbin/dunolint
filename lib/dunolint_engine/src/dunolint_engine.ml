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
module Unix = UnixLabels

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

type t =
  { config : Config.t
  ; edited_files : Edited_file.t Hashtbl.M(Relative_path).t
  }

let create ~config = { config; edited_files = Hashtbl.create (module Relative_path) }

module File_kind = struct
  type t = Unix.file_kind =
    | S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK

  let to_string t =
    match[@coverage off] t with
    | S_REG -> "Regular file"
    | S_DIR -> "Directory"
    | S_CHR -> "Character device"
    | S_BLK -> "Block device"
    | S_LNK -> "Symbolic link"
    | S_FIFO -> "Named pipe"
    | S_SOCK -> "Socket"
  ;;
end

let lint_file ?autoformat_file ?create_file ?rewrite_file t ~path =
  Log.info ~src (fun () ->
    Pp.O.[ Pp.text "Linting file " ++ Pp_tty.path (module Relative_path) path ]);
  let edited_file = Hashtbl.find t.edited_files path in
  let file_exists =
    match (Unix.stat (Relative_path.to_string path)).st_kind with
    | exception Unix.Unix_error (ENOENT, _, _) -> false
    | S_REG -> true
    | (S_DIR | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK) as file_kind ->
      Err.raise
        Pp.O.
          [ Pp.text "Linted file "
            ++ Pp_tty.path (module Relative_path) path
            ++ Pp.text " is expected to be a regular file."
          ; Pp.text "Actual file kind is "
            ++ Pp_tty.id (module File_kind) file_kind
            ++ Pp.text "."
          ]
  in
  match
    if file_exists || Option.is_some edited_file
    then (
      let previous_contents =
        match edited_file with
        | Some edited_file -> edited_file.new_contents
        | None -> In_channel.read_all (path |> Relative_path.to_string)
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

module Process_status = struct
  type t = Unix.process_status =
    | WEXITED of int
    | WSIGNALED of int
    | WSTOPPED of int
  [@@deriving sexp_of]
end

let format_dune_file_internal ~new_contents =
  let ((in_ch, out_ch, err_ch) as process) =
    Unix.open_process_full "dune format-dune-file" ~env:(Unix.environment ())
  in
  Out_channel.output_string out_ch new_contents;
  Out_channel.close out_ch;
  let output = In_channel.input_all in_ch in
  let err_output = In_channel.input_all err_ch in
  match Unix.close_process_full process with
  | WEXITED 0 -> Ok output
  | (WEXITED _ | WSIGNALED _ | WSTOPPED _) as process_status ->
    Error
      [ Pp.text "Failed to format dune file:"
      ; Pp.text
          (if Err.am_running_test ()
           then "<REDACTED IN TEST>"
           else String.strip err_output [@coverage off])
      ; Err.pp_of_sexp (Process_status.sexp_of_t process_status)
      ]
;;

let format_dune_file ~new_contents =
  match format_dune_file_internal ~new_contents with
  | Ok output -> output
  | Error text -> Err.raise text
;;

let format_dune_file_or_continue ~loc ~new_contents =
  match format_dune_file_internal ~new_contents with
  | Ok output -> output
  | Error text ->
    Err.error ~loc text;
    new_contents
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
    ~autoformat_file:(fun ~new_contents ->
      format_dune_file_or_continue
        ~loc:(Loc.of_file ~path:(path :> Fpath.t))
        ~new_contents)
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
    ~autoformat_file:(fun ~new_contents ->
      format_dune_file_or_continue
        ~loc:(Loc.of_file ~path:(path :> Fpath.t))
        ~new_contents)
;;

let rec mkdirs path =
  match (Unix.stat (Relative_path.to_string path)).st_kind with
  | exception Unix.Unix_error (ENOENT, _, _) ->
    (match Relative_path.parent path with
     | None -> ()
     | Some path -> mkdirs path);
    Unix.mkdir (Relative_path.to_string path) ~perm:0o755
  | S_DIR -> ()
  | (S_REG | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK) as file_kind ->
    Err.error
      Pp.O.
        [ Pp.text "Parent path "
          ++ Pp_tty.path (module Relative_path) path
          ++ Pp.text " is expected to be a directory."
        ; Pp.text "Actual file kind is "
          ++ Pp_tty.id (module File_kind) file_kind
          ++ Pp.text "."
        ]
;;

let materialize t =
  let running_mode = Config.running_mode t.config in
  let edited_files =
    Hashtbl.to_alist t.edited_files
    |> List.sort ~compare:(fun (p1, _) (p2, _) -> Relative_path.compare p1 p2)
    |> List.map ~f:snd
  in
  let exception Quit in
  try
    List.iteri edited_files ~f:(fun i { path; original_contents; new_contents } ->
      if i > 0 then print_endline "";
      let should_mkdir =
        match Relative_path.parent path with
        | None -> None
        | Some parent_dir as some ->
          (match (Unix.stat (Relative_path.to_string parent_dir)).st_kind with
           | exception Unix.Unix_error (ENOENT, _, _) -> some
           | S_DIR -> None
           | (S_REG | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK) as file_kind ->
             Err.error
               Pp.O.
                 [ Pp.text "Parent path "
                   ++ Pp_tty.path (module Relative_path) parent_dir
                   ++ Pp.text " is expected to be a directory."
                 ; Pp.text "Actual file kind is "
                   ++ Pp_tty.id (module File_kind) file_kind
                   ++ Pp.text "."
                 ];
             None)
      in
      let with_flow flow =
        Option.iter should_mkdir ~f:(fun parent_dir ->
          Out_channel.fprintf
            flow
            "%s `mkdir -p %s`\n"
            (match running_mode with
             | Dry_run -> "dry-run: Would run"
             | Check -> "check: Would run"
             | Interactive -> "Would run"
             | Force_yes -> "Running")
            (Relative_path.to_string parent_dir));
        Out_channel.fprintf
          flow
          "%s file %S:\n"
          (match running_mode with
           | Dry_run -> "dry-run: Would edit"
           | Check -> "check: Would edit"
           | Interactive -> "Would edit"
           | Force_yes -> "Editing")
          (Relative_path.to_string path);
        Out_channel.output_line
          flow
          (if Err.am_running_test ()
           then Expect_test_patdiff.patdiff original_contents new_contents ~context:3
           else (
             let name = Relative_path.to_string path in
             Patdiff.Patdiff_core.patdiff
               ~context:6
               ~prev:{ name; text = original_contents }
               ~next:{ name; text = new_contents }
               ()));
        Out_channel.flush flow
      in
      let () =
        match running_mode with
        | Dry_run | Check | Force_yes -> with_flow stdout
        | Interactive ->
          Git_pager.run ~f:(fun pager -> with_flow (Git_pager.write_end pager))
      in
      let do_it =
        match running_mode with
        | Dry_run | Check -> false
        | Force_yes -> true
        | Interactive ->
          print_endline "";
          (match
             Prompt.ask
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
        Option.iter should_mkdir ~f:mkdirs;
        Out_channel.write_all (Relative_path.to_string path) ~data:new_contents))
  with
  | Quit -> ()
;;

module Visitor_decision = struct
  type t =
    | Break
    | Continue
    | Skip_subtree
end

let visit ?below (_ : t) ~f =
  let root_path =
    match below with
    | None -> Relative_path.empty
    | Some below -> Relative_path.to_dir_path below
  in
  let rec visit = function
    | [] -> ()
    | [] :: tl -> visit tl
    | (parent_dir :: tl) :: rest ->
      let entries =
        Stdlib.Sys.readdir (Relative_path.to_string parent_dir)
        |> Array.to_list
        |> List.sort ~compare:String.compare
      in
      let subdirectories, files, _ =
        entries
        |> List.partition3_map ~f:(fun entry ->
          match
            (Unix.lstat
               (Stdlib.Filename.concat (Relative_path.to_string parent_dir) entry))
              .st_kind
          with
          | S_DIR -> `Fst entry
          | S_REG -> `Snd entry
          | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK -> `Trd ()
          | exception Unix.Unix_error (EACCES, _, _) ->
            Err.warning
              Pp.O.
                [ Pp.text "Permission denied - skipping "
                  ++ Pp_tty.path (module Relative_path) parent_dir
                  ++ Pp.text "."
                ];
            `Trd ())
      in
      Log.debug ~src (fun () ->
        Pp.O.
          [ Pp.text "Visiting directory " ++ Pp_tty.path (module Relative_path) parent_dir
          ]);
      (match (f ~parent_dir ~subdirectories ~files : Visitor_decision.t) with
       | Break -> ()
       | Continue ->
         visit
           (List.map subdirectories ~f:(fun subdirectory ->
              Relative_path.extend parent_dir (Fsegment.v subdirectory)
              |> Relative_path.to_dir_path)
            :: tl
            :: rest)
       | Skip_subtree ->
         Log.info ~src (fun () ->
           Pp.O.
             [ Pp.text "Skipping children of directory "
               ++ Pp_tty.path (module Relative_path) parent_dir
             ]);
         visit (tl :: rest))
  in
  visit [ [ root_path ] ]
;;

let run ~config f =
  let t = create ~config in
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

module Private = struct
  let mkdirs = mkdirs
end
