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

open! Import
module Unix = UnixLabels

module Save_in_place = struct
  type t =
    { file : Fpath.t
    ; perm : int
    }

  let invalid_file_kind ~file ~file_kind =
    Err.raise
      Pp.O.
        [ Pp.text "Linted file "
          ++ Pp_tty.path (module Fpath) file
          ++ Pp.text " is expected to be a regular file."
        ; Pp.text "Actual file kind is "
          ++ Pp_tty.id (module Dunolint_engine.File_kind) file_kind
          ++ Pp.text "."
        ]
  ;;

  let of_file ~file =
    match Unix.lstat (Fpath.to_string file) with
    | exception Unix.Unix_error (ENOENT, _, _) ->
      Err.raise
        Pp.O.[ Pp.text "No such file " ++ Pp_tty.path (module Fpath) file ++ Pp.text "." ]
    | stat ->
      (match stat.st_kind with
       | S_REG -> { file; perm = stat.st_perm }
       | (S_DIR | S_LNK) as file_kind -> invalid_file_kind ~file ~file_kind
       | (S_CHR | S_BLK | S_FIFO | S_SOCK) as file_kind ->
         invalid_file_kind ~file ~file_kind [@coverage off])
  ;;
end

let lint_file
      (module File_linter : Dunolinter.S)
      ~format_file
      ~context
      ~path
      ~original_contents
  =
  match File_linter.create ~path ~original_contents with
  | Error { loc; message } -> Err.raise ~loc [ Pp.textf "%s" message ]
  | Ok linter ->
    File_linter.visit linter ~f:(fun stanza -> Linter.lint_stanza ~path ~context ~stanza);
    let new_contents = File_linter.contents linter in
    if format_file
    then (
      match Linter.enclosing_dune_lang_version ~context ~path with
      | None -> new_contents
      | Some dune_lang_version ->
        Dunolint_engine.format_dune_file ~dune_lang_version ~new_contents)
    else new_contents
;;

let in_place_switch = "in-place"
let filename_switch = "filename"
let pp_tty_switch switch = Pp_tty.kwd (module String) ("--" ^ switch)

let select_linter ~path =
  let filename = Fpath.filename path in
  match Dunolint.Linted_file_kind.of_string filename with
  | Ok linted_file_kind ->
    (match linted_file_kind with
     | `dune -> (module Dune_linter : Dunolinter.S)
     | `dune_project -> (module Dune_project_linter : Dunolinter.S)
     | `dune_workspace -> (module Dune_workspace_linter : Dunolinter.S)
     | `dunolint -> (module Dunolint_linter : Dunolinter.S))
  | Error (`Msg _msg) ->
    Err.raise
      Pp.O.
        [ Pp.text "Cannot infer the file kind from the filename "
          ++ Pp_tty.path (module String) filename
          ++ Pp.verbatim "."
        ]
      ~hints:
        Pp.O.
          [ Pp.text "You may override the filename with the flag "
            ++ pp_tty_switch filename_switch
            ++ Pp.verbatim "."
          ]
;;

module Context = struct
  type t =
    | Autoloading of { engine : Dunolint_engine.t }
    | Root_context of { context : Dunolint_engine.Context.t }
end

module File_with_filename = struct
  (* Associates each file to lint with its logical filename for config discovery.

     This abstraction separates the validation of [--filename] compatibility
     from the iteration logic. When a single file is supplied, the user-provided
     [--filename] (if any) applies to it. When multiple files are supplied, each
     file uses its own path as filename (and [--filename] is rejected earlier).

     Without this intermediate representation, the iteration code would appear
     to use a shared [filename] variable for all files, making the intent less
     clear even though runtime checks would prevent misuse. *)
  type t =
    { file : Fpath.t
    ; filename : Fpath.t option
    }
end

let assert_visit () =
  (* This ensure the coverage for the branch is tested even though the raising
     expression is disabled. This is a work around for a bisect_ppx issue. *)
  ()
;;

let main =
  Command.make
    ~summary:"Lint file(s)."
    ~readme:(fun () ->
      "This command is meant to ease the integration with editors in workflows that \
       enable linting on save.\n\n\
       This will read the contents of a build file either from $(b,disk) or from \
       $(b,stdin) and print its linted result on $(b,stdout).\n\n\
       By default, the contents will be read from $(b,stdin). You may supply the path to \
       a file instead.\n\n\
       When using $(b,--in-place), multiple files may be supplied. In this mode, each \
       file is linted and saved back to disk (only if changed). This is useful for \
       pre-commit hooks that process multiple files in a single invocation.\n\n\
       Dunolint will locate the workspace root by searching for $(b,dune-workspace) or \
       $(b,dune-project) files in the current directory and its ancestors. If no \
       workspace is found, the current working directory is used as the default \
       workspace root (useful for editor integration with standalone files). Once the \
       workspace root is determined, dunolint will auto-discover and load $(b,dunolint) \
       config files from parent directories up to that root. The workspace root can be \
       overridden using the $(b,--root) flag.\n\n\
       When reading from stdin, the $(b,--filename) flag should be used to specify the \
       logical path of the file being linted. This path is used to: (1) infer the file \
       kind (e.g. dune vs dune-project), (2) discover which config files to load based \
       on the file's location in the directory hierarchy, and (3) evaluate skip_paths \
       rules. Note that $(b,--filename) cannot be used when multiple files are supplied.")
    (let open Command.Std in
     let+ () = Log_cli.set_config ()
     and+ files =
       Arg.pos_all
         (Param.validated_string (module Fpath))
         ~docv:"FILE"
         ~doc:
           "Path to file(s) to lint. By default reads from stdin. Multiple files may \
            only be supplied when using $(b,--in-place)."
     and+ filename =
       Arg.named_opt
         [ "filename" ]
         (Param.validated_string (module Fpath))
         ~docv:"path/to/file"
         ~doc:
           "Logical path of the file being linted. Used to infer the file kind from its \
            basename, discover config files from parent directories, and evaluate \
            skip_paths rules. This flag is particularly useful when reading from \
            $(b,stdin) to specify where the file logically resides in the project \
            hierarchy."
     and+ in_place =
       Arg.flag
         [ in_place_switch ]
         ~doc:
           "When the input is a regular file, you may use this option to save the linted \
            result in the file directly, instead of printing it to $(b,stdout). \
            Supplying this flag results in failure when the input is read from \
            $(b,stdin)."
     and+ config =
       Arg.named_opt
         [ "config" ]
         Param.file
         ~doc:
           "Path to dunolint config file. When specified, disables auto-discovery of \
            config files from parent directories."
     and+ format_file =
       Arg.named_with_default
         [ "format-file" ]
         Param.bool
         ~default:true
         ~doc:"Format file with after linting, using [dune format-dune-file]."
     and+ root = Common_helpers.root in
     let cwd = Unix.getcwd () |> Absolute_path.v in
     let workspace_root =
       Workspace_root.find_exn ~default_is_cwd:true ~specified_by_user:root
     in
     let relativize path = Common_helpers.relativize ~workspace_root ~cwd ~path in
     let config =
       Option.map config ~f:(fun config ->
         relativize (Fpath.v config) |> Relative_path.to_string)
     in
     Workspace_root.chdir workspace_root ~level:Debug;
     let autoload_config = Option.is_none config in
     let root_configs =
       List.concat
         [ [ Common_helpers.default_skip_paths_config () ]
         ; (match config with
            | None -> []
            | Some config_path ->
              [ Dunolinter.Config_handler.load_config_exn ~filename:config_path ])
         ]
     in
     let context : Context.t =
       if autoload_config
       then (
         (* [root_configs] are added to discovered configs by [build_context]. *)
         let engine = Dunolint_engine.create ~root_configs ~running_mode:Dry_run () in
         Autoloading { engine })
       else (
         (* When autoload is disabled, we still need the [root_configs]. Since
            [build_context] won't be called, manually create the context. *)
         let context =
           List.fold
             root_configs
             ~init:Dunolint_engine.Context.empty
             ~f:(fun context config ->
               Dunolint_engine.Context.add_config
                 context
                 ~config
                 ~location:Relative_path.empty)
         in
         Root_context { context })
     in
     let build_context ~path =
       match context with
       | Root_context { context } -> context
       | Autoloading { engine } -> Dunolint_engine.build_context engine ~path
     in
     let do_lint ~path ~original_contents =
       let context = build_context ~path in
       let linter = select_linter ~path:(path :> Fpath.t) in
       if Linter.should_skip_subtree ~context ~path
       then original_contents
       else lint_file linter ~format_file ~context ~path ~original_contents
     in
     if in_place
     then (
       (* --in-place mode: lint files and save back to disk. *)
       let files_with_filenames =
         match files with
         | [ file ] -> [ { File_with_filename.file; filename } ]
         | _ :: _ :: _ as files ->
           let () =
             match filename with
             | None -> ()
             | Some _ ->
               let () = assert_visit () in
               (Err.raise
                  ~exit_code:Err.Exit_code.cli_error
                  Pp.O.
                    [ Pp.text "When supplying multiple files, "
                      ++ pp_tty_switch filename_switch
                      ++ Pp.text " cannot be used."
                    ]
                (* Exercised in tests but invisible due to out-edge bisect_ppx issue. *)
                [@coverage off])
           in
           List.map files ~f:(fun file -> { File_with_filename.file; filename = None })
         | [] ->
           let () = assert_visit () in
           (Err.raise
              ~exit_code:Err.Exit_code.cli_error
              Pp.O.
                [ Pp.text "When using "
                  ++ pp_tty_switch in_place_switch
                  ++ Pp.text ", at least one file must be specified."
                ]
            (* Exercised in tests but invisible due to out-edge bisect_ppx issue. *)
            [@coverage off])
       in
       List.iter files_with_filenames ~f:(fun { file; filename } ->
         let file = relativize file in
         let { Save_in_place.file = file_for_save; perm } =
           Save_in_place.of_file ~file:(file :> Fpath.t)
         in
         let path =
           match filename with
           | Some filename -> relativize filename
           | None -> file
         in
         let original_contents = In_channel.read_all (file |> Relative_path.to_string) in
         let output = do_lint ~path ~original_contents in
         if not (String.equal original_contents output)
         then
           Out_channel.with_file ~perm (Fpath.to_string file_for_save) ~f:(fun oc ->
             Out_channel.output_string oc output)))
     else (
       (* Output to stdout mode: read from file or stdin, print result. *)
       let path, original_contents =
         match files with
         | [] ->
           let path =
             match filename with
             | Some filename -> relativize filename
             | None -> Relative_path.v "stdin"
           in
           path, In_channel.input_all In_channel.stdin
         | [ file ] ->
           let file = relativize file in
           let path =
             match filename with
             | Some filename -> relativize filename
             | None -> file
           in
           path, In_channel.read_all (file |> Relative_path.to_string)
         | _ :: _ :: _ ->
           let () = assert_visit () in
           (Err.raise
              ~exit_code:Err.Exit_code.cli_error
              Pp.O.
                [ Pp.text "When supplying multiple files, "
                  ++ pp_tty_switch in_place_switch
                  ++ Pp.text " must be used."
                ]
            (* Exercised in tests but invisible due to out-edge bisect_ppx issue. *)
            [@coverage off])
       in
       let output = do_lint ~path ~original_contents in
       Out_channel.flush Out_channel.stdout;
       Out_channel.set_binary_mode Out_channel.stdout true;
       Out_channel.output_string Out_channel.stdout output;
       Out_channel.flush Out_channel.stdout;
       Out_channel.set_binary_mode Out_channel.stdout false))
;;
