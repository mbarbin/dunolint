(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let sexpable_param (type a) (module M : Sexpable.S with type t = a) =
  let module Validate = struct
    type t = a

    let of_string str =
      try Ok (Parsexp.Single.parse_string_exn str |> M.t_of_sexp) with
      | exn -> Error (`Msg (Exn.to_string exn))
    ;;

    let to_string t =
      (* This would be used to print default value, currently not exercised. *)
      Sexp.to_string_mach (M.sexp_of_t t) [@coverage off]
    ;;
  end
  in
  Command.Param.validated_string (module Validate)
;;

let below ~doc =
  let open Command.Std in
  Arg.named_opt [ "below" ] (Param.validated_string (module Fpath)) ~docv:"PATH" ~doc
;;

let skip_subtrees ~globs =
  List.concat
    [ List.map globs ~f:Dunolint.Glob.v
    ; List.concat_map
        ~f:(fun pat -> [ Dunolint.Glob.v ("**/" ^ pat); Dunolint.Glob.v pat ])
        [ ".git/"
        ; ".hg/"
        ; "_build/"
        ; "_opam/"
        ; "_coverage/"
        ; "node_modules/"
        ; "doc/build/"
        ; ".docusaurus/"
        ; "*.t/"
        ]
    ]
;;

let default_skip_paths_config () =
  Dunolint.Config.V1.create [ `skip_paths (skip_subtrees ~globs:[]) ]
  |> Dunolint.Config.v1
;;

let enforce_rules_config ~rules =
  match rules with
  | [] -> None
  | _ :: _ ->
    Some
      (Dunolint.Config.V1.create (List.map rules ~f:(fun rule -> `rule rule))
       |> Dunolint.Config.v1)
;;

let resolve_root_path path =
  let cwd = Unix.getcwd () |> Absolute_path.v in
  Absolute_path.relativize ~root:cwd path
;;

let root =
  let open Command.Std in
  let+ root =
    Arg.named_opt
      [ "root" ]
      (Param.validated_string (module Fpath))
      ~docv:"DIR"
      ~doc:
        "Use this directory as dune workspace root instead of guessing it. Takes \
         precedence over the $(b,DUNE_ROOT) environment variable."
  in
  match root with
  | Some root -> Some (resolve_root_path root)
  | None ->
    (* Fall back to environment variable if flag is not provided. *)
    (match Sys.getenv "DUNE_ROOT" with
     | None -> None
     | Some dune_root ->
       (match Fpath.of_string dune_root with
        | Ok path -> Some (resolve_root_path path)
        | Error (`Msg msg) ->
          Err.raise
            ~exit_code:Err.Exit_code.cli_error
            Pp.O.
              [ Pp.text "Invalid value for "
                ++ Pp_tty.kwd (module String) "DUNE_ROOT"
                ++ Pp.text " environment variable."
              ; Pp.text msg
              ]))
;;

let relativize ~workspace_root ~cwd ~path =
  let path = Absolute_path.relativize ~root:cwd path in
  match
    Absolute_path.chop_prefix path ~prefix:(workspace_root |> Workspace_root.path)
  with
  | Some relative_path -> relative_path
  | None ->
    Err.raise
      Pp.O.
        [ Pp.text "Path "
          ++ Pp_tty.path (module Absolute_path) path
          ++ Pp.text " is not in dune workspace."
        ]
;;
