(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

module T = struct
  [@@@coverage off]

  type t =
    | Dry_run
    | Check
    | Force_yes
    | Interactive
  [@@deriving compare, equal, sexp_of]
end

include T

let default = Interactive

let arg =
  let open Command.Std in
  let running_mode
        ((switch :: _ : _ Command.Nonempty_list.t) as switches)
        ~(running_mode : t)
        ~doc
    =
    Arg.flag switches ~doc
    |> Arg.map ~f:(function
      | false -> None
      | true -> Some (switch, running_mode))
  in
  let+ dry_run =
    running_mode
      [ "dry-run" ]
      ~running_mode:Dry_run
      ~doc:"Print what linting actions would be done, but do not actually perform them."
  and+ interactive =
    running_mode
      [ "interactive"; "i" ]
      ~running_mode:Interactive
      ~doc:
        "Ask confirmation before applying each diff. This is the default when stdout is \
         a tty."
  and+ yes =
    running_mode
      [ "yes" ]
      ~running_mode:Force_yes
      ~doc:
        "Disable interactivity when stdout is a tty. Do not ask for confirmation and \
         apply all diffs. This is the default when stdout is $(b,not) a tty."
  and+ check =
    running_mode
      [ "check" ]
      ~running_mode:Check
      ~doc:
        "Print what linting actions would be done, but do not actually perform them. \
         Exit with a non-zero exit code in case some linting changes are required. This \
         execution mode is meant for scripts and CI pipelines."
  in
  let running_mode : t =
    match List.filter_opt [ dry_run; interactive; yes; check ] with
    | [] -> if Unix.isatty Unix.stdout then Interactive [@coverage off] else Force_yes
    | [ (_, mode) ] -> mode
    | _ :: _ :: _ as conflicts ->
      Err.raise
        ~exit_code:Err.Exit_code.cli_error
        Pp.O.
          [ Pp.text "Conflicting flags "
            ++ Pp.concat_map ~sep:(Pp.text ", ") conflicts ~f:(fun (flag, _) ->
              Pp_tty.kwd (module String) flag)
            ++ Pp.text ". Please choose one."
          ] [@coverage off]
    (* [@coverage off] is due to out edge instrumentation, but this
       case is exercised during the tests. *)
  in
  running_mode
;;
