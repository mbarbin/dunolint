(*********************************************************************************)
(*  Git_pager - Show diffs in the terminal with the user's configured git pager  *)
(*  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*                                                                               *)
(*  This file is part of Git_pager.                                              *)
(*                                                                               *)
(*  Git_pager is free software; you can redistribute it and/or modify it         *)
(*  under the terms of the GNU Lesser General Public License as published by     *)
(*  the Free Software Foundation either version 3 of the License, or any later   *)
(*  version, with the LGPL-3.0 Linking Exception.                                *)
(*                                                                               *)
(*  Git_pager is distributed in the hope that it will be useful, but WITHOUT     *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*                                                                               *)
(*  You should have received a copy of the GNU Lesser General Public License     *)
(*  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
(*********************************************************************************)

type t =
  { output_kind : [ `Tty | `Pager | `Other ]
  ; color : [ `Auto | `Always | `Never ]
  ; write_end : Eio.Flow.sink_ty Eio.Flow.sink
  }

let should_force_color { output_kind; color; write_end = _ } =
  match output_kind, color with
  | `Pager, `Auto ->
    (* This is the only case in which Git on its own would make the wrong
       decision if we did nothing to help. If we ran the command in a terminal,
       Git would normally color it, but because we are sending its output to a
       pipe, the [auto] strategy is confused and Git disables the colors. In all
       other cases we return [false]. It doesn't mean we'll get no color, it
       just means we let Git decide on its own. *)
    true
  | `Pager, (`Always | `Never) | (`Other | `Tty), (`Always | `Auto | `Never) -> false
;;

let write_end t = t.write_end

let get_git_pager ~env ~cwd =
  match
    Eio_process.run_stdout
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd
      ~prog:"git"
      ~args:[ "var"; "GIT_PAGER" ]
      ()
  with
  | Ok output -> output |> String.strip
  | Error error ->
    Err.raise
      Pp.O.
        [ Pp.text "Failed to get the value of "
          ++ Pp_tty.kwd (module String) "GIT_PAGER"
          ++ Pp.text "."
        ; Err.pp_of_sexp (Error.sexp_of_t error)
        ]
;;

let get_git_color ~env ~cwd =
  match
    Eio_process.run_stdout
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd
      ~accept_nonzero_exit:[ 1 ]
      ~prog:"git"
      ~args:[ "config"; "--get"; "color.ui" ]
      ()
  with
  | Error error ->
    Err.raise
      Pp.O.
        [ Pp.text "Failed to get the value of "
          ++ Pp_tty.kwd (module String) "color.ui"
          ++ Pp.text "."
        ; Err.pp_of_sexp (Error.sexp_of_t error)
        ]
  | Ok output ->
    (match output |> String.strip with
     | "" | "auto" -> `Auto
     | "always" -> `Always
     | "never" -> `Never
     | other ->
       Err.raise
         Pp.O.
           [ Pp.text "Unexpected "
             ++ Pp_tty.kwd (module String) "git color.ui"
             ++ Pp.text " value "
             ++ Pp_tty.id (module String) other
             ++ Pp.text "."
           ])
;;

let run ~env ~cwd ~f =
  let git_pager = get_git_pager ~env ~cwd in
  let color = get_git_color ~env ~cwd in
  let stdout_isatty = Unix.isatty Unix.stdout in
  let pager_is_disabled = (not stdout_isatty) || String.equal git_pager "cat" in
  let output_kind =
    match pager_is_disabled, stdout_isatty with
    | true, true -> `Tty
    | true, false -> `Other
    | false, true -> `Pager
    | false, false -> assert false
  in
  match output_kind with
  | `Tty | `Other ->
    f
      { output_kind
      ; color
      ; write_end = (Eio.Stdenv.stdout env :> Eio.Flow.sink_ty Eio.Flow.sink)
      }
  | `Pager ->
    let process_env =
      let env = Unix.environment () in
      if Array.exists env ~f:(fun s -> String.is_prefix s ~prefix:"LESS=")
      then env
      else Array.append env [| "LESS=FRX" |]
    in
    Eio.Switch.run
    @@ fun sw ->
    let process_mgr = Eio.Stdenv.process_mgr env in
    let r, w = Eio.Process.pipe process_mgr ~sw in
    (try
       let child =
         Eio.Process.spawn ~sw process_mgr ~cwd ~stdin:r ~env:process_env [ git_pager ]
       in
       Eio.Flow.close r;
       f { output_kind; color; write_end = (w :> Eio.Flow.sink_ty Eio.Flow.sink) };
       Eio.Flow.close w;
       Eio.Process.await_exn child
     with
     | Eio.Io (Eio.Process.E (Eio.Process.Child_error (`Signaled signal)), _) as exn ->
       let bt = Stdlib.Printexc.get_raw_backtrace () in
       if Int.( <> ) signal Stdlib.Sys.sigpipe
       then Stdlib.Printexc.raise_with_backtrace exn bt)
;;
