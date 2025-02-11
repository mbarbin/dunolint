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

module Unix = UnixLabels

type t =
  { output_kind : [ `Tty | `Pager | `Other ]
  ; color : [ `Auto | `Always | `Never ]
  ; write_end : Out_channel.t
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

module Process_status = struct
  type t = Unix.process_status =
    | WEXITED of int
    | WSIGNALED of int
    | WSTOPPED of int
  [@@deriving sexp_of]
end

let get_git_pager () =
  let ((in_ch, _) as process) =
    Unix.open_process_args "git" [| "git"; "var"; "GIT_PAGER" |]
  in
  let output = In_channel.input_all in_ch in
  match Unix.close_process process with
  | WEXITED 0 -> output |> String.strip
  | (WEXITED _ | WSIGNALED _ | WSTOPPED _) as process_status ->
    Err.raise
      Pp.O.
        [ Pp.text "Failed to get the value of "
          ++ Pp_tty.kwd (module String) "GIT_PAGER"
          ++ Pp.text "."
        ; Err.pp_of_sexp (Process_status.sexp_of_t process_status)
        ]
;;

let get_git_color () =
  let ((in_ch, _) as process) =
    Unix.open_process_args "git" [| "git"; "config"; "--get"; "color.ui" |]
  in
  let output = In_channel.input_all in_ch in
  match Unix.close_process process with
  | WEXITED (0 | 1) ->
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
  | (WEXITED _ | WSIGNALED _ | WSTOPPED _) as process_status ->
    Err.raise
      Pp.O.
        [ Pp.text "Failed to get the value of "
          ++ Pp_tty.kwd (module String) "color.ui"
          ++ Pp.text "."
        ; Err.pp_of_sexp (Process_status.sexp_of_t process_status)
        ]
;;

let git_pager = lazy (get_git_pager ())
let get_git_color = lazy (get_git_color ())

let rec waitpid_non_intr pid =
  try Unix.waitpid ~mode:[] pid with
  | Unix.Unix_error (EINTR, _, _) -> waitpid_non_intr pid
;;

let run ~f =
  let git_pager = force git_pager in
  let color = force get_git_color in
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
  | `Tty | `Other -> f { output_kind; color; write_end = stdout }
  | `Pager ->
    let process_env =
      let env = Unix.environment () in
      if Array.exists env ~f:(fun s -> String.is_prefix s ~prefix:"LESS=")
      then env
      else Array.append env [| "LESS=FRX" |]
    in
    let pager_in, pager_out = Unix.pipe ~cloexec:true () in
    let process =
      Unix.create_process_env
        ~prog:git_pager
        ~args:[| git_pager |]
        ~env:process_env
        ~stdin:pager_in
        ~stdout:Unix.stdout
        ~stderr:Unix.stderr
    in
    let out_ch = Unix.out_channel_of_descr pager_out in
    Exn.protect
      ~f:(fun () ->
        let res = f { output_kind; color; write_end = out_ch } in
        Out_channel.flush out_ch;
        res)
      ~finally:(fun () ->
        Out_channel.close out_ch;
        match waitpid_non_intr process |> snd with
        | WEXITED 0 -> ()
        | WSIGNALED signal when Int.equal signal Stdlib.Sys.sigpipe -> ()
        | (WEXITED _ | WSIGNALED _ | WSTOPPED _) as process_status ->
          Err.raise
            Pp.O.
              [ Pp.text "Call to "
                ++ Pp_tty.kwd (module String) "GIT_PAGER"
                ++ Pp.text "failed."
              ; Err.pp_of_sexp (Process_status.sexp_of_t process_status)
              ])
;;
