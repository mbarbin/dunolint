(*********************************************************************************)
(*  Git_pager - Run a Git pager to display diffs and other custom outputs        *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

module Unix = UnixLabels

type t =
  { output_kind : [ `Tty | `Pager | `Other ]
  ; git_color_mode : [ `Auto | `Always | `Never ]
  ; write_end : Out_channel.t
  }

let output_kind t = t.output_kind
let git_color_mode t = t.git_color_mode
let write_end t = t.write_end

let should_enable_color t =
  match git_color_mode t with
  | `Always -> true
  | `Never -> false
  | `Auto ->
    (match output_kind t with
     | `Tty -> true
     | `Other -> false
     | `Pager ->
       (* That case is unreachable by design. *)
       true
       [@coverage off])
;;

module Process_status = struct
  type t = Unix.process_status =
    | WEXITED of int
    | WSIGNALED of int
    | WSTOPPED of int

  let to_string t =
    match t with
    | WEXITED i -> Printf.sprintf "Exited %d" i
    | WSIGNALED i -> Printf.sprintf "Signaled %d" i [@coverage off]
    | WSTOPPED i -> Printf.sprintf "Stopped %d" i [@coverage off]
  ;;
end

module String_tty = struct
  type t = string

  let to_string t = t
end

let git_pager_value =
  lazy
    (match
       (* We shortcut git entirely when [GIT_PAGER=cat] so we can run this code in
          tests that do not have an actual git environment, such as in the dune
          [.sandbox/.git]. *)
       match Unix.getenv "GIT_PAGER" with
       | exception Stdlib.Not_found -> None
       | "cat" -> Some "cat"
       | _ -> None
     with
     | Some value -> value
     | None ->
       let ((in_ch, _) as process) =
         Unix.open_process_args "git" [| "git"; "var"; "GIT_PAGER" |]
       in
       let output = In_channel.input_all in_ch in
       (match Unix.close_process process with
        | WEXITED 0 -> output |> String.trim
        | (WEXITED _ | WSIGNALED _ | WSTOPPED _) as process_status ->
          Err.raise
            Pp.O.
              [ Pp.text "Failed to get the value of "
                ++ Pp_tty.kwd (module String_tty) "GIT_PAGER"
                ++ Pp.text "."
              ; Pp_tty.id (module Process_status) process_status
              ]))
;;

let git_color_ui_value =
  lazy
    (let ((in_ch, _) as process) =
       Unix.open_process_args "git" [| "git"; "config"; "--get"; "color.ui" |]
     in
     let output = In_channel.input_all in_ch in
     match Unix.close_process process with
     | WEXITED (0 | 1) ->
       (match output |> String.trim with
        | "" | "auto" -> `Auto
        | "always" -> `Always
        | "never" -> `Never
        | other ->
          Err.raise
            Pp.O.
              [ Pp.text "Unexpected "
                ++ Pp_tty.kwd (module String_tty) "git color.ui"
                ++ Pp.text " value "
                ++ Pp_tty.id (module String_tty) other
                ++ Pp.text "."
              ])
     | (WEXITED _ | WSIGNALED _ | WSTOPPED _) as process_status ->
       Err.raise
         Pp.O.
           [ Pp.text "Failed to get the value of "
             ++ Pp_tty.kwd (module String_tty) "color.ui"
             ++ Pp.text "."
           ; Pp_tty.id (module Process_status) process_status
           ])
;;

let get_git_pager () = Lazy.force git_pager_value
let get_git_color_ui () = Lazy.force git_color_ui_value

let rec waitpid_non_intr pid =
  try Unix.waitpid ~mode:[] pid with
  | Unix.Unix_error (EINTR, _, _) -> waitpid_non_intr pid
;;

let force_stdout_isatty_test = ref false

let run ~f =
  let git_pager = get_git_pager () in
  let output_kind =
    if (Unix.isatty Unix.stdout [@coverage off]) || !force_stdout_isatty_test
    then if String.equal git_pager "cat" then `Tty else `Pager
    else `Other
  in
  let git_color_mode =
    match Err.color_mode () with
    | (`Always | `Never) as override -> override
    | `Auto as auto ->
      (match output_kind with
       | `Tty | `Other -> auto
       | `Pager ->
         (match get_git_color_ui () with
          | (`Always | `Never) as override -> override
          | `Auto -> `Always))
  in
  match output_kind with
  | `Tty | `Other -> f { output_kind; git_color_mode; write_end = Out_channel.stdout }
  | `Pager ->
    let process_env =
      let env = Unix.environment () in
      if Array.exists (fun s -> String.starts_with ~prefix:"LESS=" s) env
      then env
      else Array.append env [| "LESS=FRX" |]
    in
    let pager_in, pager_out = Unix.pipe ~cloexec:true () in
    let process =
      let prog, args =
        match String.split_on_char ' ' git_pager with
        | [] -> assert false (* By specification of [String.split_on_char]. *)
        | [ _ ] -> git_pager, [| git_pager |]
        | prog :: _ as args -> prog, Array.of_list args
      in
      Unix.create_process_env
        ~prog
        ~args
        ~env:process_env
        ~stdin:pager_in
        ~stdout:Unix.stdout
        ~stderr:Unix.stderr
    in
    Unix.close pager_in;
    let write_end = Unix.out_channel_of_descr pager_out in
    let result =
      match
        let res = f { output_kind; git_color_mode; write_end } in
        Out_channel.flush write_end;
        res
      with
      | res -> Ok res
      | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        Error (bt, e)
    in
    (match
       Out_channel.close write_end;
       waitpid_non_intr process |> snd
     with
     | WEXITED 0 ->
       (match result with
        | Ok res -> res
        | Error (bt, exn) -> Printexc.raise_with_backtrace exn bt)
     | exception finally_exn ->
       Err.raise
         Pp.O.
           [ Pp.text "Call to "
             ++ Pp_tty.kwd (module String_tty) "GIT_PAGER"
             ++ Pp.text " raised."
           ; Pp.text "Writer Status: "
             ++ (match result with
               | Ok _ -> Pp.text "Ok"
               | Error (_, exn) -> Pp.text "Raised " ++ Pp_tty.id (module Printexc) exn)
             ++ Pp.text "."
           ; Pp.text "Pager Exception: "
             ++ Pp_tty.id (module Printexc) finally_exn
             ++ Pp.text "."
           ] [@coverage off]
     | (WEXITED _ | WSIGNALED _ | WSTOPPED _) as process_status ->
       (* This line is covered but off due to unvisitable out-edge point. *)
       Err.raise
         Pp.O.
           [ Pp.text "Call to "
             ++ Pp_tty.kwd (module String_tty) "GIT_PAGER"
             ++ Pp.text " failed."
           ; Pp.text "Writer Status: "
             ++ (match result with
               | Ok _ -> Pp.text "Ok"
               | Error (_, exn) -> Pp.text "Raised " ++ Pp_tty.id (module Printexc) exn)
             ++ Pp.text "."
           ; Pp.text "Pager Exit Status: "
             ++ Pp_tty.id (module Process_status) process_status
             ++ Pp.text "."
           ] [@coverage off])
;;

module Private = struct
  let force_stdout_isatty_test = force_stdout_isatty_test
end
