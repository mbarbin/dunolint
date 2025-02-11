(*******************************************************************************)
(*  Prompt - A library to prompt the user for simple answers in the terminal   *)
(*  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>          *)
(*                                                                             *)
(*  This file is part of Prompt.                                               *)
(*                                                                             *)
(*  Prompt is free software; you can redistribute it and/or modify it under    *)
(*  the terms of the GNU Lesser General Public License as published by the     *)
(*  Free Software Foundation either version 3 of the License, or any later     *)
(*  version, with the LGPL-3.0 Linking Exception.                              *)
(*                                                                             *)
(*  Prompt is distributed in the hope that it will be useful, but WITHOUT ANY  *)
(*  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS  *)
(*  FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License and    *)
(*  file `NOTICE.md` at the root of this repository for more details.          *)
(*                                                                             *)
(*  You should have received a copy of the GNU Lesser General Public License   *)
(*  and the LGPL-3.0 Linking Exception along with this library. If not, see    *)
(*  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.       *)
(*******************************************************************************)

let read_line () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> line
  | None -> raise End_of_file
;;

let aprintf fmt =
  Stdlib.Format.kasprintf
    (fun str ->
       Out_channel.output_string Out_channel.stdout str;
       Out_channel.flush Out_channel.stdout)
    fmt
;;

let read_char () =
  let str = read_line () in
  let len = String.length str in
  if len = 1
  then Ok (Some (Char.lowercase str.[0]))
  else if len = 0
  then Ok None
  else Error ()
;;

let styled style s = Fmt.(str_like stdout "%a" (styled style string) s)

let ask_gen ~prompt ~f =
  let rec loop () =
    aprintf "[%s] %s: " (styled (`Fg `Magenta) "?") prompt;
    let line = read_line () in
    match f line with
    | Ok res -> res
    | Error msg ->
      aprintf "[%s] %s\n" (styled (`Fg `Red) "!") msg;
      loop ()
  in
  loop ()
;;

module Choice = struct
  type 'a t = char * 'a * string

  let create char a ~help:string = char, a, string
  let default (c, a, s) = Char.uppercase c, a, s
end

let choose (type a) ~(choices : (char * a) list) : char option -> (a, unit) Result.t
  = function
  | None ->
    (match List.filter choices ~f:(fun (c, _) -> Char.is_uppercase c) with
     | _ :: _ :: _ as l ->
       raise
         (Invalid_argument
            (Printf.sprintf
               "[Prompt.choose] supplied multiple defaults %S."
               (String.of_char_list (List.map l ~f:fst))))
     | [ (_, a) ] -> Ok a
     | [] -> Error ())
  | Some ch ->
    let filter (reply, _) = Char.equal (Char.lowercase reply) (Char.lowercase ch) in
    (match List.find choices ~f:filter with
     | Some (_, a) -> Ok a
     | None -> Error ())
;;

let ask_internal (type a) ~prompt ~(choices : (char * a) list) =
  let prompt =
    let cs = List.map choices ~f:(fun (c, _) -> Char.to_string c) in
    Printf.sprintf "%s [%s]" prompt (String.concat ~sep:"/" cs)
  in
  let please_answer () =
    let num_choices = List.length choices in
    let choices =
      List.mapi choices ~f:(fun i (char, _value) ->
        let sep = if i = 0 then "" else if i = num_choices - 1 then " or " else ", " in
        Printf.sprintf "%s'%c'" sep (Char.lowercase char))
      |> String.concat
    in
    aprintf "[%s] Please answer %s.\n\n" (styled (`Fg `Red) "!") choices
  in
  let rec loop () =
    aprintf "[%s] %s: " (styled (`Fg `Magenta) "?") prompt;
    match read_char () with
    | Error () ->
      please_answer ();
      loop ()
    | Ok char ->
      (match choose ~choices char with
       | Ok res -> res
       | Error () ->
         please_answer ();
         loop ())
  in
  loop ()
;;

let ask ~prompt ~choices =
  let print_help () =
    aprintf "\nPlease choose among the following options:\n";
    List.iter choices ~f:(fun (char, _value, help) ->
      aprintf "  %c : %s\n" (Char.lowercase char) help);
    aprintf "  ? : Print this help.\n\n"
  in
  let choices =
    List.map choices ~f:(fun (char, value, _help) -> char, `Ok value) @ [ '?', `Help ]
  in
  let rec loop () =
    match ask_internal ~prompt ~choices with
    | `Ok value -> value
    | `Help ->
      print_help ();
      loop ()
  in
  loop ()
;;

let ask_yn ~prompt ~default =
  let y, n =
    match default with
    | None -> 'y', 'n'
    | Some true -> 'Y', 'n'
    | Some false -> 'y', 'N'
  in
  ask_internal ~prompt ~choices:[ y, true; n, false ]
;;

module Arg = struct
  open Command.Std

  let yes = Arg.flag [ "yes" ] ~doc:"do not prompt for confirmation"
end
