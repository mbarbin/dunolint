(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

module State = struct
  type t =
    | No_preprocessing
    | Pps of Pps.t
    | Unhandled of Sexp.t
  [@@deriving sexp_of]
end

type t = { mutable state : State.t } [@@deriving sexp_of]

let state t = t.state
let set_state t ~state = t.state <- state

let create ?pps () =
  { state =
      (match pps with
       | Some pps -> Pps pps
       | None -> No_preprocessing)
  }
;;

let field_name = "preprocess"

let read ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  match args with
  | [ Atom "no_preprocessing" ] -> { state = No_preprocessing }
  | [ (List (Atom "pps" :: _) as field) ] ->
    let pps = Pps.read ~sexps_rewriter ~field in
    { state = Pps pps }
  | [ sexp ] -> { state = Unhandled sexp }
  | _ ->
    let loc = Sexps_rewriter.loc sexps_rewriter field in
    Err.raise
      ~loc
      Pp.O.
        [ Pp.text "Unexpected "
          ++ Pp_tty.kwd (module String) field_name
          ++ Pp.text " field value."
        ]
;;

let write (t : t) =
  let args =
    match t.state with
    | No_preprocessing -> [ Sexp.Atom "no_preprocessing" ]
    | Pps pps -> [ Pps.write pps ]
    | Unhandled sexp -> [ sexp ]
  in
  Sexp.List (Atom field_name :: args)
;;

let rewrite t ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  match args with
  | [ (Atom "no_preprocessing" as arg) ] ->
    (match t.state with
     | No_preprocessing -> ()
     | Pps pps ->
       File_rewriter.replace
         file_rewriter
         ~range:(Sexps_rewriter.range sexps_rewriter arg)
         ~text:(Pps.write pps |> Sexp.to_string)
     | Unhandled _ -> ())
  | [ (List (Atom "pps" :: _) as field) ] ->
    let loc = Sexps_rewriter.loc sexps_rewriter field in
    (match t.state with
     | No_preprocessing ->
       File_rewriter.replace file_rewriter ~range:(Loc.range loc) ~text:"no_preprocessing"
     | Pps pps -> Pps.rewrite pps ~sexps_rewriter ~field
     | Unhandled _ -> ())
  | [ arg ] ->
    let loc = Sexps_rewriter.loc sexps_rewriter arg in
    (match t.state with
     | No_preprocessing ->
       File_rewriter.replace file_rewriter ~range:(Loc.range loc) ~text:"no_preprocessing"
     | Pps pps ->
       File_rewriter.replace
         file_rewriter
         ~range:(Sexps_rewriter.range sexps_rewriter arg)
         ~text:(Pps.write pps |> Sexp.to_string)
     | Unhandled _ -> ())
  | _ ->
    let loc = Sexps_rewriter.loc sexps_rewriter field in
    Err.raise
      ~loc
      Pp.O.
        [ Pp.text "Unexpected "
          ++ Pp_tty.kwd (module String) field_name
          ++ Pp.text " field value."
        ]
;;

type predicate = Dune.Preprocess.Predicate.t

let eval t ~predicate =
  match (predicate : predicate) with
  | `no_preprocessing ->
    (match t.state with
     | No_preprocessing -> true |> Dunolint.Trilang.const
     | Pps _ | Unhandled _ -> false |> Dunolint.Trilang.const)
  | `pps condition ->
    (match t.state with
     | Pps pps ->
       Dunolint.Trilang.eval condition ~f:(fun predicate -> Pps.eval pps ~predicate)
     | No_preprocessing | Unhandled _ -> false |> Dunolint.Trilang.const)
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune.Preprocess.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | Not (`no_preprocessing | `pps _) -> Eval
      | T `no_preprocessing ->
        t.state <- No_preprocessing;
        Ok
      | T (`pps condition) ->
        (match t.state with
         | Pps pps -> Pps.enforce pps ~condition
         | No_preprocessing | Unhandled _ ->
           let pps = Pps.create ~args:[] in
           Pps.enforce pps ~condition;
           t.state <- Pps pps);
        Ok)
;;
