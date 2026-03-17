(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

type t = { pps : Pps.t } [@@deriving sexp_of]

let create ?pps () =
  { pps =
      (match pps with
       | Some pps -> pps
       | None -> Pps.create ~args:[])
  }
;;

let field_name = "lint"

let read ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  match args with
  | [ (List (Atom "pps" :: _) as field) ] ->
    let pps = Pps.read ~sexps_rewriter ~field in
    { pps }
  | _ ->
    let loc = Sexps_rewriter.loc sexps_rewriter field in
    Err.raise
      ~loc
      Pp.O.
        [ Pp.text "Unexpected "
          ++ Pp_tty.kwd (module String) "lint"
          ++ Pp.text " field value."
        ]
;;

let write (t : t) = Sexp.List [ Atom field_name; Pps.write t.pps ]

let rewrite t ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  match args with
  | [ (List (Atom "pps" :: _) as field) ] -> Pps.rewrite t.pps ~sexps_rewriter ~field
  | _ ->
    let loc = Sexps_rewriter.loc sexps_rewriter field in
    Err.raise
      ~loc
      Pp.O.
        [ Pp.text "Unexpected "
          ++ Pp_tty.kwd (module String) "lint"
          ++ Pp.text " field value."
        ]
;;

type predicate = Dune.Lint.Predicate.t

let eval t ~predicate =
  match (predicate : predicate) with
  | `pps condition ->
    Dunolint.Trilang.eval condition ~f:(fun predicate -> Pps.eval t.pps ~predicate)
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune.Lint.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | Not (`pps _) -> Eval
      | T (`pps condition) ->
        Pps.enforce t.pps ~condition;
        Ok)
;;
