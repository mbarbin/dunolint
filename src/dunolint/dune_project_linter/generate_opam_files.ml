(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let field_name = "generate_opam_files"

type t = { args : Sexp.t list } [@@deriving sexp_of]

let create () = { args = [] }

module Handler = Dunolinter.Sexp_handler.Make_sexp_list (struct
    let field_name = field_name
  end)

let read ~sexps_rewriter ~field =
  let args = Handler.read ~sexps_rewriter ~field in
  { args }
;;

let write t = Handler.write t.args
let rewrite t ~sexps_rewriter ~field = Handler.rewrite t.args ~sexps_rewriter ~field

type predicate = Dune_project.Generate_opam_files.Predicate.t

let eval _t ~predicate =
  match (predicate : predicate) with
  | `is_present -> true |> Dunolint.Trilang.const
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune_project.Generate_opam_files.Predicate)
    ~eval
    ~enforce:(fun _ predicate ->
      match predicate with
      | Not `is_present -> Eval
      | T `is_present -> Ok)
;;

module Top = struct
  type nonrec t = t

  let eval = eval
  let enforce = enforce
end

module Linter = struct
  type t = Top.t
  type predicate = Dune_project.Predicate.t

  let eval (t : t) ~predicate =
    match (predicate : Dune_project.Predicate.t) with
    | `generate_opam_files condition ->
      Dunolint.Trilang.eval condition ~f:(fun predicate -> Top.eval t ~predicate)
    | predicate ->
      let () =
        match[@coverage off] predicate with
        | `generate_opam_files _ -> assert false
        | `name _ | `dune_lang_version _ | `implicit_transitive_deps _ -> ()
      in
      Dunolint.Trilang.Undefined
  ;;

  let enforce =
    Dunolinter.Linter.enforce
      (module Dune_project.Predicate)
      ~eval
      ~enforce:(fun t predicate ->
        match predicate with
        | Not _ -> Eval
        | T condition ->
          (match condition with
           | `generate_opam_files condition ->
             Top.enforce t ~condition;
             Ok
           | condition ->
             let () =
               match[@coverage off] condition with
               | `generate_opam_files _ -> assert false
               | `name _ | `dune_lang_version _ | `implicit_transitive_deps _ -> ()
             in
             Unapplicable))
  ;;
end
