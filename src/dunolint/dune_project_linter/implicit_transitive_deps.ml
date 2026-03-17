(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let field_name = "implicit_transitive_deps"

module Value = Dune_project.Implicit_transitive_deps.Value

type t = { mutable value : Value.t } [@@deriving sexp_of]

let create ~implicit_transitive_deps:value = { value }
let value t = t.value
let set_value t ~value = t.value <- value

module Handler =
  Dunolinter.Sexp_handler.Make_sexpable
    (struct
      let field_name = field_name
    end)
    (Value)

let read ~sexps_rewriter ~field =
  let value = Handler.read ~sexps_rewriter ~field in
  { value }
;;

let write t = Handler.write t.value
let rewrite t ~sexps_rewriter ~field = Handler.rewrite t.value ~sexps_rewriter ~field

type predicate = Dune_project.Implicit_transitive_deps.Predicate.t

let eval t ~predicate =
  match (predicate : predicate) with
  | `equals expected -> Value.equal expected t.value |> Dunolint.Trilang.const
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune_project.Implicit_transitive_deps.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | Not (`equals value) ->
        (* If current value already satisfies the negation, no change needed. *)
        if not (Value.equal t.value value)
        then Ok (* Already satisfies "not equals value". *)
        else (
          (* Current value equals the negated value, so change it. *)
          match value with
          | `True ->
            t.value <- `False;
            Ok
          | `False | `False_if_hidden_includes_supported ->
            t.value <- `True;
            Ok)
      | T (`equals value) ->
        t.value <- value;
        Ok)
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
    | `implicit_transitive_deps condition ->
      Dunolint.Trilang.eval condition ~f:(fun predicate -> Top.eval t ~predicate)
    | predicate ->
      let () =
        match[@coverage off] predicate with
        | `implicit_transitive_deps _ -> assert false
        | `name _ | `dune_lang_version _ | `generate_opam_files _ -> ()
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
           | `implicit_transitive_deps condition ->
             Top.enforce t ~condition;
             Ok
           | condition ->
             let () =
               match[@coverage off] condition with
               | `implicit_transitive_deps _ -> assert false
               | `name _ | `dune_lang_version _ | `generate_opam_files _ -> ()
             in
             Unapplicable))
  ;;
end
