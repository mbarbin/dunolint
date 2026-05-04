(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let field_name = "lang"

type t = { mutable dunolint_lang_version : Dunolint0.Dunolint_lang_version.t }
[@@deriving sexp_of]

let create ~dunolint_lang_version = { dunolint_lang_version }
let dunolint_lang_version t = t.dunolint_lang_version

let set_dunolint_lang_version t ~dunolint_lang_version =
  t.dunolint_lang_version <- dunolint_lang_version
;;

let read ~sexps_rewriter ~field =
  match (field : Sexp.t) with
  | List [ Atom "lang"; (Atom lang as atom_lang); (Atom version_string as atom) ] ->
    (match String.equal lang "dunolint" with
     | false ->
       Err.raise
         ~loc:(Sexps_rewriter.loc sexps_rewriter atom_lang)
         [ Pp.text "Expected (lang dunolint VERSION) format." ]
     | true ->
       (match String.split version_string ~on:'.' with
        | [ major_str; minor_str ] ->
          (match Int.of_string major_str, Int.of_string minor_str with
           | major, minor ->
             { dunolint_lang_version =
                 Dunolint0.Dunolint_lang_version.create (major, minor)
             }
           | exception _ ->
             Err.raise
               ~loc:(Sexps_rewriter.loc sexps_rewriter atom)
               [ Pp.textf "Invalid version format: %S." version_string ])
        | _ ->
          Err.raise
            ~loc:(Sexps_rewriter.loc sexps_rewriter atom)
            [ Pp.textf "Expected VERSION.MINOR format, got: %S." version_string ]))
  | _ ->
    Err.raise
      ~loc:(Sexps_rewriter.loc sexps_rewriter field)
      [ Pp.text "Expected (lang dunolint VERSION) format." ]
;;

let write t =
  let version_string =
    Dunolint0.Dunolint_lang_version.to_string t.dunolint_lang_version
  in
  Sexp.List [ Sexp.Atom "lang"; Sexp.Atom "dunolint"; Sexp.Atom version_string ]
;;

let rewrite t ~sexps_rewriter ~field =
  let new_field = write t in
  Dunolinter.Sexp_handler.replace_field ~sexps_rewriter ~field ~new_field
;;

type predicate = Dunolint0.Dunolint_lang_version.Predicate.t

let eval t ~predicate =
  (match (predicate : predicate) with
   | `eq version -> Dunolint0.Dunolint_lang_version.equal version t.dunolint_lang_version
   | `gt version ->
     Dunolint0.Dunolint_lang_version.compare t.dunolint_lang_version version > 0
   | `gte version ->
     Dunolint0.Dunolint_lang_version.compare t.dunolint_lang_version version >= 0
   | `lt version ->
     Dunolint0.Dunolint_lang_version.compare t.dunolint_lang_version version < 0
   | `lte version ->
     Dunolint0.Dunolint_lang_version.compare t.dunolint_lang_version version <= 0
   | `neq version ->
     not (Dunolint0.Dunolint_lang_version.equal version t.dunolint_lang_version))
  |> Dunolint.Trilang.const
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dunolint0.Dunolint_lang_version.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | T (`eq version) | Not (`neq version) ->
        t.dunolint_lang_version <- version;
        Ok
      | T (`gt _) | Not (`lte _) -> Eval
      | T (`gte version) | Not (`lt version) ->
        if Dunolint0.Dunolint_lang_version.compare t.dunolint_lang_version version < 0
        then t.dunolint_lang_version <- version;
        Ok
      | T (`lt _) | Not (`gte _) -> Eval
      | T (`lte version) | Not (`gt version) ->
        if Dunolint0.Dunolint_lang_version.compare t.dunolint_lang_version version > 0
        then t.dunolint_lang_version <- version;
        Ok
      | T (`neq _) | Not (`eq _) -> Eval)
;;

module Top = struct
  type nonrec t = t

  let eval = eval
  let enforce = enforce
end

module Linter = struct
  type t = Top.t
  type predicate = Dunolint0.Predicate.t

  let eval (t : t) ~predicate =
    match (predicate : Dunolint0.Predicate.t) with
    | `dunolint_lang_version condition ->
      Dunolint.Trilang.eval condition ~f:(fun predicate -> Top.eval t ~predicate)
  ;;

  let enforce =
    Dunolinter.Linter.enforce
      (module Dunolint0.Predicate)
      ~eval
      ~enforce:(fun t predicate ->
        match predicate with
        | Not _ -> Eval
        | T condition ->
          (match condition with
           | `dunolint_lang_version condition ->
             Top.enforce t ~condition;
             Ok))
  ;;
end
