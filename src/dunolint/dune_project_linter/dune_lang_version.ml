(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*                                                                               *)
(*  This file is part of Dunolint.                                               *)
(*                                                                               *)
(*  Dunolint is free software; you can redistribute it and/or modify it          *)
(*  under the terms of the GNU Lesser General Public License as published by     *)
(*  the Free Software Foundation either version 3 of the License, or any later   *)
(*  version, with the LGPL-3.0 Linking Exception.                                *)
(*                                                                               *)
(*  Dunolint is distributed in the hope that it will be useful, but WITHOUT      *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*                                                                               *)
(*  You should have received a copy of the GNU Lesser General Public License     *)
(*  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
(*********************************************************************************)

let field_name = "lang"

type t = { mutable dune_lang_version : Dune_project.Dune_lang_version.t }
[@@deriving sexp_of]

let create ~dune_lang_version = { dune_lang_version }
let dune_lang_version t = t.dune_lang_version
let set_dune_lang_version t ~dune_lang_version = t.dune_lang_version <- dune_lang_version

let read ~sexps_rewriter ~field =
  match field with
  | Sexp.List [ Sexp.Atom "lang"; Sexp.Atom "dune"; (Sexp.Atom version_string as atom) ]
    ->
    (* Parse version string like "3.17" into tuple (3, 17) *)
    (match String.split version_string ~on:'.' with
     | [ major_str; minor_str ] ->
       (match Int.of_string major_str, Int.of_string minor_str with
        | major, minor ->
          { dune_lang_version = Dune_project.Dune_lang_version.create (major, minor) }
        | exception _ ->
          Err.raise
            ~loc:(Sexps_rewriter.loc sexps_rewriter atom)
            [ Pp.textf "Invalid version format: %S." version_string ])
     | _ ->
       Err.raise
         ~loc:(Sexps_rewriter.loc sexps_rewriter atom)
         [ Pp.textf "Expected VERSION.MINOR format, got: %S." version_string ])
  | _ ->
    Err.raise
      ~loc:(Sexps_rewriter.loc sexps_rewriter field)
      [ Pp.text "Expected (lang dune VERSION) format." ]
;;

let write t =
  let version_string = Dune_project.Dune_lang_version.to_string t.dune_lang_version in
  Sexp.List [ Sexp.Atom "lang"; Sexp.Atom "dune"; Sexp.Atom version_string ]
;;

let rewrite t ~sexps_rewriter ~field =
  let new_field = write t in
  Dunolinter.Sexp_handler.replace_field ~sexps_rewriter ~field ~new_field
;;

type predicate = Dune_project.Dune_lang_version.Predicate.t

let eval t ~predicate =
  (match (predicate : predicate) with
   | `eq version | `equals version ->
     Dune_project.Dune_lang_version.equal version t.dune_lang_version
   | `neq version ->
     not (Dune_project.Dune_lang_version.equal version t.dune_lang_version)
   | `geq version | `greater_than_or_equal_to version ->
     Dune_project.Dune_lang_version.compare t.dune_lang_version version >= 0
   | `gt version -> Dune_project.Dune_lang_version.compare t.dune_lang_version version > 0
   | `leq version | `less_than_or_equal_to version ->
     Dune_project.Dune_lang_version.compare t.dune_lang_version version <= 0
   | `lt version -> Dune_project.Dune_lang_version.compare t.dune_lang_version version < 0)
  |> Dunolint.Trilang.const
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune_project.Dune_lang_version.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | T (`eq version | `equals version) | Not (`neq version) ->
        t.dune_lang_version <- version;
        Ok
      | T (`neq _) | Not (`eq _ | `equals _) -> Eval
      | T (`geq version | `greater_than_or_equal_to version) | Not (`lt version) ->
        if Dune_project.Dune_lang_version.compare t.dune_lang_version version < 0
        then t.dune_lang_version <- version;
        Ok
      | T (`lt _) | Not (`geq _ | `greater_than_or_equal_to _) -> Eval
      | T (`leq version | `less_than_or_equal_to version) | Not (`gt version) ->
        if Dune_project.Dune_lang_version.compare t.dune_lang_version version > 0
        then t.dune_lang_version <- version;
        Ok
      | T (`gt _) | Not (`leq _ | `less_than_or_equal_to _) -> Eval)
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
    | `dune_lang_version condition ->
      Dunolint.Trilang.eval condition ~f:(fun predicate -> Top.eval t ~predicate)
    | predicate ->
      let () =
        (* This construct is the same as featuring all values in the match case
           but we cannot disable individual coverage in or patterns with
           bisect_ppx atm. Left for future work. *)
        match[@coverage off] predicate with
        | `dune_lang_version _ -> assert false
        | `name _ | `generate_opam_files _ | `implicit_transitive_deps _ -> ()
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
           | `dune_lang_version condition ->
             Top.enforce t ~condition;
             Ok
           | condition ->
             let () =
               match[@coverage off] condition with
               | `dune_lang_version _ -> assert false
               | `name _ | `generate_opam_files _ | `implicit_transitive_deps _ -> ()
             in
             Unapplicable))
  ;;
end
