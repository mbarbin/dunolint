(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let field_name = "include_subdirs"

type t = { mutable mode : Dune.Include_subdirs.Mode.t } [@@deriving sexp_of]

let create ~mode = { mode }
let mode t = t.mode
let set_mode t ~mode = t.mode <- mode

module Handler =
  Dunolinter.Sexp_handler.Make_sexpable
    (struct
      let field_name = field_name
    end)
    (Dune.Include_subdirs.Mode)

let read ~sexps_rewriter ~field =
  let mode = Handler.read ~sexps_rewriter ~field in
  create ~mode
;;

let write t = Handler.write t.mode
let rewrite t ~sexps_rewriter ~field = Handler.rewrite t.mode ~sexps_rewriter ~field

type predicate = Dune.Include_subdirs.Predicate.t

let eval t ~predicate =
  match (predicate : predicate) with
  | `equals mode -> Dune.Include_subdirs.Mode.equal t.mode mode |> Dunolint.Trilang.const
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune.Include_subdirs.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | Not (`equals _) -> Eval
      | T (`equals mode) ->
        t.mode <- mode;
        Ok)
;;

module Top = struct
  type nonrec t = t

  let eval = eval
  let enforce = enforce
end

module Linter = struct
  type t = Top.t
  type predicate = Dune.Predicate.t

  let eval (t : t) ~predicate =
    (* Coverage is disabled due to many patOr, pending better bisect_ppx integration. *)
    match[@coverage off] (predicate : predicate) with
    | `stanza stanza ->
      Blang.eval stanza (fun stanza ->
        Dune.Stanza.Predicate.equal stanza `include_subdirs)
      |> Dunolint.Trilang.const
    | `include_subdirs condition ->
      Dunolint.Trilang.eval condition ~f:(fun predicate -> Top.eval t ~predicate)
    | `executable _
    | `has_field (`instrumentation | `lint | `name | `preprocess | `public_name)
    | `instrumentation _ | `libraries _ | `library _ | `lint _ | `preprocess _ ->
      Dunolint.Trilang.Undefined
  ;;

  let enforce =
    Dunolinter.Linter.enforce
      (module Dune.Predicate)
      ~eval
      ~enforce:(fun t predicate ->
        match predicate with
        | Not _ -> Eval
        | T dune ->
          (* Coverage is disabled due to many patOr, pending better bisect_ppx
             integration. *)
          (match[@coverage off] dune with
           | `include_subdirs condition ->
             Top.enforce t ~condition;
             Ok
           | `executable _
           | `has_field (`instrumentation | `lint | `name | `preprocess | `public_name)
           | `instrumentation _
           | `libraries _
           | `library _
           | `lint _
           | `preprocess _
           | `stanza _ -> Unapplicable))
  ;;
end
