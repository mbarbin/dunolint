(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

module type S = sig
  type t

  include Dunolinter.Stanza_linter.S with type t := t and type predicate = Nothing.t

  module Linter :
    Dunolinter.Linter.S with type t = t and type predicate = Dune.Predicate.t
end

module Make (M : sig
    val field_name : string
  end) =
struct
  let field_name = M.field_name

  (* [args] are the arguments of the stanza, as in the source, and [dirs] their model. *)
  type t =
    { args : Sexp.t list
    ; dirs : Directory_element.t list
    }

  let sexp_of_t { args = _; dirs } : Sexp.t =
    List [ List (Atom "dirs" :: List.map dirs ~f:Directory_element.sexp_of_t) ]
  ;;

  let read ~sexps_rewriter ~field =
    let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
    { args; dirs = List.map args ~f:(Directory_element.read ~sexps_rewriter) }
  ;;

  let write { args; dirs = _ } = Sexp.List (Atom field_name :: args)

  (* The directories are sorted as the elements of the [dirs] stanza, the head of the
     stanza staying in place. *)
  let rewrite (_ : t) ~sexps_rewriter ~field =
    let text = Dunolinter.Predicate_lang.canonical_sort_field ~sexps_rewriter ~field in
    let range = Sexps_rewriter.range sexps_rewriter field in
    let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
    let original_text =
      String.sub
        (File_rewriter.original_contents file_rewriter)
        ~pos:range.start
        ~len:(range.stop - range.start)
    in
    if not (String.equal text original_text)
    then File_rewriter.replace file_rewriter ~range ~text
  ;;

  type predicate = Nothing.t

  let eval (_ : t) ~predicate =
    match[@coverage off] (predicate : predicate) with
    | x -> Nothing.unreachable_code x
  ;;

  let enforce =
    Dunolinter.Linter.enforce
      (module Nothing)
      ~eval
      ~enforce:(fun _ predicate ->
        match[@coverage off] predicate with
        | T x | Not x -> Nothing.unreachable_code x)
  ;;

  module Linter = struct
    type nonrec t = t
    type predicate = Dune.Predicate.t

    let eval (_ : t) ~predicate =
      (* Coverage is disabled due to many patOr, pending better bisect_ppx integration. *)
      match[@coverage off] (predicate : predicate) with
      | `stanza stanza ->
        Blang.eval stanza (function
            | `include_subdirs | `library | `executable | `executables -> false)
        |> Dunolint.Trilang.const
      | `executable _
      | `has_field (`instrumentation | `lint | `name | `preprocess | `public_name)
      | `include_subdirs _
      | `instrumentation _
      | `libraries _
      | `library _
      | `lint _
      | `preprocess _ -> Dunolint.Trilang.Undefined
    ;;

    let enforce =
      Dunolinter.Linter.enforce
        (module Dune.Predicate)
        ~eval
        ~enforce:(fun _ predicate ->
          (* Coverage is disabled due to many patOr, pending better bisect_ppx
             integration. *)
          match[@coverage off] predicate with
          | T (`stanza _) | Not (`stanza _) ->
            (* The linter doesn't change stanza kinds, [stanza] invariants are only
               checked. *)
            Eval
          | T
              ( `executable _
              | `has_field (`instrumentation | `lint | `name | `preprocess | `public_name)
              | `include_subdirs _
              | `instrumentation _
              | `libraries _
              | `library _
              | `lint _
              | `preprocess _ )
          | Not
              ( `executable _
              | `has_field (`instrumentation | `lint | `name | `preprocess | `public_name)
              | `include_subdirs _
              | `instrumentation _
              | `libraries _
              | `library _
              | `lint _
              | `preprocess _ ) -> Unapplicable)
    ;;
  end
end
