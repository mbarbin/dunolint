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

module Name = Executable__name
module Public_name = Executable__public_name

let field_name = "executable"

module Field_name = struct
  module T0 = struct
    type t =
      [ `name
      | `public_name
      | `instrumentation
      | `lint
      | `preprocess
      ]
    [@@deriving compare, hash, sexp_of]
  end

  include T0
  include Comparable.Make (T0)
end

type t =
  { mutable name : Name.t option
  ; mutable public_name : Public_name.t option
  ; flags : Flags.t
  ; libraries : Libraries.t
  ; mutable instrumentation : Instrumentation.t option
  ; mutable lint : Lint.t option
  ; mutable preprocess : Preprocess.t option
  ; marked_for_removal : Hash_set.M(Field_name).t
  }
[@@deriving sexp_of]

let indicative_field_ordering =
  [ "name"
  ; "public_name"
  ; "package"
  ; "inline_tests"
  ; "flags"
  ; "libraries"
  ; "instrumentation"
  ; "lint"
  ; "preprocess"
  ]
;;

let normalize t = Libraries.dedup_and_sort t.libraries

let create
      ?name
      ?public_name
      ?(flags = [])
      ?(libraries = [])
      ?instrumentation
      ?lint
      ?preprocess
      ()
  =
  let name = Option.map name ~f:(fun name -> Name.create ~name) in
  let public_name =
    Option.map public_name ~f:(fun public_name -> Public_name.create ~public_name)
  in
  let flags = Flags.create ~flags in
  let libraries = Libraries.create ~libraries in
  let t =
    { name
    ; public_name
    ; flags
    ; libraries
    ; instrumentation
    ; lint
    ; preprocess
    ; marked_for_removal = Hash_set.create (module Field_name)
    }
  in
  normalize t;
  t
;;

let read ~sexps_rewriter ~field =
  let fields = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  let name = ref None in
  let public_name = ref None in
  let flags = ref None in
  let libraries = ref None in
  let instrumentation = ref None in
  let lint = ref None in
  let preprocess = ref None in
  List.iter fields ~f:(fun field ->
    match (field : Sexp.t) with
    | List (Atom "name" :: _) -> name := Some (Name.read ~sexps_rewriter ~field)
    | List (Atom "public_name" :: _) ->
      public_name := Some (Public_name.read ~sexps_rewriter ~field)
    | List (Atom "flags" :: _) -> flags := Some (Flags.read ~sexps_rewriter ~field)
    | List (Atom "libraries" :: _) ->
      libraries := Some (Libraries.read ~sexps_rewriter ~field)
    | List (Atom "instrumentation" :: _) ->
      instrumentation := Some (Instrumentation.read ~sexps_rewriter ~field)
    | List (Atom "lint" :: _) -> lint := Some (Lint.read ~sexps_rewriter ~field)
    | List (Atom "preprocess" :: _) ->
      preprocess := Some (Preprocess.read ~sexps_rewriter ~field)
    | List _ | Atom _ -> ());
  let libraries =
    match !libraries with
    | Some libraries -> libraries
    | None -> Libraries.create ~libraries:[]
  in
  let flags =
    match !flags with
    | Some flags -> flags
    | None -> Flags.create ~flags:[]
  in
  { name = !name
  ; public_name = !public_name
  ; flags
  ; libraries
  ; instrumentation = !instrumentation
  ; lint = !lint
  ; preprocess = !preprocess
  ; marked_for_removal = Hash_set.create (module Field_name)
  }
;;

let write_fields
      ({ name
       ; public_name
       ; flags
       ; libraries
       ; instrumentation
       ; lint
       ; preprocess
       ; marked_for_removal = _
       } as t)
  =
  normalize t;
  List.filter_opt
    [ Option.map name ~f:Name.write
    ; Option.map public_name ~f:Public_name.write
    ; (if Flags.is_empty flags then None else Some (Flags.write flags))
    ; (if Libraries.is_empty libraries then None else Some (Libraries.write libraries))
    ; Option.map instrumentation ~f:Instrumentation.write
    ; Option.map lint ~f:Lint.write
    ; Option.map preprocess ~f:Preprocess.write
    ]
;;

let write t = Sexp.List (Atom field_name :: write_fields t)

let rewrite t ~sexps_rewriter ~field ~load_existing_libraries =
  let fields = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  let () =
    if load_existing_libraries
    then (
      let existing_entries =
        Dunolinter.Sexp_handler.find (module Libraries) ~sexps_rewriter ~fields
        |> Option.value_map ~default:[] ~f:Libraries.entries
      in
      Libraries.add_entries t.libraries ~entries:existing_entries)
  in
  normalize t;
  let new_fields = write_fields t in
  (* First we insert all the missing fields. *)
  Dunolinter.Sexp_handler.insert_new_fields
    ~sexps_rewriter
    ~indicative_field_ordering
    ~fields
    ~new_fields;
  (* Then we edit them in place those that are present. *)
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let maybe_remove state field_name field =
    if Option.is_none state && Hash_set.mem t.marked_for_removal field_name
    then (
      let range = Sexps_rewriter.range sexps_rewriter field in
      File_rewriter.remove file_rewriter ~range)
  in
  List.iter fields ~f:(fun field ->
    match (field : Sexp.t) with
    | List (Atom "name" :: _) ->
      Option.iter t.name ~f:(fun t -> Name.rewrite t ~sexps_rewriter ~field);
      maybe_remove t.name `name field
    | List (Atom "public_name" :: _) ->
      Option.iter t.public_name ~f:(fun t -> Public_name.rewrite t ~sexps_rewriter ~field);
      maybe_remove t.public_name `public_name field
    | List (Atom "flags" :: _) -> Flags.rewrite t.flags ~sexps_rewriter ~field
    | List (Atom "libraries" :: _) -> Libraries.rewrite t.libraries ~sexps_rewriter ~field
    | List (Atom "instrumentation" :: _) ->
      Option.iter t.instrumentation ~f:(fun t ->
        Instrumentation.rewrite t ~sexps_rewriter ~field);
      maybe_remove t.instrumentation `instrumentation field
    | List (Atom "lint" :: _) ->
      Option.iter t.lint ~f:(fun t -> Lint.rewrite t ~sexps_rewriter ~field);
      maybe_remove t.lint `lint field
    | List (Atom "preprocess" :: _) ->
      Option.iter t.preprocess ~f:(fun t -> Preprocess.rewrite t ~sexps_rewriter ~field);
      maybe_remove t.preprocess `preprocess field
    | _ -> ())
;;

type predicate = Dune.Executable.Predicate.t

let eval t ~predicate =
  match (predicate : predicate) with
  | `name condition ->
    (match t.name with
     | None -> Dunolint.Trilang.Undefined
     | Some name ->
       Dunolint.Trilang.eval condition ~f:(fun predicate -> Name.eval name ~predicate))
  | `public_name condition ->
    (match t.public_name with
     | None -> Dunolint.Trilang.Undefined
     | Some public_name ->
       Dunolint.Trilang.eval condition ~f:(fun predicate ->
         Public_name.eval public_name ~predicate))
  | `instrumentation condition ->
    (match t.instrumentation with
     | None -> Dunolint.Trilang.Undefined
     | Some instrumentation ->
       Dunolint.Trilang.eval condition ~f:(fun predicate ->
         Instrumentation.eval instrumentation ~predicate))
  | `lint condition ->
    (match t.lint with
     | None -> Dunolint.Trilang.Undefined
     | Some lint ->
       Dunolint.Trilang.eval condition ~f:(fun predicate -> Lint.eval lint ~predicate))
  | `preprocess condition ->
    (match t.preprocess with
     | None -> Dunolint.Trilang.Undefined
     | Some preprocess ->
       Dunolint.Trilang.eval condition ~f:(fun predicate ->
         Preprocess.eval preprocess ~predicate))
  | `has_field field ->
    (match field with
     | `name -> Option.is_some t.name
     | `public_name -> Option.is_some t.public_name
     | `lint -> Option.is_some t.lint
     | `instrumentation -> Option.is_some t.instrumentation
     | `preprocess -> Option.is_some t.preprocess)
    |> Dunolint.Trilang.const
;;

let rec enforce t ~condition =
  let top_condition = condition in
  match (condition : predicate Blang.t) with
  | Base (`has_field `name) ->
    (match t.name with
     | Some _ -> ()
     | None ->
       Dunolinter.Handler.enforce_failure
         (module Dune.Executable.Predicate)
         ~loc:Loc.none
         ~condition)
  | Not (Base (`has_field (`name as to_mark))) ->
    Hash_set.add t.marked_for_removal to_mark;
    t.name <- None
  | Base (`name condition) ->
    (match t.name with
     | Some name -> Name.enforce name ~condition
     | None ->
       (match
          List.find_map
            (Dunolinter.Linter.at_positive_enforcing_position condition)
            ~f:(function
            | `equals name -> Some name
            | `is_prefix _ | `is_suffix _ -> None)
        with
        | Some name ->
          let name = Name.create ~name in
          t.name <- Some name;
          Name.enforce name ~condition
        | None ->
          Dunolinter.Handler.enforce_failure
            (module Dune.Executable.Predicate)
            ~loc:Loc.none
            ~condition:top_condition))
  | Base (`has_field `public_name) ->
    (match t.public_name with
     | Some _ -> ()
     | None ->
       Dunolinter.Handler.enforce_failure
         (module Dune.Executable.Predicate)
         ~loc:Loc.none
         ~condition)
  | Not (Base (`has_field (`public_name as to_mark))) ->
    Hash_set.add t.marked_for_removal to_mark;
    t.public_name <- None
  | Base (`public_name condition) ->
    (match t.public_name with
     | Some public_name -> Public_name.enforce public_name ~condition
     | None ->
       (match
          List.find_map
            (Dunolinter.Linter.at_positive_enforcing_position condition)
            ~f:(function
            | `equals public_name -> Some public_name
            | `is_prefix _ | `is_suffix _ -> None)
        with
        | Some public_name ->
          let public_name = Public_name.create ~public_name in
          t.public_name <- Some public_name;
          Public_name.enforce public_name ~condition
        | None ->
          Dunolinter.Handler.enforce_failure
            (module Dune.Executable.Predicate)
            ~loc:Loc.none
            ~condition:top_condition))
  | Base (`has_field `instrumentation) ->
    (match t.instrumentation with
     | Some _ -> ()
     | None ->
       t.instrumentation <- Some (Instrumentation.initialize ~condition:Blang.true_))
  | Not (Base (`has_field (`instrumentation as to_mark))) ->
    Hash_set.add t.marked_for_removal to_mark;
    t.instrumentation <- None
  | Base (`instrumentation condition) ->
    let instrumentation =
      match t.instrumentation with
      | Some instrumentation -> instrumentation
      | None ->
        let instrumentation = Instrumentation.initialize ~condition in
        t.instrumentation <- Some instrumentation;
        instrumentation
    in
    Instrumentation.enforce instrumentation ~condition
  | Base (`has_field `lint) ->
    (match t.lint with
     | Some _ -> ()
     | None -> t.lint <- Some (Lint.create ()))
  | Not (Base (`has_field (`lint as to_mark))) ->
    Hash_set.add t.marked_for_removal to_mark;
    t.lint <- None
  | Base (`lint condition) ->
    let lint =
      match t.lint with
      | Some lint -> lint
      | None ->
        let lint = Lint.create () in
        t.lint <- Some lint;
        lint
    in
    Lint.enforce lint ~condition
  | Base (`has_field `preprocess) ->
    (match t.preprocess with
     | Some _ -> ()
     | None -> t.preprocess <- Some (Preprocess.create ()))
  | Not (Base (`has_field (`preprocess as to_mark))) ->
    Hash_set.add t.marked_for_removal to_mark;
    t.preprocess <- None
  | Base (`preprocess condition) ->
    let preprocess =
      match t.preprocess with
      | Some preprocess -> preprocess
      | None ->
        let preprocess = Preprocess.create () in
        t.preprocess <- Some preprocess;
        preprocess
    in
    Preprocess.enforce preprocess ~condition
  | (And _ | If _ | True | False | Not _ | Or _) as condition ->
    Dunolinter.Linter.enforce_blang
      (module Dune.Executable.Predicate)
      t
      ~condition
      ~eval
      ~enforce
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
    match (predicate : Dune.Predicate.t) with
    | `stanza stanza ->
      Blang.eval stanza (fun stanza ->
        match stanza with
        | `executable -> true
        | `include_subdirs | `library | `executables -> false)
      |> Dunolint.Trilang.const
    | `include_subdirs _ | `library _ -> Dunolint.Trilang.Undefined
    | `executable condition ->
      Dunolint.Trilang.eval condition ~f:(fun predicate -> Top.eval t ~predicate)
    | (`instrumentation _ | `lint _ | `preprocess _ | `has_field _) as predicate ->
      Top.eval t ~predicate
  ;;

  let rec enforce (t : t) ~condition =
    match (condition : Dune.Predicate.t Blang.t) with
    | (True | False | And _ | If _ | Not _ | Or _) as condition ->
      Dunolinter.Linter.enforce_blang (module Dune.Predicate) t ~condition ~eval ~enforce
    | Base dune ->
      (match dune with
       | `include_subdirs _ | `library _ | `stanza _ -> ()
       | `executable condition -> Top.enforce t ~condition
       | (`instrumentation _ | `lint _ | `preprocess _ | `has_field _) as predicate ->
         Top.enforce t ~condition:(Blang.base predicate))
  ;;
end

module Private = struct
  let rewrite = rewrite
end

let rewrite t ~sexps_rewriter ~field =
  rewrite t ~sexps_rewriter ~field ~load_existing_libraries:false
;;
