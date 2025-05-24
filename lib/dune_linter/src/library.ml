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

module Modes = Library__modes
module Name = Library__name
module Public_name = Library__public_name

let field_name = "library"

module Field_name = struct
  module T0 = struct
    type t =
      [ `name
      | `public_name
      | `modes
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
  ; inline_tests : bool option
  ; mutable modes : Modes.t option
  ; flags : Flags.t
  ; libraries : Libraries.t
  ; libraries_to_open_via_flags : string list
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
  ; "modes"
  ; "flags"
  ; "libraries"
  ; "instrumentation"
  ; "lint"
  ; "preprocess"
  ]
;;

module Library_open_via_flags = struct
  type t = string

  let module_name (t : t) =
    String.split t ~on:'.'
    |> List.rev
    |> List.hd_exn
    |> String.capitalize
    |> String.map ~f:(function
      | '-' -> '_'
      | c -> c)
  ;;
end

let open_via_flags t ~libraries_to_open_via_flags =
  let to_add =
    List.filter_map libraries_to_open_via_flags ~f:(fun library ->
      if Libraries.mem t.libraries ~library:(Dune.Library.Name.v library)
      then Some (Library_open_via_flags.module_name library)
      else None)
  in
  let flags = Flags.flags t.flags in
  let found = ref false in
  let flags =
    let rec map_list flags =
      if not (List.mem flags (Sexp.Atom "-open") ~equal:Sexp.equal)
      then flags
      else (
        let added_modules = Hash_set.create (module String) in
        let rec aux sexps =
          match sexps with
          | [] -> []
          | Sexp.List sexps :: tl -> Sexp.List (List.map sexps ~f:map_flag) :: aux tl
          | Sexp.Atom "-open" :: Atom module_name :: rest ->
            if not !found
            then (
              found := true;
              aux
                (List.concat_map to_add ~f:(fun module_name ->
                   Sexp.[ Atom "-open"; Atom module_name ])
                 @ sexps))
            else if Hash_set.mem added_modules module_name
            then aux rest
            else (
              Hash_set.add added_modules module_name;
              Sexp.Atom "-open" :: Atom module_name :: aux rest)
          | flag :: rest -> flag :: aux rest
        in
        aux flags)
    and map_flag = function
      | Sexp.Atom _ as atom -> atom
      | Sexp.List flags -> Sexp.List (map_list flags)
    in
    map_list flags
  in
  let flags =
    if !found
    then flags
    else
      flags
      @ List.concat_map to_add ~f:(fun module_name ->
        Sexp.[ Atom "-open"; Atom module_name ])
  in
  Flags.set_flags t.flags ~flags
;;

let normalize t =
  open_via_flags t ~libraries_to_open_via_flags:t.libraries_to_open_via_flags;
  Libraries.dedup_and_sort t.libraries
;;

let create
      ?name
      ?public_name
      ?inline_tests
      ?modes
      ?(flags = [])
      ?(libraries = [])
      ?(libraries_to_open_via_flags = [])
      ?instrumentation
      ?lint
      ?preprocess
      ()
  =
  let name = Option.map name ~f:(fun name -> Name.create ~name) in
  let public_name =
    Option.map public_name ~f:(fun public_name -> Public_name.create ~public_name)
  in
  let modes = Option.map modes ~f:(fun modes -> Modes.create ~modes) in
  let flags = Flags.create ~flags in
  let libraries = Libraries.create ~libraries in
  let t =
    { name
    ; public_name
    ; inline_tests
    ; modes
    ; flags
    ; libraries
    ; libraries_to_open_via_flags
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
  let inline_tests = ref None in
  let modes = ref None in
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
    | List (Atom "inline_tests" :: _) -> inline_tests := Some true
    | List (Atom "modes" :: _) -> modes := Some (Modes.read ~sexps_rewriter ~field)
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
  ; inline_tests = !inline_tests
  ; modes = !modes
  ; flags
  ; libraries
  ; libraries_to_open_via_flags = []
  ; instrumentation = !instrumentation
  ; lint = !lint
  ; preprocess = !preprocess
  ; marked_for_removal = Hash_set.create (module Field_name)
  }
;;

let write_fields
      ({ name
       ; public_name
       ; inline_tests
       ; modes
       ; flags
       ; libraries
       ; libraries_to_open_via_flags = _
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
    ; Option.bind inline_tests ~f:(fun inline_tests ->
        if inline_tests then Some (Sexp.List [ Atom "inline_tests" ]) else None)
    ; Option.map modes ~f:Modes.write
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
  (* First we insert all missing fields. *)
  Dunolinter.Sexp_handler.insert_new_fields
    ~sexps_rewriter
    ~indicative_field_ordering
    ~fields
    ~new_fields;
  (* For those which are not missing, we edit them in place. *)
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
    | List [ Atom "inline_tests" ] ->
      Option.iter t.inline_tests ~f:(fun inline_tests ->
        if not inline_tests
        then
          File_rewriter.remove
            file_rewriter
            ~range:(Sexps_rewriter.range sexps_rewriter field))
    | List (Atom "modes" :: _) ->
      Option.iter t.modes ~f:(fun t -> Modes.rewrite t ~sexps_rewriter ~field);
      maybe_remove t.modes `modes field
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

type predicate = Dune.Library.Predicate.t

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
  | `modes condition ->
    (match t.modes with
     | None -> Dunolint.Trilang.Undefined
     | Some modes ->
       Dunolint.Trilang.eval condition ~f:(fun predicate -> Modes.eval modes ~predicate))
  | `preprocess condition ->
    (match t.preprocess with
     | None -> Dunolint.Trilang.Undefined
     | Some preprocess ->
       Dunolint.Trilang.eval condition ~f:(fun predicate ->
         Preprocess.eval preprocess ~predicate))
  | `has_field field ->
    (match field with
     | `instrumentation -> Option.is_some t.instrumentation
     | `lint -> Option.is_some t.lint
     | `modes -> Option.is_some t.modes
     | `name -> Option.is_some t.name
     | `preprocess -> Option.is_some t.preprocess
     | `public_name -> Option.is_some t.public_name)
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
         (module Dune.Library.Predicate)
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
            (module Dune.Library.Predicate)
            ~loc:Loc.none
            ~condition:top_condition))
  | Base (`has_field `public_name) ->
    (match t.public_name with
     | Some _ -> ()
     | None ->
       Dunolinter.Handler.enforce_failure
         (module Dune.Library.Predicate)
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
            (module Dune.Library.Predicate)
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
  | Base (`has_field `modes) ->
    (match t.name with
     | Some _ -> ()
     | None -> t.modes <- Some (Modes.initialize ~condition:Blang.true_))
  | Not (Base (`has_field (`modes as to_mark))) ->
    Hash_set.add t.marked_for_removal to_mark;
    t.modes <- None
  | Base (`modes condition) ->
    let modes =
      match t.modes with
      | Some modes -> modes
      | None ->
        let modes = Modes.initialize ~condition in
        t.modes <- Some modes;
        modes
    in
    Modes.enforce modes ~condition
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
      (module Dune.Library.Predicate)
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
    match (predicate : predicate) with
    | `stanza stanza ->
      Blang.eval stanza (fun stanza ->
        match stanza with
        | `library -> true
        | `include_subdirs | `executable | `executables -> false)
      |> Dunolint.Trilang.const
    | `include_subdirs _ | `executable _ -> Dunolint.Trilang.Undefined
    | `library condition ->
      Dunolint.Trilang.eval condition ~f:(fun predicate -> Top.eval t ~predicate)
    | ( `instrumentation _ | `lint _ | `preprocess _
      | `has_field (`instrumentation | `lint | `name | `preprocess | `public_name) ) as
      predicate -> Top.eval t ~predicate
  ;;

  let rec enforce t ~condition =
    match (condition : predicate Blang.t) with
    | (True | False | And _ | If _ | Not _ | Or _) as condition ->
      Dunolinter.Linter.enforce_blang (module Dune.Predicate) t ~condition ~eval ~enforce
    | Base dune ->
      (match dune with
       | `include_subdirs _ | `executable _ | `stanza _ -> ()
       | `library condition -> Top.enforce t ~condition
       | ( `instrumentation _ | `lint _ | `preprocess _
         | `has_field (`instrumentation | `lint | `name | `preprocess | `public_name) ) as
         predicate -> Top.enforce t ~condition:(Blang.base predicate))
  ;;
end

module Private = struct
  let rewrite = rewrite
end

let rewrite t ~sexps_rewriter ~field =
  rewrite t ~sexps_rewriter ~field ~load_existing_libraries:false
;;
