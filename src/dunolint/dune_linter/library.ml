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
module Package = Library__package
module Public_name = Library__public_name

let field_name = "library"

module Field_name = struct
  module T0 = struct
    type t =
      [ `name
      | `public_name
      | `package
      | `inline_tests
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
  ; mutable package : Package.t option
  ; mutable inline_tests : unit option
  ; mutable modes : Modes.t option
  ; flags : Flags.t
  ; libraries : Libraries.t
  ; mutable libraries_to_open_via_flags : string list
  ; mutable instrumentation : Instrumentation.t option
  ; mutable lint : Lint.t option
  ; mutable preprocess : Preprocess.t option
  ; marked_for_removal : Hash_set.M(Field_name).t
  }

let sexp_of_t
      { name
      ; public_name
      ; package
      ; inline_tests
      ; modes
      ; flags
      ; libraries
      ; libraries_to_open_via_flags
      ; instrumentation
      ; lint
      ; preprocess
      ; marked_for_removal
      }
  =
  Sexp.List
    (List.filter_opt
       [ Option.map name ~f:(fun v -> Sexp.List [ Atom "name"; Name.sexp_of_t v ])
       ; Option.map public_name ~f:(fun v ->
           Sexp.List [ Atom "public_name"; Public_name.sexp_of_t v ])
       ; Option.map package ~f:(fun v ->
           Sexp.List [ Atom "package"; Package.sexp_of_t v ])
       ; Option.map inline_tests ~f:(fun () -> Sexp.List [ Atom "inline_tests" ])
       ; Option.map modes ~f:(fun v -> Sexp.List [ Atom "modes"; Modes.sexp_of_t v ])
       ; (if Flags.is_empty flags
          then None
          else Some (Sexp.List [ Atom "flags"; Flags.sexp_of_t flags ]))
       ; (if Libraries.is_empty libraries
          then None
          else Some (Sexp.List [ Atom "libraries"; Libraries.sexp_of_t libraries ]))
       ; (if List.is_empty libraries_to_open_via_flags
          then None
          else
            Some
              (Sexp.List
                 [ Atom "libraries_to_open_via_flags"
                 ; Sexp.List
                     (List.map libraries_to_open_via_flags ~f:(fun s -> Sexp.Atom s))
                 ]))
       ; Option.map instrumentation ~f:(fun v ->
           Sexp.List [ Atom "instrumentation"; Instrumentation.sexp_of_t v ])
       ; Option.map lint ~f:(fun v -> Sexp.List [ Atom "lint"; Lint.sexp_of_t v ])
       ; Option.map preprocess ~f:(fun v ->
           Sexp.List [ Atom "preprocess"; Preprocess.sexp_of_t v ])
       ; (if Hash_set.is_empty marked_for_removal
          then None
          else
            Some
              (Sexp.List
                 [ Atom "marked_for_removal"
                 ; Hash_set.sexp_of_m__t (module Field_name) marked_for_removal
                 ]))
       ])
  [@coverage off]
;;

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

let name t = t.name
let flags t = t.flags

let set_libraries_to_open_via_flags t ~libraries_to_open_via_flags =
  t.libraries_to_open_via_flags <- libraries_to_open_via_flags
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

module Item = struct
  type t =
    | Open_via_flags of { module_name : string }
    | Open_via_flags_sorted of
        { module_name : string
        ; index : int
        }
    | Other of Sexp.t

  let to_flags = function
    | Open_via_flags { module_name } | Open_via_flags_sorted { module_name; index = _ } ->
      [ Sexp.Atom "-open"; Atom module_name ]
    | Other sexp -> [ sexp ]
  ;;

  let kind = function
    | Open_via_flags _ -> 0
    | Open_via_flags_sorted _ -> 1
    | Other _ -> 2
  ;;

  let index_exn = function
    | Open_via_flags_sorted { module_name = _; index } -> index
    | Other _ | Open_via_flags _ -> invalid_arg "No index on item" [@coverage off]
  ;;
end

let open_via_flags_groups sexps =
  let rec aux acc = function
    | [] -> acc
    | Sexp.Atom "-open" :: Atom module_name :: rest ->
      aux (Item.Open_via_flags { module_name } :: acc) rest
    | hd :: tl -> aux (Item.Other hd :: acc) tl
  in
  List.rev (aux [] sexps)
;;

let order_open_via_flags_sections sexps ~to_open_via_flags =
  let order_spec =
    List.mapi to_open_via_flags ~f:(fun i module_name -> module_name, i)
    |> Map.of_alist_exn (module String)
  in
  sexps
  |> open_via_flags_groups
  |> List.map ~f:(fun t ->
    match (t : Item.t) with
    | Open_via_flags_sorted _ -> assert false
    | Other _ -> t
    | Open_via_flags { module_name } ->
      (match Map.find order_spec module_name with
       | None -> t
       | Some index -> Item.Open_via_flags_sorted { module_name; index }))
  |> List.group ~break:(fun t1 t2 -> Item.kind t1 <> Item.kind t2)
  |> List.map ~f:(fun group ->
    match group with
    | [] -> assert false
    | (Item.Open_via_flags _ | Other _) :: _ -> group
    | Open_via_flags_sorted _ :: _ ->
      List.sort group ~compare:(Comparable.lift Int.compare ~f:Item.index_exn))
  |> List.concat
  |> List.concat_map ~f:Item.to_flags
;;

let open_via_flags t ~libraries_to_open_via_flags =
  let flags = Flags.flags t.flags in
  let existing_open_via_flags =
    let hset = Hash_set.create (module String) in
    let rec iter_list = function
      | [] -> ()
      | Sexp.Atom "-open" :: Atom module_name :: rest ->
        Hash_set.add hset module_name;
        iter_list rest
      | hd :: tl ->
        iter_one hd;
        iter_list tl
    and iter_one = function
      | Sexp.Atom _ -> ()
      | Sexp.List sexps -> iter_list sexps
    in
    iter_list flags;
    hset
  in
  let to_open_via_flags =
    List.filter_map libraries_to_open_via_flags ~f:(fun library ->
      if Libraries.mem t.libraries ~library:(Dune.Library.Name.v library)
      then Some (Library_open_via_flags.module_name library)
      else None)
  in
  let to_add =
    List.filter to_open_via_flags ~f:(fun module_name ->
      not (Hash_set.mem existing_open_via_flags module_name))
  in
  let flags =
    match to_add with
    | [] -> flags
    | _ :: _ ->
      flags
      @ List.concat_map to_add ~f:(fun module_name ->
        Sexp.[ Atom "-open"; Atom module_name ])
  in
  let flags =
    (* When the specified list is not empty, we try and enforce the ordering in
       which it is defined. *)
    if List.is_empty to_open_via_flags
    then flags
    else order_open_via_flags_sections flags ~to_open_via_flags
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
      ?package
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
  let package = Option.map package ~f:(fun name -> Package.create ~name) in
  let modes = Option.map modes ~f:(fun modes -> Modes.create ~modes) in
  let flags = Flags.create ~flags in
  let libraries = Libraries.create ~libraries in
  let marked_for_removal = Hash_set.create (module Field_name) in
  let inline_tests =
    match inline_tests with
    | None -> None
    | Some true -> Some ()
    | Some false ->
      Hash_set.add marked_for_removal `inline_tests;
      None
  in
  let t =
    { name
    ; public_name
    ; package
    ; inline_tests
    ; modes
    ; flags
    ; libraries
    ; libraries_to_open_via_flags
    ; instrumentation
    ; lint
    ; preprocess
    ; marked_for_removal
    }
  in
  normalize t;
  t
;;

let read ~sexps_rewriter ~field =
  let fields = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  let name = ref None in
  let public_name = ref None in
  let package = ref None in
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
    | List (Atom "package" :: _) -> package := Some (Package.read ~sexps_rewriter ~field)
    | List (Atom "inline_tests" :: _) -> inline_tests := Some ()
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
  ; package = !package
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
       ; package
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
    ; Option.map package ~f:Package.write
    ; Option.map inline_tests ~f:(fun () -> Sexp.List [ Atom "inline_tests" ])
    ; Option.map modes ~f:Modes.write
    ; (if Flags.is_empty flags then None else Some (Flags.write flags))
    ; (if Libraries.is_empty libraries then None else Some (Libraries.write libraries))
    ; Option.map instrumentation ~f:Instrumentation.write
    ; Option.map lint ~f:Lint.write
    ; Option.map preprocess ~f:Preprocess.write
    ]
;;

let write t = Sexp.List (Atom field_name :: write_fields t)

let rewrite t ~sexps_rewriter ~field =
  let fields = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
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
    | List (Atom "package" :: _) ->
      Option.iter t.package ~f:(fun t -> Package.rewrite t ~sexps_rewriter ~field);
      maybe_remove t.package `package field
    | List (Atom "inline_tests" :: _) -> maybe_remove t.inline_tests `inline_tests field
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
  | `if_present (`public_name condition) ->
    (match t.public_name with
     | None -> Dunolint.Trilang.True
     | Some public_name ->
       Dunolint.Trilang.eval condition ~f:(fun predicate ->
         Public_name.eval public_name ~predicate))
  | `package condition ->
    (match t.package with
     | None -> Dunolint.Trilang.Undefined
     | Some package ->
       Dunolint.Trilang.eval condition ~f:(fun predicate ->
         Package.eval package ~predicate))
  | `if_present (`package condition) ->
    (match t.package with
     | None -> Dunolint.Trilang.True
     | Some package ->
       Dunolint.Trilang.eval condition ~f:(fun predicate ->
         Package.eval package ~predicate))
  | `modes condition ->
    (match t.modes with
     | None -> Dunolint.Trilang.Undefined
     | Some modes ->
       Dunolint.Trilang.eval condition ~f:(fun predicate -> Modes.eval modes ~predicate))
  | `instrumentation condition ->
    (match t.instrumentation with
     | None -> Dunolint.Trilang.Undefined
     | Some instrumentation ->
       Dunolint.Trilang.eval condition ~f:(fun predicate ->
         Instrumentation.eval instrumentation ~predicate))
  | `libraries condition ->
    Dunolint.Trilang.eval condition ~f:(fun predicate ->
      Libraries.eval t.libraries ~predicate)
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
     | `package -> Option.is_some t.package
     | `inline_tests -> Option.is_some t.inline_tests
     | `modes -> Option.is_some t.modes
     | `instrumentation -> Option.is_some t.instrumentation
     | `lint -> Option.is_some t.lint
     | `preprocess -> Option.is_some t.preprocess)
    |> Dunolint.Trilang.const
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune.Library.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | Not condition ->
        (match condition with
         | `has_field has_field ->
           Hash_set.add t.marked_for_removal has_field;
           (match has_field with
            | `name -> t.name <- None
            | `public_name -> t.public_name <- None
            | `package -> t.package <- None
            | `inline_tests -> t.inline_tests <- None
            | `modes -> t.modes <- None
            | `instrumentation -> t.instrumentation <- None
            | `lint -> t.lint <- None
            | `preprocess -> t.preprocess <- None);
           Ok
         | condition ->
           let () =
             (* This construct is the same as featuring all values in the match
                case but we cannot disable individual coverage in or patterns
                with bisect_ppx atm. Left for future work. *)
             match[@coverage off] condition with
             | `has_field _ -> assert false
             | `if_present _
             | `instrumentation _
             | `libraries _
             | `lint _
             | `modes _
             | `name _
             | `package _
             | `preprocess _
             | `public_name _ -> ()
           in
           Eval)
      | T (`has_field `name) ->
        (match t.name with
         | Some _ -> Ok
         | None -> Fail)
      | T (`name condition) ->
        (match t.name with
         | Some name ->
           Name.enforce name ~condition;
           Ok
         | None ->
           (match
              Dunolinter.Linter.find_init_value condition ~f:(function
                | `equals name -> Some name
                | `is_prefix _ | `is_suffix _ -> None)
            with
            | None -> Fail
            | Some name ->
              let name = Name.create ~name in
              t.name <- Some name;
              Name.enforce name ~condition;
              Ok))
      | T (`has_field `public_name) ->
        (match t.public_name with
         | Some _ -> Ok
         | None -> Fail)
      | T (`if_present (`public_name condition)) ->
        (match t.public_name with
         | None -> Unapplicable
         | Some public_name ->
           Public_name.enforce public_name ~condition;
           Ok)
      | T (`public_name condition) ->
        (match t.public_name with
         | Some public_name ->
           Public_name.enforce public_name ~condition;
           Ok
         | None ->
           (match
              Dunolinter.Linter.find_init_value condition ~f:(function
                | `equals public_name -> Some public_name
                | `is_prefix _ | `is_suffix _ -> None)
            with
            | None -> Fail
            | Some public_name ->
              let public_name = Public_name.create ~public_name in
              t.public_name <- Some public_name;
              Public_name.enforce public_name ~condition;
              Ok))
      | T (`has_field `package) ->
        (match t.package with
         | Some _ -> Ok
         | None -> Fail)
      | T (`if_present (`package condition)) ->
        (match t.package with
         | None -> Unapplicable
         | Some package ->
           Package.enforce package ~condition;
           Ok)
      | T (`package condition) ->
        (match t.package with
         | Some package ->
           Package.enforce package ~condition;
           Ok
         | None ->
           (match
              Dunolinter.Linter.find_init_value condition ~f:(function
                | `equals name -> Some name
                | `is_prefix _ | `is_suffix _ -> None)
            with
            | None -> Fail
            | Some name ->
              let package = Package.create ~name in
              t.package <- Some package;
              Package.enforce package ~condition;
              Ok))
      | T (`has_field `inline_tests) ->
        (match t.inline_tests with
         | Some () -> Ok
         | None ->
           t.inline_tests <- Some ();
           Ok)
      | T (`has_field `modes) ->
        (match t.modes with
         | Some _ -> Ok
         | None ->
           let modes = Modes.create ~modes:(Dunolinter.Ordered_set.of_list [ `best ]) in
           t.modes <- Some modes;
           Ok)
      | T (`modes condition) ->
        let modes =
          match t.modes with
          | Some modes -> modes
          | None ->
            let modes = Modes.create ~modes:Dunolinter.Ordered_set.empty in
            t.modes <- Some modes;
            modes
        in
        Modes.enforce modes ~condition;
        Ok
      | T (`has_field `instrumentation) ->
        (match t.instrumentation with
         | Some _ -> Ok
         | None ->
           t.instrumentation <- Some (Instrumentation.initialize ~condition:Blang.true_);
           Ok)
      | T (`instrumentation condition) ->
        let instrumentation =
          match t.instrumentation with
          | Some instrumentation -> instrumentation
          | None ->
            let instrumentation = Instrumentation.initialize ~condition in
            t.instrumentation <- Some instrumentation;
            instrumentation
        in
        Instrumentation.enforce instrumentation ~condition;
        Ok
      | T (`has_field `lint) ->
        (match t.lint with
         | Some _ -> Ok
         | None ->
           t.lint <- Some (Lint.create ());
           Ok)
      | T (`libraries condition) ->
        Libraries.enforce t.libraries ~condition;
        Ok
      | T (`lint condition) ->
        let lint =
          match t.lint with
          | Some lint -> lint
          | None ->
            let lint = Lint.create () in
            t.lint <- Some lint;
            lint
        in
        Lint.enforce lint ~condition;
        Ok
      | T (`has_field `preprocess) ->
        (match t.preprocess with
         | Some _ -> Ok
         | None ->
           t.preprocess <- Some (Preprocess.create ());
           Ok)
      | T (`preprocess condition) ->
        let preprocess =
          match t.preprocess with
          | Some preprocess -> preprocess
          | None ->
            let preprocess = Preprocess.create () in
            t.preprocess <- Some preprocess;
            preprocess
        in
        Preprocess.enforce preprocess ~condition;
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
      Blang.eval stanza (fun stanza -> Dune.Stanza.Predicate.equal stanza `library)
      |> Dunolint.Trilang.const
    | `library condition ->
      Dunolint.Trilang.eval condition ~f:(fun predicate -> Top.eval t ~predicate)
    | `include_subdirs _ | `executable _ -> Dunolint.Trilang.Undefined
    | ( `has_field (`instrumentation | `lint | `name | `preprocess | `public_name)
      | `instrumentation _ | `libraries _ | `lint _ | `preprocess _ ) as predicate ->
      Top.eval t ~predicate
  ;;

  let enforce =
    Dunolinter.Linter.enforce
      (module Dune.Predicate)
      ~eval
      ~enforce:(fun t predicate ->
        (* Coverage is disabled due to many patOr, pending better bisect_ppx
           integration. *)
        match[@coverage off] predicate with
        | Not _ -> Eval
        | T (`include_subdirs _ | `executable _ | `stanza _) -> Unapplicable
        | T (`library condition) ->
          Top.enforce t ~condition;
          Ok
        | T
            (( `has_field (`instrumentation | `lint | `name | `preprocess | `public_name)
             | `instrumentation _ | `libraries _ | `lint _ | `preprocess _ ) as predicate)
          ->
          Top.enforce t ~condition:(Blang.base predicate);
          Ok)
  ;;
end

module Private = struct end
