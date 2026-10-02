(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

module Name = Executable__name
module Public_name = Executable__public_name

let field_name = "executable"

module Field_name = struct
  type t =
    [ `name
    | `public_name
    | `instrumentation
    | `lint
    | `preprocess
    ]

  [@@@coverage off]

  let compare t1 t2 =
    match t1 with
    | `name | `public_name | `instrumentation | `lint | `preprocess -> Repr.compare t1 t2
  ;;

  let equal t1 t2 =
    match t1 with
    | `name | `public_name | `instrumentation | `lint | `preprocess -> Repr.equal t1 t2
  ;;

  let sexp_of_t : t -> Sexp.t = function
    | `name -> Atom "name"
    | `public_name -> Atom "public_name"
    | `instrumentation -> Atom "instrumentation"
    | `lint -> Atom "lint"
    | `preprocess -> Atom "preprocess"
  ;;

  let hash (t : t) : int =
    match t with
    | `name | `public_name | `instrumentation | `lint | `preprocess -> Hashtbl.hash t
  ;;
end

module Field_name_table = MoreLabels.Hashtbl.Make (Field_name)

type t =
  { mutable name : Name.t option
  ; mutable public_name : Public_name.t option
  ; flags : Flags.t
  ; libraries : Libraries.t
  ; mutable instrumentations : Instrumentation.t list
  ; mutable lint : Lint.t option
  ; mutable preprocess : Preprocess.t option
  ; marked_for_removal : unit Field_name_table.t
  }

let sexp_of_t
      { name
      ; public_name
      ; flags
      ; libraries
      ; instrumentations
      ; lint
      ; preprocess
      ; marked_for_removal
      }
  =
  let opt field ~f =
    match field with
    | None -> []
    | Some field -> [ f field ]
  in
  (Sexp.List
     (List.concat
        [ opt name ~f:(fun v -> Sexp.List [ Atom "name"; Name.sexp_of_t v ])
        ; opt public_name ~f:(fun v ->
            Sexp.List [ Atom "public_name"; Public_name.sexp_of_t v ])
        ; (if Flags.is_empty flags
           then []
           else [ Sexp.List [ Atom "flags"; Flags.sexp_of_t flags ] ])
        ; (if Libraries.is_empty libraries
           then []
           else [ Sexp.List [ Atom "libraries"; Libraries.sexp_of_t libraries ] ])
        ; List.map instrumentations ~f:(fun v ->
            Sexp.List [ Atom "instrumentation"; Instrumentation.sexp_of_t v ])
        ; opt lint ~f:(fun v -> Sexp.List [ Atom "lint"; Lint.sexp_of_t v ])
        ; opt preprocess ~f:(fun v ->
            Sexp.List [ Atom "preprocess"; Preprocess.sexp_of_t v ])
        ; (if Field_name_table.length marked_for_removal = 0
           then []
           else (
             let fields =
               marked_for_removal
               |> Field_name_table.to_seq_keys
               |> List.of_seq
               |> List.sort ~compare:Field_name.compare
             in
             [ Sexp.List
                 [ Atom "marked_for_removal"
                 ; List (fields |> List.map ~f:Field_name.sexp_of_t)
                 ]
             ]))
        ])
  [@coverage off])
;;

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

let flags t = t.flags
let normalize t = Libraries.dedup_and_sort t.libraries

let create
      ?name
      ?public_name
      ?(flags = [])
      ?(libraries = [])
      ?(instrumentations = [])
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
    ; instrumentations
    ; lint
    ; preprocess
    ; marked_for_removal = Field_name_table.create 16
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
  let instrumentations = ref [] in
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
      instrumentations := Instrumentation.read ~sexps_rewriter ~field :: !instrumentations
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
  ; instrumentations = List.rev !instrumentations
  ; lint = !lint
  ; preprocess = !preprocess
  ; marked_for_removal = Field_name_table.create 16
  }
;;

let write_fields
      ({ name
       ; public_name
       ; flags
       ; libraries
       ; instrumentations
       ; lint
       ; preprocess
       ; marked_for_removal = _
       } as t)
  =
  normalize t;
  let opt field ~f =
    match field with
    | None -> []
    | Some field -> [ f field ]
  in
  List.concat
    [ opt name ~f:Name.write
    ; opt public_name ~f:Public_name.write
    ; (if Flags.is_empty flags then [] else [ Flags.write flags ])
    ; (if Libraries.is_empty libraries then [] else [ Libraries.write libraries ])
    ; List.map instrumentations ~f:Instrumentation.write
    ; opt lint ~f:Lint.write
    ; opt preprocess ~f:Preprocess.write
    ]
;;

let write t = Sexp.List (Atom field_name :: write_fields t)

let rewrite t ~sexps_rewriter ~field =
  let fields = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  normalize t;
  let new_fields = write_fields t in
  (* First we insert all the missing fields. *)
  Dunolinter.Sexp_handler.insert_new_fields
    ~sexps_rewriter
    ~indicative_field_ordering
    ~fields
    ~new_fields
    ~overlaps:(fun ~field_name ~present_args ~new_args ->
      match field_name with
      | "instrumentation" ->
        Instrumentation_entries.insertion_overlaps ~present_args ~new_args
      | _ -> true);
  (* Then we edit them in place those that are present. *)
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let remove field =
    let range = Sexps_rewriter.range sexps_rewriter field in
    File_rewriter.remove file_rewriter ~range
  in
  let remove_if_marked field_name field =
    if Field_name_table.mem t.marked_for_removal field_name then remove field
  in
  let maybe_remove state field_name field =
    if Option.is_none state then remove_if_marked field_name field
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
    | List (Atom "instrumentation" :: args) ->
      (match Instrumentation_entries.rewrite t.instrumentations ~args with
       | `Remove_if_marked -> remove_if_marked `instrumentation field
       | `Remove -> remove field
       | `Rewrite_with t -> Instrumentation.rewrite t ~sexps_rewriter ~field)
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
    Instrumentation_entries.eval t.instrumentations ~condition
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
     | `lint -> Option.is_some t.lint
     | `instrumentation -> not (List.is_empty t.instrumentations)
     | `preprocess -> Option.is_some t.preprocess)
    |> Dunolint.Trilang.const
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune.Executable.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | Not condition ->
        (match condition with
         | `has_field has_field ->
           Field_name_table.add t.marked_for_removal ~key:has_field ~data:();
           (match has_field with
            | `name -> t.name <- None
            | `public_name -> t.public_name <- None
            | `instrumentation -> t.instrumentations <- []
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
             | `instrumentation _
             | `libraries _
             | `lint _
             | `name _
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
      | T (`has_field `instrumentation) ->
        (match t.instrumentations with
         | _ :: _ -> Ok
         | [] ->
           t.instrumentations <- [ Instrumentation.initialize ~condition:Blang.true_ ];
           Ok)
      | T (`instrumentation condition) ->
        Instrumentation_entries.enforce
          t.instrumentations
          ~condition
          ~insert_instrumentation:(fun instrumentation ->
            t.instrumentations <- t.instrumentations @ [ instrumentation ]);
        (* We accept the enforcement only if it is stable through further evaluation. *)
        Eval
      | T (`libraries condition) ->
        Libraries.enforce t.libraries ~condition;
        Ok
      | T (`has_field `lint) ->
        (match t.lint with
         | Some _ -> Ok
         | None ->
           t.lint <- Some (Lint.create ());
           Ok)
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
    match[@coverage off] (predicate : Dune.Predicate.t) with
    | `stanza stanza ->
      Blang.eval stanza (fun stanza -> Dune.Stanza.Predicate.equal stanza `executable)
      |> Dunolint.Trilang.const
    | `include_subdirs _ | `library _ -> Dunolint.Trilang.Undefined
    | `executable condition ->
      Dunolint.Trilang.eval condition ~f:(fun predicate -> Top.eval t ~predicate)
    | (`has_field _ | `instrumentation _ | `libraries _ | `lint _ | `preprocess _) as
      predicate -> Top.eval t ~predicate
  ;;

  let enforce =
    Dunolinter.Linter.enforce
      (module Dune.Predicate)
      ~eval
      ~enforce:(fun t predicate ->
        (* Coverage is disabled due to many patOr, pending better bisect_ppx
           integration. *)
        match[@coverage off] predicate with
        | T (`executable condition) ->
          Top.enforce t ~condition;
          Ok
        | Not (`executable _) -> Eval
        | T
            (( `has_field (`instrumentation | `lint | `name | `preprocess | `public_name)
             | `instrumentation _ | `libraries _ | `lint _ | `preprocess _ ) as predicate)
          ->
          Top.enforce t ~condition:(Blang.base predicate);
          Ok
        | Not
            (( `has_field (`instrumentation | `lint | `name | `preprocess | `public_name)
             | `instrumentation _ | `libraries _ | `lint _ | `preprocess _ ) as predicate)
          ->
          Top.enforce t ~condition:(Blang.not_ (Blang.base predicate));
          Ok
        | T (`stanza _) | Not (`stanza _) ->
          (* The linter doesn't change stanza kinds, [stanza] invariants are only
             checked. *)
          Eval
        | T (`include_subdirs _ | `library _) | Not (`include_subdirs _ | `library _) ->
          Unapplicable)
  ;;
end

module Private = struct end
