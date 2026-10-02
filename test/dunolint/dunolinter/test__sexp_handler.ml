(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

module Int_list =
  Dunolinter.Sexp_handler.Make_sexpable_list
    (struct
      let field_name = "ints"
    end)
    (Int)

let%expect_test "rewrite" =
  let test original_contents ~f =
    let sexps_rewriter, field =
      Test_helpers.read_sexp_field ~path:(Fpath.v "dune") original_contents
    in
    let t = Int_list.read ~sexps_rewriter ~field in
    Int_list.rewrite (f t) ~sexps_rewriter ~field;
    print_endline (Sexps_rewriter.contents sexps_rewriter)
  in
  test {|(ints 1 2 3)|} ~f:(fun t -> 0 :: t);
  [%expect {| (ints 0 1 2 3) |}];
  ()
;;

let%expect_test "insert" =
  let insert ?overlaps original_contents ~indicative_field_ordering ~new_fields =
    let sexps_rewriter =
      match Sexps_rewriter.create ~path:(Fpath.v "file") ~original_contents with
      | Ok r -> r
      | Error { loc; message } -> Err.raise ~loc [ Pp.text message ] [@coverage off]
    in
    let fields = Sexps_rewriter.original_sexps sexps_rewriter in
    let overlaps =
      match overlaps with
      | Some overlaps -> overlaps
      | None -> fun ~field_name:_ ~present_args:_ ~new_args:_ -> true
    in
    Dunolinter.Sexp_handler.insert_new_fields
      ~sexps_rewriter
      ~indicative_field_ordering
      ~fields
      ~new_fields
      ~overlaps;
    let new_sexps =
      Parsexp.Many.parse_string_exn (Sexps_rewriter.contents sexps_rewriter)
    in
    print_s (List new_sexps)
  in
  insert {| () |} ~indicative_field_ordering:[] ~new_fields:[];
  [%expect {| (()) |}];
  insert
    {| (a a) (b b) (c c) |}
    ~indicative_field_ordering:[ "a"; "d"; "b" ]
    ~new_fields:[ Sexp.List [ Atom "d"; Atom "d" ]; Sexp.List [ Atom "e"; Atom "e" ] ];
  [%expect {| ((a a) (d d) (b b) (e e) (c c)) |}];
  insert
    {| (a a) ((c) c) (b b) |}
    ~indicative_field_ordering:[ "a"; "d"; "b" ]
    ~new_fields:[ Sexp.List [ Atom "d"; Atom "d" ] ];
  [%expect {| ((a a) (d d) ((c) c) (b b)) |}];
  (* With custom overlaps. *)
  (* By default new fields with names already present are considered overlapping and
     are not inserted. One can customize the [overlap] function to change that. *)
  insert
    {| (a a) (b b1) (b b2) (c c) |}
    ~indicative_field_ordering:[ "a"; "b"; "c" ]
    ~new_fields:[ Sexp.List [ Atom "b"; Atom "b-new" ] ];
  [%expect {| ((a a) (b b1) (b b2) (c c)) |}];
  (* When there are several instances of the same field, the new one is inserted
     after the last one of them. *)
  insert
    {| (a a) (b b1) (b b2) (c c) |}
    ~indicative_field_ordering:[ "a"; "b"; "c" ]
    ~new_fields:[ Sexp.List [ Atom "b"; Atom "b-new" ] ]
    ~overlaps:(fun ~field_name:_ ~present_args:_ ~new_args:_ -> false);
  [%expect {| ((a a) (b b1) (b b2) (b b-new) (c c)) |}];
  insert
    {| (a a) (b b) (c c) |}
    ~indicative_field_ordering:[ "a"; "b"; "c" ]
    ~new_fields:
      [ Sexp.List [ Atom "a"; Atom "a-new" ]
      ; Sexp.List [ Atom "b"; Atom "b-new" ]
      ; Sexp.List [ Atom "c"; Atom "c-new" ]
      ; Sexp.List [ Atom "d"; Atom "d-new" ]
      ]
    ~overlaps:(fun ~field_name ~present_args ~new_args:_ ->
      match field_name with
      | "b" -> false
      | "a" | "c" ->
        List.exists present_args ~f:(function
          | Atom "c" -> true
          | _ -> false)
      | _ -> true);
  [%expect {| ((a a) (a a-new) (b b) (b b-new) (c c) (d d-new)) |}];
  ()
;;
