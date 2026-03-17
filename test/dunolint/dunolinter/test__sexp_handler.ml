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
  let insert original_contents ~indicative_field_ordering ~new_fields =
    let sexps_rewriter =
      match Sexps_rewriter.create ~path:(Fpath.v "file") ~original_contents with
      | Ok r -> r
      | Error { loc; message } -> Err.raise ~loc [ Pp.text message ] [@coverage off]
    in
    let fields = Sexps_rewriter.original_sexps sexps_rewriter in
    Dunolinter.Sexp_handler.insert_new_fields
      ~sexps_rewriter
      ~indicative_field_ordering
      ~fields
      ~new_fields;
    print_endline (Sexps_rewriter.contents sexps_rewriter)
  in
  insert {| () |} ~indicative_field_ordering:[] ~new_fields:[];
  [%expect {| () |}];
  insert
    {| (a a) (b b) (c c) |}
    ~indicative_field_ordering:[ "a"; "d"; "b" ]
    ~new_fields:[ Sexp.List [ Atom "d"; Atom "d" ]; Sexp.List [ Atom "e"; Atom "e" ] ];
  [%expect
    {|
    (a a)
    (d d) (b b)
          (e e) (c c)
    |}];
  insert
    {| (a a) ((c) c) (b b) |}
    ~indicative_field_ordering:[ "a"; "d"; "b" ]
    ~new_fields:[ Sexp.List [ Atom "d"; Atom "d" ] ];
  [%expect
    {|
    (a a)
    (d d) ((c) c) (b b)
    |}];
  ()
;;
