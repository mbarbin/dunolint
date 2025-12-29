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

let parse contents =
  Test_helpers.parse
    (module Dune_project_linter.Generate_opam_files)
    ~path:(Fpath.v "dune-project")
    contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_project_linter.Generate_opam_files.write t))
  in
  test {| (generate_opam_files) |};
  [%expect {| (generate_opam_files) |}];
  test {| (invalid field) |};
  [%expect
    {|
    File "dune-project", line 1, characters 1-16:
    Error: Unexpected [generate_opam_files] field.
    [123]
    |}];
  test {||};
  [%expect
    {|
    Error: Expected exactly 1 sexp, got 0.
    [123]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (generate_opam_files) |} in
  print_s [%sexp (t : Dune_project_linter.Generate_opam_files.t)];
  [%expect {| ((args ())) |}];
  ()
;;

let rewrite ?(f = ignore) str =
  let (sexps_rewriter, field), t = parse str in
  f t;
  Dune_project_linter.Generate_opam_files.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (generate_opam_files) |};
  [%expect {| (generate_opam_files) |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_project_linter.Generate_opam_files.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t = Dune_project_linter.Generate_opam_files.create () in
  test t {| (generate_opam_files) |};
  [%expect {| (generate_opam_files) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune_project.Generate_opam_files.Predicate.t as 'a
    constraint 'a = [ `is_present ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (generate_opam_files) |} in
  Test_helpers.is_true
    (Dune_project_linter.Generate_opam_files.eval t ~predicate:`is_present);
  [%expect {||}];
  ()
;;

let%expect_test "enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_project_linter.Generate_opam_files.enforce t ~condition);
      Dune_project_linter.Generate_opam_files.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open! Blang.O in
  let t = parse {| (generate_opam_files) |} in
  enforce t [];
  [%expect {| (generate_opam_files) |}];
  (* Enforcing the presence of the field has no effect if it is already present. *)
  enforce t [ is_present ];
  [%expect {| (generate_opam_files) |}];
  (* Blang. *)
  enforce t [ true_ ];
  [%expect {| (generate_opam_files) |}];
  require_does_raise (fun () -> enforce t [ false_ ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition false)) |}];
  require_does_raise (fun () -> enforce t [ not_ is_present ]);
  [%expect
    {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not is_present))) |}];
  enforce t [ and_ [ is_present; is_present ] ];
  [%expect {| (generate_opam_files) |}];
  enforce t [ or_ [ is_present; is_present ] ];
  [%expect {| (generate_opam_files) |}];
  enforce t [ if_ is_present is_present (not_ is_present) ];
  [%expect {| (generate_opam_files) |}];
  ()
;;

let%expect_test "Linter.eval" =
  let _, t = parse {| (generate_opam_files) |} in
  Test_helpers.is_true
    (Dune_project_linter.Generate_opam_files.Linter.eval
       t
       ~predicate:(`generate_opam_files is_present));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_project_linter.Generate_opam_files.Linter.eval
       t
       ~predicate:(`implicit_transitive_deps true_));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_project_linter.Generate_opam_files.Linter.eval t ~predicate:(`name true_));
  [%expect {||}];
  ()
;;

let%expect_test "Linter.enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_project_linter.Generate_opam_files.Linter.enforce t ~condition);
      Dune_project_linter.Generate_opam_files.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open! Blang.O in
  let t = parse {| (generate_opam_files) |} in
  enforce t [];
  [%expect {| (generate_opam_files) |}];
  (* Enforcing the presence of the field has no effect if it is already present. *)
  enforce t [ generate_opam_files is_present ];
  [%expect {| (generate_opam_files) |}];
  (* Enforcing other toplevel stanza has no effect. *)
  enforce t [ implicit_transitive_deps false_ ];
  [%expect {| (generate_opam_files) |}];
  enforce t [ name false_ ];
  [%expect {| (generate_opam_files) |}];
  enforce t [ not_ (name false_) ];
  [%expect {| (generate_opam_files) |}];
  (* Blang. *)
  enforce t [ true_ ];
  [%expect {| (generate_opam_files) |}];
  require_does_raise (fun () -> enforce t [ false_ ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition false)) |}];
  require_does_raise (fun () -> enforce t [ generate_opam_files (not_ is_present) ]);
  [%expect
    {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not is_present))) |}];
  enforce t [ and_ [ generate_opam_files is_present; generate_opam_files is_present ] ];
  [%expect {| (generate_opam_files) |}];
  enforce t [ or_ [ generate_opam_files is_present; generate_opam_files is_present ] ];
  [%expect {| (generate_opam_files) |}];
  enforce
    t
    [ if_
        (generate_opam_files is_present)
        (generate_opam_files is_present)
        (not_ (generate_opam_files is_present))
    ];
  [%expect {| (generate_opam_files) |}];
  ()
;;
