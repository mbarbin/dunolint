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
    (module Dune_project_linter.Name)
    ~path:(Fpath.v "dune-project")
    contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_project_linter.Name.write t))
  in
  test {| (name my_project) |};
  [%expect {| (name my_project) |}];
  test {| (name (invalid field)) |};
  [%expect
    {|
    Internal Error: (Of_sexp_error (_ ((invalid_sexp (invalid field)))))
    <backtrace disabled in tests>
    [125]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (name my_project) |} in
  print_s [%sexp (t : Dune_project_linter.Name.t)];
  [%expect {| ((name my_project)) |}];
  ()
;;

let rewrite ?(f = ignore) str =
  let (sexps_rewriter, field), t = parse str in
  f t;
  Dune_project_linter.Name.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (name my_project) |};
  [%expect {| (name my_project) |}];
  (* Exercising some getters. *)
  rewrite {| (name other_project) |} ~f:(fun t ->
    print_s [%sexp (Dune_project_linter.Name.name t : Dune_project.Name.t)];
    [%expect {| other_project |}];
    ());
  [%expect {| (name other_project) |}];
  (* Exercising some setters. *)
  rewrite {| (name other_project) |} ~f:(fun t ->
    Dune_project_linter.Name.set_name t ~name:(Dune_project.Name.v "my_project");
    ());
  [%expect {| (name my_project) |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_project_linter.Name.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t = Dune_project_linter.Name.create ~name:(Dune_project.Name.v "my_project") in
  test t {| (name other_project) |};
  [%expect {| (name my_project) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune_project.Name.Predicate.t as 'a
    constraint
      'a =
      [ `equals of Dune_project.Name.t | `is_prefix of string | `is_suffix of string ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (name my_project) |} in
  Test_helpers.is_true
    (Dune_project_linter.Name.eval
       t
       ~predicate:(`equals (Dune_project.Name.v "my_project")));
  [%expect {| |}];
  Test_helpers.is_false
    (Dune_project_linter.Name.eval
       t
       ~predicate:(`equals (Dune_project.Name.v "other_project")));
  [%expect {| |}];
  Test_helpers.is_true (Dune_project_linter.Name.eval t ~predicate:(`is_prefix "my_"));
  [%expect {| |}];
  Test_helpers.is_false (Dune_project_linter.Name.eval t ~predicate:(`is_prefix "other_"));
  [%expect {| |}];
  Test_helpers.is_true
    (Dune_project_linter.Name.eval t ~predicate:(`is_suffix "_project"));
  [%expect {| |}];
  Test_helpers.is_false (Dune_project_linter.Name.eval t ~predicate:(`is_suffix "_other"));
  [%expect {| |}];
  ()
;;

let%expect_test "enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_project_linter.Name.enforce t ~condition);
      Dune_project_linter.Name.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  let t = parse {| (name my_project) |} in
  enforce t [];
  [%expect {| (name my_project) |}];
  (* Enforcing the equality with the current value has no effect. *)
  enforce t [ equals (Dune_project.Name.v "my_project") ];
  [%expect {| (name my_project) |}];
  (* Enforcing the equality with a new value changes it. *)
  enforce t [ equals (Dune_project.Name.v "other_project") ];
  [%expect {| (name other_project) |}];
  let t = parse {| (name other_project) |} in
  (* Enforcing the negation of the equality with another value has no effect. *)
  enforce t [ not_ (equals (Dune_project.Name.v "my_project")) ];
  [%expect {| (name other_project) |}];
  (* Enforcing the negation of a current equality triggers an error.
     Dunolint is not going to automatically invent a new setting, this
     requires the user's intervention. *)
  require_does_raise (fun () ->
    enforce t [ not_ (equals (Dune_project.Name.v "other_project")) ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (not (equals other_project))))
    |}];
  let t = parse {| (name prefix_name_suffix) |} in
  let invariant = and_ [ not_ (is_prefix "prefix_"); not_ (is_suffix "_suffix") ] in
  enforce t [ invariant ];
  [%expect {| (name name) |}];
  (* That invariant is idempotent. *)
  enforce t [ invariant ];
  [%expect {| (name name) |}];
  let invariant = and_ [ is_prefix "new_"; is_suffix "_v2" ] in
  enforce t [ invariant ];
  [%expect {| (name new_name_v2) |}];
  (* That invariant is idempotent. *)
  enforce t [ invariant ];
  [%expect {| (name new_name_v2) |}];
  (* Blang. *)
  let t = parse {| (name my_project) |} in
  enforce t [ true_ ];
  [%expect {| (name my_project) |}];
  require_does_raise (fun () -> enforce t [ false_ ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition false)) |}];
  enforce
    t
    [ and_
        [ not_ (equals (Dune_project.Name.v "other_project"))
        ; equals (Dune_project.Name.v "my_project")
        ]
    ];
  [%expect {| (name my_project) |}];
  (* [or] does not have an enforcement strategy when its invariant is
     not satisfied. *)
  enforce
    t
    [ or_
        [ equals (Dune_project.Name.v "my_project")
        ; equals (Dune_project.Name.v "other_project")
        ]
    ];
  [%expect {| (name my_project) |}];
  require_does_raise (fun () ->
    enforce
      t
      [ or_
          [ equals (Dune_project.Name.v "other_project")
          ; equals (Dune_project.Name.v "other_project")
          ]
      ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (or (equals other_project) (equals other_project))))
    |}];
  (* When defined, [if] enforces the clause that applies. *)
  let invariant =
    if_
      (equals (Dune_project.Name.v "my_project"))
      (equals (Dune_project.Name.v "other_project"))
      (equals (Dune_project.Name.v "my_project"))
  in
  let t = parse {| (name my_project) |} in
  enforce t [ invariant ];
  [%expect {| (name other_project) |}];
  let t = parse {| (name other_project) |} in
  enforce t [ invariant ];
  [%expect {| (name my_project) |}];
  ()
;;

let%expect_test "Linter.eval" =
  let _, t = parse {| (name foo) |} in
  Test_helpers.is_true
    (Dune_project_linter.Name.Linter.eval
       t
       ~predicate:(`name (equals (Dune_project.Name.v "foo"))));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_project_linter.Name.Linter.eval t ~predicate:(`implicit_transitive_deps true_));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_project_linter.Name.Linter.eval t ~predicate:(`generate_opam_files false_));
  [%expect {||}];
  ()
;;

let%expect_test "Linter.enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_project_linter.Name.Linter.enforce t ~condition);
      Dune_project_linter.Name.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open! Blang.O in
  let t = parse {| (name foo) |} in
  enforce t [];
  [%expect {| (name foo) |}];
  (* Enforcing the presence of the field has no effect if it is already present. *)
  enforce t [ name (equals (Dune_project.Name.v "foo")) ];
  [%expect {| (name foo) |}];
  enforce t [ name (equals (Dune_project.Name.v "bar")) ];
  [%expect {| (name bar) |}];
  (* Enforcing other toplevel stanza has no effect. *)
  enforce t [ implicit_transitive_deps false_ ];
  [%expect {| (name bar) |}];
  enforce t [ generate_opam_files false_ ];
  [%expect {| (name bar) |}];
  enforce t [ not_ (generate_opam_files true_) ];
  [%expect {| (name bar) |}];
  (* Blang. *)
  enforce t [ true_ ];
  [%expect {| (name bar) |}];
  require_does_raise (fun () -> enforce t [ false_ ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition false)) |}];
  require_does_raise (fun () ->
    enforce t [ name (not_ (equals (Dune_project.Name.v "bar"))) ]);
  [%expect
    {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not (equals bar)))) |}];
  enforce
    t
    [ and_
        [ name (equals (Dune_project.Name.v "bar"))
        ; name (equals (Dune_project.Name.v "bar"))
        ]
    ];
  [%expect {| (name bar) |}];
  enforce t [ or_ [ name (is_prefix "baz"); name (is_prefix "bar") ] ];
  [%expect {| (name bar) |}];
  enforce
    t
    [ if_ (name (is_prefix "bar")) (name (is_suffix "bapapa")) (name (is_prefix "foo")) ];
  [%expect {| (name barbapapa) |}];
  ()
;;
