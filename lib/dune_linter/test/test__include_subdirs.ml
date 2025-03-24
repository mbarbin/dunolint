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
  Test_helpers.parse (module Dune_linter.Include_subdirs) ~path:(Fpath.v "dune") contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_linter.Include_subdirs.write t))
  in
  test {| (include_subdirs no) |};
  [%expect {| (include_subdirs no) |}];
  test {| (invalid field) |};
  [%expect
    {|
    File "dune", line 1, characters 1-16:
    Error: Unexpected [Sexp] for field [include_subdirs].
    [123]
    |}];
  test {| (include_subdirs unqualified) |};
  [%expect {| (include_subdirs unqualified) |}];
  test {| (include_subdirs qualified) |};
  [%expect {| (include_subdirs qualified) |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (include_subdirs unqualified) |} in
  print_s [%sexp (t : Dune_linter.Include_subdirs.t)];
  [%expect {| ((mode unqualified)) |}];
  ()
;;

let rewrite ?(f = ignore) str =
  let (sexps_rewriter, field), t = parse str in
  f t;
  Dune_linter.Include_subdirs.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (include_subdirs no) |};
  [%expect {| (include_subdirs no) |}];
  (* Exercising some getters. *)
  rewrite {| (include_subdirs qualified) |} ~f:(fun t ->
    print_s [%sexp (Dune_linter.Include_subdirs.mode t : Dune.Include_subdirs.Mode.t)];
    [%expect {| qualified |}];
    ());
  [%expect {| (include_subdirs qualified) |}];
  (* Exercising some setters. *)
  rewrite {| (include_subdirs qualified) |} ~f:(fun t ->
    Dune_linter.Include_subdirs.set_mode t ~mode:`unqualified;
    ());
  [%expect {| (include_subdirs unqualified) |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_linter.Include_subdirs.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t = Dune_linter.Include_subdirs.create ~mode:`qualified () in
  test t {| (include_subdirs no) |};
  [%expect {| (include_subdirs qualified) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune.Include_subdirs.Predicate.t as 'a
    constraint 'a = [ `equals of Dune.Include_subdirs.Mode.t ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (include_subdirs unqualified) |} in
  Test_helpers.is_true
    (Dune_linter.Include_subdirs.eval t ~predicate:(`equals `unqualified));
  [%expect {| |}];
  Test_helpers.is_false
    (Dune_linter.Include_subdirs.eval t ~predicate:(`equals `qualified));
  [%expect {| |}];
  Test_helpers.is_false (Dune_linter.Include_subdirs.eval t ~predicate:(`equals `no));
  [%expect {| |}];
  let _, t = parse {| (include_subdirs no) |} in
  Test_helpers.is_true (Dune_linter.Include_subdirs.eval t ~predicate:(`equals `no));
  [%expect {| |}];
  ()
;;

let%expect_test "enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_linter.Include_subdirs.enforce t ~condition);
      Dune_linter.Include_subdirs.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  let t = parse {| (include_subdirs unqualified) |} in
  enforce t [];
  [%expect {| (include_subdirs unqualified) |}];
  (* Enforcing the equality with the current value has no effect. *)
  enforce t [ equals `unqualified ];
  [%expect {| (include_subdirs unqualified) |}];
  (* Enforcing the equality with a new value changes it. *)
  enforce t [ equals `qualified ];
  [%expect {| (include_subdirs qualified) |}];
  let t = parse {| (include_subdirs qualified) |} in
  (* Enforcing the negation of the equality with another value has no effect. *)
  enforce t [ not_ (equals `unqualified) ];
  [%expect {| (include_subdirs qualified) |}];
  (* Enforcing the negation of a current equality triggers an error.
     Dunolint is not going to automatically invent a new setting, this
     requires the user's intervention. *)
  require_does_raise [%here] (fun () -> enforce t [ not_ (equals `qualified) ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (not (equals qualified))))
    |}];
  (* Blang. *)
  let t = parse {| (include_subdirs no) |} in
  enforce t [ true_ ];
  [%expect {| (include_subdirs no) |}];
  require_does_raise [%here] (fun () -> enforce t [ false_ ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc       _)
      (condition false))
    |}];
  enforce t [ and_ [ not_ (equals `qualified); equals `unqualified ] ];
  [%expect {| (include_subdirs unqualified) |}];
  (* [or] does not have an enforcement strategy when its invariant is
     not satisfied. *)
  enforce t [ or_ [ equals `no; equals `unqualified ] ];
  [%expect {| (include_subdirs unqualified) |}];
  require_does_raise [%here] (fun () ->
    enforce t [ or_ [ equals `qualified; equals `no ] ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (
        or
        (equals qualified)
        (equals no))))
    |}];
  (* When defined, [if] enforces the clause that applies. *)
  let invariant = if_ (equals `no) (equals `qualified) (equals `unqualified) in
  let t = parse {| (include_subdirs no) |} in
  enforce t [ invariant ];
  [%expect {| (include_subdirs qualified) |}];
  let t = parse {| (include_subdirs unqualified) |} in
  enforce t [ invariant ];
  [%expect {| (include_subdirs unqualified) |}];
  ()
;;
