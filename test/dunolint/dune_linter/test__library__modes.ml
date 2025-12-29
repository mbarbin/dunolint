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
  Test_helpers.parse (module Dune_linter.Library.Modes) ~path:(Fpath.v "dune") contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s
        [%sexp
          { t : Dune_linter.Library.Modes.t
          ; field = (Dune_linter.Library.Modes.write t : Sexp.t)
          }])
  in
  test {| (modes) |};
  [%expect {| ((t ((modes (Union ())))) (field (modes))) |}];
  test {| (invalid field) |};
  [%expect
    {|
    File "dune", line 1, characters 1-16:
    Error: Unexpected [modes] field.
    [123]
    |}];
  test {| (modes byte native) |};
  [%expect
    {|
    ((t ((modes (Union ((Element byte) (Element native))))))
     (field (modes byte native)))
    |}];
  test {| (modes (best)) |};
  [%expect {| ((t ((modes (Element best)))) (field (modes best))) |}];
  test {| (modes byte) |};
  [%expect {| ((t ((modes (Element byte)))) (field (modes byte))) |}];
  test {| (modes ()) |};
  [%expect {| ((t ((modes (Union ())))) (field (modes))) |}];
  test {| (modes :standard) |};
  [%expect {| ((t ((modes Standard))) (field (modes :standard))) |}];
  test {| (modes :include) |};
  [%expect
    {|
    Internal Error: (Of_sexp_error (_ ((invalid_sexp :include))))
    <backtrace disabled in tests>
    [125]
    |}];
  test {| (modes (:include foo.txt)) |};
  [%expect {| ((t ((modes (Include foo.txt)))) (field (modes :include foo.txt))) |}];
  test {| (modes :standard \ byte) |};
  [%expect
    {|
    ((t ((modes (Diff Standard (Element byte)))))
     (field (modes :standard "\\" byte)))
    |}];
  test {| (modes :standard melange) |};
  [%expect
    {|
    ((t ((modes (Union (Standard (Element melange))))))
     (field (modes :standard melange)))
    |}];
  test {| (modes (:standard melange)) |};
  [%expect
    {|
    ((t ((modes (Union (Standard (Element melange))))))
     (field (modes :standard melange)))
    |}];
  test {| (modes :standard melange \ byte native) |};
  [%expect
    {|
    ((t
      ((modes
        (Diff (Union (Standard (Element melange)))
         (Union ((Element byte) (Element native)))))))
     (field (modes (:standard melange) "\\" (byte native))))
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (modes byte native) |} in
  print_s [%sexp (t : Dune_linter.Library.Modes.t)];
  [%expect {| ((modes (Union ((Element byte) (Element native))))) |}];
  ()
;;

let rewrite ?(f = ignore) str =
  let (sexps_rewriter, field), t = parse str in
  f t;
  Dune_linter.Library.Modes.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (modes) |};
  [%expect {| (modes) |}];
  rewrite {| (modes byte) |};
  [%expect {| (modes byte) |}];
  (* Exercising some getters and setters. *)
  rewrite {| (modes best) |} ~f:(fun t ->
    print_s (Dune_linter.Library.Modes.write t);
    [%expect {| (modes best) |}];
    Dune_linter.Library.Modes.set_modes
      t
      ~modes:(Dunolinter.Ordered_set.of_list [ `byte; `native ]);
    print_s (Dune_linter.Library.Modes.write t);
    [%expect {| (modes byte native) |}];
    let modes = Dune_linter.Library.Modes.modes t in
    print_s [%sexp (modes : Dune_linter.Library.Modes.Ordered_set.t)];
    [%expect {| (Union ((Element byte) (Element native))) |}];
    ());
  [%expect {| (modes byte native) |}];
  ()
;;

let%expect_test "sort" =
  (* The order in use to write the values is deterministic and the values are
     sorted by dunolint when the field is linted. *)
  rewrite {| (modes best native byte) |};
  [%expect {| (modes byte native best) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune.Library.Modes.Predicate.t as 'a
    constraint
      'a =
      [ `has_mode of Dune.Compilation_mode.t
      | `has_modes of Dune.Compilation_mode.t list
      ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (modes byte native) |} in
  Test_helpers.is_true
    (Dune_linter.Library.Modes.eval t ~predicate:(`has_modes [ `byte; `native ]));
  [%expect {||}];
  Test_helpers.is_false (Dune_linter.Library.Modes.eval t ~predicate:(`has_mode `best));
  [%expect {||}];
  Test_helpers.is_true (Dune_linter.Library.Modes.eval t ~predicate:(`has_mode `byte));
  [%expect {||}];
  Test_helpers.is_false (Dune_linter.Library.Modes.eval t ~predicate:(`has_mode `best));
  [%expect {||}];
  let _, t = parse {| (modes :standard byte) |} in
  Test_helpers.is_false (Dune_linter.Library.Modes.eval t ~predicate:(`has_mode `native));
  [%expect {||}];
  ()
;;

let%expect_test "enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_linter.Library.Modes.enforce t ~condition);
      Dune_linter.Library.Modes.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  let t = parse {| (modes) |} in
  enforce t [];
  [%expect {| (modes) |}];
  let t = parse {| (modes byte native) |} in
  enforce t [];
  [%expect {| (modes byte native) |}];
  (* equals *)
  (* Enforcing the presence of modes already present has no effect. *)
  enforce t [ has_modes [ `byte; `native ] ];
  [%expect {| (modes byte native) |}];
  (* Enforcing the presence a new value adds it. *)
  enforce t [ has_modes [ `best ] ];
  [%expect {| (modes byte native best) |}];
  (* Enforcing the negation of a mode non-present has no effect. *)
  let t = parse {| (modes byte native) |} in
  enforce t [ not_ (has_modes [ `best ]) ];
  [%expect {| (modes byte native) |}];
  (* Enforcing the negation of mixed present and non present mode removes the present ones. *)
  let t = parse {| (modes byte native) |} in
  enforce t [ not_ (has_modes [ `best; `native ]) ];
  [%expect {| (modes byte) |}];
  (* has_mode *)
  let t = parse {| (modes native) |} in
  (* Enforcing [has_mode] adds the mode if it is missing. *)
  enforce t [ has_mode `byte ];
  [%expect {| (modes byte native) |}];
  (* It does nothing if the mode is already present. *)
  enforce t [ has_mode `byte ];
  [%expect {| (modes byte native) |}];
  (* Enforcing the negation of [hash_mode] removes the mode if present. *)
  enforce t [ not_ (has_mode `native) ];
  [%expect {| (modes byte) |}];
  (* And does nothing if the mode is already absent. *)
  enforce t [ not_ (has_mode `native) ];
  [%expect {| (modes byte) |}];
  enforce t [ not_ (has_mode `byte) ];
  [%expect {| (modes) |}];
  enforce t [ has_mode `native ];
  [%expect {| (modes native) |}];
  enforce t [ has_mode `best ];
  [%expect {| (modes native best) |}];
  enforce t [ not_ (has_mode `best) ];
  [%expect {| (modes native) |}];
  enforce t [ not_ (has_mode `native) ];
  [%expect {| (modes) |}];
  (* Blang. *)
  let t = parse {| (modes native) |} in
  enforce t [ true_ ];
  [%expect {| (modes native) |}];
  require_does_raise (fun () -> enforce t [ false_ ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition false)) |}];
  enforce t [ and_ [ has_mode `byte; not_ (has_mode `native) ] ];
  [%expect {| (modes byte) |}];
  (* [or] does not have an enforcement strategy when its invariant is
     not satisfied. *)
  enforce t [ or_ [ has_mode `byte; has_mode `native ] ];
  [%expect {| (modes byte) |}];
  require_does_raise (fun () -> enforce t [ or_ [ has_mode `best; has_mode `native ] ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (or (has_mode best) (has_mode native))))
    |}];
  (* When defined, [if] enforces the clause that applies. *)
  let invariant =
    if_
      (has_mode `best)
      (and_ [ not_ (has_mode `byte); not_ (has_mode `native) ])
      (has_mode `byte)
  in
  let t = parse {| (modes native) |} in
  enforce t [ invariant ];
  [%expect {| (modes byte native) |}];
  let t = parse {| (modes best native) |} in
  enforce t [ invariant ];
  [%expect {| (modes best) |}];
  (* Presence unknown statically. *)
  let t = parse {| (modes :standard) |} in
  (* Enforcing [has_mode] adds the mode if it is missing. *)
  enforce t [ has_mode `byte ];
  [%expect {| (modes :standard byte) |}];
  (* It shall not fail when attempting to remove a mode not known to be present
     after evaluation. *)
  let t = parse {| (modes :standard) |} in
  enforce t [ not_ (has_mode `byte) ];
  [%expect {| (modes :standard) |}];
  let t = parse {| (modes :standard byte) |} in
  enforce t [ not_ (has_mode `byte) ];
  [%expect {| (modes :standard) |}];
  ()
;;

let%expect_test "initialize" =
  let test condition =
    let t = Dune_linter.Library.Modes.initialize ~condition in
    print_s (Dune_linter.Library.Modes.write t)
  in
  test true_;
  [%expect {| (modes best) |}];
  test (has_modes []);
  [%expect {| (modes best) |}];
  test (has_modes [ `byte; `native ]);
  [%expect {| (modes byte native) |}];
  test (has_mode `byte);
  [%expect {| (modes byte) |}];
  ()
;;
