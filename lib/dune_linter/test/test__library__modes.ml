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
      print_s (Dune_linter.Library.Modes.write t))
  in
  test {| (modes) |};
  [%expect {| (modes) |}];
  test {| (invalid field) |};
  [%expect
    {|
    File "dune", line 1, characters 1-16:
    Error: Unexpected [modes] field.
    [123]
    |}];
  test {| (modes byte native) |};
  [%expect {| (modes byte native) |}];
  test {| (modes (best)) |};
  [%expect
    {|
    Internal Error: (Of_sexp_error (_ ((invalid_sexp (best)))))
    <backtrace disabled in tests>
    [125]
    |}];
  test {| (modes byte) |};
  [%expect {| (modes byte) |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (modes byte native) |} in
  print_s [%sexp (t : Dune_linter.Library.Modes.t)];
  [%expect {| ((modes (byte native))) |}];
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
    print_s [%sexp (Dune_linter.Library.Modes.modes t : Dune.Library.Modes.t)];
    [%expect {| (best) |}];
    Dune_linter.Library.Modes.set_modes
      t
      ~modes:(Dune.Library.Modes.of_list [ `byte; `native ]);
    print_s [%sexp (Dune_linter.Library.Modes.modes t : Dune.Library.Modes.t)];
    [%expect {| (byte native) |}];
    ());
  [%expect {| (modes byte native) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune.Library.Modes.Predicate.t as 'a
    constraint
      'a =
      [ `equals of Dune.Library.Modes.t | `has_mode of Dune.Compilation_mode.t ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (modes byte native) |} in
  Test_helpers.is_true
    (Dune_linter.Library.Modes.eval
       t
       ~predicate:(`equals (Dune.Library.Modes.of_list [ `byte; `native ])));
  [%expect {| |}];
  Test_helpers.is_false
    (Dune_linter.Library.Modes.eval
       t
       ~predicate:(`equals (Dune.Library.Modes.of_list [ `best ])));
  [%expect {| |}];
  Test_helpers.is_true (Dune_linter.Library.Modes.eval t ~predicate:(`has_mode `byte));
  [%expect {| |}];
  Test_helpers.is_false (Dune_linter.Library.Modes.eval t ~predicate:(`has_mode `best));
  [%expect {| |}];
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
  (* Enforcing the equality with the current value has no effect. *)
  enforce t [ equals (Dune.Library.Modes.of_list [ `byte; `native ]) ];
  [%expect {| (modes byte native) |}];
  (* Enforcing the equality with a new value changes it. *)
  enforce t [ equals (Dune.Library.Modes.of_list [ `best ]) ];
  [%expect {| (modes best) |}];
  (* Enforcing the non-equality with another value has no effect. *)
  let t = parse {| (modes byte native) |} in
  enforce t [ not_ (equals (Dune.Library.Modes.of_list [ `best ])) ];
  [%expect {| (modes byte native) |}];
  let t = parse {| (modes best) |} in
  (* Enforcing the negation of a current equality triggers an error.
     Dunolint is not going to automatically invent a new setting, this
     requires the user's intervention. *)
  require_does_raise [%here] (fun () ->
    enforce t [ not_ (equals (Dune.Library.Modes.of_list [ `best ])) ]);
  [%expect
    {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not (equals (best))))) |}];
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
  [%expect {| (modes best native) |}];
  enforce t [ not_ (has_mode `best) ];
  [%expect {| (modes native) |}];
  enforce t [ not_ (has_mode `native) ];
  [%expect {| (modes) |}];
  (* Blang. *)
  let t = parse {| (modes native) |} in
  enforce t [ true_ ];
  [%expect {| (modes native) |}];
  require_does_raise [%here] (fun () -> enforce t [ false_ ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc       _)
      (condition false))
    |}];
  enforce t [ and_ [ has_mode `byte; not_ (has_mode `native) ] ];
  [%expect {| (modes byte) |}];
  (* [or] does not have an enforcement strategy when its invariant is
     not satisfied. *)
  enforce t [ or_ [ has_mode `byte; has_mode `native ] ];
  [%expect {| (modes byte) |}];
  require_does_raise [%here] (fun () ->
    enforce t [ or_ [ has_mode `best; has_mode `native ] ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (
        or
        (has_mode best)
        (has_mode native))))
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
  ()
;;

let%expect_test "initialize" =
  let test condition =
    let t = Dune_linter.Library.Modes.initialize ~condition in
    print_s (Dune_linter.Library.Modes.write t)
  in
  test true_;
  [%expect {| (modes best) |}];
  test (equals (Set.empty (module Dune.Compilation_mode)));
  [%expect {| (modes best) |}];
  test (equals (Dune.Library.Modes.of_list [ `byte; `native ]));
  [%expect {| (modes byte native) |}];
  test (has_mode `byte);
  [%expect {| (modes byte) |}];
  ()
;;
