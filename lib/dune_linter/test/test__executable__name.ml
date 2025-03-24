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
  Test_helpers.parse (module Dune_linter.Executable.Name) ~path:(Fpath.v "dune") contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_linter.Executable.Name.write t))
  in
  test {| (name pre_hello_suf) |};
  [%expect {| (name pre_hello_suf) |}];
  test {| (name (pre (hello_suf))) |};
  [%expect
    {|
    Internal Error: (Of_sexp_error (_ ((invalid_sexp (pre (hello_suf))))))
    <backtrace disabled in tests>
    [125]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (name exe_name) |} in
  print_s [%sexp (t : Dune_linter.Executable.Name.t)];
  [%expect {| ((name exe_name)) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune.Executable.Name.Predicate.t as 'a
    constraint
      'a =
      [ `equals of Dune.Executable.Name.t | `is_prefix of string | `is_suffix of string ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (name pre_hello_suf) |} in
  Test_helpers.is_true
    (Dune_linter.Executable.Name.eval
       t
       ~predicate:(`equals (Dune.Executable.Name.v "pre_hello_suf")));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Executable.Name.eval
       t
       ~predicate:(`equals (Dune.Executable.Name.v "hello_suf")));
  [%expect {||}];
  Test_helpers.is_true (Dune_linter.Executable.Name.eval t ~predicate:(`is_prefix "pre_"));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Executable.Name.eval t ~predicate:(`is_prefix "hello"));
  [%expect {||}];
  Test_helpers.is_true (Dune_linter.Executable.Name.eval t ~predicate:(`is_suffix "_suf"));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Executable.Name.eval t ~predicate:(`is_suffix "hello"));
  [%expect {||}];
  ()
;;

let%expect_test "enforce" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_linter.Executable.Name.enforce t ~condition);
      Dune_linter.Executable.Name.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  let t = parse {| (name exe_name) |} in
  enforce t [];
  [%expect {| (name exe_name) |}];
  (* Enforcing the equality with the current value has no effect. *)
  enforce t [ equals (Dune.Executable.Name.v "exe_name") ];
  [%expect {| (name exe_name) |}];
  (* Enforcing the equality with a new value changes it. *)
  enforce t [ equals (Dune.Executable.Name.v "new_name") ];
  [%expect {| (name new_name) |}];
  (* Enforcing the non-equality with another value has no effect. *)
  let t = parse {| (name exe_name) |} in
  enforce t [ not_ (equals (Dune.Executable.Name.v "not_equal")) ];
  [%expect {| (name exe_name) |}];
  require_does_raise [%here] (fun () ->
    enforce t [ not_ (equals (Dune.Executable.Name.v "exe_name")) ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (not (equals exe_name))))
    |}];
  (* Setting a prefix to an existing prefix has no effect. *)
  let t = parse {| (name pre_hello_suf) |} in
  enforce t [ is_prefix "pre_" ];
  [%expect {| (name pre_hello_suf) |}];
  enforce t [ not_ (is_prefix "pre_"); not_ (is_suffix "_suf") ];
  [%expect {| (name hello) |}];
  enforce t [ not_ (is_prefix "not_a_prefix"); not_ (is_suffix "not_a_suffix") ];
  [%expect {| (name hello) |}];
  enforce t [ is_prefix "prefix_" ];
  [%expect {| (name prefix_hello) |}];
  (* Prefixing by a package name is allowed. *)
  let t = parse {| (name hello) |} in
  enforce t [ is_prefix "priv." ];
  [%expect {| (name priv.hello) |}];
  (* Multiple invariants may be enforced one by one. *)
  let t = parse {| (name hello) |} in
  enforce t [ is_prefix "world_"; is_prefix "hello_" ];
  [%expect {| (name hello_world_hello) |}];
  (* However, note that doing something like the above (adding two conflicting
     prefix in a row) doesn't stabilize to a fix point. It is possible some
     future version of dunolint reject such enforce conditions, if they don't
     stabilize (left as future work). *)
  let t = parse {| (name hello) |} in
  enforce t [ is_suffix "_suf" ];
  [%expect {| (name hello_suf) |}];
  (* The logic doesn't try to apply the shortest suffix to make the predicate
     true. If this isn't the one the user is looking for, they can simply amend
     manually into another name that is stable across such invariant. *)
  enforce t [ is_suffix "_suf_suf" ];
  [%expect {| (name hello_suf_suf_suf) |}];
  enforce t [ not_ (is_suffix "_suf"); is_suffix "_suf_suf" ];
  [%expect {| (name hello_suf_suf) |}];
  (* Blang. *)
  let t = parse {| (name pre_hello_suf) |} in
  enforce t [ true_ ];
  [%expect {| (name pre_hello_suf) |}];
  require_does_raise [%here] (fun () -> enforce t [ false_ ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc       _)
      (condition false))
    |}];
  enforce t [ and_ [ not_ (is_prefix "pre_"); not_ (is_suffix "_suf") ] ];
  [%expect {| (name hello) |}];
  (* [or] does not have an enforcement strategy when its invariant is not
     satisfied. *)
  let t = parse {| (name pre_hello_suf) |} in
  enforce t [ or_ [ is_prefix "pre_"; equals (Dune.Executable.Name.v "not_equal") ] ];
  [%expect {| (name pre_hello_suf) |}];
  require_does_raise [%here] (fun () ->
    enforce t [ or_ [ is_prefix "prefix_"; equals (Dune.Executable.Name.v "not_equal") ] ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (
        or
        (is_prefix prefix_)
        (equals    not_equal))))
    |}];
  (* When defined, [if] enforces the clause that applies. *)
  enforce
    t
    [ if_
        (is_prefix "false")
        (not_ (is_prefix "pre_"))
        (equals (Dune.Executable.Name.v "not_equal"))
    ];
  [%expect {| (name not_equal) |}];
  let t = parse {| (name pre_hello_suf) |} in
  enforce
    t
    [ if_
        (is_prefix "pre_")
        (not_ (is_prefix "pre_"))
        (equals (Dune.Executable.Name.v "not_equal"))
    ];
  [%expect {| (name hello_suf) |}];
  ()
;;
