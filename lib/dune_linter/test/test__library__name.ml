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

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let sexps_rewriter, field = Common.read contents in
      let t = Dune_linter.Library.Name.read ~sexps_rewriter ~field in
      print_s (Dune_linter.Library.Name.write t))
  in
  test {| (name pre_hello_suf) |};
  [%expect {| (name pre_hello_suf) |}];
  test {| (name (pre (hello_suf))) |};
  [%expect
    {|
    Internal Error:
    (Of_sexp_error "string_of_sexp: atom needed"
      (invalid_sexp (pre (hello_suf))))
    <backtrace disabled in tests>
    [125]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let sexps_rewriter, field = Common.read {| (name lib_name) |} in
  let t = Dune_linter.Library.Name.read ~sexps_rewriter ~field in
  print_s [%sexp (t : Dune_linter.Library.Name.t)];
  [%expect {| ((name lib_name)) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune.Library.Name.Predicate.t as 'a
    constraint
      'a =
      [ `equals of Dune.Library.Name.t | `is_prefix of string | `is_suffix of string ]
end

open Dunolint.Config.Std

let is_true b = require_equal [%here] (module Dunolint.Trilang) b True
let is_false b = require_equal [%here] (module Dunolint.Trilang) b False

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let sexps_rewriter, field = Common.read {| (name pre_hello_suf) |} in
  let t = Dune_linter.Library.Name.read ~sexps_rewriter ~field in
  is_true
    (Dune_linter.Library.Name.eval
       t
       ~predicate:(`equals (Dune.Library.Name.v "pre_hello_suf")));
  [%expect {||}];
  is_false
    (Dune_linter.Library.Name.eval
       t
       ~predicate:(`equals (Dune.Library.Name.v "hello_suf")));
  [%expect {||}];
  is_true (Dune_linter.Library.Name.eval t ~predicate:(`is_prefix "pre_"));
  [%expect {||}];
  is_false (Dune_linter.Library.Name.eval t ~predicate:(`is_prefix "hello"));
  [%expect {||}];
  is_true (Dune_linter.Library.Name.eval t ~predicate:(`is_suffix "_suf"));
  [%expect {||}];
  is_false (Dune_linter.Library.Name.eval t ~predicate:(`is_suffix "hello"));
  [%expect {||}];
  ()
;;

let%expect_test "enforce" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let sexps_rewriter, field = Common.read {| (name pre_hello_suf) |} in
  let enforce conditions =
    Sexps_rewriter.reset sexps_rewriter;
    let t = Dune_linter.Library.Name.read ~sexps_rewriter ~field in
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_linter.Library.Name.enforce t ~condition);
      Dune_linter.Library.Name.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  enforce [];
  [%expect {| (name pre_hello_suf) |}];
  (* Enforcing the equality with the current value has no effect. *)
  enforce [ equals (Dune.Library.Name.v "pre_hello_suf") ];
  [%expect {| (name pre_hello_suf) |}];
  (* Enforcing the equality with a new value changes it. *)
  enforce [ equals (Dune.Library.Name.v "new_name") ];
  [%expect {| (name new_name) |}];
  (* Enforcing the non-equality with another value has no effect. *)
  enforce [ not_ (equals (Dune.Library.Name.v "not_equal")) ];
  [%expect {| (name pre_hello_suf) |}];
  require_does_raise [%here] (fun () ->
    enforce [ not_ (equals (Dune.Library.Name.v "pre_hello_suf")) ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (not (equals pre_hello_suf))))
    |}];
  (* Setting a prefix to an existing prefix has no effect. *)
  enforce [ is_prefix "pre_" ];
  [%expect {| (name pre_hello_suf) |}];
  enforce [ not_ (is_prefix "pre_"); not_ (is_suffix "_suf") ];
  [%expect {| (name hello) |}];
  enforce [ not_ (is_prefix "not_a_prefix"); not_ (is_suffix "not_a_suffix") ];
  [%expect {| (name pre_hello_suf) |}];
  enforce [ is_prefix "prefix_" ];
  [%expect {| (name prefix_pre_hello_suf) |}];
  (* Prefixing by a package name is allowed. *)
  enforce [ is_prefix "priv." ];
  [%expect {| (name priv.pre_hello_suf)|}];
  (* Multiple invariants may be enforced one by one. *)
  enforce [ is_prefix "world_"; is_prefix "hello_" ];
  [%expect {| (name hello_world_pre_hello_suf) |}];
  (* However, note that doing something like the above (adding two
     conflicting prefix in a row) doesn't stabilize to a fix point. It
     is possible some future version of dunolint reject such enforce
     conditions, if they don't stabilize (left as future work). *)
  enforce [ is_suffix "suf" ];
  [%expect {| (name pre_hello_suf) |}];
  (* The logic doesn't try to apply the shortest suffix to make the predicate
     true. If this isn't the one the user is looking for, they can simply amend
     manually into another name that is stable across such invariant. *)
  enforce [ is_suffix "_suf_suf" ];
  [%expect {| (name pre_hello_suf_suf_suf) |}];
  enforce [ not_ (is_suffix "_suf"); is_suffix "_suf_suf" ];
  [%expect {| (name pre_hello_suf_suf) |}];
  (* Blang. *)
  enforce [ true_ ];
  [%expect {| (name pre_hello_suf) |}];
  require_does_raise [%here] (fun () -> enforce [ false_ ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc       _)
      (condition false))
    |}];
  enforce [ and_ [ not_ (is_prefix "pre_"); not_ (is_suffix "_suf") ] ];
  [%expect {| (name hello) |}];
  (* [or] does not have an enforcement strategy when its invariant is
     not satisfied. *)
  enforce [ or_ [ is_prefix "pre_"; equals (Dune.Library.Name.v "not_equal") ] ];
  [%expect {| (name pre_hello_suf) |}];
  require_does_raise [%here] (fun () ->
    enforce [ or_ [ is_prefix "prefix_"; equals (Dune.Library.Name.v "not_equal") ] ]);
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
    [ if_
        (is_prefix "false")
        (not_ (is_prefix "pre_"))
        (equals (Dune.Library.Name.v "not_equal"))
    ];
  [%expect {| (name not_equal) |}];
  enforce
    [ if_
        (is_prefix "pre_")
        (not_ (is_prefix "pre_"))
        (equals (Dune.Library.Name.v "not_equal"))
    ];
  [%expect {| (name hello_suf) |}];
  ()
;;
