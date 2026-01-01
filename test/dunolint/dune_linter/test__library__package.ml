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

(* Tests for the [package] field of [library] stanzas.

   This file tests two aspects:

   1. The [Library.Package] sub-module: read/write, predicates (equals,
   is_prefix, is_suffix), and predicate enforcement.

   2. The public API semantics of [Library.create] with the optional
   [~package:Dune.Package.Name.t] parameter, and the [has_field `package]
   predicate.

   Internal representation vs Public API
   -------------------------------------

   Internally, the field uses [Package.t option]:
   - [Some pkg] = field should be set to that package name
   - [None] = field not enforced (check [marked_for_removal] for removal)

   The public API [Library.create ~package] provides two-way semantics:
   - [~package:name] -> [Some (Package.create ~name)] internally (set the field)
   - Not passing [~package] -> [None] (leave existing field unchanged)

   Unlike [inline_tests:bool], there is no way to remove the field via
   [Library.create]. Removal is done via [not (has_field `package)] enforcement.

   Comparison with inline_tests
   ----------------------------

   The [inline_tests] field is a simple flag (present/absent) with no predicates.
   The [package] field is a value field with predicates (equals, is_prefix,
   is_suffix) that can match the package name. *)

let parse contents =
  Test_helpers.parse (module Dune_linter.Library.Package) ~path:(Fpath.v "dune") contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_linter.Library.Package.write t))
  in
  test {| (package my_pkg) |};
  [%expect {| (package my_pkg) |}];
  test {| (package (foo bar)) |};
  [%expect
    {|
    Internal Error: (Of_sexp_error (_ ((invalid_sexp (foo bar)))))
    <backtrace disabled in tests>
    [125]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (package my_package) |} in
  print_s [%sexp (t : Dune_linter.Library.Package.t)];
  [%expect {| ((name my_package)) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune.Library.Package.Predicate.t as 'a
    constraint
      'a =
      [ `equals of Dune.Package.Name.t | `is_prefix of string | `is_suffix of string ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (package pre_hello_suf) |} in
  Test_helpers.is_true
    (Dune_linter.Library.Package.eval
       t
       ~predicate:(`equals (Dune.Package.Name.v "pre_hello_suf")));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Library.Package.eval
       t
       ~predicate:(`equals (Dune.Package.Name.v "hello_suf")));
  [%expect {||}];
  Test_helpers.is_true (Dune_linter.Library.Package.eval t ~predicate:(`is_prefix "pre_"));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Library.Package.eval t ~predicate:(`is_prefix "hello"));
  [%expect {||}];
  Test_helpers.is_true (Dune_linter.Library.Package.eval t ~predicate:(`is_suffix "_suf"));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Library.Package.eval t ~predicate:(`is_suffix "hello"));
  [%expect {||}];
  ()
;;

let%expect_test "enforce" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let sexps_rewriter, field = Common.read {| (package pre_hello_suf) |} in
  let enforce conditions =
    Sexps_rewriter.reset sexps_rewriter;
    let t = Dune_linter.Library.Package.read ~sexps_rewriter ~field in
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_linter.Library.Package.enforce t ~condition);
      Dune_linter.Library.Package.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  enforce [];
  [%expect {| (package pre_hello_suf) |}];
  (* Enforcing the equality with the current value has no effect. *)
  enforce [ equals (Dune.Package.Name.v "pre_hello_suf") ];
  [%expect {| (package pre_hello_suf) |}];
  (* Enforcing the equality with a new value changes it. *)
  enforce [ equals (Dune.Package.Name.v "new_name") ];
  [%expect {| (package new_name) |}];
  (* Enforcing the non-equality with another value has no effect. *)
  enforce [ not_ (equals (Dune.Package.Name.v "not_equal")) ];
  [%expect {| (package pre_hello_suf) |}];
  require_does_raise (fun () ->
    enforce [ not_ (equals (Dune.Package.Name.v "pre_hello_suf")) ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (not (equals pre_hello_suf))))
    |}];
  (* Setting a prefix to an existing prefix has no effect. *)
  enforce [ is_prefix "pre_" ];
  [%expect {| (package pre_hello_suf) |}];
  enforce [ not_ (is_prefix "pre_"); not_ (is_suffix "_suf") ];
  [%expect {| (package hello) |}];
  enforce [ not_ (is_prefix "not_a_prefix"); not_ (is_suffix "not_a_suffix") ];
  [%expect {| (package pre_hello_suf) |}];
  enforce [ is_prefix "prefix_" ];
  [%expect {| (package prefix_pre_hello_suf) |}];
  enforce [ is_suffix "suf" ];
  [%expect {| (package pre_hello_suf) |}];
  enforce [ is_suffix "_suf_suf" ];
  [%expect {| (package pre_hello_suf_suf_suf) |}];
  enforce [ not_ (is_suffix "_suf"); is_suffix "_suf_suf" ];
  [%expect {| (package pre_hello_suf_suf) |}];
  (* Blang. *)
  enforce [ true_ ];
  [%expect {| (package pre_hello_suf) |}];
  require_does_raise (fun () -> enforce [ false_ ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition false)) |}];
  enforce [ and_ [ not_ (is_prefix "pre_"); not_ (is_suffix "_suf") ] ];
  [%expect {| (package hello) |}];
  ()
;;

(* Note: Unlike public_name which uses dots as package/library separators
   (e.g., "pkg.mylib"), package names cannot contain dots - only alphanumeric,
   underscore, and dash characters are allowed. Therefore, the special
   [Dunolinter.Linter.public_name_is_prefix] logic that replaces existing
   package prefixes when the enforced prefix ends with "." does not apply
   to package names. The is_prefix enforcement for packages simply prepends. *)
let%expect_test "enforce - is_prefix simply prepends" =
  let sexps_rewriter, field = Common.read {| (package old-pkg) |} in
  let enforce conditions =
    Sexps_rewriter.reset sexps_rewriter;
    let t = Dune_linter.Library.Package.read ~sexps_rewriter ~field in
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_linter.Library.Package.enforce t ~condition);
      Dune_linter.Library.Package.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  enforce [ Blang.base (`is_prefix "new-") ];
  [%expect {| (package new-old-pkg) |}];
  ()
;;

(* -------------------------------------------------------------------------- *)
(* Tests for [Library.create] with [~package] and [has_field `package]        *)
(* -------------------------------------------------------------------------- *)

let parse_library str =
  Test_helpers.parse (module Dune_linter.Library) ~path:(Fpath.v "dune") str
;;

let rewrite t str =
  let sexps_rewriter, field = Common.read str in
  Dune_linter.Library.rewrite t ~sexps_rewriter ~field;
  print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
;;

let%expect_test "create - not passing package" =
  (* When not passing [~package], existing field is left unchanged. *)
  let t = Dune_linter.Library.create () in
  rewrite t {| (library (name main)) |};
  [%expect {| (library (name main)) |}];
  rewrite t {| (library (name main) (package my-pkg)) |};
  [%expect {| (library (name main) (package my-pkg)) |}]
;;

let%expect_test "create - passing package" =
  (* When passing [~package], the field is set to that value. *)
  let t = Dune_linter.Library.create ~package:(Dune.Package.Name.v "new-pkg") () in
  rewrite t {| (library (name main)) |};
  [%expect {| (library (name main) (package new-pkg)) |}];
  (* If already present, it is changed to the new value. *)
  rewrite t {| (library (name main) (package old-pkg)) |};
  [%expect {| (library (name main) (package new-pkg)) |}]
;;

let enforce_library ((sexps_rewriter, field), t) conditions =
  Sexps_rewriter.reset sexps_rewriter;
  Dunolinter.Handler.raise ~f:(fun () ->
    List.iter conditions ~f:(fun condition -> Dune_linter.Library.enforce t ~condition);
    Dune_linter.Library.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
;;

let%expect_test "enforce - has_field package (cannot add without value)" =
  (* Enforcing [has_field package] does NOT add the field because there's no
     default value to use. Unlike [inline_tests] which is a simple flag,
     [package] requires a specific package name. *)
  let t = parse_library {| (library (name my_lib)) |} in
  require_does_raise (fun () -> enforce_library t [ has_field `package ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _) (condition (has_field package)))
    |}];
  (* Does nothing if already present. *)
  let t = parse_library {| (library (name my_lib) (package my-pkg)) |} in
  enforce_library t [ has_field `package ];
  [%expect {| (library (name my_lib) (package my-pkg)) |}]
;;

let%expect_test "enforce - not (has_field package) (remove)" =
  (* Enforcing [not (has_field package)] removes the field if present. *)
  let t = parse_library {| (library (name my_lib)) |} in
  enforce_library t [ not_ (has_field `package) ];
  [%expect {| (library (name my_lib)) |}];
  let t = parse_library {| (library (name my_lib) (package my-pkg)) |} in
  enforce_library t [ not_ (has_field `package) ];
  [%expect {| (library (name my_lib)) |}]
;;
