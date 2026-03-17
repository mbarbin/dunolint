(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open Dunolint.Std

let%expect_test "Predicate.equal" =
  let equal = Dune.Stanza.Predicate.equal in
  (* Structural equality - same variant. *)
  require (equal `include_subdirs `include_subdirs);
  [%expect {||}];
  require (equal `library `library);
  [%expect {||}];
  require (equal `executable `executable);
  [%expect {||}];
  require (equal `executables `executables);
  [%expect {||}];
  (* Different variants. *)
  require (not (equal `include_subdirs `library));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

module Predicate = struct
  type t =
    [ `include_subdirs
    | `library
    | `executable
    | `executables
    ]
    constraint t = Dune.Stanza.Predicate.t
  [@@deriving enumerate]
end

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Stanza.Predicate) p in
  List.iter Predicate.all ~f:(fun predicate -> test (Blang.base predicate));
  [%expect
    {|
    include_subdirs
    library
    executable
    executables
    |}];
  ()
;;
