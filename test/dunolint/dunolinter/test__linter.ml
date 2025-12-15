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

let%expect_test "public_name_is_prefix" =
  let test a ~prefix =
    if String.is_prefix a ~prefix
    then failwith "Invalid precondition for [public_name_is_prefix]" [@coverage off];
    let result = Dunolinter.Linter.public_name_is_prefix a ~prefix in
    assert (String.is_prefix result ~prefix);
    print_endline result
  in
  test "" ~prefix:"hello";
  [%expect {| hello |}];
  test "world" ~prefix:"hello_";
  [%expect {| hello_world |}];
  (* The function is meant to help prefixing libraries by package names. *)
  test "my-lib" ~prefix:"my-package.";
  [%expect {| my-package.my-lib |}];
  (* In particular, it replaces any existing package: *)
  let package_prefix = "my-other-package." in
  test "my-package.my-lib" ~prefix:package_prefix;
  [%expect {| my-other-package.my-lib |}];
  (* Rather than naively producing a result that would satisfy the invariant,
     but is probably not what you want. *)
  let naive_result = package_prefix ^ "my-package.my-lib" in
  assert (String.is_prefix naive_result ~prefix:package_prefix);
  print_endline naive_result;
  [%expect {| my-other-package.my-package.my-lib |}];
  (* If this is really what you want, you can always manually adjust as needed. *)
  ()
;;
