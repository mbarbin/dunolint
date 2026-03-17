(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
