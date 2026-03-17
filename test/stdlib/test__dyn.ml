(***************************************************************************************)
(*  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(***************************************************************************************)

let%expect_test "inline_record" =
  print_dyn (Dyn.inline_record "Foo" [ "x", Dyn.int 1; "y", Dyn.string "hello" ]);
  [%expect {| Foo { x = 1; y = "hello" } |}];
  ()
;;

let%expect_test "inline_record - single field" =
  print_dyn (Dyn.inline_record "Bar" [ "name", Dyn.string "test" ]);
  [%expect {| Bar { name = "test" } |}];
  ()
;;

let%expect_test "inline_record - empty fields" =
  print_dyn (Dyn.inline_record "Empty" []);
  [%expect {| Empty {} |}];
  ()
;;

let%expect_test "stringable" =
  let module M = struct
    type t = int

    let to_string = Int.to_string
  end
  in
  print_dyn (Dyn.stringable (module M) 42);
  [%expect {| "42" |}];
  ()
;;

let%expect_test "stringable - empty string" =
  let module M = struct
    type t = string

    let to_string s = s
  end
  in
  print_dyn (Dyn.stringable (module M) "");
  [%expect {| "" |}];
  ()
;;

let%expect_test "stringable - complex value" =
  let module M = struct
    type t = float

    let to_string = Float.to_string
  end
  in
  print_dyn (Dyn.stringable (module M) 3.14);
  [%expect {| "3.14" |}];
  ()
;;
