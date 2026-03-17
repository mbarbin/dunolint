(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

module Comment_handler = Dunolinter.Comment_handler

let%expect_test "extended_range" =
  let test original_contents ~range =
    let { Loc.Range.start; stop } =
      Comment_handler.extended_range ~original_contents ~range
    in
    print_s [%sexp (String.sub original_contents ~pos:start ~len:(stop - start) : string)]
  in
  test "foo" ~range:{ start = 0; stop = 3 };
  [%expect {| foo |}];
  test "foo     " ~range:{ start = 0; stop = 3 };
  [%expect {| "foo     " |}];
  test "foo     ; Hello comment" ~range:{ start = 0; stop = 3 };
  [%expect {| "foo     ; Hello comment" |}];
  test "foo     \t; Hello comment" ~range:{ start = 0; stop = 3 };
  [%expect {| "foo     \t; Hello comment" |}];
  test "foo     ; Hello comment\n; And new line" ~range:{ start = 0; stop = 3 };
  [%expect {| "foo     ; Hello comment" |}];
  ()
;;
