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
