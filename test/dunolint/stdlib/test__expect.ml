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

let%expect_test "require_does_raise" =
  require_does_raise (fun () ->
    Code_error.raise "Hello Raise" [ "with", Atom "Code_error.raise" ]);
  [%expect {| ("(\"Hello Raise\" (with Code_error.raise))") |}];
  ()
;;

let%expect_test "require" =
  require true;
  [%expect {||}];
  require_does_raise (fun () -> require false);
  [%expect {| (Failure "Required condition does not hold.") |}];
  ()
;;

let%expect_test "Of_sexp_error" =
  require_does_raise (fun () ->
    raise
      (Sexplib0.Sexp_conv.Of_sexp_error (Not_found, List [ Atom "not"; Atom "found" ])));
  [%expect {| (Of_sexp_error Not_found (invalid_sexp (not found))) |}];
  ()
;;

let%expect_test "require_does_raise did not raise" =
  (match require_does_raise ignore with
   | () -> assert false
   | exception exn -> print_string (Printexc.to_string exn));
  [%expect {| Failure("Did not raise.") |}];
  ()
;;

module Int = struct
  type t = int

  let equal = Int.equal
  let sexp_of_t t = Sexp.Atom (Int.to_string t)
end

let%expect_test "require_equal not equal" =
  (match require_equal (module Int) 0 42 with
   | () -> assert false
   | exception exn -> print_string (Printexc.to_string exn));
  [%expect {| ("Values are not equal." (v1 0) (v2 42)) |}];
  ()
;;
