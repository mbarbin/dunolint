(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
