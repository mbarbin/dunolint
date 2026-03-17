(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

module type Roundtripable = sig
  type t [@@deriving equal, sexp]
end

let test_roundtrip (type a) (module M : Roundtripable with type t = a) (a : a) =
  let sexp = [%sexp (a : M.t)] in
  let a' = [%of_sexp: M.t] sexp in
  require_equal (module M) a a';
  print_s sexp;
  ()
;;

module type Predicate = sig
  type t [@@deriving equal, sexp]
end

let test_predicate (type a) (module M : Predicate with type t = a) predicate =
  let module B = struct
    type t = M.t Blang.t [@@deriving equal, sexp]
  end
  in
  test_roundtrip (module B) predicate;
  ()
;;
