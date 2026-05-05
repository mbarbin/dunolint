(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

module type Roundtripable = sig
  type t

  val equal : t -> t -> bool
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

let test_roundtrip (type a) (module M : Roundtripable with type t = a) (a : a) =
  let sexp = [%sexp (a : M.t)] in
  let a' = M.t_of_sexp sexp in
  require_equal (module M) a a';
  print_s sexp;
  ()
;;

module type Predicate = sig
  type t

  val equal : t -> t -> bool
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

let test_predicate (type a) (module M : Predicate with type t = a) predicate =
  let module B = struct
    type t = M.t Blang.t

    let equal t1 t2 = Blang.equal M.equal t1 t2
    let sexp_of_t t = Blang.sexp_of_t M.sexp_of_t t
    let t_of_sexp sexp = Blang.t_of_sexp M.t_of_sexp sexp
  end
  in
  test_roundtrip (module B) predicate;
  ()
;;
