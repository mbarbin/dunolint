(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

module Trilang = struct
  module T0 = struct
    [@@@coverage off]

    type t = Dunolint.Trilang.t =
      | True
      | False
      | Undefined

    let equal t1 t2 =
      match t1 with
      | True | False | Undefined -> Repr.equal t1 t2
    ;;

    let sexp_of_t : t -> Sexp.t = function
      | True -> Atom "True"
      | False -> Atom "False"
      | Undefined -> Atom "Undefined"
    ;;

    let t_of_sexp : Sexp.t -> t = function
      | Atom "True" -> True
      | Atom "False" -> False
      | Atom "Undefined" -> Undefined
      | sexp ->
        Sexplib0.Sexp_conv.of_sexp_error "Trilang.T0.t_of_sexp: invalid trilang" sexp
    ;;
  end

  include T0

  module S = struct
    [@@@coverage off]

    type t = T of T0.t

    let sexp_of_t : t -> Sexp.t = function
      | T t0 -> List [ Atom "T"; T0.sexp_of_t t0 ]
    ;;

    let t_of_sexp : Sexp.t -> t = function
      | List [ Atom "T"; sexp ] -> T (T0.t_of_sexp sexp)
      | sexp ->
        Sexplib0.Sexp_conv.of_sexp_error "Trilang.S.t_of_sexp: invalid wrapper" sexp
    ;;
  end

  include
    Sexpable.Of_sexpable
      (S)
      (struct
        type t = T0.t

        let to_sexpable t = S.T t
        let of_sexpable (S.T t) = t
      end)
end

module T = struct
  type t = (Trilang.t, int) Dunolint.Rule.Stable.V1.t

  let sexp_of_t (t : t) : Sexp.t =
    Dunolint.Rule.Stable.V1.sexp_of_t Trilang.sexp_of_t Int.sexp_of_t t
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Dunolint.Rule.Stable.V1.t_of_sexp Trilang.t_of_sexp Int.t_of_sexp sexp
  ;;

  let equal t1 t2 = Dunolint.Rule.Stable.V1.equal Trilang.equal Int.equal t1 t2
end

let%expect_test "equal" =
  let equal = T.equal in
  let enforce_a = `enforce 1 in
  let enforce_b = `enforce 2 in
  let return = `return in
  let cond_a = `cond [ Blang.true_, `enforce 1 ] in
  let cond_b = `cond [ Blang.true_, `enforce 2 ] in
  (* Physical equality. *)
  require (equal enforce_a enforce_a);
  [%expect {||}];
  require (equal cond_a cond_a);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require (equal (`enforce 1) (`enforce 1));
  [%expect {||}];
  require (equal `return `return);
  [%expect {||}];
  require (equal (`cond [ Blang.true_, `enforce 1 ]) (`cond [ Blang.true_, `enforce 1 ]));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal enforce_a enforce_b));
  [%expect {||}];
  require (not (equal cond_a cond_b));
  [%expect {||}];
  (* Test each variant as first argument to cover the catch-all. *)
  require (not (equal enforce_a return));
  [%expect {||}];
  require (not (equal return cond_a));
  [%expect {||}];
  require (not (equal cond_a enforce_a));
  [%expect {||}];
  ()
;;

let%expect_test "sexp" =
  let test t =
    let sexp = T.sexp_of_t t in
    let t' = T.t_of_sexp sexp in
    require_equal (module T) t t';
    print_s sexp
  in
  test (`enforce 42);
  [%expect {| (enforce 42) |}];
  test `return;
  [%expect {| return |}];
  test (`cond []);
  [%expect {| (cond) |}];
  test (`cond [ Blang.true_, `enforce 42 ]);
  [%expect {| (cond (true (enforce 42))) |}];
  test
    (`cond
        [ Blang.base Trilang.False, `enforce 1
        ; Blang.base Trilang.Undefined, `enforce 2
        ; Blang.base Trilang.True, `enforce 3
        ]);
  [%expect
    {|
    (cond ((T False) (enforce 1)) ((T Undefined) (enforce 2))
     ((T True) (enforce 3)))
    |}];
  ()
;;

let%expect_test "t_of_sexp - invalid cond clause" =
  let test str =
    let sexp = Parsexp.Single.parse_string_exn str in
    match T.t_of_sexp sexp with
    | t -> print_s [%sexp (t : T.t)]
    | exception exn -> print_s [%sexp (exn : Exn.t)]
  in
  (* Valid [cond]. *)
  test "(cond (true (enforce 1)))";
  [%expect {| (cond (true (enforce 1))) |}];
  (* Invalid: clause with only 1 element instead of 2. *)
  test "(cond (true))";
  [%expect
    {|
    (Of_sexp_error "rule.v1.t_of_sexp: tuple of size 2 expected"
     (invalid_sexp (true)))
    |}];
  (* Invalid: clause with 3 elements instead of 2. *)
  test "(cond (true (enforce 1) extra))";
  [%expect
    {|
    (Of_sexp_error "rule.v1.t_of_sexp: tuple of size 2 expected"
     (invalid_sexp (true (enforce 1) extra)))
    |}];
  (* Invalid: clause with 0 elements. *)
  test "(cond ())";
  [%expect
    {|
    (Of_sexp_error "rule.v1.t_of_sexp: tuple of size 2 expected"
     (invalid_sexp ()))
    |}];
  ()
;;

let%expect_test "eval" =
  let test t =
    let result = (Dunolint.Rule.eval t ~f:Fn.id :> T.t) in
    print_s [%sexp (result : T.t)]
  in
  test (`enforce 42);
  [%expect {| (enforce 42) |}];
  test `return;
  [%expect {| return |}];
  test (`cond [ Blang.true_, `enforce 42 ]);
  [%expect {| (enforce 42) |}];
  test (`cond [ Blang.base Trilang.Undefined, `enforce 42 ]);
  [%expect {| return |}];
  test (`cond [ Blang.base Trilang.False, `enforce 42 ]);
  [%expect {| return |}];
  test
    (`cond [ Blang.base Trilang.False, `enforce 1; Blang.base Trilang.True, `enforce 2 ]);
  [%expect {| (enforce 2) |}];
  ()
;;
