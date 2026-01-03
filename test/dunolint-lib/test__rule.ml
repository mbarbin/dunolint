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

module Trilang = struct
  module T0 = struct
    [@@@coverage off]

    type t = Dunolint.Trilang.t =
      | True
      | False
      | Undefined
    [@@deriving equal, sexp]
  end

  include T0

  module S = struct
    [@@@coverage off]

    type t = T of T0.t [@@deriving sexp]
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
  type t = (Trilang.t, int) Dunolint.Rule.Stable.V1.t [@@deriving sexp]

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
