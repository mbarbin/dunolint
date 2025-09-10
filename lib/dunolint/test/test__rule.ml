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
    [@@deriving compare, sexp]
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
  type t = (Trilang.t, int) Dunolint.Rule.Stable.V1.t [@@deriving compare, sexp]

  let equal t1 t2 = 0 = compare t1 t2
end

let%expect_test "sexp" =
  let test t =
    let sexp = T.sexp_of_t t in
    let t' = T.t_of_sexp sexp in
    require_equal [%here] (module T) t t';
    print_s sexp
  in
  test (`enforce 42);
  [%expect {| (enforce 42) |}];
  test `return;
  [%expect {| return |}];
  require_does_raise [%here] (fun () -> test `skip_subtree);
  [%expect
    {|
    (Of_sexp_error
     "The [skip_subtree] construct is not allowed in version 1 of dunolint config."
     (invalid_sexp skip_subtree))
    |}];
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
    (cond
      ((T False)     (enforce 1))
      ((T Undefined) (enforce 2))
      ((T True)      (enforce 3)))
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
  test `skip_subtree;
  [%expect {| skip_subtree |}];
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
