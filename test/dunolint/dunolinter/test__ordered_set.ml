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

module Ordered_set = Dunolinter.Ordered_set

let evaluator = Ordered_set.Evaluator.static

let%expect_test "as_set" =
  let test t =
    let res = Ordered_set.as_set (module Int) t ~evaluator in
    print_s [%sexp (res : Set.M(Int).t Ordered_set.Evaluation_result.t)]
  in
  test (Element 0);
  [%expect {| (Known (0)) |}];
  test Standard;
  [%expect {| Unknown |}];
  test (Include "foo");
  [%expect {| Unknown |}];
  test (Union [ Element 0; Element 1 ]);
  [%expect {| (Known (0 1)) |}];
  test (Union [ Element 0; Standard ]);
  [%expect {| Unknown |}];
  test (Diff (Union [ Element 0; Element 1 ], Element 0));
  [%expect {| (Known (1)) |}];
  let test t =
    let res =
      Ordered_set.as_set
        (module Int)
        t
        ~evaluator:
          { standard = (fun () -> Known [ 1; 2; 3 ])
          ; include_ = (fun var -> Known [ String.length var ])
          }
    in
    print_s [%sexp (res : Set.M(Int).t Ordered_set.Evaluation_result.t)]
  in
  test (Include "foo");
  [%expect {| (Known (3)) |}];
  test Standard;
  [%expect {| (Known (1 2 3)) |}];
  ()
;;

let%expect_test "write" =
  let test t =
    let sexps = Ordered_set.write ~write_a:Int.sexp_of_t t in
    print_s [%sexp (sexps : Sexp.t list)]
  in
  (* Empty set *)
  test (Ordered_set.of_list []);
  [%expect {| () |}];
  (* Singleton element *)
  test (Ordered_set.of_list [ 1 ]);
  [%expect {| (1) |}];
  (* Multiple elements *)
  test (Ordered_set.of_list [ 1; 2; 3 ]);
  [%expect {| (1 2 3) |}];
  (* Standard *)
  test Standard;
  [%expect {| (:standard) |}];
  (* Include *)
  test (Include "foo");
  [%expect {| (:include foo) |}];
  (* Union of elements *)
  test (Union [ Element 1; Element 2 ]);
  [%expect {| (1 2) |}];
  (* Union with Standard *)
  test (Union [ Element 1; Standard ]);
  [%expect {| (1 :standard) |}];
  (* Diff: (1 2 3) \ 2 *)
  test (Diff (Ordered_set.of_list [ 1; 2; 3 ], Element 2));
  [%expect {| ((1 2 3) \ 2) |}];
  (* Diff: empty set on left *)
  test (Diff (Ordered_set.of_list [], Element 1));
  [%expect {| (() \ 1) |}];
  (* Diff: empty set on right *)
  test (Diff (Element 1, Ordered_set.of_list []));
  [%expect {| (1 \ ()) |}];
  (* Diff: both sides empty *)
  test (Diff (Ordered_set.of_list [], Ordered_set.of_list []));
  [%expect {| (() \ ()) |}];
  (* Nested constructs: Diff (Union [...], Standard) *)
  test (Diff (Union [ Element 1; Element 2 ], Standard));
  [%expect {| ((1 2) \ :standard) |}];
  (* Nested constructs: Union with Diff *)
  test (Union [ Diff (Element 1, Element 2); Element 3 ]);
  [%expect {| (1 \ 2 3) |}];
  (* Insert and remove edge cases *)
  let s = Ordered_set.of_list [ 1; 2 ] in
  let s_insert = Ordered_set.insert (module Int) s 3 in
  test s_insert;
  [%expect {| (1 2 3) |}];
  let s_remove = Ordered_set.remove (module Int) s 2 in
  test s_remove;
  [%expect {| (1) |}];
  ()
;;

let%expect_test "mem" =
  let mem t elt ~evaluator = Ordered_set.mem (module Int) t elt ~evaluator in
  let test t elt =
    let res = mem t elt ~evaluator:Ordered_set.Evaluator.static in
    print_s [%sexp (res : bool Ordered_set.Evaluation_result.t)]
  in
  (* Default static evaluator: only Element and Union of Elements are Known *)
  test (Element 1) 1;
  [%expect {| (Known true) |}];
  test (Element 1) 2;
  [%expect {| (Known false) |}];
  test (Union [ Element 1; Element 2 ]) 2;
  [%expect {| (Known true) |}];
  test (Union [ Element 1; Element 2 ]) 3;
  [%expect {| (Known false) |}];
  test (Union [ Element 1; Standard ]) 1;
  [%expect {| (Known true) |}];
  (* Union base case: all Unknown, should return Unknown *)
  test (Union [ Standard; Include "foo" ]) 1;
  [%expect {| Unknown |}];
  (* Union with one Unknown, one Known false, should return Unknown *)
  test (Union [ Standard; Element 2 ]) 1;
  [%expect {| Unknown |}];
  test Standard 1;
  [%expect {| Unknown |}];
  test (Include "foo") 1;
  [%expect {| Unknown |}];
  test (Diff (Union [ Element 1; Element 2 ], Element 2)) 1;
  [%expect {| (Known true) |}];
  test (Diff (Union [ Element 1; Element 2 ], Element 2)) 2;
  [%expect {| (Known false) |}];
  test (Diff (Union [ Element 1; Element 2 ], Element 3)) 2;
  [%expect {| (Known true) |}];
  test (Diff (Union [ Element 1; Element 2 ], Element 3)) 3;
  [%expect {| (Known false) |}];
  test (Diff (Union [ Element 1; Element 2 ], Standard)) 1;
  [%expect {| Unknown |}];
  (* Diff: both sides Unknown *)
  test (Diff (Standard, Include "foo")) 1;
  [%expect {| Unknown |}];
  (* Diff: Unknown, Known true *)
  test (Diff (Standard, Element 1)) 1;
  [%expect {| (Known false) |}];
  (* Diff: Known true, Unknown *)
  test (Diff (Element 1, Standard)) 1;
  [%expect {| Unknown |}];
  test (Diff (Standard, Element 1)) 1;
  [%expect {| (Known false) |}];
  test (Diff (Standard, Element 1)) 2;
  [%expect {| Unknown |}];
  test (Diff (Element 1, Standard)) 1;
  [%expect {| Unknown |}];
  test (Diff (Element 1, Standard)) 2;
  [%expect {| (Known false) |}];
  test (Diff (Element 1, Element 1)) 1;
  [%expect {| (Known false) |}];
  test (Diff (Element 1, Element 2)) 1;
  [%expect {| (Known true) |}];
  test (Diff (Element 1, Element 2)) 2;
  [%expect {| (Known false) |}];
  test (Diff (Ordered_set.of_list [], Element 1)) 1;
  [%expect {| (Known false) |}];
  test (Diff (Element 1, Ordered_set.of_list [])) 1;
  [%expect {| (Known true) |}];
  test (Diff (Ordered_set.of_list [], Ordered_set.of_list [])) 1;
  [%expect {| (Known false) |}];
  (* With a custom evaluator for :standard and :include *)
  let test t elt =
    match
      mem
        t
        elt
        ~evaluator:
          { Ordered_set.Evaluator.standard = (fun () -> Known [ 1; 2; 3 ])
          ; include_ = (fun var -> Known [ String.length var ])
          }
    with
    | Known bool -> print_endline (Bool.to_string bool)
    | Unknown ->
      (* Given that the evaluator is fully specified this is unreachable. *)
      assert false
  in
  test Standard 2;
  [%expect {| true |}];
  test Standard 4;
  [%expect {| false |}];
  test (Include "foo") 3;
  [%expect {| true |}];
  test (Include "foo") 2;
  [%expect {| false |}];
  test (Union [ Element 1; Standard ]) 2;
  [%expect {| true |}];
  test (Union [ Element 1; Standard ]) 4;
  [%expect {| false |}];
  test (Union [ Element 1; Include "foo" ]) 3;
  [%expect {| true |}];
  test (Union [ Element 1; Include "foo" ]) 2;
  [%expect {| false |}];
  test (Union [ Element 1; Include "foo" ]) 4;
  [%expect {| false |}];
  test (Diff (Standard, Element 2)) 2;
  [%expect {| false |}];
  test (Diff (Standard, Element 2)) 1;
  [%expect {| true |}];
  test (Diff (Standard, Element 4)) 3;
  [%expect {| true |}];
  test (Diff (Standard, Element 4)) 4;
  [%expect {| false |}];
  test (Diff (Include "foo", Element 3)) 3;
  [%expect {| false |}];
  test (Diff (Include "foo", Element 2)) 3;
  [%expect {| true |}];
  test (Diff (Include "foo", Element 2)) 2;
  [%expect {| false |}];
  test (Diff (Include "foo", Standard)) 3;
  [%expect {| false |}];
  test (Diff (Include "foo", Standard)) 2;
  [%expect {| false |}];
  test (Diff (Standard, Include "foo")) 3;
  [%expect {| false |}];
  test (Diff (Standard, Include "foo")) 2;
  [%expect {| true |}];
  test (Diff (Standard, Include "foo")) 4;
  [%expect {| false |}];
  ()
;;

let%expect_test "insert and remove" =
  let show t =
    let t = Ordered_set.canonical_sort (module Int) t in
    let sexps = Ordered_set.write ~write_a:Int.sexp_of_t t in
    print_s [%sexp (sexps : Sexp.t list)]
  in
  let insert t x = Ordered_set.insert (module Int) t x in
  let remove t x = Ordered_set.remove (module Int) t x in
  (* Insert into empty set *)
  let t = Ordered_set.of_list [] in
  show (insert t 1);
  [%expect {| (1) |}];
  (* Insert duplicate *)
  let t = Ordered_set.of_list [ 1 ] in
  show (insert t 1);
  [%expect {| (1) |}];
  (* Insert less/greater/equal than existing *)
  let t = Ordered_set.of_list [ 2 ] in
  show (insert t 1);
  [%expect {| (1 2) |}];
  show (insert t 3);
  [%expect {| (2 3) |}];
  show (insert t 2);
  [%expect {| (2) |}];
  (* Insert into multi-element set *)
  let t = Ordered_set.of_list [ 1; 3 ] in
  show (insert t 2);
  [%expect {| (1 2 3) |}];
  show (insert t 0);
  [%expect {| (0 1 3) |}];
  show (insert t 4);
  [%expect {| (1 3 4) |}];
  (* Insert existing element into multi-element set. *)
  show (insert t 1);
  [%expect {| (1 3) |}];
  (* Insert into Standard, Include, Diff, Union *)
  show (insert Standard 1);
  [%expect {| (:standard 1) |}];
  show (insert (Include "foo") 1);
  [%expect {| (:include foo 1) |}];
  show (insert (Diff (Element 2, Element 1)) 3);
  [%expect {| ((2 3) \ 1) |}];
  show (insert (Union [ Element 2; Element 4 ]) 3);
  [%expect {| (2 3 4) |}];
  (* Insert into Union containing Standard, Include, Diff, Union *)
  show (insert (Union [ Standard; Element 2 ]) 1);
  [%expect {| (:standard 1 2) |}];
  show (insert (Union [ Include "foo"; Element 2 ]) 1);
  [%expect {| (:include foo 1 2) |}];
  show (insert (Union [ Diff (Element 2, Element 1); Element 3 ]) 4);
  [%expect {| (2 \ 1 3 4) |}];
  show (insert (Union [ Union [ Element 1; Element 2 ]; Element 3 ]) 4);
  [%expect {| (1 2 4 3) |}];
  (* Remove from empty set *)
  let t = Ordered_set.of_list [] in
  show (remove t 1);
  [%expect {| () |}];
  (* Remove only element *)
  let t = Ordered_set.of_list [ 1 ] in
  show (remove t 1);
  [%expect {| () |}];
  (* Remove non-existent element *)
  let t = Ordered_set.of_list [ 1; 2; 3 ] in
  show (remove t 4);
  [%expect {| (1 2 3) |}];
  (* Remove first, middle, last. *)
  show (remove t 1);
  [%expect {| (2 3) |}];
  show (remove t 2);
  [%expect {| (1 3) |}];
  show (remove t 3);
  [%expect {| (1 2) |}];
  (* Remove smaller, greater than one element *)
  let t = Ordered_set.of_list [ 1 ] in
  show (remove t 0);
  [%expect {| (1) |}];
  show (remove t 2);
  [%expect {| (1) |}];
  (* Remove smaller, greater than all elements *)
  let t = Ordered_set.of_list [ 1; 2 ] in
  show (remove t 0);
  [%expect {| (1 2) |}];
  show (remove t 3);
  [%expect {| (1 2) |}];
  (* Remove from Standard, Include, Diff, Union *)
  show (remove Standard 1);
  [%expect {| (:standard) |}];
  show (remove (Include "foo") 1);
  [%expect {| (:include foo) |}];
  show (remove (Diff (Element 2, Element 1)) 2);
  [%expect {| (() \ 1) |}];
  show (remove (Union [ Element 2; Element 4 ]) 2);
  [%expect {| (4) |}];
  (* Remove from Union containing Standard, Include, Diff, Union *)
  show (remove (Union [ Standard; Element 2 ]) 2);
  [%expect {| (:standard) |}];
  show (remove (Union [ Include "foo"; Element 2 ]) 2);
  [%expect {| (:include foo) |}];
  show (remove (Union [ Diff (Element 2, Element 1); Element 3 ]) 3);
  [%expect {| (2 \ 1) |}];
  show (remove (Union [ Union [ Element 1; Element 2 ]; Element 3 ]) 2);
  [%expect {| (1 3) |}];
  (* Remove from Diff containing Union, Standard, Include, Diff *)
  show (remove (Diff (Union [ Element 1; Element 2 ], Element 2)) 1);
  [%expect {| (2 \ 2) |}];
  show (remove (Diff (Standard, Element 1)) 1);
  [%expect {| (:standard \ 1) |}];
  show (remove (Diff (Include "foo", Element 1)) 1);
  [%expect {| ((:include foo) \ 1) |}];
  show (remove (Diff (Diff (Element 2, Element 1), Element 3)) 2);
  [%expect {| ((() \ 1) \ 3) |}];
  (* Remove from nested Union and Diff *)
  let t = Ordered_set.Union [ Element 1; Union [ Element 2; Element 3 ] ] in
  show (remove t 2);
  [%expect {| (1 3) |}];
  let t = Ordered_set.Diff (Union [ Element 1; Element 2 ], Element 2) in
  show (remove t 1);
  [%expect {| (2 \ 2) |}];
  ()
;;
