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

let parse contents =
  Test_helpers.parse (module Dune_linter.Pps) ~path:(Fpath.v "dune") contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_linter.Pps.write t))
  in
  test {| (pps ppx_deriving) |};
  [%expect {| (pps ppx_deriving) |}];
  test {| (pps (ppx_deriving -deriving)) |};
  [%expect
    {|
    File "dune", line 1, characters 6-30:
    Error: Unexpected [Sexp.List]. [Pps] expected to be atoms.
    [123]
    |}];
  test {| (invalid field) |};
  [%expect
    {|
    File "dune", line 1, characters 1-16:
    Error: Unexpected [pps] field.
    [123]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let test str =
    let _, t = parse str in
    print_s [%sexp (t : Dune_linter.Pps.t)]
  in
  test {| (pps --driver ppx_deriving --flag --opt=param) |};
  [%expect
    {|
    ((
      sections (((
        entries (
          ((arg (Flag (name --driver) (param ()) (applies_to driver)))
           (eol_comment ()))
          ((arg (Pp (pp_name ppx_deriving))) (eol_comment ()))
          ((arg (Flag (name --flag) (param ()) (applies_to (pp ppx_deriving))))
           (eol_comment ()))
          ((arg (Flag (name --opt) (param (param)) (applies_to (pp ppx_deriving))))
           (eol_comment ()))))))))
    |}];
  (* Entries separated by more than one line are treated as belonging to
     different sections. In particular this can be achieved with a style where
     sections are separated by comments (same as libraries). *)
  test
    {|
(pps
  --driver ;; that's a flag for the driver
 ;; The rest are individual ppx.
 ppx_one
 ;; This one has more flags and parameters.
 ppx_deriving --flag --opt=param) |};
  [%expect
    {|
    ((
      sections (
        ((
          entries ((
            (arg (Flag (name --driver) (param ()) (applies_to driver)))
            (eol_comment (";; that's a flag for the driver"))))))
        ((entries (((arg (Pp (pp_name ppx_one))) (eol_comment ())))))
        ((
          entries (
            ((arg (Pp (pp_name ppx_deriving))) (eol_comment ()))
            ((arg (Flag (name --flag) (param ()) (applies_to (pp ppx_deriving))))
             (eol_comment ()))
            ((arg (
               Flag (name --opt) (param (param)) (applies_to (pp ppx_deriving))))
             (eol_comment ()))))))))
    |}];
  (* Note a flag may be attached to the latest argument of a previous section. *)
  test
    {|
  (pps a
   -b
   -a
   ;; Another flag. Considered to be attached to a.
   -c)|};
  [%expect
    {|
    ((
      sections (
        ((
          entries (
            ((arg (Pp (pp_name a))) (eol_comment ()))
            ((arg (Flag (name -b) (param ()) (applies_to (pp a))))
             (eol_comment ()))
            ((arg (Flag (name -a) (param ()) (applies_to (pp a))))
             (eol_comment ())))))
        ((
          entries ((
            (arg (Flag (name -c) (param ()) (applies_to (pp a))))
            (eol_comment ()))))))))
    |}];
  test "(pps)";
  [%expect {| ((sections ())) |}];
  ()
;;

let%expect_test "parse" =
  let test list =
    let t = Dune_linter.Pps.parse ~loc:Loc.none list in
    print_s [%sexp (t : Dune_linter.Pps.t)]
  in
  test [ "--driver"; "ppx_deriving"; "--flag"; "--opt=param" ];
  [%expect
    {|
    ((
      sections (((
        entries (
          ((arg (Flag (name --driver) (param ()) (applies_to driver)))
           (eol_comment ()))
          ((arg (Pp (pp_name ppx_deriving))) (eol_comment ()))
          ((arg (Flag (name --flag) (param ()) (applies_to (pp ppx_deriving))))
           (eol_comment ()))
          ((arg (Flag (name --opt) (param (param)) (applies_to (pp ppx_deriving))))
           (eol_comment ()))))))))
    |}];
  require_does_raise [%here] (fun () -> test [ "" ]);
  [%expect {| "Invalid empty pp." |}];
  test [];
  [%expect {| ((sections ())) |}];
  ()
;;

let rewrite ?(f = ignore) str =
  let (sexps_rewriter, field), t = parse str in
  f t;
  Dune_linter.Pps.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (pps ppx_deriving) |};
  [%expect {| (pps ppx_deriving) |}];
  (* Exercising some getters. *)
  rewrite {| (pps ppx_deriving -flag) |} ~f:(fun t ->
    (* There are no getters to test at the moment. *)
    ignore (t : Dune_linter.Pps.t);
    ());
  [%expect {| (pps ppx_deriving -flag) |}];
  (* Exercising some setters. *)
  rewrite {| (pps ppx_deriving -flag) |} ~f:(fun t ->
    (* There are no setters to test at the moment. *)
    ignore (t : Dune_linter.Pps.t);
    ());
  [%expect {| (pps ppx_deriving -flag) |}];
  (* Here we monitor how pps and flags get reordered. *)
  rewrite {| (pps -b -a b --b --a --d a d c)|};
  [%expect {| (pps -a -b a b --a --b --d c d) |}];
  (* And how the sorting is affected in the presence of sections. *)
  rewrite
    {|
  (pps
   -b ;; driver
   -a ;; driver too
   ;; The a & b ppx
   b --b --a --d a
   ;; The c and d ppx
   d
   c ;; inline comments
   )|};
  [%expect
    {|
    (pps
     -a ;; driver too
     -b ;; driver
     ;; The a & b ppx
     a b --a --b --d
     ;; The c and d ppx
     c ;; inline comments
     d
     )
    |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_linter.Pps.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t =
    Dune_linter.Pps.create
      ~args:
        [ Flag { name = "--hello-driver"; param = None }
        ; Pp (Dune.Pp.Name.v "ppx_deriving")
        ]
  in
  test t {| (pps -flag ppx_deriving -flag) |};
  [%expect {| (pps --hello-driver ppx_deriving) |}];
  (* When dunolint doesn't understand the expression to rewrite, this triggers an error. *)
  require_does_raise [%here] (fun () -> test t {| (other_field (unexpected args)) |});
  [%expect {| "Unexpected [pps] field." |}];
  (* However if the field is [pps] the existing arguments are replaced. *)
  test t {| (pps (unexpected args)) |};
  [%expect {| (pps --hello-driver ppx_deriving) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune.Pps.Predicate.t as 'a
    constraint
      'a =
      [ `pp of Dune.Pp.Name.t
      | `flag of Dune.Pps.Predicate.Flag.t
      | `pp_with_flag of Dune.Pps.Predicate.Pp_with_flag.t
      ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (pps ppx_deriving) |} in
  Test_helpers.is_true
    (Dune_linter.Pps.eval t ~predicate:(`pp (Dune.Pp.Name.v "ppx_deriving")));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Pps.eval t ~predicate:(`pp (Dune.Pp.Name.v "ppx_other")));
  [%expect {||}];
  let _, t =
    parse
      {|
 (pps
   --flag-for-the-ppx-driver
   ppx_jane
   --flag-for-jane
   --other-flag-for-jane
   ppx_john
   --flag-for-john=value)
|}
  in
  Test_helpers.is_true
    (Dune_linter.Pps.eval t ~predicate:(`pp (Dune.Pp.Name.v "ppx_jane")));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_linter.Pps.eval
       t
       ~predicate:
         (`flag
             { name = "--flag-for-jane"
             ; param = `none
             ; applies_to = `pp (Dune.Pp.Name.v "ppx_jane")
             }));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_linter.Pps.eval
       t
       ~predicate:(`flag { name = "--flag-for-jane"; param = `none; applies_to = `any }));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_linter.Pps.eval
       t
       ~predicate:(`flag { name = "--flag-for-jane"; param = `any; applies_to = `any }));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Pps.eval
       t
       ~predicate:
         (`flag { name = "--flag-for-jane"; param = `none; applies_to = `driver }));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Pps.eval
       t
       ~predicate:(`flag { name = "--flag-for-jane"; param = `some; applies_to = `any }));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Pps.eval
       t
       ~predicate:
         (`flag { name = "--flag-for-jane"; param = `equals "blah"; applies_to = `any }));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_linter.Pps.eval
       t
       ~predicate:(`flag { name = "--flag-for-john"; param = `some; applies_to = `any }));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Pps.eval
       t
       ~predicate:(`flag { name = "--flag-for-john"; param = `none; applies_to = `any }));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_linter.Pps.eval
       t
       ~predicate:(`flag { name = "--flag-for-john"; param = `any; applies_to = `any }));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Pps.eval
       t
       ~predicate:
         (`flag { name = "--flag-for-john"; param = `equals "blah"; applies_to = `any }));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_linter.Pps.eval
       t
       ~predicate:
         (`flag { name = "--flag-for-john"; param = `equals "value"; applies_to = `any }));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_linter.Pps.eval
       t
       ~predicate:
         (`flag { name = "--flag-for-the-ppx-driver"; param = `none; applies_to = `any }));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Pps.eval
       t
       ~predicate:
         (`flag
             { name = "--flag-for-the-ppx-driver"
             ; param = `none
             ; applies_to = `pp (Dune.Pp.Name.v "ppx_jane")
             }));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_linter.Pps.eval
       t
       ~predicate:
         (`flag
             { name = "--flag-for-the-ppx-driver"; param = `none; applies_to = `driver }));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Pps.eval
       t
       ~predicate:
         (`pp_with_flag { pp = Dune.Pp.Name.v "ppx_eve"; flag = "-flag"; param = `none }));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Pps.eval
       t
       ~predicate:
         (`pp_with_flag { pp = Dune.Pp.Name.v "ppx_jane"; flag = "-flag"; param = `none }));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_linter.Pps.eval
       t
       ~predicate:
         (`pp_with_flag
             { pp = Dune.Pp.Name.v "ppx_jane"; flag = "--flag-for-jane"; param = `none }));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_linter.Pps.eval
       t
       ~predicate:
         (`pp_with_flag
             { pp = Dune.Pp.Name.v "ppx_jane"; flag = "--flag-for-jane"; param = `any }));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Pps.eval
       t
       ~predicate:
         (`pp_with_flag
             { pp = Dune.Pp.Name.v "ppx_jane"; flag = "--flag-for-jane"; param = `some }));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Pps.eval
       t
       ~predicate:
         (`pp_with_flag
             { pp = Dune.Pp.Name.v "ppx_jane"
             ; flag = "--flag-for-jane"
             ; param = `equals "blah"
             }));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Pps.eval
       t
       ~predicate:
         (`pp_with_flag
             { pp = Dune.Pp.Name.v "ppx_jane"
             ; flag = "--flag-for-the-ppx-driver"
             ; param = `none
             }));
  [%expect {||}];
  ()
;;

let%expect_test "enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition -> Dune_linter.Pps.enforce t ~condition);
      Dune_linter.Pps.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  let t = parse {| (pps ppx_sexp_conv) |} in
  enforce t [];
  [%expect {| (pps ppx_sexp_conv) |}];
  (* Enforcing the presence of a present pp has no effect. *)
  enforce t [ pp (Dune.Pp.Name.v "ppx_sexp_conv") ];
  [%expect {| (pps ppx_sexp_conv) |}];
  (* Enforcing the presence of a new pp adds it. *)
  enforce t [ pp (Dune.Pp.Name.v "ppx_other") ];
  [%expect {| (pps ppx_other ppx_sexp_conv) |}];
  (* Enforcing the absence of an absent pp has no effect. *)
  enforce t [ not_ (pp (Dune.Pp.Name.v "ppx_not_there")) ];
  [%expect {| (pps ppx_other ppx_sexp_conv) |}];
  (* Enforcing the negation of a present pp removes it. *)
  enforce t [ not_ (pp (Dune.Pp.Name.v "ppx_sexp_conv")) ];
  [%expect {| (pps ppx_other) |}];
  (* Flags. *)
  let t =
    parse
      {|
 (pps
   --flag-for-the-ppx-driver
   ppx_jane
   --flag-for-jane
   --other-flag-for-jane
   ppx_john
   --flag-for-john=value)
|}
  in
  enforce
    t
    [ pp (Dune.Pp.Name.v "ppx_eve")
    ; pp_with_flag
        { pp = Dune.Pp.Name.v "ppx_alice"; flag = "--with-flag"; param = `equals "param" }
    ; flag { name = "--hello-driver"; param = `equals "p"; applies_to = `driver }
    ; flag
        { name = "--flag-for-jane"
        ; param = `equals "new"
        ; applies_to = `pp (Dune.Pp.Name.v "ppx_jane")
        }
    ; not_
        (flag
           { name = "--other-flag-for-jane"
           ; param = `any
           ; applies_to = `pp (Dune.Pp.Name.v "ppx_jane")
           })
    ];
  [%expect
    {|
    (pps
     --flag-for-the-ppx-driver
     --hello-driver=p
     ppx_alice
     --with-flag=param
     ppx_eve
     ppx_jane
     --flag-for-jane=new
     ppx_john
     --flag-for-john=value)
    |}];
  (* Removing a pp removes its flags too. *)
  enforce t [ not_ (pp (Dune.Pp.Name.v "ppx_jane")) ];
  [%expect
    {|
    (pps
     --flag-for-the-ppx-driver
     --hello-driver=p
     ppx_alice
     --with-flag=param
     ppx_eve
     ppx_john
     --flag-for-john=value)
    |}];
  (* Specific params may be removed. *)
  enforce
    t
    [ flag
        { name = "--with-flag"
        ; param = `none
        ; applies_to = `pp (Dune.Pp.Name.v "ppx_alice")
        }
    ];
  [%expect
    {|
    (pps
     --flag-for-the-ppx-driver
     --hello-driver=p
     ppx_alice
     --with-flag
     ppx_eve
     ppx_john
     --flag-for-john=value)
    |}];
  (* Flags may be re-assigned to different pp or driver. *)
  enforce
    t
    [ flag
        { name = "--hello-driver"
        ; param = `any
        ; applies_to = `pp (Dune.Pp.Name.v "ppx_eve")
        }
    ];
  [%expect
    {|
    (pps
     --flag-for-the-ppx-driver
     ppx_alice
     --with-flag
     ppx_eve
     --hello-driver=p
     ppx_john
     --flag-for-john=value)
    |}];
  (* Blang. *)
  let t = parse {| (pps ppx_deriving) |} in
  enforce t [ true_ ];
  [%expect {| (pps ppx_deriving) |}];
  require_does_raise [%here] (fun () -> enforce t [ false_ ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc       _)
      (condition false))
    |}];
  enforce
    t
    [ and_ [ not_ (pp (Dune.Pp.Name.v "ppx_other")); pp (Dune.Pp.Name.v "ppx_deriving") ]
    ];
  [%expect {| (pps ppx_deriving) |}];
  (* [or] does not have an enforcement strategy when its invariant is
     not satisfied. *)
  enforce
    t
    [ or_ [ pp (Dune.Pp.Name.v "ppx_deriving"); pp (Dune.Pp.Name.v "ppx_other") ] ];
  [%expect {| (pps ppx_deriving) |}];
  require_does_raise [%here] (fun () ->
    enforce t [ or_ [ pp (Dune.Pp.Name.v "qualified"); pp (Dune.Pp.Name.v "no") ] ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (
        or
        (pp qualified)
        (pp no))))
    |}];
  (* When defined, [if] enforces the clause that applies. *)
  let invariant =
    if_
      (pp (Dune.Pp.Name.v "ppx_deriving"))
      (pp (Dune.Pp.Name.v "ppx_other"))
      (pp (Dune.Pp.Name.v "ppx_deriving"))
  in
  let t = parse {| (pps ppx_deriving) |} in
  enforce t [ invariant ];
  [%expect {| (pps ppx_deriving ppx_other) |}];
  let t = parse {| (pps ppx_other) |} in
  enforce t [ invariant ];
  [%expect {| (pps ppx_deriving ppx_other) |}];
  (* Add from an empty pps. *)
  let t = parse {| (pps) |} in
  enforce t [ pp (Dune.Pp.Name.v "ppx_other") ];
  [%expect {| (pps ppx_other) |}];
  let t = parse {| (pps ppx_a) |} in
  enforce
    t
    [ flag
        { name = "--flag-for-jane"
        ; param = `equals "new"
        ; applies_to = `pp (Dune.Pp.Name.v "ppx_jane")
        }
    ];
  [%expect {| (pps ppx_a --flag-for-jane=new) |}];
  let t = parse {| (pps ppx_z) |} in
  enforce
    t
    [ flag
        { name = "--flag-for-jane"
        ; param = `equals "new"
        ; applies_to = `pp (Dune.Pp.Name.v "ppx_jane")
        }
    ];
  [%expect {| (pps --flag-for-jane=new ppx_z) |}];
  let t = parse {| (pps) |} in
  enforce
    t
    [ flag
        { name = "--flag-for-jane"
        ; param = `equals "new"
        ; applies_to = `pp (Dune.Pp.Name.v "ppx_jane")
        }
    ; pp (Dune.Pp.Name.v "ppx_jane")
    ];
  [%expect {| (pps ppx_jane --flag-for-jane=new) |}];
  let t = parse {| (pps --driver) |} in
  require_does_raise [%here] (fun () ->
    enforce t [ flag { name = "--driver"; param = `some; applies_to = `driver } ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (
        flag (
          (name       --driver)
          (param      some)
          (applies_to driver)))))
    |}];
  let t = parse {| (pps --driver=screw) |} in
  enforce t [ flag { name = "--driver"; param = `some; applies_to = `any } ];
  [%expect {| (pps --driver=screw) |}];
  let t = parse {| (pps hey --driver=screw) |} in
  enforce t [ flag { name = "--driver"; param = `some; applies_to = `driver } ];
  [%expect {| (pps --driver=screw hey) |}];
  let t = parse {| (pps) |} in
  require_does_raise [%here] (fun () ->
    enforce t [ flag { name = "--driver"; param = `some; applies_to = `driver } ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (
        flag (
          (name       --driver)
          (param      some)
          (applies_to driver)))))
    |}];
  let t = parse {| (pps) |} in
  enforce t [ flag { name = "--driver"; param = `any; applies_to = `any } ];
  [%expect {| (pps --driver) |}];
  let t = parse {| (pps) |} in
  enforce t [ flag { name = "--driver"; param = `none; applies_to = `driver } ];
  [%expect {| (pps --driver) |}];
  let t = parse {| (pps) |} in
  enforce
    t
    [ not_
        (pp_with_flag
           { pp = Dune.Pp.Name.v "ppx_alice"
           ; flag = "--with-flag"
           ; param = `equals "param"
           })
    ];
  [%expect {| (pps) |}];
  let t = parse {| (pps) |} in
  enforce t [ not_ (flag { name = "--driver"; param = `none; applies_to = `driver }) ];
  [%expect {| (pps) |}];
  let t = parse {| (pps --driver) |} in
  enforce t [ not_ (flag { name = "--driver"; param = `some; applies_to = `driver }) ];
  [%expect {| (pps --driver) |}];
  let t = parse {| (pps --driver=screw) |} in
  enforce
    t
    [ not_ (flag { name = "--driver"; param = `equals "student"; applies_to = `driver }) ];
  [%expect {| (pps --driver=screw) |}];
  let t = parse {| (pps --driver=screw) |} in
  enforce t [ not_ (flag { name = "--driver"; param = `any; applies_to = `driver }) ];
  [%expect {| (pps) |}];
  let t = parse {| (pps --driver=screw) |} in
  enforce t [ not_ (flag { name = "--driver"; param = `any; applies_to = `any }) ];
  [%expect {| (pps) |}];
  let t = parse {| (pps --driver=screw) |} in
  enforce
    t
    [ not_
        (flag
           { name = "--driver"; param = `any; applies_to = `pp (Dune.Pp.Name.v "ppx_o") })
    ];
  [%expect {| (pps --driver=screw) |}];
  let t = parse {| (pps ppx_o --driver=screw) |} in
  enforce t [ not_ (flag { name = "--driver"; param = `any; applies_to = `driver }) ];
  [%expect {| (pps ppx_o --driver=screw) |}];
  ()
;;
