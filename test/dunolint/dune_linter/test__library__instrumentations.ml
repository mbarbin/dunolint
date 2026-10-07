(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

(* This test covers the behavior of dunolint when linting library stanzas that
   have several [instrumentation] fields. *)

open! Dunolint.Config.Std

let parse contents =
  Test_helpers.parse (module Dune_linter.Library) ~path:(Fpath.v "dune") contents
;;

let enforce_internal ((sexps_rewriter, field), t) conditions =
  Dunolinter.Handler.raise ~f:(fun () ->
    List.iter conditions ~f:(fun condition -> Dune_linter.Library.enforce t ~condition);
    Dune_linter.Library.rewrite t ~sexps_rewriter ~field)
;;

let format_dune_file ~new_contents =
  Dunolint_engine.format_dune_file
    ~dune_lang_version:(Dune_project.Dune_lang_version.create (3, 17))
    ~new_contents
;;

let enforce (((sexps_rewriter, _), _) as input) conditions =
  Sexps_rewriter.reset sexps_rewriter;
  enforce_internal input conditions;
  let changed = format_dune_file ~new_contents:(Sexps_rewriter.contents sexps_rewriter) in
  print_string changed
;;

let enforce_diff (((sexps_rewriter, _), _) as input) conditions =
  Sexps_rewriter.reset sexps_rewriter;
  let original =
    format_dune_file ~new_contents:(Sexps_rewriter.contents sexps_rewriter)
  in
  enforce_internal input conditions;
  let changed = format_dune_file ~new_contents:(Sexps_rewriter.contents sexps_rewriter) in
  Myers.diff original changed ~context:3 |> print_string
;;

(* In production, enforce failures are reported and linting resumes, after
   which the stanza is rewritten. This helper reproduces that flow. *)
let enforce_and_resume ((sexps_rewriter, field), t) conditions =
  Sexps_rewriter.reset sexps_rewriter;
  Err.For_test.protect (fun () ->
    Dunolinter.Handler.emit_error_and_resume () ~loc:Loc.none ~f:(fun () ->
      List.iter conditions ~f:(fun condition -> Dune_linter.Library.enforce t ~condition));
    Dune_linter.Library.rewrite t ~sexps_rewriter ~field;
    print_string (format_dune_file ~new_contents:(Sexps_rewriter.contents sexps_rewriter)))
;;

let%expect_test "two backends" =
  let ((_, t) as dune) =
    parse
      {|
(library
 (name mylib)
 (instrumentation (backend bisect_ppx))
 (instrumentation (backend landmarks))
)
|}
  in
  (* The instrumentation fields are a collection: [backend] holds when one of the
     fields has that backend, with the same flags. *)
  Test_helpers.is_true
    (Dune_linter.Library.eval
       t
       ~predicate:
         (`instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx"))));
  [%expect {| |}];
  Test_helpers.is_true
    (Dune_linter.Library.eval
       t
       ~predicate:
         (`instrumentation (backend (Dune.Instrumentation.Backend.v "landmarks"))));
  [%expect {| |}];
  (* Each backend has a field. *)
  Test_helpers.is_true
    (Dune_linter.Library.eval
       t
       ~predicate:
         (`instrumentation
             (and_
                [ backend (Dune.Instrumentation.Backend.v "bisect_ppx")
                ; backend (Dune.Instrumentation.Backend.v "landmarks")
                ])));
  [%expect {| |}];
  Test_helpers.is_true
    (Dune_linter.Library.eval
       t
       ~predicate:
         (`instrumentation
             (or_
                [ backend (Dune.Instrumentation.Backend.v "bisect_ppx")
                ; backend (Dune.Instrumentation.Backend.v "other")
                ])));
  [%expect {| |}];
  Test_helpers.is_false
    (Dune_linter.Library.eval
       t
       ~predicate:
         (`instrumentation
             (or_
                [ backend (Dune.Instrumentation.Backend.v "something_else")
                ; backend (Dune.Instrumentation.Backend.v "other")
                ])));
  [%expect {| |}];
  Test_helpers.is_false
    (Dune_linter.Library.eval
       t
       ~predicate:(`instrumentation (backend (Dune.Instrumentation.Backend.v "other"))));
  [%expect {| |}];
  Test_helpers.is_false
    (Dune_linter.Library.eval
       t
       ~predicate:
         (`instrumentation
             (backend
                (Dune.Instrumentation.Backend.v "bisect_ppx" ~flags:[ "--with-flag" ]))));
  [%expect {| |}];
  (* When nothing is enforced, having multiple instrumentations is stable. *)
  enforce_diff dune [];
  [%expect {| |}];
  (* Enforced conditions will be applied to fields with a matching backend name. *)
  enforce_diff
    dune
    [ instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx")) ];
  [%expect {| |}];
  enforce_diff
    dune
    [ instrumentation (backend (Dune.Instrumentation.Backend.v "landmarks")) ];
  [%expect {| |}];
  (* The negation supports no auto-fix thus if one of the stanzas does not verify the
     condition, an enforce failure exception is raised. *)
  let dune =
    parse
      {|
(library
 (name mylib)
 (instrumentation (backend landmarks))
 (instrumentation (backend bisect_ppx))
)
|}
  in
  require_does_raise (fun () ->
    enforce
      dune
      [ not_ (instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx"))) ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
     (condition (not (instrumentation (backend bisect_ppx)))))
    |}];
  (* If the expression containing the negation is verified, no changes are made. *)
  let dune =
    parse
      {|
(library
 (name mylib)
 (instrumentation (backend landmarks))
)
|}
  in
  enforce
    dune
    [ instrumentation (not_ (backend (Dune.Instrumentation.Backend.v "bisect_ppx"))) ];
  [%expect
    {|
    (library
     (name mylib)
     (instrumentation
      (backend landmarks)))
    |}];
  (* When a matching backend is found it will be used as the sole target for
     condition enforcement. This allows "distributing" the predicates to the
     right field. *)
  let dune =
    parse
      {|
(library
 (name mylib)
 (instrumentation (backend bisect_ppx))
 (instrumentation (backend landmarks))
)
|}
  in
  enforce_diff
    dune
    [ instrumentation
        (backend
           (Dune.Instrumentation.Backend.v "bisect_ppx" ~flags:[ "--with-this-flag" ]))
    ; instrumentation
        (backend
           (Dune.Instrumentation.Backend.v "landmarks" ~flags:[ "--with-that-flag" ]))
    ];
  [%expect
    {|
    @@ -1,6 +1,6 @@
      (library
       (name mylib)
       (instrumentation
    -|  (backend bisect_ppx))
    +|  (backend bisect_ppx --with-this-flag))
       (instrumentation
    -|  (backend landmarks)))
    +|  (backend landmarks --with-that-flag)))
    |}];
  (* Requesting no instumentation field removes then all. *)
  enforce dune [ not (has_field `instrumentation) ];
  [%expect
    {|
    (library
     (name mylib))
    |}];
  (* Dunolint can be used to enforce the presence of multiple backends. *)
  let dune =
    parse
      {|
(library
 (name mylib)
 (instrumentation (backend bisect_ppx))
 (instrumentation (backend landmarks))
 (instrumentation (backend third_backend))
)
|}
  in
  enforce_diff
    dune
    [ instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx"))
    ; instrumentation (backend (Dune.Instrumentation.Backend.v "landmarks"))
    ];
  [%expect {| |}];
  (* The addition is auto suggested, without renaming another backend. *)
  let dune =
    parse
      {|
(library
 (name mylib)
 (instrumentation (backend landmarks))
 (instrumentation (backend third_backend))
)
|}
  in
  enforce_diff
    dune
    [ instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx"))
    ; instrumentation (backend (Dune.Instrumentation.Backend.v "landmarks"))
    ];
  [%expect
    {|
    @@ -3,4 +3,6 @@
       (instrumentation
        (backend landmarks))
       (instrumentation
    -|  (backend third_backend)))
    +|  (backend third_backend))
    +| (instrumentation
    +|  (backend bisect_ppx)))
    |}];
  (* Trying the same from zero instrumentation field. *)
  let dune =
    parse
      {|
(library
 (name mylib)
)
|}
  in
  enforce
    dune
    [ instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx"))
    ; instrumentation (backend (Dune.Instrumentation.Backend.v "landmarks"))
    ];
  [%expect
    {|
    (library
     (name mylib)
     (instrumentation
      (backend bisect_ppx))
     (instrumentation
      (backend landmarks)))
    |}];
  (* Trying the same from one instrumentation field. *)
  let dune =
    parse
      {|
(library
 (name mylib)
 (instrumentation (backend bisect_ppx))
)
|}
  in
  enforce
    dune
    [ instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx"))
    ; instrumentation (backend (Dune.Instrumentation.Backend.v "landmarks"))
    ];
  [%expect
    {|
    (library
     (name mylib)
     (instrumentation
      (backend bisect_ppx))
     (instrumentation
      (backend landmarks)))
    |}];
  (* Enforcement of several backends can also be done with an outer [and] clause. *)
  let dune =
    parse
      {|
(library
 (name mylib)
 (instrumentation (backend bisect_ppx))
 (instrumentation (backend landmarks))
 (instrumentation (backend third_backend))
)
|}
  in
  enforce_diff
    dune
    [ and_
        [ instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx"))
        ; instrumentation (backend (Dune.Instrumentation.Backend.v "landmarks"))
        ]
    ];
  [%expect {| |}];
  (* The same holds with an inner [and] clause. *)
  let dune =
    parse
      {|
(library
 (name mylib)
 (instrumentation (backend bisect_ppx))
 (instrumentation (backend landmarks))
 (instrumentation (backend third_backend))
)
|}
  in
  enforce_diff
    dune
    [ instrumentation
        (and_
           [ backend (Dune.Instrumentation.Backend.v "bisect_ppx")
           ; backend (Dune.Instrumentation.Backend.v "landmarks")
           ])
    ];
  [%expect {||}];
  (* Exercising the outer and from zero instrumentation field. *)
  let dune =
    parse
      {|
(library
 (name mylib)
)
|}
  in
  enforce_diff
    dune
    [ and_
        [ instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx"))
        ; instrumentation (backend (Dune.Instrumentation.Backend.v "landmarks"))
        ]
    ];
  [%expect
    {|
    @@ -1,2 +1,6 @@
      (library
    -| (name mylib))
    +| (name mylib)
    +| (instrumentation
    +|  (backend bisect_ppx))
    +| (instrumentation
    +|  (backend landmarks)))
    |}];
  (* Exercising the "outer and" form from one instrumentation field. *)
  let dune =
    parse
      {|
(library
 (name mylib)
 (instrumentation (backend bisect_ppx))
)
|}
  in
  enforce_diff
    dune
    [ and_
        [ instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx"))
        ; instrumentation (backend (Dune.Instrumentation.Backend.v "landmarks"))
        ]
    ];
  [%expect
    {|
    @@ -1,4 +1,6 @@
      (library
       (name mylib)
       (instrumentation
    -|  (backend bisect_ppx)))
    +|  (backend bisect_ppx))
    +| (instrumentation
    +|  (backend landmarks)))
    |}];
  (* When an enforced condition does not target a field in particular it is left up
     to evaluation and won't support any auto-fix. *)
  let dune =
    parse
      {|
(library
 (name mylib)
 (instrumentation (backend bisect_ppx))
)
|}
  in
  enforce_diff
    dune
    [ instrumentation
        (or_
           [ backend (Dune.Instrumentation.Backend.v "bisect_ppx")
           ; backend (Dune.Instrumentation.Backend.v "landmarks")
           ])
    ];
  [%expect {| |}];
  require_does_raise (fun () ->
    enforce_diff
      dune
      [ instrumentation
          (or_
             [ backend (Dune.Instrumentation.Backend.v "landmarks")
             ; backend (Dune.Instrumentation.Backend.v "other")
             ])
      ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
     (condition (or (backend landmarks) (backend other))))
    |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_linter.Library.rewrite t ~sexps_rewriter ~field;
    print_string (format_dune_file ~new_contents:(Sexps_rewriter.contents sexps_rewriter))
  in
  let t =
    Dune_linter.Library.create
      ~instrumentations:
        [ Dune_linter.Instrumentation.create
            ~backend:(Dune.Instrumentation.Backend.v "bisect_ppx")
        ; Dune_linter.Instrumentation.create
            ~backend:(Dune.Instrumentation.Backend.v "landmarks")
        ]
      ()
  in
  test t {| (library (name mylib)) |};
  [%expect
    {|
    (library
     (name mylib)
     (instrumentation
      (backend bisect_ppx))
     (instrumentation
      (backend landmarks)))
    |}];
  (* The fields without a matching entry are left untouched, unless the field is marked
     for removal. *)
  test
    t
    {|
(library
  (name mylib)
  (instrumentation (backend landmarks))
  (instrumentation (backend other))
  (instrumentation (backend bisect_ppx)))
 |};
  [%expect
    {|
    (library
     (name mylib)
     (instrumentation
      (backend landmarks))
     (instrumentation
      (backend other))
     (instrumentation
      (backend bisect_ppx)))
    |}];
  test
    t
    {|
(library
  (name mylib)
  (instrumentation (backend other)))
 |};
  [%expect
    {|
    (library
     (name mylib)
     (instrumentation
      (backend other))
     (instrumentation
      (backend bisect_ppx))
     (instrumentation
      (backend landmarks)))
    |}];
  test
    t
    {|
(library
  (name mylib)
  (instrumentation invalid))
 |};
  [%expect
    {|
    (library
     (name mylib)
     (instrumentation invalid)
     (instrumentation
      (backend bisect_ppx))
     (instrumentation
      (backend landmarks)))
    |}];
  test
    t
    {|
(library
  (name mylib)
  (instrumentation (backend invalid!backend)))
 |};
  [%expect
    {|
    (library
     (name mylib)
     (instrumentation
      (backend invalid!backend))
     (instrumentation
      (backend bisect_ppx))
     (instrumentation
      (backend landmarks)))
    |}];
  (* This is also the case without entries. *)
  let t = Dune_linter.Library.create () in
  test t {| (library (name mylib) (instrumentation (backend bisect_ppx))) |};
  [%expect
    {|
    (library
     (name mylib)
     (instrumentation
      (backend bisect_ppx)))
    |}];
  Dune_linter.Library.enforce t ~condition:(not_ (has_field `instrumentation));
  test t {| (library (name mylib) (instrumentation (backend bisect_ppx))) |};
  [%expect
    {|
    (library
     (name mylib))
    |}];
  ()
;;

let%expect_test "backend change" =
  (* Linting rules do not rename the backend of a field. Instead, a field is
     added for the required backend, and the existing one is left untouched,
     preserving its other arguments (such as [deps]), its comments and its
     position. *)
  let dune =
    parse
      {|
(library
 (name mylib)
 (instrumentation (backend other) (deps foo.txt)) ; Keep me.
 (preprocess no_preprocessing)
)
|}
  in
  enforce dune [ instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx")) ];
  [%expect
    {|
    (library
     (name mylib)
     (instrumentation
      (backend other)
      (deps foo.txt)) ; Keep me.
     (instrumentation
      (backend bisect_ppx))
     (preprocess no_preprocessing))
    |}];
  ()
;;

let%expect_test "adding a backend" =
  (* When adding a backend, the existing field is left untouched, regardless of
     the order in which the conditions are enforced. *)
  let test conditions =
    let dune =
      parse
        {|
(library
 (name mylib)
 (instrumentation (backend bisect_ppx) (deps foo.txt))
)
|}
    in
    enforce_diff dune conditions
  in
  test
    [ instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx"))
    ; instrumentation (backend (Dune.Instrumentation.Backend.v "landmarks"))
    ];
  [%expect
    {|
    @@ -2,4 +2,6 @@
       (name mylib)
       (instrumentation
        (backend bisect_ppx)
    -|  (deps foo.txt)))
    +|  (deps foo.txt))
    +| (instrumentation
    +|  (backend landmarks)))
    |}];
  test
    [ instrumentation (backend (Dune.Instrumentation.Backend.v "landmarks"))
    ; instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx"))
    ];
  [%expect
    {|
    @@ -2,4 +2,6 @@
       (name mylib)
       (instrumentation
        (backend bisect_ppx)
    -|  (deps foo.txt)))
    +|  (deps foo.txt))
    +| (instrumentation
    +|  (backend landmarks)))
    |}];
  ()
;;

let%expect_test "no instrumentation field" =
  (* When there is no instrumentation field, no [backend] predicate holds. Conditions that
     hold nonetheless are left as is, a [backend] is added when required, and the
     conditions that cannot be fixed fail. *)
  let test condition =
    let dune = parse {| (library (name mylib)) |} in
    enforce_and_resume dune [ condition ]
  in
  test
    (instrumentation
       (or_
          [ backend (Dune.Instrumentation.Backend.v "bisect_ppx")
          ; backend (Dune.Instrumentation.Backend.v "landmarks")
          ]));
  [%expect
    {|
    File "<none>", line 1, characters 0-0:
    Error: Enforce Failure.
    The following condition does not hold:
      (or (backend bisect_ppx) (backend landmarks))
    Dunolint is able to suggest automatic modifications to satisfy linting rules
    when a strategy is implemented, however in this case there is none available.
    Hint: You need to attend and fix manually.
    [123]
    (library
     (name mylib))
    |}];
  test (instrumentation true_);
  [%expect
    {|
    (library
     (name mylib))
    |}];
  test (instrumentation (not_ (backend (Dune.Instrumentation.Backend.v "landmarks"))));
  [%expect
    {|
    (library
     (name mylib))
    |}];
  test (instrumentation (backend (Dune.Instrumentation.Backend.v "landmarks")));
  [%expect
    {|
    (library
     (name mylib)
     (instrumentation
      (backend landmarks)))
    |}];
  ()
;;

let%expect_test "eval and enforce agree" =
  (* Evaluation and enforcement agree. The conditions below do not hold, since the
     stanza has a field for each of [bisect_ppx] and [landmarks]. *)
  let test condition =
    let ((_, t) as dune) =
      parse
        {|
(library
 (name mylib)
 (instrumentation (backend bisect_ppx))
 (instrumentation (backend landmarks))
)
|}
    in
    Test_helpers.is_false
      (Dune_linter.Library.eval t ~predicate:(`instrumentation condition));
    enforce_and_resume dune [ instrumentation condition ]
  in
  test (not_ (backend (Dune.Instrumentation.Backend.v "bisect_ppx")));
  [%expect
    {|
    File "<none>", line 1, characters 0-0:
    Error: Enforce Failure.
    The following condition does not hold: (not (backend bisect_ppx))
    Dunolint is able to suggest automatic modifications to satisfy linting rules
    when a strategy is implemented, however in this case there is none available.
    Hint: You need to attend and fix manually.
    [123]
    (library
     (name mylib)
     (instrumentation
      (backend bisect_ppx))
     (instrumentation
      (backend landmarks)))
    |}];
  test
    (and_
       [ backend (Dune.Instrumentation.Backend.v "bisect_ppx")
       ; not_ (backend (Dune.Instrumentation.Backend.v "landmarks"))
       ]);
  [%expect
    {|
    File "<none>", line 1, characters 0-0:
    Error: Enforce Failure.
    The following condition does not hold:
      (and (backend bisect_ppx) (not (backend landmarks)))
    Dunolint is able to suggest automatic modifications to satisfy linting rules
    when a strategy is implemented, however in this case there is none available.
    Hint: You need to attend and fix manually.
    [123]
    (library
     (name mylib)
     (instrumentation
      (backend bisect_ppx))
     (instrumentation
      (backend landmarks)))
    |}];
  ()
;;

let%expect_test "no partial changes on failure" =
  (* When a condition cannot be enforced, the stanza is left unchanged. *)
  let dune =
    parse
      {|
(library
 (name mylib)
 (instrumentation (backend other))
)
|}
  in
  enforce_and_resume
    dune
    [ instrumentation
        (and_
           [ backend (Dune.Instrumentation.Backend.v "bisect_ppx")
           ; not_ (backend (Dune.Instrumentation.Backend.v "bisect_ppx"))
           ])
    ];
  [%expect
    {|
    File "<none>", line 1, characters 0-0:
    Error: Enforce Failure.
    The following condition does not hold:
      (and (backend bisect_ppx) (not (backend bisect_ppx)))
    Dunolint is able to suggest automatic modifications to satisfy linting rules
    when a strategy is implemented, however in this case there is none available.
    Hint: You need to attend and fix manually.
    [123]
    (library
     (name mylib)
     (instrumentation
      (backend other)))
    |}];
  (* A step that fails makes the enforcement fail, even if the following steps lead to
     a state where the condition holds. *)
  let dune =
    parse
      {|
(library
 (name mylib)
 (instrumentation (backend other))
)
|}
  in
  enforce_and_resume
    dune
    [ instrumentation
        (and_
           [ or_
               [ backend (Dune.Instrumentation.Backend.v "bisect_ppx")
               ; backend (Dune.Instrumentation.Backend.v "landmarks")
               ]
           ; backend (Dune.Instrumentation.Backend.v "bisect_ppx")
           ])
    ];
  [%expect
    {|
    File "<none>", line 1, characters 0-0:
    Error: Enforce Failure.
    The following condition does not hold:
      (and (or (backend bisect_ppx) (backend landmarks)) (backend bisect_ppx))
    Dunolint is able to suggest automatic modifications to satisfy linting rules
    when a strategy is implemented, however in this case there is none available.
    Hint: You need to attend and fix manually.
    [123]
    (library
     (name mylib)
     (instrumentation
      (backend other)))
    |}];
  (* Same when the steps succeed but undo each other. *)
  let dune =
    parse
      {|
(library
 (name mylib)
 (instrumentation (backend other))
)
|}
  in
  enforce_and_resume
    dune
    [ instrumentation
        (and_
           [ backend (Dune.Instrumentation.Backend.v "bisect_ppx" ~flags:[ "--x" ])
           ; backend (Dune.Instrumentation.Backend.v "bisect_ppx")
           ])
    ];
  [%expect
    {|
    File "<none>", line 1, characters 0-0:
    Error: Enforce Failure.
    The following condition does not hold:
      (and (backend bisect_ppx --x) (backend bisect_ppx))
    Dunolint is able to suggest automatic modifications to satisfy linting rules
    when a strategy is implemented, however in this case there is none available.
    Hint: You need to attend and fix manually.
    [123]
    (library
     (name mylib)
     (instrumentation
      (backend other)))
    |}];
  ()
;;

let%expect_test "present and absent" =
  let name = Dune.Instrumentation.Backend.Name.v in
  let two_backends =
    {|
(library
 (name mylib)
 (instrumentation (backend bisect_ppx))
 (instrumentation (backend landmarks)))
|}
  in
  (* [present] and [absent] are evaluated against the set of instrumentation fields. *)
  let _, t = parse two_backends in
  let eval condition =
    Dune_linter.Library.eval t ~predicate:(`instrumentation condition)
  in
  Test_helpers.is_true (eval (present [ name "bisect_ppx"; name "landmarks" ]));
  Test_helpers.is_false (eval (present [ name "bisect_ppx"; name "other" ]));
  Test_helpers.is_true (eval (absent [ name "other"; name "another" ]));
  Test_helpers.is_false (eval (absent [ name "other"; name "landmarks" ]));
  [%expect {||}];
  (* Without instrumentation fields, the collection is empty. *)
  let _, t = parse {| (library (name mylib)) |} in
  let eval condition =
    Dune_linter.Library.eval t ~predicate:(`instrumentation condition)
  in
  Test_helpers.is_false (eval (present [ name "bisect_ppx" ]));
  Test_helpers.is_true (eval (absent [ name "bisect_ppx" ]));
  Test_helpers.is_true (eval true_);
  Test_helpers.is_false (eval false_);
  Test_helpers.is_false
    (eval
       (and_
          [ absent [ name "bisect_ppx" ]
          ; backend (Dune.Instrumentation.Backend.v "landmarks")
          ]));
  Test_helpers.is_true
    (eval (if_ (present [ name "bisect_ppx" ]) false_ (absent [ name "landmarks" ])));
  Test_helpers.is_false
    (eval
       (if_
          (backend (Dune.Instrumentation.Backend.v "landmarks"))
          true_
          (present [ name "bisect_ppx" ])));
  Test_helpers.is_true
    (eval
       (if_
          (backend (Dune.Instrumentation.Backend.v "landmarks"))
          (present [ name "bisect_ppx" ])
          true_));
  [%expect {||}];
  (* Enforcing [present] adds the missing fields. *)
  enforce_diff
    (parse {| (library (name mylib) (instrumentation (backend bisect_ppx))) |})
    [ instrumentation (present [ name "bisect_ppx"; name "landmarks" ]) ];
  [%expect
    {|
    @@ -1,4 +1,6 @@
      (library
       (name mylib)
       (instrumentation
    -|  (backend bisect_ppx)))
    +|  (backend bisect_ppx))
    +| (instrumentation
    +|  (backend landmarks)))
    |}];
  (* Enforcing [absent] removes the fields, including the last one. *)
  enforce_diff (parse two_backends) [ instrumentation (absent [ name "landmarks" ]) ];
  [%expect
    {|
    @@ -1,6 +1,4 @@
      (library
       (name mylib)
       (instrumentation
    -|  (backend bisect_ppx))
    -| (instrumentation
    -|  (backend landmarks)))
    +|  (backend bisect_ppx)))
    |}];
  enforce_diff
    (parse two_backends)
    [ instrumentation (absent [ name "bisect_ppx"; name "landmarks" ]) ];
  [%expect
    {|
    @@ -1,6 +1,2 @@
      (library
    -| (name mylib)
    -| (instrumentation
    -|  (backend bisect_ppx))
    -| (instrumentation
    -|  (backend landmarks)))
    +| (name mylib))
    |}];
  (* Migrating from a backend to another. *)
  enforce_diff
    (parse {| (library (name mylib) (instrumentation (backend landmarks))) |})
    [ instrumentation (present [ name "bisect_ppx" ])
    ; instrumentation (absent [ name "landmarks" ])
    ];
  [%expect
    {|
    @@ -1,4 +1,4 @@
      (library
       (name mylib)
       (instrumentation
    -|  (backend landmarks)))
    +|  (backend bisect_ppx)))
    |}];
  (* [present] and [backend] can be combined: each [backend] targets the field with that
     backend, adding it if needed. *)
  enforce_diff
    (parse {| (library (name mylib) (instrumentation (backend landmarks))) |})
    [ instrumentation
        (and_
           [ present [ name "landmarks" ]
           ; backend (Dune.Instrumentation.Backend.v "bisect_ppx" ~flags:[ "--x" ])
           ])
    ];
  [%expect
    {|
    @@ -1,4 +1,6 @@
      (library
       (name mylib)
       (instrumentation
    -|  (backend landmarks)))
    +|  (backend landmarks))
    +| (instrumentation
    +|  (backend bisect_ppx --x)))
    |}];
  enforce_diff
    (parse two_backends)
    [ instrumentation
        (and_
           [ present [ name "landmarks" ]
           ; backend (Dune.Instrumentation.Backend.v "bisect_ppx" ~flags:[ "--x" ])
           ])
    ];
  [%expect
    {|
    @@ -1,6 +1,6 @@
      (library
       (name mylib)
       (instrumentation
    -|  (backend bisect_ppx))
    +|  (backend bisect_ppx --x))
       (instrumentation
        (backend landmarks)))
    |}];
  require_does_raise (fun () ->
    enforce_diff
      (parse two_backends)
      [ instrumentation
          (and_
             [ present [ name "bisect_ppx" ]
             ; not_ (backend (Dune.Instrumentation.Backend.v "landmarks"))
             ])
      ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
     (condition (and (present bisect_ppx) (not (backend landmarks)))))
    |}];
  (* The negation of a single backend is enforced like its opposite. *)
  enforce_diff
    (parse two_backends)
    [ instrumentation (not_ (present [ name "landmarks" ])) ];
  [%expect
    {|
    @@ -1,6 +1,4 @@
      (library
       (name mylib)
       (instrumentation
    -|  (backend bisect_ppx))
    -| (instrumentation
    -|  (backend landmarks)))
    +|  (backend bisect_ppx)))
    |}];
  enforce_diff
    (parse {| (library (name mylib) (instrumentation (backend bisect_ppx))) |})
    [ instrumentation (not_ (absent [ name "landmarks" ])) ];
  [%expect
    {|
    @@ -1,4 +1,6 @@
      (library
       (name mylib)
       (instrumentation
    -|  (backend bisect_ppx)))
    +|  (backend bisect_ppx))
    +| (instrumentation
    +|  (backend landmarks)))
    |}];
  (* With more than one backend, the negation is only checked. *)
  enforce_diff
    (parse two_backends)
    [ instrumentation (not_ (present [ name "landmarks"; name "other" ]))
    ; instrumentation (not_ (absent [ name "landmarks"; name "other" ]))
    ];
  [%expect {||}];
  require_does_raise (fun () ->
    enforce_diff
      (parse two_backends)
      [ instrumentation (not_ (present [ name "bisect_ppx"; name "landmarks" ])) ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
     (condition (not (present bisect_ppx landmarks))))
    |}];
  require_does_raise (fun () ->
    enforce_diff
      (parse two_backends)
      [ instrumentation (not_ (absent [ name "other"; name "another" ])) ]);
  [%expect
    {| (Dunolinter.Handler.Enforce_failure (condition (not (absent other another)))) |}];
  ()
;;
