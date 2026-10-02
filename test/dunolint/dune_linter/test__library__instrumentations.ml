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
  (* Conditions are matched by backend name, otherwise they are bound existentially
     to the present fields : they evaluate to [True] if they are satisfied by
     one of them and [False] if all the fields negate them. [Undefined] would be
     returned otherwise, although that does not currently happen with the
     predicates currently available. *)
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
  (* No field satisfies both backends at once. *)
  Test_helpers.is_false
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
    (Dunolinter.Handler.Enforce_failure (loc _)
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
  (* The addition is auto suggested, replacing another backend in place if any. *)
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
    @@ -3,4 +3,4 @@
       (instrumentation
        (backend landmarks))
       (instrumentation
    -|  (backend third_backend)))
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
  (* However it cannot be enforced by an inner clause which may never be satisfied. *)
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
  require_does_raise (fun () ->
    enforce_diff
      dune
      [ instrumentation
          (and_
             [ backend (Dune.Instrumentation.Backend.v "bisect_ppx")
             ; backend (Dune.Instrumentation.Backend.v "landmarks")
             ])
      ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (instrumentation (and (backend bisect_ppx) (backend landmarks)))))
    |}];
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
    (Dunolinter.Handler.Enforce_failure (loc _)
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
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
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
    (library (name mylib) (instrumentation (backend bisect_ppx))
     (instrumentation (backend landmarks)))
    |}];
  test
    t
    {|
(library
  (name mylib)
  (instrumentation (backend landmarks))
  (instrumentation (backend other))
  (instrumentation (backend bisect_ppx))
)
 |};
  [%expect
    {|
    (library (name mylib) (instrumentation (backend landmarks))
     (instrumentation (backend bisect_ppx)))
    |}];
  test
    t
    {|
(library
  (name mylib)
  (instrumentation (backend other))
)
 |};
  [%expect
    {|
    (library (name mylib) (instrumentation (backend bisect_ppx))
     (instrumentation (backend landmarks)))
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
    (library (name mylib) (instrumentation (backend bisect_ppx))
     (instrumentation (backend landmarks)))
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
    (library (name mylib) (instrumentation (backend bisect_ppx))
     (instrumentation (backend landmarks)))
    |}];
  ()
;;
