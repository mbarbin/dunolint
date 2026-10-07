(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

(* This test covers the behavior of dunolint when linting executable stanzas that
   have several [instrumentation] fields.

   It is adapted from and extends the similar test that we have for [library]. *)

open! Dunolint.Config.Std

let parse contents =
  Test_helpers.parse (module Dune_linter.Executable) ~path:(Fpath.v "dune") contents
;;

let enforce_internal ((sexps_rewriter, field), t) conditions =
  Dunolinter.Handler.raise ~f:(fun () ->
    List.iter conditions ~f:(fun condition -> Dune_linter.Executable.enforce t ~condition);
    Dune_linter.Executable.rewrite t ~sexps_rewriter ~field)
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
(executable
 (name myexe)
 (instrumentation (backend bisect_ppx))
 (instrumentation (backend landmarks))
)
|}
  in
  (* The instrumentation fields are a collection: [backend] holds when one of the
     fields has that backend, with the same flags. *)
  Test_helpers.is_true
    (Dune_linter.Executable.eval
       t
       ~predicate:
         (`instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx"))));
  [%expect {| |}];
  Test_helpers.is_true
    (Dune_linter.Executable.eval
       t
       ~predicate:
         (`instrumentation (backend (Dune.Instrumentation.Backend.v "landmarks"))));
  [%expect {| |}];
  (* Each backend has a field. *)
  Test_helpers.is_true
    (Dune_linter.Executable.eval
       t
       ~predicate:
         (`instrumentation
             (and_
                [ backend (Dune.Instrumentation.Backend.v "bisect_ppx")
                ; backend (Dune.Instrumentation.Backend.v "landmarks")
                ])));
  [%expect {| |}];
  Test_helpers.is_true
    (Dune_linter.Executable.eval
       t
       ~predicate:
         (`instrumentation
             (or_
                [ backend (Dune.Instrumentation.Backend.v "bisect_ppx")
                ; backend (Dune.Instrumentation.Backend.v "other")
                ])));
  [%expect {| |}];
  Test_helpers.is_false
    (Dune_linter.Executable.eval
       t
       ~predicate:
         (`instrumentation
             (or_
                [ backend (Dune.Instrumentation.Backend.v "something_else")
                ; backend (Dune.Instrumentation.Backend.v "other")
                ])));
  [%expect {| |}];
  Test_helpers.is_false
    (Dune_linter.Executable.eval
       t
       ~predicate:(`instrumentation (backend (Dune.Instrumentation.Backend.v "other"))));
  [%expect {| |}];
  Test_helpers.is_false
    (Dune_linter.Executable.eval
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
(executable
 (name myexe)
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
  (* When a matching backend is found it will be used as the sole target for
     condition enforcement. This allows "distributing" the predicates to the
     right field. *)
  let dune =
    parse
      {|
(executable
 (name myexe)
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
      (executable
       (name myexe)
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
    (executable
     (name myexe))
    |}];
  (* Dunolint can be used to enforce the presence of multiple backends. *)
  let dune =
    parse
      {|
(executable
 (name myexe)
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
(executable
 (name myexe)
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
(executable
 (name myexe)
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
    (executable
     (name myexe)
     (instrumentation
      (backend bisect_ppx))
     (instrumentation
      (backend landmarks)))
    |}];
  (* Trying the same from one instrumentation field. *)
  let dune =
    parse
      {|
(executable
 (name myexe)
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
    (executable
     (name myexe)
     (instrumentation
      (backend bisect_ppx))
     (instrumentation
      (backend landmarks)))
    |}];
  (* Enforcement of several backends can also be done with an outer [and] clause. *)
  let dune =
    parse
      {|
(executable
 (name myexe)
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
(executable
 (name myexe)
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
  (* When an enforced condition does not target a field in particular it is left up
     to evaluation and won't support any auto-fix. *)
  let dune =
    parse
      {|
(executable
 (name myexe)
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
    Dune_linter.Executable.rewrite t ~sexps_rewriter ~field;
    print_string (format_dune_file ~new_contents:(Sexps_rewriter.contents sexps_rewriter))
  in
  let t =
    Dune_linter.Executable.create
      ~instrumentations:
        [ Dune_linter.Instrumentation.create
            ~backend:(Dune.Instrumentation.Backend.v "bisect_ppx")
        ; Dune_linter.Instrumentation.create
            ~backend:(Dune.Instrumentation.Backend.v "landmarks")
        ]
      ()
  in
  test t {| (executable (name main)) |};
  [%expect
    {|
    (executable
     (name main)
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
(executable
  (name main)
  (instrumentation (backend landmarks))
  (instrumentation (backend other))
  (instrumentation (backend bisect_ppx)))
 |};
  [%expect
    {|
    (executable
     (name main)
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
(executable
  (name main)
  (instrumentation (backend other)))
 |};
  [%expect
    {|
    (executable
     (name main)
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
(executable
  (name main)
  (instrumentation invalid))
 |};
  [%expect
    {|
    (executable
     (name main)
     (instrumentation invalid)
     (instrumentation
      (backend bisect_ppx))
     (instrumentation
      (backend landmarks)))
    |}];
  (* This is also the case without entries. *)
  let t = Dune_linter.Executable.create () in
  test t {| (executable (name main) (instrumentation (backend bisect_ppx))) |};
  [%expect
    {|
    (executable
     (name main)
     (instrumentation
      (backend bisect_ppx)))
    |}];
  Dune_linter.Executable.enforce t ~condition:(not_ (has_field `instrumentation));
  test t {| (executable (name main) (instrumentation (backend bisect_ppx))) |};
  [%expect
    {|
    (executable
     (name main))
    |}];
  ()
;;

let%expect_test "present and absent" =
  (* Replacing a backend by another one is done with [present] and [absent]. *)
  let dune =
    parse
      {|
(executable
 (name main)
 (instrumentation (backend bisect_ppx))
 (instrumentation (backend landmarks)))
|}
  in
  enforce_diff
    dune
    [ instrumentation
        (and_
           [ present [ Dune.Instrumentation.Backend.Name.v "other" ]
           ; absent [ Dune.Instrumentation.Backend.Name.v "landmarks" ]
           ])
    ];
  [%expect
    {|
    @@ -3,4 +3,4 @@
       (instrumentation
        (backend bisect_ppx))
       (instrumentation
    -|  (backend landmarks)))
    +|  (backend other)))
    |}]
;;
