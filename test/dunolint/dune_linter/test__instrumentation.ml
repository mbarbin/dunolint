(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let parse contents =
  Test_helpers.parse (module Dune_linter.Instrumentation) ~path:(Fpath.v "dune") contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_linter.Instrumentation.write t))
  in
  test {| (instrumentation (backend bisect_ppx)) |};
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  test {| (instrumentation) |};
  [%expect
    {|
    File "dune", line 1, characters 1-18:
    Error: Required [backend] value in instrumentation.
    [123]
    |}];
  test {| (invalid field) |};
  [%expect
    {|
    File "dune", line 1, characters 1-16:
    Error: Unexpected [instrumentation] field.
    [123]
    |}];
  test {| (instrumentation (backend other_backend)) |};
  [%expect {| (instrumentation (backend other_backend)) |}];
  (* Dunolint simply ignores other fields if any. *)
  test {| (instrumentation (other field) (backend bisect_ppx)) |};
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  (* Backend with flags. *)
  test {| (instrumentation (backend ppx_windtrap --coverage)) |};
  [%expect {| (instrumentation (backend ppx_windtrap --coverage)) |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (instrumentation (backend bisect_ppx)) |} in
  print_s (t |> Dune_linter.Instrumentation.sexp_of_t);
  [%expect {| ((backend bisect_ppx)) |}];
  let _, t = parse {| (instrumentation (backend ppx_windtrap --coverage)) |} in
  print_s (t |> Dune_linter.Instrumentation.sexp_of_t);
  [%expect {| ((backend (ppx_windtrap --coverage))) |}];
  ()
;;

let rewrite ?(f = ignore) str =
  let (sexps_rewriter, field), t = parse str in
  f t;
  Dune_linter.Instrumentation.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (instrumentation (backend bisect_ppx)) |};
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  (* Exercising some getters and setters. *)
  rewrite {| (instrumentation (backend other_backend)) |} ~f:(fun t ->
    print_s
      (Dune_linter.Instrumentation.backend t |> Dune.Instrumentation.Backend.sexp_of_t);
    [%expect {| other_backend |}];
    Dune_linter.Instrumentation.set_backend
      t
      ~backend:(Dune.Instrumentation.Backend.v "bisect_ppx");
    ());
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  (* Rewrite: backend with flags. *)
  rewrite {| (instrumentation (backend ppx_windtrap --coverage)) |};
  [%expect {| (instrumentation (backend ppx_windtrap --coverage)) |}];
  (* Rewrite: bisect_ppx -> ppx_windtrap --coverage. *)
  rewrite {| (instrumentation (backend bisect_ppx)) |} ~f:(fun t ->
    Dune_linter.Instrumentation.set_backend
      t
      ~backend:(Dune.Instrumentation.Backend.v "ppx_windtrap" ~flags:[ "--coverage" ]));
  [%expect {| (instrumentation (backend ppx_windtrap --coverage)) |}];
  (* Rewrite: ppx_windtrap --coverage -> bisect_ppx. *)
  rewrite {| (instrumentation (backend ppx_windtrap --coverage)) |} ~f:(fun t ->
    Dune_linter.Instrumentation.set_backend
      t
      ~backend:(Dune.Instrumentation.Backend.v "bisect_ppx"));
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  (* Comment after name: identity rewrite preserves comment. *)
  rewrite
    {| (instrumentation (backend bisect_ppx ; coverage tool
  )) |};
  [%expect
    {|
    (instrumentation (backend bisect_ppx ; coverage tool
     ))
    |}];
  (* Comment after name: rewrite name preserves comment. *)
  rewrite
    {| (instrumentation (backend bisect_ppx ; coverage tool
  )) |}
    ~f:(fun t ->
      Dune_linter.Instrumentation.set_backend
        t
        ~backend:(Dune.Instrumentation.Backend.v "other_backend"));
  [%expect
    {|
    (instrumentation (backend other_backend ; coverage tool
     ))
    |}];
  (* Comment after name: add flags preserves comment. *)
  rewrite
    {| (instrumentation (backend bisect_ppx ; coverage tool
  )) |}
    ~f:(fun t ->
      Dune_linter.Instrumentation.set_backend
        t
        ~backend:(Dune.Instrumentation.Backend.v "ppx_windtrap" ~flags:[ "--coverage" ]));
  [%expect
    {|
    (instrumentation (backend ppx_windtrap --coverage ; coverage tool
     ))
    |}];
  (* Comment after flag: identity rewrite preserves comment. *)
  rewrite
    {| (instrumentation (backend ppx_windtrap --coverage ; flag comment
  )) |};
  [%expect
    {|
    (instrumentation (backend ppx_windtrap --coverage ; flag comment
     ))
    |}];
  (* Comment after flag: rewrite name preserves comment. *)
  rewrite
    {| (instrumentation (backend ppx_windtrap --coverage ; flag comment
  )) |}
    ~f:(fun t ->
      Dune_linter.Instrumentation.set_backend
        t
        ~backend:(Dune.Instrumentation.Backend.v "other_windtrap" ~flags:[ "--coverage" ]));
  [%expect
    {|
    (instrumentation (backend other_windtrap --coverage ; flag comment
     ))
    |}];
  (* Comment after flag: remove flag preserves comment. *)
  rewrite
    {| (instrumentation (backend ppx_windtrap --coverage ; flag comment
  )) |}
    ~f:(fun t ->
      Dune_linter.Instrumentation.set_backend
        t
        ~backend:(Dune.Instrumentation.Backend.v "bisect_ppx"));
  [%expect
    {|
    (instrumentation (backend bisect_ppx ; flag comment
     ))
    |}];
  (* Comment between name and flag. *)
  rewrite
    {| (instrumentation (backend bisect_ppx ; a comment
   --flag)) |};
  [%expect
    {|
    (instrumentation (backend bisect_ppx ; a comment
      --flag))
    |}];
  (* Multi-line with comments between components: identity rewrite. *)
  rewrite
    {| (instrumentation (backend
    ;; This is the backend in use
    ppx_windtrap
    ;; This one requires a flag
    --coverage)) |};
  [%expect
    {|
    (instrumentation (backend
       ;; This is the backend in use
       ppx_windtrap
       ;; This one requires a flag
       --coverage))
    |}];
  (* Multi-line with comments: rewrite name only. *)
  rewrite
    {| (instrumentation (backend
    ;; This is the backend in use
    ppx_windtrap
    ;; This one requires a flag
    --coverage)) |}
    ~f:(fun t ->
      Dune_linter.Instrumentation.set_backend
        t
        ~backend:(Dune.Instrumentation.Backend.v "other_windtrap" ~flags:[ "--coverage" ]));
  [%expect
    {|
    (instrumentation (backend
       ;; This is the backend in use
       other_windtrap
       ;; This one requires a flag
       --coverage))
    |}];
  (* Multi-line with comments: remove flag, comments remain. *)
  rewrite
    {| (instrumentation (backend
    ;; This is the backend in use
    ppx_windtrap
    ;; This one requires a flag
    --coverage)) |}
    ~f:(fun t ->
      Dune_linter.Instrumentation.set_backend
        t
        ~backend:(Dune.Instrumentation.Backend.v "bisect_ppx"));
  [%expect
    {|
    (instrumentation (backend
       ;; This is the backend in use
       bisect_ppx))
    |}];
  (* Multi-line with comments: add flag. *)
  rewrite
    {| (instrumentation (backend
    ;; This is the backend in use
    bisect_ppx)) |}
    ~f:(fun t ->
      Dune_linter.Instrumentation.set_backend
        t
        ~backend:(Dune.Instrumentation.Backend.v "ppx_windtrap" ~flags:[ "--coverage" ]));
  [%expect
    {|
    (instrumentation (backend
       ;; This is the backend in use
       ppx_windtrap --coverage))
    |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_linter.Instrumentation.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t =
    Dune_linter.Instrumentation.create
      ~backend:(Dune.Instrumentation.Backend.v "bisect_ppx")
  in
  test t {| (instrumentation (backend other_backend)) |};
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  (* Rewrite currently will do nothing if the stanza to rewrite doesn't have a
     backend. Maybe this is not right? Keeping as characterization test. *)
  test t {| (instrumentation (other field)) |};
  [%expect {| (instrumentation (other field)) |}];
  (* As long as the targeted field is present, it is rewritten. *)
  test t {| (instrumentation (other field) (backend other_backend)) |};
  [%expect {| (instrumentation (other field) (backend bisect_ppx)) |}];
  let windtrap = Dune.Instrumentation.Backend.v "ppx_windtrap" ~flags:[ "--coverage" ] in
  (* Rewriting of stale flags. *)
  let t_windtrap = Dune_linter.Instrumentation.create ~backend:windtrap in
  test t_windtrap {| (instrumentation (backend ppx_windtrap --stale-flag)) |};
  [%expect {| (instrumentation (backend ppx_windtrap --coverage)) |}];
  (* Rewrite a flag that is a sexp list rather than an atom. This is not valid
     dune syntax but exercises the [is_atom_and_equal] guard in [rewrite_flags],
     ensuring the list is correctly detected as different and rewritten. *)
  let t_windtrap = Dune_linter.Instrumentation.create ~backend:windtrap in
  test t_windtrap {| (instrumentation (backend ppx_windtrap (nested flag))) |};
  [%expect {| (instrumentation (backend ppx_windtrap --coverage)) |}];
  ()
;;
