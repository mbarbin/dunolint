(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** Helpers for dunolint tests.

    This module defines utils used by the tests of dunolint. It is not meant to
    be exported outside of the dunolint project, and thus is simply named
    "Test_helpers" rather than have a dunolint part in its name. *)

(** {1 Parsing Fields} *)

val read_sexp_field : path:Fpath.t -> string -> Sexps_rewriter.t * Sexp.t

val parse
  :  (module Dunolinter.Sexp_handler.S with type t = 'a)
  -> path:Fpath.t
  -> string
  -> (Sexps_rewriter.t * Sexp.t) * 'a

(** {1 Trilang} *)

val is_true : Dunolint.Trilang.t -> unit
val is_false : Dunolint.Trilang.t -> unit
val is_undefined : Dunolint.Trilang.t -> unit

(** {1 Linting} *)

(** A helper to run more or less the equivalent of the [lint] command, given a
    configuration. This runs the dunolint engine in dry-run mode, which would
    print diffs on the standard output. This is meant to be run from within a
    call to [Err.For_test.protect], such as, for example:

    {[
      Err.For_test.protct (fun () -> Test_helpers.run_linter ~config);
      [%expect {||}]
    ]} *)
val run_linter : config:Dunolint.Config.t -> unit
