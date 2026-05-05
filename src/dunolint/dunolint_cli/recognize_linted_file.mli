(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** Recognize the kind of file [dunolint] should lint based on its basename.

    [recognize] is the shared dispatch used by both workspace traversal
    ([dunolint lint]) and explicit single-file linting
    ([dunolint tools lint-file]).

    The recognition pattern is:

    - exact basename match for ["dune"], ["dune-project"], ["dune-workspace"],
      or ["dunolint"];
    - any basename of the form ["dune-workspace.<suffix>"] (e.g.
      ["dune-workspace.ci"], ["dune-workspace.5.3"]). These follow the dune
      manual convention for custom workspace files used in [dune-pkg]
      workflows, and are linted as regular [dune-workspace] files. See
      {{:https://github.com/mbarbin/dunolint/issues/191} #191}.

    Returns [None] when the basename does not match any of the patterns
    above. *)
val recognize : Fsegment.t -> Dunolint.Linted_file_kind.t option
