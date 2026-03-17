(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** Types and selectors for linting [dunolint] configuration files.

    This module is named [Dunolint0] rather than [Dunolint] to avoid name
    collision with the library module itself. *)

module Dunolint_lang_version = Dunolint1.Dunolint_lang_version
module Predicate = Dunolint1.Predicate
