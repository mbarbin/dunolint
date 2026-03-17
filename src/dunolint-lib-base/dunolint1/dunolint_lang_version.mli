(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type t = Dunolint.Dunolint0.Dunolint_lang_version.t

include module type of Dunolint.Dunolint0.Dunolint_lang_version with type t := t
include Comparable.S with type t := t
