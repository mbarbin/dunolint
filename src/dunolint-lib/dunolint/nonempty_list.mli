(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** A type to represent lists that are statically known to be non-empty.

    The way this constructor is defined allows one to use the regular list
    literal syntax where a non-empty list is expected, for example
    [present [ a; b ]], while [present []] is a type error.

    This module only contains the type definition. Helpers are found in the
    stdlib used by dunolint, where the type is re-exported. *)

type 'a t = ( :: ) of 'a * 'a list
