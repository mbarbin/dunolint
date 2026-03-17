(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module type S = sig
  type predicate
  type t [@@deriving sexp_of]

  include Sexp_handler_intf.S with type t := t
  include Linter_intf.S with type t := t and type predicate := predicate
end
