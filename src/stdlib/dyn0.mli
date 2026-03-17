(*_**************************************************************************************)
(*_  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*_  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(*_**************************************************************************************)

include module type of struct
  include Dyn
end

val inline_record : string -> (string * Dyn.t) list -> Dyn.t

module type Stringable_S = sig
  type t

  val to_string : t -> string
end

val stringable : (module Stringable_S with type t = 'a) -> 'a -> Dyn.t
