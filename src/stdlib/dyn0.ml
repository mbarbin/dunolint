(***************************************************************************************)
(*  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(***************************************************************************************)

include Dyn

let inline_record cons fields = Dyn.variant cons [ Dyn.record fields ]

module type Stringable_S = sig
  type t

  val to_string : t -> string
end

let stringable (type a) (module M : Stringable_S with type t = a) (a : a) =
  Dyn.string (M.to_string a)
;;
