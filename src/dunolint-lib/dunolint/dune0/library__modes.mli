(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** Configuration for the ["modes"] field of library.

    Some examples as found in dune files:

    {v
      (modes byte)
      (modes byte native)
      (modes best)
      (modes :standard melange)
    v} *)

module Predicate : sig
  (** A very crucial design points here is that the predicates are syntactic.
      They do not talk about the evaluation of the ordered set, but refer to
      what is written in the dune file, literally.

      So, for example even if the evaluation of the [:standard] mode includes
      [byte], evaluating: [`mem `byte] on the input [(:standard)] returns
      [false].

      The reason is that dunolint focuses on linting what the user writes in the
      dune files, as opposed to how dune interprets it. *)

  (** These names are deprecated and will be removed by a future upgrade. Do not
      use in new code and migrate at your earliest convenience. Use [`mem]
      instead. *)
  type deprecated_names =
    [ `has_mode of Compilation_mode.t
    | `has_modes of Compilation_mode.t list
    ]

  type t =
    [ `mem of Compilation_mode.t list
    | deprecated_names
    ]

  val equal : t -> t -> bool

  include Sexpable.S with type t := t
end
