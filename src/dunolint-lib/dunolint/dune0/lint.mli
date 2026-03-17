(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** Configuration for the ["lint"] field.

    This field is found in libraries and executables. It may specify ppx
    rewriters, as well as flags and params. Some examples:

    {v
     (lint (pps ppx_js_style))
     (lint (pps ppx_js_style -check-doc-comments))
    v} *)

module Predicate : sig
  type t =
    [ `pps of Pps.Predicate.t Blang.t
      (** The field is present, it has a [pps] subfield, and this subfield
          verifies the condition that is supplied. *)
    ]

  val equal : t -> t -> bool

  include Sexpable.S with type t := t
end
