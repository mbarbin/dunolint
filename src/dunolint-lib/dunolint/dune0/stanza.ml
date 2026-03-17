(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

module Predicate = struct
  let error_source = "stanza.t"

  type t =
    [ `include_subdirs
    | `library
    | `executable
    | `executables
    ]

  let equal = (Stdlib.( = ) : t -> t -> bool)

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "include_subdirs"; conv = Nullary `include_subdirs }
    ; { atom = "library"; conv = Nullary `library }
    ; { atom = "executable"; conv = Nullary `executable }
    ; { atom = "executables"; conv = Nullary `executables }
    ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    Atom
      (match t with
       | `include_subdirs -> "include_subdirs"
       | `library -> "library"
       | `executable -> "executable"
       | `executables -> "executables")
  ;;
end
