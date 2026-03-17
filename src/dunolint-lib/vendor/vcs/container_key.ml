(********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects            *)
(*  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>           *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(*                                                                              *)
(*  This file was vendored from Volgo, whose license header is included below:  *)
(*                                                                              *)
(*  Volgo - a Versatile OCaml Library for Git Operations                        *)
(*  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>           *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(********************************************************************************)

module type S = sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val seeded_hash : int -> t -> int
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

module String_impl = struct
  type t = string

  let compare = String.compare
  let equal = String.equal
  let sexp_of_t = sexp_of_string
  let t_of_sexp = string_of_sexp

  (* [hash] and [seeded_hash] were changed from [String.*] to [Hashtbl.*]
     equivalent in order to achieve compatibility with OCaml 4.14 stdlib. *)
  let hash : string -> int = Hashtbl.hash
  let seeded_hash : int -> string -> int = Hashtbl.seeded_hash
end
