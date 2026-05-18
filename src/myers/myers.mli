(*_*************************************************************************)
(*_  crs-myers - Myers diff computation and unified-diff printing          *)
(*_  Copyright (C) 2026 Mathieu Barbin <mathieu.barbin@gmail.com>          *)
(*_  SPDX-License-Identifier: ISC                                          *)
(*_*************************************************************************)

(*_ Copyright (c) 2026 Invariant Systems. All rights reserved.             *)
(*_ SPDX-License-Identifier: ISC                                           *)

(** Unified-diff renderer built on a vendored Myers shortest-edit-script.

    The renderer is vendored from windtrap; the shortest-edit-script
    computation is vendored from gazagnaire/ocaml-merge3 (kept as a private
    [Merge3] module). See [myers.ml], [merge3.ml] and the root [NOTICE.md] for
    provenance and the list of changes. *)

module type Equal = sig
  type t

  val equal : t -> t -> bool
end

module Line : sig
  type 'a t =
    | Keep of 'a
    | Delete of 'a
    | Insert of 'a
end

(** [compute (module E) before after] returns a shortest edit script from
    [before] to [after], computed with Myers' O(ND) algorithm. *)
val compute : (module Equal with type t = 'a) -> 'a list -> 'a list -> 'a Line.t list

(** [diff expected actual] renders a unified diff for text inputs. When [color]
    is [true], ANSI escape codes are used: cyan for hunk headers, red for
    deletions, green for insertions. *)
val diff
  :  ?context:int
  -> ?color:bool
  -> ?expected_label:string
  -> ?actual_label:string
  -> string
  -> string
  -> string
