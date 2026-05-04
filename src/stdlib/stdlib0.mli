(*_**************************************************************************************)
(*_  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*_  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(*_**************************************************************************************)

(** Extending [Stdlib] for use in the project. *)

module Absolute_path = Absolute_path0
module Code_error = Code_error0
module Dyn = Dyn0
module Err = Err0
module Fpath = Fpath0
module Fsegment = Fsegment0
module In_channel = In_channel0
module Int = Int0
module List = List0
module Loc = Loc0
module Myers = Myers0
module Ordering = Ordering0
module Out_channel = Out_channel0
module Pp = Pp0
module Pp_tty = Pp_tty0
module Relative_path = Relative_path0
module Sexp = Sexp0
module With_equal_and_sexp = With_equal_and_sexp0

val phys_equal : 'a -> 'a -> bool
val print_dyn : Dyn.t -> unit
val require : bool -> unit
val require_equal : (module With_equal_and_sexp.S with type t = 'a) -> 'a -> 'a -> unit
val require_does_raise : (unit -> 'a) -> unit
val print_s : Sexp.t -> unit

(** {1 Transition helpers}

    Re exporting these may help when transitioning from libraries still using
    Base. *)

(** Shadows [Base.print_endline] which is deprecated. *)
val print_endline : string -> unit

(** Shadows [Base.print_string] which is deprecated. *)
val print_string : string -> unit

(** Shadows [Base.prerr_endline] which is deprecated. *)
val prerr_endline : string -> unit
