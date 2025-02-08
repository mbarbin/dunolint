(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*                                                                               *)
(*  This file is part of Dunolint.                                               *)
(*                                                                               *)
(*  Dunolint is free software; you can redistribute it and/or modify it          *)
(*  under the terms of the GNU Lesser General Public License as published by     *)
(*  the Free Software Foundation either version 3 of the License, or any later   *)
(*  version, with the LGPL-3.0 Linking Exception.                                *)
(*                                                                               *)
(*  Dunolint is distributed in the hope that it will be useful, but WITHOUT      *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*                                                                               *)
(*  You should have received a copy of the GNU Lesser General Public License     *)
(*  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
(*********************************************************************************)

type _ Stdlib.Effect.t +=
  | Enforce_failure :
      { condition : 'a
      ; sexp_of_condition : 'a -> Sexp.t
      ; loc : Loc.t
      }
      -> unit Stdlib.Effect.t

module type Predicate = sig
  type t [@@deriving sexp_of]
end

let enforce_failure
      (type a)
      (module Predicate : Predicate with type t = a)
      ~loc
      ~condition
  =
  Stdlib.Effect.perform
    (Enforce_failure
       { condition; sexp_of_condition = [%sexp_of: Predicate.t Blang.t]; loc })
;;

let emit_error_and_resume a ~loc ~f =
  match f a with
  | r -> r
  | effect Enforce_failure { condition; sexp_of_condition; loc = _ }, k ->
    Err.error
      ~loc
      Pp.O.
        [ Pp.text "Enforce Failure."
        ; Pp.hovbox
            ~indent:2
            (Pp.text "The following condition does not hold:"
             ++ Pp.space
             ++ Pp_tty.tag
                  Details
                  (Pp.verbatim (Sexp.to_string_hum [%sexp (condition : condition)])))
        ; Pp.text
            "Dunolint is able to suggest automatic modifications to satisfy linting \
             rules when a strategy is implemented, however in this case there is none \
             available."
        ]
      ~hints:[ Pp.text "You need to attend and fix manually." ];
    Stdlib.Effect.Deep.continue k ()
;;
