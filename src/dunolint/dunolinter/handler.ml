(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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

module Exn = struct
  exception
    Enforce_failure of
      { loc : Loc.t
      ; condition : Sexp.t
      }

  let () =
    Sexplib0.Sexp_conv.Exn_converter.add
      [%extension_constructor Enforce_failure]
      (function
      | Enforce_failure { loc; condition } ->
        List
          [ Atom "Dunolinter.Handler.Enforce_failure"
          ; List [ Atom "loc"; Loc.sexp_of_t loc ]
          ; List [ Atom "condition"; condition ]
          ]
      | _ -> assert false)
  ;;
end

let raise ~f =
  match f () with
  | r -> r
  | effect Enforce_failure { condition; sexp_of_condition; loc }, k ->
    Stdlib.Effect.Deep.discontinue
      k
      (Exn.Enforce_failure { loc; condition = sexp_of_condition condition })
;;
