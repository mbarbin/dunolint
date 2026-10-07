(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

type t = { mutable instrumentations : Instrumentation.t list }

let create instrumentations = { instrumentations }
let to_list t = t.instrumentations
let is_empty t = List.is_empty t.instrumentations
let write t = List.map t.instrumentations ~f:Instrumentation.write
let clear t = t.instrumentations <- []

let initialize_if_empty t =
  match t.instrumentations with
  | _ :: _ -> ()
  | [] -> t.instrumentations <- [ Instrumentation.initialize ~condition:Blang.true_ ]
;;

let insert t instrumentation =
  t.instrumentations <- t.instrumentations @ [ instrumentation ]
;;

let find_instrumentation_backend sexps =
  List.find_map sexps ~f:(function
    | Sexp.List (Atom "backend" :: Atom name :: _) ->
      (match Dune.Instrumentation.Backend.Name.of_string name with
       | Ok name -> Some name
       | Error (`Msg _) -> None)
    | _ -> None)
;;

let insertion_overlaps ~present_args ~new_args =
  match
    find_instrumentation_backend present_args, find_instrumentation_backend new_args
  with
  | Some name1, Some name2 -> Dune.Instrumentation.Backend.Name.equal name1 name2
  | _ -> false
;;

let rewrite t ~args =
  match
    match find_instrumentation_backend args with
    | None -> `No_entry
    | Some name ->
      (match
         List.find t.instrumentations ~f:(fun instrumentation ->
           Instrumentation.has_backend_name instrumentation ~name)
       with
       | None -> `No_entry
       | Some instrumentation -> `Rewrite_with instrumentation)
  with
  | `Rewrite_with _ as rewrite -> rewrite
  | `No_entry -> if is_empty t then `Remove_if_marked else `Remove
;;

let find_entry t ~name =
  List.find t.instrumentations ~f:(fun instrumentation ->
    Instrumentation.has_backend_name instrumentation ~name)
;;

(* The entries are seen as a collection: [backend] holds when one of the entries has
   that backend, with the same flags. *)
let eval_predicate t ~(predicate : Dune.Instrumentation.Predicate.t) =
  match predicate with
  | `backend backend ->
    List.exists t.instrumentations ~f:(fun instrumentation ->
      Dune.Instrumentation.Backend.equal backend (Instrumentation.backend instrumentation))
;;

let holds t ~condition =
  Blang.eval condition (fun predicate -> eval_predicate t ~predicate)
;;

let eval t ~condition = Dunolint.Trilang.const (holds t ~condition)

(* A [backend] predicate targets the entry with that backend name, which is added if
   there is none. Existing entries are not renamed. *)
let set_backend t ~backend =
  match find_entry t ~name:(Dune.Instrumentation.Backend.name backend) with
  | Some instrumentation -> Instrumentation.set_backend instrumentation ~backend
  | None -> insert t (Instrumentation.create ~backend)
;;

let enforce_predicates =
  Dunolinter.Linter.enforce
    (module Dune.Instrumentation.Predicate)
    ~eval:(fun t ~predicate -> Dunolint.Trilang.const (eval_predicate t ~predicate))
    ~enforce:(fun t predicate ->
      match predicate with
      | T (`backend backend) ->
        set_backend t ~backend;
        Ok
      | Not (`backend _) -> Eval)
;;

(* Returns whether an enforce failure was raised while running [f], resuming after
   each of them. *)
let has_enforce_failures f =
  match f () with
  | () -> false
  | effect Dunolinter.Handler.Enforce_failure _, k ->
    ignore (Effect.Deep.continue k () : bool);
    true
;;

let enforce t ~condition =
  (* The condition is enforced on a copy of the entries, which replaces them only if the
     enforcement succeeds, so that a failure leaves the entries unchanged. *)
  let candidate =
    { instrumentations = List.map t.instrumentations ~f:Instrumentation.copy }
  in
  let has_failures =
    has_enforce_failures (fun () -> enforce_predicates candidate ~condition)
  in
  (* The enforcement fails if one of its steps did, or if the condition does not hold
     in the end. In this case, the failure is reported once for the whole condition. *)
  if has_failures || not (holds candidate ~condition)
  then
    Dunolinter.Handler.enforce_failure (module Dune.Instrumentation.Predicate) ~condition
  else t.instrumentations <- candidate.instrumentations
;;
