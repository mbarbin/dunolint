(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

module Backend_name_table = MoreLabels.Hashtbl.Make (Dune.Instrumentation.Backend.Name)

(* [removed] holds the backends of the entries removed by an [absent] condition, so that
   their fields are removed from the stanza on rewrite. *)
type t =
  { mutable instrumentations : Instrumentation.t list
  ; mutable removed : unit Backend_name_table.t
  }

let create instrumentations = { instrumentations; removed = Backend_name_table.create 4 }
let to_list t = t.instrumentations
let is_empty t = List.is_empty t.instrumentations
let write t = List.map t.instrumentations ~f:Instrumentation.write
let clear t = t.instrumentations <- []
let default_backend = Dune.Instrumentation.Backend.v "bisect_ppx"

let initialize_if_empty t =
  match t.instrumentations with
  | _ :: _ -> ()
  | [] -> t.instrumentations <- [ Instrumentation.create ~backend:default_backend ]
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

let rewrite t ~args ~marked_for_removal =
  let name = find_instrumentation_backend args in
  match
    Option.bind name ~f:(fun name ->
      List.find t.instrumentations ~f:(fun instrumentation ->
        Instrumentation.has_backend_name instrumentation ~name))
  with
  | Some instrumentation -> `Rewrite_with instrumentation
  | None ->
    if
      marked_for_removal
      || Option.exists name ~f:(fun name -> Backend_name_table.mem t.removed name)
    then `Remove
    else `Keep
;;

let find_entry t ~name =
  List.find t.instrumentations ~f:(fun instrumentation ->
    Instrumentation.has_backend_name instrumentation ~name)
;;

let mem t ~name = Option.is_some (find_entry t ~name)

(* The entries are seen as a collection: [backend] holds when one of the entries has
   that backend, with the same flags, [present] when there is an entry for each of the
   backends, and [absent] when there is none for any of them. *)
let eval_predicate t ~(predicate : Dune.Instrumentation.Predicate.t) =
  match predicate with
  | `backend backend ->
    List.exists t.instrumentations ~f:(fun instrumentation ->
      Dune.Instrumentation.Backend.equal backend (Instrumentation.backend instrumentation))
  | `present names -> Nonempty_list.for_all names ~f:(fun name -> mem t ~name)
  | `absent names -> Nonempty_list.for_all names ~f:(fun name -> not (mem t ~name))
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

let add t ~name =
  if not (mem t ~name)
  then
    insert
      t
      (Instrumentation.create
         ~backend:(Dune.Instrumentation.Backend.create ~name ~flags:[]))
;;

let remove t ~name =
  t.instrumentations
  <- List.filter t.instrumentations ~f:(fun instrumentation ->
       not (Instrumentation.has_backend_name instrumentation ~name));
  Backend_name_table.replace t.removed ~key:name ~data:()
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
      | T (`present names) | Not (`absent ([ _ ] as names)) ->
        Nonempty_list.iter names ~f:(fun name -> add t ~name);
        Ok
      | T (`absent names) | Not (`present ([ _ ] as names)) ->
        Nonempty_list.iter names ~f:(fun name -> remove t ~name);
        Ok
      | Not (`backend _) ->
        (* This could be enforced by removing the field or by changing its flags, and
           neither is clearly the intent. Left as future work. *)
        Eval
      | Not (`present (_ :: _ :: _)) | Not (`absent (_ :: _ :: _)) ->
        (* With more than one backend, the negation of [present] or [absent] only
           requires one of them to be absent (or present), which doesn't determine
           which ones to remove (or add). *)
        Eval)
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
    { instrumentations = List.map t.instrumentations ~f:Instrumentation.copy
    ; removed = Backend_name_table.copy t.removed
    }
  in
  let has_failures =
    has_enforce_failures (fun () -> enforce_predicates candidate ~condition)
  in
  (* The enforcement fails if one of its steps did, or if the condition does not hold
     in the end. In this case, the failure is reported once for the whole condition. *)
  if has_failures || not (holds candidate ~condition)
  then
    Dunolinter.Handler.enforce_failure (module Dune.Instrumentation.Predicate) ~condition
  else (
    t.instrumentations <- candidate.instrumentations;
    t.removed <- candidate.removed)
;;
