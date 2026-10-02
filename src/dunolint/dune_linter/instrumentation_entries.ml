(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

type t = Instrumentation.t list

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

let rewrite (t : t) ~args =
  match
    match find_instrumentation_backend args with
    | None -> `No_entry
    | Some name ->
      (match
         List.find t ~f:(fun instrumentation ->
           Instrumentation.has_backend_name instrumentation ~name)
       with
       | None -> `No_entry
       | Some instrumentation -> `Rewrite_with instrumentation)
  with
  | `Rewrite_with _ as rewrite -> rewrite
  | `No_entry -> if List.is_empty t then `Remove_if_marked else `Remove
;;

let find_target_instrumentation (t : t) ~condition =
  match
    Dunolinter.Linter.find_init_value condition ~f:(function `backend backend ->
        Some (Dune.Instrumentation.Backend.name backend))
  with
  | None -> None
  | Some name ->
    List.find t ~f:(fun instrumentation ->
      Instrumentation.has_backend_name instrumentation ~name)
;;

let eval (t : t) ~condition =
  match t with
  | [] -> Dunolint.Trilang.Undefined
  | _ :: _ ->
    (match find_target_instrumentation t ~condition with
     | Some instrumentation ->
       Dunolint.Trilang.eval condition ~f:(fun predicate ->
         Instrumentation.eval instrumentation ~predicate)
     | None ->
       Dunolint.Trilang.exists t ~f:(fun instrumentation ->
         Dunolint.Trilang.eval condition ~f:(fun predicate ->
           Instrumentation.eval instrumentation ~predicate)))
;;

let enforce t ~condition ~insert_instrumentation =
  Dunolinter.Linter.enforce
    (module Dune.Instrumentation.Predicate)
    ~eval:(fun t ~predicate ->
      match predicate with
      | `backend backend ->
        List.exists t ~f:(fun instrumentation ->
          Dune.Instrumentation.Backend.equal
            backend
            (Instrumentation.backend instrumentation))
        |> Dunolint.Trilang.const)
    ~enforce:(fun t predicate ->
      match predicate with
      | Not (`backend _) -> Eval
      | T (`backend backend as predicate) ->
        let name = Dune.Instrumentation.Backend.name backend in
        let instrumentation =
          match
            List.find t ~f:(fun instrumentation ->
              Instrumentation.has_backend_name instrumentation ~name)
          with
          | Some instrumentation -> instrumentation
          | None ->
            (match
               List.find t ~f:(fun instrumentation ->
                 not (Instrumentation.is_pinned instrumentation))
             with
             | Some instrumentation -> instrumentation
             | None ->
               let instrumentation = Instrumentation.create ~backend in
               insert_instrumentation instrumentation;
               instrumentation)
        in
        Instrumentation.enforce instrumentation ~condition:(Blang.base predicate);
        Ok)
    t
    ~condition
;;
