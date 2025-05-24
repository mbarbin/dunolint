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

module Arg = struct
  [@@@coverage off]

  type t =
    | Pp of Dune.Pp.Name.t
    | Flag of
        { name : string
        ; param : string option
        }
  [@@deriving equal, sexp_of]
end

module Mutable_arg = struct
  module Applies_to = struct
    type t =
      [ `pp of Dune.Pp.Name.t
      | `driver
      ]
    [@@deriving sexp_of]
  end

  type t =
    | Pp of { mutable pp_name : Dune.Pp.Name.t }
    | Flag of
        { mutable name : string
        ; mutable param : string option
        ; mutable applies_to : Applies_to.t
        }
  [@@deriving sexp_of]

  let order_by_name_and_application t1 t2 =
    match t1, t2 with
    | Pp { pp_name = pp1 }, Pp { pp_name = pp2 } -> Dune.Pp.Name.compare pp1 pp2
    | Pp { pp_name = _ }, Flag { name = _; param = _; applies_to = `driver } -> 1
    | Pp { pp_name = pp1 }, Flag { name = _; param = _; applies_to = `pp pp2 } ->
      (match Dune.Pp.Name.compare pp1 pp2 |> Ordering.of_int with
       | Less | Equal -> -1
       | Greater ->
         (* Due to the value coming from the parsing, a flag may not have been
            assigned to a pp that is located to its right, so we are not
            exercising this in tests. *)
         (1
         [@coverage off]))
    | ( Flag { name = n1; param = _; applies_to = a1 }
      , Flag { name = n2; param = _; applies_to = a2 } ) ->
      (match a1, a2 with
       | `driver, `driver -> String.compare n1 n2
       | `driver, `pp _ -> -1
       | `pp _, `driver -> 1
       | `pp pp1, `pp pp2 ->
         let res = Dune.Pp.Name.compare pp1 pp2 in
         if res <> 0 then res else String.compare n1 n2)
    | Flag { name = _; param = _; applies_to = `driver }, Pp { pp_name = _ } -> -1
    | Flag { name = _; param = _; applies_to = `pp pp1 }, Pp { pp_name = pp2 } ->
      (match Dune.Pp.Name.compare pp1 pp2 |> Ordering.of_int with
       | Less -> -1
       | Equal | Greater -> 1)
  ;;

  let of_arg (arg : Arg.t) ~applies_to =
    match arg with
    | Pp pp_name -> Pp { pp_name }
    | Flag { name; param } -> Flag { name; param; applies_to }
  ;;

  let parse str ~applies_to ~loc =
    if String.is_empty str then Err.raise ~loc [ Pp.text "Invalid empty pp." ];
    if not (Char.equal str.[0] '-')
    then Pp { pp_name = Dune.Pp.Name.v str }
    else (
      match String.lsplit2 str ~on:'=' with
      | None -> Flag { name = str; param = None; applies_to }
      | Some (name, param) -> Flag { name; param = Some param; applies_to })
  ;;

  let to_string t =
    match t with
    | Pp { pp_name } -> Dune.Pp.Name.to_string pp_name
    | Flag { name; param = None; applies_to = _ } -> name
    | Flag { name; param = Some param; applies_to = _ } -> name ^ "=" ^ param
  ;;

  let write t = Sexp.Atom (to_string t)

  let init_fold args ~f =
    let (_ : Applies_to.t), args =
      List.fold_map args ~init:`driver ~f:(fun applies_to arg ->
        let arg = f arg ~applies_to in
        let applies_to =
          match arg with
          | Pp { pp_name } -> `pp pp_name
          | Flag _ -> applies_to
        in
        applies_to, arg)
    in
    args
  ;;
end

type t = { mutable args : Mutable_arg.t list } [@@deriving sexp_of]

let create ~args =
  let args = Mutable_arg.init_fold args ~f:Mutable_arg.of_arg in
  { args }
;;

let parse ~loc atoms =
  let args =
    Mutable_arg.init_fold atoms ~f:(fun atom ~applies_to ->
      Mutable_arg.parse ~applies_to ~loc atom)
  in
  { args }
;;

let field_name = "pps"

let read ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  let args =
    Mutable_arg.init_fold args ~f:(fun arg ~applies_to ->
      let loc = Sexps_rewriter.loc sexps_rewriter arg in
      match arg with
      | Atom name -> Mutable_arg.parse ~applies_to ~loc name
      | List _ ->
        Err.raise
          ~loc
          Pp.O.
            [ Pp.text "Unexpected [Sexp.List]. "
              ++ Pp_tty.kwd (module String) "Pps"
              ++ Pp.text " expected to be atoms."
            ])
  in
  { args }
;;

let write (t : t) =
  let args = List.sort t.args ~compare:Mutable_arg.order_by_name_and_application in
  Sexp.List (Atom field_name :: List.map args ~f:Mutable_arg.write)
;;

let rewrite t ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let last_offset =
    match List.last args with
    | None -> Loc.stop_offset (Sexps_rewriter.loc sexps_rewriter field)
    | Some arg -> (Sexps_rewriter.range sexps_rewriter arg).stop
  in
  let new_args = List.sort t.args ~compare:Mutable_arg.order_by_name_and_application in
  let rec iter_fields args new_args =
    match args, new_args with
    | arg :: args, new_arg :: new_args ->
      File_rewriter.replace
        file_rewriter
        ~range:(Sexps_rewriter.range sexps_rewriter arg)
        ~text:(Mutable_arg.to_string new_arg);
      iter_fields args new_args
    | [], [] -> ()
    | [], _ :: _ ->
      List.iter new_args ~f:(fun new_arg ->
        let value = Mutable_arg.to_string new_arg in
        File_rewriter.insert file_rewriter ~offset:last_offset ~text:(" " ^ value))
    | _ :: _, [] ->
      List.iter args ~f:(fun arg ->
        File_rewriter.remove
          file_rewriter
          ~range:(Sexps_rewriter.range sexps_rewriter arg))
  in
  iter_fields args new_args
;;

type predicate = Dune.Pps.Predicate.t

let eval_param ~param ~p_condition =
  match p_condition with
  | `any -> true
  | `none -> Option.is_none param
  | `some -> Option.is_some param
  | `equals param' ->
    (match param with
     | None -> false
     | Some param -> String.equal param param')
;;

let eval t ~predicate =
  match (predicate : predicate) with
  | `pp pp_name ->
    List.exists t.args ~f:(function
      | Mutable_arg.Pp { pp_name = pp_name' } -> Dune.Pp.Name.equal pp_name pp_name'
      | Flag { name = _; param = _; applies_to = _ } -> false)
    |> Dunolint.Trilang.const
  | `flag { name; param = p_condition; applies_to = a_condition } ->
    List.exists t.args ~f:(function
      | Mutable_arg.Pp { pp_name = _ } -> false
      | Flag { name = name'; param; applies_to } ->
        String.equal name name'
        && eval_param ~param ~p_condition
        &&
          (match a_condition, applies_to with
          | `any, _ | `driver, `driver -> true
          | `driver, `pp _ | `pp _, `driver -> false
          | `pp pp1, `pp pp2 -> Dune.Pp.Name.equal pp1 pp2))
    |> Dunolint.Trilang.const
  | `pp_with_flag { pp; flag; param = p_condition } ->
    List.exists t.args ~f:(function
      | Mutable_arg.Pp { pp_name = _ } -> false
      | Flag { name; param; applies_to } ->
        String.equal flag name
        && eval_param ~param ~p_condition
        &&
          (match applies_to with
          | `driver -> false
          | `pp pp2 -> Dune.Pp.Name.equal pp pp2))
    |> Dunolint.Trilang.const
;;

let enforce =
  let rec enforce t predicate : Dunolinter.Enforce_result.t =
    match (predicate : Dune.Pps.Predicate.t Dunolinter.Linter.Predicate.t) with
    | T (`pp pp_name) ->
      let handled =
        List.exists t.args ~f:(function
          | Mutable_arg.Pp { pp_name = pp_name' } -> Dune.Pp.Name.equal pp_name pp_name'
          | Flag { name = _; param = _; applies_to = _ } -> false)
      in
      if not handled then t.args <- Mutable_arg.Pp { pp_name } :: t.args;
      Ok
    | T (`flag { name; param; applies_to }) ->
      With_return.with_return (fun { return : Dunolinter.Enforce_result.t -> unit } ->
        let handled =
          List.exists t.args ~f:(function
            | Mutable_arg.Pp { pp_name = _ } -> false
            | Flag ({ name = name'; param = _; applies_to = _ } as flag) ->
              if String.equal name name'
              then (
                let () =
                  match param with
                  | `any -> ()
                  | `equals param -> flag.param <- Some param
                  | `none -> flag.param <- None
                  | `some ->
                    (match flag.param with
                     | Some _ -> ()
                     | None -> return Fail)
                in
                let () =
                  match applies_to with
                  | `any -> ()
                  | (`pp _ | `driver) as applies_to -> flag.applies_to <- applies_to
                in
                true)
              else false)
        in
        if not handled
        then (
          match param with
          | `some -> return Fail
          | (`equals _ | `any | `none) as param ->
            let param =
              match param with
              | `equals value -> Some value
              | `any | `none -> None
            in
            let applies_to =
              match applies_to with
              | `any | `driver -> `driver
              | `pp _ as pp -> pp
            in
            t.args <- Mutable_arg.Flag { name; param; applies_to } :: t.args);
        Ok)
    | T (`pp_with_flag { pp; flag; param }) ->
      (match enforce t (T (`pp pp)) with
       | (Eval | Fail | Unapplicable) as break -> break
       | Ok ->
         enforce
           t
           (T
              (`flag
                  { Dunolint.Dune.Pps.Predicate.Flag.name = flag
                  ; param
                  ; applies_to = `pp pp
                  })))
    | Not (`pp pp) ->
      t.args
      <- List.filter t.args ~f:(function
           | Pp { pp_name } -> not (Dune.Pp.Name.equal pp_name pp)
           | Flag { name = _; param = _; applies_to } ->
             (match applies_to with
              | `driver -> true
              | `pp pp_name -> not (Dune.Pp.Name.equal pp_name pp)));
      Ok
    | Not (`flag { name; param; applies_to = a_condition }) ->
      (match param with
       | `none | `some | `equals _ -> Eval
       | `any ->
         t.args
         <- List.filter t.args ~f:(function
              | Pp { pp_name = _ } -> true
              | Flag { name = name'; param = _; applies_to } ->
                if
                  String.equal name name'
                  &&
                  match a_condition, applies_to with
                  | `any, _ | `driver, `driver -> true
                  | `driver, `pp _ | `pp _, `driver -> false
                  | `pp pp1, `pp pp2 -> Dune.Pp.Name.equal pp1 pp2
                then false
                else true);
         Ok)
    | Not (`pp_with_flag _) -> Eval
  in
  Dunolinter.Linter.enforce (module Dune.Pps.Predicate) ~eval ~enforce
;;
