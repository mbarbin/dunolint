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
       | Greater -> 1)
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

  let check_empty_str_exn str ~loc =
    if String.is_empty str then Err.raise ~loc [ Pp.text "Invalid empty pp." ]
  ;;

  let parse str ~applies_to ~loc =
    check_empty_str_exn str ~loc;
    if not (Char.equal str.[0] '-')
    then Pp { pp_name = Dune.Pp.Name.v str }
    else (
      match String.lsplit2 str ~on:'=' with
      | None -> Flag { name = str; param = None; applies_to }
      | Some (name, param) -> Flag { name; param = Some param; applies_to })
  ;;

  let parse_sexp sexp ~applies_to ~loc =
    match (sexp : Sexp.t) with
    | Atom name -> parse ~applies_to ~loc name
    | List _ ->
      Err.raise
        ~loc
        Pp.O.
          [ Pp.text "Unexpected [Sexp.List]. "
            ++ Pp_tty.kwd (module String) "Pps"
            ++ Pp.text " expected to be atoms."
          ]
  ;;

  let to_string = function
    | Pp { pp_name } -> Dune.Pp.Name.to_string pp_name
    | Flag { name; param = None; applies_to = _ } -> name
    | Flag { name; param = Some param; applies_to = _ } -> name ^ "=" ^ param
  ;;
end

module Entry = struct
  type t =
    { arg : Mutable_arg.t
    ; eol_comment : string option
    }
  [@@deriving sexp_of]

  let arg t = t.arg

  let init_fold args ~f =
    let (_ : Mutable_arg.Applies_to.t), entries =
      List.fold_map args ~init:`driver ~f:(fun applies_to arg ->
        let entry = f arg ~applies_to in
        let applies_to =
          match entry.arg with
          | Pp { pp_name } -> `pp pp_name
          | Flag _ -> applies_to
        in
        applies_to, entry)
    in
    entries
  ;;

  let write { arg; eol_comment } =
    let arg = Mutable_arg.to_string arg in
    match eol_comment with
    | None -> arg
    | Some comment -> arg ^ " " ^ comment
  ;;
end

module Section = struct
  type t = { mutable entries : Entry.t list } [@@deriving sexp_of]

  let sorted_entries t =
    List.sort
      t.entries
      ~compare:(Comparable.lift Mutable_arg.order_by_name_and_application ~f:Entry.arg)
  ;;
end

type t = { mutable sections : Section.t list } [@@deriving sexp_of]

let create ~args =
  match
    Entry.init_fold args ~f:(fun arg ~applies_to ->
      let arg = Mutable_arg.of_arg arg ~applies_to in
      { Entry.arg; eol_comment = None })
  with
  | [] -> { sections = [] }
  | _ :: _ as entries -> { sections = [ { entries } ] }
;;

let parse ~loc atoms =
  match
    Entry.init_fold atoms ~f:(fun atom ~applies_to ->
      let arg = Mutable_arg.parse ~applies_to ~loc atom in
      { Entry.arg; eol_comment = None })
  with
  | [] -> { sections = [] }
  | _ :: _ as entries -> { sections = [ { entries } ] }
;;

let field_name = "pps"

let read ~sexps_rewriter ~field =
  let sections =
    Dunolinter.Sections_handler.read_sections_fold
      ~field_name
      ~sexps_rewriter
      ~field
      ~init:`driver
      ~f:(fun applies_to ~original_index:_ ~loc ~source ~arg ->
        let arg = Mutable_arg.parse_sexp ~applies_to ~loc arg in
        let applies_to =
          match arg with
          | Pp { pp_name } -> `pp pp_name
          | Flag _ -> applies_to
        in
        let eol_comment =
          match String.lsplit2 source ~on:';' with
          | None -> None
          | Some (_, comment) -> Some (";" ^ comment)
        in
        applies_to, { Entry.arg; eol_comment })
    |> List.map ~f:(fun entries -> { Section.entries })
  in
  { sections }
;;

let write (t : t) =
  Sexp.List
    (Atom field_name
     :: List.concat_map t.sections ~f:(fun section ->
       List.map (Section.sorted_entries section) ~f:(fun entry ->
         Sexp.Atom (Entry.write entry))))
;;

let rewrite t ~sexps_rewriter ~field =
  Dunolinter.Sections_handler.rewrite_sections
    ~field_name
    ~sexps_rewriter
    ~field
    ~write_arg:Entry.write
    ~sections:(List.map t.sections ~f:Section.sorted_entries)
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

let exists_arg t ~f =
  List.exists t.sections ~f:(fun { entries } ->
    List.exists entries ~f:(fun entry -> f entry.arg))
;;

let eval t ~predicate =
  match (predicate : predicate) with
  | `pp pp_name ->
    exists_arg t ~f:(function
      | Mutable_arg.Pp { pp_name = pp_name' } -> Dune.Pp.Name.equal pp_name pp_name'
      | Flag { name = _; param = _; applies_to = _ } -> false)
    |> Dunolint.Trilang.const
  | `flag { name; param = p_condition; applies_to = a_condition } ->
    exists_arg t ~f:(function
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
    exists_arg t ~f:(function
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

let append_args t ~entries =
  let section =
    match List.last t.sections with
    | Some section -> section
    | None ->
      let section = { Section.entries = [] } in
      t.sections <- [ section ];
      section
  in
  section.entries <- section.entries @ entries
;;

let filter_args t ~f =
  List.iter t.sections ~f:(fun section ->
    section.entries <- List.filter section.entries ~f:(fun entry -> f entry.arg))
;;

let enforce_pp t ~pp_name =
  let handled =
    exists_arg t ~f:(function
      | Mutable_arg.Pp { pp_name = pp_name' } -> Dune.Pp.Name.equal pp_name pp_name'
      | Flag { name = _; param = _; applies_to = _ } -> false)
  in
  if not handled
  then append_args t ~entries:[ { Entry.arg = Pp { pp_name }; eol_comment = None } ]
;;

let enforce_flag t ~flag:{ Dunolint.Dune.Pps.Predicate.Flag.name; param; applies_to } =
  let exception Enforce_failure in
  match
    let handled =
      exists_arg t ~f:(function
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
                 | None ->
                   (* The out-edge of [return] can't be covered. *)
                   Stdlib.raise_notrace Enforce_failure [@coverage off])
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
      | `some ->
        (* The out-edge of [return] can't be covered. *)
        Stdlib.raise_notrace Enforce_failure [@coverage off]
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
        append_args
          t
          ~entries:
            [ { Entry.arg = Flag { name; param; applies_to }; eol_comment = None } ])
  with
  | () -> Dunolinter.Enforce_result.Ok
  | exception Enforce_failure -> Dunolinter.Enforce_result.Fail
;;

let enforce =
  let enforce t predicate : Dunolinter.Enforce_result.t =
    match (predicate : Dune.Pps.Predicate.t Dunolinter.Linter.Predicate.t) with
    | T (`pp pp_name) ->
      enforce_pp t ~pp_name;
      Ok
    | T (`flag flag) -> enforce_flag t ~flag
    | T (`pp_with_flag { pp; flag; param }) ->
      enforce_pp t ~pp_name:pp;
      enforce_flag
        t
        ~flag:{ Dunolint.Dune.Pps.Predicate.Flag.name = flag; param; applies_to = `pp pp }
    | Not (`pp pp) ->
      filter_args t ~f:(function
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
         filter_args t ~f:(function
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
