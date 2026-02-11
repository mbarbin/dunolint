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

module Config_with_location = struct
  type t =
    { config : Dunolint.Config.t
    ; location : Relative_path.t
    }
end

module Enclosing_result = struct
  type 'a t = ('a, Dune_project_context.Invalid_dune_project.t) Result.t
end

module Dune_project_context_with_location = struct
  type t =
    { dune_project_context : Dune_project_context.t Enclosing_result.t
    ; location : Relative_path.t
    }
end

module Item = struct
  type t =
    | Config of Config_with_location.t
    | Dune_project_context of Dune_project_context_with_location.t

  open struct
    (* Future plans include making use of the dune linter to keep some
       contextual information (e.g. enclosing library context). This part of the
       project hasn't been written yet.

       In the meanwhile we wish to enforce the right dependency ordering between
       libraries, and silencing @unused-libs warnings. *)
    open! Dune_linter
  end
end

type t = Item.t list

let empty = []

let configs (t : t) =
  (* Return configs in rule processing order: shallowest (root) to deepest.
     Since configs are added at the head as we traverse deeper, we reverse to
     get shallowest first. *)
  List.rev_filter_map t ~f:(function
    | Config config -> Some config
    | Dune_project_context _ -> None)
;;

let add_config t ~config ~location = Item.Config { config; location } :: t

let add_dune_project_context t ~dune_project_context ~location =
  Item.Dune_project_context { dune_project_context; location } :: t
;;

let enclosing_dune_project_context (t : t) =
  List.find_map t ~f:(function
    | Config _ -> None
    | Dune_project_context { dune_project_context; location = _ } ->
      Some dune_project_context)
;;

let enclosing_dune_lang_version (t : t) =
  Option.map (enclosing_dune_project_context t) ~f:(function
    | Error _ as err -> err
    | Ok dune_project_context ->
      Ok (Dune_project_context.dune_lang_version dune_project_context))
;;
