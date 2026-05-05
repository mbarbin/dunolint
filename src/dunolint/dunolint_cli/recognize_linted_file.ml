(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let recognize basename : Dunolint.Linted_file_kind.t option =
  match Fsegment.to_string basename with
  | "dune" -> Some `dune
  | "dune-project" -> Some `dune_project
  | "dune-workspace" -> Some `dune_workspace
  | "dunolint" -> Some `dunolint
  | str when String.is_prefix str ~prefix:"dune-workspace." -> Some `dune_workspace
  | _ -> None
;;
