(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

val should_skip_subtree
  :  context:Dunolint_engine.Context.t
  -> path:Relative_path.t
  -> bool

val enclosing_dune_lang_version
  :  context:Dunolint_engine.Context.t
  -> path:Relative_path.t
  -> Dune_project.Dune_lang_version.t option

val autoformat_dune_file
  :  context:Dunolint_engine.Context.t
  -> path:Relative_path.t
  -> previous_contents:string
  -> new_contents:string
  -> string

val lint_stanza
  :  path:Relative_path.t
  -> context:Dunolint_engine.Context.t
  -> stanza:'a Dunolinter.Stanza.t
  -> unit

val visit_directory
  :  dunolint_engine:Dunolint_engine.t
  -> context:Dunolint_engine.Context.t
  -> parent_dir:Relative_path.t
  -> files:string list
  -> Dunolint_engine.Visitor_decision.t
