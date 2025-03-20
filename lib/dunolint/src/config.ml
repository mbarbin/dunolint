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

module Skip_subtree = struct
  module Predicate = struct
    type t = [ `path of Path.Predicate.t Blang.t ] [@@deriving compare, equal, sexp]
  end

  module Result = struct
    type t = Nothing.t [@@deriving compare, equal, sexp]
  end

  type t = (Predicate.t, Result.t) Rule.t [@@deriving compare, equal, sexp]
end

module Rule = struct
  type t = (Predicate.t, Condition.t) Rule.t [@@deriving compare, equal, sexp]
end

type t =
  { skip_subtree : Skip_subtree.t option [@sexp.option]
  ; rules : Rule.t list
  }
[@@deriving compare, equal, sexp]

let skip_subtree t = t.skip_subtree
let rules t = t.rules
let create ?skip_subtree ?(rules = []) () = { skip_subtree; rules }

module Std = struct
  module Blang = Blang
  module Dune = Dune
  module Dune_project = Dune_project
  include Blang.O

  let backend p = Blang.base (`backend p)
  let cond clauses = `cond clauses
  let dune p = Blang.base (`dune p)
  let dune_project p = Blang.base (`dune_project p)
  let enforce p = `enforce p
  let equals p = Blang.base (`equals p)
  let executable p = Blang.base (`executable p)
  let flag p = Blang.base (`flag p)
  let generate_opam_files p = Blang.base (`generate_opam_files p)
  let glob p = Blang.base (`glob (Glob.v p))
  let has_field p = Blang.base (`has_field p)
  let has_mode p = Blang.base (`has_mode p)
  let implicit_transitive_deps p = Blang.base (`implicit_transitive_deps p)
  let include_subdirs p = Blang.base (`include_subdirs p)
  let instrumentation p = Blang.base (`instrumentation p)
  let is_prefix p = Blang.base (`is_prefix p)
  let is_suffix p = Blang.base (`is_suffix p)
  let library p = Blang.base (`library p)
  let lint p = Blang.base (`lint p)
  let modes p = Blang.base (`modes p)
  let name p = Blang.base (`name p)
  let no_preprocessing = Blang.base `no_preprocessing
  let path p = Blang.base (`path p)
  let pp p = Blang.base (`pp p)
  let pps p = Blang.base (`pps p)
  let pp_with_flag p = Blang.base (`pp_with_flag p)
  let preprocess p = Blang.base (`preprocess p)
  let public_name p = Blang.base (`public_name p)
  let return = `return
  let skip_subtree = `skip_subtree
  let stanza p = Blang.base (`stanza p)
end
