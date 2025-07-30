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

module Comment_handler = Comment_handler
module Enforce_result = Enforce_result
module Handler = Handler
module Linter = Linter
module Linters = Linters
module Ordered_set = Ordered_set
module Sections_handler = Sections_handler
module Sexp_handler = Sexp_handler
module Stanza_linter = Stanza_linter

module Stanza = struct
  type 'a t =
    { stanza : 'a
    ; path : Relative_path.t
    ; original_sexp : Sexp.t
    ; sexps_rewriter : Sexps_rewriter.t
    ; linter : Linter.t
    }
end

module type S = Dunolinter_intf.S with type 'a stanza := 'a Stanza.t

let match_stanza (t : _ Stanza.t) = t.stanza
let path (t : _ Stanza.t) = t.path
let original_sexp (t : _ Stanza.t) = t.original_sexp
let sexps_rewriter (t : _ Stanza.t) = t.sexps_rewriter
let linter (t : _ Stanza.t) = t.linter

let eval_path ~path ~condition =
  Blang.eval condition (function
    | `equals value -> Relative_path.equal path value
    | `glob glob -> Dunolint.Glob.test glob (Relative_path.to_string path))
  |> Dunolint.Trilang.const
;;

module Private = struct
  module Stanza = struct
    module For_create = struct
      type nonrec 'a t = 'a Stanza.t =
        { stanza : 'a
        ; path : Relative_path.t
        ; original_sexp : Sexp.t
        ; sexps_rewriter : Sexps_rewriter.t
        ; linter : Linter.t
        }
    end

    let create t = t
  end
end
