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

module Executable = Executable
module Flags = Flags
module Include_subdirs = Include_subdirs
module Instrumentation = Instrumentation
module Library = Library
module Libraries = Libraries
module Lint = Lint
module Pps = Pps
module Preprocess = Preprocess

type t =
  { path : Relative_path.t
  ; sexps_rewriter : Sexps_rewriter.t
  }

let create ~(path : Relative_path.t) ~original_contents =
  match Sexps_rewriter.create ~path:(path :> Fpath.t) ~original_contents with
  | Error _ as error -> error
  | Ok sexps_rewriter ->
    let t = { path; sexps_rewriter } in
    Ok t
;;

let contents t = Sexps_rewriter.contents t.sexps_rewriter
let sexps_rewriter t = t.sexps_rewriter
let path t = t.path

module Stanza = struct
  type t = ..
end

type Stanza.t +=
  | Include_subdirs of Include_subdirs.t
  | Library of Library.t
  | Executable of Executable.t
  | Unhandled

module Linter = struct
  let of_stanza
        (type m)
        (module M : Dunolinter.Linter.S
          with type t = m
           and type predicate = Dune.Predicate.t)
        ~(inner_stanza : m)
        ~(stanza : Stanza.t)
        ~path
        ~original_sexp
        ~sexps_rewriter
    =
    let eval (t : m) ~predicate =
      match (predicate : Dunolint.Predicate.t) with
      | `path condition -> Dunolinter.eval_path ~path ~condition
      | `dune_project _ -> Dunolint.Trilang.Undefined
      | `dune condition ->
        Dunolint.Trilang.eval condition ~f:(fun predicate -> M.eval t ~predicate)
    in
    let enforce =
      Dunolinter.Linter.enforce
        (module Dunolint.Predicate)
        ~eval
        ~enforce:(fun t predicate ->
          match predicate with
          | Not _ -> Eval
          | T (`dune_project _ | `path _) -> Unapplicable
          | T (`dune condition) ->
            M.enforce t ~condition;
            Ok)
    in
    let eval predicate = eval inner_stanza ~predicate in
    let enforce condition = enforce inner_stanza ~condition in
    Dunolinter.Private.Stanza.create
      { stanza; path; original_sexp; sexps_rewriter; linter = T { eval; enforce } }
  ;;

  module type S = sig
    type t

    include Dunolinter.Stanza_linter.S with type t := t

    module Linter :
      Dunolinter.Linter.S with type t = t and type predicate = Dune.Predicate.t
  end

  type t =
    | T :
        { impl : (module S with type t = 'a)
        ; wrap : 'a -> Stanza.t
        }
        -> t

  let field_name (T { impl = (module M); _ }) = M.field_name
end

let linters =
  Linter.
    [ T { impl = (module Include_subdirs); wrap = (fun a -> Include_subdirs a) }
    ; T { impl = (module Library); wrap = (fun a -> Library a) }
    ; T { impl = (module Executable); wrap = (fun a -> Executable a) }
    ]
  |> Dunolinter.Linters.create ~field_name:Linter.field_name
;;

let visit t ~f =
  let sexps_rewriter = t.sexps_rewriter in
  let path = t.path in
  List.iter (Sexps_rewriter.original_sexps sexps_rewriter) ~f:(fun original_sexp ->
    match
      match original_sexp with
      | List (Atom field_name :: _) -> Dunolinter.Linters.lookup linters ~field_name
      | _ -> None
    with
    | Some (T { impl = (module M); wrap }) ->
      let inner_stanza = M.read ~sexps_rewriter ~field:original_sexp in
      f
        (Linter.of_stanza
           (module M.Linter)
           ~inner_stanza
           ~stanza:(wrap inner_stanza)
           ~path
           ~original_sexp
           ~sexps_rewriter);
      M.rewrite inner_stanza ~sexps_rewriter ~field:original_sexp
    | None ->
      f
        (Dunolinter.Private.Stanza.create
           { stanza = Unhandled; path; original_sexp; sexps_rewriter; linter = Unhandled }))
;;
