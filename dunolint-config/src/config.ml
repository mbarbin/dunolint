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

open Dunolint.Config.Std

let rules = ref []

type dune_rule = (Dunolint.Predicate.t, Dunolint.Condition.t) Dunolint.Rule.t

let add_rule cs = rules := (cs : dune_rule) :: !rules

let () =
  add_rule
    (cond
       [ ( path (glob "dunolint-config/**/*")
         , enforce (dune (library (public_name (is_prefix "dunolint-tests.")))) )
       ; ( path (glob "lib/test_helpers/src/*")
         , enforce (dune (library (public_name (is_prefix "dunolint-tests.")))) )
       ; ( path (glob "**/test/*")
         , enforce
             (dune
                (library
                   (and_
                      [ public_name (is_prefix "dunolint-tests.")
                      ; name (is_suffix "_test")
                      ]))) )
       ; ( path (or_ [ glob "lib/**/*"; glob "vendor/**/*" ])
         , enforce
             (dune
                (library
                   (public_name
                      (or_
                         [ is_prefix "dunolint."
                         ; is_prefix "dunolint-lib."
                         ; equals (Dune.Library.Public_name.v "dunolint-lib")
                         ])))) )
       ; true_, enforce (dune (library (public_name (is_prefix "dunolint-dev."))))
       ])
;;

let () =
  add_rule
    (cond
       [ ( dune (preprocess (pps true_))
         , enforce
             (dune
                (preprocess
                   (pps
                      (flag
                         { name = "-unused-code-warnings"
                         ; param = `equals "force"
                         ; applies_to = `driver
                         })))) )
       ])
;;

let bisect_ppx = Dune.Instrumentation.Backend.Name.v "bisect_ppx"

let () =
  add_rule
    (cond
       [ ( path (or_ [ glob "vendor/**/*" ])
         , enforce (dune (library (not_ (has_field `instrumentation)))) )
       ; true_, enforce (dune (instrumentation (backend bisect_ppx)))
       ])
;;

let ppx_js_style = Dune.Pp.Name.v "ppx_js_style"

let () =
  add_rule
    (cond
       [ path (or_ [ glob "vendor/blang/*" ]), return
       ; ( true_
         , enforce
             (dune
                (lint
                   (pps
                      (and_
                         [ pp ppx_js_style
                         ; flag
                             { name = "-allow-let-operators"
                             ; param = `none
                             ; applies_to = `pp ppx_js_style
                             }
                         ; flag
                             { name = "-check-doc-comments"
                             ; param = `none
                             ; applies_to = `pp ppx_js_style
                             }
                         ])))) )
       ])
;;

let skip_subtrees = ref []

let add_skip_subtree (condition : Dunolint.Config.Skip_subtree.Predicate.t Blang.t) =
  skip_subtrees := condition :: !skip_subtrees
;;

let () =
  add_skip_subtree
    (or_
       (List.map
          ~f:(fun pat -> path (or_ [ glob ("**/" ^ pat); glob pat ]))
          [ ".git/"
          ; "_build/"
          ; "_opam/"
          ; "_coverage/"
          ; "node_modules/"
          ; "doc/build/"
          ; ".docusaurus/"
          ]))
;;

let config () =
  let skip_subtree = cond [ or_ (List.rev !skip_subtrees), skip_subtree ] in
  let rules = List.rev !rules in
  Dunolint.Config.create ~skip_subtree ~rules ()
;;

let main =
  Command.make
    ~summary:"Dunolint's Dunolint Config"
    (let open Command.Std in
     let+ () = Arg.return () in
     let config = config () in
     print_endline ";; This file is generated by [src/config.ml]. Do not edit!";
     print_s [%sexp (config : Dunolint.Config.t)])
;;
