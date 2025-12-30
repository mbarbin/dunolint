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

open Dunolint.Config.V1.Std

let rules = ref []

type dune_rule = (Dunolint.Predicate.t, Dunolint.Condition.t) Dunolint.Rule.t

let rule cs = rules := `rule (cs : dune_rule) :: !rules

let () =
  rule
    (enforce
       (dune_project
          (dune_lang_version (gte (Dune_project.Dune_lang_version.create (3, 17))))))
;;

let () =
  rule
    (enforce
       (dunolint
          (dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (1, 0))))))
;;

let () =
  rule
    (enforce
       (dune_workspace
          (dune_lang_version (gte (Dune_workspace.Dune_lang_version.create (3, 20))))))
;;

let () =
  rule
    (cond
       [ path (glob "test/**/src/*"), return
       ; path (glob "test/**"), enforce (dune (library (name (is_suffix "_test"))))
       ])
;;

let () =
  rule
    (cond
       [ path (glob "test/**/src/*"), return
       ; path (glob "test/**"), enforce (dune (library (has_field `inline_tests)))
       ])
;;

let () =
  (* Under [test/] we prefer using the [(package _)] struct rather than having
     public names that are not going to be used by any depending code. At the
     moment there is no dunolint stanza to enforce the presence of the [package]
     construct but if we add one, we'll revisit here. Or perhaps we'll use
     dune's [package.dir] stanza, TBD. *)
  rule
    (cond
       [ path (glob "test/**"), enforce (dune (library (not_ (has_field `public_name)))) ])
;;

let () =
  rule
    (cond
       [ ( path (glob "dunolint-config/**")
         , enforce (dune (library (public_name (is_prefix "dunolint-tests.")))) )
       ; ( path (glob "src/dunolint-lib/**")
         , enforce
             (dune
                (library
                   (public_name
                      (or_
                         [ is_prefix "dunolint-lib."
                         ; equals (Dune.Library.Public_name.v "dunolint-lib")
                         ])))) )
       ; ( path (glob "src/dunolint-lib-base/**")
         , enforce
             (dune
                (library
                   (public_name (equals (Dune.Library.Public_name.v "dunolint-lib-base")))))
         )
       ; ( path (or_ [ glob "src/dunolint/**"; glob "src/stdlib/**" ])
         , enforce (dune (library (public_name (is_prefix "dunolint.")))) )
       ; true_, enforce (dune (library (public_name (is_prefix "dunolint-dev."))))
       ])
;;

let () =
  rule
    (cond
       [ ( dune
             (preprocess
                (pps
                   (or_
                      [ pp (Dune.Pp.Name.v "ppx_compare")
                      ; pp (Dune.Pp.Name.v "ppx_enumerate")
                      ; pp (Dune.Pp.Name.v "ppx_sexp_conv")
                      ])))
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
  rule
    (cond
       [ ( path (or_ [ glob "**/vendor/**" ])
         , enforce (dune (library (not_ (has_field `instrumentation)))) )
       ; true_, enforce (dune (instrumentation (backend bisect_ppx)))
       ])
;;

let ppx_js_style = Dune.Pp.Name.v "ppx_js_style"

let () =
  rule
    (cond
       [ path (or_ [ glob "src/dunolint-lib/vendor/blang/**" ]), return
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

let () =
  rule
    (cond
       [ ( path (or_ [ glob "src/dunolint-lib/dunolint/*" ])
         , enforce (dune (preprocess no_preprocessing)) )
       ])
;;

let config () = Dunolint.Config.v1 (Dunolint.Config.V1.create (List.rev !rules))

let main =
  Command.make
    ~summary:"Dunolint's Dunolint Config."
    (let open Command.Std in
     let+ () = Arg.return () in
     let config = config () in
     print_endline (Dunolint.Config.to_file_contents config ~generated_by:"src/config.ml"))
;;
