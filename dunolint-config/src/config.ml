(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open Dunolint.Config.V1.Std

let rules = ref []

type dune_rule = (Dunolint.Predicate.t, Dunolint.Condition.t) Dunolint.Rule.t

let rule cs = rules := `rule (cs : dune_rule) :: !rules
let skip_paths globs = rules := `skip_paths (List.map globs ~f:Dunolint.Glob.v) :: !rules

let () =
  (* Documentation pages discuss dune-workspace files and use names like
     [dune-workspace.md] which match the [dune-workspace.<suffix>] recognition
     pattern. They are not actual build files and must not be linted. *)
  skip_paths [ "doc/**/*.md" ]
;;

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
       [ ( path
             (or_
                [ glob "src/stdlib/**"
                ; glob "src/merge3/**"
                ; glob "src/myers/**"
                ; glob "src/dunolint/vendor/**"
                ; glob "src/dunolint-lib/**"
                ; glob "src/dunolint-lib-base/**"
                ; glob "test/dunolint-lib-base/**"
                ])
         , return )
       ; ( true_
         , enforce
             (dune (library (libraries (mem [ Dune.Library.Name.v "dunolint_stdlib" ]))))
         )
       ])
;;

let () =
  rule
    (cond
       [ path (glob "test/**/src/*"), return
       ; ( path (glob "test/**")
         , enforce
             (dune (library (and_ [ name (is_suffix "_test"); has_field `inline_tests ])))
         )
       ])
;;

let () =
  (* Under [test/] and [dunolint-config/] we prefer using the [(package _)]
     stanza rather than having public names that are not going to be used by any
     depending code. All these libraries are tests internals. *)
  rule
    (cond
       [ ( path (glob "test/dunolint-lib-base/*")
         , enforce
             (dune
                (library
                   (and_
                      [ not_ (has_field `public_name)
                      ; package (equals (Dune.Package.Name.v "dunolint-lib-base-tests"))
                      ]))) )
       ; ( path (or_ [ glob "test/**"; glob "dunolint-config/**" ])
         , enforce
             (dune
                (library
                   (and_
                      [ not_ (has_field `public_name)
                      ; package (equals (Dune.Package.Name.v "dunolint-tests"))
                      ]))) )
       ])
;;

let () =
  (* Libraries under [src/] either expose themselves publicly through a prefixed
     [public_name] or remain private to their package via a [(package _)] field.
     The rules below accept either shape so that sub-libraries which are not
     part of dunolint's public API (and therefore not exposed once the package
     is installed) can opt out of having a [public_name]. *)
  rule
    (cond
       [ ( path (glob "src/dunolint-lib/vendor/**")
         , enforce
             (dune
                (library
                   (or_
                      [ public_name (is_prefix "dunolint-lib.")
                      ; package (equals (Dune.Package.Name.v "dunolint-lib"))
                      ]))) )
       ; ( path (glob "src/dunolint-lib/dunolint/*")
         , enforce
             (dune
                (library
                   (public_name (equals (Dune.Library.Public_name.v "dunolint-lib"))))) )
       ; ( path (glob "src/dunolint-lib-base/**")
         , enforce
             (dune
                (library
                   (public_name (equals (Dune.Library.Public_name.v "dunolint-lib-base")))))
         )
       ; ( path (or_ [ glob "src/dunolint/**"; glob "src/stdlib/**" ])
         , enforce
             (dune
                (library
                   (or_
                      [ public_name (is_prefix "dunolint.")
                      ; package (equals (Dune.Package.Name.v "dunolint"))
                      ]))) )
       ; ( true_
         , enforce
             (dune (library (if_present (`public_name (is_prefix "dunolint-dev."))))) )
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

let bisect_ppx = Dune.Instrumentation.Backend.v "bisect_ppx"

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
     print_endline
       (Dunolint.Config.to_file_contents
          config
          ~generated_by:"dunolint-config/src/config.ml"))
;;
