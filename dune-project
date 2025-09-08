(lang dune 3.17)

(name dunolint)

(generate_opam_files)

(license "LGPL-3.0-or-later WITH LGPL-3.0-linking-exception")

(authors "Mathieu Barbin")

(maintainers "Mathieu Barbin <opensource@mbarbin.org>")

(source
 (github mbarbin/dunolint))

(documentation "https://mbarbin.github.io/dunolint/")

(using mdx 0.4)

;; The value for the [implicit_transtive_deps] option is set during the CI
;; depending on the OCaml compiler version.
;;
;; This will be set to [false] iif [ocaml-version >= 5.2].
;;
;; For packaging purposes with older ocaml, it is simpler atm if the option is
;; set to [true] in the main branch.
;;
;; See: [.github/workflows/edit_dune_project_dot_ml].

(implicit_transitive_deps true)

(package
 (name dunolint-lib)
 (synopsis "A library to create dunolint configs")
 (depends
  (ocaml
   (>= 4.14))
  (base
   (>= v0.16))
  (fpath
   (>= 0.7.3))
  (fpath-sexp0
   (>= 0.3.1))
  (ppx_compare
   (>= v0.16))
  (ppx_enumerate
   (>= v0.16))
  (ppx_sexp_conv
   (>= v0.16))
  (ppxlib
   (>= 0.35.0))
  (re
   (>= 1.8.0))
  (sexplib0
   (>= v0.16))))

(package
 (name dunolint)
 (synopsis "A linter for build files in dune projects")
 (depends
  (ocaml
   (>= 5.3))
  (base
   (>= v0.17))
  (cmdlang
   (>= 0.0.9))
  (dunolint-lib
   (= :version))
  (file-rewriter
   (>= 0.0.3))
  (fmt
   (>= 0.9.0))
  (fpath
   (>= 0.7.3))
  (fpath-base
   (>= 0.3.1))
  (fpath-sexp0
   (>= 0.3.1))
  (loc
   (>= 0.2.2))
  (logs
   (>= 0.7.0))
  (pageantty
   (>= 0.0.2))
  (parsexp
   (>= v0.17))
  (patdiff
   (>= v0.17))
  (pp
   (>= 2.0.0))
  (pplumbing
   (>= 0.0.14))
  (ppx_compare
   (>= v0.17))
  (ppx_enumerate
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_let
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.35.0))
  (re
   (>= 1.8.0))
  (sexplib0
   (>= v0.17))
  (sexps-rewriter
   (>= 0.0.3))
  (stdio
   (>= v0.17))
  (volgo
   (>= 0.0.21))
  (volgo-base
   (>= 0.0.21))
  (volgo-git-unix
   (>= 0.0.21))
  (volgo-hg-unix
   (>= 0.0.21))))

(package
 (name dunolint-tests)
 (synopsis "Tests for dunolint")
 (depends
  (ocaml
   (and
    (>= 5.3)
    (< 5.4~)))
  (dune
   (>= 3.20))
  (base
   (>= v0.17))
  (cmdlang
   (>= 0.0.9))
  (cmdlang-stdlib-runner
   (>= 0.0.9))
  (dunolint
   (= :version))
  (dunolint-lib
   (= :version))
  (expect_test_helpers_core
   (>= v0.17))
  (file-rewriter
   (>= 0.0.3))
  (fpath
   (>= 0.7.3))
  (fpath-base
   (>= 0.3.1))
  (fpath-sexp0
   (>= 0.3.1))
  (loc
   (>= 0.2.2))
  (logs
   (>= 0.7.0))
  (pageantty
   (>= 0.0.2))
  (parsexp
   (>= v0.17))
  (patdiff
   (>= v0.17))
  (pp
   (>= 2.0.0))
  (pplumbing
   (>= 0.0.14))
  (ppx_compare
   (>= v0.17))
  (ppx_enumerate
   (>= v0.17))
  (ppx_expect
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_let
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.35.0))
  (re
   (>= 1.8.0))
  (sexplib0
   (>= v0.17))
  (sexps-rewriter
   (>= 0.0.3))
  (stdio
   (>= v0.17))
  (volgo
   (>= 0.0.21))
  (volgo-base
   (>= 0.0.21))
  (volgo-git-unix
   (>= 0.0.21))
  (volgo-hg-unix
   (>= 0.0.21))
  (volgo-vcs
   (>= 0.0.21))
  (sherlodoc
   (and
    :with-doc
    (>= 0.2)))))

(package
 (name dunolint-dev)
 (synopsis
  "Package to regroup dev targets for the dunolint project, documentation, and more")
 (allow_empty) ; The package is attached to stanza in doc/ however dune doesn't see it.
 (depends
  (ocaml
   (and
    (>= 5.3)
    (< 5.4~)))
  (dune
   (>= 3.20))
  (ocamlformat
   (= 0.27.0))
  (base
   (>= v0.17))
  (bisect_ppx
   (>= 2.8.3))
  (cmdlang
   (>= 0.0.9))
  (cmdlang-stdlib-runner
   (>= 0.0.9))
  (dunolint
   (= :version))
  (dunolint-lib
   (= :version))
  (dunolint-tests
   (= :version))
  (expect_test_helpers_core
   (>= v0.17))
  (file-rewriter
   (>= 0.0.3))
  (fmt
   (>= 0.9.0))
  (fpath
   (>= 0.7.3))
  (fpath-base
   (>= 0.3.1))
  (fpath-sexp0
   (>= 0.3.1))
  (loc
   (>= 0.2.2))
  (logs
   (>= 0.7.0))
  (mdx
   (>= 2.4))
  (pageantty
   (>= 0.0.2))
  (parsexp
   (>= v0.17))
  (patdiff
   (>= v0.17))
  (pp
   (>= 2.0.0))
  (pplumbing
   (>= 0.0.14))
  (ppx_compare
   (>= v0.17))
  (ppx_enumerate
   (>= v0.17))
  (ppx_expect
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_js_style
   (>= v0.17))
  (ppx_let
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.35.0))
  (re
   (>= 1.8.0))
  (sexplib0
   (>= v0.17))
  (sexps-rewriter
   (>= 0.0.3))
  (stdio
   (>= v0.17))
  (volgo
   (>= 0.0.21))
  (volgo-base
   (>= 0.0.21))
  (volgo-git-unix
   (>= 0.0.21))
  (volgo-hg-unix
   (>= 0.0.21))
  (volgo-vcs
   (>= 0.0.21))
  (odoc
   (and
    :with-doc
    (>= 2.4.4)))
  (sherlodoc
   (and
    :with-doc
    (>= 0.2)))))
