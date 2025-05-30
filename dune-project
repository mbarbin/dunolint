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

(implicit_transitive_deps false)

(package
 (name dunolint-lib)
 (synopsis "A library to create dunolint configs")
 (depends
  (ocaml
   (>= 4.14))
  (base
   (>= v0.16))
  (fpath-sexp0
   (>= 0.3.0))
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
   (and
    (>= v0.17)
    (< v0.18)))
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
   (>= 0.3.0))
  (fpath-sexp0
   (>= 0.3.0))
  (loc
   (>= 0.2.2))
  (logs
   (>= 0.7.0))
  (parsexp
   (and
    (>= v0.17)
    (< v0.18)))
  (patdiff
   (and
    (>= v0.17)
    (< v0.18)))
  (pp
   (>= 2.0.0))
  (pplumbing
   (>= 0.0.13))
  (ppx_compare
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_enumerate
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_hash
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_here
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_let
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_sexp_conv
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_sexp_value
   (and
    (>= v0.17)
    (< v0.18)))
  (ppxlib
   (>= 0.35.0))
  (sexplib0
   (and
    (>= v0.17)
    (< v0.18)))
  (sexps-rewriter
   (>= 0.0.3))
  (stdio
   (and
    (>= v0.17)
    (< v0.18)))))

(package
 (name dunolint-tests)
 (synopsis "Tests for dunolint")
 (depends
  (ocaml
   (>= 5.3))
  (base
   (and
    (>= v0.17)
    (< v0.18)))
  (cmdlang
   (>= 0.0.9))
  (cmdlang-stdlib-runner
   (>= 0.0.9))
  (dunolint
   (= :version))
  (dunolint-lib
   (= :version))
  (expect_test_helpers_core
   (and
    (>= v0.17)
    (< v0.18)))
  (file-rewriter
   (>= 0.0.3))
  (fpath-base
   (>= 0.3.0))
  (fpath-sexp0
   (>= 0.3.0))
  (loc
   (>= 0.2.2))
  (logs
   (>= 0.7.0))
  (parsexp
   (and
    (>= v0.17)
    (< v0.18)))
  (patdiff
   (and
    (>= v0.17)
    (< v0.18)))
  (pp
   (>= 2.0.0))
  (pplumbing
   (>= 0.0.13))
  (ppx_compare
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_enumerate
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_expect
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_hash
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_here
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_let
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_sexp_conv
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_sexp_value
   (and
    (>= v0.17)
    (< v0.18)))
  (ppxlib
   (>= 0.35.0))
  (sexplib0
   (and
    (>= v0.17)
    (< v0.18)))
  (sexps-rewriter
   (>= 0.0.3))
  (stdio
   (and
    (>= v0.17)
    (< v0.18)))
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
   (>= 5.3))
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.27.0)))
  (base
   (and
    (>= v0.17)
    (< v0.18)))
  (bisect_ppx
   (and
    :with-dev-setup
    (>= 2.8.3)))
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
   (and
    (>= v0.17)
    (< v0.18)))
  (file-rewriter
   (>= 0.0.3))
  (fmt
   (>= 0.9.0))
  (fpath
   (>= 0.7.3))
  (fpath-base
   (>= 0.3.0))
  (fpath-sexp0
   (>= 0.3.0))
  (loc
   (>= 0.2.2))
  (logs
   (>= 0.7.0))
  (mdx
   (>= 2.4))
  (parsexp
   (and
    (>= v0.17)
    (< v0.18)))
  (patdiff
   (and
    (>= v0.17)
    (< v0.18)))
  (pp
   (>= 2.0.0))
  (pplumbing
   (>= 0.0.13))
  (ppx_compare
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_enumerate
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_expect
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_hash
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_here
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_js_style
   (and
    :with-dev-setup
    (>= v0.17)
    (< v0.18)))
  (ppx_let
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_sexp_conv
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_sexp_value
   (and
    (>= v0.17)
    (< v0.18)))
  (ppxlib
   (>= 0.35.0))
  (sexplib0
   (and
    (>= v0.17)
    (< v0.18)))
  (sexps-rewriter
   (>= 0.0.3))
  (stdio
   (and
    (>= v0.17)
    (< v0.18)))
  (odoc
   (and
    :with-doc
    (>= 2.4.4)))
  (sherlodoc
   (and
    :with-doc
    (>= 0.2)))))
