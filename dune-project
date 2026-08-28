(lang dune 3.20)

(name dunolint)

(generate_opam_files)

(license "LGPL-3.0-or-later WITH LGPL-3.0-linking-exception")

(authors "Mathieu Barbin")

(maintainers "Mathieu Barbin <opensource@mbarbin.org>")

(source
 (github mbarbin/dunolint))

(documentation "https://mbarbin.github.io/dunolint/")

(implicit_transitive_deps false-if-hidden-includes-supported)

(package
 (name dunolint-lib)
 (synopsis "A library to create dunolint configs")
 (depends
  (ocaml
   (>= 4.14))
  (dyn
   (>= 3.20))
  (re
   (>= 1.8.0))
  (sexplib0
   (>= v0.16))))

(package
 (name dunolint-lib-base)
 (synopsis "An extension of dunolint-lib for use with Base")
 (depends
  (ocaml
   (>= 4.14))
  (base
   (>= v0.16))
  (dunolint-lib
   (= :version))))

(package
 (name dunolint)
 (synopsis "A linter for build files in dune projects")
 (depends
  (ocaml
   (>= 5.3))
  (cmdlang
   (>= 0.0.9))
  (cmdlang-cmdliner-err-runner
   (>= 0.0.16))
  (dune-build-info
   (>= 3.20))
  (dunolint-lib
   (= :version))
  (dyn
   (>= 3.20))
  (file-rewriter
   (>= 0.0.3))
  (fmt
   (>= 0.9.0))
  (fpath
   (>= 0.7.3))
  (fpath-sexp0
   (>= 0.4.0))
  (loc
   (>= 0.3.3))
  (logs
   (>= 0.7.0))
  (ordering
   (>= 3.20))
  (pageantty
   (>= 0.0.3))
  (parsexp
   (>= v0.17))
  (pp
   (>= 2.0.0))
  (pplumbing-err
   (>= 0.0.16))
  (pplumbing-log
   (>= 0.0.16))
  (pplumbing-log-cli
   (>= 0.0.16))
  (pplumbing-pp-tty
   (>= 0.0.16))
  (re
   (>= 1.8.0))
  (sexplib0
   (>= v0.17))
  (sexps-rewriter
   (>= 0.0.3))
  (volgo
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
   (>= 5.3))
  (dune
   (>= 3.20))
  (cmdlang
   (>= 0.0.9))
  (cmdlang-cmdliner-err-runner
   (>= 0.0.16))
  (cmdlang-stdlib-runner
   (>= 0.0.9))
  (cmdliner
   (>= 1.3.0))
  (dune-build-info
   (>= 3.20))
  (dunolint
   (= :version))
  (dunolint-lib
   (= :version))
  (file-rewriter
   (>= 0.0.3))
  (fpath
   (>= 0.7.3))
  (fpath-sexp0
   (>= 0.4.0))
  (loc
   (>= 0.3.3))
  (logs
   (>= 0.7.0))
  (pageantty
   (>= 0.0.3))
  (parsexp
   (>= v0.17))
  (pp
   (>= 2.0.0))
  (pplumbing-err
   (>= 0.0.16))
  (pplumbing-log
   (>= 0.0.16))
  (pplumbing-log-cli
   (>= 0.0.16))
  (pplumbing-pp-tty
   (>= 0.0.16))
  (ppx_expect
   (>= v0.17))
  (re
   (>= 1.8.0))
  (sexplib0
   (>= v0.17))
  (sexps-rewriter
   (>= 0.0.3))
  (volgo
   (>= 0.0.21))
  (volgo-git-unix
   (>= 0.0.21))
  (volgo-hg-unix
   (>= 0.0.21))
  (volgo-vcs
   (>= 0.0.21))))

(package
 (name dunolint-lib-base-tests)
 (synopsis "Tests for dunolint-lib-base")
 (depends
  (ocaml
   (>= 5.3))
  (dunolint-lib
   (= :version))
  (dunolint-lib-base
   (= :version))
  (ppx_expect
   (>= v0.17))))

(package
 (name dunolint-dev)
 (synopsis "Package to regroup dev targets, documentation, and more")
 (allow_empty)
 (depends
  (ocaml
   (>= 5.3))
  (dune
   (>= 3.20))
  (ocamlformat
   (= 0.29.0))
  (bisect_ppx
   (>= 2.8.3))
  (dunolint
   (= :version))
  (dunolint-lib
   (= :version))
  (dunolint-lib-base
   (= :version))
  (dunolint-lib-base-tests
   (= :version))
  (dunolint-tests
   (= :version))
  (ppx_expect
   (>= v0.17))
  (ppx_js_style
   (>= v0.17))
  (volgo
   (>= 0.0.21))
  (volgo-git-unix
   (>= 0.0.21))
  (volgo-hg-unix
   (>= 0.0.21))
  (volgo-vcs
   (>= 0.0.21))
  (odoc
   (>= 2.4.4))
  (sherlodoc
   (>= 0.2))))
