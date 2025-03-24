;; This file is generated by [src/config.ml]. Do not edit!
((skip_subtree
  (cond
   (((or (path (or (glob **/.git/) (glob .git/)))
      (path (or (glob **/_build/) (glob _build/)))
      (path (or (glob **/_opam/) (glob _opam/)))
      (path (or (glob **/_coverage/) (glob _coverage/)))
      (path (or (glob **/node_modules/) (glob node_modules/)))
      (path (or (glob **/doc/build/) (glob doc/build/)))
      (path (or (glob **/.docusaurus/) (glob .docusaurus/))))
     skip_subtree))))
 (rules
  ((cond
    (((path (glob dunolint-config/**/*))
      (enforce (dune (library (public_name (is_prefix dunolint-tests.))))))
     ((path (glob lib/test_helpers/src/*))
      (enforce (dune (library (public_name (is_prefix dunolint-tests.))))))
     ((path (glob **/test/*))
      (enforce
       (dune
        (library
         (and (public_name (is_prefix dunolint-tests.))
          (name (is_suffix _test)))))))
     ((path (or (glob lib/**/*) (glob vendor/**/*)))
      (enforce
       (dune
        (library
         (public_name
          (or (is_prefix dunolint.) (is_prefix dunolint-lib.)
           (equals dunolint-lib)))))))
     (true
      (enforce (dune (library (public_name (is_prefix dunolint-dev.))))))))
   (cond
    (((dune (preprocess (pps true)))
      (enforce
       (dune
        (preprocess
         (pps
          (flag
           ((name -unused-code-warnings) (param (equals force))
            (applies_to driver))))))))))
   (cond
    (((path (glob vendor/**/*))
      (enforce (dune (library (not (has_field instrumentation))))))
     (true (enforce (dune (instrumentation (backend bisect_ppx)))))))
   (cond
    (((path (glob vendor/blang/*)) return)
     (true
      (enforce
       (dune
        (lint
         (pps
          (and (pp ppx_js_style)
           (flag
            ((name -allow-let-operators) (param none)
             (applies_to (pp ppx_js_style))))
           (flag
            ((name -check-doc-comments) (param none)
             (applies_to (pp ppx_js_style)))))))))))))))
