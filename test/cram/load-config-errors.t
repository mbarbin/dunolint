In this test we monitor certain error messages when loading invalid configs. In
particular we look for certain regression to make sure errors are located and
messages are clear, especially for certain situations known to be tricky in the
implementation.

Initialize the project root.

  $ touch dune-workspace

Version 0 is no longer supported.

  $ cat > dunolint <<EOF
  > ((version 0))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 1, characters 0-13:
  1 | ((version 0))
      ^^^^^^^^^^^^^
  Error: Dunolint config expected to start with (lang dunolint VERSION).
  [123]

An empty config, with no mention of versions.

  $ printf '\n' > dunolint

  $ dunolint tools config validate dunolint

Conditions are unwrapped in version 1 otherwise here is the error:

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (cond
  >   (((path (glob vendor/*)) return)
  >    (true (enforce (dune_project (name (equals foo))))))))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 5, characters 3-34:
  5 |   (((path (glob vendor/*)) return)
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: predicate.t_of_sexp: a nested list is an invalid polymorphic variant.
  [123]

It it rejected to use the newer lang construct with version 0.

  $ cat > dunolint <<EOF
  > (lang dunolint 0)
  > 
  > ((rules()))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 1, characters 15-16:
  1 | (lang dunolint 0)
                     ^
  Error: Unsupported dunolint config version [0].
  [123]

Unknown versions are rejected.

  $ cat > dunolint <<EOF
  > (lang dunolint unknown)
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 1, characters 15-22:
  1 | (lang dunolint unknown)
                     ^^^^^^^
  Error: Unsupported dunolint config version [unknown].
  [123]

  $ cat > dunolint <<EOF
  > (lang dunolint 142.3)
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 1, characters 15-20:
  1 | (lang dunolint 142.3)
                     ^^^^^
  Error: Unsupported dunolint config version [142.3].
  [123]

  $ cat > dunolint <<EOF
  > (lang dunolint 1)
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 1, characters 15-16:
  1 | (lang dunolint 1)
                     ^
  Error: Unsupported dunolint config version [1].
  [123]

Configs must start with the version.

  $ cat > dunolint <<EOF
  > (rule (enforce (dune (instrumentation (backend bisect_ppx)))))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 1, characters 0-62:
  1 | (rule (enforce (dune (instrumentation (backend bisect_ppx)))))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Dunolint config expected to start with (lang dunolint VERSION).
  [123]

  $ cat > dunolint <<EOF
  > (rule (enforce (dune (instrumentation (backend bisect_ppx)))))
  > (rule (enforce (dune (instrumentation (backend bisect_ppx)))))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 1, characters 0-62:
  1 | (rule (enforce (dune (instrumentation (backend bisect_ppx)))))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Dunolint config expected to start with (lang dunolint VERSION).
  [123]

--- Located errors

In this part of the tests, we monitor that errors are located and that messages
are helpful.

Invalid sexp.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > (
  > (rule (enforce (dune (instrumentation (backend bisect_ppx)))))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 4, characters 0-0:
  Error: unclosed parentheses at end of input
  [123]

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > ())
  > (rule (enforce (dune (instrumentation (backend bisect_ppx)))))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 2, characters 2-2:
  2 | ())
        
  Error: unexpected character: ')'
  [123]

Unknown constructor.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule (enforce (duno (instrumentation (backend bisect_ppx)))))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 3, characters 16-20:
  3 | (rule (enforce (duno (instrumentation (backend bisect_ppx)))))
                      ^^^^
  Error: Unknown construct [duno].
  The constructs available at that level are:
    dune, dune_project, dune_workspace, dunolint, path.
  Hint: did you mean dune?
  [123]

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule (enforce (dune (library (modes (has_modes native))))))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 3, characters 48-54:
  3 | (rule (enforce (dune (library (modes (has_modes native))))))
                                                      ^^^^^^
  Error: list_of_sexp: list needed.
  [123]

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule (enforce (dune (library (modes (has_mode mative))))))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 3, characters 47-53:
  3 | (rule (enforce (dune (library (modes (has_mode mative))))))
                                                     ^^^^^^
  Error: Unknown construct [mative].
  The constructs available at that level are:
    best, byte, melange, native.
  Hint: did you mean native?
  [123]

There was a short period of time where dune lang versions were allowed to be
tuples, but we haved dropped the support for this.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (enforce
  >   (dune_project (dune_lang_version (greater_than_or_equal_to (3 17))))))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 5, characters 61-67:
  5 |   (dune_project (dune_lang_version (greater_than_or_equal_to (3 17))))))
                                                                   ^^^^^^
  Error: Invalid version - expected [MAJOR.MINOR].
  [123]

Located errors for invalid globs.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule (cond ((path (glob "[")) skip_subtree)))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 3, characters 25-28:
  3 | (rule (cond ((path (glob "[")) skip_subtree)))
                               ^^^
  Error: Invalid glob.
  [123]

Located errors for invalid stanzas, or stanzas with invalid args.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (invalid)
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 3, characters 1-8:
  3 | (invalid)
       ^^^^^^^
  Error: Unknown construct [invalid].
  The constructs available at that level are:
    rule, skip_paths.
  [123]

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > atom
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 3, characters 0-4:
  3 | atom
      ^^^^
  Error: Unknown construct [atom].
  The constructs available at that level are:
    rule, skip_paths.
  [123]

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > rule
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 3, characters 0-4:
  3 | rule
      ^^^^
  Error: The construct [rule] expects one or more arguments.
  Hint: Replace by: (rule ARG)
  [123]

Missing argument.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule (enforce (dune (instrumentation backend))))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 3, characters 38-45:
  3 | (rule (enforce (dune (instrumentation backend))))
                                            ^^^^^^^
  Error: The construct [backend] expects one or more arguments.
  Hint: Replace by: (backend ARG)
  [123]
