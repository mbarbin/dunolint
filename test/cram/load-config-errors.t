In this test we monitor certain error messages when loading invalid configs. In
particular we look for certain regression to make sure errors are located and
messages are clear, especially for certain situation known to be tricky in the
implementation.

Version 0 with no fields.

  $ cat > dunolint <<EOF
  > ((version 0))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 1, characters 0-13:
  1 | ((version 0))
      ^^^^^^^^^^^^^
  Error: config.v0.t_of_sexp: extra fields: version.
  [123]

Higher versions, using the version syntax.

  $ cat > dunolint <<EOF
  > ((version 1) _)
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 1, characters 10-11:
  1 | ((version 1) _)
                ^
  Error: The (version _) syntax is only supported with version 0.
  [123]

An empty config, with no mention of versions.

  $ cat > dunolint <<EOF
  > ()
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 1, characters 0-2:
  1 | ()
      ^^
  Error: Dunolint config expected to start with (lang dunolint VERSION).
  [123]

Conditions are wrapped in the version 0 and unwrapped in version 1.

Here is the error if using unwrapped at version 0:

  $ cat > dunolint <<EOF
  > ((version 0)
  >  ((skip_subtree (cond (((path (glob .git/*)) skip_subtree))))
  >   (rules
  >    ((cond
  >      ((path (glob vendor/*)) return)
  >      (true (enforce (dune_project (name (equals foo))))))))))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", lines 4-6, characters 4-104:
  4 |    ((cond
  5 |      ((path (glob vendor/*)) return)
  6 |      (true (enforce (dune_project (name (equals foo))))))))))
  Error: rule.v0.t_of_sexp: polymorphic variant tag "cond" has incorrect number
  of arguments.
  [123]

And here is the error if using wrapped at version 1:

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
  File "dunolint", line 3, characters 15-60:
  3 | (rule (enforce (duno (instrumentation (backend bisect_ppx)))))
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: predicate.t_of_sexp: no matching variant found.
  [123]

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule (enforce (dune (library (modes (has_modes modo))))))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 3, characters 48-52:
  3 | (rule (enforce (dune (library (modes (has_modes modo))))))
                                                      ^^^^
  Error: list_of_sexp: list needed.
  [123]

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule (enforce (dune (library (modes (has_mode modo))))))
  > EOF

  $ dunolint tools config validate dunolint
  File "dunolint", line 3, characters 47-51:
  3 | (rule (enforce (dune (library (modes (has_mode modo))))))
                                                     ^^^^
  Error: compilation_mode.t_of_sexp: no matching variant found.
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
  File "dunolint", line 3, characters 0-9:
  3 | (invalid)
      ^^^^^^^^^
  Error: config.v1.stanza.t_of_sexp: no matching variant found.
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
  Error: config.v1.stanza.t_of_sexp: no matching variant found.
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
  Error: config.v1.stanza.t_of_sexp: polymorphic variant tag takes an argument.
  [123]

