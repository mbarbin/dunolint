In this test we monitor behavior related to skip constructs, which is the
ability from the config to instruct dunolint to skip files or entire subtrees.

Initialize the project root.

  $ touch dune-workspace

Let's start with a setup in which rules are applied.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule (enforce (dune_project (name (equals foo)))))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (name bar)
  > EOF

  $ mkdir bar

  $ cat > bar/dune-project <<EOF
  > (lang dune 3.17)
  > (name bar)
  > EOF

With no configuration to skip things, the rule applies to both files.

  $ dunolint lint --dry-run --config dunolint
  dry-run: Would edit file "bar/dune-project":
  -1,2 +1,2
    (lang dune 3.17)
  -|(name bar)
  +|(name foo)
  
  dry-run: Would edit file "dune-project":
  -1,2 +1,2
    (lang dune 3.17)
  -|(name bar)
  +|(name foo)

Let's now check the dedicated functionality with the version 0.

  $ cat > dunolint <<EOF
  > ((version 0)
  >  ((skip_subtree (cond (((path (glob bar/)) skip_subtree))))
  >   (rules ((enforce (dune_project (name (equals foo))))))))
  > EOF

As we can see below, the directory bar/ is not included.

  $ dunolint lint --dry-run --config dunolint
  dry-run: Would edit file "dune-project":
  -1,2 +1,2
    (lang dune 3.17)
  -|(name bar)
  +|(name foo)

Let's now characterize more ways in which we can use skip_subtree construts with
version 0.

First, let's add a construct to skip the file, but placing it after the other
rule.

  $ cat > dunolint <<EOF
  > ((version 0)
  >  ((rules
  >    ((enforce (dune_project (name (equals foo))))
  >     (cond (((path (glob bar/*)) skip_subtree)))))))
  > EOF

As we can see below, the directory bar/ is not included.

  $ dunolint lint --dry-run --config dunolint
  dry-run: Would edit file "dune-project":
  -1,2 +1,2
    (lang dune 3.17)
  -|(name bar)
  +|(name foo)

Why is that? The rewrites enqueued by the enforcement of rules is only flushed
at the end of the application of all rules for each stanza, thus it won't matter
if the rule is place before or after.

  $ cat > dunolint <<EOF
  > ((version 0)
  >  ((rules
  >    ((enforce (dune_project (dune_lang_version (equals 3.18))))
  >     (cond (((path (glob bar/*)) skip_subtree)))))))
  > EOF

The directory bar/ is still not included.

  $ dunolint lint --dry-run --config dunolint
  dry-run: Would edit file "dune-project":
  -1,2 +1,2
  -|(lang dune 3.17)
  +|(lang dune 3.18)
    (name bar)

Let's consider this other scenario in which only the second stanza of the files
matches the condition to raise the [skip_subtree] payload.

  $ cat > dunolint <<EOF
  > ((version 0)
  >  ((rules (
  >    (enforce (dune_project (dune_lang_version (equals 3.18))))
  >    (enforce (dune_project (name (equals foo))))
  >    (cond (((and (dune_project (name true)) (path (glob bar/*))) skip_subtree)))))))
  > EOF

This time the directory bar/ is included for the first rewrite, but not the
second.

  $ dunolint lint --dry-run --config dunolint
  dry-run: Would edit file "bar/dune-project":
  -1,2 +1,2
  -|(lang dune 3.17)
  +|(lang dune 3.18)
    (name bar)
  
  dry-run: Would edit file "dune-project":
  -1,2 +1,2
  -|(lang dune 3.17)
  -|(name bar)
  +|(lang dune 3.18)
  +|(name foo)

This is quite confusing. Note that [skip_subtree] from rules is deprecated in
version 1 so this won't be an issue going foward.

Now let's go back to simpler case. The rule to skip the file comes first, that
file should not be included.

  $ cat > dunolint <<EOF
  > ((version 0)
  >  ((rules
  >    ((cond (((path (glob bar/*)) skip_subtree)))
  >     (enforce (dune_project (name (equals foo))))))))
  > EOF

As we can see below, the directory bar/ is not included.

  $ dunolint lint --dry-run --config dunolint
  dry-run: Would edit file "dune-project":
  -1,2 +1,2
    (lang dune 3.17)
  -|(name bar)
  +|(name foo)

We are monitoring a tricky point here in that some versions of dunolint would
not evaluate the rules in the context of the containing directory first, and
thus the glob "bar/" is not a match for the file, the rule ends up not applying.

  $ cat > dunolint <<EOF
  > ((version 0)
  >  ((rules
  >    ((cond (((path (glob bar/)) skip_subtree)))
  >     (enforce (dune_project (name (equals foo))))))))
  > EOF

Currently both files ends up included, instead of only the root one. We plan on
fixing this, this is left as regression test.

  $ dunolint lint --dry-run --config dunolint
  dry-run: Would edit file "bar/dune-project":
  -1,2 +1,2
    (lang dune 3.17)
  -|(name bar)
  +|(name foo)
  
  dry-run: Would edit file "dune-project":
  -1,2 +1,2
    (lang dune 3.17)
  -|(name bar)
  +|(name foo)

OK let's now do similar tests with the version 1 of the config.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (skip_paths bar/)
  > 
  > (rule (enforce (dune_project (name (equals foo)))))
  > EOF

As we can see below, the directory bar/ is not included.

  $ dunolint lint --dry-run --config dunolint
  dry-run: Would edit file "dune-project":
  -1,2 +1,2
    (lang dune 3.17)
  -|(name bar)
  +|(name foo)

With version 1, the `skip_paths` construct may be placed anywhere, they will
always be applied first.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule (enforce (dune_project (name (equals foo)))))
  > 
  > (skip_paths bar/)
  > EOF

As we can see below, the directory bar/ is not included.

  $ dunolint lint --dry-run --config dunolint
  dry-run: Would edit file "dune-project":
  -1,2 +1,2
    (lang dune 3.17)
  -|(name bar)
  +|(name foo)

Because [skip_subtree] from rules was too confusing, it is deprecated from
version 1.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule (enforce (dune_project (name (equals foo)))))
  > 
  > (rule (cond ((path (glob bar/*)) skip_subtree)))
  > EOF

Attempting to use the construct results in an error:

  $ dunolint lint --dry-run --config dunolint
  File "dunolint", line 5, characters 33-45:
  5 | (rule (cond ((path (glob bar/*)) skip_subtree)))
                                       ^^^^^^^^^^^^
  Error: The [skip_subtree] construct is not allowed in version 1 of dunolint
  config.
  [123]

Next we are going to characterize some behavior of the constructs `skip_subtree`
and `skip_paths` when linting a file in particular.

With version 0, there was a bug when linting one file that is in a skip subtree.
This is monitored by this test. The file shouldn't be linted. The bug was due to
matching complete file path, rather than its directories.

  $ cat > dunolint <<EOF
  > ((version 0)
  >  ((skip_subtree (cond (((path (glob bar/)) skip_subtree))))
  >   (rules ((enforce (dune_project (name (equals foo))))))))
  > EOF

  $ dunolint tools lint-file bar/dune-project --config=dunolint
  (lang dune 3.17)
  (name bar)

As a reminder, the original file has (name bar), thus showing "foo" as result
would mean that the file was linted even though it shouldn't have been.

  $ cat bar/dune-project
  (lang dune 3.17)
  (name bar)

The same bug was happening with version 1 and was fixed as well.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule (enforce (dune_project (name (equals foo)))))
  > 
  > (skip_paths bar/)
  > EOF

  $ dunolint tools lint-file bar/dune-project --config=dunolint
  (lang dune 3.17)
  (name bar)

Another somewhat related bug which as been fixed too. When linting directories,
if we go directly below a nested subdirectory in a skip subtree, we miss the
match and end up linting files that shouldn't be. This is a bug.

  $ mkdir bar/bin

  $ cat > bar/bin/dune-project <<EOF
  > (lang dune 3.17)
  > (name bar)
  > EOF

When linting from the root, bar/ is correctly ignored.

  $ dunolint lint --dry-run --config=dunolint
  dry-run: Would edit file "dune-project":
  -1,2 +1,2
    (lang dune 3.17)
  -|(name bar)
  +|(name foo)

Going straight to bar/bin shouldn't cause this directory to be linted. This bug
has been fixed - see below that no lint occurs.

  $ dunolint lint --dry-run --config=dunolint --below=bar/bin

In version 1, the skip_paths globs is matched against directories and files.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule (enforce (dune_project (name (equals foo)))))
  > 
  > (skip_paths bar/dune-project)
  > EOF

  $ dunolint tools lint-file bar/dune-project --config=dunolint
  (lang dune 3.17)
  (name bar)

This is similar to the behavior with version 0:

  $ cat > dunolint <<EOF
  > ((version 0)
  >  ((skip_subtree (cond (((path (glob bar/dune-project)) skip_subtree))))
  >   (rules ((enforce (dune_project (name (equals foo))))))))
  > EOF

  $ dunolint tools lint-file bar/dune-project --config=dunolint
  (lang dune 3.17)
  (name bar)

If the pattern used a star, this is the same:

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule (enforce (dune_project (name (equals foo)))))
  > 
  > (skip_paths bar/*)
  > EOF

  $ dunolint tools lint-file bar/dune-project --config=dunolint
  (lang dune 3.17)
  (name bar)
