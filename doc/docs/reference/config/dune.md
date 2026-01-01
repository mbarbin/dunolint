# dune

The selector named `(dune _)` gives you access to a sub hierarchy of selectors dedicated to stanzas and fields found in *dune* files.

In this document you'll find the list of all selectors and predicates that live inside the `(dune _)` hierarchy, along with their meaning and some examples.

## executable

`(dune (executable _))` is a selector for the dune top level stanza named *executable*:

Stanza:
```dune
(executable <FRAGMENT>)
```

:::warning

Beware, although this selector share sub selectors with the *executables* stanzas, its use only selects the *executable* stanza singular.

:::

### name

`(dune (executable (name _)))` is a selector for the *name* field of an *executable* stanza:

Stanza:
```dune
(executable
 (name <FRAGMENT>))
```

Its predicates are:

1. `(equals NAME)`

Returns *true* iif the NAME supplied is an exact match for the name present in the FRAGMENT.

When enforced, *dunolint* suggests to replace any existing name with the one specified by the predicate. Doesn't support suggestion when negated.

2. `(is_prefix PREFIX)`, `(is_suffix SUFFIX)`

Returns *true* iif the name has the given prefix (resp. suffix).

When enforced, *dunolint* suggests prepending or appending the supplied value directly to the existing name, with no special heuristics (it doesn't try to be smart about completing partial prefixes, etc.). You may adjust manually as you see fit.

**Negation**: When the negation of the predicates *is_prefix* and *is_suffix* is enforced, *dunolint* suggests removing the supplied value directly from the name if applicable, if it is currently a prefix (resp. suffix).

**Absent field**: Only `(equals NAME)` can initialize an absent field. Predicates like `(is_prefix _)` and `(is_suffix _)` cannot provide an initial value, so enforcing them on an absent field will fail. Use `(equals NAME)` to initialize the field, or add it manually before running dunolint.

**Examples:**

Stanza with name present:
```dune
(executable
 (name main))
```

Condition: `(dune (executable (name PREDICATE)))`

| Predicate | Result  |
| --------- | ------- |
| (equals main) | True |
| (is_prefix "my_") | False. Suggestion: "my_main" |
| (is_suffix "_test") | False. Suggestion: "main_test" |

Stanza with name absent:
```dune
(executable
 (public_name my-cli))
```

Condition: `(enforce (dune (executable (name PREDICATE))))`

| Predicate | Result  |
| --------- | ------- |
| (equals main) | Field inserted: `(name main)` |
| (is_prefix "my_") | Enforcement failure |
| (is_suffix "_test") | Enforcement failure |

### public_name

`(dune (executable (public_name _)))` is a selector for the *public_name* field of an *executable* stanza:

Stanza:
```dune
(executable
 (public_name <FRAGMENT>))
```

It is almost identical to its *name* sibling, thus we are not documenting it in details here. A notable difference is that the suggestions for the `(is_prefix my-package.)` predicate is improved when the prefix is a package name, as *dunolint* will indeed suggests to *replace* an existing package prefix if one if present. This is a minor ergonomic detail.

### Fields shared with other stanzas

This stanza share some sub selectors with other stanzas. See: *has_field*, *instrumentation*, *lint*, *preprocess*.

For example, you can use the `(dune (executable (instrumentation _)))` syntax if you want the *instrumentation* selector to apply to the *executable* stanza only.

## executables

:::warning

The `(executables _)` stanzas are currently not handled, and are ignored by dunolint. We have plans to fix this early on.

:::

## has_field

`(dune (has_field FIELD))` is a predicate for dune stanzas. If the provided field is applicable to the a given stanza type, then it returns *true* iif the stanza has such field in the dune file, regardless of its contents. When the supplied FIELD doesn't apply to the stanza currently linted, this returns *undefined*.

| Supported FIELDs | Applicable Stanzas
| ---------------- | ------- |
| instrumentation | executable, library
| lint | executable, library
| name | executable, library
| preprocess | executable, library
| public_name | executable, library

When enforced, *dunolint* suggests initializing the field if a default value can be inferred from context. Otherwise *dunolint* will request that you handle the linting error manually.

**Negation**: When the negation of the predicate *has_field* is enforced, *dunolint* suggests removing the field entirely.

**Examples:**

Stanza:
```dune
(library
 (name dunolint-lib)
 (instrumentation (backend bisect_ppx)))
```

Condition: `(dune (library PREDICATE))`

| Predicate | Result  |
| --------- | ------- |
| (has_field instrumentation) | True |
| (has_field preprocess) | False. Suggestion: initialize the field |
| (not (has_field instrumentation)) | False. Suggestion: remove the field |

## if_present

`(if_present (FIELD CONDITION))` is a conditional wrapper that applies a field predicate only when the field is present. If the field is absent, the condition evaluates to *true* and enforcement is skipped (unapplicable).

This is useful when you want to enforce constraints on a field only if it exists, without failing when the field is absent.

**Supported FIELDs:**

| Field | Stanza |
| ----- | ------ |
| public_name | library |
| package | library |

**Behavior:**

- **Eval**: If the field is absent, returns *true*. If the field is present, evaluates the inner condition.
- **Enforce**: If the field is absent, does nothing (unapplicable). If the field is present, enforces the inner condition.

:::tip

Use `if_present` when you want a constraint to apply only to existing fields without trying to create them. This is particularly useful for enforcing naming conventions like prefixes or suffixes on optional fields.

:::

**Examples:**

Stanza without public_name:
```dune
(library
 (name mylib))
```

| Condition | Result |
| --------- | ------ |
| `(if_present (public_name (is_prefix "lib.")))` | True (field absent, so condition is satisfied) |
| `(public_name (is_prefix "lib."))` | Undefined (field absent) |
| `(enforce (if_present (public_name (is_prefix "lib."))))` | No change (unapplicable) |
| `(enforce (public_name (is_prefix "lib.")))` | Enforcement failure |

Stanza with public_name:
```dune
(library
 (name mylib)
 (public_name my-public-lib))
```

| Condition | Result |
| --------- | ------ |
| `(if_present (public_name (is_prefix "lib.")))` | False. Suggestion: "lib.my-public-lib" |
| `(if_present (public_name (is_prefix "my-")))` | True |

**Use case in config:**

```dune
;; Apply prefix constraint only when public_name exists
(rule
 (cond
  ((path (glob src/**))
   (enforce
    (dune
     (library
      (if_present (public_name (is_prefix "mylib.")))))))))
```

Compare with the alternative using `if_` and `has_field`:

```dune
;; Equivalent behavior using if_ and has_field
(rule
 (cond
  ((path (glob src/**))
   (enforce
    (dune
     (library
      (if_ (has_field public_name)
           (public_name (is_prefix "mylib."))
           true_)))))))
```

## include_subdirs

`(dune (include_subdirs _))` is a selector for the dune top level stanza of the same name.

Stanza:
```dune
(include_subdirs FRAGMENT)
```

Its predicates are:

1. `(equals MODE)`

Returns *true* iif the MODE supplied is an exact match for the mode present in the FRAGMENT.

```pre
MODE := no | qualified | unqualified
```

When enforced, *dunolint* suggests to replace any existing mode with the one specified by the predicate. Doesn't support suggestion when negated.

**Examples:**

Stanza:
```dune
(include_subdirs qualified)
```

Condition: `(dune (include_subdirs PREDICATE))`

| Predicate | Result  |
| --------- | ------- |
| (equals qualified) | True |
| (equals unqualified) | False. Suggestion: unqualified |

## instrumentation

`(dune (instrumentation _))` is a selector for the *instrumentation* field found in stanzas *library*, *executable* and *executables*.

Stanza:
```dune
(library
 (instrumentation <FRAGMENT>))
```

Its predicate are:

1. `(backend ARGS)`

Returns *true* iif the ARGS supplied is an exact match for the arguments present in the FRAGMENT.

When enforced, *dunolint* suggests to replace any existing arguments with the one specified by the predicate. Doesn't support suggestion when negated.

**Examples:**

Stanza:
```dune
(library
 (instrumentation (backend bisect_ppx)))
```

Condition: `(dune (library (instrumentation PREDICATE)))`

| Predicate | Result  |
| --------- | ------- |
| (backend bisect_ppx) | True |

## library

`(dune (library _))` is a selector for the dune top level stanza named *library*:

Stanza:
```dune
(library <FRAGMENT>)
```

### name

`(dune (library (name _)))` is a selector for the *name* field of an *library* stanza:

Stanza:
```dune
(library
 (name <FRAGMENT>))
```

It is almost identical to the *name* construct of the *executable* selector, thus we are not documenting it in details here.

### public_name

`(dune (library (public_name _)))` is a selector for the *public_name* field of an *library* stanza:

Stanza:
```dune
(library
 (public_name <FRAGMENT>))
```

It is almost identical to the *public_name* construct of the *executable* selector, thus we are not documenting it in details here.

### has_field

`(dune (library (has_field FIELD)))` is a predicate for checking the presence of fields in a *library* stanza.

All fields from the generic *has_field* selector that are marked as applicable to *library* stanzas are available here. See the [has_field](#has_field) section for details on common fields like *instrumentation*, *lint*, *name*, *preprocess*, and *public_name*.

Additionally, the following library-specific fields are supported:

#### inline_tests

`(dune (library (has_field inline_tests)))` checks for the presence of the *inline_tests* field.

Stanza:
```dune
(library
 (inline_tests))
```

When enforced, *dunolint* suggests adding `(inline_tests)` to the library if not present.

**Negation**: When the negation is enforced, *dunolint* suggests removing the field entirely, including any arguments it may have (e.g., `(inline_tests (deps ./test_data))`).

**Examples:**

Stanza:
```dune
(library
 (name my_test)
 (inline_tests))
```

Condition: `(dune (library PREDICATE))`

| Predicate | Result |
| --------- | ------ |
| (has_field inline_tests) | True |
| (not (has_field inline_tests)) | False. Suggestion: remove the field |

#### package

`(dune (library (has_field package)))` checks for the presence of the *package* field.

Stanza:
```dune
(library
 (package my_package))
```

When enforced, *dunolint* does not suggest adding a field (there is no valid placeholder value). To auto-add a package field, use the `(package (equals PKG))` predicate instead.

**Negation**: When the negation is enforced, *dunolint* suggests removing the field entirely.

**Examples:**

Stanza:
```dune
(library
 (name my_lib)
 (package my_package))
```

Condition: `(dune (library PREDICATE))`

| Predicate | Result |
| --------- | ------ |
| (has_field package) | True |
| (not (has_field package)) | False. Suggestion: remove the field |

### package

`(dune (library (package _)))` is a selector for the *package* field found in *library* stanzas.

Stanza:
```dune
(library
 (package <PACKAGE_NAME>))
```

Its predicates are:

1. `(equals PACKAGE_NAME)`

Returns *true* iff the library's package name is exactly PACKAGE_NAME.

When enforced, *dunolint* suggests setting the package name to the supplied value. If the field is not present, it will be inserted. Doesn't support suggestion when negated.

2. `(is_prefix STRING)` and `(is_suffix STRING)`

Returns *true* iff the library's package name starts (resp. ends) with the specified string.

When enforced with `is_prefix`, *dunolint* suggests prepending the prefix if not already present.
When enforced with `is_suffix`, *dunolint* suggests appending the suffix if not already present.

**Negation**: When the negation is enforced, *dunolint* suggests removing the prefix (resp. suffix) from the package name if present.

**Absent field**: Only `(equals PACKAGE_NAME)` can initialize an absent package field. Predicates like `(is_prefix _)` and `(is_suffix _)` cannot provide an initial value, so enforcing them on an absent field will fail. Use `(equals PACKAGE_NAME)` to initialize the field, or add it manually before running dunolint.

**Examples:**

Stanza with package present:
```dune
(library
 (name my_lib)
 (package pkg))
```

Condition: `(dune (library PREDICATE))`

| Predicate | Result |
| --------- | ------ |
| (package (equals pkg)) | True |
| (package (is_prefix pk)) | True |
| (package (is_suffix kg)) | True |
| (package (is_prefix prefix_)) | False. Suggestion: `(package prefix_pkg)` |
| (package (is_suffix _suffix)) | False. Suggestion: `(package pkg_suffix)` |
| (not (package (is_prefix pk))) | False. Suggestion: `(package g)` |

Stanza with package absent:
```dune
(library
 (name my_lib))
```

Condition: `(enforce (dune (library PREDICATE)))`

| Predicate | Result |
| --------- | ------ |
| (package (equals pkg)) | Field inserted: `(package pkg)` |
| (package (is_prefix prefix_)) | Enforcement failure |
| (package (is_suffix _suffix)) | Enforcement failure |

### Fields shared with other stanzas

This stanza shares some sub selectors with other stanzas. See: *instrumentation*, *lint*, *preprocess*.

For example, you can use the `(dune (library (instrumentation _)))` syntax if you want the *instrumentation* selector to apply to the *library* stanza only.

## lint

`(dune (lint _))` is a selector for the *lint* field found in stanzas *library*, *executable* and *executables*.

Stanza:
```dune
(library
 (lint <FRAGMENT>))
```

Currently, it is required to be followed by a *pps* selector.

## pps

`(dune ({lint|preprocess} (pps _)))` is a selector for the *pps* field found in stanzas *library*, *executable* and *executables*

Stanza:
```dune
(library
 (preprocess (pps <FRAGMENT>)))
```

Its predicates are:

1. `(pp PP_NAME)`

Returns *true* iif the FRAGMENT contains the PP_NAME supplied.

When enforced, *dunolint* suggests to add the pp to the list of arguments, initiating the *preprocess* and *pps* field if needed.

**Negation**: When the negation of the predicate is enforced, *dunolint* suggests removing the pp from the list if present, along with any flags that may be associated with it.

2. `(flag (name FLAG) (param PARAM) (applies_to APPLIES_TO))`

- PARAM: `any | none | some | (equals VALUE)`
- APPLIES_TO: `any | driver | (pp PP_NAME)`

Returns *true* if one of the flags present fits the specification.

Flags usually start with one or two '-' characters. We recommend *dunolint* users to adhere strictly the the `-flag=PARAM` syntax for configuring ppx flags accepting parameters, as this ensure the entire construct is parsed as a single atom.

**Applies_to:**

- Flags may be applied to the ppx driver itself. This is the case for generic flags implemented by ppxlib for example. In this case, the *dunolint* convention is to place them first, right after the `pps` field, and before any ppx. This is the case for the flag `unused-code-warnings` flag below:

```dune
 (preprocess
  (pps
   -unused-code-warnings=force
   ppx_compare
   ppx_enumerate
   ...
  ))
```

- Or flags may be custom constructs targeting a dedicated ppx. In this case, the *dunolint* recommendation is to place them right after the ppx they are targeting. This is the case for the `check-doc-comments` flag below

```dune
(lint
  (pps ppx_js_style -check-doc-comments))
```

**Evaluation:**

Param matching evaluation is pretty self explanatory, *any* matches anything, *none* means no param, *some* means a param with any value, and *equals* expects an exact match for the param value.

It is possible to enforce the `(flag _)` predicate, when the specification is unambiguous as to how to create a new flag if a matching one is not already present (for example, it can't have PARAM=any, etc.). The `(flag _)` predicate may only be negated when `PARAM=any` in which case *dunolint* will suggests removing any matching flag.

3. `(pp_with_flag (pp PP_NAME) (flag FLAG) (param PARAM))`

This is a convenient wrapper for combining the two previous predicates into a single one, to assert the present of a pp with a flag applied to it.

**Examples:**

Stanza:
```dune
(library
 (name dunolint-lib)
 (lint
  (pps ppx_js_style -check-doc-comments)))
```

Condition: `(dune (lint (pps PREDICATE)))`

| Predicate | Result  |
| --------- | ------- |
| (pp ppx_js_style) | True |
| (not (pp ppx_js_style)) | False. Suggestion: remove "ppx_js_style -check-doc-comments" |
| (flag (name -allow-let-operators)(param none)(applies_to (pp ppx_js_style))) | False. Suggestion: add "-allow-let-operators" right after "ppx_js_style" |

## preprocess

`(dune (preprocess _))` is a selector for the *preprocess* field found in stanzas *library*, *executable* and *executables*.

Stanza:
```dune
(library
 (preprocess <FRAGMENT>))
```

It supports two forms:

1. `no_preprocessing`

This is a predicate that returns *true* iif the FRAGMENT is set to exactly this value.

When enforced, *dunolint* suggests to replace any existing setting with that field. Doesn't support suggestion when negated.

2. `(pps _)`

In this case, the construction follows with a `pps` selector.

When enforced, *dunolint* suggests to either replace or initiate a `pps` field based on the enforcement of that selector. Doesn't support suggestion when negated.

## modes

`(dune (library (modes _)))` is a selector for the *modes* field of a *library* stanzas:

Stanza:
```dune
(library
 (modes <FRAGMENT>))
```

The compilation modes supported are
```pre
MODE := byte | native | best | melange
```

Compilation modes are ordered by *dunolint* as: `byte < native < best < melange`.

The predicates of the `modes` selector are:

1. `(has_mode MODE)`

Returns *true* iif the mode specified is present in the list of values found in the fragment.

When enforced, *dunolint* suggests adding the mode to the list of values found in the fragment, if it is not already among the existing values.

**Negation**: When the negation of the predicate *has_mode* is enforced, *dunolint* suggests removing the supplied mode from the fragment if this mode is currently present.

2. `(has_modes MODES)`

This is a shorthand for the conjunction of `(has_mode MODE)` for all the MODES.

**Examples:**

Stanza:
```dune
(library
 (modes byte native))
```

Condition: `(dune (library (modes PREDICATE)))`

| Predicate | Result |
| --------- | ------ |
| (has_modes byte native) | True |
| (has_mode byte) | True |
| (has_mode best) | False. Suggestion: add *best*, keep existing values. |
| (not (has_mode native)) | False. Suggestion: remove *native* |

## stanza

`(stanza KIND)` defines a predicate that evaluates to *true* iif the fragment is located within a stanza of the specified kind.

Supported kinds: *include_subdirs*, *library*, *executable*, *executables*.
