# Config Language Design

Dunolint's configuration language allows the user to express custom linting rules to apply to the build files of dune projects.

This documents introduces the terminology used in the config language and then explains how to construct and use the configuration language effectively.

## Fragments

A **fragment** is a part of a file that is scrutinized during the linting process. It is loaded from disk, parsed, and is checked against specified expectations.

When a fragment meets the specified requirements, its linting is considered successful, and the linter moves on to check other fragments.

If a fragment does not meet the specifications, a *linting error* occurs. The **predicates** applied during linting may allow dunolint to suggest automatic code modifications to make the fragment compliant. If a suggestion is available, dunolint will present it during interactive linting. You can then decide whether to accept it. If no suggestion is available, dunolint will request that you handle the linting error manually.

### Examples of fragments

Many fields found in dune stanzas are handled as dunolint fragments. For example, consider the following stanzas, simplified from the dunolint project:

```dune
(library
 (name dunolint)
 (public_name dunolint-lib))

(include_subdirs qualified)
```

There are 3 fragments in this file:

1. The library name field `(name dunolint)`
2. Its public name `(public_name dunolint-lib)`
3. The configured value for including subdirs `(include_subdirs qualified)`

Each of these three fragments will be subject to linting rules when applicable.

You can find the exact list of handled fragments in the [config language reference](../../reference/config/README.md).

## Selectors

Configuration rules need to unambiguously specify the fragments to which they apply. To achieve this, the language defines special constructs called **selectors**. Selectors use special keywords to restrict the set of fragments currently under consideration for linting. By chaining these keywords, you can specify a path in the fragment hierarchy that leads to the subset you are targeting, allowing the application of a **predicate** at the leaves.

For example, `(dune (library (name _)))` is a selector that makes a rule applicable only to the first fragment of the dune file mentioned earlier, specifically the name of the library in this dune stanza:

```dune
(library
 (name dunolint))
```

The keywords used in selectors form a tree, and the order in which you chain them matters. For example, the following selector construct is invalid:

`(dune (include_subdirs (name _)))`

because there is no *name* field under the *include_subdirs* stanza. The design of selectors is based on the knowledge of which dune fragments are supported and which fields may be encountered.

## Predicates

Once you've arrived at the inner most level of a selector, it is time to specify a *predicate*: this is a qualifying proposition whose domain is the selected fragment, and which evaluates to *true* or *false*.

`(equals dunolint)` is a example of a predicate defined on a library name, that will return *true* if and only if that name equals "dunolint".

## Conditions

A condition is formed by combining a selector and a predicate, which can then be evaluated against an actual fragment.

Consider the condition: `(dune (library (name (equals dunolint))))`

It is made of the composition of:

1. The selector `(dune (library (name _)))`
2. The predicate `(equals dunolint)`

Its evaluation will return *true* when applied to the following fragment:

```dune
(library
 (name dunolint))
```

and *false* when applied to another fragment like this:

```dune
(library
 (name dunolint_engine))
```

## Trilang, a three values boolean logic

*true*, *false* and *undefined*.

To allow for a predictable and compositional evaluation strategy, we decided to use an explicit representation for conditions whose selectors do not apply to the fragment at hand.

Referring to the examples above, consider the evaluation of the condition `(dune (library (name (equals dunolint))))` when applied to the stanza `(include_subdirs qualified)`. The result is *undefined*.

## Adding blang to the mix

To increase the expressivity of what you can encode in conditions, the language allows conditions and predicates to be intertwined with the *blang* combinators *not*, *and*, *or*, and *if*.

Below we illustrate the evaluation rules used when evaluating blang expressions involving the trilang constants true (*T*), false (*F*), and undefined (*U*). We only feature cases involving *U* inputs; the other cases follow classical blang evaluation:

| Expr         | Trilang |
| ------------ | ------- |
| not(U)       | U       |
| if(U, b, c)  | U       |
| and(F, _)    | F       |
| and(U, T)    | U       |
| and(T, T)    | T       |
| or(T, _)     | T       |
| or(U, F)     | U       |
| or(F, F)     | F       |

### Example

> A library that has a name with the suffix "_test" and enables instrumentation

`(dune (library (and (name (is_suffix _test)) (has_field instrumentation))))`

## Rules

A rule specifies the actions to be taken during the linting of a fragment. Evaluating a rule returns a result among:

- `(enforce INVARIANT)`: The rule specifies an invariant to enforce during linting.
- `return`: stops the evaluation of the rule without trying to enforce any invariant.
- `skip_subtree`: this causes the linter to finish the linting of the current rule, however any remaining rule will be skipped for the fragment a hand, and the entire subtree at point will not be linted.

In addition to these results construct, a conditional control structure is provided in the form of an operator named *cond*.

```ocaml
type ('predicate, 'invariant) t =
  [ `enforce of 'invariant
  | `return
  | `skip_subtree
  | `cond of ('predicate Blang.t * ('predicate, 'invariant) t) list
  ]
[@@deriving compare, equal, sexp]
```

### Evaluation

Rules are evaluated under a context that knows to assign *trilang* values to predicates.

```ocaml
val eval
  :  ('predicate, 'invariant) t
  -> f:('predicate -> Trilang.t)
  -> [ `enforce of 'invariant | `return | `skip_subtree ]
```

**Cond:**

Clauses are evaluated one by one from left to right. Clauses that evaluate to *false* or *undefined* are discarded. If the evaluation returns *true*, then the clause is kept. The evaluation continues recursively with the right hand side of that clause. If no clause is kept, the evaluation is equivalent to *return* -- the rule doesn't select any invariant to enforce.
