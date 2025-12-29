# dune-workspace

The selector named `(dune_workspace _)` gives you access to a sub hierarchy of selectors dedicated to stanzas and fields found in *dune-workspace* files.

In this document you'll find the list of all selectors and predicates that live inside the `(dune_workspace _)` hierarchy, along with their meaning and some examples.

## dune_lang_version

`(dune_workspace (dune_lang_version _))` is a selector for the dune-workspace lang stanza:

Stanza:
```dune
(lang dune <VERSION>)
```

For example:
```dune
(lang dune 3.17)
```

:::info Scope of this selector

When using `(dune_workspace (dune_lang_version _))`, the predicates and enforcement rules only apply to the `(lang dune VERSION)` stanza found in *dune-workspace* files. This is distinct from `(dune_project (dune_lang_version _))` which targets dune-project files.

It is a valid configuration for a project to have different dune lang versions in dune-project and dune-workspace files. At the time of writing, the `(lang dune VERSION)` stanza in a dune-workspace file only governs the stanzas found in that very file, whereas the specification in a dune-project file determines the language version for all dune files in that project.

For more details on the semantics of dune-workspace files, refer to the [dune documentation](https://dune.readthedocs.io/).

:::

The predicates, operators, and enforcement behavior are identical to those used for dune-project files. For full details, see the [dune-project dune_lang_version documentation](./dune-project.md#dune_lang_version).
