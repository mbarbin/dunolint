# Canonical Ordering

## The Philosophy

Dunolint enforces canonical ordering by default. This is opinionated, and that's intentional.

Dunolint is built on the principle that consistency through automation reduces cognitive load. We understand this approach isn't universal, and that's okay. Different teams have different needs, and we respect that diversity of approaches.

## What Canonical Ordering Provides

For those who do choose to use it, canonical ordering removes an entire category of decisions from daily development:

### No More Nitpicking
When dependencies are always alphabetical, there's nothing to debate in code reviews. The tool decides, you move on.

### Predictable Patterns
AI coding assistants and automated tools work better with consistent patterns. When your codebase follows predictable conventions, these tools can more accurately suggest changes.

### Reduced Merge Conflicts
When everyone adds dependencies in the same order, you're less likely to hit conflicts when multiple developers modify the same build file.

### Visual Scanning
Alphabetically sorted lists are easier to scan. Looking for a specific dependency? You know exactly where to look.

## The Trade-offs

We acknowledge that canonical ordering has trade-offs:

- **Loss of Semantic Grouping**: Sometimes you might prefer to group related dependencies together rather than alphabetize them.
- **Initial Friction**: Existing projects may see many changes on first run.
- **Not Configurable**: These rules cannot be disabled in dunolint.

### Preserving Semantic Groups When Needed

If you need semantic grouping in specific cases, dunolint offers a solution: use section comments. When you place a comment on its own line, dunolint treats it as a section delimiter, preserving your grouping while still sorting within each section. For example:

```dune
(libraries
 ;; Core functionality
 bar
 foo
 ;; Network layer
 baz
 qux
 ;; Testing utilities
 test_lib
 test_utils)
```

This pattern gives you the best of both worlds: automated sorting within logical groups, plus forced documentation of why the grouping exists. See [Comments in Libraries](linting-equilibrium/comments-in-libraries.md) for detailed examples.

## Not a Universal Solution

Canonical ordering isn't about finding the "perfect" style. Like `gofmt` for Go or `black` for Python, it's about having one consistent style that removes decision fatigue.

Some teams prefer manual control over their formatting. Some developers have strong opinions about field ordering. That's valid, and we're not here to convince anyone otherwise.

If you value consistency and automation over manual control, dunolint's canonical ordering might work well for you. If not, there are other tools that might better match your philosophy.

## See Also

- [Default Rules Reference](../reference/default-rules.md) - What gets sorted by default
- [Comments in Libraries](linting-equilibrium/comments-in-libraries.md) - How comments interact with sorting
- [Linting Equilibrium](linting-equilibrium/README.md) - Finding balance with linting tools
