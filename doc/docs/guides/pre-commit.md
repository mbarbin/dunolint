# Using `dunolint` in pre-commit hooks

Dunolint can be used to lint and format dune files automatically before commit, using a [pre-commit](https://pre-commit.com/) hook. This repo contains the configuration for such a hook. It doesn't sandbox the command, so you need to have the `dunolint` command available in your path.

Then, add to the `.pre-commit-config.yaml` of your project:

```yaml
- repo: https://github.com/mbarbin/dunolint
  hooks:
    - id: dunolint
```
