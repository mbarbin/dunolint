## 0.0.20260211 (2026-02-11)

Starting from this version we're using GitHub immutable releases.

### Added

- Add library modes `mem` variadic construct (#174, @mbarbin).
- Adapt release artifacts jobs for immutable releases (@mbarbin).
- Added new CI workflow based on setup-dune (@mbarbin).
- Add some library and executable linter getters/setters (#163, @mbarbin).

### Changed

- Extract dune lang format directly from enclosing context (#177, @mbarbin).
- Support supplying flags to instrumentation backends (#175, @mbarbin).
- Migrate main and doc CIs to `setup-dune` (#171, @mbarbin).
- Assorted improvements to CI scripts. Upgrade an pin actions deps (#173, @mbarbin).
- Generalize `Dunolint_engine.with_linter` helper (#163, @mbarbin).

### Deprecated

- Deprecate `has_mode` and `has_modes` library modes, replaced by `mem` (#172, @mbarbin).

### Fixed

- Fix new dune build target `unused-libs` (#171, @mbarbin).
- Fix tests and prepare compatibility with `dune.3.21` (#167, @mbarbin).

### Removed

- Removed deprecated `load_existing_libraries` private helper (#163, @mbarbin).
- Removed root `dune-workspace` causing build issues with `dune.3.17`. (@mbarbin).

## 0.0.20260103 (2026-01-03)

### Added

- Show supported candidates in invalid construct sexp error (#162, @mbarbin).
- Add initial support for `package` predicates in libraries (#161, @mbarbin).
- Add new qualifier `if_present` to enforce only if field is present (#159, @mbarbin).
- Add predicates for the `(inline_tests)` library stanza (#157, @mbarbin).
- Add support for `DUNE_ROOT` environment variable (#156, @mbarbin).
- Add initial support for linting `dune-workspace` files (#153, @mbarbin).
- Add initial support for linting `dunolint` files (#146, @mbarbin).
- Enabled OCaml `5.4` in CI (#136, @mbarbin).
- Added new tests (#132, @mbarbin).

### Changed

- Improve tests for sexp and equal functions and their coverage (#162, @mbarbin).
- Support multiple input files in `tools lint-file --in-place` (#158, @mbarbin).
- Make a few dunolint config parsing errors more friendly (#155, @mbarbin).
- Refactor sexp parsing - prerequisite for future improvements (#152, @mbarbin).
- Cleanup implementation of `equal` functions (#151, @mbarbin).
- Extend lang version compare operators (#145, @mbarbin).
- Upgrade `actions/checkout` to `v6` (#138, @mbarbin).
- Improved licensing headers for vendored `blang` (#144, @mbarbin).
- Refactor pkg directory structure (#143, @mbarbin).
- Improve some format & diff in some internal tests (#137, @mbarbin).
- Upgrade to and require fixes from `fpath-sexp0.0.4.0` (#133, @mbarbin).
- Made some internal refactors suggested by the Zanuda linter (#130, @mbarbin).

### Fixed

- Fix location of sexp error on invalid atoms (#160, @mbarbin).
- Fix enforce on absent `public_name` fields (#159, @mbarbin).
- Allow dash char in package names (#154, @mbarbin).

### Removed

- Removed `expect_test_helpers_core` dependency (#154, @mbarbin).
- Removed unused `compare` functions on rules & predicates (#150, @mbarbin).
- Removed redundant `skip_paths` constructs from dunolint config (#149, @mbarbin).
- Removed now unused `fpath` dependency from `dunolint-lib` (#142, @mbarbin).
- Removed deprecated construct `path.equals` (#142, @mbarbin).
- Removed support for config version `v0` (#141, @mbarbin).

## 0.0.20251006 (2025-10-06)

### Added

- Added 'Config Autoloading' documentation page (#129, @mbarbin).
- Add test for config cache (#128, @mbarbin).
- Implement dunolint config files autoloading (#127, @mbarbin).
- Added 'First Run Through' tutorial (#115, @mbarbin).
- Added 'Quick Start' tutorial (#106, @mbarbin).

### Changed

- Always include default skip paths in config (#125, @mbarbin).
- Refactors to prepare config autoloading (#121, #122, #123, #124, #126, @mbarbin).
- Make dunolint aware of and use dune workspaces (#113, @mbarbin)
- Remove support for `--enforce` in lint-file tool (#118, @mbarbin).
- Update doc for `(lang dunolint 1.0)` (#110, @mbarbin).
- Upgrade to recent `pplumbing` repackaging (#109, @mbarbin).
- Upgrade `crs` actions (#108, @mbarbin).

### Fixed

- Fix workspace root detection in dune test envs (#120, @mbarbin).
- Fix parsing errors of dune version in dune-project (#112, @mbarbin).

## 0.0.20250910-1 (2025-09-10)

### Fixed

- Missing opam dependency `dunolint-lib-base` (@mbarbin).

## 0.0.20250910 (2025-09-10)

### Added

- Support new `(lang dunolint 1.0)` config stanzas format for `dunolint` files (#105, @mbarbin).
- New lib `dunolint-lib-base` to extend `dunolint-lib` for base users (#99, @mbarbin).

### Changed

- Now autoload config files if present at "$PWD/dunolint" (#103, #104, @mbarbin).
- Move the dunolint config at the root of the repo (#102, @mbarbin).
- Change dune-lang sexps to be atoms (e.g. "3.19") instead of tuples (#101, @mbarbin).
- Remove base and ppx dependencies from `dunolint-lib` (#99, @mbarbin).
- Always use versioned sexp for config (#98, @mbarbin).

### Fixed

- Improve location and messages for sexp loading errors (#100, @mbarbin).
- Config loading errors are no longer internal errors and now reported with locations when able (#97, @mbarbin).

## 0.0.20250907 (2025-09-07)

### Added

- Added dependency to `volgo-vcs` in preparation for future features (#89, @mbarbin).
- Supports linting the dune lang version in dune-project files (#93, @mbarbin).
- Supports implicit trans deps `false-if-hidden-includes-supported` value (#92, @mbarbin).

### Changed

- Prepare transition to versioned API and config format (#96, @mbarbin).
- Upgrade docusaurus dependencies to current latest (#94, @mbarbin).
- Changes `(implicit_transitive_deps _)` config value from bool to variant (#92, @mbarbin).
- Improved testing coverage (#90, #91, @mbarbin).

### Fixed

- Correctly propagate `skip_subtree` when returned from rules (#90, @mbarbin).

## 0.0.20250804 (2025-08-04)

### Added

- Made all names in dunolint lib export `Comparable.S` (#84, @mbarbin).

### Changed

- Move sections handling logic into helper module (#85, @mbarbin).
- Move comment handling logic into helper module (#84, @mbarbin).

### Fixed

- Entries `pps` are now sorted by sections when separated by comments (#86, @mbarbin, reported and suggested by @raphael-proust).

## 0.0.20250730 (2025-07-30)

### Added

- Improve canonical order of libraries to open via flags (#83, @mbarbin).
- Add support for ordered-set and use it in library.modes (#81, @mbarbin).
- Enable CRs workflows (experimental) (#76, #78, @mbarbin).
- Add tests (#67, #68, @mbarbin).

### Changed

- Improved testing coverage (#80, @mbarbin).
- Small refactors, tweaks & code improvement (#77, #79, @mbarbin).
- Use `Git_pager` from opam package `pageantty` instead of vendoring it (#73, @mbarbin).
- Refactor enforce functions to ease further testing (#72, @mbarbin).
- Support building dunolint-lib with OCaml 4.14 (#65, @mbarbin).
- Updated git-pager and err dependencies (#63, #64, @mbarbin).

### Fixed

- Fix unexpected rewrites on duped flags in libs (#82, @mbarbin).
- Fix some cram test failures shown by ocaml-ci (#75, @mbarbin).
- Fix disabling of color when `--color=never` is supplied (#70, @mbarbin).
- Fix doc location for installation guide (#69, @mbarbin).
- Fix git pager clean termination when displayed diff exceeds 1 page (#61, @mbarbin).

### Removed

- Replace library `equals MODES` by `has_modes` construct (#81, @mbarbin).

## 0.0.20250403 (2025-04-03)

### Added

- Added command `tools lint-file` to ease editor integration (#39, #40, #46, @mbarbin, requested by and with help from @arvidj).
- Added support for `library.modes` (#23, #27, @mbarbin, requested by @raphael-proust)
- Document release process and current state (#25, @mbarbin).
- Added coverage and regression tests (#18, #19, #30, #33, #34, #48, @mbarbin).

### Changed

- Reduce dependencies of config library (#49, #51, @mbarbin).
- Enabled setup-ocaml's *dune-cache* in CI (#52, @mbarbin).
- Upgrade Docusaurus to latest available (#24, @mbarbin).

### Fixed

- Add missing `melange` library mode (#43, #44, @mbarbin, reported by @arvidj).
- Fixes for `odoc.3.0.0` (#28, #32, @mbarbin, with help from @jonludlam)

### Removed

- Disabled failing macos build from CI for now (#20, @mbarbin).

## 0.0.20250315 (2025-03-15)

### Added

- Prepare to include more ci workflows incrementally in the future (#16, @mbarbin).

### Changed

- Dependencies in `libraries` are now sorted by sections when separated by comments (#12, #14, @mbarbin, reported and suggested by @raphael-proust).

## 0.0.20250310 (2025-03-10)

### Added

- Added new tests (#8, #10, @mbarbin).
- Enabled sending coverage reports to Coveralls (#7, @mbarbin).

### Changed

- Replace `pp-log` deps by `pplumbing` to prepare for publishing to opam (#11, @mbarbin).
- Internal lib rename (`command` -> `cli`) (#9, @mbarbin).
- Replace Eio-based deps by a combination of Unix and Stdio (#6, @mbarbin).

## 0.0.20250211 (2025-02-11)

This very early draft release is intended for publication to my custom opam-repository. It allows for initial experimentation and ensures that the release cycle and distribution process are functioning correctly.

### Added

- Initialize a dunolint config for dunolint.
- Initialize documentation.
- Initialize project, add LICENSE, etc.
