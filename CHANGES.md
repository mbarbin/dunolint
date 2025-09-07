## 0.0.2025XXXX (unreleased)

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
