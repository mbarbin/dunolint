## 0.0.2025XXXX (unreleased)

### Added

- Added support for `library.modes` (#23, #27, @mbarbin, requested by @raphael-proust)
- Document release process and current state (#25, @mbarbin).
- Added coverage and regression tests (#18, #19, #30, #33, #34, @mbarbin).

### Changed

- Upgrade Docusaurus to latest available (#24, @mbarbin).

### Deprecated

### Fixed

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
