## 0.0.2025XXXX (unreleased)

### Added

- Prepare to include more ci workflows incrementally in the future (@mbarbin).

### Changed

- Dependencies in `libraries` are now sorted by sections when separated by comments (#12, #14, @mbarbin, reported and suggested by @raphael-proust).

### Deprecated

### Fixed

### Removed

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
