# Licence

Dunolint is released under the terms of the `LGPL-3.0-or-later WITH LGPL-3.0-linking-exception` license.

This notice file contains more details, as well as document the organization of files and headers that relate to licenses.

## License, copyright & notices

- **COPYING.HEADER** contains the copyright and license notices. It is added as a header to every file in the project.

- **COPYING** contains a copy of the full [GPL-3.0 license](https://www.gnu.org/licenses/gpl-3.0.txt)

- **COPYING.LESSER** contains a copy the full [LGPL-3.0 license](https://www.gnu.org/licenses/lgpl-3.0.txt)

- **COPYING.LINKING** contains a copy of the [LGPL-3.0-linking-exception](https://spdx.org/licenses/LGPL-3.0-linking-exception.html) notice.

- **NOTICE.md** (this file) documents the project licensing.

## A note about Blang

We vendored the module `Core.Blang` from the [Core](https://github.com/janestreet/core) project. `Core` is released under `MIT`, with minor changes to simplify dependencies.

### Notice

The files we imported are in `src/dunolint-lib/vendor/blang`. We've added a notice in the files and a comment next to the code that was copied and modified, which includes `Core`'s original LICENSE, which is included in this repo at `third-party-license/janestreet/core/LICENSE.md`.

## A note about Stdio

A few helpers from the [Stdio](https://github.com/janestreet/stdio) project (released under `MIT`) are reproduced in our local `Stdlib` extensions to avoid taking on `stdio` as a direct dependency.

### Notice

The relevant file is `src/stdlib/out_channel0.ml`. It carries a notice next to the code that was copied, and `Stdio`'s original LICENSE is included in this repo at `third-party-license/janestreet/stdio/LICENSE.md`.

## A note about Base

A few helpers from the [Base](https://github.com/janestreet/base) project (released under `MIT`) are reproduced in our local `Stdlib` extensions to avoid taking on `base` as a direct dependency.

### Notice

The relevant files are `src/stdlib/char0.ml` and `src/stdlib/string0.ml`. Each carries a notice at the top of the file, and the copied functions are clearly indicated next to the code. `Base`'s original LICENSE is included in this repo at `third-party-license/janestreet/base/LICENSE.md`.

## A note about ocaml-merge3 (Myers diff)

The Myers shortest-edit-script computation is vendored from the [ocaml-merge3](https://tangled.org/gazagnaire.org/ocaml-merge3) project by Thomas Gazagnaire (released under `ISC`). Only the pure diff computation is vendored; the parts unused by this project are not included.

### Notice

The relevant file is `src/merge3/merge3.ml`. It carries a notice at the top documenting the exact provenance and list of changes (see also `src/merge3/vendor.json`). `ocaml-merge3`'s original LICENSE is included in this repo at `third-party-license/gazagnaire/ocaml-merge3/LICENSE.md`.

## A note about Windtrap

The unified-diff renderer is vendored from the [windtrap](https://github.com/invariant-hq/windtrap) project by Invariant Systems (released under `ISC`), with minor changes to the rendering of diffs.

### Notice

The relevant file is `src/myers/myers.ml`. It carries a notice at the top documenting the exact provenance and list of changes (see also `src/myers/vendor.json`). `windtrap`'s original LICENSE is included in this repo at `third-party-license/invariant-hq/windtrap/LICENSE`.
