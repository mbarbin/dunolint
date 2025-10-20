(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*_                                                                               *)
(*_  This file is part of Dunolint.                                               *)
(*_                                                                               *)
(*_  Dunolint is free software; you can redistribute it and/or modify it          *)
(*_  under the terms of the GNU Lesser General Public License as published by     *)
(*_  the Free Software Foundation either version 3 of the License, or any later   *)
(*_  version, with the LGPL-3.0 Linking Exception.                                *)
(*_                                                                               *)
(*_  Dunolint is distributed in the hope that it will be useful, but WITHOUT      *)
(*_  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*_  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*_  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*_                                                                               *)
(*_  You should have received a copy of the GNU Lesser General Public License     *)
(*_  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*_  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
(*_********************************************************************************)

(** Paths relative to the workspace root, with escaping path prevention.

    This module wraps [Relative_path.t] to provide path operations specific to
    dunolint's workspace traversal, benefiting from [Relative_path]'s additional
    safety guarantees for escaping paths.

    {1 Purpose}

    When traversing a dune workspace, dunolint needs to work with paths relative
    to the workspace root. This module provides:

    - Safe path operations that prevent escaping the workspace root
    - Validation that paths don't contain upward-escaping [".."] segments
    - Workspace-aware wrappers around [Relative_path] operations

    {1 Escaping Paths}

    An {b escaping path} is a relative path that, after [Fpath] normalization,
    contains leading [".."] segments. These segments indicate the path escapes
    upward past its starting point.

    {b Examples of escaping paths} (all rejected by this module):
    - [".."] - escapes upward by one level
    - ["../config"] - escapes upward then descends
    - ["a/../.."] - normalizes to [".."], which escapes upward
    - ["../../../etc/passwd"] - escapes multiple levels upward

    {b Why reject escaping paths?}

    + {b Workspace boundary enforcement}: Paths in a dune workspace should
      reference locations within that workspace. Escaping paths reference
      locations outside the workspace root, which violates this invariant.
    + {b Memory safety}: In previous versions, operations like [parent] could
      create unbounded escaping paths when called repeatedly on the empty path,
      leading to memory growth bugs. By rejecting escaping paths at construction
      time, these bugs are prevented.
    + {b Semantic clarity}: Escaping paths have ambiguous meaning without
      additional context about where the "starting point" is. By requiring
      all workspace paths to be non-escaping, the semantics are clear: they're
      paths relative to the workspace root.

    {1 Relationship to fpath-base}

    This module builds upon the [Relative_path] module in the fpath-base library
    (see fpath-base v0.4.0+). The upstream library does:

    - Reject escaping paths in [Relative_path.of_fpath], [Relative_path.of_string],
      etc.
    - Make [Relative_path.parent] return [None] for the empty path instead of
      creating ["../"]
    - Add runtime checks in operations like [Relative_path.extend] to prevent
      creating escaping paths

    In this module we rely on the upstream guarantees.

    See [fpath-base/doc/docs/explanation/path-normalization.md] for detailed
    documentation of the upstream approach. *)

type t = Relative_path.t

(** [ancestors_autoloading_dirs ~path] returns all ancestor directories of
    [path], from the workspace root down to the parent of [path].

    This function is specifically designed for config autoloading: it returns
    the list of directories that should be checked for dunolint configuration
    files when linting a file at [path].

    The returned list is ordered from root to deepest ancestor (i.e., from
    shortest to longest paths), which matches the order in which configs should
    be loaded and accumulated.

    Returns [[]] when:
    - [path] is equal to [empty] (the path ["./"])

    Raises [Invalid_argument] if [path] is an escaping path (contains leading
    [".."] after normalization).

    {b Examples:}

    Linting file ["a/b/c.ml"] should check configs in:
    {[
      ancestors_autoloading_dirs ~path:(v "a/b/c.ml")
    ]}
    Returns: [["./"; "a/"; "a/b/"]]

    Linting file ["file.ml"] at workspace root checks root config:
    {[
      ancestors_autoloading_dirs ~path:(v "file.ml")
    ]}
    Returns: [["./"]].

    Empty path has no ancestors:
    {[
      ancestors_autoloading_dirs ~path:empty
    ]}
    Returns: [[]]

    This function is used internally by the engine when linting individual files
    to discover which configuration files should be loaded from ancestor
    directories. *)
val ancestors_autoloading_dirs : path:t -> t list

(** [paths_to_check_for_skip_predicates ~path] returns paths to check against
    skip predicates during tree traversal, including the path itself.

    Returns parent directories plus the path itself, ordered from root to
    deepest. The workspace root ["./"] is never included in the results.

    Returns [[]] when [path] is equal to [empty] (the path ["."]).

    Raises [Invalid_argument] if [path] is an escaping path (contains leading
    [".."] after normalization).

    {b Examples:}

    File paths return parent directories and the file itself:
    {[
      paths_to_check_for_skip_predicates ~path:(v "foo/bar/bin")
      (* Returns: ["foo/"; "foo/bar/"; "foo/bar/bin"] *)
    ]}

    Directory paths (trailing ["/"]) include parent directories and the
    directory itself:
    {[
      paths_to_check_for_skip_predicates ~path:(v "foo/bar/bin/")
      (* Returns: ["foo/"; "foo/bar/"; "foo/bar/bin/"] *)
    ]}

    Single files at the root return just the file:
    {[
      paths_to_check_for_skip_predicates ~path:(v "file.ml")
      (* Returns: ["file.ml"] *)
    ]}

    Workspace root returns empty:
    {[
      paths_to_check_for_skip_predicates ~path:empty
      (* Returns: [] *)
    ]}

    This function is used when checking if paths match skip predicates in
    already-loaded configs. The path itself is included so that skip predicates
    can be checked against both the path and its ancestors. *)
val paths_to_check_for_skip_predicates : path:t -> t list
