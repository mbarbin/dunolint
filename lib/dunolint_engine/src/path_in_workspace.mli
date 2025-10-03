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

    This module wraps [Relative_path.t] to provide path operations specific
    to dunolint's workspace traversal, with additional safety guarantees for
    escaping paths.

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

    This module anticipates upcoming changes to the [Relative_path] module in
    the fpath-base library (see fpath-base v0.4.0+). The upstream library will:

    - Reject escaping paths in [Relative_path.of_fpath], [Relative_path.of_string],
      etc.
    - Make [Relative_path.parent] return [None] for the empty path instead of
      creating ["../"]
    - Add runtime checks in operations like [Relative_path.extend] to prevent
      creating escaping paths

    For the time being, we implement our own wrapper that provides these
    guarantees, using [check_escape_path_exn] to validate paths. When the
    upstream changes are released, this module can be simplified to rely on
    the upstream guarantees.

    See [fpath-base/doc/docs/explanation/path-normalization.md] for detailed
    documentation of the upstream approach.

    {1 Migration Note}

    Once fpath-base v0.4.0+ is released with escaping path rejection built-in,
    the explicit [check_escape_path_exn] calls in this module can be removed,
    as the upstream [Relative_path] module will guarantee that no escaping
    paths can be constructed. *)

type t = Relative_path.t

(** [check_escape_path_exn t] validates that path [t] does not escape upward.

    Raises [Invalid_argument] if [t] contains leading [".."] segments after
    normalization (i.e., if it is an escaping path).

    This function is used internally to validate results of path operations.
    It will become unnecessary once fpath-base v0.4.0+ guarantees that
    [Relative_path.t] values cannot be escaping paths.

    {b Examples:}

    These would raise [Invalid_argument]:
    {[
      check_escape_path_exn (Relative_path.v "..");
      (* escapes upward *)
      check_escape_path_exn (Relative_path.v "a/../..")
      (* normalizes to ".." *)
    ]}

    These are OK:
    {[
      check_escape_path_exn (Relative_path.v "a/b");
      (* descends only *)
      check_escape_path_exn (Relative_path.v "a/../b")
      (* normalizes to "b" *)
    ]} *)
val check_escape_path_exn : Relative_path.t -> unit

(** [chop_prefix t ~prefix] removes the prefix [prefix] from path [t].

    Returns:
    - [Some result] where [result] is [t] with [prefix] removed from the start
    - [Some t] (unchanged) when [prefix] is [empty] - removing nothing returns
      the original path
    - [None] if [prefix] is not actually a prefix of [t]

    Note: This operation works on path segments, not string prefixes.
    For example, ["foo/bar-baz"] does not have prefix ["foo/bar"]. *)
val chop_prefix : t -> prefix:Relative_path.t -> t option

(** [parent t] returns the parent directory of path [t], or [None] if [t] has
    no parent.

    Returns [None] when:
    - [t] is equal to [empty] (the path ["./"])

    Raises [Invalid_argument] if [t] is an escaping path (contains leading [".."]
    after normalization). This should not occur for paths constructed through
    this module's API, as escaping paths are rejected during construction.

    {b Note}: This behavior prevents infinite loops that occurred in
    previous versions where [parent empty] would return ["../"], creating
    paths that escape unboundedly.

    If you need to navigate upward through parent directories (including
    above the starting point), use [Absolute_path.parent] or work with
    [Fpath.t] directly. *)
val parent : t -> t option

(** [ancestors_autoloading_dirs ~path] returns all ancestor directories of [path],
    from the workspace root down to the parent of [path].

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
    skip predicates during tree traversal.

    This function has different semantics from {!ancestors_autoloading_dirs}:
    - For files: returns parent directories only
    - For directories (trailing ["/"]): returns ancestors {b including the directory itself}
    - Never includes workspace root ["./"]

    The returned list is ordered from root to deepest.

    Returns [[]] when:
    - [path] is equal to [empty] (the path ["./"])
    - [path] is a file in the workspace root

    Raises [Invalid_argument] if [path] is an escaping path (contains leading
    [".."] after normalization).

    {b Examples:}

    File paths return parent directories only:
    {[
      paths_to_check_for_skip_predicates ~path:(v "foo/bar/bin")
      (* Returns: ["foo/"; "foo/bar/"] *)
    ]}

    Directory paths (trailing ["/"]) include the directory itself:
    {[
      paths_to_check_for_skip_predicates ~path:(v "foo/bar/bin/")
      (* Returns: ["foo/"; "foo/bar/"; "foo/bar/bin/"] *)
    ]}

    Root and single files return empty:
    {[
      paths_to_check_for_skip_predicates ~path:(v "file.ml");
      (* Returns: [] *)
      paths_to_check_for_skip_predicates ~path:empty
      (* Returns: [] *)
    ]}

    This function is used when checking if paths match skip predicates in
    already-loaded configs. The directory-includes-self behavior is important:
    when visiting a directory during traversal, you want to check if that
    directory itself should be skipped. *)
val paths_to_check_for_skip_predicates : path:t -> t list
