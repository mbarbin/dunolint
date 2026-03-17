(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(*_ This module is derived from the dune code base file
 * [./bin/workspace_root.mli] which is released under MIT:
 *
 * Copyright (c) 2016 Jane Street Group, LLC <opensource@janestreet.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 *  Changes:
 *
 * - Use [Err] instead of [User_message].
 * - Use [Workspace_root] for the root dir.
 * - Remove [create].
 * - Make the type abstract add getters.
 * - Rename [create_exn] as [find_exn].
 *)

(** Finding the root of the workspace *)

type t

val find_exn : default_is_cwd:bool -> specified_by_user:Absolute_path.t option -> t

(** {1 Getters} *)

val path : t -> Absolute_path.t

(** {1 chdir} *)

(** Early in the programs life we would change the running directory to be the
    workspace root. If [t] does not equal to [cwd] this function will also print
    a message on stderr indicating that the directory is changing if the specified
    log-level is turned on. *)
val chdir : t -> level:Err.Level.t -> unit
