(*_********************************************************************************)
(*_  Git_pager - Show diffs in the terminal with the user's configured git pager  *)
(*_  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*_                                                                               *)
(*_  This file is part of Git_pager.                                              *)
(*_                                                                               *)
(*_  Git_pager is free software; you can redistribute it and/or modify it         *)
(*_  under the terms of the GNU Lesser General Public License as published by     *)
(*_  the Free Software Foundation either version 3 of the License, or any later   *)
(*_  version, with the LGPL-3.0 Linking Exception.                                *)
(*_                                                                               *)
(*_  Git_pager is distributed in the hope that it will be useful, but WITHOUT     *)
(*_  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*_  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*_  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*_                                                                               *)
(*_  You should have received a copy of the GNU Lesser General Public License     *)
(*_  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*_  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
(*_********************************************************************************)

type t

(** Git commands should force colors in the case where the color configuration
    is set to "auto" and we are sending the diff to a pager. This would be
    the behavior of [git diff] itself, but since we are running the pager
    ourselves here, without taking specific measures, [git diff] would
    disable colors. When [should_force_color=true], the expectation is that
    the git commands should be supplied [--color=always]. *)
val should_force_color : t -> bool

(** This is where lines to be sent to the pager should be written to. *)
val write_end : t -> Out_channel.t

val run : f:(t -> 'a) -> 'a
