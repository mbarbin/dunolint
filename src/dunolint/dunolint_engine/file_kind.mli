(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type t = Unix.file_kind =
  | S_REG
  | S_DIR
  | S_CHR
  | S_BLK
  | S_LNK
  | S_FIFO
  | S_SOCK
[@@deriving enumerate]

val to_string : t -> string
