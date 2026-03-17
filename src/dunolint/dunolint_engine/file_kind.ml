(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

type t = Unix.file_kind =
  | S_REG
  | S_DIR
  | S_CHR
  | S_BLK
  | S_LNK
  | S_FIFO
  | S_SOCK
[@@deriving enumerate]

let to_string = function
  | S_REG -> "Regular file"
  | S_DIR -> "Directory"
  | S_CHR -> "Character device"
  | S_BLK -> "Block device"
  | S_LNK -> "Symbolic link"
  | S_FIFO -> "Named pipe"
  | S_SOCK -> "Socket"
;;
