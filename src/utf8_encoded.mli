(** Parser combinator library for OCaml.

    Copyright (C) 2024  Bogdan-Cristian Tataroiu

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>. *)

open! Base
open! Import

type t [@@immediate] [@@deriving hash, sexp_of]

val replacement_character : t

include Comparable.S with type t := t
include Hashable.Key with type t := t

val to_code : t -> int
val of_code_exn : int -> t
val unchecked_of_code : int -> t
val to_string : t -> string

val encoded_data
  :  t
  -> ascii:(char -> 'a)
  -> two_byte:(char -> char -> 'a)
  -> three_byte:(char -> char -> char -> 'a)
  -> four_byte:(char -> char -> char -> char -> 'a)
  -> 'a

val emit_encoded_data : t -> emit:(char -> unit) -> unit

(** Given a chunk of data found in buffer [buf] between positions [pos] and
    [pos + len - 1] inclusive, parses a single UTF8-encoded code found at the
    beginning of the chunk and dispatch to one of the two [on_valid] and
    [on_invalid] methods depending on whether the consumed data was valid or
    invalid UTF8.

    In both cases, [parse_single] provides the callback with the number of
    characters from the input that it has consumed and which should be skipped
    over in order to continue parsing the input buffer. *)
val parse_single
  :  string
  -> pos:int
  -> len:int
  -> on_valid:(consumed:int -> t -> 'a)
  -> on_invalid:(consumed:int -> 'a)
  -> 'a

(** Given a chunk of data found in buffer [buf] between positions [pos] and
    [pos + len - 1] inclusive, parses as much valid UTF8-encoded data as
    possible until either invalid data is encountered or the end of the data
    chunk is encountered. The method then returns the number of bytes consumed
    which constitute valid UTF8 data.

    The method considers truncated multi-byte sequences at the end of the data
    chunk to be invalid and does not consume them. *)
val valid_data_length : string -> pos:int -> len:int -> int
