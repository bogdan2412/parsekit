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

module type Combinators := sig
  type 'a t

  val return : 'a -> 'a t
  val fail : string -> _ t
  val fail' : string Lazy.t -> _ t

  (** Combinators *)

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
  val ( << ) : 'a t -> 'b t -> 'a t
  val skip_many : 'a t -> at_least:int -> at_most:int option -> unit t
  val many : 'a t -> at_least:int -> at_most:int option -> 'a list t
  val sep_by : 'a t -> sep:_ t -> at_least:int -> at_most:int option -> 'a list t
  val fix : max_recursion_depth:int -> ('a t -> 'a t) -> 'a t

  (** Alternatives *)

  val either : 'a t -> 'b t -> ('a, 'b) Either.t t
  val choices : 'a t list -> 'a t

  (** Prevents backtracking before the current point in the input. *)
  val commit : unit t

  (** Special parsers *)

  val at_end_of_input : bool t
  val end_of_input : unit t
  val consumed_bytes : 'a t -> string t
  val value_and_consumed_bytes : 'a t -> ('a * string) t

  (** Basic parsers - character level *)

  val peek1 : char option t
  val skip1 : unit t
  val take1 : char t
  val match1 : char -> char t
  val take1_cond : (char -> bool) -> char t

  (** Basic parsers - string level *)

  val peek : len:int -> string option t
  val skip : len:int -> unit t
  val take : len:int -> string t
  val match_ : string -> string t
  val skip_while : f:(char -> bool) -> at_least:int -> at_most:int option -> unit t
  val take_while : f:(char -> bool) -> at_least:int -> at_most:int option -> string t

  val fold
    :  init:'acc
    -> f:
         ('acc
          -> peek:[ `Char of char | `Eof ]
          -> [ `Fail of string Lazy.t | `Return of 'acc | `Advance of 'acc ])
    -> 'acc t

  val foldn
    :  n:int
    -> init:'acc
    -> f:('acc -> char -> ('acc, string Lazy.t) Result.t)
    -> 'acc t

  (** Matches any amount of whitespace characters, defined as one of [' '; '\t';
    '\n'; '\r'] *)
  val whitespace0 : unit t

  (** Similar to [whitespace0], but requires at least one whitespace character to
      be present. *)
  val whitespace : unit t

  val non_negative_integer : int t
  val integer : int t

  (** More efficient API for constructing large strings by running parsers that
      can emit characters into a buffer as they progress through the input.
      Avoids the need to construct intermediate lists of characters or strings
      that are then concatenated together. *)
  val buffered_output
    :  ?initial_capacity:int
    -> (emit:(char -> unit) -> unit t)
    -> string t

  module Utf8_encoded = Utf8_encoded

  (** UTF8 encoding parsers.

      The strict version raises parse errors on invalid byte sequences, whereas the
      non-strict version converts the invalid sequence to the Unicode Replacement
      Character instead. *)

  val skip1_strict_utf8 : unit t
  val take1_strict_utf8 : Utf8_encoded.t t
  val take1_utf8 : Utf8_encoded.t t

  (** The two methods below consume input for as long as it is valid UTF8. *)

  val skip_strict_utf8 : unit t
  val take_strict_utf8 : string t

  (** Consumes all of the input until the end, replacing invalid UTF8 values with
      the Unicode Replacement Character. *)
  val take_utf8 : string t
end

module type Parser := sig
  exception
    ParseError of
      { pos : int
      ; message : string
      }

  type 'a t

  val run : 'a t -> string -> require_input_entirely_consumed:bool -> 'a

  include Monad.S with type 'a t := 'a t
  include Combinators with type 'a t := 'a t
end

include Parser

module Let_syntax : sig
  include module type of struct
    include Let_syntax
  end

  module Let_syntax : sig
    include module type of struct
      include Let_syntax
    end

    module Open_on_rhs : Combinators with type 'a t := 'a t
  end
end

module With_let_syntax : sig
  include Parser with type 'a t := 'a t

  include module type of struct
    include Let_syntax
  end
end
