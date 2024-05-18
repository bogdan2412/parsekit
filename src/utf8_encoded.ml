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

module M = struct
  type t = int [@@deriving compare, hash]

  let[@inline] encoded_data t ~ascii ~two_byte ~three_byte ~four_byte =
    match t <= 0xff with
    | true -> ascii (Char.unsafe_of_int t)
    | false ->
      (match t <= 0xffff with
       | true ->
         let c1 = t lsr 8 in
         let c2 = t land 0xff in
         two_byte (Char.unsafe_of_int c1) (Char.unsafe_of_int c2)
       | false ->
         (match t <= 0xffffff with
          | true ->
            let c1 = t lsr 16 in
            let c2 = (t lsr 8) land 0xff in
            let c3 = t land 0xff in
            three_byte
              (Char.unsafe_of_int c1)
              (Char.unsafe_of_int c2)
              (Char.unsafe_of_int c3)
          | false ->
            let c1 = t lsr 24 in
            let c2 = (t lsr 16) land 0xff in
            let c3 = (t lsr 8) land 0xff in
            let c4 = t land 0xff in
            four_byte
              (Char.unsafe_of_int c1)
              (Char.unsafe_of_int c2)
              (Char.unsafe_of_int c3)
              (Char.unsafe_of_int c4)))
  ;;

  let to_string t =
    encoded_data
      t
      ~ascii:String.of_char
      ~two_byte:(fun c1 c2 -> String.of_char_list [ c1; c2 ])
      ~three_byte:(fun c1 c2 c3 -> String.of_char_list [ c1; c2; c3 ])
      ~four_byte:(fun c1 c2 c3 c4 -> String.of_char_list [ c1; c2; c3; c4 ])
  ;;

  let sexp_of_t = Fn.compose [%sexp_of: string] to_string
end

include M
include Comparable.Make (M)

let[@inline] to_code t =
  encoded_data
    t
    ~ascii:Char.to_int
    ~two_byte:(fun [@inline] c1 c2 ->
      let c1 = Char.to_int c1 in
      let c2 = Char.to_int c2 in
      ((c1 land 0b00011111) lsl 6) lor (c2 land 0b00111111))
    ~three_byte:(fun [@inline] c1 c2 c3 ->
      let c1 = Char.to_int c1 in
      let c2 = Char.to_int c2 in
      let c3 = Char.to_int c3 in
      ((c1 land 0b00001111) lsl 12)
      lor ((c2 land 0b00111111) lsl 6)
      lor (c3 land 0b00111111))
    ~four_byte:(fun [@inline] c1 c2 c3 c4 ->
      let c1 = Char.to_int c1 in
      let c2 = Char.to_int c2 in
      let c3 = Char.to_int c3 in
      let c4 = Char.to_int c4 in
      ((c1 land 0b00000111) lsl 18)
      lor ((c2 land 0b00111111) lsl 12)
      lor ((c3 land 0b00111111) lsl 6)
      lor (c4 land 0b00111111))
;;

let[@cold] exn_invalid_code code =
  raise_s [%message "Invalid Unicode code point" (code : int)]
;;

let[@inline] of_code_exn code =
  match code <= 127 with
  | true ->
    (match code >= 0 with
     | true -> code
     | false -> raise @@ exn_invalid_code code)
  | false ->
    (match code <= 0x7ff with
     | true -> 0xc080 lor ((code lsl 2) land 0x1f00) lor (code land 0x3f)
     | false ->
       (match code <= 0xffff with
        | true ->
          (match code < 0xd800 || code > 0xdfff with
           | true ->
             0xe08080
             lor ((code lsl 4) land 0x0f0000)
             lor ((code lsl 2) land 0x3f00)
             lor (code land 0x3f)
           | false -> raise @@ exn_invalid_code code)
        | false ->
          (match code <= 0x10ffff with
           | true ->
             0xf0808080
             lor ((code lsl 6) land 0x07000000)
             lor ((code lsl 4) land 0x3f0000)
             lor ((code lsl 2) land 0x3f00)
             lor (code land 0x3f)
           | false -> raise @@ exn_invalid_code code)))
;;

let[@inline] unchecked_of_code code =
  match code <= 127 with
  | true -> code
  | false ->
    (match code <= 0x7ff with
     | true -> 0xc080 lor ((code lsl 2) land 0x1f00) lor (code land 0x3f)
     | false ->
       (match code <= 0xffff with
        | true ->
          0xe08080
          lor ((code lsl 4) land 0x0f0000)
          lor ((code lsl 2) land 0x3f00)
          lor (code land 0x3f)
        | false ->
          0xf0808080
          lor ((code lsl 6) land 0x07000000)
          lor ((code lsl 4) land 0x3f0000)
          lor ((code lsl 2) land 0x3f00)
          lor (code land 0x3f)))
;;

let replacement_character = of_code_exn 0xfffd

let[@inline] is_valid_byte_2_4 = function
  | '\128' .. '\191' -> true
  | _ -> false
;;

let[@inline] parse_exn
  ~first_byte
  ~next_byte_exists
  ~unsafe_peek
  ~unsafe_advance_byte
  ~parse_error
  =
  let[@inline] with_next_char ~is_valid f =
    match next_byte_exists () with
    | false -> parse_error ()
    | true ->
      let chr = unsafe_peek () in
      (match is_valid chr with
       | true ->
         unsafe_advance_byte ();
         f chr
       | false -> parse_error ())
  in
  match first_byte with
  | '\000' .. '\127' as c -> Char.to_int c
  | '\128' .. '\193' | '\245' .. '\255' -> parse_error ()
  | '\194' .. '\223' as c1 ->
    with_next_char ~is_valid:is_valid_byte_2_4 (fun [@inline] c2 ->
      let c1 = Char.to_int c1 in
      let c2 = Char.to_int c2 in
      (c1 lsl 8) lor c2)
  | '\224' .. '\239' as c1 ->
    with_next_char
      ~is_valid:
        (match c1 with
         | '\224' ->
           (function
             | '\160' .. '\191' -> true
             | _ -> false)
         | '\237' ->
           (function
             | '\128' .. '\159' -> true
             | _ -> false)
         | _ -> is_valid_byte_2_4)
      (fun [@inline] c2 ->
        with_next_char ~is_valid:is_valid_byte_2_4 (fun [@inline] c3 ->
          let c1 = Char.to_int c1 in
          let c2 = Char.to_int c2 in
          let c3 = Char.to_int c3 in
          (c1 lsl 16) lor (c2 lsl 8) lor c3))
  | '\240' .. '\244' as c1 ->
    with_next_char
      ~is_valid:
        (match c1 with
         | '\240' ->
           (function
             | '\144' .. '\191' -> true
             | _ -> false)
         | '\244' ->
           (function
             | '\128' .. '\143' -> true
             | _ -> false)
         | _ -> is_valid_byte_2_4)
      (fun [@inline] c2 ->
        with_next_char ~is_valid:is_valid_byte_2_4 (fun [@inline] c3 ->
          with_next_char ~is_valid:is_valid_byte_2_4 (fun [@inline] c4 ->
            let c1 = Char.to_int c1 in
            let c2 = Char.to_int c2 in
            let c3 = Char.to_int c3 in
            let c4 = Char.to_int c4 in
            (c1 lsl 24) lor (c2 lsl 16) lor (c3 lsl 8) lor c4)))
;;

let[@inline] emit_encoded_data t ~emit =
  encoded_data
    t
    ~ascii:(fun [@inline] byte -> emit byte)
    ~two_byte:(fun [@inline] c1 c2 ->
      emit c1;
      emit c2)
    ~three_byte:(fun [@inline] c1 c2 c3 ->
      emit c1;
      emit c2;
      emit c3)
    ~four_byte:(fun [@inline] c1 c2 c3 c4 ->
      emit c1;
      emit c2;
      emit c3;
      emit c4)
;;
