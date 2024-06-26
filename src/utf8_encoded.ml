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

(* Parsing UTF-8 data naively requires a non-trivial amount of branching based
   on the values of the first two bytes. In particular, the branching happens
   on the first byte and the most significant 4 bits of the second byte. We can
   instead view this as 3 consecutive 4-bit values.

   Rather than performing the branches explicitly, we will look at each of the
   3 4-bit values independently. For each of these, we will identify which
   possible error paths in the decision tree they might be part of. We assign a
   different error code to each path in the decision tree, while aiming to
   share error codes for equivalent paths (such as an ASCII second byte
   following a non-ASCII first byte). If an error code is shared between all
   three 4-bit values, that indicates we've received invalid input
   corresponding to the error path in the decision tree labeled with the given
   code. If no error code is shared between the three values, then the two byte
   sequence forms a legal beginning of an UTF-8 encoded sequence (the first
   code of which might still be 1, 2, 3 or 4 bytes long).

   It turns out that 8 bits are sufficient for encoding all different error
   paths, which means we can represent the possible error conditions each value
   corresponds to with 8-bit values. These can be combined using bit-wise and
   and compared against 0 to indicate correct input.

   A missing second byte can simply be modeled with a value of 0 since if the first
   byte is ASCII, then the value of the second byte is ignored and if the first byte
   is non-ASCII, the second byte should be a continuation, which 0 is not causing
   an error to be raised correctly.

   The decision tree is as follows:

   {v
| 0..7 -> ok
| 8..b ->                       # first byte should not be a continuation
  | 0..f ->
    | 0..f -> fail 0x01
| c ->
  | 2..f ->
    | 0..7 | c..f -> fail 0x02  # second byte should be a continuation
    | 8..b -> ok
  | 0..1 ->
    | 0..7 | c..f -> fail 0x02
    | 8..b -> fail 0x04         # value could instead be encoded using a single byte
| d ->
  | 0..f ->
    | 0..7 | c..f -> fail 0x02
    | 8..b -> ok
| e ->
  | 0 ->
    | 0..7 | c..f -> fail 0x02
    | 8..9 -> fail 0x08         # value could instead be encoded using two bytes
    | a..b -> ok
  | d ->
    | 0..7 | c..f -> fail 0x02
    | a..b -> fail 0x10         # surrogate pairs
    | 8..9 -> ok
  | 1..c | e..f ->
    | 0..7 | c..f -> fail 0x02
    | 8..b -> ok
| f ->
  | 0 ->
    | 0..7 | c..f -> fail 0x02
    | 8 -> fail 0x20            # value could instead be encoded using three bytes
    | 9..b -> ok
  | 4 ->
    | 0..7 | c..f -> fail 0x02
    | 9..b -> fail 0x40         # value would be too large
    | 8 -> ok
  | 1..3 ->
    | 0..7 | c..f -> fail 0x02
    | 8..b -> ok
  | 5..f ->
    | 0..f -> fail 0x80        # value would be too large
   v}

   The above corresponds to the following potential error code lookup tables.

   {v
byte1_high:
0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x01 0x01 0x01 0x01 0x06 0x02 0x1A 0xE2
byte1_low:
0x2F 0x07 0x03 0x03 0x43 0x83 0x83 0x83 0x83 0x83 0x83 0x83 0x83 0x93 0x83 0x83
byte2_high:
0x03 0x03 0x03 0x03 0x83 0x83 0x83 0x83 0xAD 0xCD 0xD5 0xD5 0x83 0x83 0x83 0x83
   v} *)

let byte1_high =
  [| 0x00
   ; 0x00
   ; 0x00
   ; 0x00
   ; 0x00
   ; 0x00
   ; 0x00
   ; 0x00
   ; 0x01
   ; 0x01
   ; 0x01
   ; 0x01
   ; 0x06
   ; 0x02
   ; 0x1A
   ; 0xE2
  |]
;;

let byte1_low =
  [| 0x2F
   ; 0x07
   ; 0x03
   ; 0x03
   ; 0x43
   ; 0x83
   ; 0x83
   ; 0x83
   ; 0x83
   ; 0x83
   ; 0x83
   ; 0x83
   ; 0x83
   ; 0x93
   ; 0x83
   ; 0x83
  |]
;;

let byte2_high =
  [| 0x03
   ; 0x03
   ; 0x03
   ; 0x03
   ; 0x83
   ; 0x83
   ; 0x83
   ; 0x83
   ; 0xAD
   ; 0xCD
   ; 0xD5
   ; 0xD5
   ; 0x83
   ; 0x83
   ; 0x83
   ; 0x83
  |]
;;

let[@inline] parse_single' buf ~pos ~end_pos ~on_valid ~on_invalid =
  let first_byte = String.unsafe_get buf pos in
  let byte1 = Char.to_int first_byte in
  (* Performance optimization - shortcut ASCII code. *)
  match first_byte with
  | '\x00' .. '\x7f' -> on_valid ~consumed:1 byte1
  | _ ->
    let byte2 =
      match pos + 1 < end_pos with
      | true -> Char.to_int (String.unsafe_get buf (pos + 1))
      | false -> 0
    in
    let byte1_high = byte1_high.(byte1 lsr 4) in
    let byte1_low = byte1_low.(byte1 land 15) in
    let byte2_high = byte2_high.(byte2 lsr 4) in
    let errors = byte1_high land byte1_low land byte2_high in
    (match errors = 0 with
     | false -> on_invalid ~consumed:1
     | true ->
       let[@inline] with_continuation_char ~offset f =
         match pos + offset < end_pos with
         | false -> on_invalid ~consumed:offset
         | true ->
           let chr = String.unsafe_get buf (pos + offset) in
           (match chr with
            | '\x80' .. '\xbf' -> f (Char.to_int chr)
            | _ -> on_invalid ~consumed:offset)
       in
       (match first_byte with
        | '\xc2' .. '\xdf' -> on_valid ~consumed:2 ((byte1 lsl 8) lor byte2)
        | '\xe0' .. '\xef' ->
          with_continuation_char ~offset:2 (fun [@inline] byte3 ->
            on_valid ~consumed:3 ((byte1 lsl 16) lor (byte2 lsl 8) lor byte3))
        | '\xf0' .. '\xf4' ->
          with_continuation_char ~offset:2 (fun [@inline] byte3 ->
            with_continuation_char ~offset:3 (fun [@inline] byte4 ->
              on_valid
                ~consumed:4
                ((byte1 lsl 24) lor (byte2 lsl 16) lor (byte3 lsl 8) lor byte4)))
        | _ -> failwith "BUG: branch should be unreachable"))
;;

let[@inline] parse_single buf ~pos ~len ~on_valid ~on_invalid =
  let end_pos = pos + len in
  parse_single' buf ~pos ~end_pos ~on_valid ~on_invalid
;;

let valid_data_length =
  let[@inline] single_loop buf ~pos ~end_pos =
    let len = end_pos - pos in
    let consumed = ref 0 in
    while
      match len - !consumed with
      | 0 -> false
      | _ ->
        parse_single'
          buf
          ~pos:(pos + !consumed)
          ~end_pos
          ~on_valid:(fun [@inline] ~consumed:newly_consumed (_ : t) ->
            consumed := !consumed + newly_consumed;
            true)
          ~on_invalid:(fun [@inline] ~consumed:_ -> false)
    do
      ()
    done;
    !consumed
  in
  fun [@inline] buf ~pos ~len ->
    let end_pos = pos + len in
    single_loop buf ~pos ~end_pos
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
