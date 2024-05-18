(** JSON parsing library built using the Parsekit library.

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

type t =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | List of t list
  | Dictionary of t Map.M(String).t
[@@deriving sexp_of]

module Parser = struct
  open Parsekit.With_let_syntax

  let null = match_ "null" >> return ()
  let bool = choices [ match_ "true" >> return true; match_ "false" >> return false ]

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  ;;

  let number =
    let sign =
      match%bind peek1 with
      | Some '-' -> skip1
      | _ -> return ()
    in
    let non_negative_integer =
      match%bind take1 with
      | '0' -> return ()
      | '1' .. '9' -> skip_while ~f:is_digit ~at_least:0 ~at_most:None
      | _ -> fail "not a number"
    in
    let frac =
      match%bind peek1 with
      | Some '.' -> skip1 >> skip_while ~f:is_digit ~at_least:1 ~at_most:None
      | _ -> return ()
    in
    let exp =
      match%bind peek1 with
      | Some ('e' | 'E') ->
        skip1
        >> (match%bind peek1 with
            | Some ('-' | '+') -> skip1
            | _ -> return ())
        >> skip_while ~f:is_digit ~at_least:1 ~at_most:None
      | _ -> return ()
    in
    consumed_bytes (sign >> non_negative_integer >> frac >> exp) >>| Float.of_string
  ;;

  let string =
    match1 '"'
    >> buffered_output (fun [@inline] ~emit ->
      let unicode_escaped_char =
        let hex_4_digit_code =
          foldn ~n:4 ~init:0 ~f:(fun [@inline] acc c ->
            let[@inline] ok value = Ok ((acc lsl 4) lor value) in
            match c with
            | '0' .. '9' as c -> ok (Char.to_int c - Char.to_int '0')
            | 'a' .. 'f' as c -> ok (Char.to_int c - Char.to_int 'a' + 10)
            | 'A' .. 'F' as c -> ok (Char.to_int c - Char.to_int 'A' + 10)
            | _ -> Error (lazy "non-hexadecimal character"))
        in
        let utf16_low_surrogate =
          let%bind code = match1 '\\' >> match1 'u' >> hex_4_digit_code in
          if code >= 0xdc00 && code <= 0xdfff
          then return code
          else fail "Invalid UTF-16 surrogate pair sequence"
        in
        let utf16_pair ~high ~low =
          0x10000 + (((high - 0xd800) lsl 10) lor (low - 0xdc00))
        in
        let%bind code = hex_4_digit_code in
        match code >= 0xd800 && code <= 0xdfff with
        | true ->
          (match code <= 0xdbff with
           | true ->
             let%bind low = utf16_low_surrogate in
             let code = utf16_pair ~high:code ~low in
             Utf8_encoded.emit_encoded_data (Utf8_encoded.unchecked_of_code code) ~emit;
             return ()
           | false -> fail "Lone UTF-16 low surrogate sequence")
        | false ->
          Utf8_encoded.emit_encoded_data (Utf8_encoded.unchecked_of_code code) ~emit;
          return ()
      in
      let escaped_char =
        let emit_m chr =
          emit chr;
          return ()
        in
        match%bind take1 with
        | '"' -> emit_m '"'
        | '\\' -> emit_m '\\'
        | '/' -> emit_m '/'
        | 'b' -> emit_m '\b'
        | 'f' -> emit_m '\x0c'
        | 'n' -> emit_m '\n'
        | 'r' -> emit_m '\r'
        | 't' -> emit_m '\t'
        | 'u' -> unicode_escaped_char
        | _ -> fail "unexpected escaped character"
      in
      skip_many
        (let%bind utf8_encoded = take1_strict_utf8 in
         Utf8_encoded.encoded_data
           utf8_encoded
           ~ascii:(fun [@inline] char ->
             match char with
             | '\\' -> escaped_char
             | '"' -> fail "end of string"
             | '\000' .. '\031' -> fail "control character"
             | _ ->
               emit char;
               return ())
           ~two_byte:(fun [@inline] c1 c2 ->
             emit c1;
             emit c2;
             return ())
           ~three_byte:(fun [@inline] c1 c2 c3 ->
             emit c1;
             emit c2;
             emit c3;
             return ())
           ~four_byte:(fun [@inline] c1 c2 c3 c4 ->
             emit c1;
             emit c2;
             emit c3;
             emit c4;
             return ()))
        ~at_least:0
        ~at_most:None)
    << match1 '"'
  ;;

  let list' ~data =
    match1 '['
    >> whitespace0
    >> sep_by
         data
         ~sep:(whitespace0 >> match1 ',' << whitespace0)
         ~at_least:0
         ~at_most:None
    << whitespace0
    << match1 ']'
  ;;

  let dictionary' ~key ~data =
    match1 '{'
    >> whitespace0
    >> (sep_by
          (let%map key = key << whitespace0 << match1 ':' << whitespace0
           and value = data in
           key, value)
          ~sep:(whitespace0 >> match1 ',' << whitespace0)
          ~at_least:0
          ~at_most:None
        >>| Map.of_alist_reduce (module String) ~f:(fun _ snd -> snd))
    << whitespace0
    << match1 '}'
  ;;

  let json =
    let list json = list' ~data:json >>| fun value -> List value in
    let dictionary json =
      dictionary' ~key:string ~data:json >>| fun value -> Dictionary value
    in
    let null = null >>| fun () -> Null in
    let true_ = match_ "true" >> return (Bool true) in
    let false_ = match_ "false" >> return (Bool false) in
    let number = number >>| fun value -> Number value in
    let string = string >>| fun value -> String value in
    whitespace0
    >> fix ~max_recursion_depth:30_000 (fun json ->
      match%bind peek1 with
      | Some 'n' -> null
      | Some 't' -> true_
      | Some 'f' -> false_
      | Some '"' -> string
      | Some '[' -> list json
      | Some '{' -> dictionary json
      | _ -> number)
    << whitespace0
  ;;

  let list = list' ~data:json
  let dictionary = dictionary' ~key:string ~data:json

  module Custom = struct
    let list' = list'
    let dictionary' = dictionary'
  end
end

let parser = Parser.json

let null_exn = function
  | Null -> ()
  | t -> raise_s [%message "Expected null json value" (t : t)]
;;

let bool_exn = function
  | Bool value -> value
  | t -> raise_s [%message "Expected bool json value" (t : t)]
;;

let number_exn = function
  | Number value -> value
  | t -> raise_s [%message "Expected numeric json value" (t : t)]
;;

let string_exn = function
  | String value -> value
  | t -> raise_s [%message "Expected string json value" (t : t)]
;;

let list_exn = function
  | List value -> value
  | t -> raise_s [%message "Expected list json value" (t : t)]
;;

let dictionary_exn = function
  | Dictionary value -> value
  | t -> raise_s [%message "Expected dictionary json value" (t : t)]
;;

let dictionary_member_exn t ~key =
  let t = dictionary_exn t in
  match Map.find t key with
  | None ->
    raise_s
      [%message "Expected key in json dictionary" (key : string) (t : t Map.M(String).t)]
  | Some value -> value
;;
