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

let%expect_test "Data type examples" =
  let input =
    {|
    {
      "null": null,
      "true": true,
      "false": false,
      "array": [ 1, 2, 3, 4, "5", "6", "7", "8" ],
      "object": {
        "number": 1234,
        "negative_number": -1234,
        "floating_point": 123.456,
        "big_number": 12345678901234567890,
        "float_with_exp": 1e-100,
        "float_with_exp_and_frac": 1.2345e10,
        "negative_float_with_exp_and_frac": -1.2345e10,
        "string": "example string"
      },
      "string escaping": "\"\\\/\b\f\n\r\t",
      "single_byte_utf8": "\u0061",
      "two_byte_utf8": "\u00BC \u0219",
      "three_byte_utf8": "\u2E26 \uFB04",
      "four_byte_utf8": "\uD83D\uDE80",
      "utf8_boundaries": "\u0000 \u007f \u0080 \u07ff \u0800 \uffff \ud800\udc00 \udbff\udfff"
    }
  |}
  in
  let t = Parsekit.run Json.parser input ~require_input_entirely_consumed:true in
  print_s [%sexp (t : Json.t)];
  [%expect
    {|
    (Dictionary
     ((array
       (List
        ((Number 1) (Number 2) (Number 3) (Number 4) (String 5) (String 6)
         (String 7) (String 8))))
      (false (Bool false)) (four_byte_utf8 (String "\240\159\154\128"))
      (null Null)
      (object
       (Dictionary
        ((big_number (Number 1.2345678901234567E+19))
         (float_with_exp (Number 1E-100))
         (float_with_exp_and_frac (Number 12345000000))
         (floating_point (Number 123.456))
         (negative_float_with_exp_and_frac (Number -12345000000))
         (negative_number (Number -1234)) (number (Number 1234))
         (string (String "example string")))))
      (single_byte_utf8 (String a))
      ("string escaping" (String  "\"\\/\b\012\
                                 \n\r\t"))
      (three_byte_utf8 (String "\226\184\166 \239\172\132")) (true (Bool true))
      (two_byte_utf8 (String "\194\188 \200\153"))
      (utf8_boundaries
       (String
        "\000 \127 \194\128 \223\191 \224\160\128 \239\191\191 \240\144\128\128 \243\191\191\191")))) |}];
  print_endline (Json.dictionary_member_exn t ~key:"single_byte_utf8" |> Json.string_exn);
  print_endline (Json.dictionary_member_exn t ~key:"two_byte_utf8" |> Json.string_exn);
  print_endline (Json.dictionary_member_exn t ~key:"three_byte_utf8" |> Json.string_exn);
  print_endline (Json.dictionary_member_exn t ~key:"four_byte_utf8" |> Json.string_exn);
  [%expect {|
    a
    ¼ ș
    ⸦ ﬄ
    🚀 |}]
;;
