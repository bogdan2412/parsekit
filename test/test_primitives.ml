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

let%expect_test "fail" =
  let t = fail "test" in
  show_raise (fun () -> run t "a" false [%sexp_of: _]);
  [%expect {| (raised ("Parsekit.ParseError(0, \"test\")")) |}];
  show_raise (fun () -> run t "a" true [%sexp_of: _]);
  [%expect {| (raised ("Parsekit.ParseError(0, \"test\")")) |}]
;;

let%expect_test "return" =
  let t = return 3 in
  run t "" false [%sexp_of: int];
  [%expect {| 3 |}];
  run t "" true [%sexp_of: int];
  [%expect {| 3 |}];
  run t "a" false [%sexp_of: int];
  [%expect {| 3 |}];
  show_raise (fun () -> run t "a" true [%sexp_of: int]);
  [%expect {| (raised ("Parsekit.ParseError(0, \"not at end of input\")")) |}]
;;

let%expect_test "at_end_of_input" =
  let t = take ~len:3 >> at_end_of_input in
  run t "abcd" false [%sexp_of: bool];
  [%expect {| false |}];
  run t "abc" false [%sexp_of: bool];
  [%expect {| true |}]
;;

let%expect_test "end_of_input" =
  let t = take ~len:3 << end_of_input in
  show_raise (fun () -> run t "abcd" false [%sexp_of: string]);
  [%expect {| (raised ("Parsekit.ParseError(3, \"not at end of input\")")) |}];
  run t "abc" false [%sexp_of: string];
  [%expect {| abc |}]
;;

let%expect_test "char parsers" =
  run peek1 "" true [%sexp_of: char option];
  [%expect {| () |}];
  run peek1 "abc" false [%sexp_of: char option];
  [%expect {| (a) |}];
  run (peek1 << match_ "abc") "abc" false [%sexp_of: char option];
  [%expect {| (a) |}];
  run (skip1 << match_ "bc") "abc" true [%sexp_of: unit];
  [%expect {| () |}];
  show_raise (fun () -> run skip1 "" false [%sexp_of: unit]);
  [%expect {| (raised ("Parsekit.ParseError(0, \"insufficient input\")")) |}];
  run (take1 << match_ "bc") "abc" true [%sexp_of: char];
  [%expect {| a |}];
  show_raise (fun () -> run take1 "" false [%sexp_of: char]);
  [%expect {| (raised ("Parsekit.ParseError(0, \"insufficient input\")")) |}];
  run (match1 'a' << match_ "bc") "abc" true [%sexp_of: char];
  [%expect {| a |}];
  show_raise (fun () -> run (match1 'b') "abc" true [%sexp_of: char]);
  [%expect {| (raised ("Parsekit.ParseError(0, \"expected character b\")")) |}];
  run (take1_cond Char.is_digit) "2" true [%sexp_of: char];
  [%expect {| 2 |}];
  show_raise (fun () -> run (take1_cond Char.is_digit) "a" false [%sexp_of: char]);
  [%expect
    {| (raised ("Parsekit.ParseError(0, \"character did not satisfy condition\")")) |}]
;;

let%expect_test "peek" =
  let t = peek ~len:3 in
  run t "abcd" false [%sexp_of: string option];
  [%expect {| (abc) |}];
  run t "abc" false [%sexp_of: string option];
  [%expect {| (abc) |}];
  run t "ab" false [%sexp_of: string option];
  [%expect {| () |}];
  show_raise (fun () -> run t "abc" true [%sexp_of: string option]);
  [%expect {| (raised ("Parsekit.ParseError(0, \"not at end of input\")")) |}];
  run (peek ~len:0 << match_ "abc") "abc" true [%sexp_of: string option];
  [%expect {| ("") |}];
  show_raise (fun () -> peek ~len:(-1));
  [%expect {| (raised ("Expected [len] argument to be at least 0" (len -1))) |}]
;;

let%expect_test "skip" =
  let t = skip ~len:3 in
  run t "abcd" false [%sexp_of: unit];
  [%expect {| () |}];
  run t "abc" false [%sexp_of: unit];
  [%expect {| () |}];
  run t "abc" true [%sexp_of: unit];
  [%expect {| () |}];
  show_raise (fun () -> run t "ab" false [%sexp_of: unit]);
  [%expect {| (raised ("Parsekit.ParseError(0, \"insufficient input\")")) |}];
  run (skip ~len:0 << match_ "abc") "abc" true [%sexp_of: unit];
  [%expect {| () |}];
  show_raise (fun () -> skip ~len:(-1));
  [%expect {| (raised ("Expected [len] argument to be at least 0" (len -1))) |}]
;;

let%expect_test "take" =
  let t = take ~len:3 in
  run t "abcd" false [%sexp_of: string];
  [%expect {| abc |}];
  run t "abc" false [%sexp_of: string];
  [%expect {| abc |}];
  run t "abc" true [%sexp_of: string];
  [%expect {| abc |}];
  show_raise (fun () -> run t "ab" false [%sexp_of: string]);
  [%expect {| (raised ("Parsekit.ParseError(0, \"insufficient input\")")) |}];
  run (take ~len:0 << match_ "abc") "abc" true [%sexp_of: string];
  [%expect {| "" |}];
  show_raise (fun () -> take ~len:(-1));
  [%expect {| (raised ("Expected [len] argument to be at least 0" (len -1))) |}]
;;

let%expect_test "match_" =
  let t = match_ "abc" in
  run t "abcd" false [%sexp_of: string];
  [%expect {| abc |}];
  run t "abc" false [%sexp_of: string];
  [%expect {| abc |}];
  run t "abc" true [%sexp_of: string];
  [%expect {| abc |}];
  show_raise (fun () -> run t "ab" false [%sexp_of: string]);
  [%expect {| (raised ("Parsekit.ParseError(0, \"insufficient input\")")) |}];
  run (match_ "") "abc" false [%sexp_of: string];
  [%expect {| "" |}]
;;

let%expect_test "fold" =
  let t =
    fold ~init:0 ~f:(fun acc ~peek ->
      match peek with
      | `Eof -> `Return acc
      | `Char char ->
        (match Char.( <= ) '0' char && Char.( <= ) char '9' with
         | false -> `Return acc
         | true ->
           (match Char.to_int char = Char.to_int '0' + acc with
            | true -> `Advance (acc + 1)
            | false -> `Fail [%string "Expected %{acc#Int}"])))
  in
  run t "01234" true [%sexp_of: int];
  [%expect {| 5 |}];
  run (t << match_ "abc") "01234abc" true [%sexp_of: int];
  [%expect {| 5 |}];
  show_raise (fun () -> run t "01245" false [%sexp_of: int]);
  [%expect {| (raised ("Parsekit.ParseError(3, \"Expected 3\")")) |}];
  run t "a" false [%sexp_of: int];
  [%expect {| 0 |}];
  run t "0a" false [%sexp_of: int];
  [%expect {| 1 |}]
;;

let%expect_test "take_while" =
  let valid_char = function
    | 'a' | 'b' | 'c' -> true
    | _ -> false
  in
  let t = take_while ~f:valid_char ~at_least:3 ~at_most:(Some 5) in
  run t "abcd" false [%sexp_of: string];
  [%expect {| abc |}];
  run t "abc" false [%sexp_of: string];
  [%expect {| abc |}];
  run t "abc" true [%sexp_of: string];
  [%expect {| abc |}];
  show_raise (fun () -> run t "ab" false [%sexp_of: string]);
  [%expect
    {| (raised ("Parsekit.ParseError(2, \"expected at least 3 matching chars\")")) |}];
  run t "abcba" false [%sexp_of: string];
  [%expect {| abcba |}];
  run t "abccba" false [%sexp_of: string];
  [%expect {| abccb |}];
  let t = take_while ~f:valid_char ~at_least:3 ~at_most:(Some 3) in
  run t "abccba" false [%sexp_of: string];
  [%expect {| abc |}];
  let t = take_while ~f:valid_char ~at_least:0 ~at_most:None in
  run t "def" false [%sexp_of: string];
  [%expect {| "" |}];
  show_raise (fun () -> take_while ~f:valid_char ~at_least:(-1) ~at_most:None);
  [%expect {| (raised ("Expected [at_least] argument to be at least 0" (at_least -1))) |}];
  show_raise (fun () -> take_while ~f:valid_char ~at_least:3 ~at_most:(Some 2));
  [%expect
    {|
    (raised (
      "Expected [at_most] argument to be at least [at_least]"
      (at_least 3)
      (at_most  2))) |}]
;;

let%expect_test "whitespace" =
  run (consumed_bytes whitespace) " abcd" false [%sexp_of: string];
  [%expect {| " " |}];
  run (consumed_bytes whitespace) " \t abcd" false [%sexp_of: string];
  [%expect {| " \t " |}];
  show_raise (fun () -> run (consumed_bytes whitespace) "abcd" false [%sexp_of: string]);
  [%expect
    {| (raised ("Parsekit.ParseError(0, \"expected at least 1 matching chars\")")) |}]
;;

let%expect_test "integer" =
  run integer "01234" false [%sexp_of: int];
  [%expect {| 1234 |}];
  run integer "-1234" false [%sexp_of: int];
  [%expect {| -1234 |}];
  run non_negative_integer "01234" false [%sexp_of: int];
  [%expect {| 1234 |}];
  show_raise (fun () -> run non_negative_integer "-1234" false [%sexp_of: int]);
  [%expect
    {| (raised ("Parsekit.ParseError(0, \"expected at least 1 matching chars\")")) |}];
  show_raise (fun () -> run integer "abcd" false [%sexp_of: int]);
  [%expect
    {| (raised ("Parsekit.ParseError(0, \"expected at least 1 matching chars\")")) |}]
;;
