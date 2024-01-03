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

let%expect_test "map" =
  let t =
    let%map value = integer in
    value * value
  in
  run t "3" true [%sexp_of: int];
  [%expect {| 9 |}];
  run t "5" true [%sexp_of: int];
  [%expect {| 25 |}]
;;

let%expect_test "bind" =
  let t =
    let%bind len = integer in
    take ~len
  in
  run t "3abc2de" false [%sexp_of: string];
  [%expect {| abc |}];
  run t "5abcde" true [%sexp_of: string];
  [%expect {| abcde |}]
;;

let%expect_test "map2" =
  let t =
    let%map x = integer
    and y = whitespace >> integer in
    x + y
  in
  run t "3 4" true [%sexp_of: int];
  [%expect {| 7 |}];
  run t "3 -3" true [%sexp_of: int];
  [%expect {| 0 |}]
;;

let%expect_test "ignore" =
  let t = ignore_m integer in
  run t "3" true [%sexp_of: unit];
  [%expect {| () |}];
  show_raise (fun () -> run t "a" true [%sexp_of: unit]);
  [%expect
    {| (raised ("Parsekit.ParseError(0, \"expected at least 1 matching chars\")")) |}]
;;

let%expect_test ">>" =
  let t = match_ "a" >> match_ "b" in
  run t "ab" true [%sexp_of: string];
  [%expect {| b |}];
  show_raise (fun () -> run t "a" false [%sexp_of: string]);
  [%expect {| (raised ("Parsekit.ParseError(1, \"insufficient input\")")) |}];
  show_raise (fun () -> run t "b" false [%sexp_of: string]);
  [%expect {| (raised ("Parsekit.ParseError(0, \"expected string a\")")) |}]
;;

let%expect_test "<<" =
  let t = match_ "a" << match_ "b" in
  run t "ab" true [%sexp_of: string];
  [%expect {| a |}];
  show_raise (fun () -> run t "a" false [%sexp_of: string]);
  [%expect {| (raised ("Parsekit.ParseError(1, \"insufficient input\")")) |}];
  show_raise (fun () -> run t "b" false [%sexp_of: string]);
  [%expect {| (raised ("Parsekit.ParseError(0, \"expected string a\")")) |}]
;;

let%expect_test "consumed_bytes" =
  let is_hex = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false
  in
  let t =
    match_ "0x"
    >> take_while ~f:is_hex ~at_least:1 ~at_most:None
    |> consumed_bytes
    |> map ~f:Int.Hex.of_string
  in
  run t "0xdeadbeef" true [%sexp_of: Int.Hex.t];
  [%expect {| 0xdeadbeef |}]
;;

let%expect_test "many" =
  let t = match_ "ab" |> many ~at_least:3 ~at_most:(Some 5) in
  let sexp_of = [%sexp_of: string list] in
  run t "ababab" true sexp_of;
  [%expect {| (ab ab ab) |}];
  run t "ababababab" true sexp_of;
  [%expect {| (ab ab ab ab ab) |}];
  run t "abababababab" false sexp_of;
  [%expect {| (ab ab ab ab ab) |}];
  show_raise (fun () -> run t "abab" false sexp_of);
  [%expect
    {|
    (raised (
      "Parsekit.ParseError(4, \"not enough instances of `parser`: insufficient input\")")) |}];
  let t = match_ "ab" |> many ~at_least:3 ~at_most:(Some 3) in
  run t "ababababab" false sexp_of;
  [%expect {| (ab ab ab) |}];
  let t = match_ "ab" |> many ~at_least:0 ~at_most:(Some 0) in
  run t "ababab" false sexp_of;
  [%expect {| () |}];
  let t = match_ "ab" |> many ~at_least:0 ~at_most:(Some 1) in
  run t "ababab" false sexp_of;
  [%expect {| (ab) |}];
  run t "cdcdcd" false sexp_of;
  [%expect {| () |}];
  let t =
    many
      (let%map a = match_ "a"
       and b = match_ "b" in
       String.append a b)
      ~at_least:0
      ~at_most:None
  in
  (* Ensure parser does not consume trailing "a" which matches the
     first half of the [map2]. *)
  run (t << match_ "a") "aba" true sexp_of;
  [%expect {| (ab) |}]
;;

let%expect_test "many nested" =
  let t =
    take ~len:1 |> many ~at_least:0 ~at_most:None |> many ~at_least:4 ~at_most:(Some 10)
  in
  let sexp_of = [%sexp_of: string list list] in
  run t "" true sexp_of;
  [%expect {| (() () () ()) |}];
  run t "1" true sexp_of;
  [%expect {| ((1) () () ()) |}];
  run t "12" true sexp_of;
  [%expect {| ((1 2) () () ()) |}];
  let t =
    take ~len:1 |> many ~at_least:0 ~at_most:None |> many ~at_least:4 ~at_most:None
  in
  run t "12" true sexp_of;
  [%expect {| ((1 2) () () ()) |}];
  let t =
    take ~len:1 |> many ~at_least:1 ~at_most:None |> many ~at_least:4 ~at_most:None
  in
  show_raise (fun () -> run t "12345678" true sexp_of);
  [%expect
    {|
    (raised (
      "Parsekit.ParseError(8, \"not enough instances of `parser`: not enough instances of `parser`: insufficient input\")")) |}];
  let t =
    take ~len:1 |> many ~at_least:1 ~at_most:(Some 2) |> many ~at_least:4 ~at_most:None
  in
  run t "12345678" true sexp_of;
  [%expect {| ((1 2) (3 4) (5 6) (7 8)) |}]
;;

let%expect_test "sep_by" =
  let t = sep_by (match_ "a") ~sep:whitespace ~at_least:3 ~at_most:(Some 5) in
  let sexp_of = [%sexp_of: string list] in
  run t "a a  a" true sexp_of;
  [%expect {| (a a a) |}];
  (* Ensure parser does not consume trailing separator *)
  run (t << match_ " ") "a a  a\ta " true sexp_of;
  [%expect {| (a a a a) |}];
  run t "a a  a\ta a" true sexp_of;
  [%expect {| (a a a a a) |}];
  run t "a a  a\ta a a" false sexp_of;
  [%expect {| (a a a a a) |}];
  show_raise (fun () -> run t "a a" false sexp_of);
  [%expect
    {|
    (raised (
      "Parsekit.ParseError(3, \"not enough instances of `parser`: expected at least 1 matching chars\")")) |}];
  let t = sep_by (match_ "a") ~sep:whitespace ~at_least:3 ~at_most:(Some 3) in
  run t "a a a a a" false sexp_of;
  [%expect {| (a a a) |}];
  let t = sep_by (match_ "a") ~sep:whitespace ~at_least:0 ~at_most:(Some 0) in
  run t "a a a" false sexp_of;
  [%expect {| () |}];
  let t = sep_by (match_ "a") ~sep:whitespace ~at_least:0 ~at_most:(Some 1) in
  run t "a a a" false sexp_of;
  [%expect {| (a) |}];
  run t "b b b" false sexp_of;
  [%expect {| () |}]
;;

let%expect_test "either" =
  let t =
    either (match_ "abc") (many (match_ "a") ~at_least:1 ~at_most:None |> consumed_bytes)
  in
  let sexp_of = [%sexp_of: (string, string) Either.t] in
  run t "abc" true sexp_of;
  [%expect {| (First abc) |}];
  run t "a" true sexp_of;
  [%expect {| (Second a) |}];
  run t "aa" true sexp_of;
  [%expect {| (Second aa) |}];
  show_raise (fun () -> run t "b" false sexp_of);
  [%expect {| (raised ("Parsekit.ParseError(0, \"no matching choice\")")) |}];
  let t = either (match_ "ab") integer in
  let sexp_of = [%sexp_of: (string, int) Either.t] in
  run t "ab" true sexp_of;
  [%expect {| (First ab) |}];
  run t "123" true sexp_of;
  [%expect {| (Second 123) |}]
;;

let%expect_test "choices" =
  let t =
    choices
      [ match_ "abc"
      ; many (match_ "a") ~at_least:1 ~at_most:None |> consumed_bytes
      ; match_ "b"
      ]
  in
  let sexp_of = [%sexp_of: string] in
  run t "abc" true sexp_of;
  [%expect {| abc |}];
  run t "a" true sexp_of;
  [%expect {| a |}];
  run t "aa" true sexp_of;
  [%expect {| aa |}];
  run t "b" true sexp_of;
  [%expect {| b |}];
  show_raise (fun () -> run t "c" false sexp_of);
  [%expect {| (raised ("Parsekit.ParseError(0, \"no matching choice\")")) |}];
  let t = choices [ match_ "abc" ] in
  run t "abc" true sexp_of;
  [%expect {| abc |}];
  show_raise (fun () -> run t "bcd" true sexp_of);
  [%expect {| (raised ("Parsekit.ParseError(0, \"no matching choice\")")) |}];
  let t = choices [] in
  show_raise (fun () -> run t "abc" false [%sexp_of: _]);
  [%expect {| (raised ("Parsekit.ParseError(0, \"no matching choice\")")) |}]
;;

let%expect_test "commit avoids exponential glob implementation behaviour" =
  let t =
    (* Build the glob [a*a*a*...a*b] where [a*] is repeated 100 times.

       Removing the [commit] will cause exponential behaviour for the
       non-matching input below and result in this test not terminating in a
       reasonable amount of time. *)
    List.fold (List.init 7 ~f:Fn.id) ~init:(match_ "b") ~f:(fun acc (_ : int) ->
      match_ "a" >> commit >> fix (fun glob -> choices [ acc; take ~len:1 >> glob ]))
    |> consumed_bytes
  in
  let str = String.init 60 ~f:(Fn.const 'a') in
  show_raise (fun () -> run t str true [%sexp_of: string]);
  [%expect
    {|
    (raised (
      "Parsekit.ParseError(7, \"parser cannot backtrack to position 6, it committed at position 7: no matching choice\")")) |}];
  run t (str ^ "b") true [%sexp_of: string];
  [%expect {| aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab |}];
  run
    t
    "accccccacccccaccccaccccccacccccaccccccacccccacaccccccccaccccab"
    true
    [%sexp_of: string];
  [%expect {| accccccacccccaccccaccccccacccccaccccccacccccacaccccccccaccccab |}]
;;

let%expect_test "commit" =
  let t = choices [ match_ "a" >> match_ "b" >> match_ "c"; match_ "abd" ] in
  run t "abd" true [%sexp_of: string];
  [%expect {| abd |}];
  let t = choices [ match_ "a" >> commit >> match_ "b" >> match_ "c"; match_ "abd" ] in
  show_raise (fun () -> run t "abd" false [%sexp_of: string]);
  [%expect
    {|
    (raised (
      "Parsekit.ParseError(2, \"parser cannot backtrack to position 0, it committed at position 1: expected string c\")")) |}];
  let t = many (consumed_bytes (match_ "ab" >> match_ "c")) ~at_least:0 ~at_most:None in
  run t "abcabcabd" false [%sexp_of: string list];
  [%expect {| (abc abc) |}];
  let t =
    many (consumed_bytes (match_ "ab" >> commit >> match_ "c")) ~at_least:0 ~at_most:None
  in
  show_raise (fun () -> run t "abcabcabd" false [%sexp_of: string list]);
  [%expect
    {|
    (raised (
      "Parsekit.ParseError(8, \"parser cannot backtrack to position 6, it committed at position 8: expected string c\")")) |}]
;;

let%expect_test "all" =
  let t = all [ match_ "abc"; match_ "de"; match_ "f" ] in
  let sexp_of = [%sexp_of: string list] in
  run t "abcdef" true sexp_of;
  [%expect {| (abc de f) |}];
  show_raise (fun () -> run t "abcde" false sexp_of);
  [%expect {| (raised ("Parsekit.ParseError(5, \"insufficient input\")")) |}];
  let t = all [ match_ "abc" ] in
  run t "abc" true sexp_of;
  [%expect {| (abc) |}];
  show_raise (fun () -> run t "ab" false sexp_of);
  [%expect {| (raised ("Parsekit.ParseError(0, \"insufficient input\")")) |}];
  let t = all [] in
  run t "abcd" false sexp_of;
  [%expect {| () |}];
  run t "" true sexp_of;
  [%expect {| () |}]
;;

let%expect_test "fix" =
  let simple_double_quoted_string =
    match_ {|"|}
    >> take_while
         ~f:(function
           | '\\' | '"' -> false
           | _ -> true)
         ~at_least:0
         ~at_most:None
    << match_ {|"|}
  in
  let list json =
    match_ "["
    >> whitespace0
    >> sep_by
         json
         ~sep:(whitespace0 >> match_ "," << whitespace0)
         ~at_least:0
         ~at_most:None
    << whitespace0
    << match_ "]"
  in
  let open struct
    type t =
      | String of string
      | Int of int
      | List of t list
    [@@deriving sexp_of]
  end in
  let json =
    fix (fun json ->
      choices
        [ (let%map string = simple_double_quoted_string in
           String string)
        ; (let%map int = integer in
           Int int)
        ; (let%map list = list json in
           List list)
        ])
  in
  let sexp_of = [%sexp_of: t] in
  run json {|"abcd"|} true sexp_of;
  [%expect {| (String abcd) |}];
  run json "1234" true sexp_of;
  [%expect {| (Int 1234) |}];
  run json "[]" true sexp_of;
  [%expect {| (List ()) |}];
  run json {|[ "abcd", "efg", 89 ]|} true sexp_of;
  [%expect {| (List ((String abcd) (String efg) (Int 89))) |}];
  run json {|[ "abcd", "efg", 89, [ 10, 11, 12 ] ]|} true sexp_of;
  [%expect
    {| 
    (List
     ((String abcd) (String efg) (Int 89) (List ((Int 10) (Int 11) (Int 12))))) |}]
;;
