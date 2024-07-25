(** SIMD operations in OCaml.

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

[%%import "../src/simd_support.h"]
[%%ifndef PARSEKIT_NO_SIMD]

let simd8_width = Simd.simd8_width

module type S = sig
  type t [@@deriving sexp_of]

  val to_string : t -> string
  val of_string_exn : string -> t
  val land_ : t -> t -> t
  val lxor_ : t -> t -> t
  val lsr_ : t -> int -> t
  val saturating_sub : t -> t -> t
  val lookup16 : table:t -> t -> t
  val align_and_drop_right : t -> t -> int -> t
  val no_bits_set : t -> bool
end

module Simd : S = struct
  type t = Simd.t [@@deriving sexp_of]

  let to_string = Simd.to_string

  let of_string_exn str =
    assert (String.length str = simd8_width);
    let t = Simd.create_exn ~init:0 in
    Simd.unsafe_load_string ~dst:t str ~pos:0;
    t
  ;;

  let land_ t1 t2 =
    let dst = Simd.create_exn ~init:0 in
    Simd.land_ ~dst t1 t2;
    dst
  ;;

  let lxor_ t1 t2 =
    let dst = Simd.create_exn ~init:0 in
    Simd.lxor_ ~dst t1 t2;
    dst
  ;;

  let lsr_ t n =
    let dst = Simd.create_exn ~init:0 in
    Simd.lsr_ ~dst t n;
    dst
  ;;

  let saturating_sub t1 t2 =
    let dst = Simd.create_exn ~init:0 in
    Simd.saturating_sub ~dst t1 t2;
    dst
  ;;

  let lookup16 ~table t =
    let dst = Simd.create_exn ~init:0 in
    Simd.lookup16 ~dst ~table t;
    dst
  ;;

  let align_and_drop_right t1 t2 n =
    let dst = Simd.create_exn ~init:0 in
    Simd.align_and_drop_right ~dst t1 t2 n;
    dst
  ;;

  let no_bits_set = Simd.no_bits_set
end

module Naive : S = struct
  type t = string

  let sexp_of_t t =
    let[@inline] hex_digit value =
      match 0 <= value && value <= 9 with
      | true -> Char.unsafe_of_int (value + Char.to_int '0')
      | false -> Char.unsafe_of_int (value - 10 + Char.to_int 'a')
    in
    let hex_bytes = Bytes.create (simd8_width * 2) in
    String.iteri t ~f:(fun idx chr ->
      let byte = Char.to_int chr in
      Bytes.unsafe_set hex_bytes (2 * idx) (hex_digit (byte lsr 4));
      Bytes.unsafe_set hex_bytes ((2 * idx) + 1) (hex_digit (byte land 0x0f)));
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:hex_bytes
    |> [%sexp_of: string]
  ;;

  let to_string = Fn.id

  let of_string_exn str =
    assert (String.length str = simd8_width);
    str
  ;;

  let land_ t1 t2 =
    String.init simd8_width ~f:(fun idx ->
      let c1 = String.unsafe_get t1 idx in
      let c2 = String.unsafe_get t2 idx in
      Char.unsafe_of_int (Char.to_int c1 land Char.to_int c2))
  ;;

  let lxor_ t1 t2 =
    String.init simd8_width ~f:(fun idx ->
      let c1 = String.unsafe_get t1 idx in
      let c2 = String.unsafe_get t2 idx in
      Char.unsafe_of_int (Char.to_int c1 lxor Char.to_int c2))
  ;;

  let lsr_ t n =
    String.init simd8_width ~f:(fun idx ->
      let c = String.unsafe_get t idx in
      Char.unsafe_of_int (Char.to_int c lsr n))
  ;;

  let saturating_sub t1 t2 =
    String.init simd8_width ~f:(fun idx ->
      let c1 = String.unsafe_get t1 idx in
      let c2 = String.unsafe_get t2 idx in
      Char.unsafe_of_int (Int.max 0 (Char.to_int c1 - Char.to_int c2)))
  ;;

  let lookup16 ~table t =
    String.init simd8_width ~f:(fun idx ->
      let byte = Char.to_int (String.unsafe_get t idx) in
      assert (0 <= byte && byte <= 15);
      String.unsafe_get table (idx - (idx % 16) + byte))
  ;;

  let align_and_drop_right t1 t2 n =
    List.append (String.to_list t1) (String.to_list t2)
    |> List.rev
    |> Fn.flip List.drop n
    |> Fn.flip List.take simd8_width
    |> List.rev
    |> String.of_list
  ;;

  let no_bits_set t = String.for_all t ~f:(Char.( = ) (Char.unsafe_of_int 0))
end

module Dual : S = struct
  type t = Simd.t * Naive.t

  let sexp_of_t (simd, naive) =
    let sexp_simd = [%sexp (simd : Simd.t)] in
    let sexp_naive = [%sexp (naive : Naive.t)] in
    [%test_result: Sexp.t] ~expect:sexp_naive sexp_simd;
    sexp_simd
  ;;

  let to_string (simd, naive) =
    let str_simd = Simd.to_string simd in
    let str_naive = Naive.to_string naive in
    [%test_result: string] ~expect:str_naive str_simd;
    str_simd
  ;;

  let of_string_exn str = Simd.of_string_exn str, Naive.of_string_exn str
  let land_ (s1, n1) (s2, n2) = Simd.land_ s1 s2, Naive.land_ n1 n2
  let lxor_ (s1, n1) (s2, n2) = Simd.lxor_ s1 s2, Naive.lxor_ n1 n2
  let lsr_ (s1, n1) by = Simd.lsr_ s1 by, Naive.lsr_ n1 by

  let saturating_sub (s1, n1) (s2, n2) =
    Simd.saturating_sub s1 s2, Naive.saturating_sub n1 n2
  ;;

  let lookup16 ~table:(stbl, ntbl) (s, n) =
    Simd.lookup16 ~table:stbl s, Naive.lookup16 ~table:ntbl n
  ;;

  let align_and_drop_right (s1, n1) (s2, n2) n =
    Simd.align_and_drop_right s1 s2 n, Naive.align_and_drop_right n1 n2 n
  ;;

  let no_bits_set (s, n) =
    let res_simd = Simd.no_bits_set s in
    let res_naive = Naive.no_bits_set n in
    [%test_result: bool] ~expect:res_naive res_simd;
    res_simd
  ;;
end

module Operation = struct
  type input = string

  let sexp_of_input input = [%sexp (Naive.of_string_exn input : Naive.t)]

  let quickcheck_generator_input =
    Base_quickcheck.Generator.string_with_length_of
      ~length:simd8_width
      (Base_quickcheck.Generator.char_uniform_inclusive Char.min_value Char.max_value)
  ;;

  type shift = int [@@deriving sexp_of]

  let quickcheck_generator_shift = Base_quickcheck.Generator.int_uniform_inclusive 0 8

  type drop = int [@@deriving sexp_of]

  let quickcheck_generator_drop = Base_quickcheck.Generator.int_uniform_inclusive 1 15

  type input_no_bits_set = string [@@deriving sexp_of]

  let quickcheck_generator_input_no_bits_set =
    match%bind.Base_quickcheck.Generator Base_quickcheck.Generator.bool with
    | true ->
      Base_quickcheck.Generator.return
        (String.init simd8_width ~f:(fun (_ : int) -> Char.unsafe_of_int 0))
    | false -> quickcheck_generator_input
  ;;

  type t =
    | Land of input * input
    | Lxor of input * input
    | Lsr of input * shift
    | Saturating_sub of input * input
    | Lookup16 of
        { table : input
        ; t : input
        }
    | Align_and_drop_right of input * input * drop
    | No_bits_set of input_no_bits_set
  [@@deriving sexp_of, quickcheck ~generator]

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

  let check_all dual =
    let (_ : Sexp.t) = Dual.sexp_of_t dual in
    let (_ : string) = Dual.to_string dual in
    let (_ : bool) = Dual.no_bits_set dual in
    ()
  ;;

  let run = function
    | Land (t1, t2) ->
      check_all (Dual.land_ (Dual.of_string_exn t1) (Dual.of_string_exn t2))
    | Lxor (t1, t2) ->
      check_all (Dual.lxor_ (Dual.of_string_exn t1) (Dual.of_string_exn t2))
    | Lsr (t, n) -> check_all (Dual.lsr_ (Dual.of_string_exn t) n)
    | Saturating_sub (t1, t2) ->
      check_all (Dual.saturating_sub (Dual.of_string_exn t1) (Dual.of_string_exn t2))
    | Lookup16 { table; t } ->
      let t = String.map t ~f:(fun chr -> Char.unsafe_of_int (Char.to_int chr land 15)) in
      check_all (Dual.lookup16 ~table:(Dual.of_string_exn table) (Dual.of_string_exn t))
    | Align_and_drop_right (t1, t2, n) ->
      check_all
        (Dual.align_and_drop_right (Dual.of_string_exn t1) (Dual.of_string_exn t2) n)
    | No_bits_set t ->
      let (_ : bool) = Dual.no_bits_set (Dual.of_string_exn t) in
      ()
  ;;
end

let%expect_test "quickcheck" =
  Base_quickcheck.Test.run_exn
    (module Operation)
    ~f:Operation.run
    ~config:{ Base_quickcheck.Test.default_config with test_count = 100_000 };
  [%expect {||}]
;;

[%%endif]
