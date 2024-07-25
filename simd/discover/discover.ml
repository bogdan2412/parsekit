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
module C = Configurator.V1

let c_flags = [ "-march=native" ]

let detect_arm_neon =
  {|
#include <arm_neon.h>

int main() {
  uint8x16_t a, b, c;

  a = vmovq_n_u8(0);
  b = vmovq_n_u8(1);
  c = vqtbl1q_u8(a, b);
  return vmaxvq_u8(c) == 0;
}
|}
;;

let detect_x64_sse41 =
  {|
#include <x86intrin.h>

int main () {
  __m128i a, b, c;

  a = _mm_set1_epi8(0);
  b = _mm_set1_epi8(1);
  c = _mm_shuffle_epi8(a, b);
  return _mm_testz_si128(c, c);
}
|}
;;

let detect_x64_avx2 =
  {|
#include <x86intrin.h>

int main () {
  __m256i a, b, c;

  a = _mm256_set1_epi8(0);
  b = _mm256_set1_epi8(1);
  c = _mm256_shuffle_epi8(a, b);
  return _mm256_testz_si256(c, c);
}
|}
;;

let detect_x64_avx512 =
  {|
#include <x86intrin.h>

int main () {
  __m512i a, b, c;

  a = _mm512_set1_epi8(0);
  b = _mm512_set1_epi8(1);
  c = _mm512_shuffle_epi8(a, b);
  return _mm512_test_epi8_mask(t, t) == 0;
}
|}
;;

let () =
  C.main ~name:"parsekit" (fun c ->
    let support_arm_neon = C.c_test ~c_flags c detect_arm_neon in
    let support_x64_sse41 = C.c_test ~c_flags c detect_x64_sse41 in
    let support_x64_avx2 = C.c_test ~c_flags c detect_x64_avx2 in
    let support_x64_avx512 = C.c_test ~c_flags c detect_x64_avx512 in
    let no_simd =
      not (support_arm_neon || support_x64_sse41 || support_x64_avx2 || support_x64_avx512)
    in
    let use_arm_neon, use_x64_sse41, use_x64_avx2, use_x64_avx512 =
      match support_arm_neon, support_x64_sse41, support_x64_avx2, support_x64_avx512 with
      | false, false, false, false -> false, false, false, false
      | true, false, false, false -> true, false, false, false
      | false, true, false, false -> false, true, false, false
      | false, true, true, false -> false, false, true, false
      | false, true, true, true -> false, false, false, true
      | _ ->
        raise_s
          [%message
            "Inconsistent discoveries"
              (support_arm_neon : bool)
              (support_x64_sse41 : bool)
              (support_x64_avx2 : bool)
              (support_x64_avx512 : bool)]
    in
    C.C_define.gen_header_file
      c
      ~fname:"simd_support.h"
      [ "PARSEKIT_ARM_NEON", Switch use_arm_neon
      ; "PARSEKIT_X64_SSE41", Switch use_x64_sse41
      ; "PARSEKIT_X64_AVX2", Switch use_x64_avx2
      ; "PARSEKIT_X64_AVX512", Switch use_x64_avx512
      ; "PARSEKIT_NO_SIMD", Switch no_simd
      ])
;;
