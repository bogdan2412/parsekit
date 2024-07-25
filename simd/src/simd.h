/* SIMD operations in OCaml.

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
   along with this program.  If not, see <https://www.gnu.org/licenses/>. */

#include "simd_support.h"

#ifndef PARSEKIT_NO_SIMD

#include <stdint.h>

#if defined(PARSEKIT_X64_SSE41) || defined(PARSEKIT_X64_AVX2) ||               \
    defined(PARSEKIT_X64_AVX512)
#include <x86intrin.h>
#elif defined(PARSEKIT_ARM_NEON)
#include <arm_neon.h>
#endif

namespace Parsekit {
namespace SIMD {

#if defined(PARSEKIT_X64_SSE41)
typedef __m128i simd8;
#elif defined(PARSEKIT_X64_AVX2)
typedef __m256i simd8;
#elif defined(PARSEKIT_X64_AVX512)
typedef __m512i simd8;
#elif defined(PARSEKIT_ARM_NEON)
typedef uint8x16_t simd8;
#endif

#if defined(PARSEKIT_X64_SSE41)
const uint8_t SIMD8_WIDTH = 16;
#elif defined(PARSEKIT_X64_AVX2)
const uint8_t SIMD8_WIDTH = 32;
#elif defined(PARSEKIT_X64_AVX512)
const uint8_t SIMD8_WIDTH = 64;
#elif defined(PARSEKIT_ARM_NEON)
const uint8_t SIMD8_WIDTH = 16;
#endif

inline static simd8 load(const void *data) {
#if defined(PARSEKIT_X64_SSE41)
  return _mm_loadu_si128((const simd8 *)data);
#elif defined(PARSEKIT_X64_AVX2)
  return _mm256_loadu_si256((const simd8 *)data);
#elif defined(PARSEKIT_X64_AVX512)
  return _mm512_loadu_si512((const simd8 *)data);
#elif defined(PARSEKIT_ARM_NEON)
  return vld1q_u8((const uint8_t *)data);
#endif
}

inline static void store(void *data, const simd8 value) {
#if defined(PARSEKIT_X64_SSE41)
  return _mm_storeu_si128((simd8 *)data, value);
#elif defined(PARSEKIT_X64_AVX2)
  return _mm256_storeu_si256((simd8 *)data, value);
#elif defined(PARSEKIT_X64_AVX512)
  return _mm512_storeu_si512((simd8 *)data, value);
#elif defined(PARSEKIT_ARM_NEON)
  return vst1q_u8((uint8_t *)data, value);
#endif
}

inline static simd8 set_all(uint8_t value) {
#if defined(PARSEKIT_X64_SSE41)
  return _mm_set1_epi8(value);
#elif defined(PARSEKIT_X64_AVX2)
  return _mm256_set1_epi8(value);
#elif defined(PARSEKIT_X64_AVX512)
  return _mm512_set1_epi8(value);
#elif defined(PARSEKIT_ARM_NEON)
  return vmovq_n_u8(value);
#endif
}

inline static simd8 land(const simd8 t1, const simd8 t2) {
#if defined(PARSEKIT_X64_SSE41)
  return _mm_and_si128(t1, t2);
#elif defined(PARSEKIT_X64_AVX2)
  return _mm256_and_si256(t1, t2);
#elif defined(PARSEKIT_X64_AVX512)
  return _mm512_and_si512(t1, t2);
#elif defined(PARSEKIT_ARM_NEON)
  return vandq_u8(t1, t2);
#endif
}

inline static simd8 lxor(const simd8 t1, const simd8 t2) {
#if defined(PARSEKIT_X64_SSE41)
  return _mm_xor_si128(t1, t2);
#elif defined(PARSEKIT_X64_AVX2)
  return _mm256_xor_si256(t1, t2);
#elif defined(PARSEKIT_X64_AVX512)
  return _mm512_xor_si512(t1, t2);
#elif defined(PARSEKIT_ARM_NEON)
  return veorq_u8(t1, t2);
#endif
}

inline static simd8 lsr(const simd8 t, const uint8_t n) {
#if defined(PARSEKIT_X64_SSE41)
  return land(_mm_srli_epi64(t, n), set_all(0xFF >> n));
#elif defined(PARSEKIT_X64_AVX2)
  return land(_mm256_srli_epi64(t, n), set_all(0xFF >> n));
#elif defined(PARSEKIT_X64_AVX512)
  return land(_mm512_srli_epi64(t, n), set_all(0xFF >> n));
#elif defined(PARSEKIT_ARM_NEON)
  switch (n) {
  case 0:
    return t;
  case 1:
    return vshrq_n_u8(t, 1);
  case 2:
    return vshrq_n_u8(t, 2);
  case 3:
    return vshrq_n_u8(t, 3);
  case 4:
    return vshrq_n_u8(t, 4);
  case 5:
    return vshrq_n_u8(t, 5);
  case 6:
    return vshrq_n_u8(t, 6);
  case 7:
    return vshrq_n_u8(t, 7);
  default:
    return vshrq_n_u8(t, 8);
  }
#endif
}

inline static simd8 saturating_sub(const simd8 t1, const simd8 t2) {
#if defined(PARSEKIT_X64_SSE41)
  return _mm_subs_epu8(t1, t2);
#elif defined(PARSEKIT_X64_AVX2)
  return _mm256_subs_epu8(t1, t2);
#elif defined(PARSEKIT_X64_AVX512)
  return _mm512_subs_epu8(t1, t2);
#elif defined(PARSEKIT_ARM_NEON)
  return vqsubq_u8(t1, t2);
#endif
}

inline static simd8 lookup16(const simd8 lookup_table, const simd8 value) {
#if defined(PARSEKIT_X64_SSE41)
  return _mm_shuffle_epi8(lookup_table, value);
#elif defined(PARSEKIT_X64_AVX2)
  return _mm256_shuffle_epi8(lookup_table, value);
#elif defined(PARSEKIT_X64_AVX512)
  return _mm512_shuffle_epi8(lookup_table, value);
#elif defined(PARSEKIT_ARM_NEON)
  return vqtbl1q_u8(lookup_table, value);
#endif
}

// Concatenates data in [t1] and [t2], drops the [drop] most significant bytes
// from [t2] and discards remaining data from [t1] that does not fit in a
// [simd8].
//
// Input:  <.....  t1  .....><.....  t2  .....>
// Output:           <...  return  ...>[ drop ]
template <uint8_t drop>
inline static simd8 align_and_drop_right(const simd8 t1, const simd8 t2) {
  static_assert(1 <= drop && drop <= 15,
                "[drop] must be between [1] and [15].");
#if defined(PARSEKIT_X64_SSE41)
  // The first term is the more significant one for __mm_alignr_epi8.
  return _mm_alignr_epi8(t2, t1, 16 - drop);
#elif defined(PARSEKIT_X64_AVX2)
  // __mm256_alignr_epi8(t2, t1, drop) operates on the two halves of each term
  // independently.
  //
  // If the input data looks like
  // <.. t1_low  ..><.. t1_high ..><.. t2_low  ..><.. t2_high ..>
  // the command will process each of
  // <.. t1_low  ..><.. t2_low  ..> and <.. t1_high ..><.. t2_high ..>
  //
  // Instead we'd like it to look at
  // <.. t1_high ..><.. t2_low  ..> and <.. t2_low  ..><.. t2_high ..>
  //
  // So the data should look like
  // new_t1: <.. t1_high ..><.. t2_low  ..>
  //     t2: <.. t2_low  ..><.. t2_high ..>
  return _mm256_alignr_epi8(t2, _mm256_permute2x128_si256(t1, t2, 0x21),
                            16 - drop);
#elif defined(PARSEKIT_X64_AVX512)
  // As in the AVX2 case, __mm512_alignr_epi8(t2, t1, drop) operates on the 4
  // 128-bit chunks of each term independently.
  //
  // If the input data looks like
  // <t1_1><t1_2><t1_3><t1_4><t2_1><t2_2><t2_3><t2_4>
  // we would first like to reshuffle it so that it looks like
  // new_t1: <t1_4><t2_1><t2_2><t2_3>
  //     t2: <t2_1><t2_2><t2_3><t2_4>
  const simd8 permute_idx = _mm512_setr_epi64(6, 7, 8, 9, 10, 11, 12, 13);
  const simd8 new_t1 = _mm512_permutex2var_epi64(t1, permute_idx, t2);
  return _mm512_alignr_epi8(t2, new_t1, 16 - drop);
#elif defined(PARSEKIT_ARM_NEON)
  return vextq_u8(t1, t2, 16 - drop);
#endif
}

inline static bool no_bits_set(const simd8 t) {
#if defined(PARSEKIT_X64_SSE41)
  return _mm_testz_si128(t, t);
#elif defined(PARSEKIT_X64_AVX2)
  return _mm256_testz_si256(t, t);
#elif defined(PARSEKIT_X64_AVX512)
  return _mm512_test_epi8_mask(t, t) == 0;
#elif defined(PARSEKIT_ARM_NEON)
  return vmaxvq_u8(t) == 0;
#endif
}

} // namespace SIMD
} // namespace Parsekit

#endif
