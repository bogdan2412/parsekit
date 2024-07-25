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

#include "simd.h"

#include <assert.h>

#include <caml/alloc.h>
#include <caml/memory.h>

#ifndef PARSEKIT_NO_SIMD

using namespace Parsekit::SIMD;

struct data {
  uint8_t values[SIMD8_WIDTH];
};

#define Data_val(x) ((struct data *)(Data_abstract_val(x)))

inline static simd8 load_ml(value v_t) { return load(&Data_val(v_t)->values); }

inline static void store_ml(value v_dst, const simd8 data) {
  return store(&Data_val(v_dst)->values, data);
}

inline static value alloc_ml() {
  return caml_alloc_small(SIMD8_WIDTH >> 3, Abstract_tag);
}

extern "C" {
value parsekit_simd8_width() { return Val_int(SIMD8_WIDTH); }

CAMLprim value parsekit_simd_to_string(value v_t) {
  CAMLparam1(v_t);
  value v_ret = caml_alloc_string(SIMD8_WIDTH);
  store(Bytes_val(v_ret), load_ml(v_t));
  CAMLreturn(v_ret);
}

CAMLprim value parsekit_simd_unsafe_create(value v_init) {
  CAMLparam1(v_init);
  value v_t = alloc_ml();
  store_ml(v_t, set_all(Int_val(v_init)));
  CAMLreturn(v_t);
}

CAMLprim value parsekit_simd_unsafe_create_repeat_array16(value v_data) {
  CAMLparam1(v_data);
  value v_t = alloc_ml();
  for (size_t idx = 0; idx < SIMD8_WIDTH; idx++) {
    Data_val(v_t)->values[idx] = Int_val(Field(v_data, idx & 15));
  }
  CAMLreturn(v_t);
}

value parsekit_simd_unsafe_load_string(value v_dst, value v_data, value v_pos) {
  const char *data = String_val(v_data) + Long_val(v_pos);
  store_ml(v_dst, load(data));
  return Val_unit;
}

value parsekit_simd_land(value v_dst, value v_t1, value v_t2) {
  store_ml(v_dst, land(load_ml(v_t1), load_ml(v_t2)));
  return Val_unit;
}

value parsekit_simd_lxor(value v_dst, value v_t1, value v_t2) {
  store_ml(v_dst, lxor(load_ml(v_t1), load_ml(v_t2)));
  return Val_unit;
}

value parsekit_simd_lsr(value v_dst, value v_t, value v_n) {
  store_ml(v_dst, lsr(load_ml(v_t), Int_val(v_n)));
  return Val_unit;
}

value parsekit_simd_saturating_sub(value v_dst, value v_t1, value v_t2) {
  store_ml(v_dst, saturating_sub(load_ml(v_t1), load_ml(v_t2)));
  return Val_unit;
}

value parsekit_simd_lookup16(value v_dst, value v_table, value v_src) {
  store_ml(v_dst, lookup16(load_ml(v_table), load_ml(v_src)));
  return Val_unit;
}

inline static simd8 align_and_drop_right_dispatch(const simd8 t1,
                                                  const simd8 t2,
                                                  const uint8_t drop) {
  assert(1 <= drop && drop <= 15);
  switch (drop) {
  case 1:
    return align_and_drop_right<1>(t1, t2);
  case 2:
    return align_and_drop_right<2>(t1, t2);
  case 3:
    return align_and_drop_right<3>(t1, t2);
  case 4:
    return align_and_drop_right<4>(t1, t2);
  case 5:
    return align_and_drop_right<5>(t1, t2);
  case 6:
    return align_and_drop_right<6>(t1, t2);
  case 7:
    return align_and_drop_right<7>(t1, t2);
  case 8:
    return align_and_drop_right<8>(t1, t2);
  case 9:
    return align_and_drop_right<9>(t1, t2);
  case 10:
    return align_and_drop_right<10>(t1, t2);
  case 11:
    return align_and_drop_right<11>(t1, t2);
  case 12:
    return align_and_drop_right<12>(t1, t2);
  case 13:
    return align_and_drop_right<13>(t1, t2);
  case 14:
    return align_and_drop_right<14>(t1, t2);
  default:
    return align_and_drop_right<15>(t1, t2);
  }
}

value parsekit_simd_align_and_drop_right(value v_dst, value v_t1, value v_t2,
                                         value v_drop) {
  store_ml(v_dst, align_and_drop_right_dispatch(load_ml(v_t1), load_ml(v_t2),
                                                Int_val(v_drop)));
  return Val_unit;
}

value parsekit_simd_no_bits_set(value v_t) {
  return Val_bool(no_bits_set(load_ml(v_t)));
}
}

#endif
