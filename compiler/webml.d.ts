/* tslint:disable */
/* eslint-disable */
/**
* @param {string} input
* @returns {Uint8Array}
*/
export function compile_string(input: string): Uint8Array;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly compile_string: (a: number, b: number, c: number) => void;
  readonly atou8_range: (a: number, b: number) => number;
  readonly atou16_range: (a: number, b: number) => number;
  readonly atou32_range: (a: number, b: number) => number;
  readonly atou64_range: (a: number, b: number) => number;
  readonly atousize_range: (a: number, b: number) => number;
  readonly atoi8_range: (a: number, b: number) => number;
  readonly atoi16_range: (a: number, b: number) => number;
  readonly atoi32_range: (a: number, b: number) => number;
  readonly atoi64_range: (a: number, b: number) => number;
  readonly atoisize_range: (a: number, b: number) => number;
  readonly try_atou8_range: (a: number, b: number, c: number) => void;
  readonly try_atou16_range: (a: number, b: number, c: number) => void;
  readonly try_atou32_range: (a: number, b: number, c: number) => void;
  readonly try_atou64_range: (a: number, b: number, c: number) => void;
  readonly try_atoi8_range: (a: number, b: number, c: number) => void;
  readonly try_atoi16_range: (a: number, b: number, c: number) => void;
  readonly try_atoi32_range: (a: number, b: number, c: number) => void;
  readonly try_atoi64_range: (a: number, b: number, c: number) => void;
  readonly atou128_range: (a: number, b: number, c: number) => void;
  readonly atoi128_range: (a: number, b: number, c: number) => void;
  readonly try_atou128_range: (a: number, b: number, c: number) => void;
  readonly try_atoi128_range: (a: number, b: number, c: number) => void;
  readonly try_atoisize_range: (a: number, b: number, c: number) => void;
  readonly try_atousize_range: (a: number, b: number, c: number) => void;
  readonly is_success: (a: number, b: number) => number;
  readonly is_overflow: (a: number, b: number) => number;
  readonly is_invalid_digit: (a: number, b: number) => number;
  readonly is_empty: (a: number, b: number) => number;
  readonly get_nan_string_ffi: (a: number, b: number) => number;
  readonly set_nan_string_ffi: (a: number, b: number) => number;
  readonly get_inf_string_ffi: (a: number, b: number) => number;
  readonly set_inf_string_ffi: (a: number, b: number) => number;
  readonly get_infinity_string_ffi: (a: number, b: number) => number;
  readonly set_infinity_string_ffi: (a: number, b: number) => number;
  readonly f32toa_range: (a: number, b: number, c: number) => number;
  readonly f64toa_range: (a: number, b: number, c: number) => number;
  readonly u8toa_range: (a: number, b: number, c: number) => number;
  readonly u16toa_range: (a: number, b: number, c: number) => number;
  readonly u32toa_range: (a: number, b: number, c: number) => number;
  readonly u64toa_range: (a: number, b: number, c: number) => number;
  readonly usizetoa_range: (a: number, b: number, c: number) => number;
  readonly i8toa_range: (a: number, b: number, c: number) => number;
  readonly i16toa_range: (a: number, b: number, c: number) => number;
  readonly i32toa_range: (a: number, b: number, c: number) => number;
  readonly i64toa_range: (a: number, b: number, c: number) => number;
  readonly isizetoa_range: (a: number, b: number, c: number) => number;
  readonly u128toa_range: (a: number, b: number, c: number, d: number) => number;
  readonly i128toa_range: (a: number, b: number, c: number, d: number) => number;
  readonly atof32_range: (a: number, b: number) => number;
  readonly atof64_range: (a: number, b: number) => number;
  readonly atof32_lossy_range: (a: number, b: number) => number;
  readonly atof64_lossy_range: (a: number, b: number) => number;
  readonly try_atof32_range: (a: number, b: number, c: number) => void;
  readonly try_atof64_range: (a: number, b: number, c: number) => void;
  readonly try_atof32_lossy_range: (a: number, b: number, c: number) => void;
  readonly try_atof64_lossy_range: (a: number, b: number, c: number) => void;
  readonly __wbindgen_malloc: (a: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number) => number;
  readonly __wbindgen_free: (a: number, b: number) => void;
}

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {InitInput | Promise<InitInput>} module_or_path
*
* @returns {Promise<InitOutput>}
*/
export default function init (module_or_path?: InitInput | Promise<InitInput>): Promise<InitOutput>;
        