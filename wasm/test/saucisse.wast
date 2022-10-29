(module
  (import "spectest" "print_i32" (func $print_i32 (param i32)))
  (func (export "saucisse") (param $a (ref eq)) (result (ref eq))
    (call $print_i32 (i31.get_s (ref.cast i31 (local.get $a))))
    (i31.new (i32.const 0))
  )

  (func (export "caml_int64_float_of_bits_unboxed") (param $x i64) (result f64)
      (f64.reinterpret_i64 (local.get $x)))

)


(register "import")
