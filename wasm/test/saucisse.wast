(module
  (type $Float (struct (field (mut f64))))
  (type $Int64 (struct (field (mut i64))))

  (import "spectest" "print_i32" (func $print_i32 (param i32)))
  (import "spectest" "print_f64" (func $print_f64 (param f64)))
  (func (export "saucisse") (param $a (ref eq)) (result (ref eq))
    (call $print_i32 (i31.get_s (ref.cast i31 (local.get $a))))
    (i31.new (i32.const 0))
  )
  (func (export "saucissef") (param $a (ref eq)) (result (ref eq))
    (call $print_f64 (struct.get $Float 0 (ref.cast $Float (local.get $a))))
    (i31.new (i32.const 0))
  )

  (func (export "caml_int64_float_of_bits") (param $x (ref eq)) (result (ref $Float))
    (struct.new_canon $Float
      (f64.reinterpret_i64
        (struct.get $Int64 0 (ref.cast $Int64 (local.get $x))))))

  (func (export "caml_int64_float_of_bits_unboxed") (param $x i64) (result f64)
      (f64.reinterpret_i64 (local.get $x)))

)


(register "import")
