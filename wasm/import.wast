(module
  (type $Float (struct (field (mut f64))))
  (type $Int64 (struct (field (mut i64))))

  (func (export "caml_int64_float_of_bits") (param $x (ref eq)) (result (ref $Float))
    (struct.new_canon $Float
      (f64.reinterpret_i64
        (struct.get $Int64 0 (ref.cast $Int64 (local.get $x))))))

  (func (export "caml_int64_float_of_bits_unboxed") (param $x i64) (result f64)
      (f64.reinterpret_i64 (local.get $x)))

  (func (export "caml_obj_tag") (param $obj (ref eq)) (result (ref eq))
      (unreachable))

  (func (export "caml_obj_make_forward") (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
      (unreachable))

  (global $oo_id (mut i32) (i32.const 0))

  (func (export "caml_fresh_oo_id") (param $unit (ref eq)) (result (ref eq))
      (local $oo_id i32)
      (global.set $oo_id
        (i32.add (i32.const 1)
          (local.tee $oo_id (global.get $oo_id))))
      (i31.new (local.get $oo_id))
  )
)

(register "import")
