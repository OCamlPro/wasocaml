(module
  (type $Float (struct (field (mut f64))))
  (type $Array (array (mut (ref eq))))
  (type $FloatArray (array (mut f64)))

  (func (export "compare_ints") (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
    (local $a' i32) (local $b' i32)
    (local.set $a' (i31.get_s (ref.cast i31 (local.get $a))))
    (local.set $b' (i31.get_s (ref.cast i31 (local.get $b))))
    (i31.new
      (i32.sub
        (i32.gt_s (local.get $a') (local.get $b'))
        (i32.lt_s (local.get $a') (local.get $b'))))
  )

  (func (export "compare_floats") (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
    (local $a' f64) (local $b' f64)
    (local.set $a' (struct.get $Float 0 (ref.cast $Float (local.get $a))))
    (local.set $b' (struct.get $Float 0 (ref.cast $Float (local.get $b))))
    (i31.new
      (i32.add
        (i32.sub
          (f64.gt (local.get $a') (local.get $b'))
          (f64.lt (local.get $a') (local.get $b')))
        (i32.sub
          (f64.eq (local.get $a') (local.get $a'))
          (f64.eq (local.get $b') (local.get $b'))))))

  (func $array_get_float_safe (param $arr (ref $FloatArray)) (param $field (ref eq)) (result (ref $Float))
    ;; TODO exceptions
    (struct.new_canon $Float
      (array.get $FloatArray
        (local.get $arr)
        (i31.get_s (ref.cast i31 (local.get $field)))))
  )

  (func (export "array_get_float_safe") (param $arr (ref eq)) (param $field (ref eq)) (result (ref eq))
    (call $array_get_float_safe
      (ref.cast $FloatArray (local.get $arr))
      (local.get $field)))

  (func $array_get_int_or_addr_safe (param $arr (ref $Array)) (param $field (ref eq)) (result (ref eq))
    ;; TODO exceptions
    (array.get $Array
      (local.get $arr)
      (i31.get_s (ref.cast i31 (local.get $field))))
  )

  (func (export "array_get_int_or_addr_safe") (param $arr (ref eq)) (param $field (ref eq)) (result (ref eq))
    (call $array_get_int_or_addr_safe (ref.cast $Array (local.get $arr)) (local.get $field)))

  (func (export "array_get_safe") (param $arr (ref eq)) (param $field (ref eq)) (result (ref eq))
    (return
      (call $array_get_float_safe
        (block $floatarray (result (ref $FloatArray))
          (br_on_cast $floatarray $FloatArray (local.get $arr))
          (return (call $array_get_int_or_addr_safe (ref.cast $Array) (local.get $field))))
        (local.get $field)))
  )
)

(register "runtime")
