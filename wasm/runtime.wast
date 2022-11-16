(module
  (type $Float (struct (field (mut f64))))
  (type $String (array (mut i8)))
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

  (func $array_get_safe (param $arr (ref eq)) (param $field (ref eq)) (result (ref eq))
    (return
      (call $array_get_float_safe
        (block $floatarray (result (ref $FloatArray))
          (br_on_cast $floatarray $FloatArray (local.get $arr))
          (return (call $array_get_int_or_addr_safe (ref.cast $Array) (local.get $field))))
        (local.get $field)))
  )

  (export "array_get_safe" (func $array_get_safe))
  (export "array_get_unsafe" (func $array_get_safe))

  (func $array_set_float_unsafe (param $arr (ref $FloatArray)) (param $field (ref eq))
                                (param $value (ref eq)) (result (ref eq))
      (array.set $FloatArray
        (local.get $arr)
        (i31.get_s (ref.cast i31 (local.get $field)))
        (struct.get $Float 0 (ref.cast $Float (local.get $value))))
      (i31.new (i32.const 0))
  )

  (func $array_set_int_or_addr_unsafe (param $arr (ref $Array)) (param $field (ref eq))
                                      (param $value (ref eq)) (result (ref eq))
      (array.set $Array
        (local.get $arr)
        (i31.get_s (ref.cast i31 (local.get $field)))
        (local.get $value))
      (i31.new (i32.const 0))
  )

  (func (export "array_set_unsafe") (param $arr (ref eq)) (param $field (ref eq))
                                    (param $value (ref eq)) (result (ref eq))
    (return
      (call $array_set_float_unsafe
        (block $floatarray (result (ref $FloatArray))
          (br_on_cast $floatarray $FloatArray (local.get $arr))
          (return
            (call $array_set_int_or_addr_unsafe
              (ref.cast $Array) (local.get $field) (local.get $value))))
        (local.get $field)
        (local.get $value)
      ))
  )

  (func $string_eq (param $a (ref $String)) (param $b (ref $String)) (result i32)
    (local $len_a i32)
    (local $len_b i32)
    (local $pos i32)
    (local.set $len_a (array.len (local.get $a)))
    (local.set $len_b (array.len (local.get $b)))
    (if (i32.ne (local.get $len_a) (local.get $len_b))
        (then (return (i32.const 0)))
        (else))
    (local.set $pos (i32.const 0))
    (loop $next_char
      (if (i32.eq (local.get $len_a) (local.get $pos))
        (then (return (i32.const 1)))
        (else))
      (if (i32.ne (array.get_s $String (local.get $a) (local.get $pos))
                  (array.get_s $String (local.get $b) (local.get $pos)))
        (then (return (i32.const 0)))
        (else
          (local.set $pos (i32.add (i32.const 1) (local.get $pos)))
          (br $next_char))))
    (unreachable)
  )

  (func (export "string_eq") (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
    (i31.new
      (call $string_eq (ref.cast $String (local.get $a)) (ref.cast $String (local.get $b))))
  )

  ;; TODO exceptions
  (global (export "caml_exn_Assert_failure") (ref eq) (i31.new (i32.const 0)))
)

(register "runtime")
