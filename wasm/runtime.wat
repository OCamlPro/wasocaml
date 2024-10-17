(module
  (type $Float (sub (struct (field (mut f64)))))
  (type $String (sub (array (mut i8))))
  (type $Array (sub (array (mut (ref eq)))))
  (type $FloatArray (sub (array (mut f64))))
  (type $Gen_block (sub (array (mut (ref eq)))))
  ;; TODO: re-enable exception
  ;;(import "exn_tag" "exc" (tag $exc (param (ref eq))))

  ;; ==========
  ;; Exceptions
  ;; ==========

  (global $index_out_of_bound_string (ref $String)
     (array.new_fixed $String 19
       (i32.const 105)(i32.const 110)(i32.const 100)(i32.const 101)(i32.const 120)
       (i32.const 32)(i32.const 111)(i32.const 117)(i32.const 116)(i32.const 32)
       (i32.const 111)(i32.const 102)(i32.const 32)(i32.const 98)(i32.const 111)
       (i32.const 117)(i32.const 110)(i32.const 100)(i32.const 115)))

  ;; TODO exceptions
  (global (export "caml_exn_Match_failure") (ref eq)
       (array.new_fixed $Gen_block 3
         (ref.i31 (i32.const 248))
         (array.new_fixed $String 3 (i32.const 78) (i32.const 78) (i32.const 78))
         (ref.i31 (i32.const 0))))
  (global (export "caml_exn_Assert_failure") (ref eq)
       (array.new_fixed $Gen_block 3
         (ref.i31 (i32.const 248))
         (array.new_fixed $String 3 (i32.const 78) (i32.const 78) (i32.const 78))
         (ref.i31 (i32.const 1))))
  (global $invalid_argument (export "caml_exn_Invalid_argument") (ref eq)
       (array.new_fixed $Gen_block 3
         (ref.i31 (i32.const 248))
         (array.new_fixed $String 3 (i32.const 78) (i32.const 78) (i32.const 78))
         (ref.i31 (i32.const 2))))
  (global (export "caml_exn_Failure") (ref eq)
       (array.new_fixed $Gen_block 3
         (ref.i31 (i32.const 248))
         (array.new_fixed $String 3 (i32.const 78) (i32.const 78) (i32.const 78))
         (ref.i31 (i32.const 3))))
  (global (export "caml_exn_Not_found") (ref eq)
     (array.new_fixed $Gen_block 3
       (ref.i31 (i32.const 248))
       (array.new_fixed $String 3 (i32.const 78) (i32.const 78) (i32.const 78))
       (ref.i31 (i32.const 4))))

  (global (export "caml_exn_Out_of_memory") (ref eq) (ref.i31 (i32.const 5)))
  (global (export "caml_exn_Stack_overflow") (ref eq) (ref.i31 (i32.const 6)))
  (global (export "caml_exn_Sys_error") (ref eq) (ref.i31 (i32.const 7)))
  (global (export "caml_exn_End_of_file") (ref eq) (ref.i31 (i32.const 8)))
  (global (export "caml_exn_Division_by_zero") (ref eq) (ref.i31 (i32.const 9)))
  (global (export "caml_exn_Sys_blocked_io") (ref eq) (ref.i31 (i32.const 10)))
  (global (export "caml_exn_Undefined_recursive_module") (ref eq) (ref.i31 (i32.const 11)))

  ;; =========
  ;; functions
  ;; =========

  (func (export "compare_ints") (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
    (local $a' i32) (local $b' i32)
    (local.set $a' (i31.get_s (ref.cast (ref i31) (local.get $a))))
    (local.set $b' (i31.get_s (ref.cast (ref i31) (local.get $b))))
    (ref.i31
      (i32.sub
        (i32.gt_s (local.get $a') (local.get $b'))
        (i32.lt_s (local.get $a') (local.get $b'))))
  )

  (func (export "compare_floats") (param $a (ref eq)) (param $b (ref eq)) (result (ref i31))
    (local $a' f64) (local $b' f64)
    (local.set $a' (struct.get $Float 0 (ref.cast (ref $Float) (local.get $a))))
    (local.set $b' (struct.get $Float 0 (ref.cast (ref $Float) (local.get $b))))
    (ref.i31
      (i32.add
        (i32.sub
          (f64.gt (local.get $a') (local.get $b'))
          (f64.lt (local.get $a') (local.get $b')))
        (i32.sub
          (f64.eq (local.get $a') (local.get $a'))
          (f64.eq (local.get $b') (local.get $b'))))))

  ;; ======
  ;; Arrays
  ;; ======

  (func $array_length (export "array_length") (param $arr (ref eq)) (result (ref eq))
      (block $floatarray (result (ref $FloatArray))
        (return (ref.i31 (array.len (ref.cast (ref $Array)
          (br_on_cast $floatarray (ref eq) (ref $FloatArray) (local.get $arr)))))))
  )

  (func $array_get_float_safe (param $arr (ref $FloatArray)) (param $field (ref eq)) (result (ref $Float))
    ;; TODO exceptions
    (struct.new $Float
      (array.get $FloatArray
        (local.get $arr)
        (i31.get_s (ref.cast (ref i31) (local.get $field)))))
  )

  (func (export "array_get_float_safe") (param $arr (ref eq)) (param $field (ref eq)) (result (ref eq))
    (call $array_get_float_safe
      (ref.cast (ref $FloatArray) (local.get $arr))
      (local.get $field)))

  (func $array_get_int_or_addr_safe (param $arr (ref $Array)) (param $field (ref eq)) (result (ref eq))
    ;; TODO exceptions
    (array.get $Array
      (local.get $arr)
      (i31.get_s (ref.cast (ref i31) (local.get $field))))
  )

  (func (export "array_get_int_or_addr_safe") (param $arr (ref eq)) (param $field (ref eq)) (result (ref eq))
    (call $array_get_int_or_addr_safe (ref.cast (ref $Array) (local.get $arr)) (local.get $field)))

  (func $array_get_safe (param $arr (ref eq)) (param $field (ref eq)) (result (ref eq))
    (return
      (call $array_get_float_safe
        (block $floatarray (result (ref $FloatArray))
          (br_on_cast $floatarray (ref eq) (ref $FloatArray) (local.get $arr))
          (return (call $array_get_int_or_addr_safe (ref.cast (ref $Array)) (local.get $field))))
        (local.get $field)))
  )


  (export "array_get_safe" (func $array_get_safe))
  ;; TODO: implement get_unsafe properly
  (export "array_get_unsafe" (func $array_get_safe))

  (func $array_set_float_unsafe (param $arr (ref eq)) (param $field (ref eq))
                                 (param $value (ref eq)) (result (ref eq))
       (array.set $FloatArray
         (ref.cast (ref $FloatArray) (local.get $arr))
         (i31.get_s (ref.cast (ref i31) (local.get $field)))
         (struct.get $Float 0 (ref.cast (ref $Float) (local.get $value))))
       (ref.i31 (i32.const 0))
   )

   (func $array_set_int_or_addr_unsafe (export "array_set_int_or_addr_unsafe") (param $arr (ref eq)) (param $field (ref eq))
                                       (param $value (ref eq)) (result (ref eq))
       (array.set $Array
         (ref.cast (ref $Array) (local.get $arr))
         (i31.get_s (ref.cast (ref i31) (local.get $field)))
         (local.get $value))
       (ref.i31 (i32.const 0))
   )

   ;; TODO: exception + implement properly
   (export "array_set_int_or_addr_safe" (func $array_set_int_or_addr_unsafe))

   (func $array_set_unsafe (export "array_set_unsafe")
                           (param $arr (ref eq)) (param $field (ref eq))
                           (param $value (ref eq)) (result (ref eq))
     (return
       (call $array_set_float_unsafe
         (block $floatarray (result (ref $FloatArray))
           (br_on_cast $floatarray (ref eq) (ref $FloatArray) (local.get $arr))
           (return
             (call $array_set_int_or_addr_unsafe
               (ref.cast (ref $Array)) (local.get $field) (local.get $value))))
         (local.get $field)
         (local.get $value)
       ))
   )

  (func (export "array_set_safe")
                          (param $arr (ref eq)) (param $field (ref eq))
                          (param $value (ref eq)) (result (ref eq))
    ;; TODO exceptions
    (call $array_set_unsafe (local.get $arr) (local.get $field) (local.get $value))
  )

  ;; TODO: exceptions
  (export "array_set_float_safe" (func $array_set_float_unsafe))

  (func (export "duparray") (param $src (ref eq)) (result (ref eq))
    ;; src is a block
    (local $a (ref $Array))
    (local $dst (ref $Array))
    (local.set $a (ref.cast (ref $Array) (local.get $src)))
    (local.set $dst
      (array.new $Array
        (ref.i31 (i32.const 42))
        (i32.add (i32.const -1) (array.len (local.get $a)))))
    (array.copy $Array $Array
      (local.get $dst)
      (i32.const 0)
      (local.get $a)
      (i32.const 1)
      (i32.add (i32.const -1) (array.len (local.get $a))))
    (local.get $dst)
  )

  ;; ============
  ;; String/Bytes
  ;; ============

  (func (export "bytes_set") (param $arr (ref eq)) (param $field (ref eq))
                             (param $value (ref eq)) (result (ref eq))
      ;; TODO exceptions
      (array.set $String
        (ref.cast (ref $String) (local.get $arr))
        (i31.get_s (ref.cast (ref i31) (local.get $field)))
        (i31.get_s (ref.cast (ref i31) (local.get $value))))
      (ref.i31 (i32.const 0))
  )

  (func (export "string_get") (param $arr (ref eq)) (param $field_i31 (ref eq))
                              (result (ref eq))
      (local $field i32)
      (local.set $field (i31.get_s (ref.cast (ref i31) (local.get $field_i31))))
      (if (result (ref i31))
        (i32.lt_s (local.get $field) (array.len (ref.cast (ref $String) (local.get $arr))))
        (then
          (ref.i31
            (array.get_s $String
              (ref.cast (ref $String) (local.get $arr))
              (local.get $field))))
        (else
          unreachable
          ;; TODO: re-enable exception
          (;(throw $exc
            (array.new_fixed $Gen_block 3
              (ref.i31 (i32.const 0))
              (global.get $invalid_argument)
              (global.get $index_out_of_bound_string)))
          ;)
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
        (else (nop)))
    (local.set $pos (i32.const 0))
    (loop $next_char
      (if (i32.eq (local.get $len_a) (local.get $pos))
        (then (return (i32.const 1)))
        (else (nop)))
      (if (i32.ne (array.get_s $String (local.get $a) (local.get $pos))
                  (array.get_s $String (local.get $b) (local.get $pos)))
        (then (return (i32.const 0)))
        (else
          (local.set $pos (i32.add (i32.const 1) (local.get $pos)))
          (br $next_char))))
    (unreachable)
  )

  (func (export "string_eq") (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
    (ref.i31
      (call $string_eq (ref.cast (ref $String) (local.get $a)) (ref.cast (ref $String) (local.get $b))))
  )


  ;; ==========
  ;; Exceptions
  ;; ==========

  ;; TODO exceptions
  ;;(global (export "caml_exn_Match_failure") (ref eq) (i31.new (i32.const 0)))
  ;;(global (export "caml_exn_Assert_failure") (ref eq) (i31.new (i32.const 1)))
  ;;(global (export "caml_exn_Invalid_argument") (ref eq) (i31.new (i32.const 2)))
  ;;(global (export "caml_exn_Failure") (ref eq) (i31.new (i32.const 3)))
  ;;(global (export "caml_exn_Not_found") (ref eq) (i31.new (i32.const 4)))
  ;;(global (export "caml_exn_Out_of_memory") (ref eq) (i31.new (i32.const 5)))
  ;;(global (export "caml_exn_Stack_overflow") (ref eq) (i31.new (i32.const 6)))
  ;;(global (export "caml_exn_Sys_error") (ref eq) (i31.new (i32.const 7)))
  ;;(global (export "caml_exn_End_of_file") (ref eq) (i31.new (i32.const 8)))
  ;;(global (export "caml_exn_Division_by_zero") (ref eq) (i31.new (i32.const 9)))
  ;;(global (export "caml_exn_Sys_blocked_io") (ref eq) (i31.new (i32.const 10)))
  ;;(global (export "caml_exn_Undefined_recursive_module") (ref eq) (i31.new (i32.const 11)))

  ;; ==========
  ;; Undefineds
  ;; ==========

  (func (export "unimplemented_1") (param (ref eq)) (result (ref eq))
    (unreachable))

  (func (export "unimplemented_2") (param (ref eq)) (param (ref eq)) (result (ref eq))
    (unreachable))

  (func (export "unimplemented_3") (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
    (unreachable))

)
