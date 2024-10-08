(module
  (type $Float (sub (struct (field (mut f64)))))
  (type $Int64 (struct (field (mut i64))))
  (type $String (sub (array (mut i8))))
  (type $Gen_block (sub (array (mut (ref eq)))))


  (import "js_runtime" "print_string" (func $print_string (param (ref $String))))
  (import "js_runtime" "print_string_mem"
    (func $print_string_mem (param i32) (param i32)))
  (import "js_runtime" "print_endline" (func $print_endline))

  (import "js_runtime" "print_i32" (func $print_i32 (param i32)))
  (import "js_runtime" "print_f64" (func $print_f64 (param f64)))

  (import "js_runtime" "putchar" (func $putchar (param i32)))
  (import "js_runtime" "flush" (func $flush))

  (import "js_runtime" "memory" (memory $mem 1))

  (import "runtime" "compare_ints"
    (func $compare_int (param (ref eq)) (param (ref eq)) (result (ref i31))))

  (import "runtime" "string_eq"
    (func $string_eq (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))))

  (func (export "caml_int64_float_of_bits") (param $x (ref eq)) (result (ref $Float))
    (struct.new $Float
      (f64.reinterpret_i64
        (struct.get $Int64 0 (ref.cast (ref $Int64) (local.get $x))))))

  (func (export "caml_int64_float_of_bits_unboxed") (param $x i64) (result f64)
      (f64.reinterpret_i64 (local.get $x)))

  ;; (func (export "caml_obj_tag") (param $obj (ref eq)) (result (ref eq))
  ;;     (unreachable))

  ;; (func (export "caml_obj_make_forward") (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
  ;;     (unreachable))

  ;; (func (export "caml_lazy_make_forward") (param $a (ref eq)) (result (ref eq))
  ;;     (unreachable))

  ;; (func (export "caml_obj_block") (param $tag (ref eq)) (param $size (ref eq)) (result (ref eq))
  ;;     (unreachable))

  (global $oo_id (mut i32) (i32.const 0))

  (func (export "caml_fresh_oo_id") (param $unit (ref eq)) (result (ref eq))
      (local $oo_id i32)
      (global.set $oo_id
        (i32.add (i32.const 1)
          (local.tee $oo_id (global.get $oo_id))))
      (ref.i31 (local.get $oo_id))
  )

  ;; =====
  ;; Bytes
  ;; =====

  (func (export "caml_create_bytes") (param $size (ref eq)) (result (ref eq))
      (array.new_default $String (i31.get_s (ref.cast (ref i31) (local.get $size))))
  )

  ;; (func $caml_fill_bytes (param $arr (ref $String))
  ;;                        (param $off i32) (param $length i32)
  ;;                        (param $value i32)
  ;;   (block $break
  ;;     (loop $continue
  ;;       (br_if $break (i32.le_s (local.get $length) (i32.const 0)))
  ;;       (array.set $String (local.get $arr) (local.get $off) (local.get $value))
  ;;       (local.set $off (i32.add (local.get $off) (i32.const 1)))
  ;;       (local.set $length (i32.sub (local.get $length) (i32.const 1)))
  ;;       (br $continue)
  ;;     )
  ;;   )
  ;; )

  ;; (func (export "caml_fill_bytes") (param $arr (ref eq))
  ;;                                  (param $off (ref eq)) (param $length (ref eq))
  ;;                                  (param $value (ref eq)) (result (ref eq))
  ;;   (call $caml_fill_bytes
  ;;     (ref.cast $String (local.get $arr))
  ;;     (i31.get_s (ref.cast i31 (local.get $off)))
  ;;     (i31.get_s (ref.cast i31 (local.get $length)))
  ;;     (i31.get_s (ref.cast i31 (local.get $value))))
  ;;   (i31.new (i32.const 0)))

  (export "caml_bytes_equal" (func $string_eq))
  (export "caml_string_equal" (func $string_eq))


   ;; Stolen from Jerome's wasm_of_ocaml
   (func $compare_strings
      (param $s1 (ref $String)) (param $s2 (ref $String)) (result i32)
      (local $l1 i32) (local $l2 i32) (local $len i32) (local $i i32)
      (local $c1 i32) (local $c2 i32)
      (if (ref.eq (local.get $s1) (local.get $s2))
         (then (return (i32.const 0))))
      (local.set $l1 (array.len (local.get $s1)))
      (local.set $l2 (array.len (local.get $s2)))
      (local.set $len
        (select (local.get $l1) (local.get $l2)
           (i32.le_u (local.get $l1) (local.get $l2))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $c1
                  (array.get_u $String (local.get $s1) (local.get $i)))
               (local.set $c2
                  (array.get_u $String (local.get $s2) (local.get $i)))
               (if (i32.ne (local.get $c1) (local.get $c2))
                  (then
                     (if (i32.le_u (local.get $c1) (local.get $c2))
                        (then (return (i32.const -1)))
                        (else (return (i32.const 1))))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i32.sub (local.get $l1) (local.get $l2)))

  (func $caml_string_compare (param $a (ref eq)) (param $b (ref eq)) (result (ref i31))
      (ref.i31 (call $compare_strings
        (ref.cast (ref $String) (local.get $a))
        (ref.cast (ref $String) (local.get $b)))))

  (export "caml_bytes_compare" (func $caml_string_compare))
  (export "caml_string_compare" (func $caml_string_compare))

  (func (export "caml_blit_string") (param (ref eq)) (param (ref eq))
                                    (param (ref eq)) (param (ref eq))
                                    (param (ref eq))
                                    (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_blit_bytes") (param (ref eq)) (param (ref eq))
                                   (param (ref eq)) (param (ref eq))
                                   (param (ref eq))
                                   (result (ref eq))
      ;; TODO
      (unreachable))

  ;; ======
  ;; Stdlib
  ;; ======

  ;; (func (export "caml_classify_float_unboxed") (param f64) (result (ref eq))
  ;;     ;; TODO
  ;;     (unreachable))

  ;; ==========
  ;; Comparison
  ;; ==========

  ;; int < block < unknown
  (func (export "caml_compare") (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
    (local $a_block (ref $Gen_block))
    (local $b_block (ref $Gen_block))
    (if (result (ref i31)) (ref.test (ref i31) (local.get $a))
      (then
        (if (result (ref i31)) (ref.test (ref i31) (local.get $b))
        (then (return_call $compare_int (local.get $a) (local.get $b)))
        (else (ref.i31 (i32.const -1))))
      )
      (else
        (if (result (ref i31)) (ref.test (ref i31) (local.get $b))
        (then (ref.i31 (i32.const 1)))
        (else
          (local.set $b_block
          (block $both_block (result (ref $Gen_block))
            (local.set $a_block
            (block $a_is_block (result (ref $Gen_block))
              (drop (br_on_cast $a_is_block (ref eq) (ref $Gen_block) (local.get $a)))
              (return_call $caml_compare_data_non_block (local.get $a) (local.get $b))
            ))
            ;; a block, b unknown
            (drop (br_on_cast $both_block (ref eq) (ref $Gen_block) (local.get $b)))
            (return (ref.i31 (i32.const -1)))
          ))
          ;; Both blocks (test b = block)
          (local.set $a_block (ref.cast (ref $Gen_block) (local.get $a)))
          ;; This cast shouldn't be required
          (return_call $caml_compare_blocks (local.get $a_block) (local.get $b_block))
        )
        )
      )
    )
  )

  (func $caml_compare_data_non_block (export "compare_data_non_block") (param $a (ref eq)) (param $b (ref eq)) (result (ref i31))
    ;; (block $both_string (result (ref $String))

    ;; )
    (return_call $caml_string_compare (local.get $a) (local.get $b))
  )

  (func $caml_compare_blocks (param $a (ref $Gen_block)) (param $b (ref $Gen_block)) (result (ref i31))
    (local $len_a i32) (local $len_b i32)
    (local.set $len_a (array.len (local.get $a)))
    (local.set $len_b (array.len (local.get $b)))
    (if (i32.ne (local.get $len_a) (local.get $len_b))
        (then
          (return (ref.i31
            (i32.sub
              (i32.gt_s (local.get $len_a) (local.get $len_b))
              (i32.lt_s (local.get $len_a) (local.get $len_b)))))))
    ;; TODO
    (unreachable)
  )

  (func $caml_equal (export "caml_equal") (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
    (local $a_block (ref $Gen_block))
    (local $b_block (ref $Gen_block))
    (if (result (ref i31)) (ref.test (ref i31) (local.get $a))
      (then
        (if (result (ref i31)) (ref.test (ref i31) (local.get $b))
        (then (ref.i31 (ref.eq (local.get $a) (local.get $b))))
        (else (ref.i31 (i32.const 0))))
      )
      (else
        (if (result (ref i31)) (ref.test (ref i31) (local.get $b))
        (then (ref.i31 (i32.const 0)))
        (else
          (local.set $b_block
          (block $both_block (result (ref $Gen_block))
            (local.set $a_block
            (block $a_is_block (result (ref $Gen_block))
              (drop (br_on_cast $a_is_block (ref eq) (ref $Gen_block) (local.get $a)))
              (return_call $equal_data_non_block (local.get $a) (local.get $b))
            ))
            (drop (local.get $a_block))
            ;; a block, b unknown
            (drop (br_on_cast $both_block (ref eq) (ref $Gen_block) (local.get $b)))
            (return (ref.i31 (i32.const 0)))
          ))
          ;; Both blocks (test b = block)
          (local.set $a_block (ref.cast (ref $Gen_block) (local.get $a)))
          ;; This cast shouldn't be required
          (return_call $equal_blocks (local.get $a_block) (local.get $b_block))
        )
        )
      )
    )
  )

  (func $equal_data_non_block (export "equal_data_non_block") (param $a (ref eq)) (param $b (ref eq)) (result (ref i31))
    (ref.i31 (i32.eq
      (i32.const 0)
      (call $compare_strings
        (ref.cast (ref $String) (local.get $a))
        (ref.cast (ref $String) (local.get $b)))))
  )

  (func $equal_blocks (param $a (ref $Gen_block)) (param $b (ref $Gen_block)) (result (ref i31))
    (local $len_a i32)
    (local $len_b i32)
    (local $v_a (ref eq))
    (local $v_b (ref eq))
    (local $i i32)
    (local.set $len_a (array.len (local.get $a)))
    (local.set $len_b (array.len (local.get $b)))
    (if (i32.ne (local.get $len_a) (local.get $len_b))
        (then (return (ref.i31 (i32.const 0)))))
    ;; Same length
    (loop $loop
      (if (i32.eq (local.get $i) (local.get $len_a))
        (then (return (ref.i31 (i32.const 1)))))
      (local.set $v_a (array.get $Gen_block (local.get $a) (local.get $i)))
      (local.set $v_b (array.get $Gen_block (local.get $b) (local.get $i)))
      (if (ref.eq
            (ref.i31 (i32.const 0))
            (call $caml_equal (local.get $v_a) (local.get $v_b)))
        (then (return (ref.i31 (i32.const 0)))))
      (local.set $i (i32.add (i32.const 1) (local.get $i)))
      (br $loop)
    )
  )


  (func (export "caml_notequal") (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_lessequal") (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_greaterequal") (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))


    ;; Conversions
    ;; ===========

   ;; Stolen from Jerome's wasm_of_ocaml
   (func $format_int_default (param $d i32) (result (ref eq))
      (local $s (ref $String))
      (local $negative i32) (local $i i32) (local $n i32)
      (if (i32.lt_s (local.get $d) (i32.const 0))
         (then
            (local.set $negative (i32.const 1))
            (local.set $i (i32.const 1))
            (local.set $d (i32.sub (i32.const 0) (local.get $d)))))
      (local.set $n (local.get $d))
      (loop $count
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (local.set $n (i32.div_u (local.get $n) (i32.const 10)))
         (br_if $count (local.get $n)))
      (local.set $s (array.new $String (i32.const 0) (local.get $i)))
      (loop $write
         (local.set $i (i32.sub (local.get $i) (i32.const 1)))
         (array.set $String (local.get $s) (local.get $i)
            (i32.add (i32.const 48)
               (i32.rem_u (local.get $d) (i32.const 10))))
         (local.set $d (i32.div_u (local.get $d) (i32.const 10)))
         (br_if $write (local.get $d)))
      (if (local.get $negative)
         (then
            (array.set $String (local.get $s) (i32.const 0)
               (i32.const 45)))) ;; '-'
      (local.get $s))

  (func (export "caml_format_int") (param $format (ref eq)) (param $d (ref eq)) (result (ref eq))
      (call $format_int_default (i31.get_s (ref.cast (ref i31) (local.get $d)))))

  (func (export "caml_format_float") (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_int_of_string") (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_float_of_string") (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

    ;; Channels
    ;; ========

  (func (export "caml_ml_open_descriptor_out") (param (ref eq)) (result (ref eq))
    (ref.cast (ref i31) (local.get 0)))

  (func (export "caml_ml_open_descriptor_in") (param (ref eq)) (result (ref eq))
    (ref.cast (ref i31) (local.get 0)))

  (func (export "caml_sys_open") (param (ref eq)) (param (ref eq))
                                 (param (ref eq))
                                 (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_flush") (param (ref eq))
                                 (result (ref eq))
      (call $flush)
      (ref.i31 (i32.const 0)))

  (func $cons (param $h (ref eq)) (param $t (ref eq)) (result (ref $Gen_block))
     (array.new_fixed $Gen_block 3
       (ref.i31 (i32.const 0))
       (local.get $h)
       (local.get $t)))

  (global $empty_list (ref eq) (ref.i31 (i32.const 0)))

  (func (export "caml_ml_out_channels_list") (param (ref eq))
                                 (result (ref eq))
     (call $cons (ref.i31 (i32.const 0)) (global.get $empty_list))
  )

   (func $caml_ml_output (export "caml_ml_output")
      (param $ch (ref eq)) (param $s (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $pos i32) (local $len i32)
      (local.set $pos (i31.get_s (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (loop $loop
         (if (i32.gt_s (local.get $len) (i32.const 0))
            (then
               (call $putchar
                  (array.get $String
                    (ref.cast (ref $String) (local.get $s))
                    (local.get $pos)))
               (local.set $pos (i32.add (local.get $pos) (i32.const 1)))
               (local.set $len (i32.sub (local.get $len) (i32.const 1)))
               (br $loop))))
      (ref.i31 (i32.const 0)))

  (func (export "caml_ml_output_bytes") (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
                                   (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_output_int") (param (ref eq)) (param (ref eq))
                                   (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_output_char") (param $ch (ref eq)) (param $char (ref eq))
                                   (result (ref eq))
    (call $putchar (i31.get_s (ref.cast (ref i31) (local.get $char))))
    (ref.i31 (i32.const 0))
  )

  (func (export "caml_ml_output_string") (param (ref eq)) (param (ref eq))
                                   (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_output_value") (param (ref eq)) (param (ref eq)) (param (ref eq))
                                   (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_seek_out") (param (ref eq)) (param (ref eq))
                                   (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_seek_out_64") (param (ref eq)) (param (ref eq))
                                   (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_input_scan_line") (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_input") (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
                                   (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_input_char") (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_input_int") (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_input_value") (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_seek_in") (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_seek_in_64") (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_pos_in") (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_pos_in_64") (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_channel_size") (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_channel_size_64") (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_close_channel") (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_pos_out") (param (ref eq))
                                 (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_pos_out_64") (param (ref eq))
                                 (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_set_binary_mode") (param (ref eq)) (param (ref eq))
                                   (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_set_channel_name") (param (ref eq)) (param (ref eq))
                                   (result (ref eq))
      ;; TODO
      (unreachable))

 (func (export "caml_sys_exit") (param (ref eq))
                                 (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_register_named_value") (param (ref eq)) (param (ref eq))
                                 (result (ref eq))
      ;; TODO
    (ref.i31 (i32.const 0)))


 (func $C_caml_sys_getenv  (export "caml_sys_getenv") (param (ref eq)) (result (ref eq)) (unreachable))

 (global $os_type (ref $String) (array.new_fixed $String 4 (i32.const 87)(i32.const 97)(i32.const 115)(i32.const 109)))

 (func $C_caml_sys_get_config  (export "caml_sys_get_config") (param (ref eq)) (result (ref eq))
   (array.new_fixed $Gen_block 4
     (ref.i31 (i32.const 0))
     (global.get $os_type)
     (ref.i31 (i32.const 32))
     (ref.i31 (i32.const 0))
   )
 )

  (global $executable_name (ref $String) (array.new_fixed $String 15
    (i32.const 119)
    (i32.const 97)
    (i32.const 115)
    (i32.const 111)
    (i32.const 99)
    (i32.const 97)
    (i32.const 109)
    (i32.const 108)
    (i32.const 95)
    (i32.const 98)
    (i32.const 105)
    (i32.const 110)
    (i32.const 97)
    (i32.const 114)
    (i32.const 121)))

 (func $C_caml_sys_executable_name  (export "caml_sys_executable_name") (param (ref eq)) (result (ref eq))
   (global.get $executable_name))
 (func $C_caml_ml_runtime_warnings_enabled  (export "caml_ml_runtime_warnings_enabled") (param (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_ml_enable_runtime_warnings  (export "caml_ml_enable_runtime_warnings") (param (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_install_signal_handler  (export "caml_install_signal_handler") (param (ref eq) (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_obj_tag  (export "caml_obj_tag") (param (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_obj_raw_field  (export "caml_obj_raw_field") (param (ref eq) (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_floatarray_set  (export "caml_floatarray_set") (param (ref eq) (ref eq) (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_floatarray_get  (export "caml_floatarray_get") (param (ref eq) (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_ephe_unset_key  (export "caml_ephe_unset_key") (param (ref eq) (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_ephe_unset_data  (export "caml_ephe_unset_data") (param (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_ephe_set_key  (export "caml_ephe_set_key") (param (ref eq) (ref eq) (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_ephe_set_data  (export "caml_ephe_set_data") (param (ref eq) (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_ephe_get_key_copy  (export "caml_ephe_get_key_copy") (param (ref eq) (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_ephe_get_key  (export "caml_ephe_get_key") (param (ref eq) (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_ephe_get_data_copy  (export "caml_ephe_get_data_copy") (param (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_ephe_get_data  (export "caml_ephe_get_data") (param (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_ephe_create  (export "caml_ephe_create") (param (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_ephe_check_key  (export "caml_ephe_check_key") (param (ref eq) (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_ephe_check_data  (export "caml_ephe_check_data") (param (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_ephe_blit_key  (export "caml_ephe_blit_key") (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_ephe_blit_data  (export "caml_ephe_blit_data") (param (ref eq) (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_obj_make_forward  (export "caml_obj_make_forward") (param (ref eq) (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_obj_block  (export "caml_obj_block") (param (ref eq) (ref eq)) (result (ref eq)) (unreachable))
 (func $C_caml_lazy_make_forward  (export "caml_lazy_make_forward") (param (ref eq)) (result (ref eq)) (unreachable))

  (func (export "caml_gc_major") (param (ref eq)) (result (ref eq))
    (ref.i31 (i32.const 0)))
  (func (export "caml_gc_minor") (param (ref eq)) (result (ref eq))
    (ref.i31 (i32.const 0)))

  (func (export "caml_sys_const_naked_pointers_checked") (param (ref eq)) (result (ref eq))
    (ref.i31 (i32.const 0)))

  ;; ==================
  ;; CamlinternalFormat
  ;; ==================

  (func (export "caml_int32_format") (param (ref eq)) (param (ref eq)) (result (ref eq)) (unreachable))
  (func (export "caml_nativeint_format") (param (ref eq)) (param (ref eq)) (result (ref eq)) (unreachable))
  (func (export "caml_int64_format") (param (ref eq)) (param (ref eq)) (result (ref eq)) (unreachable))
  (func (export "caml_hexstring_of_float") (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq)) (unreachable))

  ;; =====
  ;; Array
  ;; =====

  (func (export "caml_make_vect") (param (ref eq)) (param (ref eq)) (result (ref eq)) (unreachable))
  (func (export "caml_make_float_vect") (param (ref eq)) (result (ref eq)) (unreachable))
  (func (export "caml_array_sub") (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq)) (unreachable))
  (func (export "caml_array_append") (param (ref eq)) (param (ref eq)) (result (ref eq)) (unreachable))
  (func (export "caml_array_concat") (param (ref eq)) (result (ref eq)) (unreachable))
  (func (export "caml_array_blit") (param (ref eq)) (param (ref eq)) (param (ref eq))
                     (param (ref eq)) (param (ref eq)) (result (ref eq)) (unreachable))
  (func (export "caml_array_fill") (param (ref eq)) (param (ref eq)) (param (ref eq))
                     (param (ref eq)) (result (ref eq)) (unreachable))



  ;; ====
  ;; MISC
  ;; ====

  (func (export "print_int") (param $a (ref eq)) (result (ref eq))
    (call $print_i32 (i31.get_s (ref.cast (ref i31) (local.get $a))))
    (ref.i31 (i32.const 0))
  )
  (func (export "print_float") (param $a (ref eq)) (result (ref eq))
    (call $print_f64 (struct.get $Float 0 (ref.cast (ref $Float) (local.get $a))))
    (ref.i31 (i32.const 0))
  )

  ;; (func (export "print_string") (param $a (ref eq)) (result (ref eq))
  ;;   (call $print_string (ref.cast $String (local.get $a)))
  ;;   (i31.new (i32.const 0)))

  (func $copy_string (param $s (ref $String)) (result i32)
    (local $len i32)
    (local $pos i32)
    (local.set $len (array.len (local.get $s)))
    (local.set $pos (i32.const 0))
    (loop $continue
       (if (i32.lt_u (local.get $pos) (local.get $len))
           (then
              (i32.store8 $mem
                (local.get $pos)
                (array.get $String (local.get $s) (local.get $pos)))
              (local.set $pos (i32.add (i32.const 1) (local.get $pos)))
              (br $continue)
           )
           (else (return (local.get $len))))
    )

)

  (func (export "print_string") (param $a (ref eq)) (result (ref eq))
    (call $print_string_mem (i32.const 0)
      (call $copy_string (ref.cast (ref $String) (local.get $a))))
    (ref.i31 (i32.const 0)))

  (func (export "print_endline") (param $a (ref eq)) (result (ref eq))
    (call $print_endline)
    (ref.i31 (i32.const 0)))
)
