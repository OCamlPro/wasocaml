(module
  (type $Float (struct (field (mut f64))))
  (type $Int64 (struct (field (mut i64))))
  (type $String (array (mut i8)))

  (import "runtime" "string_eq"
    (func $string_eq (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))))

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

  (func (export "caml_lazy_make_forward") (param $a (ref eq)) (result (ref eq))
      (unreachable))

  (func (export "caml_obj_block") (param $tag (ref eq)) (param $size (ref eq)) (result (ref eq))
      (unreachable))

  (global $oo_id (mut i32) (i32.const 0))

  (func (export "caml_fresh_oo_id") (param $unit (ref eq)) (result (ref eq))
      (local $oo_id i32)
      (global.set $oo_id
        (i32.add (i32.const 1)
          (local.tee $oo_id (global.get $oo_id))))
      (i31.new (local.get $oo_id))
  )

  ;; =====
  ;; Bytes
  ;; =====

  (func (export "caml_create_bytes") (param $size (ref eq)) (result (ref eq))
      (array.new_canon_default $String (i31.get_s (ref.cast i31 (local.get $size))))
  )

  (func $caml_fill_bytes (param $arr (ref $String))
                         (param $off i32) (param $length i32)
                         (param $value i32)
    (block $break
      (loop $continue
        (br_if $break (i32.le_s (local.get $length) (i32.const 0)))
        (array.set $String (local.get $arr) (local.get $off) (local.get $value))
        (local.set $off (i32.add (local.get $off) (i32.const 1)))
        (local.set $length (i32.sub (local.get $length) (i32.const 1)))
        (br $continue)
      )
    )
  )

  (func (export "caml_fill_bytes") (param $arr (ref eq))
                                   (param $off (ref eq)) (param $length (ref eq))
                                   (param $value (ref eq)) (result (ref eq))
    (call $caml_fill_bytes
      (ref.cast $String (local.get $arr))
      (i31.get_s (ref.cast i31 (local.get $off)))
      (i31.get_s (ref.cast i31 (local.get $length)))
      (i31.get_s (ref.cast i31 (local.get $value))))
    (i31.new (i32.const 0)))

  (export "caml_bytes_equal" (func $string_eq))
  (export "caml_string_equal" (func $string_eq))

  (func $string_compare (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (export "caml_bytes_compare" (func $string_compare))
  (export "caml_string_compare" (func $string_compare))

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

  (func (export "caml_classify_float_unboxed") (param f64) (result (ref eq))
      ;; TODO
      (unreachable))

    ;; Comparison
    ;; ==========

  (func (export "caml_compare") (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_equal") (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

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

  (func (export "caml_format_int") (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))

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
    (ref.cast i31 (local.get 0)))

  (func (export "caml_ml_open_descriptor_in") (param (ref eq)) (result (ref eq))
    (ref.cast i31 (local.get 0)))

  (func (export "caml_sys_open") (param (ref eq)) (param (ref eq))
                                 (param (ref eq))
                                 (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_flush") (param (ref eq))
                                 (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_out_channels_list") (param (ref eq))
                                 (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_output") (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
                                   (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_output_bytes") (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
                                   (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_output_int") (param (ref eq)) (param (ref eq))
                                   (result (ref eq))
      ;; TODO
      (unreachable))

  (func (export "caml_ml_output_char") (param (ref eq)) (param (ref eq))
                                   (result (ref eq))
      ;; TODO
      (unreachable))

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
    (i31.new (i32.const 0)))


  (func (export "caml_gc_major") (param (ref eq)) (result (ref eq))
    (i31.new (i32.const 0)))

  (func (export "caml_sys_const_naked_pointers_checked") (param (ref eq)) (result (ref eq))
    (i31.new (i32.const 0)))

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

)
