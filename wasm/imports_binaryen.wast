(module
  (type $Float (struct (field (mut f64))))
  (type $Int64 (struct (field (mut i64))))
  (type $String (array (mut i8)))
  (type $Gen_block (array (mut (ref eq))))


  (import "js_runtime" "print_string" (func $print_string (param (ref $String))))
  (import "js_runtime" "print_string_mem"
    (func $print_string_mem (param i32) (param i32)))
  (import "js_runtime" "print_endline" (func $print_endline))

  (import "js_runtime" "print_i32" (func $print_i32 (param i32)))
  (import "js_runtime" "print_f64" (func $print_f64 (param f64)))

  (import "js_runtime" "putchar" (func $putchar (param i32)))
  (import "js_runtime" "flush" (func $flush))

  (import "js_runtime" "memory" (memory $mem 1))

  (import "runtime" "string_eq"
    (func $string_eq (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))))

  (func (export "caml_int64_float_of_bits") (param $x (ref eq)) (result (ref $Float))
    (struct.new $Float
      (f64.reinterpret_i64
        (struct.get $Int64 0 (ref.cast $Int64 (local.get $x))))))

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
      (i31.new (local.get $oo_id))
  )

  ;; =====
  ;; Bytes
  ;; =====

  (func (export "caml_create_bytes") (param $size (ref eq)) (result (ref eq))
      (array.new_default $String (i31.get_s (ref.cast i31 (local.get $size))))
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

  ;; (func $string_compare (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
  ;;     ;; TODO
  ;;     (unreachable))

  ;; (export "caml_bytes_compare" (func $string_compare))
  ;; (export "caml_string_compare" (func $string_compare))

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

    ;; Comparison
    ;; ==========

  (func (export "caml_compare") (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable)
      )

  (func (export "caml_equal") (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
      ;; TODO
      (unreachable))
      ;; (block $true
      ;;   (block $false
      ;;     (if (ref.is_i31 (local.get $a))
      ;;         (then (unreachable))
      ;;         (else
      ;;           (if (ref.is_i31 (local.get $b))
      ;;           (then 
      ;;     )))))
      ;;   )
      ;;   (return (i31.new (i32.const 0))))
      ;;   (return (i31.new (i32.const 1))))

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
      (call $format_int_default (i31.get_s (ref.cast i31 (local.get $d)))))

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
      (call $flush)
      (i31.new (i32.const 0)))

  (func $cons (param $h (ref eq)) (param $t (ref eq)) (result (ref $Gen_block))
     (array.init_static $Gen_block
       (i31.new (i32.const 0))
       (local.get $h)
       (local.get $t)))

  (global $empty_list (ref eq) (i31.new (i32.const 0)))

  (func (export "caml_ml_out_channels_list") (param (ref eq))
                                 (result (ref eq))
     (call $cons (i31.new (i32.const 0)) (global.get $empty_list))
  )

   (func $caml_ml_output (export "caml_ml_output")
      (param $ch (ref eq)) (param $s (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $pos i32) (local $len i32)
      (local.set $pos (i31.get_s (ref.cast i31 (local.get $vpos))))
      (local.set $len (i31.get_s (ref.cast i31 (local.get $vlen))))
      (loop $loop
         (if (i32.gt_s (local.get $len) (i32.const 0))
            (then
               (call $putchar
                  (array.get $String
                    (ref.cast $String (local.get $s))
                    (local.get $pos)))
               (local.set $pos (i32.add (local.get $pos) (i32.const 1)))
               (local.set $len (i32.sub (local.get $len) (i32.const 1)))
               (br $loop))))
      (i31.new (i32.const 0)))

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



  ;; ====
  ;; MISC
  ;; ====

  (func (export "print_int") (param $a (ref eq)) (result (ref eq))
    (call $print_i32 (i31.get_s (ref.cast i31 (local.get $a))))
    (i31.new (i32.const 0))
  )
  (func (export "print_float") (param $a (ref eq)) (result (ref eq))
    (call $print_f64 (struct.get $Float 0 (ref.cast $Float (local.get $a))))
    (i31.new (i32.const 0))
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
      (call $copy_string (ref.cast $String (local.get $a))))
    (i31.new (i32.const 0)))

  (func (export "print_endline") (param $a (ref eq)) (result (ref eq))
    (call $print_endline)
    (i31.new (i32.const 0)))
)

(register "imports")
