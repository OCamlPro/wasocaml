(module
  (type $Float (struct (field (mut f64))))

  (import "spectest" "print_i32" (func $print_i32 (param i32)))
  (import "spectest" "print_f64" (func $print_f64 (param f64)))
  (func (export "saucisse") (param $a (ref any)) (result (ref any))
    (call $print_i32 (i31.get_s (ref.cast i31 (local.get $a))))
    (i31.new (i32.const 0))
  )
  (func (export "morteau") (param $a (ref any)) (result (ref any))
    (call $print_f64 (struct.get $Float 0 (ref.cast $Float (local.get $a))))
    (i31.new (i32.const 0))
  )
  (func (export "montbeliard") (param $a f64) (result (ref any))
    (call $print_f64 (local.get $a))
    (i31.new (i32.const 0))
  )
)

(register "import")
