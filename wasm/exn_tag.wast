(module
  (tag $exc (param (ref eq)))
  (export "exc" (tag $exc))
)
(register "exn_tag")

(module
  (import "exc_tag" "exc" $exc (tag (param (ref eq))))
)