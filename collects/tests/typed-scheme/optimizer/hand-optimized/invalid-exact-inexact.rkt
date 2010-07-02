(module exact-inexact typed/scheme #:optimize
  (exact->inexact 1.0)) ; not a integer, can't optimize
