(module exact-inexact typed/scheme #:optimize
  (exact->inexact 1.0)) ; not an integer, can't optimize
