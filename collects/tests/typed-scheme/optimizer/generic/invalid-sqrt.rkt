(module invalid-sqrt typed/scheme #:optimize
  (sqrt -2.0)) ; not a nonnegative flonum, can't optimize
