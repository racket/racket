(module invalid-sqrt typed/scheme 
  (sqrt -2.0)) ; not a nonnegative flonum, can't optimize
