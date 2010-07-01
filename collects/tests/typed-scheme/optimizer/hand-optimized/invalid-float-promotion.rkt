(module float-promotion typed/scheme
  (/ 1 2.0)) ; result is not a float, can't optimize
