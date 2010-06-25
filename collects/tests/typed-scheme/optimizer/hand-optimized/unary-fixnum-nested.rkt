(module unary-fixnum-nested typed/scheme #:optimize
  (require racket/unsafe/ops racket/fixnum)
  (unsafe-fxabs (unsafe-fxnot (length '(1 2 3)))))
