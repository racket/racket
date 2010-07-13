(module fixnum-comparison typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-fx< (vector-length '#(1 2 3)) (string-length "asdf")))
