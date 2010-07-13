(module fixnum-comparison typed/scheme 
  (require racket/unsafe/ops)
  (< (vector-length '#(1 2 3)) (string-length "asdf")))
