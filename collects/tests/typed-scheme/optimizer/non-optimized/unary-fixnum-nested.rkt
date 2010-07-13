(module unary-fixnum-nested typed/scheme 
  (require racket/unsafe/ops racket/fixnum)
  (abs (bitwise-not (length '(1 2 3)))))
