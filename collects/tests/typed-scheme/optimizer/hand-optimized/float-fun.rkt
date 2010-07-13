(module float-fun typed/scheme #:optimize
  (require racket/unsafe/ops)
  (: f (Float -> Float))
  (define (f x)
    (unsafe-fl+ x 1.0)))
