(module inexact-complex-parts typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-flreal-part 1.0+2.0i)
  (unsafe-flimag-part 1+2.0i)
  (unsafe-flreal-part 1.0+2i))
