(module nested-pair2 typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-car (unsafe-cdr (cons 3 (cons (cons 2 '()) 1)))))
