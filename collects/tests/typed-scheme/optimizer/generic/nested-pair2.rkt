(module nested-pair2 typed/scheme #:optimize
  (require racket/unsafe/ops)
  (car (cdr (cons 3 (cons (cons 2 '()) 1)))))
