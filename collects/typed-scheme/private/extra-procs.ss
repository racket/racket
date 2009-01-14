#lang scheme/base
(provide assert call-with-values* values* foo)

(define (assert v)
  (unless v
    (error "Assertion failed - value was #f"))
  v)

(define (fold-right f c as . bss)
  (if (or (null? as)
          (ormap null? bss))
      c
      (apply f
             (apply fold-right f c (cdr as) (map cdr bss))
             (car as) (map car bss))))

(define call-with-values* call-with-values)
(define values* values)

(define (foo x #:bar [bar #f])
  bar)
