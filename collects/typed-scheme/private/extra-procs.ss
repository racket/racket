#lang scheme/base
(provide assert)

(define (assert v)
  (unless v
    (error "Assertion failed - value was #f"))
  v)
#;
(define (fold-right f c as . bss)
  (if (or (null? as)
          (ormap null? bss))
      c
      (apply f
             (apply fold-right f c (cdr as) (map cdr bss))
             (car as) (map car bss))))


