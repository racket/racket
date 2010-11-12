#lang scheme/base
(require scheme/class
         "syntax.rkt")

(provide point% point-x point-y
         list-of-pair-of-real?)

(define point%
  (class object%
    (init-field [x 0.0]
                [y 0.0])
    (define/public (get-x) x)
    (define/public (get-y) y)
    (def/public (set-x [real? v]) (set! x (exact->inexact v)))
    (def/public (set-y [real? v]) (set! y (exact->inexact v)))
    (super-new)))

(define point-x (class-field-accessor point% x))
(define point-y (class-field-accessor point% y))

(define (list-of-pair-of-real? p)
  (and (list? p)
       (andmap (lambda (p) (and (pair? p)
                                (real? (car p))
                                (real? (cdr p))))
               p)))

