#lang scheme/base
(require scheme/class)

(provide point% point-x point-y
         list-of-pair-of-real?)

(define point%
  (class object%
    (init-field [x 0.0]
                [y 0.0])
    (super-new)))

(define point-x (class-field-accessor point% x))
(define point-y (class-field-accessor point% y))

(define (list-of-pair-of-real? p)
  (and (list? p)
       (andmap (lambda (p) (and (pair? p)
                                (real? (car p))
                                (real? (cdr p))))
               p)))

