#lang racket/base
(provide flatten-begin)

(define (flatten-begin stx)
  (let ([l (syntax->list stx)])
    (if l
        (map (lambda (e)
               (syntax-track-origin e stx (car l)))
             (cdr l))
        (raise-syntax-error
         #f
         "bad syntax"
         stx))))
