#lang racket/base

(require (for-template racket/base))

(provide flatten-begin
         flatten-all-begins)

(define (flatten-begin stx)
  (syntax-case stx ()
    [(beg l ...)
     (identifier? #'beg)
     (map (lambda (e) (syntax-track-origin e stx #'beg))
          (syntax->list #'(l ...)))]))

;; start-with-begin? : (U (Listof Syntax) #f) -> Bool
(define (start-with-begin? lst)
  (and lst
       (not (null? lst))
       (identifier? (car lst))
       (free-identifier=? (car lst) #'begin)))

;; flatten-all-begins : Syntax -> (Listof Syntax)
;; Flatten `begin` expressions recursively
(define (flatten-all-begins orig-stx)
  (define lst (syntax->list orig-stx))
  (unless (start-with-begin? lst)
    (raise-syntax-error
     #f
     "not a begin expression"
     orig-stx))
  (let loop ([stx orig-stx])
    (define lst (syntax->list stx))
    (if (start-with-begin? lst)
        (apply append (map loop (cdr lst)))
        (list (syntax-track-origin stx orig-stx #'begin)))))
