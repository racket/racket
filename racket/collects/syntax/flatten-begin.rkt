#lang racket/base
(provide flatten-begin
         flatten-all-begins)

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

;; flatten-all-begins : Syntax -> (Listof Syntax)
;; Flatten `begin` expressions recursively
(define (flatten-all-begins orig-stx)
  (define val (syntax-e orig-stx))
  (unless (and (pair? val)
               (not (null? val))
               (identifier? (car val))
               (free-identifier=? (car val) #'begin #f #f))
    (raise-syntax-error
     #f
     "not a begin expression"
     orig-stx))
  (let loop ([stx orig-stx])
    (define lst (syntax->list stx))
    (if (and lst
             (not (null? lst))
             (free-identifier=? (car lst) #'begin #f #f))
        (apply append (map loop (cdr lst)))
        (list (syntax-track-origin stx orig-stx #'begin)))))
