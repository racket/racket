#lang racket/kernel

(#%declare #:require=define)

(#%declare #:cross-phase-persistent)

(#%require "stx.rkt")

(#%provide member
           memw
           (rename reverse alt-reverse)
           car+cdr)


; Various functions which are useful in upper layers,
; and relatively easy to write in kernel

(define-values (bad-list)
  (λ (who orig-l)
    (raise-arguments-error who "not a proper list"
                           "in" orig-l)))

(define-values (member)
  (letrec-values ([(member)
                   (lambda (v orig-l eql?)
                     (define-values (loop)
                       (lambda (ls turtle)
                       (if (null? ls)
                           #f
                           (if (not (pair? ls))
                               (bad-list 'member orig-l)
                               (if (eql? v (car ls))
                                   ls
                                   (let-values ([(ls) (cdr ls)])
                                     (if (null? ls)
                                         #f
                                         (if (not (pair? ls))
                                             (bad-list 'member orig-l)
                                             (if (eql? v (car ls))
                                                 ls
                                                 (if (eq? ls turtle)
                                                     (bad-list 'member orig-l)
                                                     (loop (cdr ls) (cdr turtle))))))))))))
                     (loop orig-l orig-l))])
    (case-lambda
      [(v ls)
       (member v ls equal?)]
      [(v ls eql?)
       (if (if (procedure? eql?)
               (procedure-arity-includes? eql? 2)
               #f)
           (void)
           (raise-argument-error 'member "(procedure-arity-includes/c 2)" eql?))
       (member v ls eql?)])))

(define-values (memw)
  (lambda (v ls)
    (member v ls equal-always?)))


; define it as `reverse` so it has the right procedure name
(define-values (reverse)
    (lambda (l)
      (if (variable-reference-from-unsafe? (#%variable-reference))
          (void)
          (if (list? l)
              (void)
              (raise-argument-error 'reverse "list?" l)))
      (letrec-values ([(loop)
                       (lambda (a l)
                         (if (null? l)
                             a
                             (loop (cons (car l) a) (cdr l))))])
                     (loop null l))))


; (car+cdr (cons a b)) -> (values a b)
(define-values (car+cdr)
  (lambda (p)
    (if (pair? p)
        (values (car p) (cdr p))
        (raise-argument-error 'car+cdr "pair?" p))))


