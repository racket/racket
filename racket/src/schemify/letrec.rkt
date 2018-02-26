#lang racket/base
(require "wrap.rkt"
         "infer-known.rkt")

(provide letrec-splitable-values-binding?
         letrec-split-values-binding)

;; Detect binding of lambdas that were probably generated from an
;; R[56]RS program

(define (letrec-splitable-values-binding? idss rhss)
  (and (pair? idss)
       (null? (cdr idss))
       (wrap-pair? (car rhss))
       (eq? 'values (wrap-car (car rhss)))
       (= (length (wrap-cdr (car rhss)))
          (length (car idss)))
       (for/and ([rhs (in-list (wrap-cdr (car rhss)))])
         (lambda? rhs #:simple? #t))))

(define (letrec-split-values-binding idss rhss bodys)
  `(letrec-values ,(for/list ([id (in-list (car idss))]
                              [rhs (in-list (wrap-cdr (car rhss)))])
                     `[(,id) ,rhs])
     . ,bodys))

