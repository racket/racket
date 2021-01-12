#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "infer-known.rkt"
         "mutated-state.rkt"
         "aim.rkt")

(provide letrec-splitable-values-binding?
         letrec-split-values-binding
         letrec-conversion)

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

(define (letrec-conversion ids mutated target e)
  (define need-convert?
    (and (not (aim? target 'cify))
         (let loop ([ids ids])
           (cond
             [(symbol? ids)
              (needs-letrec-convert-mutated-state? (hash-ref mutated ids #f))]
             [(wrap? ids) (loop (unwrap ids))]
             [(pair? ids) (or (loop (car ids))
                              (loop (cdr ids)))]
             [else #f]))))
  (if need-convert?
      (match e
        [`(,_ ([,ids ,rhss] ...) . ,body)
         `(let ,(for/list ([id (in-list ids)])
                  `[,id unsafe-undefined])
            ,@(for/list ([id (in-list ids)]
                         [rhs (in-list rhss)])
                `(set! ,id ,rhs))
            . ,body)])
      e))
