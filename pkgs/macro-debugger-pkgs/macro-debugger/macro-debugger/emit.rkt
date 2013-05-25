#lang racket/base
(require racket/contract/base)

(define current-expand-observe
  (dynamic-require ''#%expobs 'current-expand-observe))

(define (emit-remark #:unmark? [unmark? (syntax-transforming?)] . args)
  (let ([observe (current-expand-observe)])
    (when observe
      (let ([args (flatten-emit-args args unmark?)])
        (observe 'local-remark args)))))

(define (emit-local-step before after #:id id)
  (let ([observe (current-expand-observe)])
    (when observe
      (observe 'local-artificial-step
               (list (list id)
                     before (syntax-local-introduce before)
                     (syntax-local-introduce after) after)))))

(define emit-arg/c
  (recursive-contract
   (or/c string?
         syntax?
         (listof emit-arg/c)
         (-> emit-arg/c))))

(define (flatten-emit-args x unmark?)
  (define (loop x onto)
    (cond [(string? x)
           (cons x onto)]
          [(syntax? x)
           (cons (if unmark? (syntax-local-introduce x) x) onto)]
          [(list? x)
           (foldr loop onto x)]
          [(procedure? x)
           (loop (x) onto)]))
  (loop x null))

(provide/contract
 [emit-remark
  (->* () (#:unmark? any/c) #:rest (listof emit-arg/c)
       any)]
 [emit-local-step
  (-> syntax? syntax? #:id identifier? any)])
