#lang racket/base

;; This module provides helpers for syntax lifting

(require syntax/parse
         (for-template racket/base))

(provide local-expand/capture*)

;; like `local-expand/capture-lifts` but expands the lifted expression, which
;; allows us to type-check lifted expressions at the top-level
(define (local-expand/capture* stx ctx stop-ids)
  (define-values (defs expr)
    ;; at each iteration, get lifted definitions and the expanded expression
    (let loop ([stx stx])
      (define stx* (local-expand/capture-lifts stx ctx stop-ids))
      (syntax-parse stx*
        #:literals (begin define-values)
        [(begin (define-values (n) e) ... e*)
         (define-values (sub-defss defs)
           (for/lists (_1 _2) ([e (in-list (syntax->list #'(e ...)))]
                               [n (in-list (syntax->list #'(n ...)))])
             ;; lifted expressions may re-lift, so recur
             (define-values (sub-defs e-expanded) (loop e))
             (values sub-defs #`(define-values (#,n) #,e-expanded))))
         (values (append (apply append sub-defss) defs) #'e*)])))
  #`(begin #,@defs #,expr))
