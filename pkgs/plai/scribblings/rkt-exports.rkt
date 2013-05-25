#lang racket/base
(require (for-label racket)
         (for-syntax racket/base)
         scribble/manual)
(define-syntax-rule (is x ...)  (begin (i x) ...))
(define-syntax (i stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax ([ext-id (string->symbol (format "rkt:~a" (symbol->string (syntax-e #'id))))])
       #`(begin (provide ext-id)
                (define ext-id (racket id))))]))

(is add1 sub1 zero? + - * / even? odd? = < > <= >= 
    symbol? symbol=? number? boolean? empty? eq?)
(is if and or cond case define-values let let-values let* set! quote error begin)
