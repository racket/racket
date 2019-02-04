#lang racket/base

;; A built-in symbol is one that the compiler must avoid using for a
;; binding. Built-in symbols include the names of run-time primitives
;; and identifiers reserved by the compiler itself (see
;; "reserved-symbol.rkt")

(provide register-built-in-symbol!
         built-in-symbol?
         make-built-in-symbol!)

(define built-in-symbols (make-hasheq))

(define (register-built-in-symbol! s)
  (hash-set! built-in-symbols s #t))

(define (built-in-symbol? s)
  (hash-ref built-in-symbols s #f))

(define (make-built-in-symbol! s)
  ;; Make a symbol that is a little more obscure than just `s`
  (define built-in-s (string->symbol (format ".~s" s)))
  (register-built-in-symbol! built-in-s)
  built-in-s)

;; ----------------------------------------

(void
 (begin
   ;; Primitive expression forms
   (for-each register-built-in-symbol!
             '(lambda case-lambda
                if begin begin0
                let-values letrec-values
                set! quote
                with-continuation-mark
                #%variable-reference))

   ;; Source-mode linklet glue
   (for-each register-built-in-symbol!
             '(check-not-undefined 
               instance-variable-box
               variable-reference
               variable-reference?
               variable-reference->instance
               variable-reference-constant?
               variable-reference-from-unsafe?))

   ;; Linklet compilation on Chez Scheme 
   (for-each register-built-in-symbol!
             '(or
               and
               let
               letrec*
               define
               pariah
               variable-set!
               variable-ref
               variable-ref/no-check
               make-instance-variable-reference
               annotation?
               annotation-expression
               #%app
               #%call-with-values
               make-pthread-parameter
               break-enabled-key
               fasl->s-exp/intern))))
