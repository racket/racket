#lang racket/base
(require "context.rkt")

(provide log-expand
         log-expand*
         log-expand...
         ...log-expand
         log-expand-start)

(define-syntax log-expand...
  (syntax-rules (lambda)
    [(_ ctx (lambda (obs) body ...))
     (let ([obs (expand-context-observer ctx)])
       (when obs
         body ...))]))

(define-syntax-rule (...log-expand obs [key arg ...] ...)
  (begin
    (call-expand-observe obs key arg ...)
    ...))

(define-syntax log-expand*
  (syntax-rules ()
    [(_ ctx #:when guard [key arg ...] ...)
     (log-expand... ctx
                    (lambda (obs)
                      (when guard
                        (...log-expand obs [key arg ...] ...))))]
    [(_ ctx #:unless guard [key arg ...] ...)
     (log-expand* ctx #:when (not guard) [key arg ...] ...)]
    [(_ ctx [key arg ...] ...)
     (log-expand* ctx #:when #t [key arg ...] ...)]))

(define-syntax-rule (log-expand ctx key arg ...)
  (log-expand* ctx #:when #t [key arg ...]))

(define (call-expand-observe obs key . args)
  (obs (hash-ref key->number key) (cond
                                   [(null? args) #f]
                                   [else (apply list* args)])))

(define (log-expand-start)
  (define obs (current-expand-observe))
  (when obs
    (call-expand-observe obs 'start-expand)))

;; For historical reasons, an expander observer currently expects
;; numbers
(define key->number
  #hash((visit         . 0)
        (resolve       . 1)
        (return        . 2)
        (next          . 3)
        (enter-list    . 4)
        (exit-list     . 5)
        (enter-prim    . 6)
        (exit-prim     . 7)
        (enter-macro   . 8)
        (exit-macro    . 9)
        (enter-block   . 10)
        (splice        . 11)
        (block->list   . 12)
        (next-group    . 13)
        (block->letrec . 14)
        (let-renames        . 16)
        (lambda-renames     . 17)
        (case-lambda-renames . 18)
        (letrec-syntaxes-renames . 19)
        (phase-up           . 20)

        (macro-pre-x        . 21)
        (macro-post-x       . 22)

        (module-body        . 23)
        (block-renames      . 24)

        (prim-stop          . 100)
        (prim-module        . 101)
        (prim-module-begin  . 102)
        (prim-define-syntaxes  . 103)
        (prim-define-values . 104)
        (prim-if            . 105)
        (prim-with-continuation-mark . 106)
        (prim-begin         . 107)
        (prim-begin0        . 108)
        (prim-#%app         . 109)
        (prim-lambda        . 110)
        (prim-case-lambda   . 111)
        (prim-let-values    . 112)
        (prim-letrec-values . 113)
        (prim-letrec-syntaxes+values . 114)
        (prim-#%datum         . 115)
        (prim-#%top         . 116)
        (prim-quote         . 117)
        (prim-quote-syntax  . 118)
        (prim-require       . 119)
        (prim-provide       . 122)

        (prim-set!          . 123)
        (prim-#%expression  . 138)
        (prim-#%variable-reference . 149)

        (prim-#%stratified  . 155)

        (prim-begin-for-syntax . 156)

        (prim-submodule     . 158)
        (prim-submodule*    . 159)

        (variable           . 125)

        (enter-check        . 126)
        (exit-check         . 127)

        (lift-loop          . 128)
        (letlift-loop       . 136)
        (module-lift-loop   . 137)
        (module-lift-end-loop . 135)

        (local-lift         . 129)
        (lift-statement     . 134)
        (lift-require       . 150)
        (lift-provide       . 151)

        (enter-local        . 130)
        (exit-local         . 131)
        (local-pre          . 132)
        (local-post         . 133)

        (enter-local-expr   . 139)
        (exit-local-expr    . 140)

        (start-expand       . 141)

        (tag                . 142)

        (local-bind         . 143)
        (exit-local-bind    . 160)
        (enter-bind         . 144)
        (exit-bind          . 145)

        (opaque-expr        . 146)

        (rename-list        . 147)

        (rename-one         . 148)

        (track-origin       . 152)

        (local-value        . 153)

        (local-value-result . 154)

        (prepare-env        . 157)))
