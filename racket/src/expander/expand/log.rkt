#lang racket/base
(require "context.rkt")

(provide log-expand
         log-expand*
         log-expand...
         ...log-expand)

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
  (cond
   [(hash-ref key->arity key #f)
    => (lambda (arity)
         (unless (or (eq? arity 'any) (eqv? (length args) arity))
           (error 'call-expand-observe "wrong arity for ~s: ~e" key args)))]
   [else (error 'call-expand-observe "bad key: ~s" key)])
  (obs key (cond
            [(null? args) #f]
            [else (apply list* args)])))

(define key->arity
  ;; event-symbol => (U Nat 'any)
  #hash(;; basic empty tokens
        (start         . 0)
        (start-top     . 0)
        (next          . 0)
        (next-group    . 0)
        (phase-up      . 0)
        (enter-bind    . 0)
        (exit-bind     . 0)
        (exit-local-bind . 0)
        (prepare-env   . 0)

        ;; basic tokens
        (visit         . 1)
        (resolve       . 1)
        (enter-macro   . 1)
        (macro-pre-x   . 1)
        (macro-post-x  . 2)
        (exit-macro    . 1)
        (enter-prim    . 1)
        (exit-prim     . 1)
        (return        . 1)
        (enter-block   . 1)
        (block->list   . 1)
        (block->letrec . 1)
        (splice        . 1)
        (enter-list    . 1)
        (exit-list     . 1)
        (enter-check   . 1)
        (exit-check    . 1)
        (module-body   . 1)
        (lift-loop     . 1)
        (letlift-loop  . 1)
        (module-lift-loop . 1)
        (module-lift-end-loop . 1)
        (lift-expr     . 2)
        (lift-statement . 1)
        (lift-require  . 3)
        (lift-provide  . 1)
        (enter-local   . 1)
        (local-pre     . 1)
        (local-post    . 1)
        (exit-local    . 1)
        (local-bind    . 1)
        (opaque-expr   . 1)
        (variable      . 2)
        (tag           . 1)
        (rename-one    . 1)
        (rename-list   . 1)
        (track-origin  . 2)
        (local-value   . 1)
        (local-value-result . 1)

        ;; renames tokens **
        (lambda-renames . 2)
        (let-renames    . any)  ;; renames consed by expander... sometimes
        (letrec-syntaxes-renames . any)  ;; renames consed by expander... sometimes
        (block-renames  . 2)

        ;; prim tokens
        (prim-stop          . 0)
        (prim-module        . 0)
        (prim-module-begin  . 0)
        (prim-define-syntaxes . 0)
        (prim-define-values . 0)
        (prim-if            . 0)
        (prim-with-continuation-mark . 0)
        (prim-begin         . 0)
        (prim-begin0        . 0)
        (prim-#%app         . 0)
        (prim-lambda        . 0)
        (prim-case-lambda   . 0)
        (prim-let-values    . 0)
        (prim-letrec-values . 0)
        (prim-letrec-syntaxes+values . 0)
        (prim-#%datum       . 0)
        (prim-#%top         . 0)
        (prim-quote         . 0)
        (prim-quote-syntax  . 0)
        (prim-require       . 0)
        (prim-provide       . 0)
        (prim-set!          . 0)
        (prim-#%expression  . 0)
        (prim-#%variable-reference . 0)
        (prim-#%stratified  . 0)
        (prim-begin-for-syntax . 0)
        (prim-submodule     . 0)
        (prim-submodule*    . 0)))
