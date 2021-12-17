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

;; Expansion logging is interpreted by the macro stepper: see
;;
;;   (lib macro-debugger/model/deriv-{tokens,parser})
;;
;; In particular, deriv-tokens.rkt describes the payloads carried by each of the
;; events listed below, and deriv-parser.rkt describes the grammar of events and
;; how it corresponds to the procedures in the expander implementation.

;; Here are a few non-obvious considerations for the logging design:
;;
;; - 'prim-X events should occur before error checking (including define-match)
;; - payloads should contain no artificial syntax objects (that is, they should
;;   only contain syntax objects from the input or that will be the basis for
;;   results (possibly adjusted by scopes, etc))

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
        (enter-begin-for-syntax . 0)
        (exit-begin-for-syntax . 0)

        ;; basic tokens
        (visit         . 1)
        (resolve       . 1)
        (enter-macro   . 2)
        (macro-pre-x   . 1)
        (macro-post-x  . 2)
        (exit-macro    . 2)
        (enter-prim    . 1)
        (exit-prim     . 1)
        (return        . 1)
        (stop/return   . 1)
        (exit-prim/return . 1)

        (enter-block   . 1)
        (block->list   . 0)
        (block->letrec . 3)
        (finish-block  . 1)
        (splice        . 1)
        (enter-list    . 1)
        (exit-list     . 1)
        (module-body   . 1)
        (lift-loop     . 1)
        (letlift-loop  . 1)
        (module-lift-loop . 1)
        (module-lift-end-loop . 1)
        (lift-expr     . 3)
        (lift-end-decl . 3)
        (lift-require  . 3)
        (lift-provide  . 1)
        (lift-module   . 2)
        (enter-local   . 1)
        (local-pre     . 1)
        (local-post    . 1)
        (exit-local    . 1)
        (local-bind    . 1)
        (opaque-expr   . 1)
        (variable      . 2)
        (tag           . 1)
        (tag2          . 2)
        (tag/context   . 1)
        (rename-one    . 1)
        (rename-list   . 1)
        (track-syntax  . 3)
        (local-value   . 1)
        (local-value-result . 1)
        (rename-transformer . 1)
        (module-end-lifts . 1)
        (module-pass1-lifts . 3)
        (module-pass2-lifts . 3)
        (module-pass1-case . 1)
        (exit-case . 1)

        ;; renames tokens **
        (lambda-renames . 2)
        (letX-renames   . 5)
        (block-renames  . 2)

        ;; prim tokens
        (prim-stop          . 1)
        (prim-module        . 1)
        (prim-module-begin  . 1)
        (prim-define-syntaxes . 1)
        (prim-define-values . 1)
        (prim-if            . 1)
        (prim-with-continuation-mark . 1)
        (prim-begin         . 1)
        (prim-begin0        . 1)
        (prim-#%app         . 1)
        (prim-lambda        . 1)
        (prim-case-lambda   . 1)
        (prim-let-values    . 1)
        (prim-letrec-values . 1)
        (prim-letrec-syntaxes+values . 1)
        (prim-#%datum       . 1)
        (prim-#%top         . 1)
        (prim-quote         . 1)
        (prim-quote-syntax  . 1)
        (prim-require       . 1)
        (prim-provide       . 1)
        (prim-set!          . 1)
        (prim-#%expression  . 1)
        (prim-#%variable-reference . 1)
        (prim-#%stratified  . 1)
        (prim-begin-for-syntax . 1)
        (prim-declare       . 1)
        (prim-submodule     . 1)
        (prim-submodule*    . 1)))
