#lang racket/base
(require "context.rkt")

(provide log-expand
         log-expand*
         log-expand...
         ...log-expand
         log-expand-start-top)

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
  (cond [(hash-ref key->number+arity key)
         => (lambda (number+arity)
              (let ([arity (cdr number+arity)])
                (unless (or (not arity) (= (length args) arity))
                  (error 'call-expand-observe "wrong arity for ~s: ~e" key args)))
              (obs (car number+arity)
                   (cond [(null? args) #f]
                         [else (apply list* args)])))]
        [else (error 'call-expand-observe "bad key: ~s" key)]))

(define (log-expand-start-top)
  (define obs (current-expand-observe))
  (when obs
    (call-expand-observe obs 'start-top)))

;; For historical reasons, an expander observer currently expects
;; numbers
(define key->number+arity
  ;; event-symbol => (event-num . #args)
  #hash((visit         . (0 . 1))
        (resolve       . (1 . 1))
        (return        . (2 . 1))
        (next          . (3 . 0))
        (enter-list    . (4 . 1))
        (exit-list     . (5 . 1))
        (enter-prim    . (6 . 1))
        (exit-prim     . (7 . 1))
        (enter-macro   . (8 . 1))
        (exit-macro    . (9 . 1))
        (enter-block   . (10 . 1))
        (splice        . (11 . 1))
        (block->list   . (12 . 1))
        (next-group    . (13 . 0))
        (block->letrec . (14 . 1))
        (let-renames        . (16 . #f))  ;; renames consed by expander... sometimes
        (lambda-renames     . (17 . 2))
        (case-lambda-renames . (18 . 2))
        (letrec-syntaxes-renames . (19 . #f)) ;; renames consed by expander... sometimes
        (phase-up           . (20 . 0))

        (macro-pre-x        . (21 . 1))
        (macro-post-x       . (22 . 2))

        (module-body        . (23 . 1))
        (block-renames      . (24 . 2))

        (prim-stop          . (100 . 0))
        (prim-module        . (101 . 0))
        (prim-module-begin  . (102 . 0))
        (prim-define-syntaxes  . (103 . 0))
        (prim-define-values . (104 . 0))
        (prim-if            . (105 . 0))
        (prim-with-continuation-mark . (106 . 0))
        (prim-begin         . (107 . 0))
        (prim-begin0        . (108 . 0))
        (prim-#%app         . (109 . 0))
        (prim-lambda        . (110 . 0))
        (prim-case-lambda   . (111 . 0))
        (prim-let-values    . (112 . 0))
        (prim-letrec-values . (113 . 0))
        (prim-letrec-syntaxes+values . (114 . 0))
        (prim-#%datum         . (115 . 0))
        (prim-#%top         . (116 . 0))
        (prim-quote         . (117 . 0))
        (prim-quote-syntax  . (118 . 0))
        (prim-require       . (119 . 0))
        (prim-provide       . (122 . 0))

        (prim-set!          . (123 . 0))
        (prim-#%expression    . (138 . 0))
        (prim-#%variable-reference . (149 . 0))

        (prim-#%stratified  . (155 . 0))

        (prim-begin-for-syntax . (156 . 0))

        (prim-submodule     . (158 . 0))
        (prim-submodule*    . (159 . 0))

        (variable           . (125 . 2))

        (enter-check        . (126 . 1))
        (exit-check         . (127 . 1))

        (lift-loop          . (128 . 1))
        (letlift-loop       . (136 . 1))
        (module-lift-loop   . (137 . 1))
        (module-lift-end-loop . (135 . 1))

        (local-lift         . (129 . 2))
        (lift-statement     . (134 . 1))
        (lift-require       . (150 . 3))
        (lift-provide       . (151 . 1))

        (enter-local        . (130 . 1))
        (exit-local         . (131 . 1))
        (local-pre          . (132 . 1))
        (local-post         . (133 . 1))

        (enter-local-expr   . (139 . 1))
        (exit-local-expr    . (140 . 2))

        (start-expand       . (141 . 0))

        (tag                . (142 . 1))

        (local-bind         . (143 . 1))
        (exit-local-bind    . (160 . 0))
        (enter-bind         . (144 . 0))
        (exit-bind          . (145 . 0))

        (opaque-expr        . (146 . 1))

        (rename-list        . (147 . 1))

        (rename-one         . (148 . 1))

        (track-origin       . (152 . 2))

        (local-value        . (153 . 1))

        (local-value-result . (154 . 1))

        (prepare-env        . (157 . 0))

        (start-top          . (201 . 0))))
