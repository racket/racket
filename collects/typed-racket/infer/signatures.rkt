#lang racket/base
(require "../utils/utils.rkt"
         racket/unit (contract-req)
         (infer constraint-structs)
         (utils unit-utils)
         (rep type-rep))
(provide (all-defined-out))

(define-signature dmap^
  ([cond-contracted dmap-meet (dmap? dmap? . -> . dmap?)]))

(define-signature promote-demote^
  ([cond-contracted var-promote (Type/c (listof symbol?) . -> . Type/c)]
   [cond-contracted var-demote  (Type/c (listof symbol?) . -> . Type/c)]))

(define-signature constraints^
  ([cond-contracted exn:infer? (any/c . -> . boolean?)]
   [cond-contracted fail-sym symbol?]
   ;; inference failure - masked before it gets to the user program
   (define-syntaxes (fail!)
     (syntax-rules ()
       [(_ s t) (raise (list fail-sym s t))]))
   [cond-contracted cset-meet (cset? cset? . -> . cset?)]
   [cond-contracted cset-meet* ((listof cset?) . -> . cset?)]
   no-constraint
   [cond-contracted empty-cset ((listof symbol?) (listof symbol?) . -> . cset?)]
   [cond-contracted insert (cset? symbol? Type/c Type/c . -> . cset?)]
   [cond-contracted cset-combine ((listof cset?) . -> . cset?)]
   [cond-contracted c-meet ((c? c?) (symbol?) . ->* . c?)]))

(define-signature restrict^
  ([cond-contracted restrict ((Type/c Type/c) ((or/c 'new 'orig)) . ->* . Type/c)]))

(define-signature infer^
  ([cond-contracted infer ((;; variables from the forall
                            (listof symbol?)
                            ;; indexes from the forall
                            (listof symbol?)
                            ;; actual argument types from call site
                            (listof Type/c)
                            ;; domain
                            (listof Type/c)
                            ;; range
                            (or/c #f Values/c ValuesDots?))
                           ;; optional expected type
                           ((or/c #f Values/c AnyValues? ValuesDots?))
                           . ->* . any)]
   [cond-contracted infer/vararg ((;; variables from the forall
                                   (listof symbol?)
                                   ;; indexes from the forall
                                   (listof symbol?)
                                   ;; actual argument types from call site
                                   (listof Type/c)
                                   ;; domain
                                   (listof Type/c)
                                   ;; rest
                                   (or/c #f Type/c)
                                   ;; range
                                   (or/c #f Values/c ValuesDots?))
                                  ;; [optional] expected type
                                  ((or/c #f Values/c AnyValues? ValuesDots?)) . ->* . any)]
   [cond-contracted infer/dots (((listof symbol?)
                                 symbol?
                                 (listof Values/c)
                                 (listof Values/c)
                                 Values/c
                                 (or/c Values/c ValuesDots?)
                                 (listof symbol?))
                                (#:expected (or/c #f Values/c AnyValues? ValuesDots?)) . ->* . any)]))
