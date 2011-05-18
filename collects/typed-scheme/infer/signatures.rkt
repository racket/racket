#lang racket/base
(require racket/unit racket/contract racket/require
         "constraint-structs.rkt"
         (path-up "utils/utils.rkt" "utils/unit-utils.rkt" "rep/type-rep.rkt"))
(provide (all-defined-out))

(define-signature dmap^
  ([cond-contracted dmap-meet (dmap? dmap? . -> . dmap?)]))

(define-signature promote-demote^
  ([cond-contracted var-promote (Type? (listof symbol?) . -> . Type?)]
   [cond-contracted var-demote  (Type? (listof symbol?) . -> . Type?)]))

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
   [cond-contracted insert (cset? symbol? Type? Type? . -> . cset?)]
   [cond-contracted cset-combine ((listof cset?) . -> . cset?)]
   [cond-contracted c-meet ((c? c?) (symbol?) . ->* . c?)]))

(define-signature restrict^
  ([cond-contracted restrict (Type? Type? . -> . Type?)]))

(define-signature infer^
  ([cond-contracted infer ((;; variables from the forall
                            (listof symbol?)
                            ;; indexes from the forall
                            (listof symbol?)
                            ;; actual argument types from call site
                            (listof Type?)
                            ;; domain
                            (listof Type?)
                            ;; range
                            (or/c #f Type?))
                           ;; optional expected type
                           ((or/c #f Type?))
                           . ->* . any)]
   [cond-contracted infer/vararg ((;; variables from the forall
                                   (listof symbol?)
                                   ;; indexes from the forall
                                   (listof symbol?)
                                   ;; actual argument types from call site
                                   (listof Type?)
                                   ;; domain
                                   (listof Type?)
                                   ;; rest
                                   (or/c #f Type?)
                                   ;; range
                                   (or/c #f Type?))
                                  ;; [optional] expected type
                                  ((or/c #f Type?)) . ->* . any)]
   [cond-contracted infer/dots (((listof symbol?)
                                 symbol?
                                 (listof Type?) (listof Type?) Type? Type? (listof symbol?))
                                (#:expected (or/c #f Type?)) . ->* . any)]))
