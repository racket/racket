#lang racket/base
(require "../utils/utils.rkt"
         racket/unit (contract-req)
         (infer constraint-structs)
         (utils unit-utils)
         (rep type-rep))
(provide (all-defined-out))

(define-signature dmap^
  ([cond-contracted dcon-meet (dcon/c dcon/c . -> . (or/c #f dcon/c))]
   [cond-contracted dmap-meet (dmap? dmap? . -> . (or/c #f dmap?))]))

(define-signature constraints^
  ([cond-contracted cset-meet ((cset? cset?) #:rest (listof cset?) . ->* . (or/c #f cset?))]
   [cond-contracted cset-meet* ((listof cset?) . -> . (or/c #f cset?))]
   [cond-contracted no-constraint c?]
   [cond-contracted empty-cset cset?]
   [cond-contracted reduce/var (cset? symbol? Type/c Type/c . -> . (or/c #f cset?))]
   [cond-contracted reduce/dotted-var (cset? symbol? dcon/c . -> . (or/c #f cset?))]
   [cond-contracted cset-join ((listof cset?) . -> . (or/c #f cset?))]
   [cond-contracted map/cset
    (((hash/c symbol? c?) dmap? . -> . (or/c #f cset?)) cset?  . -> .  (listof cset?))]
   [cond-contracted c-meet (c? c? . -> . (or/c #f c?))]))

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
