#lang racket/base
(require "../utils/utils.rkt"
         racket/unit (contract-req)
         (infer constraint-structs)
         (utils unit-utils)
         (rep type-rep))
(provide (all-defined-out))

(define-signature dmap^
  ([cond-contracted dmap-meet (dmap? dmap? . -> . (or/c #f dmap?))]))

(define-signature constraints^
  ([cond-contracted cset-meet ((cset? cset?) #:rest (listof cset?) . ->* . (or/c #f cset?))]
   [cond-contracted cset-meet* ((listof cset?) . -> . (or/c #f cset?))]
   [cond-contracted no-constraint c?]
   [cond-contracted empty-cset ((listof symbol?) (listof symbol?) . -> . cset?)]
   [cond-contracted insert (cset? symbol? Type/c Type/c . -> . cset?)]
   [cond-contracted cset-join ((listof cset?) . -> . cset?)]
   [cond-contracted c-meet ((c? c?) (symbol?) . ->* . (or/c #f c?))]))

(define-signature restrict^
  ([cond-contracted restrict ((Type/c Type/c) ((or/c 'new 'orig)) . ->* . Type/c)]))

(define-signature infer^
  ([cond-contracted infer ((;; variables from the forall
                            (listof symbol?)
                            ;; indexes from the forall
                            (listof symbol?)
                            ;; actual argument types from call site
                            Type/c
                            ;; domain
                            Type/c
                            ;; range
                            (or/c #f Values/c AnyValues? ValuesDots?))
                           ;; optional expected type
                           ((or/c #f Values/c AnyValues? ValuesDots?))
                           . ->* . any)]))
