#lang racket/base
(require racket/unit racket/contract racket/require
         "constraint-structs.rkt" 
         (path-up "utils/utils.rkt" "utils/unit-utils.rkt" "rep/type-rep.rkt"))
(provide (all-defined-out))

(define-signature dmap^
  ([cnt dmap-meet (dmap? dmap? . -> . dmap?)]))

(define-signature promote-demote^
  ([cnt var-promote (Type? (listof symbol?) . -> . Type?)]
   [cnt var-demote  (Type? (listof symbol?) . -> . Type?)]))

(define-signature constraints^
  ([cnt exn:infer? (any/c . -> . boolean?)]
   [cnt fail-sym symbol?]
   ;; inference failure - masked before it gets to the user program
   (define-syntaxes (fail!)
     (syntax-rules ()
       [(_ s t) (raise fail-sym)]))
   [cnt cset-meet (cset? cset? . -> . cset?)]
   [cnt cset-meet* ((listof cset?) . -> . cset?)]
   no-constraint
   [cnt empty-cset ((listof symbol?) . -> . cset?)]
   [cnt insert (cset? symbol? Type? Type? . -> . cset?)]
   [cnt cset-combine ((listof cset?) . -> . cset?)]
   [cnt c-meet ((c? c?) (symbol?) . ->* . c?)]))

(define-signature restrict^
  ([cnt restrict (Type? Type? . -> . Type?)]))

(define-signature infer^
  ([cnt infer ((;; variables from the forall
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
   [cnt infer/vararg ((;; variables from the forall
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
   [cnt infer/dots (((listof symbol?) 
                     symbol?
                     (listof Type?) (listof Type?) Type? Type? (listof symbol?))
                    (#:expected (or/c #f Type?)) . ->* . any)]))
