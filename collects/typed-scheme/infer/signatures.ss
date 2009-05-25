#lang scheme/base
(require scheme/unit scheme/contract "constraint-structs.ss" "../utils/utils.ss")
(require (rep type-rep) (utils unit-utils))
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
  ([cnt infer (((listof symbol?) (listof Type?) (listof Type?) Type? (listof symbol?)) ((or/c #f Type?)) . ->* . any)]
   [cnt infer/vararg (((listof symbol?) 
                       (listof Type?)
                       (listof Type?)
                       (or/c #f Type?)
                       Type?
                       (listof symbol?))
                      ((or/c #f Type?)) . ->* . any)]
   [cnt infer/dots (((listof symbol?) 
                     symbol?
                     (listof Type?) (listof Type?) Type? Type? (listof symbol?))
                    (#:expected (or/c #f Type?)) . ->* . any)]))
