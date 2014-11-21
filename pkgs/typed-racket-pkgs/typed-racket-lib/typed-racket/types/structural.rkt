#lang racket/base

;; Module for providing recursive operations over types when the operation doesn't care about the
;; type constructor.

;; This file is meant to implement more general versions of type-case.
;; Currently supported
;; * Trivial type constructors (only have Rep? or (listof Rep?) fields)
;; * A variance aware traversal of a Rep? with the return value having the same type constructor as
;;    the input.
;; To be added
;; * Support for type constructors with non Rep? fields
;; * Support for objects and filters
;; * Support for smart constructors for the return value
;; * Support for return values that are not Rep?
;; * Parallel traversal of two types

(require
  "../utils/utils.rkt"
  racket/match
  (rep type-rep)
  (for-syntax
    racket/base
    syntax/parse
    racket/syntax))
(provide
  structural?
  structural-map)


(define-for-syntax structural-reps
  #'([BoxTop ()]
     [ChannelTop ()]
     [Async-ChannelTop ()]
     [ClassTop ()]
     [Continuation-Mark-KeyTop ()]
     [Error ()]
     [HashtableTop ()]
     [MPairTop ()]
     [Prompt-TagTop ()]
     [StructTypeTop ()]
     [ThreadCellTop ()]
     [Univ ()]
     [VectorTop ()]

     [CustodianBox (#:co)]
     [Ephemeron (#:co)]
     [Evt (#:co)]
     [Future (#:co)]
     [Instance (#:co)]
     [Promise (#:co)]
     [Set (#:co)]
     [StructTop (#:co)]
     [StructType (#:co)]
     [Syntax (#:co)]
     [Pair (#:co #:co)]
     [Sequence ((#:listof #:co))]
     [Function ((#:listof #:co))]

     [Param (#:contra #:co)]

     [Continuation-Mark-Keyof (#:inv)]
     [Box (#:inv)]
     [Channel (#:inv)]
     [Async-Channel (#:inv)]
     [ThreadCell (#:inv)]
     [Vector (#:inv)]
     [Hashtable (#:inv #:inv)]
     [MPair (#:inv #:inv)]
     [Prompt-Tagof (#:inv #:inv)]
     [HeterogeneousVector ((#:listof #:inv))]

     ;; Non Types
     [Result (#:co #:co #:co)]
     [Values ((#:listof #:co))]
     [AnyValues (#:co)]))

(begin-for-syntax
  (define-syntax-class type-name
    #:attributes (pred? matcher: maker)
    (pattern t:id
      #:with pred? (format-id #'t "~a?" #'t)
      #:with matcher: (format-id #'t "~a:" #'t)
      #:with maker (format-id #'t "make-~a" #'t))))

(begin-for-syntax
  (define-syntax-class type-variance
    #:attributes (sym)
    (pattern #:co #:with sym 'co)
    (pattern #:inv #:with sym 'inv)
    (pattern #:contra #:with sym 'contra))

  (define-syntax-class type-field
    (pattern var:type-variance)
    (pattern (#:listof var:type-variance))))



(define-syntax (gen-structural? stx)
  (syntax-parse structural-reps
   [([type:type-name (field:type-field ...)] ...)
    #'(lambda (t)
        (or (type.pred? t) ...))]))

;; Returns true if the type/filter/object supports structural operations.
(define structural? (gen-structural?))


(define-syntax (gen-structural-map stx)
  (syntax-parse stx
   [(_ input-type:id recur-f:id)
    (define-syntax-class type-field*
      #:attributes (recur)
      (pattern var:type-variance
        #:with recur #'(λ (t) (recur-f t 'var.sym)))
      (pattern (#:listof var:type-variance)
        #:with recur #'(λ (ts) (for/list ([t (in-list ts)]) (recur-f t 'var.sym)))))

    (define-syntax-class type-clause
      #:attributes (match-clause)
      (pattern [type:type-name (field:type-field* ...)]
        #:with (field-pat ...) (generate-temporaries #'(field ...))
        #:with match-clause
          #'[(type.matcher: field-pat ...)
             (type.maker (field.recur field-pat) ...)]))

    (syntax-parse structural-reps
     [(:type-clause ...)
       #'(match input-type match-clause ...)])]))

;; Rep? (-> Rep? (or/c 'co 'contra 'inv) Rep?) -> Rep?
;; Calls `f` on each sub-type with the corresponding variance of the sub-type and combines the results
;; using the type constructor of the input type
(define (structural-map t f)
  (gen-structural-map t f))
