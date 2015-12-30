#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/contract/base racket/contract/combinator
         racket/set racket/generic racket/stream syntax/id-table
         "private/id-set.rkt")

(provide id-set/c free-id-set/c bound-id-set/c)

(define (id-set? s)
  (or (free-id-set? s)
      (bound-id-set? s)))
(define (mutable-id-set? s)
  (or (mutable-free-id-set? s)
      (mutable-bound-id-set? s)))
(define (immutable-id-set? s)
   (or (immutable-free-id-set? s)
       (immutable-bound-id-set? s)))

;; elem/c must be flat contract
;; use 'immutable default, to default to flat contract
(define (free-id-set/c elem/c #:mutability [mutability 'immutable])
  (id-set/c elem/c #:idsettype 'free #:mutability mutability))
(define (bound-id-set/c elem/c #:mutability [mutability 'immutable])
  (id-set/c elem/c #:idsettype 'bound #:mutability mutability))

(define (id-set/c elem/c
                  #:idsettype [idsettype 'dont-care]
                  #:mutability [mutability 'immutable])
  (define idsettype/c
    (case idsettype
      [(dont-care) any/c]
      [(free) free-id-set?]
      [(bound) bound-id-set?]
      [else (raise-arguments-error 'id-set/c
                                  "invalid #:idsettype argument"
                                  "#:idsettype argument" idsettype)]))
  (define mutability/c
    (case mutability
      [(dont-care) any/c]
      [(mutable) mutable-id-set?]
      [(immutable) immutable-id-set?]
      [else (raise-arguments-error 'id-set/c
                                   "invalid #:mutability argument"
                                   "#:mutability argument" mutability)]))
  (unless (flat-contract? elem/c)
       (raise-arguments-error
        'id-set/c
        "element contract must be a flat contract"
        "element contract" (contract-name elem/c)))
  (case mutability
    [(immutable) (flat-id-set-contract elem/c idsettype mutability)]
    [else (chaperone-id-set-contract elem/c idsettype mutability)]))

(struct id-set-contract [elem/c idsettype mutability])

(define (id-set-contract-name ctc)
  (define elem/c (id-set-contract-elem/c ctc))
  (define idsettype (id-set-contract-idsettype ctc))
  (define mutability (id-set-contract-mutability ctc))
  `(id-set/c ,(contract-name elem/c)
             ,@(if (eq? idsettype 'dont-care)
                   `[]
                   `[#:idsettype (quote ,idsettype)])
             ,@(if (eq? mutability 'dont-care)
                   `[]
                   `[#:mutability (quote ,mutability)])))

(define (id-set-contract-first-order ctc)
  (define idsettype (id-set-contract-idsettype ctc))
  (define mutability (id-set-contract-mutability ctc))
  (define idsettype?
    (case idsettype
      [(dont-care) (lambda (x) #t)]
      [(free) free-id-set?]
      [(bound) bound-id-set?]))
  (define mutability?
    (case mutability
      [(dont-care) (lambda (x) #t)]
      [(mutable) mutable-id-set?]
      [(immutable) immutable-id-set?]))
  (lambda (s)
    (and (id-set? s) (idsettype? s) (mutability? s))))

(define (id-set-contract-check idsettype mutability b s)
  (unless (id-set? s)
    (raise-blame-error b s "expected either a free or bound identifier set"))
  (case idsettype
    [(free)
     (unless (free-id-set? s)
       (raise-blame-error b s "expected a free-identifier set"))]
    [(bound)
     (unless (bound-id-set? s)
       (raise-blame-error b s "expected a bound-identifier set"))])
  (case mutability
    [(mutable)
     (unless (mutable-id-set? s)
       (raise-blame-error b s "expected a mutable id set"))]
    [(immutable)
     (unless (immutable-id-set? s)
       (raise-blame-error b s "expected an immutable id set"))]))

(define (flat-id-set-contract-first-order ctc)
  (define set-passes? (id-set-contract-first-order ctc))
  (define elem-passes? (contract-first-order (id-set-contract-elem/c ctc)))
  (lambda (s)
    (and (set-passes? s)
         (for/and ([e (in-set s)]) (elem-passes? e)))))

(define (flat-id-set-late-neg-contract-projection ctc)
  (define elem/c (id-set-contract-elem/c ctc))
  (define idsettype (id-set-contract-idsettype ctc))
  (define mutability (id-set-contract-mutability ctc))
  (lambda (b)
    (define proj
      ((contract-late-neg-projection elem/c) (blame-add-context b "an element of")))
    (lambda (s neg-party)
      (id-set-contract-check idsettype mutability b s neg-party)
      (for ([e (in-set s)]) (proj e neg-party))
      s)))

(define (id-set-late-neg-contract-projection ctc)
  (define elem/c (id-set-contract-elem/c ctc))
  (define idsettype (id-set-contract-idsettype ctc))
  (define mutability (id-set-contract-mutability ctc))
  (lambda (b)
    (define neg-proj
      ((contract-late-neg-projection elem/c) (blame-add-context b "an element of" #:swap? #t)))
    (lambda (s neg-party)
      (id-set-contract-check idsettype mutability b s neg-party)
      (cond
        [(immutable-free-id-set? s)
         (chaperone-immutable-free-id-set 
          s (free-id-table/c (λ (v) (neg-proj v neg-party)) any/c #:immutable #t))]
        [(mutable-free-id-set? s)
         (chaperone-mutable-free-id-set 
          s (free-id-table/c (λ (v) (neg-proj v neg-party)) any/c #:immutable #f))]
        [(immutable-bound-id-set? s)
         (chaperone-immutable-bound-id-set 
          s (bound-id-table/c (λ (v) (neg-proj v neg-party)) any/c #:immutable #t))]
        [(mutable-bound-id-set? s)
         (chaperone-mutable-bound-id-set 
          s (bound-id-table/c (λ (v) (neg-proj v neg-party)) any/c #:immutable #f))]))))
          

(struct flat-id-set-contract id-set-contract []
  #:property prop:flat-contract
  (build-flat-contract-property
    #:name id-set-contract-name
    #:first-order flat-id-set-contract-first-order
    #:late-neg-projection flat-id-set-late-neg-contract-projection))

(struct chaperone-id-set-contract id-set-contract []
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
    #:name id-set-contract-name
    #:first-order id-set-contract-first-order
    #:late-neg-projection id-set-late-neg-contract-projection))

(define-syntax (provide-contracted-id-set-fns stx)
  (syntax-parse stx
    [(_ #:type type)
     #:with id-set (fmt-id "~a-id-set" #'type)
     #:with mutable-id-set (fmt-id "mutable-~a" #'id-set)
     #:with immutable-id-set (fmt-id "immutable-~a" #'id-set)
     #:with id-set? (fmt-pred-name #'id-set)
     #:with mutable-id-set? (fmt-pred-name #'mutable-id-set)
     #:with immutable-id-set? (fmt-pred-name #'immutable-id-set)
     #:with id-set-empty? (fmt-set-id-fn-name #'type "empty?")
     #:with id-set-count (fmt-set-id-fn-name #'type "count")
     #:with id-set-member? (fmt-set-id-fn-name #'type "member?")
     #:with id-set-add (fmt-set-id-fn-name #'type "add")
     #:with id-set-add! (fmt-set-id-fn-name #'type "add!")
     #:with id-set-remove (fmt-set-id-fn-name #'type "remove")
     #:with id-set-remove! (fmt-set-id-fn-name #'type "remove!")
     #:with id-set-first (fmt-set-id-fn-name #'type "first")
     #:with id-set-rest (fmt-set-id-fn-name #'type "rest")
     #:with id-set->stream (fmt-set-id-fn-name #'type ">stream")
     #:with id-set->list (fmt-set-id-fn-name #'type ">list")
     #:with id-set-copy (fmt-set-id-fn-name #'type "copy")
     #:with id-set-copy-clear (fmt-set-id-fn-name #'type "copy-clear")
     #:with id-set-clear (fmt-set-id-fn-name #'type "clear")
     #:with id-set-clear! (fmt-set-id-fn-name #'type "clear!")
     #:with id-set-union (fmt-set-id-fn-name #'type "union")
     #:with id-set-union! (fmt-set-id-fn-name #'type "union!")
     #:with id-set-intersect (fmt-set-id-fn-name #'type "intersect")
     #:with id-set-intersect! (fmt-set-id-fn-name #'type "intersect!")
     #:with id-set-subtract (fmt-set-id-fn-name #'type "subtract")
     #:with id-set-subtract! (fmt-set-id-fn-name #'type "subtract!")
     #:with id-set-symmetric-difference (fmt-set-id-fn-name #'type "symmetric-difference")
     #:with id-set-symmetric-difference! (fmt-set-id-fn-name #'type "symmetric-difference!")
     #:with id-set-map (fmt-set-id-fn-name #'type "map")
     #:with id-set-for-each (fmt-set-id-fn-name #'type "for-each")
     ;; these fns don't have the conventional (eg "free-id-set-") prefix
     #:with id-subset? (fmt-id "~a-id-subset?" #'type)
     #:with id-proper-subset? (fmt-id "~a-id-proper-subset?" #'type)
     #:with in-id-set (fmt-id "in-~a-id-set" #'type)
     #:with id-set=? (fmt-id "~a-id-set=?" #'type)
     #'(provide/contract
        [mutable-id-set
         (->* () (generic-set? #:phase (or/c #f exact-integer?)) mutable-id-set?)]
        [immutable-id-set
         (->* () (generic-set? #:phase (or/c #f exact-integer?)) immutable-id-set?)]
        [id-set?           (-> any/c boolean?)]
        [mutable-id-set?   (-> any/c boolean?)]
        [immutable-id-set? (-> any/c boolean?)]
        [id-set-empty?     (-> id-set? boolean?)]
        [id-set-count      (-> id-set? exact-nonnegative-integer?)]
        [id-set-member?    (-> id-set? identifier? boolean?)]
        [id-set=?          (-> id-set? id-set? boolean?)]
        [id-set-add        (-> immutable-id-set? identifier? immutable-id-set?)]
        [id-set-add!       (-> mutable-id-set? identifier? void?)]
        [id-set-remove     (-> immutable-id-set? identifier? immutable-id-set?)]
        [id-set-remove!    (-> mutable-id-set? identifier? void?)]
        [id-set-first      (-> id-set? identifier?)]
        [id-set-rest       (-> immutable-id-set? immutable-id-set?)]
        [in-id-set         (-> id-set? sequence?)]
        [id-set->stream    (-> id-set? stream?)]
        [id-set->list      (-> id-set? list?)]
        [id-set-copy       (-> id-set? id-set?)]
        [id-set-copy-clear (-> id-set? id-set?)]
        [id-set-clear      (-> immutable-id-set? immutable-id-set?)]
        [id-set-clear!     (-> mutable-id-set? mutable-id-set?)]
        [id-set-union 
         (->* [immutable-id-set?] [] #:rest (listof id-set?) immutable-id-set?)]
        [id-set-union! 
         (->* [mutable-id-set?] [] #:rest (listof id-set?) void?)]
        [id-set-intersect 
         (->* [immutable-id-set?] [] #:rest (listof id-set?) immutable-id-set?)]
        [id-set-intersect! 
         (->* [mutable-id-set?] [] #:rest (listof id-set?) void?)]
        [id-set-subtract 
         (->* [immutable-id-set?] [] #:rest (listof id-set?) immutable-id-set?)]
        [id-set-subtract! 
         (->* [mutable-id-set?] [] #:rest (listof id-set?) void?)]
        [id-set-symmetric-difference 
         (->* [immutable-id-set?] [] #:rest (listof id-set?) immutable-id-set?)]
        [id-set-symmetric-difference! 
         (->* [mutable-id-set?] [] #:rest (listof id-set?) void?)]
        [id-subset? (-> id-set? id-set? boolean?)]
        [id-proper-subset? (-> id-set? id-set? boolean?)]
        [id-set-map (-> id-set? (-> identifier? any/c) list?)]
        [id-set-for-each (-> id-set? (-> identifier? any/c) void?)])]))

(provide-contracted-id-set-fns #:type free)
(provide-contracted-id-set-fns #:type bound)
