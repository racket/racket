#lang racket/base

(require racket/contract/base
         racket/contract/combinator
         racket/private/set
         racket/private/set-types
         racket/generic
         racket/private/for)

(provide (all-from-out racket/private/set)
         (all-from-out racket/private/set-types)
         set/c)

(define (set/c elem/c
               #:cmp [cmp 'dont-care]
               #:kind [kind 'immutable])
  (define cmp/c
    (case cmp
      [(dont-care) any/c]
      [(equal) set-equal?]
      [(eqv) set-eqv?]
      [(eq) set-eq?]
      [else (raise-arguments-error 'set/c
                                  "invalid #:cmp argument"
                                  "#:cmp argument" cmp)]))
  (define kind/c
    (case kind
      [(dont-care) any/c]
      [(mutable-or-weak) (or/c set-weak? set-mutable?)]
      [(mutable) set-mutable?]
      [(weak) set-weak?]
      [(immutable) set?]
      [else (raise-arguments-error 'set/c
                                   "invalid #:kind argument"
                                   "#:kind argument" kind)]))
  (case cmp
    [(eqv eq)
     (unless (flat-contract? elem/c)
       (raise-arguments-error
        'set/c
        "element contract must be a flat contract for eqv? and eq?-based sets"
        "element contract" (contract-name elem/c)
        "#:cmp option" cmp))]
    [else
     (unless (chaperone-contract? elem/c)
       (raise-argument-error 'set/c "chaperone-contract?" elem/c))])
  (cond
    [(and (eq? kind 'immutable)
          (flat-contract? elem/c))
     (flat-set-contract elem/c cmp kind)]
    [(chaperone-contract? elem/c)
     (chaperone-set-contract elem/c cmp kind)]
    [else
     (impersonator-set-contract elem/c cmp kind)]))

(struct set-contract [elem/c cmp kind])

(define (set-contract-name ctc)
  (define elem/c (set-contract-elem/c ctc))
  (define cmp (set-contract-cmp ctc))
  (define kind (set-contract-kind ctc))
  `(set/c ,(contract-name elem/c)
          ,@(if (eq? cmp 'dont-care)
                `[]
                `[#:cmp (quote ,cmp)])
          ,@(if (eq? kind 'immutable)
                `[]
                `[#:kind (quote ,kind)])))

(define (set-contract-first-order ctc)
  (define cmp (set-contract-cmp ctc))
  (define kind (set-contract-kind ctc))
  (define cmp?
    (case cmp
      [(dont-care) (lambda (x) #t)]
      [(equal) set-equal?]
      [(eqv) set-eqv?]
      [(eq) set-eq?]))
  (define kind?
    (case kind
      [(dont-care) (lambda (x) #t)]
      [(mutable-or-weak) (lambda (x) (or (set-mutable? x) (set-weak? x)))]
      [(mutable) set-mutable?]
      [(weak) set-weak?]
      [(immutable) set?]))
  (lambda (x)
    (and (generic-set? x) (cmp? x) (kind? x))))

(define (set-contract-check cmp kind b x)
  (unless (generic-set? x)
    (raise-blame-error b x "expected a set"))
  (case cmp
    [(equal)
     (unless (set-equal? x)
       (raise-blame-error b x "expected an equal?-based set"))]
    [(eqv)
     (unless (set-eqv? x)
       (raise-blame-error b x "expected an eqv?-based set"))]
    [(eq)
     (unless (set-eq? x)
       (raise-blame-error b x "expected an eq?-based set"))])
  (case kind
    [(mutable-or-weak)
     (unless (or (set-mutable? x) (set-weak? x))
       (raise-blame-error b x "expected a mutable or weak set"))]
    [(mutable)
     (unless (set-mutable? x)
       (raise-blame-error b x "expected a mutable set"))]
    [(weak)
     (unless (set-weak? x)
       (raise-blame-error b x "expected a weak set"))]
    [(immutable)
     (unless (set? x)
       (raise-blame-error b x "expected an immutable set"))]))

(define (set-contract-projection mode)
  (lambda (ctc)
    (define elem/c (set-contract-elem/c ctc))
    (define cmp (set-contract-cmp ctc))
    (define kind (set-contract-kind ctc))
    (lambda (b)
      (lambda (x)
        (set-contract-check cmp kind b x)
        (cond
          [(list? x)
           (define proj
             ((contract-projection elem/c)
              (blame-add-context b "an element of")))
           (map proj x)]
          [else
           (define (method sym c)
             (lambda (x)
               (define name (contract-name c))
               (define str (format "method ~a with contract ~.s" sym name))
               (define b2 (blame-add-context b str))
               (((contract-projection c) b2) x)))
           (define-syntax-rule (redirect [id expr] ...)
             (redirect-generics mode gen:set x [id (method 'id expr)] ...))
           (redirect
             [set-member? (-> generic-set? elem/c boolean?)]
             [set-empty? (or/c (-> generic-set? boolean?) #f)]
             [set-count (or/c (-> generic-set? exact-nonnegative-integer?) #f)]
             [set=? (or/c (-> generic-set? ctc boolean?) #f)]
             [subset? (or/c (-> generic-set? ctc boolean?) #f)]
             [proper-subset? (or/c (-> generic-set? ctc boolean?) #f)]
             [set-map (or/c (-> generic-set? (-> elem/c any/c) list?) #f)]
             [set-for-each (or/c (-> generic-set? (-> elem/c any) void?) #f)]
             [set-copy (or/c (-> generic-set? generic-set?) #f)]
             [in-set (or/c (-> generic-set? sequence?) #f)]
             [set->list (or/c (-> generic-set? (listof elem/c)) #f)]
             [set->stream (or/c (-> generic-set? stream?) #f)]
             [set-first (or/c (-> generic-set? elem/c) #f)]
             [set-rest (or/c (-> generic-set? ctc) #f)]
             [set-add (or/c (-> generic-set? elem/c ctc) #f)]
             [set-remove (or/c (-> generic-set? elem/c ctc) #f)]
             [set-clear (or/c (-> generic-set? ctc) #f)]
             [set-copy-clear (or/c (-> generic-set? generic-set?) #f)]
             [set-union
              (or/c (->* [generic-set?] [] #:rest (listof ctc) ctc) #f)]
             [set-intersect
              (or/c (->* [generic-set?] [] #:rest (listof ctc) ctc) #f)]
             [set-subtract
              (or/c (->* [generic-set?] [] #:rest (listof ctc) ctc) #f)]
             [set-symmetric-difference
              (or/c (->* [generic-set?] [] #:rest (listof ctc) ctc) #f)]
             [set-add! (or/c (-> generic-set? elem/c void?) #f)]
             [set-remove! (or/c (-> generic-set? elem/c void?) #f)]
             [set-clear! (or/c (-> generic-set? void?) #f)]
             [set-union!
              (or/c (->* [generic-set?] [] #:rest (listof ctc) void?) #f)]
             [set-intersect!
              (or/c (->* [generic-set?] [] #:rest (listof ctc) void?) #f)]
             [set-subtract!
              (or/c (->* [generic-set?] [] #:rest (listof ctc) void?) #f)]
             [set-symmetric-difference!
              (or/c (->* [generic-set?] [] #:rest (listof ctc) void?) #f)])])))))

(define (flat-set-contract-first-order ctc)
  (define set-passes? (set-contract-first-order ctc))
  (define elem-passes? (contract-first-order (set-contract-elem/c ctc)))
  (lambda (x)
    (and (set-passes? x)
         (for/and ([e (in-set x)])
           (elem-passes? e)))))

(define (flat-set-contract-projection ctc)
  (define elem/c (set-contract-elem/c ctc))
  (define cmp (set-contract-cmp ctc))
  (define kind (set-contract-kind ctc))
  (lambda (b)
    (lambda (x)
      (set-contract-check cmp kind b x)
      (define proj
        ((contract-projection elem/c)
         (blame-add-context b "an element of")))
      (for ([e (in-set x)])
        (proj e))
      x)))

(struct flat-set-contract set-contract []
  #:property prop:flat-contract
  (build-flat-contract-property
    #:name set-contract-name
    #:first-order flat-set-contract-first-order
    #:projection flat-set-contract-projection))

(struct chaperone-set-contract set-contract []
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
    #:name set-contract-name
    #:first-order set-contract-first-order
    #:projection (set-contract-projection #t)))

(struct impersonator-set-contract set-contract []
  #:property prop:contract
  (build-contract-property
    #:name set-contract-name
    #:first-order set-contract-first-order
    #:projection (set-contract-projection #f)))
