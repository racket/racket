#lang racket/base

(require racket/contract/base
         racket/contract/combinator
         racket/private/set
         racket/private/set-types
         racket/generic
         racket/private/for
         (for-syntax racket/base))

(provide (all-from-out racket/private/set)
         (all-from-out racket/private/set-types)
         set/c)

(define/subexpression-pos-prop/name
  real-set/c-name (set/c elem/c
                         #:cmp [cmp 'dont-care]
                         #:kind [kind 'immutable]
                         #:lazy? [_lazy? (lazy-default kind elem/c)])
  (define lazy? (and _lazy? #t))
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
          (not lazy?)
          (flat-contract? elem/c))
     (flat-set-contract elem/c cmp kind lazy?)]
    [(chaperone-contract? elem/c)
     (chaperone-set-contract elem/c cmp kind lazy?)]
    [else
     (impersonator-set-contract elem/c cmp kind lazy?)]))

(struct set-contract [elem/c cmp kind lazy?])

(define (lazy-default kind elem/c)
  (not (and (equal? kind 'immutable)
            (flat-contract? elem/c))))

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
                `[#:kind (quote ,kind)])
          ,@(if (equal? (set-contract-lazy? ctc)
                        (lazy-default kind elem/c))
                '()
                `(#:lazy? ,(set-contract-lazy? ctc)))))

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

(define (set-contract-check cmp kind b neg-party x)
  (unless (generic-set? x)
    (raise-blame-error b #:missing-party neg-party x "expected a set"))
  (case cmp
    [(equal)
     (unless (set-equal? x)
       (raise-blame-error b #:missing-party neg-party x "expected an equal?-based set"))]
    [(eqv)
     (unless (set-eqv? x)
       (raise-blame-error b #:missing-party neg-party x "expected an eqv?-based set"))]
    [(eq)
     (unless (set-eq? x)
       (raise-blame-error b #:missing-party neg-party x "expected an eq?-based set"))])
  (case kind
    [(mutable-or-weak)
     (unless (or (set-mutable? x) (set-weak? x))
       (raise-blame-error b #:missing-party neg-party x "expected a mutable or weak set"))]
    [(mutable)
     (unless (set-mutable? x)
       (raise-blame-error b #:missing-party neg-party x "expected a mutable set"))]
    [(weak)
     (unless (set-weak? x)
       (raise-blame-error b #:missing-party neg-party x "expected a weak set"))]
    [(immutable)
     (unless (set? x)
       (raise-blame-error b #:missing-party neg-party x "expected an immutable set"))]))

(define (set-contract-late-neg-projection chaperone-ctc?)
  (lambda (ctc)
    (cond
      [(allows-generic-sets? ctc)
       (generic-set-late-neg-projection ctc chaperone-ctc?)]
      [else
       (hash-set-late-neg-projection ctc chaperone-ctc?)])))

(define (allows-generic-sets? ctc)
  (and (equal? 'dont-care (set-contract-kind ctc))
       (equal? 'dont-care (set-contract-cmp ctc))))

(define (hash-set-late-neg-projection ctc chaperone-ctc?)
  (define elem/c (set-contract-elem/c ctc))
  (define cmp (set-contract-cmp ctc))
  (define kind (set-contract-kind ctc))
  (define late-neg-ele-proj (contract-late-neg-projection elem/c))
  (define lazy? (set-contract-lazy? ctc))
  (λ (blame)
    (define late-neg-pos-proj (late-neg-ele-proj (blame-add-element-context blame #f)))
    (define late-neg-neg-proj (late-neg-ele-proj (blame-add-element-context blame #t)))
    (define set/c-lazy-late-neg-proj
      (λ (val neg-party)
        (set-contract-check cmp kind blame neg-party val)
        (define (pos-interpose val ele) (late-neg-pos-proj ele neg-party))
        (cond
          [(set? val)
           (chaperone-hash-set
            val
            pos-interpose
            (λ (val ele) ele)
            pos-interpose
            impersonator-prop:contracted
            ctc)]
          [else
           (chaperone-hash-set
            val
            pos-interpose
            (λ (val ele) (late-neg-neg-proj ele neg-party))
            pos-interpose
            impersonator-prop:contracted
            ctc)])))
    (cond
      [lazy? set/c-lazy-late-neg-proj]
      [else
       (λ (val neg-party)
         (set-contract-check cmp kind blame neg-party val)
         (define w/chaperone
           (cond
             [(set? val) val]
             [else
              (chaperone-hash-set
               val
               (λ (val ele) ele)
               (λ (val ele) (late-neg-neg-proj ele neg-party))
               (λ (val ele) ele))]))
         (chaperone-hash-set
          (for/set ([ele (in-set w/chaperone)])
            (late-neg-pos-proj ele neg-party))
          (chaperone-hash-set
           val
           #f #f #f
           impersonator-prop:contracted
           ctc)))])))
            

(define (generic-set-late-neg-projection ctc chaperone-ctc?)
  (define elem/c (set-contract-elem/c ctc))
  (define cmp (set-contract-cmp ctc))
  (define kind (set-contract-kind ctc))
  (define lazy? (set-contract-lazy? ctc))
  (lambda (blame)
    (define (method sym c)
      (define name (contract-name c))
      (define str (format "method ~a with contract ~.s" sym name))
      (define b2 (blame-add-context blame str))
      ((contract-late-neg-projection c) b2))
    (define-syntax (redirect stx)
      (syntax-case stx ()
        [(_ [id expr] ...)
         (with-syntax ([(proj-id ...) (generate-temporaries #'(id ...))])
           #'(let ([proj-id (method 'id expr)] ...)
               (λ (x neg-party)
                 (redirect-generics chaperone-ctc?
                                    gen:set x [id (λ (x) (proj-id x neg-party))] ...))))]))
    (define me (if chaperone-contract?
                   (make-chaperone-contract
                    #:name (set-contract-name ctc)
                    #:stronger set-contract-stronger
                    #:late-neg-projection
                    (λ (blame) (λ (val neg-party) (do-redirect val neg-party))))
                   (make-contract
                    #:name (set-contract-name ctc)
                    #:stronger set-contract-stronger
                    #:late-neg-projection
                    (λ (blame) (λ (val neg-party) (do-redirect val neg-party))))))
    (define do-redirect
      (redirect
       [set-member? (-> generic-set? elem/c boolean?)]
       [set-empty? (or/c (-> generic-set? boolean?) #f)]
       [set-count (or/c (-> generic-set? exact-nonnegative-integer?) #f)]
       [set=? (or/c (-> generic-set? me boolean?) #f)]
       [subset? (or/c (-> generic-set? me boolean?) #f)]
       [proper-subset? (or/c (-> generic-set? me boolean?) #f)]
       [set-map (or/c (-> generic-set? (-> elem/c any/c) list?) #f)]
       [set-for-each (or/c (-> generic-set? (-> elem/c any) void?) #f)]
       [set-copy (or/c (-> generic-set? generic-set?) #f)]
       [in-set (or/c (-> generic-set? sequence?) #f)]
       [set->list (or/c (-> generic-set? (listof elem/c)) #f)]
       [set->stream (or/c (-> generic-set? stream?) #f)]
       [set-first (or/c (-> generic-set? elem/c) #f)]
       [set-rest (or/c (-> generic-set? me) #f)]
       [set-add (or/c (-> generic-set? elem/c me) #f)]
       [set-remove (or/c (-> generic-set? elem/c me) #f)]
       [set-clear (or/c (-> generic-set? me) #f)]
       [set-copy-clear (or/c (-> generic-set? generic-set?) #f)]
       [set-union
        (or/c (->* [generic-set?] [] #:rest (listof me) me) #f)]
       [set-intersect
        (or/c (->* [generic-set?] [] #:rest (listof me) me) #f)]
       [set-subtract
        (or/c (->* [generic-set?] [] #:rest (listof me) me) #f)]
       [set-symmetric-difference
        (or/c (->* [generic-set?] [] #:rest (listof me) me) #f)]
       [set-add! (or/c (-> generic-set? elem/c void?) #f)]
       [set-remove! (or/c (-> generic-set? elem/c void?) #f)]
       [set-clear! (or/c (-> generic-set? void?) #f)]
       [set-union!
        (or/c (->* [generic-set?] [] #:rest (listof me) void?) #f)]
       [set-intersect!
        (or/c (->* [generic-set?] [] #:rest (listof me) void?) #f)]
       [set-subtract!
        (or/c (->* [generic-set?] [] #:rest (listof me) void?) #f)]
       [set-symmetric-difference!
        (or/c (->* [generic-set?] [] #:rest (listof me) void?) #f)]))
    (define proj
      ((contract-late-neg-projection elem/c) (blame-add-element-context blame #f)))
    (lambda (x neg-party)
      (set-contract-check cmp kind blame neg-party x)
      (cond
        [(list? x)
         (for/list ([e (in-list x)])
           (proj e neg-party))]
        [else
         (do-redirect x neg-party)]))))
      

(define (blame-add-element-context blame swap?)
  (blame-add-context blame "an element of" #:swap? swap?))

(define (flat-set-contract-first-order ctc)
  (define set-passes? (set-contract-first-order ctc))
  (define elem-passes? (contract-first-order (set-contract-elem/c ctc)))
  (lambda (x)
    (and (set-passes? x)
         (for/and ([e (in-set x)])
           (elem-passes? e)))))

(define (flat-set-contract-late-neg-projection ctc)
  (define elem/c (set-contract-elem/c ctc))
  (define cmp (set-contract-cmp ctc))
  (define kind (set-contract-kind ctc))
  (define mk-elem/c-proj (contract-late-neg-projection elem/c))
  (lambda (b)
    (define proj (mk-elem/c-proj (blame-add-context b "an element of")))
    (lambda (x neg-party)
      (set-contract-check cmp kind b neg-party x)
      (for ([e (in-set x)])
        (proj e neg-party))
      x)))

(define (set-contract-stronger this that)
  #f)

(struct flat-set-contract set-contract []
  #:property prop:flat-contract
  (build-flat-contract-property
    #:name set-contract-name
    #:stronger set-contract-stronger
    #:first-order flat-set-contract-first-order
    #:late-neg-projection flat-set-contract-late-neg-projection))

(struct chaperone-set-contract set-contract []
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
    #:name set-contract-name
    #:stronger set-contract-stronger
    #:first-order set-contract-first-order
    #:late-neg-projection (set-contract-late-neg-projection #t)))

(struct impersonator-set-contract set-contract []
  #:property prop:contract
  (build-contract-property
    #:name set-contract-name
    #:stronger set-contract-stronger
    #:first-order set-contract-first-order
    #:late-neg-projection (set-contract-late-neg-projection #f)))
