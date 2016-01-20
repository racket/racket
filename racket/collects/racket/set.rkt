#lang racket/base

(require racket/contract/base
         racket/contract/combinator
         "private/set.rkt"
         "private/set-types.rkt"
         racket/generic
         racket/private/for
         (for-syntax racket/base))

(provide gen:set generic-set? set-implements?

         set-empty? set-member? set-count
         set=? subset? proper-subset?
         set-map set-for-each
         set-copy set-copy-clear
         set->list set->stream set-first set-rest
         set-add set-remove set-clear
         set-union set-intersect set-subtract set-symmetric-difference
         set-add! set-remove! set-clear!
         set-union! set-intersect! set-subtract! set-symmetric-difference!

         in-set
         in-immutable-set
         in-mutable-set
         in-weak-set
         set-implements/c

         set seteq seteqv
         weak-set weak-seteq weak-seteqv
         mutable-set mutable-seteq mutable-seteqv
         list->set list->seteq list->seteqv
         list->weak-set list->weak-seteq list->weak-seteqv
         list->mutable-set list->mutable-seteq list->mutable-seteqv
         set-eq? set-eqv? set-equal?
         set-weak? set-mutable? set?
         for/set for/seteq for/seteqv
         for*/set for*/seteq for*/seteqv
         for/weak-set for/weak-seteq for/weak-seteqv
         for*/weak-set for*/weak-seteq for*/weak-seteqv
         for/mutable-set for/mutable-seteq for/mutable-seteqv
         for*/mutable-set for*/mutable-seteq for*/mutable-seteqv

         define-custom-set-types
         make-custom-set-types
         make-custom-set
         make-weak-custom-set
         make-mutable-custom-set

         chaperone-hash-set
         impersonate-hash-set
         
         set/c)

(define/subexpression-pos-prop/name
  real-set/c-name (set/c _elem/c
                         #:equal-key/c [_equal-key/c any/c]
                         #:cmp [cmp 'dont-care]
                         #:kind [kind 'immutable]
                         #:lazy? [_lazy? (lazy-default kind _elem/c)])
  (define elem/c (coerce-contract 'set/c _elem/c))
  (define equal-key/c (coerce-contract 'set/c _equal-key/c))
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
        "element contract" elem/c
        "#:cmp option" cmp))]
    [else
     (unless (chaperone-contract? elem/c)
       (raise-argument-error 'set/c "chaperone-contract?" elem/c))])
  (cond
    [(and (eq? kind 'immutable)
          (not lazy?)
          (flat-contract? elem/c)
          (flat-contract? equal-key/c))
     (flat-set-contract elem/c equal-key/c cmp kind lazy?)]
    [(chaperone-contract? elem/c)
     (chaperone-set-contract elem/c equal-key/c cmp kind lazy?)]
    [else
     (impersonator-set-contract elem/c equal-key/c cmp kind lazy?)]))

(struct set-contract [elem/c equal-key/c cmp kind lazy?]
  #:property prop:custom-write contract-custom-write-property-proc)

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
  (define equal-key/c (set-contract-equal-key/c ctc))
  (define cmp (set-contract-cmp ctc))
  (define kind (set-contract-kind ctc))
  (define late-neg-ele-proj (contract-late-neg-projection elem/c))
  (define late-neg-equal-key-proj (contract-late-neg-projection equal-key/c))
  (define lazy? (set-contract-lazy? ctc))
  (λ (blame)
    (define ele-neg-blame (blame-add-element-context blame #t))
    (define late-neg-pos-proj (late-neg-ele-proj (blame-add-element-context blame #f)))
    (define late-neg-neg-proj (late-neg-ele-proj ele-neg-blame))
    (define late-neg-equal-key-pos-proj (late-neg-equal-key-proj ele-neg-blame))
    (cond
      [lazy?
       (λ (val neg-party)
         (set-contract-check cmp kind blame neg-party val)
         (define (pos-interpose val ele) (late-neg-pos-proj ele neg-party))
         (define blame+neg-party (cons blame neg-party))
         (cond
           [(set? val)
            (chaperone-hash-set
             val
             (λ (val ele) ele)
             (λ (val ele) ele)
             (λ (val ele) ele)
             (λ (val ele) (with-contract-continuation-mark
                           blame+neg-party
                           (late-neg-pos-proj ele neg-party)))
             (λ (val) (void))
             (λ (val ele) (with-contract-continuation-mark
                           blame+neg-party
                           (late-neg-equal-key-pos-proj ele neg-party)))
             impersonator-prop:contracted ctc
             impersonator-prop:blame (cons blame neg-party))]
           [else
            (chaperone-hash-set
             val
             (λ (val ele) ele)
             (λ (val ele) (with-contract-continuation-mark
                           blame+neg-party
                           (late-neg-neg-proj ele neg-party)))
             (λ (val ele) ele)
             (λ (val ele) (with-contract-continuation-mark
                           blame+neg-party
                           (late-neg-pos-proj ele neg-party)))
             (λ (val) (void))
             (λ (val ele) (with-contract-continuation-mark
                           blame+neg-party
                           (late-neg-equal-key-pos-proj ele neg-party)))
             impersonator-prop:contracted ctc
             impersonator-prop:blame (cons blame neg-party))]))]
      [else
       (λ (val neg-party)
         (define blame+neg-party (cons blame neg-party))
         (set-contract-check cmp kind blame neg-party val)
         (cond
           [(set? val)
            (chaperone-hash-set
             (for/fold ([s (set-clear val)])
                       ([e (in-set val)])
               (set-add s (with-contract-continuation-mark
                           blame+neg-party
                           (late-neg-pos-proj e neg-party))))
             #f #f #f
             impersonator-prop:contracted ctc
             impersonator-prop:blame (cons blame neg-party))]
           [else
            (for ([ele (in-list (set->list val))])
              (set-remove! val ele)
              (set-add! val (late-neg-pos-proj ele neg-party)))
            (chaperone-hash-set
             val
             (λ (val ele) ele)
             (λ (val ele) (with-contract-continuation-mark
                           blame+neg-party
                           (late-neg-neg-proj ele neg-party)))
             (λ (val ele) ele)
             (λ (val ele) (with-contract-continuation-mark
                           blame+neg-party
                           (late-neg-pos-proj ele neg-party)))
             (λ (val) (void))
             (λ (val ele) (with-contract-continuation-mark
                           blame+neg-party
                           (late-neg-equal-key-pos-proj ele neg-party)))
             impersonator-prop:contracted ctc
             impersonator-prop:blame (cons blame neg-party))]))])))

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

;; since the equal-key/c must be a flat contract
;; in order for the entire set/c to be a flat contract,
;; then we know that it doesn't have any negative blame
;; and thus can never fail; so this projection ignores it.
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
