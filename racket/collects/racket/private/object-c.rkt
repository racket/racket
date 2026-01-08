#lang racket/base
(require (for-syntax racket/base)
         racket/list
         (except-in "class-internal.rkt" class)
         (rename-in "class-internal.rkt" [class racket/class:class])
         "class-struct.rkt"
         (for-syntax "classidmap.rkt")
         "../contract/combinator.rkt"
         "../contract/base.rkt"
         "../contract/private/arrow-common.rkt"
         "../contract/private/kwd-info-struct.rkt"
         (only-in "../contract/private/prop.rkt" trust-me contract-struct-stronger?))
(provide (rename-out [-object/c object/c])
         dynamic-object/c
         make-instanceof/c-object/c
         set-instanceof/c-object/c-class/c!
         (for-syntax make-object/c-method-proc-stx do-object/c)
         make-object-contract
         sort-object/c-members)

(begin-for-syntax
  (struct opaque-stx (kwd-stx expr))
  (struct allow opaque-stx ())
  (struct disallow opaque-stx ())
  (define (handle-opaque form-name stx parsed-forms allow/disallow)
    (cond
      [(hash-ref parsed-forms 'opaque-expr #f)
       =>
       (λ (other-allow/disallow)
         (raise-syntax-error form-name
                             "expected only one #:opaque or #:opaque-except keyword"
                             stx
                             (opaque-stx-kwd-stx allow/disallow)
                             (list (opaque-stx-kwd-stx other-allow/disallow))))]
      [else
       (hash-set! parsed-forms 'opaque-expr allow/disallow)]))
  (define (handle-opaque-field form-name stx parsed-forms expr)
    (cond
      [(hash-ref parsed-forms 'opaque-field-expr #f)
       =>
       (λ (other)
         (raise-syntax-error form-name
                             "expected only one #:opaque-field keyword"
                             stx
                             expr
                             (list other)))]
      [else
       (hash-set! parsed-forms 'opaque-field-expr expr)])))

(define-for-syntax do-not-check-class-field-accessor-or-mutator-access-sym
  'do-not-check-class-field-accessor-or-mutator-access)

(define-for-syntax (parse-object/c-specs stx form-name allow-opaque? forms)
  (define pfs (make-hasheq))
  (define bindings '())
  (define (parse-name-ctc name-ctc)
    (syntax-case name-ctc ()
      [x
       (identifier? #'x)
       (with-syntax ([id (localize #'x)])
         (values #'id #f))]
      [(x ctc)
       (identifier? #'x)
       (with-syntax ([id (localize #'x)])
         (values #'id #'ctc))]
      [_
       (raise-syntax-error form-name "expected identifier or (id contract)" stx name-ctc)]))
  (define (parse-names-ctcs stx)
    (for/fold ([names null]
               [ctcs null])
              ([stx (in-list (syntax->list stx))])
      (let-values ([(name ctc) (parse-name-ctc stx)])
        (values (cons name names) (cons ctc ctcs)))))
  (define (parse-a-spec specs)
    (syntax-case specs (field)
      [(#:do-not-check-class-field-accessor-or-mutator-access)
       (hash-set! pfs do-not-check-class-field-accessor-or-mutator-access-sym #t)]
      [(#:opaque expr . more)
       allow-opaque?
       (begin
         (handle-opaque form-name stx pfs (allow (car (syntax->list #`#,specs)) #'expr))
         (parse-a-spec #'more))]
      [(#:opaque-except expr . more)
       allow-opaque?
       (begin
         (handle-opaque form-name stx pfs (disallow (car (syntax->list #`#,specs)) #'expr))
         (parse-a-spec #'more))]
      [(#:opaque-fields expr . more)
       allow-opaque?
       (begin
         (handle-opaque-field form-name stx pfs #'expr)
         (parse-a-spec #'more))]
      [(thing . more)
       (keyword? (syntax-e #'thing))
       (raise-syntax-error form-name "unexpected keyword" stx #'thing)]
      [((field f-spec ...) . more)
       (let-values ([(names ctcs) (parse-names-ctcs #'(f-spec ...))])
         (hash-set! pfs 'fields
                    (append names (hash-ref pfs 'fields null)))
         (hash-set! pfs 'field-contracts
                    (append (add-bindings/return-vars ctcs)
                            (hash-ref pfs 'field-contracts null)))
         (parse-a-spec #'more))]
      [(m-spec . more)
       (let-values ([(name ctc1) (parse-name-ctc #'m-spec)])
         (hash-set! pfs 'methods
                    (cons name (hash-ref pfs 'methods null)))
         (hash-set! pfs 'method-contracts
                    (append (add-bindings/return-vars (list ctc1))
                            (hash-ref pfs 'method-contracts null)))
         (parse-a-spec #'more))]
      [()
       (void)]
      [(thing . more)
       (raise-syntax-error form-name
                           (format "expected ~a subform" form-name)
                           stx #'thing)]))
  
  (define (add-bindings/return-vars ctcs)
    (for/list ([ctc (in-list ctcs)])
      (and ctc
           (with-syntax ([(ctc-x) (generate-temporaries (list ctc))])
             (set! bindings (cons #`[ctc-x #,ctc] bindings))
             #'ctc-x))))
  (parse-a-spec forms)
  (combine-same-named-member-names pfs
                                   'methods
                                   'method-contracts)
  (combine-same-named-member-names pfs
                                   'fields
                                   'field-contracts)
  (values (reverse bindings) pfs))

(define-for-syntax (combine-same-named-member-names pfs names-key expressions-key)
  (define (split-names name names-in expressions-in)
    (for/fold ([names-out '()]
               [expressions-out '()]
               [expressions-with-matching-names '()]
               #:result (values (reverse expressions-out)
                                (reverse names-out)
                                expressions-with-matching-names))
              ([name-in (in-list names-in)]
               [expression-in (in-list expressions-in)])
      (cond
        [(refers-to-same-member-name? name-in name)
         (values names-out expressions-out
                 (cons expression-in expressions-with-matching-names))]
        [else
         (values (cons name-in names-out)
                 (cons expression-in expressions-out)
                 expressions-with-matching-names)])))
  (let loop ([names-in (hash-ref pfs names-key '())]
             [expressions-in (hash-ref pfs expressions-key '())]
             [names-out '()]
             [expressions-out '()])
    (cond
      [(null? names-in)
       (hash-set! pfs names-key (reverse names-out))
       (hash-set! pfs expressions-key (reverse expressions-out))]
      [else
       (define name (car names-in))
       (define exp (car expressions-in))
       (define-values (other-names other-expressions matching-expressions)
         (split-names name (cdr names-in) (cdr expressions-in)))
       (cond
         [(null? matching-expressions)
          (loop (cdr names-in) (cdr expressions-in)
                (cons name names-out) (cons exp expressions-out))]
         [else
          (loop other-names other-expressions
                (cons name names-out)
                (cons #`(and/c #,exp #,@matching-expressions)
                      expressions-out))])])))

;; make-object/c-method-proc-stx : (listof (or/c identifier? syntax?)) (listof (or/c #f syntax?)) -> syntax?
;; If an element of the first input list is an identifier, then that is a regular method and
;;   if it is something else then it is a method bound with define-method-name
;; The second list are the contracts; if an element is #f, then that corresponds to
;;   a position where the contract just wants the method to exist but doesn't ask
;;   for it to be a particular shape.
;; The order in the two input lists must match and also they match the order in the original program
;;
;; The result is the syntax of a function that accepts the method interposition procs
;;   and returns a function that accepts a method name and selects the appropriate
;;   interposition proc.
(define-for-syntax (make-object/c-method-proc-stx method-names method-contracts)
  (with-syntax ([(all-method-ctc-x ...)
                 (generate-temporaries
                  (for/list ([method-stx (in-list method-contracts)]
                             #:when method-stx)
                    method-stx))])
    (with-syntax ([((non-localized-method-name non-localized-ctc-x) ...)
                   (for/list ([ctc-x (in-list (syntax->list #'(all-method-ctc-x ...)))]
                              [method-name (in-list method-names)]
                              [method-stx (in-list method-contracts)]
                              #:when method-stx
                              #:when (identifier? method-name))
                     (list method-name ctc-x))]
                  [((localized-method-name localized-ctc-x) ...)
                   (for/list ([ctc-x (in-list (syntax->list #'(all-method-ctc-x ...)))]
                              [method-name (in-list method-names)]
                              [method-stx (in-list method-contracts)]
                              #:when method-stx
                              #:unless (identifier? method-name))
                     (list method-name ctc-x))])
      #`(λ (all-method-ctc-x ...)
          (λ (incoming-method-name)
            (case incoming-method-name
              [(non-localized-method-name) non-localized-ctc-x] ...
              [else
               (cond
                 [(equal? `localized-method-name incoming-method-name)
                  localized-ctc-x]
                 ...
                 [else #f])]))))))

(define-syntax (-object/c stx)
  (syntax-case stx ()
    [(_ . form)
     (do-object/c 'object/c stx #t #'make-object/c #'form)]))

(define-for-syntax (do-object/c form-name stx allow-opaque? constructor forms)
  (syntax-case forms ()
    [(form ...)
     (let ()
       (define-values (bindings pfs)
         (parse-object/c-specs stx form-name allow-opaque? (syntax->list #'(form ...))))
       (with-syntax ([(methods ...) (reverse (hash-ref pfs 'methods null))]
                     [(method-ctcs ...) (for/list ([method-stx (in-list (reverse (hash-ref pfs 'method-contracts null)))])
                                          (or method-stx
                                              #'just-check-existence))]
                     [(fields ...) (reverse (hash-ref pfs 'fields null))]
                     [(field-ctcs ...) (for/list ([field-stx (in-list (reverse (hash-ref pfs 'field-contracts null)))])
                                         (or field-stx
                                             #'just-check-existence))]
                     [bindings bindings])
         (quasisyntax/loc stx
           (let bindings
             (build-object/c '#,form-name
                             #,constructor
                             (list `methods ...)
                             (list method-ctcs ...)
                             #,(make-object/c-method-proc-stx (reverse (hash-ref pfs 'methods null))
                                                              (reverse (hash-ref pfs 'method-contracts null)))
                             (list `fields ...)
                             (list field-ctcs ...)
                             #,(cond
                                 [(allow? (hash-ref pfs 'opaque-expr #f))
                                  #''allow]
                                 [(disallow? (hash-ref pfs 'opaque-expr #f))
                                  #''disallow]
                                 [else #f])
                             #,(cond
                                 [(hash-ref pfs 'opaque-expr #f)
                                  =>
                                  (λ (opq) (opaque-stx-expr opq))]
                                 [else #'#f])
                             #,(hash-has-key? pfs 'opaque-field-expr)
                             #,(hash-ref pfs 'opaque-field-expr #f)
                             #,(hash-ref pfs do-not-check-class-field-accessor-or-mutator-access-sym #f))))))]))

(define (build-object/c form-name constructor methods raw-method-ctcs make-object/c-method-proc fields field-ctcs
                        opaque-kind opaque-val opaque-field-specified? opaque-field-val
                        do-not-check-class-field-accessor-or-mutator-access)
  (define opaque-methods
    (cond
      [(equal? opaque-kind 'allow)
       (unless (or (boolean? opaque-val)
                   (impersonator-property-predicate-procedure? opaque-val))
         (raise-argument-error form-name
                               (format "~s" '(or/c boolean? impersonator-property-predicate-procedure?))
                               opaque-val))
       (if (boolean? opaque-val)
           opaque-val
           (allowed-opaque opaque-val))]
      [(equal? opaque-kind 'disallow)
       (unless (impersonator-property-predicate-procedure? opaque-val)
         (raise-argument-error form-name
                               (format "~s" 'impersonator-property-predicate-procedure?)
                               opaque-val))
       opaque-val]
      [else #f]))
  (define opaque-fields
    (cond
      [opaque-field-specified?
       (unless (boolean? opaque-field-val)
         (raise-argument-error form-name "boolean?" opaque-field-val))
       opaque-field-val]
      [else
       (cond
         [(procedure? opaque-methods) (equal? opaque-kind 'allow)]
         [else opaque-methods])]))
  (define unsorted-method-ctcs
    (for/list ([method-ctc (in-list raw-method-ctcs)])
      (cond
        [(just-check-existence? method-ctc) method-ctc]
        [else (coerce-contract 'object/c method-ctc)])))
  (define-values (sorted-methods sorted-method-ctcs)
    (sort-object/c-members methods unsorted-method-ctcs))
  (define-values (sorted-fields sorted-field-ctcs)
    (sort-object/c-members fields field-ctcs))
  (constructor
   sorted-methods
   sorted-method-ctcs
   make-object/c-method-proc
   unsorted-method-ctcs
   methods
   sorted-fields
   (for/list ([field-ctc (in-list sorted-field-ctcs)])
     (cond [(just-check-existence? field-ctc) field-ctc]
           [else (coerce-contract 'object/c field-ctc)]))
   fields
   opaque-methods
   opaque-fields
   do-not-check-class-field-accessor-or-mutator-access))

(define (sort-object/c-members names ctcs)
  (define sorted (sort (map cons names ctcs) symbol<? #:key car))
  (values (map car sorted) (map cdr sorted)))

(define (check-object-contract obj
                               method-names method-first-order-checks method-contracts
                               field-names
                               blame neg-party)
  (let/ec escape
    (unless (object? obj)
      (if blame
          (raise-blame-error blame #:missing-party neg-party
                             obj
                             '(expected: "an object" given: "~e") obj)
          (escape #f)))
    (define cls (object-ref/unwrap obj))

    (define method-ht (class-method-ht cls))
    (define methods (class-methods cls))
    (for ([m (in-list method-names)]
          [method-first-order-check (in-list method-first-order-checks)]
          [method-contract (in-list method-contracts)])
      (define mtd-index (hash-ref method-ht m #f))
      (unless mtd-index
        (if blame
            (raise-blame-error blame #:missing-party neg-party
                               obj
                               '(expected: "an object with the ~a method" given: "~e\n~a")
                               m
                               obj
                               (names-as-strings "method" (hash-keys method-ht)))
            (escape #f)))
      (define mtd (vector-ref methods mtd-index))
      (when method-first-order-check
        (unless (method-first-order-check mtd)
          (if blame
              ;; this will signal the contract violation;
              ;; we check method-first-order-check first as its a lot faster
              (((contract-late-neg-projection method-contract) blame) mtd neg-party)
              (escape #f)))))

    (define field-ht (class-field-ht cls))
    (for ([f (in-list field-names)])
      (unless (hash-ref field-ht f #f)
        (if blame
            (raise-blame-error blame #:missing-party neg-party
                               obj
                               '(expected: "an object with the ~a field" given: "~e\n~a")
                               f
                               obj
                               (names-as-strings "field" (hash-keys field-ht)))
            (escape #f))))
    #t))

(define (names-as-strings what names)
  (cond
    [(empty? names) (format "  ~as: none" what)]
    [(empty? (cdr names)) (format "  method: ~a" (car names))]
    [else
     (apply
      string-append
      (format "  ~as: " what)
      (add-between
       (for/list ([name (in-list (sort names symbol<?))])
         (symbol->string name))
       " "))]))

(define (object/c-first-order ctc)
  (define unsorted-methods-names (base-object/c-unsorted-method-names ctc))
  (define unsorted-method-contracts (base-object/c-unsorted-method-contracts ctc))
  (define unsorted-method-first-order-checks
    (for/list ([ctc (in-list unsorted-method-contracts)])
      (if (just-check-existence? ctc)
          #f
          (contract-first-order ctc))))
  (define fields (base-object/c-fields ctc))
  (define field-contracts (base-object/c-fields ctc))
  (define field-first-order-checks (map contract-first-order field-contracts))
  (λ (obj)
    (check-object-contract obj
                           unsorted-methods-names unsorted-method-first-order-checks unsorted-method-contracts
                           fields
                           #f #f)))

(define (object/c-equivalent this that)
  (cond
    [(base-object/c? that)
     (and (equal? (base-object/c-methods that)
                  (base-object/c-methods this))
          (equal? (base-object/c-fields that)
                  (base-object/c-fields this))
          (for/and ([this-ctc (base-object/c-method-contracts this)]
                    [that-ctc (base-object/c-method-contracts that)])
            (contract-equivalent? this-ctc that-ctc))
          (for/and ([this-ctc (base-object/c-field-contracts this)]
                    [that-ctc (base-object/c-field-contracts that)])
            (contract-equivalent? this-ctc that-ctc))
          (equal? (base-object/c-opaque-methods this)
                  (base-object/c-opaque-methods that))
          (equal? (base-object/c-opaque-fields this)
                  (base-object/c-opaque-fields that))
          (equal? (base-object/c-do-not-check-class-field-accessor-or-mutator-access? this)
                  (base-object/c-do-not-check-class-field-accessor-or-mutator-access? that)))]
    [else #f]))


(define (object/c-stronger this that)
  (cond
    [(base-object/c? that)
     (and (object/c-methods-stronger? this that)
          (object/c-fields-stronger? this that))]
    [else #f]))

(define (object/c-methods-stronger? this that)
  (define this-opaque-methods (base-object/c-opaque-methods this))
  (define that-opaque-methods (base-object/c-opaque-methods that))
  (cond
    [(or (procedure? this-opaque-methods)
         (allowed-opaque? this-opaque-methods)
         (procedure? that-opaque-methods)
         (allowed-opaque? that-opaque-methods))
     ;; this might be too restrictive; if that-opaque-methods is a procedure
     ;; and this-opaque-methods is #t then it might be possible to allow
     ;; some subsetting on the set of methods, but we'll be conservative for now.
     (and (equal? this-opaque-methods (base-object/c-opaque-methods that))
          (equal? (base-object/c-methods that) (base-object/c-methods this))
          (for/and ([this-ctc (in-list (base-object/c-method-contracts this))]
                    [that-ctc (in-list (base-object/c-method-contracts that))])
            (contract-struct-stronger? this-ctc that-ctc)))]
    [else
     (let loop ([this-methods (base-object/c-methods this)]
                [that-methods (base-object/c-methods that)]
                [this-ctcs (base-object/c-method-contracts this)]
                [that-ctcs (base-object/c-method-contracts that)])
       (cond
         [(and (null? this-methods) (null? that-methods))
          #t]
         [(and (pair? this-methods) (pair? that-methods))
          (cond
            [(equal? (car this-methods) (car that-methods))
             (and (contract-struct-stronger? (car this-ctcs) (car that-ctcs))
                  (loop (cdr this-methods) (cdr that-methods)
                        (cdr this-ctcs) (cdr that-ctcs)))]
            [(symbol<? (car this-methods) (car that-methods))
             (and (contract-struct-stronger? (car this-ctcs) (if that-opaque-methods none/c any/c))
                  (loop (cdr this-methods) that-methods
                        (cdr this-ctcs) that-ctcs))]
            [else
             (and (contract-struct-stronger? (if this-opaque-methods none/c any/c) (car that-ctcs))
                  (loop this-methods (cdr that-methods)
                        this-ctcs (cdr that-ctcs)))])]
         [(and (pair? this-methods) (null? that-methods))
          ;; ideally, this should really be in parallel to the case below, namely:
          #; (contract-struct-stronger? (car this-ctcs) (if that-opaque-methods none/c any/c))
          ;; but contract stronger does cases on the first argument and that first argument
          ;; may not have a case for any/c, so we lift out the `if` and just return #t
          (and (if that-opaque-methods
                   (contract-struct-stronger? (car this-ctcs) none/c)
                   #t)
               (loop (cdr this-methods) that-methods
                     (cdr this-ctcs) that-ctcs))]
         [(and (null? this-methods) (pair? that-methods))
          (and (contract-struct-stronger? (if this-opaque-methods none/c any/c) (car that-ctcs))
               (loop this-methods (cdr that-methods)
                     this-ctcs (cdr that-ctcs)))]))]))

(define (object/c-fields-stronger? this that)
  ;; check both ways for fields (since mutable)
  (and (equal? (base-object/c-opaque-fields this) (base-object/c-opaque-fields that))
       (equal? (base-object/c-fields this) (base-object/c-fields that))
       (check-one-object/common-names base-object/c-fields base-object/c-field-contracts
                                      this that contract-equivalent?)))

;; Extract names (using `names-sel`) and contracts (`ctcs-sel`) from objects `this` and `that`.
;; For all contracts with the same name, compare the contracts using `compare-ctcs`.
(define (check-one-object/common-names names-sel ctcs-sel this that compare-ctcs)
  (for/and ([this-name (in-list (names-sel this))]
            [this-ctc (in-list (ctcs-sel this))])
    (or (not (member this-name (names-sel that)))
        (for/or ([that-name (in-list (names-sel that))]
                 [that-ctc (in-list (ctcs-sel that))])
          (and (equal? this-name that-name)
               (compare-ctcs
                (if (just-check-existence? this-ctc)
                    any/c
                    this-ctc)
                (if (just-check-existence? that-ctc)
                    any/c
                    that-ctc)))))))

(define-logger racket/contract)

(define object/c-late-neg-proj
  (λ (ctc)
    (define unsorted-method-names (base-object/c-unsorted-method-names ctc))
    (define unsorted-method-contracts (base-object/c-unsorted-method-contracts ctc))
    (define blame-acceptors
      (for/list ([m-ctc (in-list unsorted-method-contracts)])
        (and (not (just-check-existence? m-ctc))
             (contract-late-neg-projection m-ctc))))
    (define unsorted-method-first-order-checks
      (for/list ([ctc (in-list unsorted-method-contracts)])
        (if (just-check-existence? ctc)
            #f
            (contract-first-order ctc))))
    (define fields (base-object/c-fields ctc))
    (define field-contracts (base-object/c-field-contracts ctc))
    (define opaque-methods (base-object/c-opaque-methods ctc))
    (define opaque-fields (base-object/c-opaque-fields ctc))
    (define do-not-check-class-field-accessor-or-mutator-access?
      (base-object/c-do-not-check-class-field-accessor-or-mutator-access?
       ctc))
    (λ (blame)
      (define wrapper-methods
        (for/list ([m-ctc (in-list unsorted-method-contracts)]
                   [m-name (in-list unsorted-method-names)]
                   [blame-acceptor (in-list blame-acceptors)]
                   #:when (and m-ctc (not (just-check-existence? m-ctc))))
          (define blame* (blame-add-context blame (format "the ~a method in" m-name)
                                            #:important m-name))
          (cond
            [(and (base->? m-ctc) (base->-object/c-wrapper m-ctc))
             =>
             (λ (builder)
               (define swapped-blame* (blame-swap blame*))
               (define (get-lnp x context) ((contract-late-neg-projection x) (blame-add-context blame* context)))
               (define (get-lnp-swap x context) ((contract-late-neg-projection x) (blame-add-context blame* context #:swap? #t)))
               (apply builder m-name blame*
                      (append (for/list ([dom (in-list (base->-doms m-ctc))]
                                         [i (in-naturals)])
                                (get-lnp-swap dom (format "the ~a argument of" (n->th i))))
                              (for/list ([a-kwd-info (in-list (base->-kwd-infos m-ctc))]
                                         #:when (kwd-info-mandatory? a-kwd-info))
                                (get-lnp-swap (kwd-info-ctc a-kwd-info) (format "the ~a argument of" (kwd-info-kwd a-kwd-info))))
                              (for/list ([a-kwd-info (in-list (base->-kwd-infos m-ctc))]
                                         #:unless (kwd-info-mandatory? a-kwd-info))
                                (get-lnp-swap (kwd-info-ctc a-kwd-info) (format "the ~a argument of" (kwd-info-kwd a-kwd-info))))
                              (if (base->-rest m-ctc)
                                  (list (get-lnp-swap (base->-rest m-ctc) "the rest argument of"))
                                  '())
                              (cond
                                [(base->-rngs m-ctc)
                                 (define rngs (base->-rngs m-ctc))
                                 (if (and (pair? rngs)
                                          (null? (cdr rngs)))
                                     (list (get-lnp (car rngs) "the range of"))
                                     (for/list ([rng (in-list rngs)]
                                                [i (in-naturals)])
                                       (get-lnp rng (format "the ~a range of" (n->th i)))))]
                                [else '()]))))]
            [else
             (define lnp (blame-acceptor blame*))
             (log-racket/contract-info "no object/c-wrapper on method: ~a; contract: ~s" m-name m-ctc)
             (make-forward-proc lnp m-name)])))
      (define methods-proc (apply (base-object/c-make-methods-proc ctc) wrapper-methods))
      (define-values (filled? maybe-pos-fields maybe-neg-fields)
        (contract-pos/neg-doubling
         (for/hash ([f (in-list fields)]
                    [c (in-list field-contracts)]
                    #:unless (just-check-existence? c))
           (define prj (contract-late-neg-projection c))
           (values f (prj (blame-add-field-context blame f #:swap? #f))))
         (for/hash ([f (in-list fields)]
                    [c (in-list field-contracts)]
                    #:unless (just-check-existence? c))
           (define prj (contract-late-neg-projection c))
           (values f (prj (blame-add-field-context blame f #:swap? #t))))))
      (define tc (and (not filled?) (make-thread-cell #f)))
      (λ (val neg-party)
        (check-object-contract val
                               unsorted-method-names unsorted-method-first-order-checks unsorted-method-contracts
                               fields
                               blame neg-party)
        (define-values (pos-fields neg-fields)
          (cond
            [filled? (values maybe-pos-fields maybe-neg-fields)]
            [(thread-cell-ref tc)
             =>
             (λ (pr) (values (car pr) (cdr pr)))]
            [else
             (define pos-fields (maybe-pos-fields))
             (define neg-fields (maybe-neg-fields))
             (thread-cell-set! tc (cons pos-fields neg-fields))
             (values pos-fields neg-fields)]))
        ;; it may be the case that it is a good idea to look at `val`
        ;; and extract the method table indicies and somehow supply
        ;; them to `methods-proc` instead of doing those lookups when
        ;; a method is invoked, as if baking a generic method call
        ;; into the wrapper

        (define already-got (find-same-ctc+blame val ctc blame neg-party))
        (cond
          [already-got
           already-got]
          [else
           (define blame+neg-party (cons blame neg-party))
           (define wrapper-info (object/c-wrapper-info val methods-proc blame+neg-party pos-fields neg-fields
                                                       opaque-methods opaque-fields ctc
                                                       do-not-check-class-field-accessor-or-mutator-access?))
           (impersonate-struct
            val
            object-ref
            (λ (self val) wrapper-info)
            impersonator-prop:contracted ctc)])))))

(define (find-same-ctc+blame val ctc blame neg-party)
  (let loop ([o val])
    (define cls-or-object/c-wrapper-info (object-ref o))
    (cond
      [(class? cls-or-object/c-wrapper-info)
       #f]
      [else
       (cond
         [(or (eq? ctc (object/c-wrapper-info-ctc cls-or-object/c-wrapper-info))
              (contract-equivalent? ctc (object/c-wrapper-info-ctc cls-or-object/c-wrapper-info)))
          (define blame+neg-party (object/c-wrapper-info-blame+neg-party cls-or-object/c-wrapper-info))
          (cond
            [(and (eq? neg-party (cdr blame+neg-party))
                  (eq? blame (car blame+neg-party)))
             o]
            [else (loop (object/c-wrapper-info-val cls-or-object/c-wrapper-info))])]
         [else
          #f])])))

(define (wrapper-depth o)
  (let loop ([o o]
             [n 0])
    (define cls-or-object/c-wrapper-info (object-ref o))
    (cond
      [(class? cls-or-object/c-wrapper-info)
       n]
      [else
       (loop
        (object/c-wrapper-info-val
         cls-or-object/c-wrapper-info)
        (+ n 1))])))

(define (blame-add-field-context blame f #:swap? swap?)
  (blame-add-context blame (format "the ~a field in" f) #:swap? swap?))

(define (make-forward-proc lnp method-name)
  (make-keyword-procedure
   (λ (kwds kwd-vals this . args)
     (define an-object/c-wrapper-info (object-ref this))
     (define unwrapped (object/c-wrapper-info-val an-object/c-wrapper-info))
     (define blame+neg-party (object/c-wrapper-info-blame+neg-party an-object/c-wrapper-info))
     (define neg-party (cdr blame+neg-party))
     (define meth (find-method/who 'slow-path-in-object/c-should-never-fail.1 unwrapped method-name))
     (keyword-apply (lnp meth neg-party) kwds kwd-vals unwrapped args))
   (λ (this . args)
     (define an-object/c-wrapper-info (object-ref this))
     (define unwrapped (object/c-wrapper-info-val an-object/c-wrapper-info))
     (define blame+neg-party (object/c-wrapper-info-blame+neg-party an-object/c-wrapper-info))
     (define neg-party (cdr blame+neg-party))
     (define meth (find-method/who 'slow-path-in-object/c-should-never-fail.2 unwrapped method-name))
     (apply (lnp meth neg-party) unwrapped args))))

;; methods : (listof symbol?), in symbol<? order
;; method-contracts : (listof contact?), in an order matching `methods`
;; make-methods-proc : contract ... -> symbol -> contract
;; unsorted-method-contracts : (listof contact?), same as method-contracts,
;;    but in the order specified in the original contract syntax; used
;;    to build the arugment to make-methods-proc
;; original-method-order : (listof symbol?), in the order the original contract syntax
;; fields : (listof symbol?), in symbol<? order
;; field-contract : (listof contact?), in an order matching `fields`
;; original-field-order : (listof symbol?), in the order the original contract syntax

(define-struct base-object/c (methods method-contracts make-methods-proc unsorted-method-contracts unsorted-method-names
                              fields field-contracts original-field-order
                              opaque-methods opaque-fields
                              do-not-check-class-field-accessor-or-mutator-access?)
  #:constructor-name NEVER_CALL_THIS)

(define-struct (object/c base-object/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:trusted trust-me
   #:late-neg-projection object/c-late-neg-proj
   #:name
   (λ (ctc)
     (build-object/c-type-name
      'object/c
      (base-object/c-methods ctc)
      (base-object/c-method-contracts ctc)
      (base-object/c-unsorted-method-names ctc)
      (base-object/c-fields ctc)
      (base-object/c-field-contracts ctc)
      (base-object/c-original-field-order ctc)
      (base-object/c-opaque-methods ctc)
      (base-object/c-opaque-fields ctc)
      (base-object/c-do-not-check-class-field-accessor-or-mutator-access? ctc)))
   #:first-order object/c-first-order
   #:equivalent object/c-equivalent
   #:stronger object/c-stronger))

(define-struct (object-contract base-object/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:trusted trust-me
   #:late-neg-projection object/c-late-neg-proj
   #:name
   (λ (ctc)
     (build-object-contract-type-name (base-object/c-methods ctc)
                                      (base-object/c-method-contracts ctc)
                                      (base-object/c-unsorted-method-names ctc)
                                      (base-object/c-fields ctc)
                                      (base-object/c-field-contracts ctc)
                                      (base-object/c-original-field-order ctc)))
   #:first-order object/c-first-order
   #:equivalent object/c-equivalent
   #:stronger object/c-stronger))


(define-struct (instanceof/c-object/c base-object/c) ([class/c #:mutable])
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:trusted trust-me
   #:late-neg-projection object/c-late-neg-proj
   #:name
   (λ (ctc)
     (list 'instanceof/c (contract-name (instanceof/c-object/c-class/c ctc))))
   #:first-order object/c-first-order
   #:equivalent object/c-equivalent
   #:stronger object/c-stronger))

(struct dynamic-object/c base-object/c ()
  #:constructor-name make-dynamic-object/c
  #:name dynamic-object/c-struct
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:trusted trust-me
   #:late-neg-projection object/c-late-neg-proj
   #:name
   (λ (ctc)
     (build-dynamic-object/c-type-name
      (base-object/c-methods ctc)
      (base-object/c-method-contracts ctc)
      (base-object/c-unsorted-method-names ctc)
      (base-object/c-fields ctc)
      (base-object/c-field-contracts ctc)
      (base-object/c-original-field-order ctc)))
   #:first-order object/c-first-order
   #:equivalent object/c-equivalent
   #:stronger object/c-stronger))

(define (build-object/c-type-name name method-names method-ctcs original-method-order
                                  field-names field-ctcs original-field-order
                                  opaque-methods opaque-fields
                                  do-not-check-class-field-accessor-or-mutator-access?)
  (apply build-compound-type-name
         name
         (append
          (pair-ids-ctcs method-names method-ctcs original-method-order)
          (handle-optional 'field field-names field-ctcs original-field-order)
          (cond
            [(allowed-opaque? opaque-methods)
             (list '#:opaque (object-name (allowed-opaque-pred opaque-methods)))]
            [(procedure? opaque-methods)
             (list '#:opaque-except (object-name opaque-methods))]
            [opaque-methods
             (list '#:opaque #t)]
            [else '()])
          (cond
            [(and (boolean? opaque-methods)
                  (equal? opaque-methods opaque-fields))
             '()]
            [(and (procedure? opaque-methods)
                  (not opaque-fields))
             '()]
            [(and (allowed-opaque? opaque-methods)
                  opaque-fields)
             '()]
            [opaque-fields
             (list '#:opaque-fields #t)]
            [else
             (list '#:opaque-fields #f)])
          (if do-not-check-class-field-accessor-or-mutator-access?
              (list '#:do-not-check-class-field-accessor-or-mutator-access)
              '())
          )))

(define (pair-ids-ctcs ids ctcs original-order)
  (define (find-ctc name)
    (for/first ([id (in-list ids)]
                [ctc (in-list ctcs)]
                #:when (equal? name id))
      ctc))
  (for/list ([i (in-list original-order)])
    (define ctc (find-ctc i))
    (cond
      [(just-check-existence? ctc)
       i]
      [else
       (build-compound-type-name i ctc)])))

(define (handle-optional name is ctcs original-order)
  (if (null? is)
      null
      (list (cons name (pair-ids-ctcs is ctcs original-order)))))

(define (build-object-contract-type-name method-names method-ctcs original-method-order
                                         field-names field-ctcs original-field-order)
  (define (rewrite->m sexp)
    (define id (list-ref sexp 0))
    (define ctc (list-ref sexp 1))
    (list id
          (cond
            [(and (pair? ctc)
                  (symbol? (car ctc)))
             (case (car ctc)
               ['->m (cons '-> (cdr ctc))]
               ['->*m (cons '->* (cdr ctc))]
               ['case->m (cons 'case-> (cdr ctc))]
               ['->dm (cons '->d (cdr ctc))]
               [else ctc])]
            [else ctc])))
  (define mtd-portion
    (for/list ([i (in-list original-method-order)])
      (define ctc
        (for/first ([name (in-list method-names)]
                    [ctc (in-list method-ctcs)]
                    #:when (equal? i name))
          ctc))
      (cond
        [(just-check-existence? ctc)
         i]
        [else
         (rewrite->m (build-compound-type-name i ctc))])))
  (define fld-portion
    (for/list ([i (in-list original-field-order)])
      (define ctc
        (for/first ([name (in-list field-names)]
                    [ctc (in-list field-ctcs)]
                    #:when (equal? i name))
          ctc))
      (cond
        [(just-check-existence? ctc)
         i]
        [else
         (build-compound-type-name 'field i ctc)])))
  (apply build-compound-type-name
         'object-contract
         (append
          mtd-portion
          fld-portion)))

(define (build-dynamic-object/c-type-name method-names method-ctcs original-method-order
                                          field-names field-ctcs original-field-order)
  (build-compound-type-name
   'dynamic-object/c
   `'(,@original-method-order)
   (apply build-compound-type-name
          'list
          (for/list ([i (in-list original-method-order)])
            (for/first ([name (in-list method-names)]
                        [ctc (in-list method-ctcs)]
                        #:when (equal? i name))
              ctc)))
   `'(,@original-field-order)
   (apply build-compound-type-name
          'list
          (for/list ([i (in-list original-field-order)])
            (for/first ([name (in-list field-names)]
                        [ctc (in-list field-ctcs)]
                        #:when (equal? i name))
              ctc)))))


;; dynamic-object/c : Listof<Symbol> Listof<Contract>
;;                    Listof<Symbol> Listof<Contract>
;;                    -> Contract
;; An external constructor provided in order to allow runtime
;; construction of object contracts by libraries that want to
;; implement their own object contract variants
(define (dynamic-object/c method-names/dups method-contracts/dups
                          field-names/dups field-contracts/dups)
  (define (ensure-symbols names)
    (unless (and (list? names) (andmap symbol? names))
      (raise-argument-error 'dynamic-object/c "(listof symbol?)" names)))
  (define (ensure-length names ctcs)
    (unless (= (length names) (length ctcs))
      (raise-arguments-error 'dynamic-object/c
                             "expected the same number of names and contracts"
                             "names" names
                             "contracts" ctcs)))
  (ensure-symbols method-names/dups)
  (ensure-length method-names/dups method-contracts/dups)
  (ensure-symbols field-names/dups)
  (ensure-length field-names/dups field-contracts/dups)
  (define coerced-method-ctcs
    (for/list ([ctc (in-list method-contracts/dups)])
      (coerce-contract 'dynamic-object/c ctc)))
  (define coerced-field-ctcs
    (for/list ([ctc (in-list field-contracts/dups)])
      (coerce-contract 'dynamic-object/c ctc)))
  (define-values (field-names field-contracts)
    (combine-dynamic-duplicates field-names/dups coerced-field-ctcs))
  (define-values (method-names method-contracts)
    (combine-dynamic-duplicates method-names/dups coerced-method-ctcs))
  (define-values (sorted-method-names sorted-method-contracts)
    (sort-object/c-members method-names method-contracts))
  (define-values (sorted-field-names sorted-field-contracts)
    (sort-object/c-members field-names field-contracts))
  (make-dynamic-object/c sorted-method-names
                         sorted-method-contracts
                         (λ lnps
                           (define methods-ht (make-hash))
                           (for ([method-name (in-list method-names/dups)]
                                 [lnp (in-list lnps)])
                             (hash-set! methods-ht method-name lnp))
                           (λ (incoming-method-name)
                             (hash-ref methods-ht incoming-method-name #f)))
                         method-contracts
                         method-names
                         sorted-field-names
                         sorted-field-contracts
                         field-names
                         #f
                         #f
                         #f))

(define (combine-dynamic-duplicates names contracts)
  (define ht (make-hash))
  (define skips (make-hash))
  (for ([name (in-list names)]
        [contract (in-list contracts)]
        [i (in-naturals)])
    (define old (hash-ref ht name '()))
    (hash-set! skips i (pair? old))
    (hash-set! ht name (cons contract (hash-ref ht name '()))))
  (define names+contracts
    (for/list ([name (in-list names)]
               [i (in-naturals)]
               #:unless (hash-ref skips i))
      (define ctc (hash-ref ht name))
      (cons name (if (and (pair? ctc) (null? (cdr ctc)))
                     (car ctc)
                     (apply and/c ctc)))))
  (values (map car names+contracts)
          (map cdr names+contracts)))
