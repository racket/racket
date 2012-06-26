#lang racket/base
(require racket/local
         racket/contract/base
         racket/contract/combinator
         (for-syntax racket/base
                     racket/local
                     racket/syntax)
         (only-in "define-struct.rkt" define/generic))

(define-for-syntax (keyword-stx? v)
  (keyword? (syntax->datum v)))

(provide define-generics define/generic)
(define-syntax (define-generics stx)
  (syntax-case stx () ; can't use syntax-parse, since it depends on us
    ;; keyword arguments must _all_ be provided _in_order_. For the
    ;; user-facing version of `define-generics', see racket/generic.
    ;;
    ;; The `header` is the original name the library writer provides
    ;; that is used to define the `name`, `prop:name`, and `name?`
    ;; identifiers. We have it here so that we can use it to match
    ;; the method header's self argument.
    [(_ (header name prop:name name?
                #:defined-table defined-table
                ;; are we being passed an existing struct property? If so,
                ;; this kw arg is bound to the struct property accessor, and
                ;; we don't define the struct property
                #:prop-defined-already? defined-already?)
        (generic . generic-args) ...)
     (and (identifier? #'header)
          (identifier? #'name)
          (identifier? #'prop:name)
          (identifier? #'name?)
          (identifier? #'defined-table)
          (let ([generics (syntax->list #'(generic ...))])
            (and (pair? generics) (andmap identifier? generics))))
     (let* ([idxs (for/list ([i (in-naturals 0)]
                             [_ (syntax->list #'(generic ...))])
                    i)]
            [name-str (symbol->string (syntax-e #'name?))]
            [generics (syntax->list #'(generic ...))]
            [prop-defined-already? (syntax-e #'defined-already?)])
       (with-syntax ([name-str name-str]
                     [how-many-generics (length idxs)]
                     [(generic-arity-coerce ...) (generate-temporaries #'(generic ...))]
                     [(generic-idx ...) idxs]
                     [(generic-this-idx ...)
                      (for/list ([top-ga (syntax->list #'(generic-args ...))])
                        (let loop ([ga top-ga]
                                   [i 0])
                          (syntax-case ga ()
                            [(keyword id . ga)
                             (and (keyword-stx? #'keyword)
                                  (identifier? #'id))
                             (loop #'ga i)]
                            [(id . ga)
                             (and (identifier? #'id))
                             (if (free-identifier=? #'header #'id)
                                 i
                                 (loop #'ga (add1 i)))]
                            [(keyword [id] . ga)
                             (and (keyword-stx? #'keyword)
                                  (identifier? #'id))
                             (loop #'ga i)]
                            [([id] . ga)
                             (and (identifier? #'id))
                             (loop #'ga i)]
                            [_
                             (identifier? #'id)
                             (raise-syntax-error #f "No required by-position generic argument" top-ga)])))]
                     [(fake-args ...)
                      (for/list ([ga (syntax->list #'(generic-args ...))])
                        (let loop ([ga ga])
                          (syntax-case ga ()
                            [(keyword id . ga)
                             (and (keyword-stx? #'keyword)
                                  (identifier? #'id))
                             #`(keyword id . #,(loop #'ga))]
                            [(id . ga)
                             (and (identifier? #'id))
                             #`(id . #,(loop #'ga))]
                            [(keyword [id] . ga)
                             (and (keyword-stx? #'keyword)
                                  (identifier? #'id))
                             #`(keyword [id #f] . #,(loop #'ga))]
                            [([id] . ga)
                             (and (identifier? #'id))
                             #`([id #f] . #,(loop #'ga))]
                            [id
                             (identifier? #'id)
                             #'id]
                            [()
                             #'()])))]
                     ;; if we're the ones defining the struct property,
                     ;; generate a new id, otherwise use the struct property
                     ;; accessor that we were passed
                     [get-generics
                      (if prop-defined-already?
                          #'defined-already?
                          (generate-temporary 'get-generics))])
         #`(begin
             (define-syntax name (list #'prop:name #'generic ...))
             ; XXX optimize no kws or opts
             (define generic-arity-coerce
               (let*-values ([(p) (lambda fake-args #f)]
                             [(generic-arity-spec) (procedure-arity p)]
                             [(generic-required-kws generic-allowed-kws) (procedure-keywords p)])
                 (lambda (f)
                   (procedure-reduce-keyword-arity f generic-arity-spec generic-required-kws generic-allowed-kws))))
             ...
             #,@(if prop-defined-already?
                    '() ; we don't need to define it
                    (list
                     #'(define-values (prop:name name? get-generics)
                         (make-struct-type-property
                          'name
                          (lambda (generic-vector si)
                            (unless (vector? generic-vector)
                              (error 'name
                                     "bad generics table, expecting a vector, got ~e"
                                     generic-vector))
                            (unless (= (vector-length generic-vector)
                                       how-many-generics)
                              (error 'name
                                     "bad generics table, expecting a vector of length ~e, got ~e"
                                     how-many-generics
                                     (vector-length generic-vector)))
                            (vector (let ([mthd-generic (vector-ref generic-vector generic-idx)])
                                      (and mthd-generic
                                           (generic-arity-coerce mthd-generic)))
                                    ...))))))
             ;; Hash table mapping method name symbols to
             ;; whether the given method is implemented
             (define (defined-table this)
               (unless (name? this)
                 (raise-argument-error 'defined-table name-str this))
               (for/hash ([name (in-list '(#,@(map syntax->datum generics)))]
                          [gen (in-vector (get-generics this))])
                 (values name (not (not gen)))))
             ;; Define the contract that goes with this generic interface
             (define-generics-contract header name? get-generics
               (generic generic-idx) ...)
             ;; Define generic functions
             (define generic
               (generic-arity-coerce
                (make-keyword-procedure
                 (lambda (kws kws-args . given-args)
                   (define this (list-ref given-args generic-this-idx))
                   (if (name? this)
                       (let ([m (vector-ref (get-generics this) generic-idx)])
                         (if m
                             (keyword-apply m kws kws-args given-args)
                             (error 'generic "not implemented for ~e" this)))
                       (raise-argument-error 'generic name-str this)))
                 ; XXX (non-this ... this . rst)
                 (lambda given-args
                   (define this (list-ref given-args generic-this-idx))
                   (if (name? this)
                       (let ([m (vector-ref (get-generics this) generic-idx)])
                         (if m
                             (apply m given-args)
                             (error 'generic "not implemented for ~e" this)))
                       (raise-argument-error 'generic name-str this))))))
             ...)))]))

;; generate a contract combinator for instances of a generic interface
(define-syntax (define-generics-contract stx)
  (syntax-case stx ()
    [(_ name name? accessor (generic generic-idx) ...)
     (with-syntax ([name/c (format-id #'name "~a/c" #'name)])
       #`(define-syntax (name/c stx)
           (syntax-case stx ()
             [(_ [method-id ctc] (... ...))
              (andmap (λ (id) (and (identifier? id)
                                   ;; make sure the ids are all
                                   ;; in the interface
                                   (member (syntax-e id) (list 'generic ...))))
                      (syntax->list #'(method-id  (... ...))))
              #'(make-generic-instance/c
                 (quote #,(syntax-e #'name/c))
                 name?
                 accessor
                 (list 'method-id (... ...))
                 (list ctc (... ...))
                 (make-immutable-hash
                  (list (cons 'generic generic-idx) ...)))])))]))

;; make a generic instance contract
(define (make-generic-instance/c name name? accessor ids ctc-args method-map)
  (define ctcs (coerce-contracts 'generic-instance/c ctc-args))
  ;; map method table indices to ids & projections
  (define id+ctc-map
    (for/hash ([id ids] [ctc ctcs])
      (values (hash-ref method-map id)
              (cons id (contract-projection ctc)))))
  (cond [(andmap chaperone-contract? ctcs)
         (chaperone-generic-instance/c
          name name? ids ctcs accessor id+ctc-map method-map)]
        [else
         (impersonator-generic-instance/c
          name name? ids ctcs accessor id+ctc-map method-map)]))

(define (generic-instance/c-name ctc)
  (define method-names
    (map (λ (id ctc) (build-compound-type-name id ctc))
         (base-generic-instance/c-ids ctc)
         (base-generic-instance/c-ctcs ctc)))
  (apply build-compound-type-name
         (cons (base-generic-instance/c-name ctc) method-names)))

;; redirect for use with chaperone-vector
(define ((method-table-redirect ctc blame) vec idx val)
  (define id+ctc-map (base-generic-instance/c-id+ctc-map ctc))
  (define maybe-id+ctc (hash-ref id+ctc-map idx #f))
  (cond [maybe-id+ctc
         (define id (car maybe-id+ctc))
         (define proj (cdr maybe-id+ctc))
         (define blame-string (format "the ~a method of" id))
         ((proj (blame-add-context blame blame-string)) val)]
        [else val]))

;; projection for generic methods
(define ((generic-instance/c-proj proxy-struct) ctc)
  (λ (blame)
    ;; for redirecting the method table accessor
    (define (redirect struct v)
      (chaperone-vector
       v
       (method-table-redirect ctc blame)
       (λ (vec i v) v)))
    (λ (val)
      (unless (contract-first-order-passes? ctc val)
        (raise-blame-error
         blame val
         '(expected: "~s," given: "~e")
         (contract-name ctc)
         val))
      (define accessor (base-generic-instance/c-accessor ctc))
      (proxy-struct val accessor redirect))))

;; recognizes instances of this generic interface
(define ((generic-instance/c-first-order ctc) v)
  (cond [((base-generic-instance/c-name? ctc) v)
         (define accessor (base-generic-instance/c-accessor ctc))
         (define method-table (accessor v))
         (define ids (base-generic-instance/c-ids ctc))
         (define ctcs (base-generic-instance/c-ctcs ctc))
         (define method-map (base-generic-instance/c-method-map ctc))
         ;; do sub-contract first-order checks
         (for/and ([id ids] [ctc ctcs])
           (contract-first-order-passes?
            ctc
            (vector-ref method-table (hash-ref method-map id))))]
        [else #f]))

;; name        - for building ctc name
;; name?       - for first-order checks
;; ids         - for method names (used to build the ctc name)
;; ctcs        - for the contract name
;; accessor    - for chaperoning the struct type property
;; id+ctc-map  - for chaperoning the method table vector
;; method-map  - for first-order checks
(struct base-generic-instance/c
  (name name? ids ctcs accessor id+ctc-map method-map))

(struct chaperone-generic-instance/c base-generic-instance/c ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:projection (generic-instance/c-proj chaperone-struct)
   #:first-order generic-instance/c-first-order
   #:name generic-instance/c-name))

(struct impersonator-generic-instance/c base-generic-instance/c ()
  #:property prop:contract
  (build-contract-property
   #:projection (generic-instance/c-proj impersonate-struct)
   #:first-order generic-instance/c-first-order
   #:name generic-instance/c-name))
