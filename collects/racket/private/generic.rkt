#lang racket/base
(require racket/local
         (for-syntax racket/base
                     racket/local
                     racket/syntax
                     syntax/stx)
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
                #:defaults
                ([pred? impl ...]
                 ;; TODO fallthrough?
                 ...)
                ;; are we being passed an existing struct property? If so,
                ;; this kw arg is bound to the struct property accessor, and
                ;; we don't define the struct property
                #:prop-defined-already? defined-already?
                ;; Passed in by `define-generics` in racket/generic.
                ;; This enables us to cut the dependency on racket/contract
                ;; for users of this private module. Pass in #f
                ;; to avoid defining a contract.
                #:define-contract define-generics-contract)
        (generic . generic-args) ...)
     (and (identifier? #'header)
          (identifier? #'name)
          (identifier? #'prop:name)
          (identifier? #'name?)
          (identifier? #'defined-table)
          (let ([generics (syntax->list #'(generic ...))])
            (and (list? generics)
                 (andmap identifier? generics))))
     (let* ([generics (syntax->list #'(generic ...))]
            [name-str (symbol->string (syntax-e #'name?))]
            [idxs (for/list ([i (in-naturals 0)]
                             [_ generics])
                    i)]
            [prop-defined-already? (syntax-e #'defined-already?)]
            ;; syntax introducers for each default implementation set
            ;; these connect the default method definitions to the
            ;; appropriate dispatch reference in the generic function body
            [pred-introducers (map (λ (_) (make-syntax-introducer))
                                   (syntax->list #'(pred? ...)))]
            ;; mark each set of default methods for a default set and
            ;; then flatten all of the default definitions
            [method-impl-list
             (apply append
              (map syntax->list
                   (for/list ([introducer pred-introducers]
                              [meths (syntax->list #'((impl ...) ...))])
                     (introducer meths))))]
            ;; mark each generic function name for a default set
            [marked-generics
             (for/list ([generic generics])
               (for/list ([introducer pred-introducers])
                 (introducer generic)))])
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
                          (generate-temporary 'get-generics))]
                     ;; for each generic method, builds a cond clause to do the
                     ;; predicate dispatch found in method-impl-list
                     [((cond-impl ...) ...) marked-generics])
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
                     #'(begin
                         (define-values (prop:name -name? get-generics)
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
                                      ...))
                            null #t))
                         ;; overrides the interface predicate so that any of the default
                         ;; types also answer #t
                         (define (name? x)
                           (or (-name? x) (pred? x) ...)))))
             ;; Hash table mapping method name symbols to
             ;; whether the given method is implemented
             (define (defined-table this)
               (unless (name? this)
                 (raise-argument-error 'defined-table name-str this))
               (for/hash ([name (in-list '(#,@(map syntax->datum generics)))]
                          [gen (in-vector (get-generics this))])
                 (values name (not (not gen)))))
             ;; Define the contract that goes with this generic interface
             #,@(if (syntax-e #'define-generics-contract)
                    (list #'(define-generics-contract header name? get-generics
                             (generic generic-idx) ...))
                    ;; don't define a contract when given #f
                    '())
             ;; Define default implementations
             #,@method-impl-list
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
                   (cond
                    ;; default cases
                    [(pred? this) (apply cond-impl given-args)]
                    ...
                    ;; Fallthrough
                    [(name? this)
                     (let ([m (vector-ref (get-generics this) generic-idx)])
                       (if m
                           (apply m given-args)
                           (error 'generic "not implemented for ~e" this)))]
                    [else (raise-argument-error 'generic name-str this)])))))
             ...)))]))

