#lang racket/base
(require racket/local
         (for-syntax racket/base
                     racket/local
                     racket/syntax))

(define-for-syntax (keyword-stx? v)
  (keyword? (syntax->datum v)))

(provide define-generics)
(define-syntax (define-generics stx)
  (syntax-case stx () ; can't use syntax-parse, since it depends on us
    ;; keyword arguments must _all_ be provided _in_order_. For the
    ;; user-facing version of `define-generics', see racket/generics.
    [(_ (name prop:name name?
              #:defined-table defined-table
              ;; use of coercion functions is explained below
              #:coerce-method-table coerce-method-table)
        (generic . generic-args) ...)
     (and (identifier? #'name)
          (identifier? #'prop:name)
          (identifier? #'name?)
          (identifier? #'defined-table)
          (let ([generics (syntax->list #'(generic ...))])
            (and (pair? generics) (andmap identifier? generics))))
     (let* ([idxs (for/list ([i (in-naturals 0)]
                             [_ (syntax->list #'(generic ...))])
                    i)]
            [name-str (symbol->string (syntax-e #'name))]
            [generics (syntax->list #'(generic ...))]
            [need-coercion? (syntax->datum #'coerce-method-table)])
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
                             (if (free-identifier=? #'name #'id)
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
                     [prop:method-table
                      ;; if we need to coerce what's at prop:name into a
                      ;; method table, we need to generate a new struct
                      ;; property for the method table
                      (if need-coercion?
                          (generate-temporary (syntax->datum #'prop:name))
                          #'prop:name)])
         #`(begin
             (define-syntax name (list #'generic ...))
             ; XXX optimize no kws or opts
             (define generic-arity-coerce
               (let*-values ([(p) (lambda fake-args #f)]
                             [(generic-arity-spec) (procedure-arity p)]
                             [(generic-required-kws generic-allowed-kws) (procedure-keywords p)])
                 (lambda (f)
                   (procedure-reduce-keyword-arity f generic-arity-spec generic-required-kws generic-allowed-kws))))
             ...
             (define-values (prop:method-table name? get-generics)
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
                          ...))))
                          ;; Use case for method table coercion: retrofitting a generics-
             ;; based API on top of a struct property that uses its own ad-hoc
             ;; extension mechanism.
             ;; If coercion is used, prop:method-table and prop:name are
             ;; distinct. We define prop:name (e.g. prop:equals+hash-code,
             ;; the user-facing name) to "push" its method table to
             ;; prop:method-table, calling the coercion function if necessary.
             ;; prop:method-table is then used for dispatch and all.
             ;; That way, existing code can use prop:name using its old
             ;; extension API, and new code can use the generics-based
             ;; interface.
             ;; The coercion function should take whatever lives at prop:name
             ;; according to its old extension API, and produce a vector of
             ;; methods in the defined order.
             #,@(if need-coercion?
                    (list
                     #'(define-values (prop:name unused unused2)
                         (make-struct-type-property
                          'front-facing-name
                          #f ; no guard, we accept anything;
                          ;; prop:method-table does the checking
                          (list
                           (cons prop:method-table
                                 (lambda (maybe-method-table)
                                   ;; if we get a valid method table, (methods
                                   ;; was used, not the old API provided for
                                   ;; prop:name) we just use it. otherwise, we
                                   ;; call the coercion function
                                   (if (and (vector? maybe-method-table)
                                            (= (vector-length
                                                maybe-method-table)
                                               how-many-generics)
                                            (for/and ([g (in-vector
                                                          maybe-method-table)])
                                              (procedure? g)))
                                       ;; valid method table
                                       maybe-method-table
                                       (coerce-method-table
                                        maybe-method-table))))))))
                    ;; no need for coercions, methods are stored at prop:name
                    '())
             ;; Hash table mapping method name symbols to
             ;; whether the given method is implemented
             (define (defined-table this)
               (unless (name? this)
                 (raise-type-error 'defined-table name-str this))
               (for/hash ([name (in-list '(#,@(map syntax->datum generics)))]
                          [gen (in-vector (get-generics this))])
                 (values name (not (not gen)))))
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
                       (raise-type-error 'generic name-str this)))
                 ; XXX (non-this ... this . rst)
                 (lambda given-args
                   (define this (list-ref given-args generic-this-idx))
                   (if (name? this)
                       (let ([m (vector-ref (get-generics this) generic-idx)])
                         (if m
                             (apply m given-args)
                             (error 'generic "not implemented for ~e" this)))
                       (raise-type-error 'generic name-str this))))))
             ...)))]))

(require racket/stxparam)
(define-syntax-parameter define/generic
  (lambda (stx)
    (raise-syntax-error 'define/generic "only allowed inside methods" stx)))
(provide define/generic)

;; utility for specification of methods for a group of generic functions
;; (could make this do all the checks instead of a guard for the property)
(provide methods)
(define-syntax (methods stx)
  (syntax-case stx (=>)
    [(_ generics . mthds)
     (identifier? #'generics)
     (let ([specs (syntax-local-value #'generics (lambda () #f))])
       (unless (and (list? specs) (andmap identifier? specs))
         (raise-syntax-error
          #f "not a name for a generics group" stx #'generics))
       (with-syntax ([(generic ...)
                      specs]
                     [(mthd-generic ...)
                      (map (λ (g) (datum->syntax #'mthds (syntax->datum g)))
                           specs)])
         (syntax-property
         (syntax/loc stx
           (let (; XXX this could be a signal to the guard to error early,
                 ;     but is seems okay to allow missing methods
                 [mthd-generic #f]
                 ...)
             (syntax-parameterize
                 ([define/generic
                    (lambda (stx)
                      (syntax-case stx (mthd-generic ...)
                        [(_ new-name mthd-generic)
                         (syntax/loc stx
                           (define new-name generic))]
                        ...
                        [(_ new-name method-name)
                         (raise-syntax-error 'define/generic
                                             (format "~.s not a method of ~.s"
                                                     (syntax->datum #'method-name)
                                                     'generics)
                                             stx
                                             #'method-name)]))])
               (local mthds
                 (vector mthd-generic ...)))))
         'disappeared-use
         (list #'generics))))]))
