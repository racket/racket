#lang racket/base
(require racket/contract/base
         racket/contract/combinator
         "private/generic.rkt"
         (for-syntax racket/base racket/local racket/syntax syntax/stx))

;; Convenience layer on top of racket/private/generic.
;; To avoid circular dependencies, racket/private/generic cannot use
;; `parse-keyword-options' (which depends on racket/dict). So we do
;; keyword argument parsing here.
;; Files that use racket/private/generic _must_ pass _all_ keyword
;; arguments to define-generics _in_order_.

(provide define-generics define/generic)

(begin-for-syntax

  (define (parse stx [options (hasheq)])
    (syntax-case stx ()
      [(#:defined-predicate name . args)
       (identifier? #'name)
       (if (hash-ref options 'support #f)
           (wrong-syntax (stx-car stx)
                         "duplicate #:defined-predicate specification")
           (parse #'args (hash-set options 'support #'name)))]
      [(#:defined-predicate . other)
       (wrong-syntax (stx-car stx) "invalid #:defined-predicate specification")]
      [(#:defined-table name . args)
       (identifier? #'name)
       (if (hash-ref options 'table #f)
           (wrong-syntax (stx-car stx)
                         "duplicate #:defined-table specification")
           (parse #'args (hash-set options 'table #'name)))]
      [(#:defined-table . other)
       (wrong-syntax (stx-car stx) "invalid #:defined-table specification")]
      [(#:defaults (clause ...) . args)
       (if (hash-ref options 'defaults #f)
           (wrong-syntax (stx-car stx) "duplicate #:defaults specification")
           (let loop ([defaults '()]
                      [defns (hash-ref options 'defns '())]
                      [clauses (reverse (syntax->list #'(clause ...)))])
             (if (pair? clauses)
                 (syntax-case (car clauses) ()
                   [(pred #:dispatch disp defn ...)
                    (loop (cons #'[pred disp defn ...] defaults)
                          defns
                          (cdr clauses))]
                   [(pred defn ...)
                    (with-syntax ([name (generate-temporary #'pred)])
                      (loop (cons #'[name name defn ...] defaults)
                            (cons #'(define name pred) defns)
                            (cdr clauses)))]
                   [clause
                    (wrong-syntax #'clause "invalid #:defaults specification")])
                 (parse #'args
                        (hash-set* options 'defaults defaults 'defns defns)))))]
      [(#:defaults . other)
       (wrong-syntax (stx-car stx) "invalid #:defaults specification")]
      [(#:fast-defaults (clause ...) . args)
       (if (hash-ref options 'fast-defaults #f)
           (wrong-syntax (stx-car stx)
                         "duplicate #:fast-defaults specification")
           (let loop ([fast-defaults '()]
                      [defns (hash-ref options 'defns '())]
                      [clauses (reverse (syntax->list #'(clause ...)))])
             (if (pair? clauses)
                 (syntax-case (car clauses) ()
                   [(pred #:dispatch disp defn ...)
                    (loop (cons #'[pred disp defn ...] fast-defaults)
                          defns
                          (cdr clauses))]
                   [(pred defn ...)
                    (with-syntax ([name (generate-temporary #'pred)])
                      (loop (cons #'[name name defn ...] fast-defaults)
                            (cons #'(define name pred) defns)
                            (cdr clauses)))]
                   [clause
                    (wrong-syntax #'clause
                                  "invalid #:fast-defaults specification")])
                 (parse #'args
                        (hash-set* options
                                   'fast-defaults fast-defaults
                                   'defns defns)))))]
      [(#:fast-defaults . other)
       (wrong-syntax (stx-car stx) "invalid #:fast-defaults specification")]
      [(#:fallbacks [fallback ...] . args)
       (if (hash-ref options 'fallbacks #f)
           (wrong-syntax (stx-car stx) "duplicate #:fallbacks specification")
           (parse #'args (hash-set options 'fallbacks #'[fallback ...])))]
      [(#:fallbacks . other)
       (wrong-syntax (stx-car stx) "invalid #:fallbacks specification")]
      [(#:derive-property prop impl . args)
       (parse #'args
              (hash-set options
                        'derived
                        (cons (list #'prop #'impl)
                              (hash-ref options 'derived '()))))]
      [(#:derive-property . other)
       (wrong-syntax (stx-car stx) "invalid #:derive-property specification")]
      [(kw . args)
       (keyword? (syntax-e #'kw))
       (wrong-syntax #'kw "invalid keyword argument")]
      [((_ . _) . args)
       (if (hash-ref options 'methods #f)
           (wrong-syntax (stx-car stx) "duplicate methods list specification")
           (let loop ([methods (list (stx-car stx))] [stx #'args])
             (syntax-case stx ()
               [((_ . _) . args) (loop (cons (stx-car stx) methods) #'args)]
               [_ (parse stx (hash-set options 'methods (reverse methods)))])))]
      [(other . args)
       (wrong-syntax #'other
                     "expected a method identifier with formal arguments")]
      [() (values (hash-ref options 'methods '())
                  (hash-ref options 'defns '())
                  (hash-ref options 'support generate-temporary)
                  (hash-ref options 'table #f)
                  (hash-ref options 'fast-defaults '())
                  (hash-ref options 'defaults '())
                  (hash-ref options 'fallbacks '())
                  (hash-ref options 'derived '()))]
      [other
       (wrong-syntax #'other
                     "expected a list of arguments with no dotted tail")])))

(define-syntax (define-generics stx) ; allows out-of-order / optional kw args
  (syntax-case stx ()
    [(_ name . rest)
     (parameterize ([current-syntax-context stx])
       (unless (identifier? #'name)
         (wrong-syntax #'name "expected an identifier"))
       (define-values
         (methods defns support table fasts defaults fallbacks derived)
         (parse #'rest))
       (define/with-syntax [defn ...] defns)
       (define/with-syntax [fast-default ...] fasts)
       (define/with-syntax [default ...] defaults)
       (define/with-syntax [fallback ...] fallbacks)
       (define/with-syntax [derive ...] derived)
       (define/with-syntax [method ...] methods)
       (define/with-syntax [method-name ...] (map stx-car methods))
       (define/with-syntax [method-index ...]
         (for/list ([method (in-list methods)]
                    [index (in-naturals 0)])
           index))
       (define/with-syntax pred-name (format-id #'name "~a?" #'name))
       (define/with-syntax gen-name (format-id #'name "gen:~a" #'name))
       (define/with-syntax prop-name (generate-temporary #'name))
       (define/with-syntax get-name (generate-temporary #'name))
       (define/with-syntax support-name support)
       (define/with-syntax original stx)
       (define/with-syntax table-defn
         (if table
             (with-syntax ([table-name table])
               #'(define (table-name name)
                   (for/hasheq ([sym (in-list '(method ...))])
                     (values sym (support-name name sym)))))
             #'(begin)))
       #'(begin
           defn ...
           (define-primitive-generics/derived
             original
             (name gen-name prop-name get-name pred-name support-name)
             #:fast-defaults [fast-default ...]
             #:defaults [default ...]
             #:fallbacks [fallback ...]
             #:derive-properties [derive ...]
             method ...)
           table-defn
           (define-generics-contract name pred-name get-name
             [method-name method-index]
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
(define ((generic-instance/c-proj proxy-struct proxy-vector) ctc)
  (λ (blame)
    ;; for redirecting the method table accessor
    (define (redirect struct v)
      (proxy-vector
       v
       (method-table-redirect ctc blame)
       (λ (vec i v) v)))
    (λ (val)
       (unless ((base-generic-instance/c-name? ctc) val)
         (raise-blame-error
          blame val
          '(expected: "~s" given: "~e")
          (contract-name ctc)
          val))
       (define accessor (base-generic-instance/c-accessor ctc))
       (define method-table (accessor val))
       (define ids (base-generic-instance/c-ids ctc))
       (define ctcs (base-generic-instance/c-ctcs ctc))
       (define method-map (base-generic-instance/c-method-map ctc))
       ;; do sub-contract first-order checks
       (for ([id ids] [ctc ctcs])
         (define v (vector-ref method-table (hash-ref method-map id)))
         (unless (contract-first-order-passes? ctc v)
           (raise-blame-error
            (blame-add-context blame (format "method ~s of" id))
            v
            '(expected: "~s" given: "~e")
            (contract-name ctc)
            v)))
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
   #:projection (generic-instance/c-proj chaperone-struct chaperone-vector)
   #:first-order generic-instance/c-first-order
   #:name generic-instance/c-name))

(struct impersonator-generic-instance/c base-generic-instance/c ()
  #:property prop:contract
  (build-contract-property
   #:projection (generic-instance/c-proj impersonate-struct impersonate-vector)
   #:first-order generic-instance/c-first-order
   #:name generic-instance/c-name))
