#lang racket/base
(require racket/contract/base
         racket/contract/combinator
         "private/generic.rkt"
         "private/generic-methods.rkt"
         (for-syntax racket/base racket/local racket/syntax syntax/stx))

;; Convenience layer on top of racket/private/generic.
;; To avoid circular dependencies, racket/private/generic cannot use
;; `parse-keyword-options' (which depends on racket/dict). So we do
;; keyword argument parsing here.
;; Files that use racket/private/generic _must_ pass _all_ keyword
;; arguments to define-generics _in_order_.

(provide define-generics
         define/generic
         raise-support-error
         (struct-out exn:fail:support)
         chaperone-generics
         impersonate-generics
         redirect-generics
         generic-instance/c)

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
                      [clauses (reverse (syntax->list #'(clause ...)))])
             (if (pair? clauses)
                 (syntax-case (car clauses) ()
                   [(pred #:dispatch disp defn ...)
                    (loop (cons #'[pred disp defn ...] defaults)
                          (cdr clauses))]
                   [(pred defn ...)
                    (with-syntax ([name (generate-temporary #'pred)])
                      (loop (cons #'[pred #:same defn ...] defaults)
                            (cdr clauses)))]
                   [clause
                    (wrong-syntax #'clause "invalid #:defaults specification")])
                 (parse #'args
                        (hash-set* options 'defaults defaults)))))]
      [(#:defaults . other)
       (wrong-syntax (stx-car stx) "invalid #:defaults specification")]
      [(#:fast-defaults (clause ...) . args)
       (if (hash-ref options 'fast-defaults #f)
           (wrong-syntax (stx-car stx)
                         "duplicate #:fast-defaults specification")
           (let loop ([fast-defaults '()]
                      [clauses (reverse (syntax->list #'(clause ...)))])
             (if (pair? clauses)
                 (syntax-case (car clauses) ()
                   [(pred #:dispatch disp defn ...)
                    (loop (cons #'[pred disp defn ...] fast-defaults)
                          (cdr clauses))]
                   [(pred defn ...)
                    (with-syntax ([name (generate-temporary #'pred)])
                      (loop (cons #'[pred #:same defn ...] fast-defaults)
                            (cdr clauses)))]
                   [clause
                    (wrong-syntax #'clause
                                  "invalid #:fast-defaults specification")])
                 (parse #'args
                        (hash-set* options
                                   'fast-defaults fast-defaults)))))]
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
         (methods support table fasts defaults fallbacks derived)
         (parse #'rest))
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
       (define/with-syntax ctc-name (format-id #'name "~a/c" #'name))
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
           (define-primitive-generics/derived
             original
             (name gen-name prop-name get-name pred-name support-name)
             #:fast-defaults [fast-default ...]
             #:defaults [default ...]
             #:fallbacks [fallback ...]
             #:derive-properties [derive ...]
             method ...)
           table-defn
           (define-generics-contract ctc-name gen-name)))]))

(define-syntax (redirect-generics/derived stx)
  (syntax-case stx ()
    [(_ orig mode gen-name val-expr [method-name proc-expr] ...)
     (parameterize ([current-syntax-context #'orig])
       (define gen-id #'gen-name)
       (unless (identifier? gen-id)
         (wrong-syntax gen-id "expected an identifier"))
       (define gen-info (syntax-local-value gen-id (lambda () #f)))
       (unless (generic-info? gen-info)
         (wrong-syntax gen-id "expected a name for a generic interface"))
       (define delta (syntax-local-make-delta-introducer gen-id))
       (define predicate (generic-info-predicate gen-info))
       (define accessor (generic-info-accessor gen-info))
       (define method-ids (syntax->list #'(method-name ...)))
       (define indices
         (for/list ([method-id (in-list method-ids)])
           (find-generic-method-index #'orig gen-id delta gen-info method-id)))
       (define/with-syntax pred-name predicate)
       (define/with-syntax ref-name accessor)
       (define/with-syntax [method-index ...] indices)
       #'(redirect-generics-proc 'gen-name mode pred-name ref-name val-expr
                                 (lambda (i x)
                                   (case i
                                     [(method-index) (proc-expr x)]
                                     ...
                                     [else x]))))]))

(define-syntax (redirect-generics stx)
  (syntax-case stx ()
    [(_ mode gen-name val-expr [id expr] ...)
     #`(redirect-generics/derived #,stx mode gen-name val-expr [id expr] ...)]))

(define-syntax (chaperone-generics stx)
  (syntax-case stx ()
    [(_ gen-name val-expr [id expr] ...)
     #`(redirect-generics/derived #,stx #t gen-name val-expr [id expr] ...)]))

(define-syntax (impersonate-generics stx)
  (syntax-case stx ()
    [(_ gen-name val-expr [id expr] ...)
     #`(redirect-generics/derived #,stx #f gen-name val-expr [id expr] ...)]))

(define (redirect-generics-proc name chaperoning? pred ref x proc)
  (unless (pred x)
    (raise-argument-error name (format "a structure implementing ~a" name) x))
  (define-values (redirect-struct redirect-vector)
    (if chaperoning?
        (values chaperone-struct chaperone-vector)
        (values impersonate-struct impersonate-vector)))
  (define (vec-proc vec i method)
    (proc i method))
  (define (struct-proc x vec)
    (redirect-vector vec vec-proc vec-proc))
  (redirect-struct x ref struct-proc))

(define-syntax-rule (define-generics-contract ctc-name gen-name)
  (define-syntax (ctc-name stx)
    (syntax-case stx ()
      [(_ [id expr] (... ...))
       #`(generic-instance/c/derived #,stx
                                     [ctc-name]
                                     gen-name
                                     [id expr]
                                     (... ...))])))

(define-syntax (generic-instance/c stx)
  (syntax-case stx ()
    [(_ gen-name [id expr] ...)
     #`(generic-instance/c/derived #,stx
                                   [generic-instance/c gen-name]
                                   gen-name
                                   [id expr]
                                   ...)]))

(define-syntax (generic-instance/c/derived stx)
  (syntax-case stx ()
    [(_ original [prefix ...] gen-name [method-id ctc-expr] ...)
     (parameterize ([current-syntax-context #'original])
       (define gen-id #'gen-name)
       (unless (identifier? gen-id)
         (wrong-syntax gen-id "expected an identifier"))
       (define gen-info (syntax-local-value gen-id (lambda () #f)))
       (unless (generic-info? gen-info)
         (wrong-syntax gen-id "expected a name for a generic interface"))
       (define predicate (generic-info-predicate gen-info))
       (define/with-syntax pred predicate)
       (define/with-syntax [ctc-id ...]
         (generate-temporaries #'(ctc-expr ...)))
       (define/with-syntax [proj-id ...]
         (generate-temporaries #'(ctc-expr ...)))
       #'(let* ([ctc-id ctc-expr] ...)
           (make-generics-contract
            'gen-name
            '[prefix ...]
            pred
            '(method-id ...)
            (list ctc-id ...)
            (lambda (b x mode)
              (redirect-generics
               mode
               gen-name
               x
               [method-id
                (lambda (m)
                  (define b2
                    (blame-add-context b (format "method ~a" 'method-id)))
                  (((contract-projection ctc-id) b2) m))]
               ...)))))]))

(define (make-generics-contract ifc pfx pred mths ctcs proc)
  (define chaperoning?
    (for/and ([mth (in-list mths)] [ctc (in-list ctcs)])
      (unless (contract? ctc)
        (raise-arguments-error
         (car pfx)
         "non-contract value supplied for method"
         "value" ctc
         "method" mth
         "generic interface" ifc))
      (chaperone-contract? ctc)))
  (if chaperoning?
      (chaperone-generics-contract pfx pred mths ctcs proc)
      (impersonator-generics-contract pfx pred mths ctcs proc)))

(struct generics-contract [prefix predicate methods contracts redirect])

(define (generics-contract-name ctc)
  `(,@(generics-contract-prefix ctc)
    ,@(for/list ([method (in-list (generics-contract-methods ctc))]
                 [c (in-list (generics-contract-contracts ctc))])
        (list method (contract-name c)))))

(define (generics-contract-first-order ctc)
  (generics-contract-predicate ctc))

(define (generics-contract-projection mode)
  (lambda (c)
    (lambda (b)
      (lambda (x)
        ((generics-contract-redirect c) b x mode)))))

(struct chaperone-generics-contract generics-contract []
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name generics-contract-name
   #:first-order generics-contract-first-order
   #:projection (generics-contract-projection #t)))

(struct impersonator-generics-contract generics-contract []
  #:property prop:contract
  (build-contract-property
   #:name generics-contract-name
   #:first-order generics-contract-first-order
   #:projection (generics-contract-projection #f)))
