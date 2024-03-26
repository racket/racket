#lang racket/base

(provide (protect-out (for-syntax generate-in/out-code)))

(require (for-syntax racket/base
                     racket/list
                     racket/string
                     racket/struct-info
                     setup/path-to-relative
                     "../../private/struct-util.rkt"
                     (prefix-in a: "helpers.rkt")
                     (rename-in syntax/private/boundmap
                                ;; the private version of the library
                                ;; (the one without contracts)
                                ;; has these old, wrong names in it.
                                [make-module-identifier-mapping make-free-identifier-mapping]
                                [module-identifier-mapping-get free-identifier-mapping-get]
                                [module-identifier-mapping-put! free-identifier-mapping-put!]))
         "module-boundary-ctc.rkt"
         "arrow-val-first.rkt"
         "guts.rkt"
         "exists.rkt"
         "blame.rkt"
         syntax/location
         syntax/srcloc)

(define-for-syntax (self-ctor-transformer orig stx)
  (with-syntax ([orig orig])
    (syntax-case stx ()
      [(_ arg ...) (datum->syntax stx
                                  (syntax-e (syntax (orig arg ...)))
                                  stx
                                  stx)]
      [_ (syntax orig)])))

;; make-contract-out-redirect-struct-info
;; : (-> (-> (and/c struct-info? list?)) (-> identifier?) struct-info?)
;; Create a struct-info? value from two thunks:
;;  the 1st must be a valid argument for `make-struct-info`, and
;;  the 2nd must return an identifier for a structure type descriptor.
;; The 2nd thunk is used to recover the original names for a struct --- from before
;;  `contract-out` started to mangle them.
;;
;; make-applicable-contract-out-redirect-struct-info
;; : (-> (-> (and/c struct-info? list?)) (-> identifier?) (-> identifier?) struct-info?)
;; Similar to the above, but the 3rd thunk must return an identifier for a
;;  contract-protected constructor.
;; Creates a value that can be applied to construct instances of the struct type.
;;
;; undo-contract-out-redirect
;; : (-> any/c (or/c identifier? #f))
;; Return the original struct name associated with the argument, or #f if
;;  the input is not an indirect struct info.
(define-values-for-syntax [make-contract-out-redirect-struct-info
                           make-contract-out-redirect/field-struct-info
                           make-applicable-contract-out-redirect-struct-info
                           make-applicable-contract-out-redirect/field-struct-info
                           undo-contract-out-redirect]
  (let ()
    (define-values (struct:r make-r r? r-ref r-set!)
      (make-struct-type
       'contract-out-redirect-struct-info struct:struct-info
       1 0 #f
       '()
       (current-inspector) #f '(0)))

    (define-values (struct:r/field make-r/field r/field? r/field-ref r/field-set!)
      (make-struct-type
       'contract-out-redirect/field-struct-info struct:r
       1 0 #f
       (list (cons prop:struct-field-info
                   (lambda (rec)
                     (r/field-ref rec 0))))))

    (define-values (struct:app-r make-app-r app-r? app-r-ref app-r-set!)
      (make-struct-type
       'applicable-contract-out-redirect-struct-info struct:r
       1 0 #f
       (list (cons prop:procedure
                   (lambda (v stx)
                     (self-ctor-transformer ((app-r-ref v 0)) stx))))
       (current-inspector) #f '(0)))

    (define-values (struct:app-r/field
                    make-app-r/field
                    app-r/field?
                    app-r/field-ref
                    app-r/field-set!)
      (make-struct-type
       'applicable-contract-out-redirect/field-struct-info struct:app-r
       1 0 #f
       (list (cons prop:struct-field-info
                   (lambda (rec)
                     (app-r/field-ref rec 0))))))

    (define (undo-contract-out-redirect v)
      (and (r? v) ((r-ref v 0))))

    (values make-r make-r/field make-app-r make-app-r/field undo-contract-out-redirect)))


(define-for-syntax current-unprotected-submodule-name (make-parameter #f))

;; make-unprotected-submodule-code
;; : (-> (-> syntax?) (listof syntax?))
;; Calls `thunk` if `current-unprotected-submodule-name` is non-#f,
;; incorporating the resulting syntax object into a singleton list of
;; `module+` form; otherwise returns empty list.
(define-for-syntax (make-unprotected-submodule-code thunk)
  (define upe-submod (current-unprotected-submodule-name))
  (if upe-submod (list #`(module+ #,upe-submod #,(thunk))) null))

;; tl-code-for-one-id/new-name : syntax syntax syntax (union syntax #f) -> (values syntax syntax)
;; given the syntax for an identifier and a contract,
;; builds a begin expression for the entire contract
;; and for `define-module-boundary-contract/proc`
;; the first syntax object is used for source locations
(define-for-syntax (tl-code-for-one-id/new-name id-for-one-id
                                                stx id reflect-id ctrct/no-prop user-rename-id
                                                pos-module-source
                                                mangle-for-maker?
                                                upe-id
                                                who
                                                provide?)
  (define ex-id (or reflect-id id))
  (define id-rename-without-source (id-for-one-id who (or user-rename-id reflect-id id) mangle-for-maker?))
  (define id-rename (datum->syntax id-rename-without-source
                                   (syntax-e id-rename-without-source)
                                   ex-id
                                   id-rename-without-source))
  (with-syntax ([ctrct (syntax-property
                        (syntax-property
                         ctrct/no-prop
                         'racket/contract:contract-on-boundary
                         (gensym 'in-out/contract-boundary))
                        'inferred-name ex-id)]
                [external-name (or user-rename-id id)])
    (define srcloc-id
      (if (syntax-source id)
          id
          (if (and user-rename-id
                   (syntax-source user-rename-id))
              user-rename-id
              ex-id)))
    (with-syntax ([code
                   (syntax-property
                    (quasisyntax/loc stx
                      (begin #,(define-module-boundary-contract/proc id-rename
                                 id
                                 #'ctrct
                                 ex-id
                                 #'external-name
                                 #`(quote-srcloc #,srcloc-id)
                                 '#,who
                                 pos-module-source
                                 #f
                                 #t)
                             ;; `upe-id` is punned as an indicator of whether the `provide`s will be
                             ;; generated as well as the uncontracted identifier to be exported.  This
                             ;; is fine because we always need to generate both `provide`s anyway.
                             #,@(if upe-id
                                    (append
                                     (make-unprotected-submodule-code
                                      (lambda ()
                                        #`(provide (rename-out [#,id external-name]))))
                                     (list #`(provide (rename-out [#,id-rename external-name]))))
                                    null)))
                    (if provide? 'provide/contract-original-contract 'require/contract-original-contract)
                    (vector #'external-name #'ctrct))])
      #`(code #,id-rename))))

(define-for-syntax (generate-in/out-code who stx p/c-clauses unprotected-submodule-name
                                         just-check-errors?
                                         provide?
                                         pos-module-source-expression)
  (define mangled-id-scope (make-syntax-introducer))

  ;; ids : table[id -o> (listof id)]
  ;; code-for-each-clause adds identifiers to this map.
  ;; when it binds things; they are then used to signal
  ;; a syntax error for duplicates
  (define dups-table (make-hash))
  (define (add-to-dups-table id)
    (hash-update!
     dups-table
     (syntax-e id)
     (λ (ids) (cons id ids))
     '()))
  (define (signal-dup-syntax-error)
    (hash-for-each
     dups-table
     (λ (k ids)
       (let loop ([ids ids])
         (cond
           [(null? ids) (void)]
           [else
            (cond
              [(ormap (λ (x) (bound-identifier=? (car ids) x)) (cdr ids))
               (let ([dups (filter (λ (x) (bound-identifier=? (car ids) x))
                                   ids)])
                 (raise-syntax-error who
                                     "duplicate identifiers"
                                     stx
                                     (car dups)
                                     (cdr dups)))]
              [else
               (loop (cdr ids))])])))))

  ;; code-for-each-clause : (listof syntax) -> (listof syntax)
  ;; constructs code for each clause of a contract-in/contract-out
  (define (code-for-each-clause clauses)
    (let loop ([clauses clauses]
               [exists-binders '()])
      (cond
        [(null? clauses) null]
        [else
         (let ([clause (car clauses)])
           ;; compare raw identifiers for `struct` and `rename` just like `provide` does
           (syntax-case* clause (struct rename) (λ (x y) (eq? (syntax-e x) (syntax-e y)))
             [exists
              (or (eq? '#:exists (syntax-e #'exists)) (eq? '#:∃ (syntax-e #'exists))
                  (eq? '#:forall (syntax-e #'exists)) (eq? '#:∀ (syntax-e #'exists)))
              (cond
                [(null? (cdr clauses))
                 (raise-syntax-error
                  who
                  (format (string-append
                           "expected either a single variable or a sequence of variables"
                           " to follow ~a, but found nothing")
                          (syntax-e #'exists))
                  stx
                  clause)]
                [else
                 (syntax-case (cadr clauses) ()
                   [x
                    (identifier? #'x)
                    (if just-check-errors?
                        (loop (cddr clauses) exists-binders)
                        (with-syntax ([(x-gen) (generate-temporaries #'(x))])
                          (cons (code-for-one-poly-id #'x #'x-gen #'exists)
                                (loop (cddr clauses)
                                      (add-a-binder #'x #'x-gen exists-binders)))))]
                   [(x ...)
                    (andmap identifier? (syntax->list #'(x ...)))
                    (if just-check-errors?
                        (loop (cddr clauses) exists-binders)
                        (with-syntax ([(x-gen ...) (generate-temporaries #'(x ...))])
                          (append (map (λ (x x-gen) (code-for-one-poly-id x x-gen #'exists))
                                       (syntax->list #'(x ...))
                                       (syntax->list #'(x-gen ...)))
                                  (loop (cddr clauses)
                                        (let loop ([binders exists-binders]
                                                   [xs (syntax->list #'(x ...))]
                                                   [x-gens (syntax->list #'(x-gen ...))])
                                          (cond
                                            [(null? xs) binders]
                                            [else
                                             (loop (add-a-binder (car xs) (car x-gens) binders)
                                                   (cdr xs)
                                                   (cdr x-gens))]))))))]
                   [else
                    (raise-syntax-error
                     who
                     (format (string-append "expected either a single variable or a sequence"
                                            " of variables to follow ~a")
                             (syntax-e #'exists))
                     stx
                     (cadr clauses))])])]
             [(rename this-name new-name contract)
              (and (identifier? (syntax this-name))
                   (identifier? (syntax new-name)))
              (begin
                (add-to-dups-table #'new-name)
                (if just-check-errors?
                    (loop (cdr clauses) exists-binders)
                    (cons (code-for-one-id stx
                                           (syntax this-name) #f
                                           (add-exists-binders (syntax contract) exists-binders)
                                           (syntax new-name))
                          (loop (cdr clauses) exists-binders))))]
             [(rename this-name new-name contract)
              (identifier? (syntax this-name))
              (raise-syntax-error who
                                  "malformed rename clause, expected an identifier"
                                  stx
                                  (syntax new-name))]
             [(rename this-name new-name contract)
              (identifier? (syntax new-name))
              (raise-syntax-error who
                                  "malformed rename clause, expected an identifier"
                                  stx
                                  (syntax this-name))]
             [(rename . _)
              (raise-syntax-error who "malformed rename clause" stx clause)]
             [(struct struct-name ((field-name contract) ...) options ...)
              (and (well-formed-struct-name? (syntax struct-name))
                   (andmap identifier? (syntax->list (syntax (field-name ...)))))
              (let ()
                (for ([option (in-list (syntax->list #'(options ...)))])
                  (unless (member (syntax-e option) '(#:omit-constructor))
                    (raise-syntax-error who
                                        "malformed struct option"
                                        stx
                                        option)))
                (unless (<= (length (syntax->list #'(options ...))) 1)
                  (raise-syntax-error who
                                      "malformed struct option"
                                      stx))
                (add-to-dups-table #'struct-name)
                (define omit-constructor?
                  (member '#:omit-constructor (map syntax-e (syntax->list #'(options ...)))))
                (if just-check-errors?
                    (loop (cdr clauses) exists-binders)
                    (let ([sc (build-struct-code stx
                                                 (syntax struct-name)
                                                 (syntax->list (syntax (field-name ...)))
                                                 (map (λ (x) (add-exists-binders x exists-binders))
                                                      (syntax->list (syntax (contract ...))))
                                                 omit-constructor?)])
                      (cons sc (loop (cdr clauses) exists-binders)))))]
             [(struct name)
              (identifier? (syntax name))
              (raise-syntax-error who
                                  "missing fields"
                                  stx
                                  clause)]
             [(struct name . rest)
              (not (well-formed-struct-name? (syntax name)))
              (raise-syntax-error
               who
               "name must be an identifier or two identifiers with parens around them"
               stx
               (syntax name))]
             [(struct name (fields ...) options ...)
              (let ()
                (for ([field [in-list (syntax->list (syntax (fields ...)))]])
                  (syntax-case field ()
                    [(x y)
                     (identifier? (syntax x))
                     (void)]
                    [(x y)
                     (raise-syntax-error who
                                         "malformed struct field, expected identifier"
                                         stx
                                         (syntax x))]
                    [else
                     (raise-syntax-error who
                                         "malformed struct field"
                                         stx
                                         field)]))
                ;; if we didn't find a bad field something is wrong!
                (raise-syntax-error who "internal error.1" stx clause))]
             [(struct name . fields)
              (raise-syntax-error who
                                  "malformed struct fields"
                                  stx
                                  clause)]
             [(name contract)
              (identifier? (syntax name))
              (begin
                (add-to-dups-table #'name)
                (if just-check-errors?
                    (loop (cdr clauses) exists-binders)
                    (cons (code-for-one-id stx
                                           (syntax name) #f
                                           (add-exists-binders (syntax contract)
                                                               exists-binders)
                                           #f)
                          (loop (cdr clauses) exists-binders))))]
             [(name contract)
              (raise-syntax-error who
                                  "expected identifier"
                                  stx
                                  (syntax name))]
             [unk
              (raise-syntax-error who
                                  "malformed clause"
                                  stx
                                  (syntax unk))]))])))

  ;; well-formed-struct-name? : syntax -> bool
  (define (well-formed-struct-name? stx)
    (or (identifier? stx)
        (syntax-case stx ()
          [(name super)
           (and (identifier? (syntax name))
                (identifier? (syntax super)))
           #t]
          [_ #f])))

  ;; build-struct-code : syntax syntax (listof syntax) (listof syntax) -> syntax
  ;; constructs the code for a struct clause
  ;; first arg is the original syntax object, for source locations
  (define (build-struct-code stx struct-name-position field-names field-contracts
                             omit-constructor?)
    (let* ([struct-name (syntax-case struct-name-position ()
                          [(a b) (syntax a)]
                          [else struct-name-position])]
           [the-struct-info (a:lookup-struct-info struct-name-position stx)]
           [true-field-names (and (struct-field-info? the-struct-info)
                                  (struct-field-info-list the-struct-info))]
           [orig-struct-name
            (or (undo-contract-out-redirect the-struct-info)
                struct-name)]
           [the-struct-info-list (extract-struct-info the-struct-info)]
           [orig-struct-info-list (extract-struct-info (syntax-local-value orig-struct-name))]
           [constructor-id (list-ref the-struct-info-list 1)]
           [predicate-id (list-ref the-struct-info-list 2)]
           [orig-predicate-id (list-ref orig-struct-info-list 2)]
           [selector-ids (reverse (list-ref the-struct-info-list 3))]
           [_ (when (and (not (null? selector-ids))
                         (not (last selector-ids)))
                (raise-syntax-error
                 who
                 (format "cannot determine the number of fields in struct")
                 stx
                 struct-name))]
           [orig-selector-ids (reverse (list-ref orig-struct-info-list 3))]
           [super-id (list-ref the-struct-info-list 5)]
           [parent-struct-count (cond
                                  [(boolean? super-id) #f]
                                  [else (length
                                         (list-ref
                                          (extract-struct-info
                                           (a:lookup-struct-info
                                            super-id
                                            stx))
                                          3))])]
           [type-is-only-constructor? (free-identifier=? constructor-id struct-name)]
           ; I think there's no way to detect when the struct-name binding isn't a constructor
           [type-is-constructor? #t]
           [chaperone-constructor-id
            (and constructor-id (car (generate-temporaries (list constructor-id))))]
           [is-id-ok?
            (λ (id i)
              (if (or (not parent-struct-count)
                      (parent-struct-count . <= . i))
                  id
                  #t))]
           [mutator-ids (reverse (list-ref the-struct-info-list 4))] ;; (listof (union #f identifier))
           [orig-mutator-ids (reverse (list-ref orig-struct-info-list 4))]

           [struct:struct-name
            (or (list-ref the-struct-info-list 0)
                (datum->syntax
                 struct-name
                 (string->symbol
                  (string-append
                   "struct:"
                   (symbol->string (syntax-e struct-name))))))]

           [-struct:struct-name
            (datum->syntax
             struct-name
             (string->symbol
              (string-append
               "-struct:"
               (symbol->string (syntax-e struct-name)))))]

           [is-new-id?
            (λ (index)
              (or (not parent-struct-count)
                  (parent-struct-count . <= . index)))])

      (let ([unknown-info
             (λ (what names)
               (raise-syntax-error
                who
                (format "cannot determine ~a, found ~s" what names)
                stx
                struct-name))])

        (unless (or (null? selector-ids)
                    (identifier? (last selector-ids)))
          (unknown-info "the selectors" (map syntax->datum selector-ids)))

        (unless constructor-id (unknown-info "constructor" constructor-id))
        (unless predicate-id (unknown-info "predicate" predicate-id))
        (unless (andmap/count is-id-ok? selector-ids)
          (unknown-info "selectors"
                        (map (λ (x) (if (syntax? x)
                                        (syntax->datum x)
                                        x))
                             selector-ids))))

      (unless (equal? (length selector-ids)
                      (length field-names))
        (raise-syntax-error who
                            (format "found ~a field~a in struct, but ~a contract~a"
                                    (length selector-ids)
                                    (if (= 1 (length selector-ids)) "" "s")
                                    (length field-names)
                                    (if (= 1 (length field-names)) "" "s"))
                            stx
                            struct-name))

      ;; make sure the field names are right.
      (define all-field+struct-names
        (extract-field+struct-names the-struct-info struct-name stx))
      (for ([field+struct-name (in-list all-field+struct-names)]
            [field-name (in-list (reverse field-names))])
        (define field-name-should-be (car field+struct-name))
        (define field-name-is (syntax-e field-name))
        (unless (equal? field-name-should-be field-name-is)
          (raise-syntax-error who
                              (format "expected field name to be ~a, but found ~a"
                                      field-name-should-be
                                      field-name-is)
                              stx
                              field-name)))

      (define (make-identifier sym)
        (datum->syntax #f sym))

      (define field-contract-ids
        (for/list ([field+struct-name (in-list all-field+struct-names)])
          (mangled-id-scope
           (a:mangle-id "in-out/contract-field-contract"
                        (make-identifier (car field+struct-name))
                        (make-identifier (cdr field+struct-name))
                        (make-identifier 'for)
                        struct-name))))

      (with-syntax ([((selector-codes selector-new-names) ...)
                     (for/list ([selector-id (in-list selector-ids)]
                                [orig-selector-id (in-list orig-selector-ids)]
                                [field-contract-id (in-list field-contract-ids)]
                                [index (in-naturals)]
                                #:when (is-new-id? index))
                       (code-for-one-id/new-name
                        who stx
                        selector-id #f
                        (build-selector-contract struct-name
                                                 predicate-id
                                                 field-contract-id)
                        (datum->syntax stx orig-selector-id)))]
                    [(rev-selector-old-names ...)
                     (reverse
                      (for/list ([selector-id (in-list selector-ids)]
                                 [index (in-naturals)]
                                 #:unless (is-new-id? index))
                        (let ([in-map (free-identifier-mapping-get struct-id-mapping
                                                                   selector-id
                                                                   (λ () #f))])
                          (or in-map
                              selector-id))))]
                    [(mutator-codes/mutator-new-names ...)
                     (for/list ([mutator-id (in-list mutator-ids)]
                                [orig-mutator-id (in-list orig-mutator-ids)]
                                [field-contract-id (in-list field-contract-ids)]
                                [index (in-naturals)])
                       (if (and mutator-id (is-new-id? index))
                           (code-for-one-id/new-name
                            who stx
                            mutator-id #f
                            (build-mutator-contract struct-name
                                                    predicate-id
                                                    field-contract-id)
                            (datum->syntax stx orig-mutator-id))
                           #f))]
                    [(predicate-code predicate-new-name)
                     (code-for-one-id/new-name who stx predicate-id #f (syntax predicate/c)
                                               (datum->syntax stx orig-predicate-id))]
                    [(constructor-code constructor-new-name)
                     (if omit-constructor?
                         #'((void) (void))
                         (code-for-one-id/new-name
                          who stx
                          chaperone-constructor-id struct-name
                          (build-constructor-contract stx
                                                      field-contract-ids
                                                      predicate-id)
                          constructor-id
                          #t
                          (and (not type-is-only-constructor?)
                               constructor-id)))]

                    [(field-contract-id-definitions ...)
                     (map (λ (field-contract-id field-contract)
                            #`(define #,field-contract-id
                                (coerce-contract '#,who #,field-contract)
                                #;
                                (opt/c field-contract #:error-name #,who)))
                          field-contract-ids
                          field-contracts)]
                    [(field-contract-ids ...) field-contract-ids])

        (with-syntax ([((mutator-codes _) ...)
                       (filter syntax-e (syntax->list #'(mutator-codes/mutator-new-names ...)))])
          (with-syntax ([(rev-selector-new-names ...)
                         (reverse (syntax->list (syntax (selector-new-names ...))))])
            (with-syntax ([struct-code
                           (with-syntax ([id-rename
                                          (or (free-identifier-mapping-get struct-id-mapping
                                                                           struct-name
                                                                           (λ () #f))
                                              (error 'contract/in-out.rkt
                                                     "internal error.2: ~s"
                                                     struct-name))]
                                         [struct-name struct-name]
                                         [orig-struct-name orig-struct-name]
                                         [-struct:struct-name -struct:struct-name]
                                         [super-id
                                          (if (boolean? super-id)
                                              super-id
                                              (with-syntax ([the-super-id
                                                             (or (free-identifier-mapping-get
                                                                  struct-id-mapping
                                                                  super-id
                                                                  (λ () #f))
                                                                 super-id)])
                                                (syntax (quote-syntax the-super-id))))]
                                         [(rev-mutator-id-info ...)
                                          (reverse
                                           (for/list ([x (in-list
                                                          (syntax->list
                                                           #'(mutator-codes/mutator-new-names ...)))])
                                             (syntax-case x ()
                                               [(a b) #'(quote-syntax b)]
                                               [else #f])))]
                                         [(exported-selector-ids ...) (reverse selector-ids)])
                             (define mk
                               (if (and type-is-constructor? (not omit-constructor?))
                                   (if true-field-names
                                       #'make-applicable-contract-out-redirect/field-struct-info
                                       #'make-applicable-contract-out-redirect-struct-info)
                                   (if true-field-names
                                       #'make-contract-out-redirect/field-struct-info
                                       #'make-contract-out-redirect-struct-info)))
                             (define proc
                               #`(λ ()
                                   (list (quote-syntax -struct:struct-name)
                                         #,(if type-is-only-constructor?
                                               #'(quote-syntax id-rename)
                                               #'(quote-syntax constructor-new-name))
                                         (quote-syntax predicate-new-name)
                                         (list (quote-syntax rev-selector-new-names) ...
                                               (quote-syntax rev-selector-old-names) ...)
                                         (list rev-mutator-id-info ...)
                                         super-id)))
                             (define the-constructor
                               (if (and type-is-constructor? (not omit-constructor?))
                                   #'((lambda () (quote-syntax constructor-new-name)))
                                   #'()))
                             (define the-field-names
                               (if true-field-names
                                   #`('#,true-field-names)
                                   #'()))
                             #`(begin
                                 #,@(make-unprotected-submodule-code
                                     (lambda ()
                                       #'(provide struct-name)))
                                 #,(if provide?
                                       #'(provide (rename-out [id-rename struct-name]))
                                       #'(define-syntax struct-name (make-rename-transformer #'id-rename)))
                                 (define-syntax id-rename
                                   (#,mk
                                    #,proc
                                    (lambda () (quote-syntax orig-struct-name))
                                    #,@the-constructor
                                    #,@the-field-names))))]
                          [struct:struct-name struct:struct-name]
                          [-struct:struct-name -struct:struct-name]
                          [struct-name struct-name]
                          [(selector-ids ...) selector-ids]
                          [(constructor-args ...) (generate-temporaries selector-ids)]
                          [struct-name-srcloc `'(,(and (path-string? (syntax-source struct-name))
                                                       (path->relative-string/library
                                                        (syntax-source struct-name)))
                                                 ,(syntax-line struct-name)
                                                 ,(syntax-column struct-name)
                                                 ,(syntax-position struct-name)
                                                 ,(syntax-span struct-name))])
              (quasisyntax/loc stx
                (begin
                  struct-code
                  field-contract-id-definitions ...
                  selector-codes ...
                  mutator-codes ...
                  predicate-code
                  (define #,chaperone-constructor-id
                    (let ([struct-name
                           (λ (constructor-args ...)
                             (chaperone-struct (#,constructor-id constructor-args ...)
                                               struct:struct-name
                                               struct-info
                                               (λ (struct-type skipped?)
                                                 (values -struct:struct-name skipped?))))])
                      struct-name))
                  constructor-code

                  ;; expanding out the body of the `make-pc-struct-type' function
                  ;; directly here in the expansion makes this very expensive at compile time
                  ;; when there are a lot of in-out/contract clause using structs
                  (define -struct:struct-name
                    (make-pc-struct-type #,pos-module-source-id
                                         'struct-name
                                         struct-name-srcloc
                                         struct:struct-name
                                         '(#,@field-names)
                                         field-contract-ids ...))
                  #,@(make-unprotected-submodule-code
                      (lambda ()
                        #'(provide struct:struct-name)))
                  #,(if provide?
                        #`(provide (rename-out [-struct:struct-name struct:struct-name]))
                        #`(define-syntax struct:struct-name (make-rename-transformer #'-struct:struct-name)))))))))))

  ;; andmap/count : (X Y int -> Z) (listof X) (listof Y) -> (listof Z)
  (define (andmap/count f l1)
    (let loop ([l1 l1]
               [i 0])
      (cond
        [(null? l1) #t]
        [else (and (f (car l1) i)
                   (loop (cdr l1)
                         (+ i 1)))])))

  ;; get-field-names/no-field-info :: string?
  ;;                                  (listof identifier?)
  ;;                                  (or/c identifier? boolean?)
  ;;                                  syntax?
  ;;                                  syntax?
  ;;                                  ->
  ;;                                  (listof symbol?)
  ;; attempts to extract field names from accessors
  (define (get-field-names/no-field-info struct-name
                                         accessors
                                         super-info
                                         orig-struct-name-stx
                                         stx)
    (define own-accessors
      (cond
        [(boolean? super-info) accessors]
        [else
         (define parent-accessors
           (list-ref (extract-struct-info (a:lookup-struct-info super-info stx)) 3))
         (drop-right accessors (length parent-accessors))]))
    (for/list ([accessor (in-list own-accessors)])
      (define accessor-str (symbol->string (syntax-e accessor)))
      (unless (string-prefix? accessor-str (string-append struct-name "-"))
        (raise-syntax-error
         who
         (format "unexpected accessor name ~a should start with ~a-"
                 accessor-str struct-name)
         stx
         orig-struct-name-stx))
      (string->symbol (substring accessor-str (add1 (string-length struct-name))))))

  ;; extract-field+struct-names : struct-info? syntax? syntax? -> (listof (cons/c symbol? symbol?))
  ;; returns a list of pair of field name and the struct name the field belongs to
  (define (extract-field+struct-names the-struct-info orig-struct-name-stx stx)
    (define struct-info-list (extract-struct-info the-struct-info))
    (define predicate (list-ref struct-info-list 2))
    (define accessors (list-ref struct-info-list 3))
    (define super-info (list-ref struct-info-list 5))
    (define struct-name (predicate->struct-name who stx predicate))
    (define immediate-field-names
      (if (struct-field-info? the-struct-info)
          (struct-field-info-list the-struct-info)
          (get-field-names/no-field-info struct-name
                                         accessors
                                         super-info
                                         orig-struct-name-stx
                                         stx)))
    (define immediate-field+struct-names
      (for/list ([fld (in-list immediate-field-names)])
        (cons fld (string->symbol struct-name))))
    (cond
      [(boolean? super-info) immediate-field+struct-names]
      [else (append immediate-field+struct-names
                    (extract-field+struct-names
                     (a:lookup-struct-info super-info stx)
                     orig-struct-name-stx
                     stx))]))

  ;; build-constructor-contract : syntax (listof syntax) syntax -> syntax
  (define (build-constructor-contract stx field-contract-ids predicate-id)
    (with-syntax ([(field-contract-ids ...) field-contract-ids]
                  [predicate-id predicate-id])
      (syntax/loc stx
        (-> field-contract-ids ...
            predicate-id))))

  ;; build-selector-contract : syntax syntax -> syntax
  ;; constructs the contract for a selector
  (define (build-selector-contract struct-name predicate-id field-contract-id)
    (with-syntax ([field-contract-id field-contract-id]
                  [predicate-id predicate-id])
      (syntax (-> predicate-id field-contract-id))))

  ;; build-mutator-contract : syntax syntax -> syntax
  ;; constructs the contract for a selector
  (define (build-mutator-contract struct-name predicate-id field-contract-id)
    (with-syntax ([field-contract-id field-contract-id]
                  [predicate-id predicate-id])
      (syntax (-> predicate-id
                  field-contract-id
                  void?))))

  ;; code-for-one-poly-id : syntax -> syntax
  (define (code-for-one-poly-id x x-gen poly)
    (if (or (eq? '#:exists (syntax-e poly)) (eq? '#:∃ (syntax-e poly)))
        #`(define #,x-gen (new-∃/c '#,x))
        #`(define #,x-gen (new-∀/c '#,x))))

  (define (add-exists-binders stx exists-binders)
    (if (null? exists-binders)
        stx
        #`(let #,exists-binders #,stx)))

  (define (add-a-binder id id-gen binders)
    (cons #`[#,id #,id-gen] binders))

  ;; code-for-one-id : syntax syntax syntax (union syntax #f) -> syntax
  ;; given the syntax for an identifier and a contract,
  ;; builds a begin expression for the entire contract and
  ;; define-module-boundary-contract/proc use;
  ;; the first syntax object is used for source locations
  (define (code-for-one-id stx id reflect-id ctrct user-rename-id)
    (with-syntax ([(code id) (code-for-one-id/new-name who stx id reflect-id ctrct user-rename-id)])
      (syntax code)))

  (define (id-for-one-id who id mangle-for-maker?)
    (mangled-id-scope
     ((if mangle-for-maker?
          a:mangle-id-for-maker
          a:mangle-id)
      (format "~a/contract-id" who)
      id)))

  (define pos-module-source-id
    ;; Avoid context on this identifier, since it will be defined
    ;; in another module, and the definition may have to pull
    ;; along all context to support `module->namespace`:
    (datum->syntax #f 'pos-module-source))

  (define (code-for-one-id/new-name who stx id reflect-id ctrct/no-prop user-rename-id
                                    [mangle-for-maker? #f]
                                    [upe-id id])
    (tl-code-for-one-id/new-name id-for-one-id
                                 stx id reflect-id ctrct/no-prop user-rename-id
                                 pos-module-source-id
                                 mangle-for-maker?
                                 upe-id
                                 who
                                 provide?))
  (define struct-id-mapping (make-free-identifier-mapping))
  (define (add-struct-clause-to-struct-id-mapping a flds/stx)
    (define flds (syntax->list flds/stx))
    (define compile-time-info (syntax-local-value a (λ () #f)))
    (when (and (identifier? a)
               (struct-info? compile-time-info))
      (define parent
        (let ([parent (list-ref (extract-struct-info compile-time-info) 5)])
          (if (boolean? parent) #f parent)))
      (when (and (or (not parent)
                     (and (identifier? parent)
                          (struct-info? (syntax-local-value parent (λ () #f)))))
                 flds
                 (andmap identifier? flds))
        (free-identifier-mapping-put!
         struct-id-mapping
         a
         (mangled-id-scope
          (a:mangle-id (format "~a/contract-struct-expansion-info-id" who)
                       a)))
        (define parent-selectors
          (if parent
              (let ([parent-selectors (list-ref (extract-struct-info (syntax-local-value parent))
                                                3)])
                (length parent-selectors))
              0))
        ;; this test will fail when the syntax is bad; we catch syntax errors elsewhere
        (when (< parent-selectors (length flds))
          (for ([f (in-list (list-tail flds parent-selectors))])
            (define selector-id (datum->syntax
                                 a
                                 (string->symbol (format "~a-~a" (syntax-e a) (syntax-e f)))))
            (free-identifier-mapping-put!
             struct-id-mapping
             selector-id
             (id-for-one-id who selector-id #f)))))))
  (parameterize ([current-unprotected-submodule-name unprotected-submodule-name])
    (cond
      [just-check-errors?
       (code-for-each-clause p/c-clauses)
       (signal-dup-syntax-error)]
      [else
       (for ([clause (in-list p/c-clauses)])
         (syntax-case* clause (struct) (λ (x y) (eq? (syntax-e x) (syntax-e y)))
           [(struct a ((fld ctc) ...) options ...)
            (identifier? #'a)
            (add-struct-clause-to-struct-id-mapping #'a #'(fld ...))]
           [(struct (a b) ((fld ctc) ...) options ...)
            (add-struct-clause-to-struct-id-mapping #'a #'(fld ...))]
           [_ (void)]))
       (with-syntax ([(bodies ...) (code-for-each-clause p/c-clauses)]
                     [pos-module-source-id pos-module-source-id])
         #`(begin
             (define pos-module-source-id #,pos-module-source-expression)
             bodies ...))])))

(define (make-pc-struct-type pos-module-source struct-name srcloc struct-type field-names . ctcs)
  (define blame
    (make-blame (build-source-location srcloc) struct-type (λ () `(substruct-of ,struct-name))
                pos-module-source #f #t))
  (define late-neg-acceptors
    (for/list ([ctc (in-list ctcs)]
               [field-name (in-list field-names)])
      ((get/build-late-neg-projection ctc)
       (blame-add-context blame
                          (format "the ~a field of" field-name)
                          #:swap? #t))))
  (chaperone-struct-type
   struct-type
   (λ (a b c d e f g h) (values a b c d e f g h))
   (λ (x) x)
   (λ args
     (define name #f)
     (define vals
       (let loop ([args args])
         (cond
           [(null? args) null]
           [(null? (cdr args))
            (set! name (car args))
            null]
           [else (cons (car args) (loop (cdr args)))])))
     (apply values
            (map (λ (late-neg-acceptors val)
                   (late-neg-acceptors val 'not-enough-info-for-blame))
                 late-neg-acceptors
                 vals)))))
