#lang racket/base

(provide (rename-out [-struct/dc struct/dc]
                     [-struct/c struct/c]))

(require (for-syntax racket/base
                     racket/list
                     racket/struct-info
                     syntax/stx
                     "opt-guts.rkt"
                     "top-sort.rkt"
                     (only-in "ds-helpers.rkt" defeat-inlining)
                     (rename-in syntax/private/boundmap
                                ;; the private version of the library 
                                ;; (the one without contracts)
                                ;; has these old, wrong names in it.
                                [make-module-identifier-mapping make-free-identifier-mapping]
                                [module-identifier-mapping-get free-identifier-mapping-get]
                                [module-identifier-mapping-put! free-identifier-mapping-put!]
                                [module-identifier-mapping-for-each free-identifier-mapping-for-each]))
         syntax/location
         racket/list
         "guts.rkt"
         "blame.rkt"
         "prop.rkt"
         "misc.rkt"
         "opt.rkt")

;; these are the runtime structs for struct/dc.
;; each struct/dc contract has a list of subcontract's attached
;; to it. They indicate if each clause of the struct/dc was 
;; dependent or not, lazy or not, and, in the case that there are
;; some dependent contracts, which fields are depended on.
;; They also contain the selectors and mutators for the
;; corresponding fields. The runtime support for struct/dc
;; inspects these and uses them to build a chaperone & impersonator
;; for the struct passed into the contract.

;; The list is ordered, such that the fields of the corresponding
;; struct should be processed in that order. As each field is 
;; processed, an indy-contracted version of the field is accumuated
;; into a list (assuming the dependend-on? field is #t) and
;; that list is supplied to the dep-proc field of any dep
;; subcontracts that are encountered (in the reverse order of the
;; traversal). 

;; thus, at compile time, the struct/dc macro does a topological sort
;; of the clauses (preferring to keep things in the order the programmer
;; wrote, if that doesn't violate the ordering that checking has to
;; happen in) and rewrites the dependent variables so that each of
;; available dependent vars are listed at each step. For example,
;; if the user writes:
;;   (struct/dc s [a (b) ...][b (c) ...][c ...][d ...])
;; then the list would have c, followed by d, followed by b, followed by a.
;; and the dependent procedure generated for 'a' would accept both
;; 'b' and 'c', not just 'c' (to make it easier to build the arguments
;; in the runtime support).

(struct subcontract (field-name ref depended-on?) #:transparent)

(struct indep subcontract (ctc) #:transparent)
(struct dep   subcontract (dep-proc dep-names type) #:transparent)

(struct immutable              indep ()    #:transparent)
(struct lazy-immutable         indep ()    #:transparent)
(struct mutable                indep (set) #:transparent)

(struct dep-immutable          dep  ()     #:transparent)
(struct dep-lazy-immutable     dep  ()     #:transparent)
(struct dep-mutable            dep  (set)  #:transparent)

(struct dep-on-state-immutable dep  ()     #:transparent)
(struct dep-on-state-mutable   dep  (set)  #:transparent)

(define (subcontract-mutable-field? x)
  (or (mutable? x)
      (dep-mutable? x)
      (dep-on-state-mutable? x)))

;; these are the compile-time structures, representing
;; parsed clauses of a struct/dc expression
(begin-for-syntax
  ;; exp : syntax
  ;; lazy? : boolean
  ;; sel-id : identifier?
  ;; type : (or/c '#:flat '#:chaperone '#:impersonator) 
  ;; depends-on-state? : boolean? -- only set if the keyword #:depends-on-state is passed
  ;; deps : (listof identifier?)
  (struct clause (exp lazy? sel-id) #:transparent)
  (struct dep-clause clause (type depends-on-state? deps) #:transparent)
  (struct indep-clause clause () #:transparent))

(define-syntax-rule
  (cache-λ (id ...) e)
  (let ([cached unique])
    (λ (id ...)
      (cond [(eq? cached unique)
             (set! cached e)
             cached]
            [else cached]))))
(define unique (box #f))

(define (struct/dc-name ctc)
  (define struct/c? (base-struct/dc-struct/c? ctc))
  `(,(if struct/c?
         'struct/c
         'struct/dc)
    ,(base-struct/dc-name-info ctc)
    ,@(for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))])
        (cond
          [(indep? subcontract)
           (if struct/c?
               (contract-name (indep-ctc subcontract))
               `[,(subcontract-field-name subcontract)
                 ,@(if (lazy-immutable? subcontract)
                       '(#:lazy)
                       '())
                 ,(contract-name (indep-ctc subcontract))])]
          [else
           `[,(subcontract-field-name subcontract) 
             ,(dep-dep-names subcontract)
             ,@(if (dep-lazy-immutable? subcontract)
                   '(#:lazy)
                   '())
             ,@(if (eq? '#:chaperone (dep-type subcontract))
                   '()
                   (list (dep-type subcontract)))
             ...]]))))

(define (struct/dc-first-order ctc)
  (base-struct/dc-pred ctc))

(define (struct/dc-proj ctc)
  (define pred? (base-struct/dc-pred ctc))
  (λ (blame)
    (define orig-blames
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))])
        (blame-add-context blame (format "the ~a field of" (subcontract-field-name subcontract)))))
    (define orig-mut-blames
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))])
        (blame-add-context blame (format "the ~a field of" (subcontract-field-name subcontract)) #:swap? #t)))
    (define orig-indy-blames 
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))])
        (blame-replace-negative
         (blame-add-context blame (format "the ~a field of" (subcontract-field-name subcontract)))
         (base-struct/dc-here ctc))))
    (define orig-mut-indy-blames 
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))])
        (blame-replace-negative
         (blame-add-context blame (format "the ~a field of" (subcontract-field-name subcontract))
                            #:swap? #t)
         (base-struct/dc-here ctc))))
    (define projs
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))]
                 [blame+ctxt (in-list orig-blames)])
        (cond
          [(indep? subcontract)
           (define sub-ctc (indep-ctc subcontract))
           ((contract-projection sub-ctc) blame+ctxt)]
          [else #f])))
    (define mut-projs
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))]
                 [blame+ctxt (in-list orig-mut-blames)])
        (cond
          [(and (indep? subcontract) (mutable? subcontract))
           (define sub-ctc (indep-ctc subcontract))
           ((contract-projection sub-ctc) blame+ctxt)]
          [else #f])))
    (define orig-indy-projs
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))]
                 [blame+ctxt (in-list orig-indy-blames)])
        (cond
          [(indep? subcontract)
           (define sub-ctc (indep-ctc subcontract))
           ((contract-projection sub-ctc) blame+ctxt)]
          [else #f])))
    (define orig-mut-indy-projs
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))]
                 [blame+ctxt (in-list orig-mut-indy-blames)])
        (cond
          [(and (indep? subcontract) (mutable? subcontract))
           (define sub-ctc (indep-ctc subcontract))
           ((contract-projection sub-ctc) blame+ctxt)]
          [else #f])))
    (λ (v)
      (cond
        [(and (struct/c-imp-prop-pred? v)
              (contract-stronger? (struct/c-imp-prop-get v) ctc))
         v]
        [else
         (unless (pred? v)
           (raise-blame-error blame v '(expected: "~a")
                              (base-struct/dc-struct-name ctc)))
         (let loop ([subcontracts (base-struct/dc-subcontracts ctc)]
                    [projs projs]
                    [mut-projs mut-projs]
                    [indy-projs orig-indy-projs]
                    [mut-indy-projs orig-mut-indy-projs]
                    [blames orig-blames]
                    [mut-blames orig-mut-blames]
                    [indy-blames orig-indy-blames]
                    [mut-indy-blames orig-mut-indy-blames]
                    [chaperone-args '()]
                    [impersonate-args '()]
                    [dep-args '()])
           (cond
             [(null? subcontracts) 
              (apply chaperone-struct
                     (apply impersonate-struct
                            v
                            impersonate-args)
                     chaperone-args)]
             [else
              (define subcontract (car subcontracts))
              (define proj (car projs))
              (define mut-proj (car mut-projs))
              (define indy-proj (car indy-projs))
              (define mut-indy-proj (car mut-indy-projs))
              (define sel (subcontract-ref subcontract))
              (define blame (car blames))
              (define mut-blame (car mut-blames))
              (define indy-blame (car indy-blames))
              (define mut-indy-blame (car mut-indy-blames))
              (define dep-ctc 
                (and (dep? subcontract) 
                     (coerce-contract 
                      'struct/dc 
                      (apply (dep-dep-proc subcontract) dep-args))))
              (when dep-ctc (check-flat/chaperone dep-ctc subcontract))
              (define dep-ctc-blame-proj (and dep-ctc (contract-projection dep-ctc)))
              (define-values (new-chaperone-args new-impersonate-args)
                (cond
                  [(immutable? subcontract)
                   (define projd (proj (sel v)))
                   (values (if (flat-contract? (indep-ctc subcontract))
                               chaperone-args
                               (list* sel
                                      (λ (fld v) projd)
                                      chaperone-args))
                           impersonate-args)]
                  [(lazy-immutable? subcontract)
                   (values (list* sel
                                  (cache-λ (fld v) (proj v))
                                  chaperone-args)
                           impersonate-args)]
                  [(mutable? subcontract)
                   (if (impersonator-contract? (indep-ctc subcontract))
                       (values chaperone-args
                               (list* sel
                                      (λ (fld v) (proj v))
                                      (mutable-set subcontract)
                                      (λ (fld v) (mut-proj v))
                                      impersonate-args))
                       (values (list* sel
                                      (λ (fld v) (proj v))
                                      (mutable-set subcontract)
                                      (λ (fld v) (mut-proj v))
                                      chaperone-args)
                               impersonate-args))]
                  [else
                   (define proj (dep-ctc-blame-proj blame))
                   (cond
                     [(dep-immutable? subcontract)
                      (define projd (proj (sel v)))
                      (values (if (flat-contract? dep-ctc)
                                  chaperone-args
                                  (list* sel
                                         (λ (fld v) projd)
                                         chaperone-args))
                              impersonate-args)]
                     [(dep-lazy-immutable? subcontract)
                      (values (list* sel
                                     (cache-λ (fld v) (proj v))
                                     chaperone-args)
                              impersonate-args)]
                     [(dep-mutable? subcontract)
                      (define mut-proj (dep-ctc-blame-proj mut-blame))
                      (if (eq? (dep-type subcontract) '#:impersonator)
                          (values (list* sel
                                         (λ (fld v) (proj v))
                                         (dep-mutable-set subcontract)
                                         (λ (fld v) (mut-proj v))
                                         chaperone-args)
                                  impersonate-args)
                          (values chaperone-args
                                  (list* sel
                                         (λ (fld v) (proj v))
                                         (dep-mutable-set subcontract)
                                         (λ (fld v) (mut-proj v))
                                         impersonate-args)))]
                     [(dep-on-state-immutable? subcontract)
                      (proj (sel v))
                      (values (list* sel
                                     (λ (strct val) (build-dep-on-state-proj (base-struct/dc-subcontracts ctc) subcontract strct
                                                                             orig-indy-projs orig-indy-blames blame val))
                                     chaperone-args)
                              impersonate-args)]
                     [(dep-on-state-mutable? subcontract)
                      (proj (sel v))
                      (define (get-chap-proc strct val)
                        (build-dep-on-state-proj (base-struct/dc-subcontracts ctc) subcontract strct
                                                 orig-indy-projs orig-indy-blames blame val))
                      (define (set-chap-proc strct val)
                        (build-dep-on-state-proj (base-struct/dc-subcontracts ctc) subcontract strct
                                                 orig-mut-indy-projs orig-mut-indy-blames mut-blame val))
                      (if (eq? (dep-type subcontract) '#:impersonator)
                          (values chaperone-args
                                  (list* sel
                                         get-chap-proc
                                         (dep-on-state-mutable-set subcontract)
                                         set-chap-proc
                                         impersonate-args))
                          (values (list* sel
                                         get-chap-proc
                                         (dep-on-state-mutable-set subcontract)
                                         set-chap-proc
                                         chaperone-args)
                                  impersonate-args))])]))
              (loop (cdr subcontracts) 
                    (cdr projs)  (cdr mut-projs)  (cdr indy-projs)  (cdr mut-indy-projs) 
                    (cdr blames) (cdr mut-blames) (cdr indy-blames) (cdr mut-indy-blames)
                    new-chaperone-args
                    new-impersonate-args
                    (if (subcontract-depended-on? subcontract)
                        (cons (if dep-ctc-blame-proj 
                                  ((dep-ctc-blame-proj indy-blame) ((subcontract-ref subcontract) v))
                                  (indy-proj ((subcontract-ref subcontract) v)))
                              dep-args)
                        dep-args))]))]))))

(define (build-dep-on-state-proj orig-subcontracts this-subcontract strct projs blames blame val)
  (let loop ([subcontracts orig-subcontracts]
             [blames blames]
             [projs projs]
             [dep-args '()])
    (cond
      [(null? subcontracts) 
       (error 'build-dep-on-state-proj "ran out of subcontracts ~s ~s ~s" orig-subcontracts this-subcontract strct)]
      [else
       (define subcontract (car subcontracts))
       (cond
         [(eq? subcontract this-subcontract)
          (define the-ctc (coerce-contract 'struct/dc (apply (dep-dep-proc this-subcontract) dep-args)))
          (check-flat/chaperone the-ctc subcontract)
          (((contract-projection the-ctc) blame) val)]
         [else
          (define indy-blame (car blames))
          (define proj (car projs))
          (define dep-ctc 
            (and (dep? subcontract) 
                 (coerce-contract 
                  'struct/dc 
                  (apply (dep-dep-proc subcontract) dep-args))))
          (define dep-ctc-blame-proj (and dep-ctc (contract-projection dep-ctc)))
          
          (when (dep? subcontract)
            (check-flat/chaperone dep-ctc subcontract))
          
          (define new-dep-args
            (if (subcontract-depended-on? subcontract)
                (cons (if dep-ctc-blame-proj 
                          ((dep-ctc-blame-proj indy-blame) ((subcontract-ref subcontract) strct))
                          (proj ((subcontract-ref subcontract) strct)))
                      dep-args)
                dep-args))
          (loop (cdr subcontracts)
                (cdr blames)
                (cdr projs)
                new-dep-args)])])))

(define (check-flat/chaperone dep-ctc subcontract)
  (case (dep-type subcontract)
    [(#:flat)
     (unless (flat-contract? dep-ctc)
       (error 'struct/dc "expected a flat contract for the field: ~a, got ~s" 
              (subcontract-field-name subcontract)
              (contract-name dep-ctc)))]
    [(#:chaperone)
     (unless (chaperone-contract? dep-ctc)
       (error 'struct/dc "expected a chaperone contract for the field: ~a, got ~s" 
              (subcontract-field-name subcontract)
              (contract-name dep-ctc)))]))

(define (struct/dc-stronger? this that)
  (and (base-struct/dc? that)
       (eq? (base-struct/dc-pred this) (base-struct/dc-pred that))
       (for/and ([this-subcontract (in-list (base-struct/dc-subcontracts this))]
                 [that-subcontract (in-list (base-struct/dc-subcontracts that))])
         (cond
           [(and (indep? this-subcontract)
                 (indep? that-subcontract))
            (and (or (mutable? this-subcontract)
                     (and (immutable? this-subcontract)
                          (immutable? that-subcontract))
                     (and (lazy-immutable? this-subcontract)
                          (lazy-immutable? that-subcontract)))
                 (contract-stronger? (indep-ctc this-subcontract)
                                     (indep-ctc that-subcontract)))]
           [(and (dep? this-subcontract)
                 (dep? that-subcontract))
            (and (or (dep-mutable? this-subcontract)
                     (and (dep-immutable? this-subcontract)
                          (dep-immutable? that-subcontract))
                     (and (dep-lazy-immutable? this-subcontract)
                          (dep-lazy-immutable? that-subcontract)))
                 (procedure-closure-contents-eq?
                  (dep-dep-proc this-subcontract)
                  (dep-dep-proc that-subcontract)))]
           [else #t]))))

(define-struct base-struct/dc (subcontracts pred struct-name here name-info struct/c?))

(define-struct (struct/dc base-struct/dc) ()
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:name struct/dc-name
     #:first-order struct/dc-first-order
     #:projection struct/dc-proj
     #:stronger struct/dc-stronger?)))

(define-struct (flat-struct/dc base-struct/dc) ()
  #:property prop:flat-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-flat-contract-property
     #:name struct/dc-name
     #:first-order struct/dc-first-order
     #:projection struct/dc-proj
     #:stronger struct/dc-stronger?)))

(define-struct (impersonator-struct/dc base-struct/dc) ()
  #:property prop:contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-contract-property
     #:name struct/dc-name
     #:first-order struct/dc-first-order
     #:projection struct/dc-proj
     #:stronger struct/dc-stronger?)))

(define (build-struct/dc subcontracts pred struct-name here name-info struct/c?)
  (for ([subcontract (in-list subcontracts)])
    (when (and (indep? subcontract)
               (not (mutable? subcontract)))
      (unless (chaperone-contract? (indep-ctc subcontract))
        (error 'struct/dc "expected chaperone contracts, but field ~a has ~e"
               (subcontract-field-name subcontract)
               (indep-ctc subcontract)))))
  (define (flat-subcontract? subcontract)
    (cond
      [(indep? subcontract) (flat-contract? (indep-ctc subcontract))]
      [(dep? subcontract) (eq? '#:flat (dep-type subcontract))]))
  
  (define (impersonator-subcontract? subcontract)
    (cond
      [(indep? subcontract) (impersonator-contract? (indep-ctc subcontract))]
      [(dep? subcontract) (eq? '#:impersonator (dep-type subcontract))]))
  ((cond
     [(and (andmap flat-subcontract? subcontracts)
           (not (ormap subcontract-mutable-field? subcontracts)))
      make-flat-struct/dc]
     [(ormap impersonator-subcontract? subcontracts)
      make-impersonator-struct/dc]
     [else
      make-struct/dc])
   subcontracts pred struct-name here name-info struct/c?))
   

(define-for-syntax (get-struct-info id stx)
  (unless (identifier? id)
    (raise-syntax-error #f "expected a struct name" stx id))
  (define inf (syntax-local-value id (λ () #f)))
  (unless (struct-info? inf)
    (raise-syntax-error 'struct/dc "expected a struct" stx id))
  (define the-info (extract-struct-info inf))
  (unless (list-ref the-info 2)
    (raise-syntax-error 'struct/dc 
                        "expected a struct with a known predicate"
                        stx id))
  the-info)
      
(define-values (struct/c-imp-prop-desc
                struct/c-imp-prop-pred?
                struct/c-imp-prop-get)
  (make-impersonator-property 'struct/dc))


(define-for-syntax (parse-struct/dc stx)
  (syntax-case stx ()
    [(_ id clauses ...)
     (let ()
       (define info (get-struct-info #'id stx))
       (define (ensure-valid-field sel-id)
         (define selector-candidate (id->sel-id #'id sel-id))
         (unless (for/or ([selector (in-list (list-ref info 3))])
                   (and selector (free-identifier=? selector-candidate selector)))
           (raise-syntax-error #f 
                               "expected an identifier that names a field"
                               stx
                               sel-id)))
       
       (define (check-not-both this that)
         (when (and this that)
           (raise-syntax-error #f 
                               (format "found both ~a and ~a on the same field"
                                       (syntax-e this)
                                       (syntax-e that))
                               stx
                               that
                               (list this))))
       
       (define parsed-clauses
         (for/list ([clause (in-list (syntax->list #'(clauses ...)))])
           (syntax-case clause ()
             [(sel-id (dep-id ...) stuff1 . stuff) ;; need stuff1 here so that things like [a (>=/c x)] do not fall into this case
              (let ()
                (unless (identifier? #'sel-id)
                  (raise-syntax-error #f "expected an identifier (naming a field)" stx #'sel-id))
                (for ([id (in-list (syntax->list #'(dep-id ...)))])
                  (unless (identifier? id)
                    (raise-syntax-error #f "expected an identifier (naming a field)" stx id)))
                (ensure-valid-field #'sel-id)
                (define-values (ctc-exp lazy? type depends-on-state?)
                  (let loop ([stuff  #'(stuff1 . stuff)]
                             [lazy? #f]
                             [type #f]
                             [depends-on-state? #f])
                    (syntax-case stuff ()
                      [(exp) (values #'exp lazy? type depends-on-state?)]
                      [(flat/impersonator-kwd . more-stuff) 
                       (memq (syntax-e #'flat/impersonator-kwd) '(#:flat #:impersonator))
                       (begin
                         (check-not-both type (stx-car stuff))
                         (loop #'more-stuff lazy? (stx-car stuff) depends-on-state?))]
                      [(#:depends-on-state . more-stuff) (loop #'more-stuff lazy? type #t)]
                      [(#:lazy . more-stuff) (loop #'more-stuff #t type depends-on-state?)]
                      [_ (raise-syntax-error #f "could not parse clause" stx clause)])))
                (dep-clause ctc-exp lazy?
                            #'sel-id
                            (if type (syntax-e type) '#:chaperone)
                            depends-on-state?
                            (syntax->list #'(dep-id ...))))]
             [(sel-id . rest)
              (let ()
                (unless (identifier? #'sel-id)
                  (raise-syntax-error #f "expected an identifier (naming a field)" stx #'sel-id))
                (ensure-valid-field #'sel-id)
                (define-values (lazy? exp)
                  (syntax-case #'rest ()
                    [(#:lazy exp) (values #t #'exp)]
                    [(exp) (values #f #'exp)]
                    [else (raise-syntax-error #f "could not parse clause" stx clause)]))
                (indep-clause exp lazy? #'sel-id))]
             [_ (raise-syntax-error #f "could not parse clause" stx #'clause)])))
       
       
       (let ()
         (define lazy-mapping (make-free-identifier-mapping))
         (for ([clause (in-list parsed-clauses)])
           (free-identifier-mapping-put! lazy-mapping 
                                         (clause-sel-id clause)
                                         (clause-lazy? clause)))
         
         ;; check that non-lazy don't depend on lazy
         (for ([clause (in-list parsed-clauses)])
           (when (dep-clause? clause)
             (unless (clause-lazy? clause)
               (for ([dep-id (in-list (dep-clause-deps clause))])
                 (when (free-identifier-mapping-get lazy-mapping dep-id)
                   (raise-syntax-error 
                    #f
                    (format "the dependent clause for ~a is not lazy, but depends on ~a"
                            (syntax-e (clause-sel-id clause))
                            (syntax-e dep-id))
                    stx
                    dep-id))))))
         
         (for ([clause (in-list parsed-clauses)])
           (define this-sel (id->sel-id #'id (clause-sel-id clause)))
           (for ([sel (in-list (list-ref info 3))]
                 [mut (in-list (list-ref info 4))])
             (when (and sel
                        (free-identifier=? sel this-sel))
               
               
               ;; check that fields depended on actually exist
               (when (dep-clause? clause)
                 (for ([id (in-list (dep-clause-deps clause))])
                   (free-identifier-mapping-get
                    lazy-mapping
                    id
                    (λ () (raise-syntax-error #f
                                              (format "the field: ~a is depended on (by the contract on the field: ~a), but it has no contract"
                                                      (syntax-e id)
                                                      (syntax-e (clause-sel-id clause)))
                                              stx
                                              (clause-sel-id clause))))))
               
               ;; check that impersonator fields are mutable
               (when (and (dep-clause? clause)
                          (eq? (dep-clause-type clause) '#:impersonator))
                 (unless mut
                   (raise-syntax-error #f
                                       (format "the ~a field is immutable, so the contract cannot be an impersonator contract"
                                               (syntax-e (clause-sel-id clause)))
                                       stx
                                       (clause-sel-id clause))))

               ;; check that mutable fields aren't lazy
               (when (and (clause-lazy? clause) mut)
                 (raise-syntax-error #f 
                                     (format "the ~a field is mutable, so the contract cannot be lazy"
                                             (syntax-e (clause-sel-id clause)))
                                     stx
                                     (clause-sel-id clause)))))))
                          
       (values info #'id parsed-clauses))]))

(define-for-syntax (id->sel-id struct-id id) 
  (datum->syntax
   id
   (string->symbol
    (format "~a-~a" 
            (syntax-e struct-id)
            (syntax-e id)))))

(define-for-syntax (top-sort/clauses stx clauses)
  (define id->children (make-free-identifier-mapping))
  
  (for ([clause (in-list clauses)])
    (define id (clause-sel-id clause))
    (free-identifier-mapping-put! id->children id clause))
  
  (define (neighbors x)
    (cond
      [(dep-clause? x)
       (for/list ([id (in-list (dep-clause-deps x))])
         (free-identifier-mapping-get id->children id
                                      (λ ()
                                        (raise-syntax-error #f "unknown clause" stx id))))]
      [else '()]))
  
  (top-sort clauses neighbors
            (λ (leftovers)
              (raise-syntax-error #f "found cyclic dependencies"
                                  stx))))

(define-for-syntax (do-struct/dc struct/c? stx)
  (define-values (info struct-id clauses) (parse-struct/dc stx))
  (define sorted-clauses (top-sort/clauses stx clauses))
  
  ;; maps the sel-ids to #t when they are depended on
  (define depended-on-clauses (make-free-identifier-mapping))
  
  ;; map the sel-id/dep field identifiers to the corresponding clauses
  (define sel-id->clause (make-free-identifier-mapping))
  
  ;; track which clauses correspond to mutable fields
  (define mutable-clauses (make-free-identifier-mapping))
  
  ;; track which clauses (transitively) depend on mutable state
  ;; (either by directly depending on a mutable field or by having
  ;; the #:depends-on-state? keyword
  (define dep-on-mutable-clauses (make-free-identifier-mapping))
  

  ;; find-selector/mutator : clause -> (values identifier? identifier?)
  (define (find-selector/mutator clause)
    (define fld-name (clause-sel-id clause))
    (define this-selector 
      (datum->syntax fld-name
                     (string->symbol
                      (string-append 
                       (symbol->string (syntax-e struct-id))
                       "-"
                       (symbol->string (syntax-e fld-name))))))
    (define mutator (for/or ([selector (in-list (list-ref info 3))]
                             [mutator (in-list (list-ref info 4))])
                      (and (free-identifier=? this-selector selector)
                           mutator)))
    (values this-selector mutator))
  
  ;; init the first three mappings above
  (for ([clause (in-list sorted-clauses)])
    (define-values (sel mut) (find-selector/mutator clause))
    (free-identifier-mapping-put! mutable-clauses (clause-sel-id clause) (and mut #t))
    (free-identifier-mapping-put! sel-id->clause (clause-sel-id clause) clause)
    (when (dep-clause? clause)
      (for ([var (in-list (dep-clause-deps clause))])
        (free-identifier-mapping-put! depended-on-clauses var #t))))

  ;; init the dep-on-mutable-clauses mapping
  (for ([clause (in-list clauses)])
    (let loop ([clause clause])
      (define sel-id (clause-sel-id clause))
      (define current (free-identifier-mapping-get dep-on-mutable-clauses sel-id (λ () 'unknown)))
      (cond
        [(eq? current 'unknown)
         (define ans 
           (or (free-identifier-mapping-get mutable-clauses sel-id)
               (and (dep-clause? clause)
                    (or (dep-clause-depends-on-state? clause)
                        (for/or ([dep (in-list (dep-clause-deps clause))])
                          (loop (free-identifier-mapping-get sel-id->clause dep)))))))
         (free-identifier-mapping-put! dep-on-mutable-clauses sel-id ans)
         ans]
        [else
         current])))

  (define structs
    (let loop ([dep-args '()]
               [clauses sorted-clauses])
      (cond
        [(null? clauses) '()]
        [else
         (define clause (car clauses))
         (define-values (selector mutator) (find-selector/mutator clause))
         (define subcontract-constructor
           (if (dep-clause? clause)
               (if (free-identifier-mapping-get dep-on-mutable-clauses (clause-sel-id clause))
                   (if (clause-lazy? clause)
                       (raise-syntax-error 
                        #f 
                        (format "the contract on field ~a depends on mutable state (possibly indirectly), so cannot be lazy"
                                (syntax-e (clause-sel-id clause)))
                        stx (clause-sel-id clause))
                       (if mutator
                           #'dep-on-state-mutable
                           #'dep-on-state-immutable))
                   (if (clause-lazy? clause)
                       #'dep-lazy-immutable
                       (if mutator
                           #'dep-mutable
                           #'dep-immutable)))
               (if (clause-lazy? clause)
                   #'lazy-immutable
                   (if mutator
                       #'mutable
                       #'immutable))))
         (define depended-on? (free-identifier-mapping-get
                               depended-on-clauses
                               (clause-sel-id clause)
                               (λ () #f)))
         (define subcontract-args 
           (list #`'#,(clause-sel-id clause) selector depended-on?))
         (define indep/dep-args
           (if (dep-clause? clause)
               (list #`(λ (#,@dep-args) #,(clause-exp clause))
                     #`'(#,@(reverse dep-args))
                     #`'#,(dep-clause-type clause))
               (list #`(coerce-contract 'struct/dc #,(clause-exp clause)))))
         (cons #`(#,subcontract-constructor #,@subcontract-args
                                            #,@indep/dep-args
                                            #,@(if mutator
                                                   (list mutator)
                                                   '()))
               (loop (if depended-on?
                         (cons (clause-sel-id clause) dep-args)
                         dep-args)
                     (cdr clauses)))])))
  
  #`(build-struct/dc (list #,@structs)
                     #,(list-ref info 2)
                     'struct-id
                     (quote-module-name)
                     '#,struct-id
                     #,struct/c?))

(define-syntax (-struct/dc stx) (do-struct/dc #f stx))

(define-for-syntax (traverse-no-neg-blame-identifiers no-neg-blame)
  (for/and ([id (in-list no-neg-blame)])
    (let loop ([parent-id id]
               [path '()])
      (define x (syntax-local-value parent-id))
      (define box-id (define-opt/recursive-fn-neg-blame?-id x))
      (define bx (syntax-local-value box-id))
      (define content (unbox bx))
      (cond
        [(boolean? content) content]
        [(eq? content 'unknown) #f] ;; have to give up here
        [else
         (define ans
           (for/and ([id (in-list content)])
             (cond
               [(ormap (λ (y) (free-identifier=? id y)) path)
                ;; if we have a loop, then we know there is
                ;; no refutation of 'no-neg-blame' just cyclic
                ;; dependencies in define-opt/c, so we can 
                ;; conclude 'no-neg-blame' holds
                #t]
               [else
                (loop id (cons parent-id path))])))
         (set-box! bx ans) 
         ans]))))
       
(define/opter (-struct/dc opt/i opt/info stx)
  (syntax-case stx ()
    [(_ struct-id clause ...)
     (let/ec k
       (define-values (info _1 _2) (parse-struct/dc stx))
       (define (give-up [extra ""]) (k (opt/unknown opt/i opt/info stx extra)))
       (cond
         [(ormap values (list-ref info 4))
          ;; any mutable fields, just give up
          (give-up)]
         [else
          (define depended-on-fields (make-free-identifier-mapping))
          (define no-negative-blame-fields (make-free-identifier-mapping))
          (define-values (s-fo-code s-chap-code s-lifts s-super-lifts 
                                    s-partially-applied can-be-optimized? stronger-ribs chaperone? no-negative-blame)
            (for/fold ([s-fo-code '()]
                       [s-chap-code '()]
                       [s-lifts '()]
                       [s-super-lifts '()]
                       [s-partially-applied '()]
                       [can-be-optimized? #t]
                       [stronger-ribs '()]
                       [chaperone? #t]
                       [no-negative-blame #t])
                      ([clause (in-list (syntax->list #'(clause ...)))])
              
              (define-values (sel-id lazy? dep-vars exp)
                (syntax-case clause ()
                  [(sel-id #:lazy exp) (values #'sel-id #t #f #'exp)]
                  [(sel-id exp) (values #'sel-id #f #f #'exp)]
                  [(sel-id (dep-id ...) #:lazy exp)
                   (andmap identifier? (syntax->list #'(dep-id ...)))
                   (values #'sel-id #t #'(dep-id ...) #'exp)]
                  [(sel-id (dep-id ...) exp)
                   (andmap identifier? (syntax->list #'(dep-id ...)))
                   (values #'sel-id #f #'(dep-id ...) #'exp)]
                  [other (give-up)]))
              
              (define sub-val (car (generate-temporaries '(struct/dc))))
              
              (define this-optres (opt/i (opt/info-change-val sub-val opt/info) exp))
              
              (when dep-vars
                (for ([dep-var (in-list (syntax->list dep-vars))])
                  (free-identifier-mapping-put! depended-on-fields dep-var sel-id)))
              (free-identifier-mapping-put! no-negative-blame-fields sel-id (optres-no-negative-blame? this-optres))
              
              (define this-body-code 
                (cond
                  [dep-vars
                   (with-syntax ([(sel ...) (map (λ (var) (id->sel-id #'struct-id var)) 
                                                 (syntax->list dep-vars))]
                                 [(dep-var ...) dep-vars])
                     #`(let ([dep-var (sel #,(opt/info-val opt/info))] ...)
                         #,(bind-superlifts
                            (optres-superlifts this-optres)
                            (bind-lifts
                             (optres-lifts this-optres)
                             (bind-lifts
                              (optres-partials this-optres)
                              (optres-exp this-optres))))))]
                  [else (optres-exp this-optres)]))
              
              (define this-chap-code
                (and (or (not (optres-flat this-optres))
                         lazy?)
                     (with-syntax ([proc-name (string->symbol
                                               (format "~a-~a-chap" 
                                                       (syntax-e #'struct-id)
                                                       (syntax-e sel-id)))])
                       (if lazy?
                           #`(let ([proc-name
                                    (cache-λ (strct #,sub-val)
                                             #,this-body-code)])
                               proc-name)
                           #`(let ([answer (let ([#,sub-val 
                                                  (#,(id->sel-id #'struct-id sel-id)
                                                   #,(opt/info-val opt/info))])
                                             #,this-body-code)])
                               (let ([proc-name (λ (strct fld) answer)])
                                 proc-name))))))

              (define this-fo-code 
                (and (and (optres-flat this-optres)
                          (not lazy?))
                     #`(let ([#,sub-val 
                              (#,(id->sel-id #'struct-id sel-id)
                               #,(opt/info-val opt/info))])
                         #,this-body-code)))

              (values (if this-fo-code
                          (cons this-fo-code s-fo-code)
                          s-fo-code)
                      (if this-chap-code 
                          (list* this-chap-code (id->sel-id #'struct-id sel-id) s-chap-code)
                          s-chap-code)
                      (if dep-vars s-lifts (append (optres-lifts this-optres) s-lifts))
                      (if dep-vars s-super-lifts (append (optres-superlifts this-optres) s-super-lifts))
                      (if dep-vars s-partially-applied (append (optres-partials this-optres) s-partially-applied))
                      (and (optres-opt this-optres) can-be-optimized?)
                      (if dep-vars stronger-ribs (append (optres-stronger-ribs this-optres) stronger-ribs))
                      (combine-two-chaperone?s chaperone? (optres-chaperone this-optres))
                      (combine-two-no-negative-blame no-negative-blame (optres-no-negative-blame? this-optres)))))
          
          ;; to avoid having to deal with indy-ness, just give up if any
          ;; of the fields that are depended on can possibly raise negative blame
          (free-identifier-mapping-for-each
           depended-on-fields
           (λ (depended-on-id field-doing-the-depending)
             (define no-neg-blame (free-identifier-mapping-get no-negative-blame-fields depended-on-id))
             (define dep-answer (cond
                                  [(boolean? no-neg-blame) no-neg-blame]
                                  [else (traverse-no-neg-blame-identifiers no-neg-blame)]))
             (unless no-neg-blame
               (give-up 
                (format " because the contract on field: ~a depends on: ~a and its contract may have negative blame"
                        (syntax-e field-doing-the-depending)
                        (syntax-e depended-on-id))))))
          
          (with-syntax ([(stronger-prop-desc stronger-prop-pred? stronger-prop-get)
                         (syntax-local-lift-values-expression
                          3
                          #'(make-impersonator-property 'struct/dc-stronger-prop))]
                        [(free-var ...) (opt/info-free-vars opt/info)]
                        [(index ...) (build-list (length (opt/info-free-vars opt/info)) values)]
                        [pred? (list-ref info 2)])
            
            (build-optres
             #:exp
             (if (null? s-chap-code) ;; if this is #t, when we have to avoid putting the property on here.
                 #`(if (pred? #,(opt/info-val opt/info))
                       (begin
                         #,@s-fo-code
                         #,(opt/info-val opt/info))
                       (struct/dc-error blame #,(opt/info-val opt/info) 'struct-name))
                 #`(if (and (stronger-prop-pred? #,(opt/info-val opt/info))
                            (let ([v (stronger-prop-get #,(opt/info-val opt/info))])
                              (and (eq? (vector-ref v index) free-var) ...)))
                       #,(opt/info-val opt/info)
                       (if (pred? #,(opt/info-val opt/info))
                           (begin
                             #,@s-fo-code
                             (chaperone-struct
                              #,(opt/info-val opt/info)
                              #,@(reverse s-chap-code) ;; built the last backwards, so reverse it here
                              stronger-prop-desc
                              (vector free-var ...)))
                           (struct/dc-error blame #,(opt/info-val opt/info) 'struct-name))))
             #:lifts
             s-lifts
             #:superlifts
             s-super-lifts
             #:partials
             s-partially-applied
             #:flat #f  
             #:opt can-be-optimized?
             #:stronger-ribs stronger-ribs
             #:chaperone #t
             #:no-negative-blame? no-negative-blame))]))]))

(define (struct/dc-error blame obj what)
  (raise-blame-error blame obj 
                     '(expected "a struct of type ~a")
                     what))

(define-syntax (-struct/c stx)
  (syntax-case stx ()
    [(_ . args) 
     (with-syntax ([x (syntax/loc stx (struct/c . args))])
       (syntax/loc stx (#%expression x)))]))

(define-syntax (struct/c stx)
  (syntax-case stx ()
    [(_ struct-name args ...)
     (and (identifier? (syntax struct-name))
          (struct-info? (syntax-local-value (syntax struct-name) (λ () #f))))
     (let* ([si (extract-struct-info (syntax-local-value (syntax struct-name)))]
            [predicate-id (third si)]
            [selector-ids (reverse (fourth si))]
            [mutator-ids (reverse (fifth si))]
            [ctcs (syntax->list #'(args ...))]
            [ctc-names (generate-temporaries #'(args ...))])
       (unless (= (length selector-ids) (length ctcs))
         (raise-syntax-error 'struct/c 
                             (format "expected ~a contracts because struct ~a has ~a fields"
                                     (length selector-ids)
                                     (syntax-e #'struct-name)
                                     (length selector-ids))
                             stx))
       (unless predicate-id
         (raise-syntax-error 'struct/c 
                             (format "could not determine predicate for ~s" (syntax-e #'struct-name))
                             stx))
       (unless (andmap values selector-ids)
         (raise-syntax-error 'struct/c
                             (format "could not determine selectors for ~s" (syntax-e #'struct-name))
                             stx))
       
       (define strip-reg (regexp (format "^~a-" (regexp-quote (symbol->string (syntax-e #'struct-name))))))
       (define (selector-id->field sel)
         (datum->syntax sel
                        (string->symbol (regexp-replace strip-reg (symbol->string (syntax-e sel)) ""))))
       
       (do-struct/dc
        #t
        (with-syntax ([(fields ...) (map selector-id->field selector-ids)])
          #`(-struct/dc struct-name [fields args] ...))))]
    [(_ struct-name anything ...)
     (raise-syntax-error 'struct/c "expected a struct identifier" stx (syntax struct-name))]))