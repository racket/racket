#lang racket/base

(provide (rename-out [-struct/dc struct/dc]))

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
(struct dep   subcontract (dep-proc flat?) #:transparent)

(struct immutable          indep ()    #:transparent)
(struct lazy-immutable     indep ()    #:transparent)
(struct mutable            indep (set) #:transparent)

(struct dep-immutable      dep   ()    #:transparent)
(struct dep-lazy-immutable dep   ()    #:transparent)
(struct dep-mutable        dep   (set) #:transparent)


;; these are the compile-time structures, representing
;; parsed clauses of a struct/dc expression
(begin-for-syntax
  ;; exp : syntax
  ;; lazy? : boolean
  ;; type : (or/c '#:chaperone '#:flat)
  ;; sel-id : identifier?
  ;; deps : (listof identifier?)
  (struct clause (exp lazy? sel-id))
  (struct dep-clause clause (type deps))
  (struct indep-clause clause ()))

(define-syntax-rule
  (cache-λ (id ...) e)
  (let ([cached unique])
    (λ (id ...) 
      (cond [(eq? cached unique)
             (set! cached e)
             cached]
            [else cached]))))

(define unique (box #f))
(define (un-dep ctc obj blame immutable-field)
  (let ([ctc (coerce-contract 'struct/dc ctc)])
    (when immutable-field
      (check-chaperone-contract immutable-field ctc))
    (((contract-projection ctc) blame) obj)))

(define (struct/dc-name ctc)
  (define info (base-struct/dc-name-info ctc))
  `(struct/dc ,(vector-ref info 0)
              #;
              ,@(for/list ([x (in-list (vector-ref info 1))]
                           [subctc (in-list (struct/dc-procs/ctcs ctc))])
                  `[,@(vector-ref x 1) 
                    ,(if (vector-ref x 0)
                         (contract-name subctc)
                         '...)])))

(define (struct/dc-first-order ctc)
  (base-struct/dc-pred ctc))


(define (struct/dc-proj ctc)
  (define pred? (base-struct/dc-pred ctc))
  (λ (blame)
    (define blames
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))])
        (blame-add-context blame (format "the ~a field of" (subcontract-field-name subcontract)))))
    (define mut-blames
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))])
        (blame-add-context blame (format "the ~a field of" (subcontract-field-name subcontract)) #:swap? #t)))
    (define indy-blames 
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))])
        (blame-replace-negative
         (blame-add-context blame (format "the ~a field of" (subcontract-field-name subcontract)))
         (base-struct/dc-here ctc))))
    (define mut-indy-blames 
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))])
        (blame-replace-negative
         (blame-add-context blame (format "the ~a field of" (subcontract-field-name subcontract))
                            #:swap? #t)
         (base-struct/dc-here ctc))))
    (define projs
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))]
                 [blame+ctxt (in-list blames)])
        (cond
          [(indep? subcontract)
           (define sub-ctc (indep-ctc subcontract))
           ((contract-projection sub-ctc) blame+ctxt)]
          [else #f])))
    (define mut-projs
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))]
                 [blame+ctxt (in-list mut-blames)])
        (cond
          [(and (indep? subcontract) (mutable? subcontract))
           (define sub-ctc (indep-ctc subcontract))
           ((contract-projection sub-ctc) blame+ctxt)]
          [else #f])))
    (define indy-projs
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))]
                 [blame+ctxt (in-list indy-blames)])
        (cond
          [(indep? subcontract)
           (define sub-ctc (indep-ctc subcontract))
           ((contract-projection sub-ctc) blame+ctxt)]
          [else #f])))
    (define mut-indy-projs
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))]
                 [blame+ctxt (in-list mut-indy-blames)])
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
           (raise-blame-error blame v "expected a ~a"
                              (base-struct/dc-struct-name ctc)))
         (let loop ([subcontracts (base-struct/dc-subcontracts ctc)]
                    [projs projs]
                    [mut-projs mut-projs]
                    [indy-projs indy-projs]
                    [mut-indy-projs mut-indy-projs]
                    [blames blames]
                    [mut-blames mut-blames]
                    [indy-blames indy-blames]
                    [mut-indy-blames mut-indy-blames]
                    [chaperone-args '()]
                    [dep-args '()])
           (cond
             [(null? subcontracts) 
              (apply chaperone-struct v chaperone-args)]
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
              (when dep-ctc
                (unless (chaperone-contract? dep-ctc)
                  (error 'struct/dc "expected a chaperone contract for the immutable field ~a, got ~e" 
                         (subcontract-field-name subcontract)
                         dep-ctc)))
              (define dep-ctc-blame-proj (and dep-ctc (contract-projection dep-ctc)))
              (define new-chaperone-args
                (cond
                  [(immutable? subcontract)
                   (list* sel
                          (let ([projd (proj (sel v))])
                            (λ (fld v) projd))
                          chaperone-args)]
                  [(lazy-immutable? subcontract)
                   (list* sel
                          (cache-λ (fld v) (proj v))
                          chaperone-args)]
                  [(mutable? subcontract)
                   (list* sel
                          (λ (fld v) (proj v))
                          (mutable-set subcontract)
                          (λ (fld v) (mut-proj v))
                          chaperone-args)]
                  [else
                   (define proj (dep-ctc-blame-proj blame))
                   (cond
                     [(dep-immutable? subcontract)
                      (list* sel
                             (let ([projd (proj (sel v))])
                               (λ (fld v) projd))
                             chaperone-args)]
                     [(dep-lazy-immutable? subcontract)
                      (list* sel
                             (cache-λ (fld v) (proj v))
                             chaperone-args)]
                     [(dep-mutable? subcontract)
                      (define mut-proj (dep-ctc-blame-proj mut-blame))
                      (list* sel
                             (λ (fld v) (proj v))
                             (mutable-set subcontract)
                             (λ (fld v) (mut-proj v))
                             chaperone-args)])]))
              (loop (cdr subcontracts) 
                    (cdr projs)  (cdr mut-projs)  (cdr indy-projs)  (cdr mut-indy-projs) 
                    (cdr blames) (cdr mut-blames) (cdr indy-blames) (cdr mut-indy-blames)
                    new-chaperone-args
                    (if (subcontract-depended-on? subcontract)
                        (cons (if dep-ctc-blame-proj 
                                  ((dep-ctc-blame-proj indy-blame) ((subcontract-ref subcontract) v))
                                  (indy-proj ((subcontract-ref subcontract) v)))
                              dep-args)
                        dep-args))]))])))
  
  #;
  (begin
     (define pred? (base-struct/dc-pred ctc))
     (define mk-proj ((base-struct/dc-apply-proj ctc) ctc))
     (λ (blame)
       (define proj (mk-proj blame))
       (λ (v)
         (cond
           [(and (struct/c-imp-prop-pred? v)
                 (contract-stronger? (struct/c-imp-prop-get v) ctc))
            v]
           [else
            (unless (pred? v)
              (raise-blame-error blame v "expected a ~a"
                                 (base-struct/dc-struct-name ctc)))
            (proj v)])))))

(define (struct/dc-stronger? this that)
  (and (struct/dc? that)
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

(define-struct base-struct/dc (subcontracts pred struct-name here name-info))

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

(define (build-struct/dc subcontracts pred struct-name here name-info)
  (for ([subcontract (in-list subcontracts)])
    (when (indep? subcontract)
      (unless (chaperone-contract? (indep-ctc subcontract))
        (error 'struct/dc "expected chaperone contracts, but field ~a has ~e"
               (subcontract-field-name subcontract)
               (indep-ctc subcontract)))))
  (define (flat-subcontract? subcontract)
    (cond
      [(indep? subcontract) (flat-contract? (indep-ctc subcontract))]
      [(dep? subcontract) (dep-flat? subcontract)]))
  ((if (andmap flat-subcontract? subcontracts)
       make-flat-struct/dc
       make-struct/dc)
   subcontracts pred struct-name here name-info))
   

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
       (values
        info
        #'id
        (for/list ([clause (in-list (syntax->list #'(clauses ...)))])
          (syntax-case clause ()
            [(sel-id (id ...) stuff1 . stuff) ;; need stuff1 here so that things like [a (>=/c x)] do not fall into this case
             (let ()
               (unless (identifier? #'sel-id)
                 (raise-syntax-error #f "expected an identifier (naming a field)" stx #'sel-id))
               (for ([id (in-list (syntax->list #'(id ...)))])
                 (unless (identifier? id)
                   (raise-syntax-error #f "expected an identifier (naming a field)" stx id)))
               (define-values (ctc-exp lazy? type)
                 (let loop ([stuff  #'(stuff1 . stuff)]
                            [lazy? #f]
                            [type #f])
                   (syntax-case stuff ()
                     [(exp) (values #'exp 
                                    lazy? 
                                    (string->symbol
                                     (keyword->string
                                      (if type (syntax-e type) '#:chaperone))))]
                     [(#:lazy . stuff) (loop #'stuff #t type)]
                     [(#:flat . more-stuff) 
                      (when type (raise-syntax-error #f (format "found both #:flat and ~a" (syntax-e type))
                                                     stx
                                                     #f
                                                     (list type (stx-car stuff))))
                      (loop #'stuff lazy? (stx-car stuff))]
                     [(#:impersonator . more-stuff) 
                      (when type (raise-syntax-error #f (format "found both #:impersonator and ~a" (syntax-e type))
                                                     stx
                                                     #f
                                                     (list type (stx-car stuff))))
                      (loop #'more-stuff lazy? (stx-car stuff))]
                     [(#:depends-on-state . more-stuff)
                      (raise-syntax-error #f "#:depends-on-state not yet implemented" stx (stx-car stuff))]
                     [_ (raise-syntax-error #f "could not parse clause" stx clause)])))
               (dep-clause ctc-exp lazy? #'sel-id type (syntax->list #'(id ...))))]
            [(sel-id . rest)
             (let ()
               (unless (identifier? #'sel-id)
                 (raise-syntax-error #f "expected an identifier (naming a field)" stx #'sel-id))
               (define-values (lazy? exp)
                 (syntax-case #'rest ()
                   [(#:lazy exp) (values #t #'exp)]
                   [(exp) (values #f #'exp)]
                   [else (raise-syntax-error #f "could not parse clause" stx clause)]))
               (indep-clause exp lazy? #'sel-id))]
            [_ (raise-syntax-error #f "could not parse clause" stx #'clause)]))))]))
           

(define-for-syntax (clause->chap-proc struct-id info stx clause-stx)
  (define sel-id (syntax-case clause-stx ()
                   [(sel-id . rest) #'sel-id]))
  (define (add-prefix id)
    (datum->syntax id
                   (string->symbol (format "~a-~a" 
                                           (syntax-e sel-id)
                                           (syntax-e id)))))
  (define immutable-field
    (for/or ([mutator (in-list (list-ref info 4))]
             [selector (in-list (list-ref info 3))])
      (cond
        [(and (not mutator) (not selector))
         ;; end, with some hidden info
         ;; just assume not immutable
         #f]
        [else
         (and (not mutator)
              (let ([id (id->sel-id struct-id sel-id)])
                (and (free-identifier=? id selector)
                     id)))])))
  (define (add-immutable-check ctc-id stx)
    (if immutable-field
        (list stx
              #`(check-chaperone-contract '#,immutable-field #,ctc-id))
        (list stx)))

  (syntax-case clause-stx ()
    ;; with caching
    [(sel-id #:lazy (id ...) exp)
     (with-syntax ([(dep-sel-id ...) (map (λ (x) (id->sel-id struct-id x)) (syntax->list #'(id ...)))])
       (with-syntax ([dep-proc (add-prefix #'dep-proc)])
         #`(((define dep-proc (λ (id ...) #,(defeat-inlining #'exp))))
            (begin)
            (begin)
            (begin)
            (let ([cached unique])
              (λ (strct fld)
                (if (eq? cached unique)
                    (begin
                      (set! cached (un-dep (dep-proc (dep-sel-id strct) ...) fld blame '#,immutable-field))
                      cached)
                    cached)))
            #(#f (sel-id #:lazy (id ...)))
            )))]
    [(sel-id (id ...) exp)
     (with-syntax ([(dep-sel-id ...) (map (λ (x) (id->sel-id struct-id x)) (syntax->list #'(id ...)))])
       (with-syntax ([dep-proc (add-prefix #'dep-proc)])
         #`(((define dep-proc (λ (id ...) #,(defeat-inlining #'exp))))
            (begin)
            (begin)
            (un-dep (dep-proc (dep-sel-id v) ...) (#,(id->sel-id struct-id #'sel-id) v) blame '#,immutable-field)
            (λ (strct fld)
              (un-dep (dep-proc (dep-sel-id strct) ...) fld blame '#,immutable-field))
            #(#f (sel-id (id ...)))
            )))]
    [(sel-id #:lazy exp)
     (with-syntax ([ctc (add-prefix #'ctc)]
                   [blame-to-proj (add-prefix #'blame-to-proj)]
                   [proj (add-prefix #'proj)])
       #`(#,(add-immutable-check #'ctc #'(define ctc (coerce-contract 'struct/dc exp)))
          (define blame-to-proj (contract-struct-projection ctc))
          (define proj (blame-to-proj blame))
          (begin)
          (let ([cached unique])
            (λ (strct fld)
              (if (eq? cached unique)
                  (begin
                    (set! cached (proj fld))
                    cached)
                  cached)))
          #(#t (sel-id #:lazy))))]
    [(sel-id exp)
     (with-syntax ([ctc (add-prefix #'ctc)]
                   [blame-to-proj (add-prefix #'blame-to-proj)]
                   [proj (add-prefix #'proj)])
       #`(#,(add-immutable-check #'ctc #'(define ctc (coerce-contract 'struct/dc exp)))
          (define blame-to-proj (contract-struct-projection ctc))
          (define proj (blame-to-proj blame))
          (proj (#,(id->sel-id struct-id #'sel-id) v))
          (if (flat-contract? ctc)
              (λ (strct fld) fld)
              (λ (strct fld) (proj fld)))
          #(#t (sel-id))))]
    [_ (raise-syntax-error #f "malformed clause" stx clause-stx)]))

(define (check-chaperone-contract immutable-field ctc)
  (unless (chaperone-contract? ctc)
    (error 'struct/dc "expected a chaperone contract for the immutable field ~a, got ~e" 
           (if (number? immutable-field)
               (format "number ~a (counting from 0)" immutable-field)
               immutable-field)
           ctc)))

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
         (free-identifier-mapping-get id->children id))]
      [else '()]))
  
  (top-sort clauses neighbors))

(define-syntax (-struct/dc stx)
  (define-values (info struct-id clauses) (parse-struct/dc stx))
  (define sorted-clauses (top-sort/clauses stx clauses))
  
  ;; maps the sel-ids to #t when they are depended on
  (define depended-on-clauses (make-free-identifier-mapping))
  (for ([clause (in-list sorted-clauses)])
    (when (dep-clause? clause)
      (for ([var (in-list (dep-clause-deps clause))])
        (free-identifier-mapping-put! depended-on-clauses var #t))))
  
  
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
               (if (clause-lazy? clause)
                   #'dep-lazy-immutable
                   (if mutator
                       #'dep-immutable
                       #'dep-immutable))
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
                     (eq? (dep-clause-type clause) '#:flat))
               (list #`(coerce-contract 'struct/dc #,(clause-exp clause)))))
         (cons #`(#,subcontract-constructor #,@subcontract-args
                                            #,@indep/dep-args
                                            #,@(if mutator
                                                   (list mutator)
                                                   '()))
               (loop (if depended-on?
                         (cons (clause-sel-id clause) dep-args)
                         '())
                     (cdr clauses)))])))
  
  #`(build-struct/dc (list #,@structs)
                     #,(list-ref info 2)
                     'struct-id
                     (quote-module-name)
                     '#(struct-id 'missing-name-information-in-struct/dc))
                    
  
  #;
  (syntax-case stx ()
    [(_ struct-id clause ...)
     (let ()
       (define info (get-struct-info #'struct-id stx))
       (with-syntax ([(((before-ctc-bound ...) after-ctc-bound after-blame-bound first-order-check chap-proc name-info) ...)
                      (for/list ([clause (in-list (syntax->list #'(clause ...)))])
                        (clause->chap-proc #'struct-id info stx clause))])
         (with-syntax ([(id ...) (syntax-case #'((before-ctc-bound ...) ...) ()
                                   [(((define id exp) . whatever) ...) #'(id ...)])]
                       [(selectors+chap-procs ...)
                        (apply
                         append
                         (for/list ([clause (in-list (syntax->list #'(clause ...)))]
                                    [chap-proc (in-list (syntax->list #'(chap-proc ...)))])
                           (list (id->sel-id
                                  #'struct-id
                                  (syntax-case clause ()
                                    [(x . rest) #'x]))
                                 chap-proc)))])
           #`(let ()
               before-ctc-bound ... ...
               (letrec ([me
                         (make-struct/dc
                          (λ (ctc)
                            after-ctc-bound ...
                            (λ (blame)
                              after-blame-bound ...
                              (λ (v)
                                first-order-check ...
                                (chaperone-struct
                                 v
                                 selectors+chap-procs ...
                                 struct/c-imp-prop-desc
                                 me))))
                          (list id ...)
                          #,(list-ref info 2)
                          'struct-id
                          (quote-module-name)
                          '#(struct-id (name-info ...))
                          )])
                 me)))))]))

(define/opter (-struct/dc opt/i opt/info stx)
  (syntax-case stx ()
    [(_ struct-id clause ...)
     (let/ec k
       (define info (get-struct-info #'struct-id stx))
       (define (give-up) 
         (call-with-values (λ () (opt/unknown opt/i opt/info stx)) 
                           k))
       (cond
         [(ormap values (list-ref info 4))
          ;; any mutable struct, just give up
          (give-up)]
         [else
          (define depended-on-fields (make-free-identifier-mapping))
          (define flat-fields (make-free-identifier-mapping))
          (define-values (s-chap-code s-flat-code s-lifts s-super-lifts s-partially-applied can-be-optimized? stronger-ribs chaperone?)
            (for/fold ([s-chap-code '()]
                       [s-flat-code '()]
                       [s-lifts '()]
                       [s-super-lifts '()]
                       [s-partially-applied '()]
                       [can-be-optimized? #t]
                       [stronger-ribs '()]
                       [chaperone? #t])
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
              
              (define-values (this-code 
                              this-lifts this-super-lifts this-partially-applied 
                              this-flat? this-can-be-optimized? this-stronger-ribs
                              this-chaperone?)
                (opt/i opt/info exp))
              
              (when dep-vars
                (for ([dep-var (in-list (syntax->list dep-vars))])
                  (free-identifier-mapping-put! depended-on-fields dep-var #t)))
              (free-identifier-mapping-put! flat-fields sel-id this-flat?)
              
              (values (cond
                        [(and this-flat? (not lazy?) (not dep-vars))
                         s-chap-code]
                        [else
                         (with-syntax ([(strct cache) (generate-temporaries '(struct cache))]
                                       [proc-name (string->symbol
                                                   (format "~a-~a-chap/dep" 
                                                           (syntax-e #'struct-id)
                                                           (syntax-e sel-id)))])
                           (list* (cond
                                    [dep-vars
                                     (with-syntax ([(sel ...) (map (λ (var) (id->sel-id #'struct-id var)) 
                                                                   (syntax->list dep-vars))]
                                                   [(dep-var ...) dep-vars])
                                       (with-syntax ([this-code+lifts
                                                      #`(let ([dep-var (sel strct)] ...)
                                                          #,(bind-superlifts
                                                             this-super-lifts
                                                             (bind-lifts
                                                              this-lifts
                                                              (bind-lifts
                                                               this-partially-applied
                                                               this-code))))])
                                         (if lazy?
                                             #`(let ([cache unique])
                                                 (let ([proc-name
                                                        (λ (strct #,(opt/info-val opt/info)) 
                                                          (cond
                                                            [(eq? cache unique)
                                                             (set! cache this-code+lifts)
                                                             cache]
                                                            [else cache]))])
                                                   proc-name))
                                             #`(let ([proc-name
                                                      (λ (strct #,(opt/info-val opt/info)) 
                                                        this-code+lifts)])
                                                 proc-name))))]
                                    [else
                                     (if lazy?
                                         #`(let ([cache unique])
                                             (let ([proc-name
                                                    (λ (strct #,(opt/info-val opt/info))
                                                      (cond
                                                        [(eq? cache unique)
                                                         (set! cache #,this-code)
                                                         cache]
                                                        [else cache]))])
                                               proc-name))
                                         #`(let ([proc-name
                                                  (λ (strct #,(opt/info-val opt/info))
                                                    #,this-code)])
                                             proc-name))])
                                  (id->sel-id #'struct-id sel-id)
                                  s-chap-code))])
                      (cond
                        [lazy?
                         s-flat-code]
                        [dep-vars
                         (with-syntax ([(sel ...) (map (λ (var) (id->sel-id #'struct-id var)) 
                                                       (syntax->list dep-vars))]
                                       [(dep-var ...) dep-vars])
                           (cons #` (let ([dep-var (sel #,(opt/info-val opt/info))] ...)
                                      (let ([#,(opt/info-val opt/info) (#,(id->sel-id #'struct-id sel-id)
                                                                        #,(opt/info-val opt/info))])
                                        #,this-code))
                                 s-flat-code))]
                        [else
                         (cons #`(let ([#,(opt/info-val opt/info) (#,(id->sel-id #'struct-id sel-id)
                                                                   #,(opt/info-val opt/info))])
                                   #,this-code)
                               s-flat-code)])
                      (if dep-vars s-lifts (append this-lifts s-lifts))
                      (if dep-vars s-super-lifts (append this-super-lifts s-super-lifts))
                      (if dep-vars s-partially-applied (append this-partially-applied s-partially-applied))
                      (and this-can-be-optimized? can-be-optimized?)
                      (append this-stronger-ribs stronger-ribs)
                      (and this-chaperone? chaperone?))))
          
          ;; to avoid having to deal with indy-ness, just give up if any
          ;; of the fields that are depended on aren't flat
          (free-identifier-mapping-for-each
           depended-on-fields
           (λ (depended-on-id flat?)
             (unless (free-identifier-mapping-get flat-fields depended-on-id)
               (give-up))))
          
          (with-syntax ([(stronger-prop-desc stronger-prop-pred? stronger-prop-get)
                         (syntax-local-lift-values-expression
                          3
                          #'(make-impersonator-property 'struct/dc-stronger-prop))]
                        [(free-var ...) (opt/info-free-vars opt/info)]
                        [(index ...) (build-list (length (opt/info-free-vars opt/info)) values)]
                        [pred? (list-ref info 2)])
            (values #`(if (and (stronger-prop-pred? #,(opt/info-val opt/info))
                               (let ([v (stronger-prop-get #,(opt/info-val opt/info))])
                                 (and (eq? (vector-ref v index) free-var) ...)))
                          #,(opt/info-val opt/info)
                          (if (pred? #,(opt/info-val opt/info))
                              (begin
                                #,@(reverse s-flat-code) ;; built the last backwards, so reverse it here
                                (chaperone-struct
                                 #,(opt/info-val opt/info)
                                 #,@(reverse s-chap-code) ;; built the last backwards, so reverse it here
                                 stronger-prop-desc
                                 (vector free-var ...)))
                              (struct/dc-error blame #,(opt/info-val opt/info) 'struct-name)))
                    s-lifts
                    s-super-lifts
                    s-partially-applied
                    #f  ;; flat sexp
                    can-be-optimized?
                    stronger-ribs
                    #t  ;;chaperone?
                    ))]))]))

(define (struct/dc-error blame obj what)
  (raise-blame-error blame obj 
                     "expected a struct of type ~a"
                     what))
