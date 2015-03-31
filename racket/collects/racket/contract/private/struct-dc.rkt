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
                     (rename-in
                      syntax/private/boundmap
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
         "opt.rkt"
         "generate.rkt")

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

;; dep-proc : procedure? -- pass the depended on fields's values
;;            values and get back a boolean that says whether 
;;            or not the invariant holds
;; fields : (listof symbol?) -- in reverse order that the 
;;          corresponding fields are evaluated (not necc. 
;;          the order specified in the contract itself)
;; muts : (listof mutator) -- the field mutators for mutable fields
;;        on which the invariant depends
(struct invariant (dep-proc fields sels muts) #:transparent)

(define (subcontract-mutable-field? x)
  (or (mutable? x)
      (dep-mutable? x)
      (dep-on-state-mutable? x)))

;; these are the compile-time structures, representing
;; parsed clauses of a struct/dc expression
(begin-for-syntax
  ;; d/i-clause's are the "normal" clauses in a struct/dc (field-spec) in the grammar
  ;; exp : syntax[boolean-valued expression]
  ;; lazy? : boolean
  ;; sel-id : identifier?
  (struct d/i-clause (exp lazy? sel-name sel-id) #:transparent)
  
  ;; type : (or/c '#:flat '#:chaperone '#:impersonator) 
  ;; depends-on-state? : boolean? -- only set if the keyword #:depends-on-state is passed
  ;; dep-names : (listof syntax?) -- the user's notation for the depended-on fields
  ;; dep-ids : (listof identifier?) -- the dependened on selector
  (struct dep-clause d/i-clause (type depends-on-state? dep-names dep-ids) #:transparent)
  
  (struct indep-clause d/i-clause () #:transparent)
  
  ;; inv-clauses come from the information following the #:inv keyword
  (struct inv-clause (exp dep-names dep-sel-ids dep-mut-ids))
  
  (define (has-deps? cl)
    (or (inv-clause? cl)
        (dep-clause? cl)))
  (define (get-dep-names cl)
    (cond
      [(inv-clause? cl) (inv-clause-dep-names cl)]
      [(dep-clause? cl) (dep-clause-dep-names cl)]))
  (define (get-dep-ids cl)
    (cond
      [(inv-clause? cl) (inv-clause-dep-sel-ids cl)]
      [(dep-clause? cl) (dep-clause-dep-ids cl)])))

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
  (define invariant-stuff '())
  (define field-stuff
    (apply
     append
     (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))])
       (cond
         [(invariant? subcontract)
          (set! invariant-stuff (list '#:inv (reverse (invariant-fields subcontract)) '...))
          '()]
         [(indep? subcontract)
          (if struct/c?
              (list (contract-name (indep-ctc subcontract)))
              (list `[,(subcontract-field-name subcontract)
                      ,@(if (lazy-immutable? subcontract)
                            '(#:lazy)
                            '())
                      ,(contract-name (indep-ctc subcontract))]))]
         [else
          (list `[,(subcontract-field-name subcontract) 
                  ,(dep-dep-names subcontract)
                  ,@(if (dep-lazy-immutable? subcontract)
                        '(#:lazy)
                        '())
                  ,@(if (eq? '#:chaperone (dep-type subcontract))
                        '()
                        (list (dep-type subcontract)))
                  ...])]))))
  `(,(if struct/c?
         'struct/c
         'struct/dc)
    ,(base-struct/dc-name-info ctc)
    ,@field-stuff
    ,@invariant-stuff))

(define (struct/dc-flat-first-order ctc)
  (define struct-pred? (base-struct/dc-pred ctc))
  (λ (v)
    (and (struct-pred? v) 
         (let loop ([subcs (base-struct/dc-subcontracts ctc)]
                    [args '()])
           (cond
             [(null? subcs) #t]
             [else
              (define subc (car subcs))
              (cond
                [(invariant? subc)
                 (and (apply (invariant-dep-proc subc) args)
                      (loop (cdr subcs) args))]
                [else
                 (define val ((subcontract-ref subc) v))
                 (define next-args 
                   (if (subcontract-depended-on? subc)
                       (cons val args)
                       args))
                 (cond
                   [(indep? subc)
                    (and ((flat-contract-predicate (indep-ctc subc)) val)
                         (loop (cdr subcs) next-args))]
                   [else
                    (and ((flat-contract-predicate (apply (dep-dep-proc subc) args)) val)
                         (loop (cdr subcs) next-args))])])])))))

(define ((struct/dc-generate ctc) fuel)
  (define constructor (base-struct/dc-constructor ctc))
  (and constructor
       (let loop ([subcs (base-struct/dc-subcontracts ctc)]
                  [gens '()])
         (cond
           [(null? subcs)
            (λ ()
              (let loop ([gens gens]
                         [args '()])
                (cond
                  [(null? gens) (apply constructor args)]
                  [else (loop (cdr gens)
                              (cons ((car gens)) args))])))]
           [else
            (define subc (car subcs))
            (cond
              [(invariant? subc) #f]
              [(indep? subc)
               (define sgen (contract-random-generate/choose (indep-ctc subc) fuel))
               (cond
                 [sgen (loop (cdr subcs) (cons sgen gens))]
                 [else #f])]
              [else #f])]))))

(define (struct/dc-first-order ctc)
  (base-struct/dc-pred ctc))

(define (struct/dc-proj ctc)
  (define pred? (base-struct/dc-pred ctc))
  (λ (blame)
    (define orig-blames
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))])
        (if (subcontract? subcontract)
            (blame-add-context 
             blame
             (format "the ~a field of" (subcontract-field-name subcontract)))
            blame)))
    (define orig-mut-blames
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))])
        (cond
          [(subcontract? subcontract)
           (define ctxt-string (format "the ~a field of" (subcontract-field-name subcontract)))
           (blame-add-context blame ctxt-string #:swap? #t)]
          [else #f])))
    (define orig-indy-blames 
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))])
        (and (subcontract? subcontract)
             (blame-replace-negative
              (blame-add-context 
               blame (format "the ~a field of" (subcontract-field-name subcontract)))
              (base-struct/dc-here ctc)))))
    (define orig-mut-indy-blames 
      (for/list ([subcontract (in-list (base-struct/dc-subcontracts ctc))])
        (and (subcontract? subcontract)
             (blame-replace-negative
              (blame-add-context blame (format "the ~a field of" (subcontract-field-name subcontract))
                                 #:swap? #t)
              (base-struct/dc-here ctc)))))
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
          [(indep? subcontract)
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
           (raise-blame-error blame v '(expected: "~a?" given: "~e")
                              (base-struct/dc-struct-name ctc)
                              v))
         (define invariant (for/or ([c (in-list (base-struct/dc-subcontracts ctc))])
                             (and (invariant? c)
                                  c)))
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
              (define (app* f v l) (if (null? l) v (apply f v l)))
              (app* chaperone-struct
                    (app* impersonate-struct
                          v
                          impersonate-args)
                    (if invariant
                        (add-invariant-checks blame invariant chaperone-args)
                        chaperone-args))]
             [else
              (define subcontract (car subcontracts)) ;; (or/c subcontract? invariant?)
              (define proj (car projs))
              (define mut-proj (car mut-projs))
              (define indy-proj (car indy-projs))
              (define mut-indy-proj (car mut-indy-projs))
              (define sel (and (subcontract? subcontract) (subcontract-ref subcontract)))
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
                  [(invariant? subcontract)
                   (unless (with-continuation-mark contract-continuation-mark-key blame
                             (apply (invariant-dep-proc subcontract) dep-args))
                     (raise-invariant-blame-failure blame v
                                                    (reverse dep-args)
                                                    (reverse (invariant-fields subcontract))))
                   (values chaperone-args impersonate-args)]
                  [(immutable? subcontract)
                   (define (chk fld v) (with-continuation-mark
                                           contract-continuation-mark-key blame
                                         (proj v)))
                   (chk #f (sel v)) ;; check the field contract immediately
                   (values (if (flat-contract? (indep-ctc subcontract))
                               chaperone-args
                               (list* sel chk chaperone-args))
                           impersonate-args)]
                  [(lazy-immutable? subcontract)
                   (values (list* sel
                                  (cache-λ (fld v)
                                           (with-continuation-mark
                                            contract-continuation-mark-key blame
                                            (proj v)))
                                  chaperone-args)
                           impersonate-args)]
                  [(mutable? subcontract)
                   (if (impersonator-contract? (indep-ctc subcontract))
                       (values chaperone-args
                               (list* sel
                                      (λ (fld v)
                                        (with-continuation-mark
                                         contract-continuation-mark-key blame
                                         (proj v)))
                                      (mutable-set subcontract)
                                      (λ (fld v)
                                        (with-continuation-mark
                                         contract-continuation-mark-key blame
                                         (mut-proj v)))
                                      impersonate-args))
                       (values (list* sel
                                      (λ (fld v)
                                        (with-continuation-mark
                                         contract-continuation-mark-key blame
                                         (proj v)))
                                      (mutable-set subcontract)
                                      (λ (fld v)
                                        (with-continuation-mark
                                         contract-continuation-mark-key blame
                                         (mut-proj v)))
                                      chaperone-args)
                               impersonate-args))]
                  [else
                   (define proj (dep-ctc-blame-proj blame))
                   (cond
                     [(dep-immutable? subcontract)
                      (define (chk fld v) (with-continuation-mark
                                              contract-continuation-mark-key blame
                                            (proj v)))
                      (chk #f (sel v)) ;; check the field contract immediately
                      (values (if (flat-contract? dep-ctc)
                                  chaperone-args
                                  (list* sel chk chaperone-args))
                              impersonate-args)]
                     [(dep-lazy-immutable? subcontract)
                      (values (list* sel
                                     (cache-λ (fld v)
                                              (with-continuation-mark
                                               contract-continuation-mark-key blame
                                               (proj v)))
                                     chaperone-args)
                              impersonate-args)]
                     [(dep-mutable? subcontract)
                      (define mut-proj (dep-ctc-blame-proj mut-blame))
                      (if (equal? (dep-type subcontract) '#:impersonator)
                          (values (list* sel
                                         (λ (fld v)
                                           (with-continuation-mark
                                            contract-continuation-mark-key blame
                                            (proj v)))
                                         (dep-mutable-set subcontract)
                                         (λ (fld v)
                                           (with-continuation-mark
                                            contract-continuation-mark-key blame
                                            (mut-proj v)))
                                         chaperone-args)
                                  impersonate-args)
                          (values chaperone-args
                                  (list* sel
                                         (λ (fld v)
                                           (with-continuation-mark
                                            contract-continuation-mark-key blame
                                            (proj v)))
                                         (dep-mutable-set subcontract)
                                         (λ (fld v)
                                           (with-continuation-mark
                                            contract-continuation-mark-key blame
                                            (mut-proj v)))
                                         impersonate-args)))]
                     [(dep-on-state-immutable? subcontract)
                      (proj (sel v))
                      (values (list* sel
                                     (λ (strct val)
                                       (with-continuation-mark
                                        contract-continuation-mark-key blame
                                        (build-dep-on-state-proj
                                         (base-struct/dc-subcontracts ctc) subcontract strct
                                         orig-indy-projs orig-indy-blames blame val)))
                                     chaperone-args)
                              impersonate-args)]
                     [(dep-on-state-mutable? subcontract)
                      (proj (sel v))
                      (define (get-chap-proc strct val)
                        (with-continuation-mark
                         contract-continuation-mark-key blame
                         (build-dep-on-state-proj (base-struct/dc-subcontracts ctc) subcontract strct
                                                  orig-indy-projs orig-indy-blames blame val)))
                      (define (set-chap-proc strct val)
                        (with-continuation-mark contract-continuation-mark-key blame
                          (build-dep-on-state-proj
                           (base-struct/dc-subcontracts ctc) subcontract strct
                           orig-mut-indy-projs orig-mut-indy-blames mut-blame val)))
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
                    (if (and (subcontract? subcontract) (subcontract-depended-on? subcontract))
                        (cons (if dep-ctc-blame-proj 
                                  ((dep-ctc-blame-proj indy-blame) ((subcontract-ref subcontract) v))
                                  (indy-proj ((subcontract-ref subcontract) v)))
                              dep-args)
                        dep-args))]))]))))

(define (check-invariant/mut blame invariant val sel field-v)
  (define args
    (let loop ([sels (invariant-sels invariant)]
               [args '()])
      (cond
        [(null? sels) args]
        [else 
         (define this-sel (car sels))
         (if (equal? this-sel sel)
             (loop (cdr sels) (cons field-v args))
             (loop (cdr sels) (cons (sel val) args)))])))
  (unless (apply (invariant-dep-proc invariant) args)
    (raise-invariant-blame-failure (blame-swap blame) val
                                   (reverse args)
                                   (reverse
                                    (invariant-fields invariant)))))

(define (raise-invariant-blame-failure blame v vals field-names)
  (raise-blame-error 
   blame
   v 
   "#:inv does not hold~a" 
   (apply
    string-append
    (if (null? field-names) "" " for:")
    (for/list ([dep-arg (in-list vals)]
               [field-name (in-list field-names)])
      (format "\n  ~a: ~e" field-name dep-arg)))))

(define (add-invariant-checks blame invariant chaperone-args)
  (let loop ([invariant-field-sels/muts
              (for/list ([sel (in-list (invariant-sels invariant))]
                         [mut (in-list (invariant-muts invariant))]
                         #:when mut)
                (cons sel mut))]
             [chaperone-args chaperone-args])
    (cond
      [(null? chaperone-args)
       (apply
        append
        (for/list ([sel/mut (in-list invariant-field-sels/muts)])
          (define sel (car sel/mut))
          (define mut (cdr sel/mut))
          (list mut
                (λ (stct field-v)
                  (check-invariant/mut blame invariant stct sel field-v)
                  field-v))))]
      [else
       (define fn (car chaperone-args))
       (define proc (cadr chaperone-args))
       (define sel #f)
       (define which (for/or ([i (in-naturals)]
                              [sel/mut (in-list invariant-field-sels/muts)])
                       (cond
                         [(equal? (cdr sel/mut) fn)
                          (set! sel (car sel/mut))
                          i]
                         [else #f])))
       (cond
         [which
          (list* fn
                 (λ (stct field-v)
                   (check-invariant/mut blame invariant stct sel field-v)
                   (proc stct field-v))
                 (loop (remove-ith invariant-field-sels/muts which)
                       (cddr chaperone-args)))]
         [else
          (list* fn proc
                 (loop invariant-field-sels/muts
                       (cddr chaperone-args)))])])))

(define (remove-ith l i)
  (cond
    [(null? l) '()]
    [else
     (if (= i 0)
         (cdr l)
         (cons (car l) (remove-ith (cdr l) (- i 1))))]))

(define (build-dep-on-state-proj orig-subcontracts this-subcontract strct projs blames blame val)
  (let loop ([subcontracts orig-subcontracts]
             [blames blames]
             [projs projs]
             [dep-args '()])
    (cond
      [(null? subcontracts) 
       (error 'build-dep-on-state-proj
              "ran out of subcontracts ~s ~s ~s"
              orig-subcontracts this-subcontract strct)]
      [else
       (define subcontract (car subcontracts))
       (cond
         [(eq? subcontract this-subcontract)
          (define the-ctc 
            (coerce-contract 'struct/dc (apply (dep-dep-proc this-subcontract) dep-args)))
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
            (if (and (subcontract? subcontract) (subcontract-depended-on? subcontract))
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
       (raise-argument-error
        'struct/dc
        (format "a flat-contract? for field ~a" (subcontract-field-name subcontract))
        dep-ctc))]
    [(#:chaperone)
     (unless (chaperone-contract? dep-ctc)
       (raise-argument-error
        'struct/dc
        (format "a chaperone-contract? for field ~a" (subcontract-field-name subcontract))
        dep-ctc))]))

(define (struct/dc-stronger? this that)
  (and (base-struct/dc? that)
       (eq? (base-struct/dc-pred this) (base-struct/dc-pred that))
       (let ([this-inv (get-invariant this)]
             [that-inv (get-invariant that)])
         (cond
           [(not that-inv) #t]
           [(not this-inv) #f]
           [else
            (procedure-closure-contents-eq? (invariant-dep-proc this-inv)
                                            (invariant-dep-proc that-inv))]))
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

(define (get-invariant sc)
  (for/or ([sub (base-struct/dc-subcontracts sc)]
           #:when (invariant? sub))
    sub))

(define-struct base-struct/dc (subcontracts constructor pred struct-name here name-info struct/c?))

(define (struct/dc-exercise stct)
  (λ (fuel)
    (define env (contract-random-generate-get-current-environment))
    (values
     (λ (val) 
       ;; need to extract the fields and do it in 
       ;; the right order to figure out the contracts
       ;; and then throw them into the environment
       (void))
     (map indep-ctc (filter indep? (base-struct/dc-subcontracts stct))))))

(define-struct (struct/dc base-struct/dc) ()
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:name struct/dc-name
     #:first-order struct/dc-first-order
     #:projection struct/dc-proj
     #:stronger struct/dc-stronger?
     #:generate struct/dc-generate
     #:exercise struct/dc-exercise)))

(define-struct (flat-struct/dc base-struct/dc) ()
  #:property prop:flat-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-flat-contract-property
     #:name struct/dc-name
     #:first-order struct/dc-flat-first-order
     #:projection struct/dc-proj
     #:stronger struct/dc-stronger?
     #:generate struct/dc-generate
     #:exercise struct/dc-exercise)))

(define-struct (impersonator-struct/dc base-struct/dc) ()
  #:property prop:contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-contract-property
     #:name struct/dc-name
     #:first-order struct/dc-first-order
     #:projection struct/dc-proj
     #:stronger struct/dc-stronger?
     #:generate struct/dc-generate
     #:exercise struct/dc-exercise)))

(define (build-struct/dc subcontracts constructor pred struct-name here name-info struct/c?)
  (for ([subcontract (in-list subcontracts)])
    (when (and (indep? subcontract)
               (not (mutable? subcontract)))
      (unless (chaperone-contract? (indep-ctc subcontract))
        (raise-argument-error
         'struct/dc
         (format "a chaperone-contract? for field ~a" (subcontract-field-name subcontract))
         (indep-ctc subcontract)))))
  (define (flat-subcontract? subcontract)
    (cond
      [(indep? subcontract) (flat-contract? (indep-ctc subcontract))]
      [(dep? subcontract) (equal? '#:flat (dep-type subcontract))]
      [(invariant? subcontract) #t]
      [else (error 'struct-dc.rkt "internal error")]))
  
  (define (impersonator-subcontract? subcontract)
    (cond
      [(indep? subcontract) (impersonator-contract? (indep-ctc subcontract))]
      [(dep? subcontract) (equal? '#:impersonator (dep-type subcontract))]
      [(invariant? subcontract) #f]
      [else (error 'struct-dc.rkt "internal error")]))
  (cond
    [(and (andmap flat-subcontract? subcontracts)
          (not (ormap lazy-immutable? subcontracts))
          (not (ormap dep-lazy-immutable? subcontracts))
          (not (ormap subcontract-mutable-field? subcontracts)))
     (make-flat-struct/dc subcontracts constructor pred struct-name here name-info struct/c?)]
    [(ormap impersonator-subcontract? subcontracts)
     (make-impersonator-struct/dc subcontracts constructor pred struct-name here name-info struct/c?)]
    [else
     (make-struct/dc subcontracts constructor pred struct-name here name-info struct/c?)]))
   

(define-for-syntax (get-struct-info id stx)
  (unless (identifier? id)
    (raise-syntax-error 'struct/dc "expected a struct name" stx id))
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
    [(_ id pre-clauses ...)
     (let ()
       (define info (get-struct-info #'id stx))
       (define (ensure-valid-field sel-name)
         (define selector-candidate (name->sel-id #'id sel-name))
         (unless (for/or ([selector (in-list (list-ref info 3))])
                   (and selector (free-identifier=? selector-candidate selector)))
           (raise-syntax-error 
            'struct/dc 
            (string-append 
             "expected an identifier that names a field or a sequence with a field name,"
             " the #:parent keyword, and the parent struct")
            stx
            sel-name)))
       
       (define (is-a-mutable-field? sel-name)
         (define mutator-candidate (name->mut-id stx #'id sel-name))
         (for/or ([mutator (in-list (list-ref info 4))])
           (and mutator (free-identifier=? mutator mutator-candidate))))
       
       (define (check-not-both this that)
         (when (and this that)
           (raise-syntax-error 'struct/dc
                               (format "found both ~a and ~a on the same field"
                                       (syntax-e this)
                                       (syntax-e that))
                               stx
                               that
                               (list this))))
       
       (define (sel-name? stx)
         (syntax-case stx ()
           [sel-id
            (identifier? #'sel-id) 
            #t]
           [(#:selector sel-id)
            (identifier? #'sel-id)
            #t]
           [(sel-id #:parent struct-id)
            (and (identifier? #'sel-id)
                 (identifier? #'struct-id))
            #t]
           [_else #f]))
       
       (define not-field-name-str
         (string-append "expected a field-name (either an identifier or a sequence:"
                        " (selector-id #:parent struct-id))"))
       
       (define-values (clauses invariant)
         (let loop ([pre-clauses (syntax->list #'(pre-clauses ...))]
                    [clauses '()])
           (cond
             [(null? pre-clauses) (values (reverse clauses) #f)]
             [else
              (define pre-clause (car pre-clauses))
              (cond
                [(keyword? (syntax-e pre-clause))
                 (unless (equal? '#:inv (syntax-e pre-clause))
                   (raise-syntax-error 
                    'struct/dc
                    "unknown keyword, expected only #:inv"
                    stx
                    pre-clause))
                 (when (null? (cdr pre-clauses))
                   (raise-syntax-error 
                    'struct/dc
                    "expected a sequence of identifiers and an invariant expression to follow #:inv"
                    stx
                    pre-clause))
                 (define sel-names-stx (cadr pre-clauses))
                 (define sel-names (syntax->list sel-names-stx))
                 (unless sel-names
                   (raise-syntax-error 
                    'struct/dc
                    "expected a sequence of identifiers to follow #:inv"
                    stx
                    sel-names-stx))
                 (for ([sel-name (in-list sel-names)])
                   (unless (sel-name? sel-name)
                     (raise-syntax-error 'struct/dc not-field-name-str stx sel-name)))
                 (unless (pair? (cddr pre-clauses))
                   (raise-syntax-error
                    'struct/dc
                    "expected a sequence of identifiers and an invariant expression to follow #:inv"
                    stx
                    pre-clause))
                 (define expr (caddr pre-clauses))
                 (unless (null? (cdddr pre-clauses))
                   (raise-syntax-error
                    'struct/dc
                    "expected only a sequence of identifiers and an invariant expression after #:inv"
                    stx
                    pre-clause))
                 (values (reverse clauses) 
                         (inv-clause expr 
                                     sel-names 
                                     (map (λ (name) (name->sel-id #'id name))
                                          sel-names)
                                     (map (λ (name) (and (is-a-mutable-field? name)
                                                         (name->mut-id stx #'id name)))
                                          sel-names)))]
                [else
                 (loop (cdr pre-clauses) (cons pre-clause clauses))])])))
       (define parsed-clauses
         (for/list ([clause (in-list clauses)])
           (syntax-case clause ()
             [(sel-name (dep-name ...) stuff1 . stuff)
              ;; need stuff1 here so that things like [a (>=/c x)] do not fall into this case
              (sel-name? #'sel-name)
              (let ()
                (for ([name (in-list (syntax->list #'(dep-name ...)))])
                  (unless (sel-name? name)
                    (raise-syntax-error 'struct/dc not-field-name-str stx name)))
                (ensure-valid-field #'sel-name)
                (define-values (ctc-exp lazy? type depends-on-state?)
                  (let loop ([stuff  #'(stuff1 . stuff)]
                             [lazy? #f]
                             [type #f]
                             [depends-on-state? #f])
                    (syntax-case stuff ()
                      [(exp) (values #'exp lazy? type depends-on-state?)]
                      [(flat/impersonator-kwd . more-stuff) 
                       (memq (syntax-e #'flat/impersonator-kwd) '(#:flat #:chaperone #:impersonator))
                       (begin
                         (check-not-both type (stx-car stuff))
                         (loop #'more-stuff lazy? (stx-car stuff) depends-on-state?))]
                      [(#:depends-on-state . more-stuff) (loop #'more-stuff lazy? type #t)]
                      [(#:lazy . more-stuff) (loop #'more-stuff #t type depends-on-state?)]
                      [_ (raise-syntax-error 'struct/dc "could not parse clause" stx clause)])))
                (when (and lazy? type (equal? '#:flat (syntax-e type)))
                  (raise-syntax-error 'struct/dc
                                      "cannot have #:lazy on a flat contract field"
                                      stx
                                      type))
                (dep-clause ctc-exp lazy?
                            #'sel-name (name->sel-id #'id #'sel-name)
                            (if type (syntax-e type) '#:chaperone)
                            depends-on-state?
                            (syntax->list #'(dep-name ...))
                            (map (λ (name) (name->sel-id #'id name))
                                 (syntax->list #'(dep-name ...)))))]
             [(sel-name . rest)
              (let ()
                (unless (sel-name? #'sel-name)
                  (raise-syntax-error 'struct/dc not-field-name-str stx #'sel-name))
                (ensure-valid-field #'sel-name)
                (define-values (lazy? exp)
                  (syntax-case #'rest ()
                    [(#:lazy exp) (values #t #'exp)]
                    [(exp) (values #f #'exp)]
                    [else (raise-syntax-error 'struct/dc "could not parse clause" stx clause)]))
                (indep-clause exp lazy? #'sel-name (name->sel-id #'id #'sel-name)))]
             [_ (raise-syntax-error 'struct/dc "could not parse clause" stx clause)])))
       
       (define all-clauses (if invariant (cons invariant parsed-clauses) parsed-clauses))
       
       (let ()
         (define lazy-mapping (make-free-identifier-mapping))
         (define mutable-mapping (make-free-identifier-mapping))
         
         (for ([clause (in-list all-clauses)])
           (when (d/i-clause? clause)
             (free-identifier-mapping-put! lazy-mapping 
                                           (d/i-clause-sel-id clause)
                                           (d/i-clause-lazy? clause))
             (free-identifier-mapping-put! mutable-mapping 
                                           (d/i-clause-sel-id clause)
                                           '(d/i-clause-mutable? clause))))
         
         ;; check that non-lazy don't depend on lazy
         (for ([clause (in-list all-clauses)])
           (when (has-deps? clause)
             (when (or (inv-clause? clause)
                       (not (d/i-clause-lazy? clause)))
               (for ([dep-id (in-list (get-dep-ids clause))]
                     [dep-name (in-list (get-dep-names clause))])
                 (when (free-identifier-mapping-get lazy-mapping dep-id (λ () #f))
                   (cond
                     [(d/i-clause? clause)
                      (raise-syntax-error 
                       #f
                       (format
                        "the dependent clause for field: ~s is not lazy, but depends on field: ~s"
                        (syntax->datum (d/i-clause-sel-name clause))
                        (syntax->datum dep-name))
                       stx
                       dep-id)]
                     [else
                      (raise-syntax-error 
                       #f
                       (format "the #:inv clause depends on field: ~s, but it is lazy"
                               (syntax->datum dep-name))
                       stx
                       dep-id)]))))))
         
         (for ([clause (in-list all-clauses)])
           (for ([sel (in-list (list-ref info 3))]
                 [mut (in-list (list-ref info 4))]
                 [i (in-naturals)])
             (when (or (and (inv-clause? clause)
                            (zero? i))
                       (and (d/i-clause? clause)
                            sel
                            (free-identifier=? sel 
                                               (d/i-clause-sel-id clause))))
               
               ;; check that fields depended on actually exist
               (when (has-deps? clause)
                 (for ([id (in-list (get-dep-ids clause))]
                       [dep-name (in-list (get-dep-names clause))])
                   (free-identifier-mapping-get
                    lazy-mapping
                    id
                    (λ () (raise-syntax-error 
                           'struct/dc
                           (format 
                            (string-append
                             "the field: ~s is depended on (by the ~a),"
                             " but it has no contract")
                            (syntax->datum dep-name)
                            (if (d/i-clause? clause)
                                (format "contract on the field: ~s"
                                        (syntax->datum (d/i-clause-sel-name clause)))
                                "#:inv clause"))
                           stx
                           (if (d/i-clause? clause)
                               (d/i-clause-sel-name clause)
                               dep-name))))))
               
               ;; check that impersonator fields are mutable
               (when (and (dep-clause? clause)
                          (eq? (dep-clause-type clause) '#:impersonator))
                 (unless mut
                   (raise-syntax-error 'struct/dc
                                       (format 
                                        (string-append
                                         "the field: ~a is immutable, so the contract"
                                         " cannot be an impersonator contract")
                                        (syntax-e (d/i-clause-sel-name clause)))
                                       stx
                                       (d/i-clause-sel-name clause))))

               ;; check that mutable fields aren't lazy
               (when (and (d/i-clause? clause) (d/i-clause-lazy? clause) mut)
                 (raise-syntax-error
                  'struct/dc 
                  (format "the field: ~s is mutable, so its contract cannot be lazy"
                          (syntax->datum (d/i-clause-sel-name clause)))
                  stx
                  (d/i-clause-sel-name clause)))))))
                          
       (values info #'id all-clauses))]))

;; name->sel-id : identifier syntax -> identifier
;; returns the identifier for the selector, where the 'id'
;; argument is either an identifier or a #'(id #:parent id)
;; configuration (something else must check this is a valid id)
(define-for-syntax (name->sel-id struct-id id)
  (define (combine struct-id id)
    (datum->syntax
     id
     (string->symbol
      (format "~a-~a" 
              (syntax-e struct-id)
              (syntax-e id)))))
  (syntax-case id ()
    [x 
     (identifier? #'x)
     (combine struct-id id)]
    [(#:selector sel-id)
     (identifier? #'sel-id)
     #'sel-id]
    [(sel-id #:parent parent-id)
     (combine #'parent-id #'sel-id)]))

(define-for-syntax (name->mut-id stx struct-id id)
  (define (combine struct-id id)
    (datum->syntax
     id
     (string->symbol
      (format "set-~a-~a!"
              (syntax-e struct-id)
              (syntax-e id)))))
  (syntax-case id ()
    [x 
     (identifier? #'x)
     (combine struct-id id)]
    [(#:selector sel-id)
     (identifier? #'sel-id)
     (raise-syntax-error
      'struct/dc
      "cannot use #:selector to choose a mutable field in an invariant declaration"
      stx
      id)]
    [(sel-id #:parent parent-id)
     (combine #'parent-id #'sel-id)]))

(define-for-syntax (top-sort/clauses stx clauses)
  (define id->children (make-free-identifier-mapping))
  
  (for ([clause (in-list clauses)])
    (when (d/i-clause? clause)
      (define id (d/i-clause-sel-id clause))
      (free-identifier-mapping-put! id->children id clause)))
  
  (define (neighbors x)
    (cond
      [(has-deps? x)
       (for/list ([id (in-list (get-dep-ids x))])
         (free-identifier-mapping-get id->children id
                                      (λ ()
                                        (raise-syntax-error 'struct/dc "unknown clause" stx id))))]
      [else '()]))
  
  (top-sort clauses neighbors
            (λ (leftovers)
              (raise-syntax-error 'struct/dc
                                  "found cyclic dependencies"
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
  

  ;; find-selector/mutator : d/i-clause -> (values identifier? identifier?)
  (define (find-selector/mutator clause)
    (define this-selector (d/i-clause-sel-id clause))
    (define mutator (for/or ([selector (in-list (list-ref info 3))]
                             [mutator (in-list (list-ref info 4))])
                      (and (free-identifier=? this-selector selector)
                           mutator)))
    (values this-selector mutator))
  
  ;; init the first three mappings above
  (for ([clause (in-list sorted-clauses)])
    (when (d/i-clause? clause)
      (define-values (sel mut) (find-selector/mutator clause))
      (free-identifier-mapping-put! mutable-clauses (d/i-clause-sel-id clause) (and mut #t))
      (free-identifier-mapping-put! sel-id->clause (d/i-clause-sel-id clause) clause))
    (when (has-deps? clause)
      (for ([var (in-list (get-dep-ids clause))])
        (free-identifier-mapping-put! depended-on-clauses var #t))))

  ;; init the dep-on-mutable-clauses mapping
  (for ([clause (in-list clauses)])
    (when (d/i-clause? clause)
      (let loop ([clause clause])
        (define sel-id (d/i-clause-sel-id clause))
        (define current (free-identifier-mapping-get dep-on-mutable-clauses sel-id (λ () 'unknown)))
        (cond
          [(equal? current 'unknown)
           (define ans 
             (or (free-identifier-mapping-get mutable-clauses sel-id)
                 (and (dep-clause? clause)
                      (or (dep-clause-depends-on-state? clause)
                          (for/or ([dep (in-list (dep-clause-dep-ids clause))])
                            (loop (free-identifier-mapping-get sel-id->clause dep)))))))
           (free-identifier-mapping-put! dep-on-mutable-clauses sel-id ans)
           ans]
          [else
           current]))))
  
  (define structs
    (let loop ([dep-args '()]
               [clauses sorted-clauses])
      (cond
        [(null? clauses) '()]
        [else
         (define clause (car clauses))
         (define-values (selector mutator)
           (if (d/i-clause? clause)
               (find-selector/mutator clause)
               (values #f #f)))
         (define subcontract-constructor
           (if (d/i-clause? clause)
               (if (dep-clause? clause)
                   (if (free-identifier-mapping-get dep-on-mutable-clauses (d/i-clause-sel-id clause))
                       (if (d/i-clause-lazy? clause)
                           (raise-syntax-error 
                            #f 
                            (format (string-append
                                     "the contract on field ~a depends on mutable state"
                                     " (possibly indirectly), so cannot be lazy")
                                    (syntax->datum (d/i-clause-sel-name clause)))
                            stx 
                            (d/i-clause-sel-name clause))
                           (if mutator
                               #'dep-on-state-mutable
                               #'dep-on-state-immutable))
                       (if (d/i-clause-lazy? clause)
                           #'dep-lazy-immutable
                           (if mutator
                               #'dep-mutable
                               #'dep-immutable)))
                   (if (d/i-clause-lazy? clause)
                       #'lazy-immutable
                       (if mutator
                           #'mutable
                           #'immutable)))
               'this-shouldnt-get-used))
         (define depended-on? (and (d/i-clause? clause)
                                   (free-identifier-mapping-get
                                    depended-on-clauses
                                    (d/i-clause-sel-id clause)
                                    (λ () #f))))
         
         (define (get-id name)
           (syntax-case name ()
             [x
              (identifier? #'x)
              name]
             [(x #:parent y)
              #'x]))
         
         (define subcontract-call
           (cond
             [(d/i-clause? clause)
              (define subcontract-args 
                (list #`'#,(d/i-clause-sel-name clause) selector depended-on?))
              (define indep/dep-args
                (cond
                  [(dep-clause? clause)
                   (list #`(λ (#,@dep-args) #,(d/i-clause-exp clause))
                         #`'(#,@(reverse dep-args))
                         #`'#,(dep-clause-type clause))]
                  [else
                   (list #`(coerce-contract 'struct/dc #,(d/i-clause-exp clause)))]))     
              #`(#,subcontract-constructor #,@subcontract-args
                                           #,@indep/dep-args
                                           #,@(if mutator
                                                  (list mutator)
                                                  '()))]
             [else #`(invariant (λ (#,@dep-args) #,(inv-clause-exp clause))
                                '#,dep-args
                                (list #,@(inv-clause-dep-sel-ids clause))
                                (list #,@(inv-clause-dep-mut-ids clause)))]))
         (cons subcontract-call
               (loop (if depended-on?
                         (cons (get-id (d/i-clause-sel-name clause)) dep-args)
                         dep-args)
                     (cdr clauses)))])))
  
  (syntax-property
   #`(build-struct/dc (list #,@structs)
                      #,(list-ref info 1)
                      #,(list-ref info 2)
                      '#,struct-id
                      (quote-module-name)
                      '#,struct-id
                      #,struct/c?)
   'disappeared-use
   (list (syntax-local-introduce struct-id))))

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
                                    s-partially-applied can-be-optimized?
                                    stronger-ribs chaperone? no-negative-blame)
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
              
              (define-values (sel-name lazy? dep-names exp)
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
              
              (define this-optres (opt/i 
                                   (opt/info-add-blame-context 
                                    (opt/info-change-val sub-val opt/info)
                                    (λ (blame-stx)
                                      #`(blame-add-struct-context #,blame-stx '#,sel-name)))
                                   exp))
              
              (define sel-id (name->sel-id #'struct-id sel-name))
              
              (when dep-names
                (for ([dep-name (in-list (syntax->list dep-names))])
                  (define dep-var (name->sel-id #'struct-id dep-name))
                  (free-identifier-mapping-put! depended-on-fields dep-var sel-id)))
              (free-identifier-mapping-put! no-negative-blame-fields sel-id
                                            (optres-no-negative-blame? this-optres))
              
              (define this-body-code 
                (cond
                  [dep-names
                   (with-syntax ([(sel ...) (map (λ (var) (name->sel-id #'struct-id var)) 
                                                 (syntax->list dep-names))]
                                 [(dep-var ...) (map (λ (x)
                                                       (syntax-case x ()
                                                         [x (identifier? #'x) #'x]
                                                         [(x #:parent y) #'x]))
                                                     (syntax->list dep-names))])
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
                     (with-syntax ([proc-name (string->symbol (format "~a-chap" sel-id))])
                       (if lazy?
                           #`(let ([proc-name
                                    (cache-λ (strct #,sub-val)
                                             #,this-body-code)])
                               proc-name)
                           #`(let ([proc-name (λ (strct #,sub-val) #,this-body-code)])
                               ;; check the field contract immediately
                               (proc-name #f (#,sel-id #,(opt/info-val opt/info)))
                               proc-name)))))

              (define this-fo-code 
                (and (and (optres-flat this-optres)
                          (not lazy?))
                     #`(let ([#,sub-val 
                              (#,sel-id
                               #,(opt/info-val opt/info))])
                         #,this-body-code)))

              (values (if this-fo-code
                          (cons this-fo-code s-fo-code)
                          s-fo-code)
                      (if this-chap-code 
                          (list* this-chap-code sel-id s-chap-code)
                          s-chap-code)
                      (if dep-names
                          s-lifts
                          (append (optres-lifts this-optres) s-lifts))
                      (if dep-names
                          s-super-lifts
                          (append (optres-superlifts this-optres) s-super-lifts))
                      (if dep-names 
                          s-partially-applied 
                          (append (optres-partials this-optres) s-partially-applied))
                      (and (optres-opt this-optres) can-be-optimized?)
                      (if dep-names
                          stronger-ribs
                          (append (optres-stronger-ribs this-optres) stronger-ribs))
                      (combine-two-chaperone?s chaperone? (optres-chaperone this-optres))
                      (combine-two-no-negative-blame no-negative-blame
                                                     (optres-no-negative-blame? this-optres)))))
          
          ;; to avoid having to deal with indy-ness, just give up if any
          ;; of the fields that are depended on can possibly raise negative blame
          (free-identifier-mapping-for-each
           depended-on-fields
           (λ (depended-on-id field-doing-the-depending)
             (define no-neg-blame 
               (free-identifier-mapping-get no-negative-blame-fields depended-on-id))
             (define dep-answer (cond
                                  [(boolean? no-neg-blame) no-neg-blame]
                                  [else (traverse-no-neg-blame-identifiers no-neg-blame)]))
             (unless no-neg-blame
               (give-up 
                (format 
                 (string-append " because the contract on field: ~a depends on: ~a and"
                                " its contract may have negative blame")
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
             ;; if this is #t, when we have to avoid putting the property on here.
             (if (null? s-chap-code) 
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
                           (struct/dc-error #,(opt/info-blame opt/info) #,(opt/info-val opt/info) 'struct-name))))
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

(define (blame-add-struct-context blame fld)
  (blame-add-context blame (format "the ~a field of" fld)))

(define (struct/dc-error blame obj what)
  (raise-blame-error blame obj 
                     '(expected: "a struct of type ~a")
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
     (let* ([si (extract-struct-info (syntax-local-value #'struct-name))]
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
       
       (define (selector-id->field sel i)
         (define candidate
           (let loop ([struct-id #'struct-name])
             (cond
               [(identifier? struct-id)
                (define si (extract-struct-info (syntax-local-value struct-id)))
                (define si-parent (sixth si))
                (cond
                  [(loop si-parent) => values]
                  [else
                   (define si-selectors (fourth si))
                   (cond
                     [(ormap (λ (x) (and x 
                                         (free-identifier=? x sel)
                                         (free-identifier=? (datum->syntax stx x)
                                                            sel)))
                             si-selectors)
                      (define strip-reg 
                        (regexp (format "^~a-" (regexp-quote (symbol->string (syntax-e struct-id))))))
                      (define field-name
                        (datum->syntax 
                         sel
                         (string->symbol (regexp-replace strip-reg
                                                         (symbol->string (syntax-e sel))
                                                         ""))))
                      (cond
                        [(free-identifier=? #'struct-name struct-id)
                         #`(#:selector #,sel)]
                        [else
                         #`(#,field-name #:parent #,struct-id)])]
                     [else #f])])]
               [else #f])))
         (unless candidate
           (raise-syntax-error 'struct/c
                               (format 
                                (string-append
                                 "could not find selector id for field ~a"
                                 " (counting from 0) in current scope")
                                i)
                               stx
                               sel))
         candidate)
       
       (do-struct/dc
        #t
        (with-syntax ([(fields ...) (for/list ([selector-id (in-list selector-ids)]
                                               [i (in-naturals)])
                                      (selector-id->field selector-id i))])
          #`(-struct/dc struct-name [fields args] ...))))]
    [(_ struct-name anything ...)
     (raise-syntax-error 'struct/c "expected a struct identifier" stx (syntax struct-name))]))
