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
(struct dep   subcontract (dep-proc kind) #:transparent)

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
  ;; type : (or/c '#:impersonator '#:chaperone '#:flat)
  ;; sel-id : identifier?
  ;; deps : (listof identifier?)
  (struct clause (exp lazy? sel-id))
  (struct dep-clause clause (type deps))
  (struct indep-clause clause ()))

(define-syntax (struct/c stx)
  (syntax-case stx ()
    [(_ . args) 
     (with-syntax ([x (syntax/loc stx (do-struct/c . args))])
       (syntax/loc stx (#%expression x)))]))

(define (struct/c-name ctc)
  '(let ([ctcs (map second
                   (sort (append (base-struct/c-immutables ctc) (base-struct/c-mutables ctc))
                         < #:key first))])
    (apply build-compound-type-name 'struct/c (base-struct/c-name ctc) ctcs)))

(define (check-struct/c ctc)
  '(let ([name (base-struct/c-name ctc)]
        [pred? (base-struct/c-predicate ctc)]
        [ctc/ref-pairs (map (λ (l) (cons (second l) (third l)))
                            (append (base-struct/c-immutables ctc) (base-struct/c-mutables ctc)))])
    (λ (val fail [first-order? #f])
      (unless (pred? val)
        (fail "expected: ~s, got ~e" name val))
      (when first-order?
        (for ([p (in-list ctc/ref-pairs)])
          (let ([c (car p)] [v ((cdr p) val)])
            (unless (contract-first-order-passes? c v)
              (fail "expected: ~s, got ~e" (contract-name c) v)))))
      #t)))

(define (struct/c-first-order ctc)
  (let ([f (check-struct/c ctc)])
    (λ (val)
      (let/ec fail
        (f val (λ args (fail #f)) #t)))))


(define-syntax-rule
  (cache-λ (id ...) e)
  (let ([cached unique])
    (λ (id ...) 
      (cond [(eq? cached unique)
             (set! cached e)
             cached]
            [else cached]))))
 
(define (struct/c-proj ctc)
  (define sub-contracts (base-struct/c-sub-contracts ctc))
  (λ (blame) 
    (define swapped-blame (blame-swap blame))
    
    (define immutable-proj+refs
      (for/list ([sub-contract (in-list sub-contracts)]
                 #:when (immutable? sub-contract))
        (cons 
         (subcontract-ref sub-contract)
         ((contract-struct-projection (indep-ctc immutable)) 
          blame))))
    
    (define init-chaperone-args 
      (list struct/c-imp-prop-desc
            ctc))
    (define init-impersonator-args '())
    
    (for ([subcontract (in-list sub-contracts)])
      (cond
        [(lazy-immutable? subcontract)
         (define proj ((contract-struct-projection (indep-ctc subcontract)) blame))
         (set! init-chaperone-args (list* (subcontract-ref subcontract)
                                          (cache-λ (strct fld) (proj fld))
                                          init-chaperone-args))]
        [(mutable? subcontract)
         (define mk-proj (indep-ctc subcontract))
         (define get-proj (mk-proj blame))
         (define set-proj (mk-proj swapped-blame))
         (set! init-impersonator-args (list* (subcontract-ref subcontract)
                                             (λ (strct fld) (get-proj fld))
                                             (mutable-set subcontract)
                                             (λ (strct fld) (set-proj fld))
                                             init-impersonator-args))]))
    
    (λ (val)
      (cond
        [(and (struct/c-imp-prop-pred? val)
              (eq? (struct/c-imp-prop-get val) ctc))
         val]
        [else
         ;; need to check val is an instance of the right struct
         ;(checker val (λ args (apply raise-blame-error blame val args)))
         
         (define chaperone-args init-chaperone-args)
         (define impersonator-args init-impersonator-args)
         
         (for ([immutable-proj+ref (in-list immutable-proj+refs)])
           (define sel (car immutable-proj+ref))
           (define immutable-proj (cdr immutable-proj+ref))
           (define nv (immutable-proj (sel val)))
           (set! chaperone-args (list* sel
                                       (λ (strct fld) nv)
                                       chaperone-args)))
         
         (for ([sub-contract (in-list sub-contracts)])
           (cond
             [(dep-immutable? sub-contract)
              (define ctc ((dep-dep-proc sub-contract) val))
              (define ref (subcontract-ref sub-contract))
              (define proj ((contract-struct-projection ctc) blame))
              (cond
                [(flat-contract? ctc)
                 (proj (ref val))]
                [else
                 (define projected (proj (ref val)))
                 (cond
                   [(chaperone-contract? ctc)
                    (set! chaperone-args (list* ref
                                                (λ (strct fld) projected)
                                                chaperone-args))]
                   [else ;; impersonator contract
                    (error 'struct/dc 
                           "got an impersonator contract for the field ~a, but it is an immutable field"
                           (object-name ref))])])]
             [(dep-lazy-immutable? sub-contract)
              (define ctc ((dep-dep-proc sub-contract) val))
              (define ref (subcontract-ref sub-contract))
              (define proj ((contract-struct-projection ctc) blame))
              (cond
                [(chaperone-contract? ctc)
                 (set! chaperone-args (list* ref
                                             (cache-λ (strct fld) (proj fld))
                                             chaperone-args))]
                [else ;; impersonator contract
                 (error 'struct/dc 
                        "got an impersonator contract for the field ~a, but it is an immutable field"
                        (object-name ref))])]
             [(dep-mutable? sub-contract)
              (define ctc ((dep-dep-proc sub-contract) val))
              (define ref (subcontract-ref sub-contract))
              (define set (dep-mutable-set sub-contract))
              (define get-proj ((contract-struct-projection ctc) blame))
              (define set-proj ((contract-struct-projection ctc) swapped-blame))
              (cond
                [(chaperone-contract? ctc)
                 (set! chaperone-args (list* ref
                                             (λ (strct fld) (get-proj fld))
                                             set
                                             (λ (strct fld) (set-proj fld))
                                             chaperone-args))]
                [else ;; impersonator contract
                 (set! impersonator-args (list* ref
                                                (λ (strct fld) (get-proj fld))
                                                set
                                                (λ (strct fld) (set-proj fld))
                                                impersonator-args))])]))
         
         (define chaperoned-val
           (if (null? (cddr chaperone-args))
               val
               (apply chaperone-struct val chaperone-args)))
         (apply impersonate-struct
                chaperoned-val
                (if (and (null? (cddr chaperone-args))
                         (not (null? impersonator-args)))
                    (append impersonator-args
                            (list struct/c-imp-prop-desc ctc))
                    impersonator-args))]))))
  
;; name is symbol
;; predicate is (-> any bool)
;; immutables is (listof (list natural contract selector-proc))
;; mutables is (listof (list natural contract selector-proc mutator-proc))
(define-struct base-struct/c (name predicate sub-contracts))

(define-struct (flat-struct/c base-struct/c) ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name struct/c-name
   #:first-order struct/c-first-order
   #:projection struct/c-proj))

(define-struct (chaperone-struct/c base-struct/c) ()
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:name struct/c-name
     #:first-order struct/c-first-order
     #:projection struct/c-proj)))

(define-struct (impersonator-struct/c base-struct/c) ()
  #:property prop:contract
  (build-contract-property
   #:name struct/c-name
   #:first-order struct/c-first-order
   #:projection struct/c-proj))

(define-syntax (do-struct/c stx)
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
       
       (let ([combined-ids (for/list ([n (in-naturals)]
                                      [ctc-name (in-list ctc-names)]
                                      [ref-name (in-list selector-ids)]
                                      [mut-name (in-list mutator-ids)])
                             (list n ctc-name ref-name mut-name))])
         (let-values ([(mutables immutables) (partition (λ (l) (fourth l)) combined-ids)])
           (with-syntax ([(ctc-x ...) ctc-names]
                         [predicate-id predicate-id]
                         [((imm-count imm-ctc-x imm-ref _) ...) immutables]
                         [((mut-count mut-ctc-x mut-ref mut-set) ...) mutables])
             (syntax
              (let ([ctc-x (coerce-contract 'struct/c args)] ...)
                (let ([immutables (list (immutable imm-count imm-ctc-x imm-ref) ...)]
                      [mutables (list (mutable mut-count mut-ctc-x mut-ref mut-set) ...)])
                  (struct/c/proc 'struct-name predicate-id immutables mutables))))))))]
    [(_ struct-name anything ...)
     (raise-syntax-error 'struct/c "expected a struct identifier" stx (syntax struct-name))]))

(define (struct/c/proc struct-name predicate sub-contracts)
  (for ([sub-contract (in-list sub-contracts)]
        #:when (or (immutable? sub-contract)
                   (lazy-immutable? sub-contract)))
    (define imm-ctc (indep-ctc sub-contract))
    (unless (chaperone-contract? imm-ctc)
      (error 'struct/c
             "expected a chaperone contract for immutable field ~a, got ~e"
             (subcontract-field-name sub-contract)
             imm-ctc)))
  (cond
    [(and (not (ormap dep-mutable? sub-contracts))
          (not (ormap mutable? sub-contracts))
          (andmap flat-subcontract? sub-contracts))
     (make-flat-struct/c struct-name predicate sub-contracts)]
    [(andmap chaperone-subcontract? sub-contracts)
     (make-chaperone-struct/c struct-name predicate sub-contracts)]
    [else
     (make-impersonator-struct/c struct-name predicate sub-contracts)]))

(define (flat-subcontract? sc)
  (cond
    [(indep? sc) (flat-contract? (indep-ctc sc))]
    [(dep? sc) (eq? (dep-kind sc) 'flat)]))

(define (chaperone-subcontract? sc)
  (cond
    [(indep? sc) (chaperone-contract? (indep-ctc sc))]
    [(dep? sc) (or (eq? (dep-kind sc) 'chaperone)
                   (eq? (dep-kind sc) 'flat))]))

(define unique (box #f))
(define (un-dep ctc obj blame immutable-field)
  (let ([ctc (coerce-contract 'struct/dc ctc)])
    (when immutable-field
      (check-chaperone-contract immutable-field ctc))
    (((contract-projection ctc) blame) obj)))

(define (struct/dc-name ctc)
  (define info (struct/dc-name-info ctc))
  `(struct/dc ,(vector-ref info 0)
              #;
              ,@(for/list ([x (in-list (vector-ref info 1))]
                           [subctc (in-list (struct/dc-procs/ctcs ctc))])
                  `[,@(vector-ref x 1) 
                    ,(if (vector-ref x 0)
                         (contract-name subctc)
                         '...)])))

(define (struct/dc-first-order ctc)
  (struct/dc-pred ctc))


(define (struct/dc-proj ctc)
  (define pred? (struct/dc-pred ctc))
  (λ (blame)
    (define blames
      (for/list ([subcontract (in-list (struct/dc-subcontracts ctc))])
        (blame-add-context blame (format "the ~a field of" (subcontract-field-name subcontract)))))
    (define mut-blames
      (for/list ([subcontract (in-list (struct/dc-subcontracts ctc))])
        (blame-add-context blame (format "the ~a field of" (subcontract-field-name subcontract)) #:swap? #t)))
    (define indy-blames 
      (for/list ([subcontract (in-list (struct/dc-subcontracts ctc))])
        (blame-replace-negative
         (blame-add-context blame (format "the ~a field of" (subcontract-field-name subcontract)))
         (struct/dc-here ctc))))
    (define mut-indy-blames 
      (for/list ([subcontract (in-list (struct/dc-subcontracts ctc))])
        (blame-replace-negative
         (blame-add-context blame (format "the ~a field of" (subcontract-field-name subcontract))
                            #:swap? #t)
         (struct/dc-here ctc))))
    (define projs
      (for/list ([subcontract (in-list (struct/dc-subcontracts ctc))]
                 [blame+ctxt (in-list blames)])
        (cond
          [(indep? subcontract)
           (define sub-ctc (indep-ctc subcontract))
           ((contract-projection sub-ctc) blame+ctxt)]
          [else #f])))
    (define mut-projs
      (for/list ([subcontract (in-list (struct/dc-subcontracts ctc))]
                 [blame+ctxt (in-list mut-blames)])
        (cond
          [(and (indep? subcontract) (mutable? subcontract))
           (define sub-ctc (indep-ctc subcontract))
           ((contract-projection sub-ctc) blame+ctxt)]
          [else #f])))
    (define indy-projs
      (for/list ([subcontract (in-list (struct/dc-subcontracts ctc))]
                 [blame+ctxt (in-list indy-blames)])
        (cond
          [(indep? subcontract)
           (define sub-ctc (indep-ctc subcontract))
           ((contract-projection sub-ctc) blame+ctxt)]
          [else #f])))
    (define mut-indy-projs
      (for/list ([subcontract (in-list (struct/dc-subcontracts ctc))]
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
                              (struct/dc-struct-name ctc)))
         (let loop ([subcontracts (struct/dc-subcontracts ctc)]
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
                     (contract-projection 
                      (coerce-contract 
                       'struct/dc 
                       (apply (dep-dep-proc subcontract) dep-args)))))
              (define new-chaperone-args
                (cond
                  [(immutable? subcontract)
                   (list* sel
                          (let ([projd (proj (sel v))])
                            (λ (fld v) projd))
                          chaperone-args)]
                  [(lazy-immutable? subcontract)
                   (list* sel
                          (let ([cache unique])
                            (λ (fld v) 
                              (cond
                                [(eq? cache unique)
                                 (set! cache (proj v))
                                 cache]
                                [else cache])))
                          chaperone-args)]
                  [(mutable? subcontract)
                   (list* sel
                          (λ (fld v) (proj v))
                          (mutable-set subcontract)
                          (λ (fld v) (mut-proj v))
                          chaperone-args)]
                  [else
                   (define proj (dep-ctc blame))
                   (cond
                     [(dep-immutable? subcontract)
                      (list* sel
                             (let ([projd (proj (sel v))])
                               (λ (fld v) projd))
                             chaperone-args)]
                     [(dep-lazy-immutable? subcontract)
                      (list* sel
                             (let ([cached unique])
                               (λ (fld v) 
                                 (cond
                                   [(eq? cached unique)
                                    (set! cached (proj v))
                                    cached]
                                   [else
                                    cached])))
                             chaperone-args)]
                     [(dep-mutable? subcontract)
                      (define mut-proj (dep-ctc mut-blame))
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
                        (cons (if dep-ctc 
                                  ((dep-ctc indy-blame) ((subcontract-ref subcontract) v))
                                  (indy-proj ((subcontract-ref subcontract) v)))
                              dep-args)
                        dep-args))]))])))
  
  #;
  (begin
     (define pred? (struct/dc-pred ctc))
     (define mk-proj ((struct/dc-apply-proj ctc) ctc))
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
                                 (struct/dc-struct-name ctc)))
            (proj v)])))))

(define (struct/dc-stronger? this that)
  (and (struct/dc? that)
       (eq? (struct/dc-pred this) (struct/dc-pred that))
       (for/and ([this-subcontract (in-list (struct/dc-subcontracts this))]
                 [that-subcontract (in-list (struct/dc-subcontracts that))])
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
         
(define-struct struct/dc (subcontracts pred struct-name here name-info)
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:name struct/dc-name
     #:first-order struct/dc-first-order
     #:projection struct/dc-proj
     #:stronger struct/dc-stronger?)))

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
                     #`'#,(dep-clause-type clause))
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
  
  #`(make-struct/dc (list #,@structs)
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

#;
(define/opter (-struct/dc opt/i opt/info stx)
  (syntax-case stx ()
    [(_ struct-id clause ...)
     (let ()
       (define info (get-struct-info #'struct-id stx))
       (cond
         [(ormap values (list-ref info 4))
          ;; any mutable struct, just give up (could generate impersonator code, but
          ;; would have to check that the compiled subcontracts are all chaperones/flats)
          (opt/unknown opt/i opt/info stx)]
         [else
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
                  [(sel-id #:lazy (dep-id ...) exp) (values #'sel-id #t #'(dep-id ...) #'exp)]
                  [(sel-id (dep-id ...) exp) (values #'sel-id #f #'(dep-id ...) #'exp)]))
              
              (define-values (this-code 
                              this-lifts this-super-lifts this-partially-applied 
                              this-flat? this-can-be-optimized? this-stronger-ribs
                              this-chaperone?)
                (opt/i opt/info exp))
              
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
