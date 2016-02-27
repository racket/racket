#lang racket/base
(require "class-internal.rkt"
         "class-c-old.rkt"
         "class-wrapped.rkt"
         "../contract/base.rkt"
         "../contract/combinator.rkt"
         (only-in "../contract/private/guts.rkt"
                  wrapped-extra-arg-arrow?)
         (for-syntax racket/base
                     syntax/name
                     syntax/stx))

(provide class/c2)

(define-syntax (class/c2 stx)
  (define-values (opaque? args)
    (syntax-case stx ()
      [(_ #:opaque args ...)
       (values #t (syntax->list #'(args ...)))]
      [(_ args ...)
       (let ()
         (define stx-args (syntax->list #'(args ...)))
         (when (and (pair? stx-args) (keyword? (syntax-e (car stx-args))))
           (raise-syntax-error #f "unrecognized keyword" stx (car stx-args)))
         (values #f stx-args))]))
  (define-values (bindings pfs) (parse-class/c-specs args #f))
  (with-syntax ([methods #`(list #,@(reverse (hash-ref pfs 'methods null)))]
                [method-ctcs #`(list #,@(reverse (hash-ref pfs 'method-contracts null)))]
                [fields #`(list #,@(reverse (hash-ref pfs 'fields null)))]
                [field-ctcs #`(list #,@(reverse (hash-ref pfs 'field-contracts null)))]
                [(i ...) (reverse (hash-ref pfs 'inits null))]
                [(i-c ...) (reverse (hash-ref pfs 'init-contracts null))]
                [inherits #`(list #,@(reverse (hash-ref pfs 'inherits null)))]
                [inherit-ctcs #`(list #,@(reverse (hash-ref pfs 'inherit-contracts null)))]
                [inherit-fields #`(list #,@(reverse (hash-ref pfs 'inherit-fields null)))]
                [inherit-field-ctcs #`(list #,@(reverse (hash-ref pfs 'inherit-field-contracts
                                                                  null)))]
                [supers #`(list #,@(reverse (hash-ref pfs 'supers null)))]
                [super-ctcs #`(list #,@(reverse (hash-ref pfs 'super-contracts null)))]
                [inners #`(list #,@(reverse (hash-ref pfs 'inners null)))]
                [inner-ctcs #`(list #,@(reverse (hash-ref pfs 'inner-contracts null)))]
                [overrides #`(list #,@(reverse (hash-ref pfs 'overrides null)))]
                [override-ctcs #`(list #,@(reverse (hash-ref pfs 'override-contracts null)))]
                [augments #`(list #,@(reverse (hash-ref pfs 'augments null)))]
                [augment-ctcs #`(list #,@(reverse (hash-ref pfs 'augment-contracts null)))]
                [augrides #`(list #,@(reverse (hash-ref pfs 'augrides null)))]
                [augride-ctcs #`(list #,@(reverse (hash-ref pfs 'augride-contracts null)))]
                [absents #`(list #,@(reverse (hash-ref pfs 'absents null)))]
                [absent-fields #`(list #,@(reverse (hash-ref pfs 'absent-fields null)))])
    (with-syntax ([name 
                   ;; same as syntax-local-infer-name, except doesn't
                   ;; make a name up from the src loc; in that case,
                   ;; we just use the big ole (class/c  ...)-based name
                   (or (let loop ([prop (syntax-property stx 'inferred-name)])
                         (cond
                           [(symbol? prop) prop]
                           [(pair? prop) (or (loop (car prop))
                                             (loop (cdr prop)))]
                           [else #f]))
                       (syntax-local-name))]
                  [bindings bindings]
                  [opaque? opaque?])
      (syntax/loc stx
        (let bindings
          (make-an-ext-class/c-contract
           'opaque?
           methods method-ctcs
           fields field-ctcs
           (list i ...)
           (list i-c ...)
           absents 
           absent-fields
           'name
           (build-internal-class/c
            inherits inherit-ctcs
            inherit-fields inherit-field-ctcs
            supers super-ctcs
            inners inner-ctcs
            overrides override-ctcs
            augments augment-ctcs
            augrides augride-ctcs)))))))

(define (class/c2-proj this)
  (λ (blame)
    (λ (cls)
      (let/ec k
        (define (maybe-err neg-accepter)
          (if (blame-original? blame)
              (neg-accepter #f)
              (k neg-accepter)))
        (cond
          [(impersonator-prop:has-wrapped-class-neg-party? cls)
           (define wrapper-neg-party (impersonator-prop:get-wrapped-class-neg-party cls))
           (define the-info (impersonator-prop:get-wrapped-class-info cls))
           (define neg-acceptors (wrapped-class-info-neg-acceptors-ht the-info))
           (define mth->idx (class-method-ht cls))
           (define new-mths (make-vector (vector-length (class-methods cls)) #f))
           (for ([(mth neg-acceptor) (in-hash neg-acceptors)])
             (define mth-idx (hash-ref mth->idx mth))
             (vector-set! new-mths mth-idx (neg-acceptor wrapper-neg-party)))
           (define fixed-neg-init-projs
             (for/list ([proj-pair (wrapped-class-info-init-proj-pairs the-info)])
               (cons (list-ref proj-pair 0)
                     (for/list ([func (in-list (cdr proj-pair))])
                       (λ (val) (λ (neg-party) 
                                  ((func val) wrapper-neg-party)))))))
           (build-neg-acceptor-proc this maybe-err blame 
                                    cls
                                    new-mths
                                    fixed-neg-init-projs
                                    (wrapped-class-info-pos-field-projs the-info)
                                    (wrapped-class-info-neg-field-projs the-info))]
          [(class-struct-predicate? cls)
           (define mtd-vec (class-methods cls))
           (cond
             [(for/or ([x (in-vector mtd-vec)])
                (pair? x))
              ;; if we find what appears to be an interface contract
              ;; in the given class, then we fall back to the old-style
              ;; class/c contracts by making up a class/c record and 
              ;; handing it off to old-style class/c projection.
              (define mth-lst 
                (for/list ([(mth ctc) 
                            (in-hash (ext-class/c-contract-table-of-meths-to-ctcs this))])
                  (cons mth 
                        (if (just-check-existence? ctc) 
                            any/c
                            ctc))))
              
              (define fields 
                (for/list ([(fld ctc) (in-hash (ext-class/c-contract-table-of-flds-to-ctcs this))])
                  fld))
              (define field-ctcs
                (for/list ([(fld ctc) (in-hash (ext-class/c-contract-table-of-flds-to-ctcs this))])
                  (if (just-check-existence? ctc)
                      #f
                      ctc)))
              
              (define ctc
                (make-class/c
                 ;; methods
                 (map car mth-lst)
                 (map cdr mth-lst)
                 
                 fields field-ctcs
                 
                 ;; inits
                 (map (λ (x) (list-ref x 0)) (ext-class/c-contract-init-ctc-pairs this))
                 (map (λ (x) 
                        (define ctc (list-ref x 1))
                        (if (just-check-existence? ctc)
                            any/c
                            ctc))
                      (ext-class/c-contract-init-ctc-pairs this))
                 
                 (ext-class/c-contract-absent-methods this)
                 (ext-class/c-contract-absent-fields this)

                 (ext-class/c-contract-internal-ctc this)
                 (ext-class/c-contract-opaque? this)
                 (ext-class/c-contract-name this)))
              (λ (neg-party)
                (((class/c-late-neg-proj ctc) blame) cls neg-party))]
             [else 
              (build-neg-acceptor-proc this maybe-err blame cls #f '() 
                                       (make-hasheq) (make-hasheq))])]
          [else
           (maybe-err
            (λ (neg-party)
              (raise-blame-error 
               blame #:missing-party neg-party cls
               '(expected: "a class"))))])))))

(define (build-neg-acceptor-proc this maybe-err blame cls old-mths-vec old-init-pairs 
                                 old-pos-fld-ht old-neg-fld-ht)
  (define mth->idx (class-method-ht cls))
  (define mtd-vec (class-methods cls))
  
  (define internal-late-neg-proj
    (internal-class/c-late-neg-proj (ext-class/c-contract-internal-ctc this)))
  
  ;; The #f may survive if the method is just-check-existence or
  ;; if the contract doesn't mention the method (and it isn't opaque)
  (define neg-extra-arg-vec (make-vector (vector-length mtd-vec) #f))
  (define neg-acceptors-ht (make-hash))
  
  (define pos-field-projs (hash-copy old-pos-fld-ht))
  (define neg-field-projs (hash-copy old-neg-fld-ht))
  
  (for ([(mth-name proj) (in-hash (ext-class/c-contract-table-of-meths-to-projs this))])
    (define mth-idx (hash-ref mth->idx mth-name #f))
    (unless mth-idx
      (maybe-err
       (λ (neg-party)
         (raise-blame-error 
          blame #:missing-party neg-party cls
          '(expected: "a class with a public method named ~a")
          mth-name))))
    
    (unless (just-check-existence? proj)
      (define w/blame (proj (blame-add-method-context blame mth-name)))
      (define m-mth (if old-mths-vec
                        (or (vector-ref old-mths-vec mth-idx)
                            (vector-ref mtd-vec mth-idx))
                        (vector-ref mtd-vec mth-idx)))
      (define projd-mth (w/blame m-mth))
      (hash-set! neg-acceptors-ht mth-name projd-mth)
      (define neg-extra-arg
        (cond
          [(wrapped-extra-arg-arrow? projd-mth)
           (wrapped-extra-arg-arrow-extra-neg-party-argument projd-mth)]
          [else 
           ;; if some contract doesn't subscribe to the wrapped-extra-arg-arrow
           ;; protocol, then make an inefficient wrapper for it.
           (make-keyword-procedure
            (λ (kwds kwd-args neg-party . args)
              (keyword-apply (projd-mth neg-party) kwds kwd-args args))
            (λ (neg-party . args)
              (apply (projd-mth neg-party) args)))]))
      (vector-set! neg-extra-arg-vec mth-idx neg-extra-arg)))
  
  (define absent-methods (ext-class/c-contract-absent-methods this))
  (for ([(mth-name mth-idx) (in-hash mth->idx)])
    (when (member mth-name absent-methods)
      (maybe-err
       (λ (neg-party)
         (raise-blame-error 
          blame #:missing-party neg-party cls
          '(expected: "a class that does not have the method ~a")
          mth-name))))
    
    (when (ext-class/c-contract-opaque? this)
      (unless (hash-ref (ext-class/c-contract-table-of-meths-to-projs this) mth-name #f)
        (maybe-err
         (λ (neg-party)
           (define mth-names
             (for/list ([(mth proj) (in-hash (ext-class/c-contract-table-of-meths-to-projs this))])
               (format " ~a" mth)))
           (raise-blame-error 
            blame #:missing-party neg-party cls
            '(expected: "~a" given: "a class that has a method: ~a")
            (cond
              [(null? mth-names) "a class with no methods"]
              [(null? (cdr mth-names)) 
               (format "a class with only one method:~a" (car mth-names))]
              [else
               (format "a class with only the methods:~a" 
                       (apply string-append mth-names))])
            mth-name))))))
  
  (for ([(fld proj) (in-hash (ext-class/c-contract-table-of-flds-to-projs this))])
    (define field-ht (class-field-ht cls))
    (define fi (hash-ref field-ht fld #f))
    (unless fi
      (maybe-err
       (λ (neg-party)
         (raise-blame-error 
          blame #:missing-party neg-party cls
          '(expected: "a class with a public field named ~a")
          fld))))
    
    (unless (just-check-existence? proj)
      (define (update-ht field-projs field-info-internal-ref/set! swap?)
        (define prior (hash-ref field-projs fld (λ () (field-info-internal-ref/set! fi))))
        (define w-blame (proj (blame-add-field-context blame proj #:swap? swap?)))
        (hash-set! field-projs fld (cons w-blame prior)))
      (update-ht pos-field-projs field-info-internal-ref #f)
      (update-ht neg-field-projs field-info-internal-set! #t)))
  
  (define absent-fields (ext-class/c-contract-absent-fields this))
  (unless (null? absent-fields)
    (for ([(fld proj) (in-hash (class-field-ht cls))])
      (when (member fld absent-fields)
        (maybe-err
         (λ (neg-party)
           (raise-blame-error 
            blame #:missing-party neg-party cls
            '(expected: "a class that does not have the field ~a")
            fld))))))
  
  (when (ext-class/c-contract-opaque? this)
    (define allowed-flds (ext-class/c-contract-table-of-flds-to-projs this))
    (for ([(fld proj) (in-hash (class-field-ht cls))])
      (unless (hash-ref allowed-flds fld #f)
        (maybe-err
         (λ (neg-party)
           (define fld-names
             (for/list ([(fld proj) (in-hash allowed-flds)])
               (format " ~a" fld)))
           (raise-blame-error 
            blame #:missing-party neg-party cls
            '(expected: "~a" given: "a class that has the field: ~a")
            (cond
              [(null? fld-names) "a class with no fields"]
              [(null? (cdr fld-names))
               (format "a class with only one field:~a" (car fld-names))]
              [else
               (format "a class with only the fields:~a" 
                       (apply string-append fld-names))])
            fld))))))
      
  (define new-init-projs 
    (for/list ([ctc-pair (in-list (ext-class/c-contract-init-ctc-pairs this))])
      (define ctc (list-ref ctc-pair 1))
      (if (just-check-existence? ctc)
          (list (car ctc-pair)
                (λ (x) (λ (y) x)))
          (list (car ctc-pair)
                ((get/build-val-first-projection ctc) 
                 (blame-add-init-context blame (car ctc-pair)))))))
  (define merged-init-pairs (merge-init-pairs old-init-pairs new-init-projs))
  (define the-info (wrapped-class-info blame neg-extra-arg-vec neg-acceptors-ht
                                       pos-field-projs neg-field-projs
                                       merged-init-pairs))
  (define class+one-property 
    (chaperone-struct cls
                        set-class-orig-cls! (λ (a b) b)
                        impersonator-prop:wrapped-class-info
                        the-info))
  
  (λ (neg-party)
    ;; run this for the side-effect of 
    ;; checking that first-order tests on
    ;; methods (arity, etc) all pass
    (for ([(mth-name neg-party-acceptor) (in-hash neg-acceptors-ht)])
      (neg-party-acceptor neg-party))
    
    ;; XXX: we have to not do this;
    ;; (instead we should use just the-info)
    ;; the internal projection should run
    ;; on the class only when it is
    ;; time to instantiate it; not here
    (define class+one-property/adjusted
      (chaperone-struct ((internal-late-neg-proj blame) cls neg-party)
                        set-class-orig-cls! (λ (a b) b)
                        impersonator-prop:wrapped-class-info
                        the-info))
    
    (chaperone-struct class+one-property/adjusted
                      set-class-orig-cls! (λ (a b) b)
                      impersonator-prop:wrapped-class-neg-party
                      neg-party)))

(define (merge-init-pairs old-init-pairs new-init-pairs)
  (cond
    [(null? old-init-pairs) new-init-pairs]
    [else
     (define (leq? x y) (string<? (symbol->string (car x)) (symbol->string (car y))))
     (define (same? x y) (eq? (car x) (car y)))
     (let loop ([olds (sort old-init-pairs leq?)]
                [news (sort new-init-pairs leq?)])
       (cond
         [(null? olds) news]
         [(null? news) olds]
         [else
          (define old (car olds))
          (define new (car news))
          (cond
            [(same? old new)
             (cons (cons (car old) (append (cdr old) (cdr new)))
                   (loop (cdr olds) (cdr news)))]
            [(leq? old new)
             (cons old (loop (cdr olds) news))]
            [else
             (cons new (loop olds (cdr news)))])]))]))

(define (make-an-ext-class/c-contract opaque? 
                                      mth-names mth-ctcs
                                      fld-names fld-ctcs
                                      init-names init-ctcs 
                                      absent-methods absent-fields
                                      ctc-name internal-ctc)
  (define (build-a-ctc-table names ctcs)
    (make-hash (for/list ([raw-ctc (in-list ctcs)]
                          [name (in-list names)])
                 (define ctc (if (just-check-existence? raw-ctc)
                                 raw-ctc
                                 (coerce-contract 'class/c raw-ctc)))
                 (cons name ctc))))
  (define (build-a-proj-table hash names)
    (make-hash
     (for/list ([name (in-list names)])
       (define ctc (hash-ref hash name))
       (cond
         [(just-check-existence? ctc)
          (cons name ctc)]
         [else
          (define proj (get/build-val-first-projection ctc))
          (cons name proj)]))))
  (define mth-ctc-hash (build-a-ctc-table mth-names mth-ctcs))
  (define fld-ctc-hash (build-a-ctc-table fld-names fld-ctcs))
  (define mth-proj-hash (build-a-proj-table mth-ctc-hash mth-names))
  (define fld-proj-hash (build-a-proj-table fld-ctc-hash fld-names))
  (ext-class/c-contract 
   opaque?
   mth-ctc-hash mth-proj-hash
   fld-ctc-hash fld-proj-hash
   (for/list ([name (in-list init-names)]
              [ctc (in-list init-ctcs)])
     (list name
           (if (just-check-existence? ctc)
               ctc
               (coerce-contract 'class/c ctc))))
   absent-methods absent-fields
   ctc-name
   internal-ctc))

(define (class/c-first-order-passes? ctc cls)
  (cond
    [(class-struct-predicate? cls)
     (define mth->idx (class-method-ht cls))
     (define mtd-vec (class-methods cls))
     (for/and ([(name ctc) (in-hash (ext-class/c-contract-table-of-meths-to-ctcs ctc))])
       (define mth-idx (hash-ref mth->idx name #f))
       (cond
         [mth-idx
          (define mth-record (vector-ref mtd-vec mth-idx))
          (contract-first-order-passes? 
           ctc 
           (if (pair? mth-record)
               (car mth-record)
               mth-record))]
         [else #f]))]
    [else #f]))

(struct ext-class/c-contract (opaque?
                              table-of-meths-to-ctcs 
                              table-of-meths-to-projs
                              table-of-flds-to-ctcs
                              table-of-flds-to-projs
                              init-ctc-pairs
                              absent-methods absent-fields
                              name
                              internal-ctc)
  #:property prop:contract
  (build-contract-property
   #:projection 
   (λ (c) (λ (blame) (λ (v) ((((class/c2-proj c) blame) v) #f))))
   #:val-first-projection class/c2-proj
   #:first-order
   (λ (ctc)
     (λ (cls)
       (class/c-first-order-passes? ctc cls)))
   #:name
   (λ (c)
     (cond
       [(ext-class/c-contract-name c) => values]
       [else
        (define field-names
          (for/list ([(fld ctc) (in-hash (ext-class/c-contract-table-of-flds-to-ctcs c))])
            (if (just-check-existence? ctc)
                fld
                `(,fld ,(contract-name ctc)))))
        (define init-fields '())
        (define init-names
          (filter
           values
           (for/list ([pr (in-list (ext-class/c-contract-init-ctc-pairs c))])
             (define name (list-ref pr 0))
             (define ctc (list-ref pr 1))
             (cond
               [(just-check-existence? ctc)
                name]
               [else 
                (define c-name (contract-name ctc))
                (define clause `[,name ,c-name])
                (define fld-ctc (hash-ref (ext-class/c-contract-table-of-flds-to-ctcs c) name #f))
                (cond
                  [(and fld-ctc (equal? c-name (contract-name fld-ctc)))
                   (set! init-fields (cons clause init-fields))
                   #f]
                  [else clause])]))))
        (set! field-names (filter (λ (x) (or (not (pair? x))
                                             (not (member (car x) (map car init-fields)))))
                                  field-names))
        
        (define meth-names
          (for/list ([(name ctc) (in-hash (ext-class/c-contract-table-of-meths-to-ctcs c))])
            (if (just-check-existence? ctc)
                name
                `[,name ,(contract-name ctc)])))
        
        (define absents
          (let ([ams (ext-class/c-contract-absent-methods c)]
                [afs (ext-class/c-contract-absent-fields c)])
            (cond
              [(and (null? ams) (null? afs)) '()]
              [(null? afs) (list `(absent ,@ams))]
              [else (list `(absent ,@ams (field ,@afs)))])))
        
        `(class/c ,@(if (null? init-names)
                        (list)
                        (list `(init ,@init-names)))
                  ,@(if (null? field-names)
                        (list)
                        (list `(field ,@field-names)))
                  ,@(if (null? init-fields)
                        (list)
                        (list `(init-field ,@init-fields)))
                  ,@meth-names
                  ,@absents
                  ,@(class/c-internal-name-clauses (ext-class/c-contract-internal-ctc c)))]))))
