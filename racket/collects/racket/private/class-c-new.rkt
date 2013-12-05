#lang racket/base
(require "class-internal.rkt"
         "../contract/base.rkt"
         "../contract/combinator.rkt"
         (only-in "../contract/private/guts.rkt"
                  wrapped-extra-arg-arrow?)
         (for-syntax racket/base
                     syntax/name))

(provide class/c2)

(define-for-syntax (parse-class/c stx)
  (define (give-up) (values #f #f #f #f #f))
  (define-values (opaque? args)
    (syntax-case stx ()
      [(_ #:opaque args ...)
       (values #t #'(args ...))]
      [(_ args ...)
       (values #f #'(args ...))]))
  (syntax-case args ()
    [(clauses ...)
     (let loop ([clauses (syntax->list #'(clauses ...))]
                [mths '()]
                [flds '()]
                [inits '()]
                [let-bindings '()])
       (cond
         [(null? clauses) 
          (values opaque?
                  (reverse mths)
                  (reverse flds)
                  (reverse inits)
                  (reverse let-bindings))]
         [else
          (syntax-case (car clauses) (field inherit inherit-field init init-field super inner 
                                            override augment augride absent)
            [(super . x) (give-up)]
            [(inner . x) (give-up)]
            [(override . x) (give-up)]
            [(augment . x) (give-up)]
            [(augride . x) (give-up)]
            [(absent . x) (give-up)]
            [(inherit . x) (give-up)]
            [(inherit-field . x) (give-up)]
            [(field x ...) (give-up)]
            [(init x ...) 
             (let ()
               (define new-let-bindings let-bindings)
               (define new-inits '())
               (for ([clause (in-list (syntax->list #'(x ...)))])
                 (syntax-case clause ()
                   [(id ctc)
                    (with-syntax ([(x) (generate-temporaries #'(id))])
                      (set! new-let-bindings (cons #`[x ctc] new-let-bindings))
                      (set! new-inits (cons #`[id x] new-inits)))]
                   [id
                    (identifier? #'id)
                    (begin
                      (set! new-inits (cons #`[id just-check-existence] new-inits)))]
                   [_
                    (raise-syntax-error 'class/c "expected a field-spec" stx clause)]))
               (loop (cdr clauses)
                     mths flds (append new-inits inits) new-let-bindings))]
            [(init-field x ...)
             (let ()
               (define new-let-bindings let-bindings)
               (define clauses '())
               (for ([cl (in-list (syntax->list #'(x ...)))])
                 (syntax-case cl ()
                   [(id ctc) 
                    (identifier? #'id)
                    (with-syntax ([(x) (generate-temporaries (list #'id))])
                      (set! new-let-bindings (cons #`[x ctc] new-let-bindings))
                      (set! clauses (cons #'[id x] clauses)))]
                   [id
                    (identifier? #'id)
                    (begin
                      (set! clauses (cons #'[id just-check-existence] clauses)))]
                   [_
                    (raise-syntax-error 'class/c "expected a field-spec" stx cl)]))
               (loop (cdr clauses) mths (append clauses flds) (append clauses inits)
                     new-let-bindings))]
                                           
            [x (identifier? #'x) 
               (loop (cdr clauses) 
                     (cons #`[#,(car clauses) just-check-existence] mths)
                     flds inits let-bindings)]
            [[mth ctc] 
             (identifier? #'mth)
             (with-syntax ([(x) (generate-temporaries #'(mth))])
               (loop (cdr clauses) 
                     (cons #`[mth x] mths)
                     flds
                     inits
                     (cons #`[x ctc] let-bindings)))]
            [else (give-up)])]))]))

(define-values (just-check-existence just-check-existence?)
  (let ()
    (struct just-check-existence ())
    (values (just-check-existence) 
            just-check-existence?)))

(define-syntax (class/c2 stx)
  (define-values (opaque? mths flds inits let-bindings) (parse-class/c stx))
  (cond
    [(and mths (null? flds))
     (syntax-case (list mths inits) ()
       [(((mth-name mth-ctc) ...)
         ((init-name init-ctc) ...))
        ;(printf " yup: ~a:~a\n" (syntax-source stx) (syntax-line stx))
        (with-syntax ([(lmth-name ...) (for/list ([m (in-list (syntax->list #'(mth-name ...)))])
                                         (localize m))]
                      [name (syntax-local-infer-name stx)])
          ;#'(class/c [m ctc] ...)
          #`(let #,let-bindings
              (make-an-ext-class/c-contract '#,opaque?
                                            (list `lmth-name ...)
                                            (list mth-ctc ...)
                                            (list 'init-name ...)
                                            (list init-ctc ...)
                                            'name)))])]
    [else
     ;(printf "nope: ~a:~a\n" (syntax-source stx) (syntax-line stx))
     (syntax-case stx ()
       [(_ args ...)
        #'(class/c args ...)])]))

(define (class/c2-proj this)
  (λ (blame)
    (λ (cls)
      (let/ec k
        (define (maybe-err neg-accepter)
          (if (blame-original? blame)
              (neg-accepter #f)
              (k neg-accepter)))
        (cond
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
              (define ctc
                (make-class/c
                 ;; methods
                 (map car mth-lst)
                 (map cdr mth-lst)
                 
                 '() '() ;; fields  
                 
                 ;; inits
                 (map (λ (x) (list-ref x 0)) (ext-class/c-contract-init-ctc-pairs this))
                 (map (λ (x) 
                        (define ctc (list-ref x 1))
                        (if (just-check-existence? ctc)
                            any/c
                            ctc))
                      (ext-class/c-contract-init-ctc-pairs this))
                 
                 
                 '() '() ;; inherit
                 '() '() ;; inherit fields
                 '() '() ;; super
                 '() '() ;; inner
                 '() '() ;; override
                 '() '() ;; augment
                 '() '() ;; augride
                 '() '() ;; absent
                 #f #f))
              (λ (neg-party)
                (((class/c-proj ctc) (blame-add-missing-party blame neg-party)) cls))]
             [else (build-neg-acceptor-proc this maybe-err blame cls (make-hash) '())])]
          [(wrapped-class? cls) 
           (define neg-acceptors-ht
             (wrapped-class-info-neg-acceptors-ht (wrapped-class-the-info cls)))
           (define wrapper-neg-party (wrapped-class-neg-party cls))
           (define new-mths-ht
             (for/hash ([(mth neg-acceptor) (in-hash neg-acceptors-ht)])
               (values mth (neg-acceptor wrapper-neg-party))))
           (define the-info (wrapped-class-the-info cls))
           (define fixed-neg-init-projs
             (for/list ([proj-pair (wrapped-class-info-init-proj-pairs the-info)])
               (cons (list-ref proj-pair 0)
                     (for/list ([func (in-list (cdr proj-pair))])
                       (λ (val) (λ (neg-party) 
                                  ((func val) wrapper-neg-party)))))))
           (build-neg-acceptor-proc this maybe-err blame 
                                    (wrapped-class-info-class the-info)
                                    new-mths-ht
                                    fixed-neg-init-projs)]
          [else
           (maybe-err
            (λ (neg-party)
              (raise-blame-error 
               blame #:missing-party neg-party cls
               '(expected: "a class"))))])))))

(define (build-neg-acceptor-proc this maybe-err blame cls new-mths-ht old-init-pairs)
  (define mth->idx (class-method-ht cls))
  (define mtd-vec (class-methods cls))
  
  (define (get-unwrapped-method name)
    (cond
      [(hash-ref new-mths-ht name #f) => values]
      [else
       (define mth-idx (hash-ref mth->idx name #f))
       (and mth-idx
            (vector-ref mtd-vec mth-idx))]))
  
  (define neg-extra-arg-ht (make-hash))
  (define neg-acceptors-ht (make-hash))
  
  (define (generic-wrapper mth)
    (define raw-proc (get-unwrapped-method mth))
    (make-keyword-procedure
     (λ (kwds kwd-args neg-party . args)
       (keyword-apply raw-proc kwds kwd-args args))
     (λ (neg-party . args)
       (apply raw-proc args))))
  
  (for ([(mth proj) (in-hash (ext-class/c-contract-table-of-meths-to-projs this))])
    (define m-mth (get-unwrapped-method mth))
    (unless m-mth
      (maybe-err
       (λ (neg-party)
         (raise-blame-error 
          blame #:missing-party neg-party cls
          '(expected: "a class with a public method named ~a")
          mth))))
    
    (cond
      [(just-check-existence? proj)
       ;; if we just check the method's existence,
       ;; then make an inefficient wrapper for it
       ;; that discards the neg-party argument
       (generic-wrapper mth)]
      [else
       (define w/blame (proj (blame-add-method-context blame mth)))
       (define projd-mth (w/blame m-mth))
       (hash-set! neg-acceptors-ht mth projd-mth)
       (define neg-acceptor
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
       (hash-set! neg-extra-arg-ht mth neg-acceptor)]))
  
  (for ([(mth _) (in-hash mth->idx)])
    ;; use a generic wrapper to drop the neg-party argument, which means
    ;; methods without explicit contracts are going to be slow
    (unless (hash-ref neg-extra-arg-ht mth #f)
      (if (ext-class/c-contract-opaque? this)
          (maybe-err
           (λ (neg-party)
             (define mth-names
               (for/list ([(mth proj) (in-hash (ext-class/c-contract-table-of-meths-to-projs this))])
                 (format " ~a" mth)))
             (raise-blame-error 
              blame #:missing-party neg-party cls
              '(expected: "~a" given: "a class that has a method ~a")
              (cond
                [(null? mth-names) "a class with no methods"]
                [(null? (cdr mth-names)) 
                 (format "a class with only one method:~a" (car mth-names))]
                [else
                 (format "a class with only the methods:~a" 
                         (apply string-append mth-names))])
              mth)))
          (hash-set! neg-extra-arg-ht mth (generic-wrapper mth)))))
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
  (define the-info (wrapped-class-info cls blame neg-extra-arg-ht neg-acceptors-ht 
                                       merged-init-pairs))
  (λ (neg-party)
    ;; run this for the side-effect of 
    ;; checking that first-order tests on
    ;; methods (arity, etc) all pass
    (for ([(mth neg-party-acceptor) (in-hash neg-acceptors-ht)])
      (neg-party-acceptor neg-party))
    (wrapped-class the-info neg-party)))

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

(define (make-an-ext-class/c-contract opaque? mth-names mth-ctcs init-names init-ctcs ctc-name)
  (define ctc-hash
    (make-hash (for/list ([raw-ctc (in-list mth-ctcs)]
                          [name (in-list mth-names)])
                 (define ctc (if (just-check-existence? raw-ctc)
                                 raw-ctc
                                 (coerce-contract 'class/c raw-ctc)))
                 (cons name ctc))))
  (ext-class/c-contract 
   opaque?
   ctc-hash
   (make-hash (for/list ([name (in-list mth-names)])
                (define ctc (hash-ref ctc-hash name))
                (cond
                  [(just-check-existence? ctc)
                   (cons name ctc)]
                  [else
                   (define proj (get/build-val-first-projection ctc))
                   (cons name proj)])))
   (for/list ([name (in-list init-names)]
              [ctc (in-list init-ctcs)])
     (list name
           (if (just-check-existence? ctc)
               ctc
               (coerce-contract 'class/c ctc))))
   ctc-name))

(define (class/c-first-order-passes? ctc cls)
  (cond
    [(class-struct-predicate? cls)
     (define mth->idx (class-method-ht cls))
     (define mtd-vec (class-methods cls))
     (for/and ([(name ctc) (in-hash (ext-class/c-contract-table-of-meths-to-ctcs ctc))])
       (define mth-idx (hash-ref mth->idx name #f))
       (and mth-idx
            (contract-first-order-passes? ctc (vector-ref mtd-vec mth-idx))))]
    [else #f]))

(struct ext-class/c-contract (opaque?
                              table-of-meths-to-ctcs 
                              table-of-meths-to-projs
                              init-ctc-pairs
                              name)
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
        (define init-names
          (for/list ([pr (in-list (ext-class/c-contract-init-ctc-pairs c))])
            (define name (list-ref pr 0))
            (define ctc (list-ref pr 1))
            (if (just-check-existence? ctc)
                name
                `[,name ,(contract-name ctc)])))
        (define meth-names
          (for/list ([(name ctc) (in-hash (ext-class/c-contract-table-of-meths-to-ctcs c))])
            (if (just-check-existence? ctc)
                name
                `[,name ,(contract-name ctc)])))
        `(class/c ,@(if (null? init-names)
                        (list)
                        (list `(init ,@init-names)))
                  ,@meth-names)]))))
