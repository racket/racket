(module kw '#%kernel
  (#%require "define.rkt"
             "small-scheme.rkt"
             "more-scheme.rkt"
             (only '#%unsafe
                   unsafe-chaperone-procedure
                   unsafe-impersonate-procedure)
             (for-syntax '#%kernel
                         '#%unsafe
                         "procedure-alias.rkt"
                         "stx.rkt"
                         "small-scheme.rkt"
                         "stxcase-scheme.rkt"
                         "member.rkt"
                         "name.rkt"
                         "norm-define.rkt"
                         "qqstx.rkt"
                         "sort.rkt"
                         "kw-prop-key.rkt"))

  (#%provide new-lambda new-λ
             new-define
             new-app
             make-keyword-procedure
             keyword-apply
             procedure-keywords
             new:procedure-reduce-arity
             procedure-reduce-keyword-arity
             new-prop:procedure
             new:procedure->method
             new:procedure-rename
             new:chaperone-procedure
             (protect new:unsafe-chaperone-procedure)
             new:impersonate-procedure
             (protect new:unsafe-impersonate-procedure)
             new:chaperone-procedure*
             new:impersonate-procedure*
             (for-syntax kw-expander? kw-expander-impl kw-expander-proc
                         syntax-procedure-alias-property 
                         syntax-procedure-converted-arguments-property))

  ;; ----------------------------------------

  (define-values (prop:keyword-impersonator keyword-impersonator? keyword-impersonator-ref)
    (make-struct-type-property 'keyword-impersonator))
  (define (keyword-procedure-impersonator-of v)
    (cond
     [(keyword-impersonator? v) ((keyword-impersonator-ref v) v)]
     [else #f]))

  (define-values (struct:keyword-procedure mk-kw-proc keyword-procedure?
                                           keyword-procedure-ref keyword-procedure-set!)
    (make-struct-type 'keyword-procedure #f 4 0 #f
                      (list (cons prop:checked-procedure #t)
                            (cons prop:impersonator-of keyword-procedure-impersonator-of))
                      (current-inspector)
                      #f
                      '(0 1 2 3)))
  (define keyword-procedure-checker (make-struct-field-accessor keyword-procedure-ref 0))
  (define keyword-procedure-proc (make-struct-field-accessor keyword-procedure-ref 1))
  (define keyword-procedure-required (make-struct-field-accessor keyword-procedure-ref 2))
  (define keyword-procedure-allowed (make-struct-field-accessor keyword-procedure-ref 3))

  (define-values (struct:keyword-method make-km keyword-method? km-ref km-set!)
    (make-struct-type 'procedure
                      struct:keyword-procedure
                      0 0 #f
                      (list (cons prop:method-arity-error #t))))

  (define (generate-arity-string proc)
    (let-values ([(req allowed) (procedure-keywords proc)]
                 [(a) (procedure-arity proc)]
                 [(keywords-desc)
                  (lambda (opt req)
                    (format "~a with keyword~a~a"
                            (if (null? (cdr req))
                                (format "an ~aargument" opt)
                                (format "~aarguments" opt))
                            (if (null? (cdr req))
                                ""
                                "s")
                            (case (length req)
                              [(1) (format " ~a" (car req))]
                              [(2) (format " ~a and ~a" (car req) (cadr req))]
                              [else
                               (let loop ([req req])
                                 (if (null? (cdr req))
                                     (format " and ~a" (car req))
                                     (format " ~a,~a" (car req)
                                             (loop (cdr req)))))])))]
                 [(method-adjust)
                  (lambda (a)
                    (if (or (okm? proc)
                            (keyword-method? proc))
                        (if (zero? a) 0 (sub1 a))
                        a))])

      (string-append
       (cond
        [(number? a) 
         (let ([a (method-adjust a)])
           (format "~a" a))]
        [(arity-at-least? a)
         (let ([a (method-adjust (arity-at-least-value a))])
           (format "at least ~a" a))]
        [else
         "a different number"])
       (if (null? req)
           ""
           (format " plus ~a" (keywords-desc "" req)))
       (if allowed
           (let ([others (let loop ([req req][allowed allowed])
                           (cond
                            [(null? req) allowed]
                            [(eq? (car req) (car allowed))
                             (loop (cdr req) (cdr allowed))]
                            [else
                             (cons (car allowed) (loop req (cdr allowed)))]))])
             (if (null? others)
                 ""
                 (format " plus ~a"
                         (keywords-desc "optional " others))))
           " plus arbitrary keyword arguments"))))

  ;; Constructor for a procedure with only optional keywords.
  ;; The `procedure' property dispatches to a procedure in the 
  ;; struct (which has exactly the right arity).
  (define-values (struct:okp make-optional-keyword-procedure okp? okp-ref okp-set!)
    (make-struct-type 'procedure
                      struct:keyword-procedure
                      1 0 #f
                      (list (cons prop:arity-string generate-arity-string))
                      (current-inspector) 0))

  ;; A ``method'' (for arity reporting)
  (define-values (struct:okm make-optional-keyword-method okm? okm-ref okm-set!)
    (make-struct-type 'procedure
                      struct:okp
                      0 0 #f
                      (list (cons prop:method-arity-error #t))))

  (define-values (prop:named-keyword-procedure named-keyword-procedure? keyword-procedure-name+fail)
    (make-struct-type-property 'named-keyword-procedure))

  ;; Constructor generator for a procedure with a required keyword.
  ;; (This is used with lift-expression, so that the same constructor
  ;;  is used for each evaluation of a keyword lambda.)
  ;; The `procedure' property is a per-type method that has exactly
  ;;  the right arity, and that sends all arguments to `missing-kw'.
  (define (make-required name fail-proc method? impersonator?)
    (let-values ([(s: mk ? -ref -set!)
                  (make-struct-type (or name 'unknown)
                                    (if impersonator?
                                        (if method?
                                            struct:keyword-method-impersonator
                                            struct:keyword-procedure-impersonator)
                                        (if method?
                                            struct:keyword-method
                                            struct:keyword-procedure))
                                    0 0 #f
                                    (list (cons prop:arity-string 
                                                generate-arity-string)
                                          (cons prop:named-keyword-procedure 
                                                (cons name fail-proc))
                                          (cons prop:incomplete-arity
                                                #t))
                                    (current-inspector) fail-proc)])
      mk))

  ;; Allows support for new-prop:procedure to extract a field (i.e., this property
  ;; makes it possible to extract a field for an integer `new-prop:procedure` value):
  (define-values (prop:procedure-accessor procedure-accessor? procedure-accessor-ref)
    (make-struct-type-property 'procedure (lambda (v info-l)
                                            (if (exact-integer? v)
                                                (make-struct-field-accessor
                                                 (list-ref info-l 3)
                                                 v)
                                                #f))))

  ;; Allows keyword application to see into a "method"-style procedure attribute:
  (define-values (new-prop:procedure new-procedure? new-procedure-ref)
    (make-struct-type-property 'procedure #f
                               (list
                                ;; Imply normal `prop:procedure`:
                                (cons prop:procedure values)
                                ;; Also imply `prop:procedure-accessor`, in case property
                                ;; value is an integer:
                                (cons prop:procedure-accessor values))))

  
  ;; Proxies
  (define-values (struct:keyword-procedure-impersonator make-kpp keyword-procedure-impersonator? kpp-ref kpp-set!)
    (make-struct-type 'procedure
                      struct:keyword-procedure
                      1 0 #f
                      (list (cons prop:keyword-impersonator (lambda (v) (kpp-ref v 0))))))
  (define-values (struct:keyword-method-impersonator make-kmp keyword-method-impersonator? kmp-ref kmp-set!)
    (make-struct-type 'procedure
                      struct:keyword-method
                      1 0 #f
                      (list (cons prop:keyword-impersonator (lambda (v) (kmp-ref v 0))))))
  (define-values (struct:okpp make-optional-keyword-procedure-impersonator okpp? okpp-ref okpp-set!)
    (make-struct-type 'procedure
                      struct:okp
                      1 0 #f
                      (list (cons prop:keyword-impersonator (lambda (v) (okpp-ref v 0))))))
  (define-values (struct:okmp make-optional-keyword-method-impersonator okmp? okmp-ref okmp-set!)
    (make-struct-type 'procedure
                      struct:okp
                      1 0 #f
                      (list (cons prop:keyword-impersonator (lambda (v) (okmp-ref v 0))))))

  ;; ----------------------------------------

  (define make-keyword-procedure
    (case-lambda 
     [(proc) (make-keyword-procedure 
              proc
              (lambda args
                (apply proc null null args)))]
     [(proc plain-proc)
      (make-optional-keyword-procedure
       (make-keyword-checker null #f (procedure-arity proc))
       proc
       null
       #f
       plain-proc)]))
                         
  (define (keyword-apply proc kws kw-vals normal-args . normal-argss)
    (let ([type-error
           (lambda (what which)
             (apply raise-argument-error
                    'keyword-apply
                    what
                    which
                    proc
                    kws
                    kw-vals
                    normal-args
                    normal-argss))])
      (unless (procedure? proc)
        (type-error "procedure?" 0))
      (let loop ([ks kws])
        (cond
          [(null? ks) (void)]
          [(or (not (pair? ks))
               (not (keyword? (car ks))))
           (type-error "(listof keyword?)" 1)]
          [(null? (cdr ks)) (void)]
          [(or (not (pair? (cdr ks)))
               (not (keyword? (cadr ks))))
           (loop (cdr ks))]
          [(keyword<? (car ks) (cadr ks))
           (loop (cdr ks))]
          [else (type-error "(and/c (listof? keyword?) sorted? distinct?)" 1)]))
      (unless (list? kw-vals)
        (type-error "list?" 2))
      (unless (= (length kws) (length kw-vals))
        (raise-arguments-error
         'keyword-apply
         "keyword list length does not match value list length"
         "keyword list length" (length kws)
         "value list length" (length kw-vals)
         "keyword list" kws
         "value list" kw-vals))
      
      (let ([normal-args
             (let loop ([normal-argss (cons normal-args normal-argss)][pos 3])
               (if (null? (cdr normal-argss))
                   (let ([l (car normal-argss)])
                     (if (list? l)
                         l
                         (type-error "list?" pos)))
                   (cons (car normal-argss)
                         (loop (cdr normal-argss) (add1 pos)))))])
        (if (null? kws)
            (apply proc normal-args)
            (apply
             (keyword-procedure-extract/method kws (+ 2 (length normal-args)) proc 0)
             kws
             kw-vals
             normal-args)))))

  (define (procedure-keywords p)
    (cond
     [(keyword-procedure? p)
      (values (keyword-procedure-required p)
              (keyword-procedure-allowed p))]
     [(procedure? p)
      (if (new-procedure? p)
          (let ([v (new-procedure-ref p)])
            (if (procedure? v)
                (procedure-keywords v)
                (let ([a (procedure-accessor-ref p)])
                  (if a
                      (procedure-keywords (a p))
                      (values null null)))))
          (values null null))]
     [else (raise-argument-error 'procedure-keywords
                                 "procedure?"
                                 p)]))

  ;; ----------------------------------------
  ;; `lambda' with optional and keyword arguments
  
  (define-for-syntax (simple-args? args)
    (cond
      [(identifier? args) #t]
      [(pair? args) (and (identifier? (car args))
                         (simple-args? (cdr args)))]
      [(syntax? args) (simple-args? (syntax-e args))]
      [(null? args) #t]
      [else #f]))
  
  ;; Helper to parse the argument list.
  ;; The result is syntax:
  ;;    ((plain-id ...)           ; non-potional, non-keyword args
  ;;     (opt-id ...)             ; optional, non-keyword args
  ;;     ([id opt-expr kind] ...) ; all args, kind is one of: #:plain, #:opt, #:kw
  ;;     ([kw kw-id req?] ...)    ; kw args
  ;;     (req-kw ...)             ; required keywords (could be extracted from previous)
  ;;     rest)                    ; either () or (rest-id)
  (define-for-syntax (parse-formals stx args)
    (let* ([kw-ht (make-hasheq)]
           [check-kw (lambda (kw)
                       (when (hash-ref kw-ht (syntax-e kw) #f)
                         (raise-syntax-error
                          #f
                          "duplicate keyword for argument"
                          stx
                          kw))
                       (hash-set! kw-ht (syntax-e kw) #t))])
      (let loop ([args args] [needs-default? #f])
        (syntax-case args ()
          [id
           (identifier? (syntax id))
           #'(() () () () () (id))]
          [()
           #'(() () () () () ())]
          [(id . rest)
           (identifier? (syntax id))
           (begin
             (when needs-default?
               (raise-syntax-error
                #f "default-value expression missing" stx (syntax id)))
             (with-syntax ([(plain opt-ids opts kws need-kw rest) (loop #'rest #f)])
               #'((id . plain) opt-ids ([id #f #:plain] . opts) kws need-kw rest)))]
          [([id default] . rest)
           (identifier? (syntax id))
           (with-syntax ([(plain opt-ids opts kws need-kw rest) (loop #'rest #t)])
             #'(plain (id . opt-ids) ([id default #:opt] . opts) kws need-kw rest))]
          [(kw id . rest)
           (and (identifier? #'id)
                (keyword? (syntax-e #'kw)))
           (begin
             (check-kw #'kw)
             (with-syntax ([(plain opt-ids opts kws need-kw rest) (loop #'rest needs-default?)])
               #'(plain opt-ids ([id #f #:kw-req] . opts) ([kw id #t] . kws) (kw . need-kw) rest)))]
          [(kw [id default] . rest)
           (and (identifier? #'id)
                (keyword? (syntax-e #'kw)))
           (begin
             (check-kw #'kw)
             (with-syntax ([(plain opt-ids opts kws need-kw rest) (loop #'rest needs-default?)])
               #'(plain opt-ids ([id default #:kw-opt] . opts) ([kw id #f] . kws) need-kw rest)))]
          [(kw)
           (keyword? (syntax-e #'kw))
           (begin
             (check-kw #'kw)
             (raise-syntax-error
              #f
              "missing argument identifier after keyword"
              stx
              #'kw))]
          [(kw bad . rest)
           (keyword? (syntax-e #'kw))
           (raise-syntax-error
            #f
            "after keyword, not an identifier or identifier with default"
            stx
            (syntax bad))]
          [(bad . rest)
           (raise-syntax-error
            #f
            "not an identifier, identifier with default, or keyword"
            stx
            (syntax bad))]
          [else
           (raise-syntax-error
            #f "bad argument sequence" stx args)]))))
  
  ;; The new `lambda' form:
  (define-for-syntax (parse-lambda stx local-name non-kw-k kw-k)
    (syntax-case stx ()
      [(_ args body1 body ...)
       (if (simple-args? #'args)
           ;; Use plain old `lambda':
           (non-kw-k
            (syntax/loc stx
              (lambda args body1 body ...)))
           ;; Handle keyword or optional arguments:
           (with-syntax ([((plain-id ...)
                           (opt-id ...)
                           ([id opt-expr kind] ...)
                           ([kw kw-id kw-req] ...)
                           need-kw
                           rest)
                          (parse-formals stx #'args)])
             (let ([dup-id (check-duplicate-identifier (syntax->list #'(id ... . rest)))])
               (when dup-id
                 (raise-syntax-error
                  #f
                  "duplicate argument identifier"
                  stx
                  dup-id)))
             (let* ([kws (syntax->list #'(kw ...))]
                    [opts (syntax->list #'(opt-id ...))]
                    [ids (syntax->list #'(id ...))]
                    [plain-ids (syntax->list #'(plain-id ...))]
                    [kw-reqs (syntax->list #'(kw-req ...))]
                    [kw-args (generate-temporaries kws)]     ; to hold supplied value
                    [kw-arg?s (generate-temporaries kws)]    ; to indicated whether it was supplied
                    [opt-args (generate-temporaries opts)]   ; supplied value
                    [opt-arg?s (generate-temporaries opts)]  ; whether supplied
                    [needed-kws (sort (syntax->list #'need-kw)
                                      (lambda (a b) (keyword<? (syntax-e a) (syntax-e b))))]
                    [sorted-kws (sort (map list kws kw-args kw-arg?s kw-reqs)
                                      (lambda (a b) (keyword<? (syntax-e (car a))
                                                               (syntax-e (car b)))))]
                    [method? (syntax-property stx 'method-arity-error)]
                    [annotate-method (lambda (stx)
                                       (if method?
                                           (syntax-property stx 'method-arity-error #t)
                                           stx))]
                    [flatten-keywords (lambda (kws)
                                        (let loop ([kws kws])
                                          (cond
                                           [(null? kws) null]
                                           [(syntax-e (cadddr (car kws)))
                                            (cons (cadar kws) (loop (cdr kws)))]
                                           [else
                                            (list* (cadar kws) (caddar kws) (loop (cdr kws)))])))])
               (with-syntax ([(kw-arg ...) kw-args]
                             [(kw-arg? ...) (let loop ([kw-arg?s kw-arg?s]
                                                       [kw-reqs kw-reqs])
                                              (cond
                                               [(null? kw-arg?s) null]
                                               [(not (syntax-e (car kw-reqs)))
                                                (cons (car kw-arg?s) (loop (cdr kw-arg?s) (cdr kw-reqs)))]
                                               [else (loop (cdr kw-arg?s) (cdr kw-reqs))]))]
                             [kws-sorted sorted-kws]
                             [(opt-arg ...) opt-args]
                             [(opt-arg? ...) opt-arg?s]
                             [(new-plain-id  ...) (generate-temporaries #'(plain-id ...))]
                             [new-rest (if (null? (syntax-e #'rest))
                                           '()
                                           '(new-rest))]
                             [(rest-id) (if (null? (syntax-e #'rest))
                                            '(())
                                            #'rest)]
                             [rest-empty (if (null? (syntax-e #'rest))
                                             '()
                                             '(null))]
                             [fail-rest (if (null? (syntax-e #'rest))
                                            '(null)
                                            #'rest)]
                             [make-okp (if method?
                                           #'make-optional-keyword-method
                                           #'make-optional-keyword-procedure)]
                             [method? method?]
                             [with-kw-min-args (+ 2 (length plain-ids))]
                             [with-kw-max-arg (if (null? (syntax-e #'rest))
                                                  (+ 2 (length plain-ids) (length opts))
                                                  #f)]
                             [core (car (generate-temporaries (if (identifier? local-name)
                                                                  (list local-name)
                                                                  '(core))))]
                             [unpack (car (generate-temporaries '(unpack)))])
                 (let ([mk-core
                        (lambda (kw-core?)
                          ;; body of procedure, where all optional
                          ;; arguments (both keyword and positional)
                          ;; come in as a pair of arguments (value and
                          ;; whether the value is valid):
                          ;; the arguments are in the following order:
                          ;; - optional kw/kw?, interspersed
                          ;; - mandatory kw
                          ;; - mandatory positional arguments
                          ;; - optional positional arguments
                          ;; - optional positional argument validity flags
                          ;; - rest arguments
                          (annotate-method
                           (quasisyntax/loc stx
                             (lambda (#,@(if kw-core?
                                             (flatten-keywords sorted-kws)
                                             null)
                                      new-plain-id ... 
                                      opt-arg ...
                                      opt-arg? ...
                                      . new-rest)
                               ;; sort out the arguments into the user-supplied bindings,
                               ;; evaluating default-value expressions as needed:
                               #,(syntax-property
                                  (quasisyntax/loc stx ; kw-opt profiler uses this srcloc
                                    (let-maybe ([id opt-expr kind] ... . rest)
                                               (kw-arg ...) (kw-arg? ...)
                                               (opt-arg ...) (opt-arg? ...)
                                               (new-plain-id ... . new-rest)
                                               ;; the original body, finally:
                                               body1 body ...))
                                  'feature-profile:kw-opt-protocol #t)))))]
                       [mk-unpack
                        (lambda ()
                          ;; like core, but keywords must be unpacked:
                          (annotate-method
                           (quasisyntax/loc stx
                             (lambda (given-kws given-args
                                      new-plain-id ... 
                                      opt-arg ...
                                      opt-arg? ...
                                      . new-rest)
                               ;; sort out the arguments into the user-supplied bindings,
                               ;; evaluating default-value expressions as needed:
                               #,(syntax-property
                                  (quasisyntax/loc stx ; kw-opt profiler uses this srcloc
                                    (let-kws given-kws given-args kws-sorted
                                             #,(syntax-property
                                                #`(core #,@(flatten-keywords sorted-kws)
                                                        new-plain-id ... opt-arg ... opt-arg? ...
                                                        . new-rest)
                                                'kw-feature-profile:opt-protocol 'antimark)))
                                  'feature-profile:kw-opt-protocol #f)))))]
                       [mk-no-kws
                        (lambda (kw-core?)
                          ;; entry point without keywords:
                          (annotate-method
                           (quasisyntax/loc stx
                             (opt-cases #,(if kw-core?
                                              #'(unpack null null)
                                              #'(core))
                                        ([opt-id opt-arg opt-arg?] ...) (plain-id ...) 
                                        () (rest-empty rest-id . rest)
                                        ()))))]
                       [mk-with-kws
                        (lambda ()
                          ;; entry point with keywords:
                          (if (and (null? opts)
                                   (null? #'new-rest))
                              #'core
                              (annotate-method
                               (syntax/loc stx
                                 (opt-cases (unpack) ([opt-id opt-arg opt-arg?] ...) (given-kws given-args plain-id ...) 
                                            () (rest-empty rest-id . rest)
                                            ())))))]
                       [mk-kw-arity-stub
                        (lambda ()
                          ;; struct-type entry point for no keywords when a keyword is required
                          (annotate-method
                           (syntax/loc stx
                             (fail-opt-cases (missing-kw) (opt-id ...) (self plain-id ...) 
                                             () (rest-id . fail-rest)
                                             ()))))]
                       [kw-k* (lambda (impl kwimpl wrap)
                                (kw-k impl kwimpl wrap #'core #'unpack
                                      (length plain-ids) (length opts) 
                                      (not (null? (syntax-e #'rest)))
                                      needed-kws
                                      (map car sorted-kws)))])
                   (cond
                    [(null? kws)
                     ;; just the no-kw part
                     (non-kw-k
                      (syntax-protect
                       (quasisyntax/loc stx
                         (let ([core #,(mk-core #f)])
                           #,(mk-no-kws #f)))))]
                    [(null? needed-kws)
                     ;; both parts dispatch to core
                     (kw-k*
                      (mk-core #t)
                      (mk-unpack)
                      (with-syntax ([kws (map car sorted-kws)]
                                    [no-kws (let ([p (mk-no-kws #t)]
                                                  [n (or local-name
                                                         (syntax-local-infer-name stx))])
                                              (if n
                                                  #`(let ([#,n #,p]) #,n)
                                                  p))]
                                    [with-kws (mk-with-kws)])
                        (quasisyntax/loc stx
                          (make-okp
                           (lambda (given-kws given-argc)
                             #,(syntax-property
                                (syntax/loc stx ; kw-opt profiler uses this srcloc
                                  (and (in-range?/static given-argc with-kw-min-args with-kw-max-arg)
                                       (subset?/static given-kws 'kws)))
                                'feature-profile:kw-opt-protocol #t))
                           with-kws
                           null
                           'kws
                           no-kws))))]
                    [else
                     ;; just the keywords part dispatches to core,
                     ;; and the other part dispatches to failure
                     (kw-k*
                      (mk-core #t)
                      (mk-unpack)
                      (with-syntax ([kws (map car sorted-kws)]
                                    [needed-kws needed-kws]
                                    [no-kws (mk-no-kws #t)]
                                    [with-kws (mk-with-kws)]
                                    [mk-id (with-syntax ([n (or local-name
                                                                (syntax-local-infer-name stx))]
                                                         [call-fail (mk-kw-arity-stub)])
                                             (syntax-local-lift-expression
                                              #'(make-required 'n call-fail method? #f)))])
                        (quasisyntax/loc stx
                          (mk-id
                           (lambda (given-kws given-argc)
                             #,(syntax-property
                                (syntax/loc stx ; kw-opt profiler uses this srcloc
                                  (and (in-range?/static given-argc with-kw-min-args with-kw-max-arg)
                                       (subsets?/static 'needed-kws given-kws 'kws)))
                                'feature-profile:kw-opt-protocol #t))
                           with-kws
                           'needed-kws
                           'kws))))]))))))]))
  
  (define-syntaxes (new-lambda new-λ)
    (let ([new-lambda
           (lambda (stx)
             (if (eq? (syntax-local-context) 'expression)
                 (parse-lambda
                  stx
                  #f
                  (lambda (e) e)
                  (lambda (impl kwimpl wrap core-id unpack-id n-req n-opt rest? req-kws all-kws)
                    (syntax-protect
                     (quasisyntax/loc stx
                       (let ([#,core-id #,impl])
                         (let ([#,unpack-id #,kwimpl])
                           #,wrap))))))
                 (quasisyntax/loc stx (#%expression #,stx))))])
      (values new-lambda new-lambda)))
  
  (define (missing-kw proc . args)
    (apply
     (keyword-procedure-extract/method null 0 proc 0)
     null
     null
     args))

  ;; ----------------------------------------

  ;; Helper macro:
  ;; Steps through the list bound to `kw-args', extracting
  ;; the available values. For each keyword, this binds one
  ;; id to say whether the value is present, and one id
  ;; to the actual value (if present); if the keyword isn't
  ;; available, then the corresponding `req' is applied, which
  ;; should signal an error if the keyword is required.
  (define-syntax let-kws
    (syntax-rules ()
      [(_ kws kw-args () . body)
       (begin . body)]
      [(_ kws kw-args ([kw arg arg? #f]) . body)
       ;; last optional argument doesn't need to check as much or take as many cdrs
       (let ([arg? (pair? kws)])
         (let ([arg (if arg? (car kw-args) (void))])
           . body))]
      [(_ kws kw-args ([kw arg arg? #f] . rest) . body)
       (let ([arg? (and (pair? kws)
                        (eq? 'kw (car kws)))])
         (let ([arg (if arg? (car kw-args) (void))]
               [kws (if arg? (cdr kws) kws)]
               [kw-args (if arg? (cdr kw-args) kw-args)])
           (let-kws kws kw-args rest . body)))]
      [(_ kws kw-args ([kw arg arg? #t]) . body)
       ;; last required argument doesn't need to take cdrs
       (let ([arg (car kw-args)])
         . body)]
      [(_ kws kw-args ([kw arg arg? #t] . rest) . body)
       (let ([arg (car kw-args)]
             [kws (cdr kws)]
             [kw-args (cdr kw-args)])
         (let-kws kws kw-args rest . body))]))

  ;; Used for `req' when the keyword argument is optional:
  (define-syntax missing-ok
    (syntax-rules ()
      [(_ x y) #f]))
  
  ;; ----------------------------------------

  ;; Helper macro:
  ;; Builds up a `case-lambda' to handle the arities
  ;; possible due to optional arguments. Each clause
  ;; jumps directory to `core', where each optional
  ;; argument is split into two: a boolean argument that
  ;; indicates whether it was supplied, and an argument 
  ;; for the value (if supplied).
  (define-syntax opt-cases 
    (syntax-rules ()
      [(_ (core ...) () (base ...) () (rest-empty rest-id . rest) ())
       ;; This case only happens when there are no optional arguments
       (case-lambda
         [(base ... . rest-id)
          (core ... base ... . rest)])]
      [(_ (core ...) ([opt-id opt-arg opt-arg?]) (base ...) (done-id ...) (rest-empty rest-id . rest) clauses)
       ;; Handle the last optional argument and the rest args (if any)
       ;; at the same time.
       (case-lambda
         [(base ...) (core ... base ... (a-false done-id) ... #f (a-false done-id) ... #f . rest-empty)]
         [(base ... done-id ... opt-arg . rest-id)
          (core ... base ... done-id ... opt-arg (a-true done-id) ... #t . rest)]
         . clauses)]
      [(_ (core ...) ([opt-id opt-arg opt-arg?] more ...) (base ...) (done-id ...) (rest-empty rest-id . rest) clauses)
       ;; Handle just one optional argument, add it to the "done" sequence,
       ;; and continue generating clauses for the remaining optional arguments.
       (opt-cases (core ...) (more ...) (base ...) (done-id ... opt-id) (rest-empty rest-id . rest)
                  ([(base ... done-id ... opt-arg)
                    (core ... base ... 
                          done-id ... opt-arg (a-false more) ... 
                          (a-true done-id) ... #t (a-false more) ... . rest-empty)]
                   . clauses))]))

  ;; Helper macro:
  ;; Similar to opt-cases, but just pass all arguments along to `fail'.
  (define-syntax fail-opt-cases
    (syntax-rules ()
      [(_ (fail ...) () (base ...) () (rest-id . rest) ())
       ;; This case only happens when there are no optional arguments
       (case-lambda
         [(base ... . rest-id)
          (apply fail ... base ... . rest)])]
      [(_ (fail ...) (opt-id) (base ...) (done ...) (rest-id . rest) clauses)
       ;; Handle the last optional argument and the rest args (if any)
       ;; at the same time.
       (case-lambda
         [(base ...) (fail ... base ...)]
         [(base ... done ... opt-id . rest-id) (apply fail ... base ... done ... opt-id . rest)]
         . clauses)]
      [(_ (fail ...) (opt-id more ...) (base ...) (done ...) (rest-id . rest) clauses)
       ;; Handle just one more optional argument:
       (fail-opt-cases (fail ...) (more ...) (base ...) (done ... opt-id) (rest-id . rest)
                       ([(base ... done ... opt-arg)
                         (fail ... base ... done ... opt-arg)]
                        . clauses))]))
  
  ;; Helper macros:
  (define-syntax (a-false stx) #'#f)
  (define-syntax (a-true stx) #'#t)
  
  ;; ----------------------------------------

  ;; Helper macro:
  ;; Walks through all arguments in order, shifting supplied
  ;; optional values into the user-supplied binding, and
  ;; evaluating default-value expressions when the optional
  ;; value is not available. The binding order here is
  ;; consistent with the original order of the arguments
  ;; (where, e.g., an optional keyword argument might
  ;; precede a required argument, so the required argument
  ;; cannot be used to compute the default).
  (define-syntax (let-maybe stx)
    (syntax-case stx (required)
      [(_ () () () () () () . body)
       (syntax-property
        #'(let () . body)
        'feature-profile:kw-opt-protocol 'antimark)]
      [(_ ([id ignore #:plain] . more) kw-args kw-arg?s opt-args opt-arg?s (req-id . req-ids) . body)
       #'(let ([id req-id])
           (let-maybe more kw-args kw-arg?s opt-args opt-arg?s req-ids . body))]
      [(_ ([id expr #:opt] . more)  kw-args kw-arg?s (opt-arg . opt-args) (opt-arg? . opt-arg?s) req-ids . body)
       #'(let ([id (if opt-arg?
                       opt-arg
                       expr)])
           (let-maybe more kw-args kw-arg?s opt-args opt-arg?s req-ids . body))]
      [(_ ([id expr #:kw-req] . more)  (kw-arg . kw-args) kw-arg?s opt-args opt-arg?s req-ids . body)
       #'(let ([id kw-arg])
           (let-maybe more kw-args kw-arg?s opt-args opt-arg?s req-ids . body))]
      [(_ ([id expr #:kw-opt] . more)  (kw-arg . kw-args) (kw-arg? . kw-arg?s) opt-args opt-arg?s req-ids . body)
       #'(let ([id (if kw-arg?
                       kw-arg
                       expr)])
           (let-maybe more kw-args kw-arg?s opt-args opt-arg?s req-ids . body))]
      [(_ (id) () () () () (req-id) . body)
       (syntax-property
        #'(let ([id req-id]) . body)
        'feature-profile:kw-opt-protocol 'antimark)]))

  ;; ----------------------------------------
  ;; Helper macros:
  ;;  Generate arity and keyword-checking procedure statically
  ;;  as much as is reasonable.

  (define-syntax (in-range?/static stx)
    (syntax-case stx ()
      [(_ v min #f)
       #'(v . >= . min)]
      [(_ v min max)
       (if (equal? (syntax-e #'min) (syntax-e #'max))
           #'(= v min)
           #'(and (v . >= . min) (v . <= . max)))]))
  
  (define-syntax (subset?/static stx)
    (syntax-case stx (quote)
      [(_ l1-expr '()) #'(null? l1-expr)]
      [(_ '() l2-expr) #'#t]
      [(_ l1-expr '(kw . kws)) #'(let ([l1 l1-expr])
                                   (let ([l1 (if (null? l1)
                                                 l1
                                                 (if (eq? (car l1) 'kw)
                                                     (cdr l1)
                                                     l1))])
                                     (subset?/static l1 'kws)))]
      [(_ l1-expr l2-expr) #'(subset? l1-expr l2-expr)]))

  (define-syntax (subsets?/static stx)
    (syntax-case stx (quote)
      [(_ '() l2-expr l3-expr)
       #'(subset?/static l2-expr l3-expr)]
      [(_ l1-expr l2-expr '())
       #'(subset?/static l1-expr l2-expr)]
      [(_ 'l1-elems l2-expr 'l3-elems)
       (if (equal? (map syntax-e (syntax->list #'l1-elems))
                   (map syntax-e (syntax->list #'l3-elems)))
           ;; l2 must be equal to l1/l3:
           #'(equal?/static 'l1-elems l2-expr)
           #'(subsets? 'l1-elems l2-expr 'l3-elems))]))

  (define-syntax (equal?/static stx)
    ;; Unroll loop at expansion time
    (syntax-case stx (quote)
      [(_ '() l2-expr) #'(null? l2-expr)]
      [(_ '(kw . kw-rest) l2-expr)
       #'(let ([l2 l2-expr])
           (and (pair? l2)
                (eq? (car l2) 'kw)
                (equal?/static 'kw-rest (cdr l2))))]))
  
  ;; ----------------------------------------
  ;; `define' with keyword arguments
  
  (define-syntax (new-define stx)
    (let-values ([(id rhs)
                  (normalize-definition stx #'new-lambda #t #t)])
      (let* ([plain (lambda (rhs)
                      (quasisyntax/loc stx
                        (define #,id #,rhs)))]
             [can-opt? (lambda (lam-id)
                         (and (identifier? lam-id)
                              (or (free-identifier=? lam-id #'new-lambda)
                                  (free-identifier=? lam-id #'new-λ))
                              (let ([ctx (syntax-local-context)])
                                (or (and (memq ctx '(module module-begin))
                                         (compile-enforce-module-constants))
                                    (and (list? ctx)
                                         (andmap liberal-define-context? ctx))))))]
             [opt (lambda (rhs core-wrap plain)
                    (parse-lambda rhs
                                  id
                                  plain
                                  (lambda (impl kwimpl wrap 
                                                core-id unpack-id
                                                n-req n-opt rest? req-kws all-kws)
                                    (with-syntax ([proc (car (generate-temporaries (list id)))])
                                      (syntax-protect
                                       (quasisyntax/loc stx
                                         (begin
                                           #,(quasisyntax/loc stx
                                               (define-syntax #,id 
                                                 (make-keyword-syntax (lambda () 
                                                                        (values (quote-syntax #,core-id)
                                                                                (quote-syntax proc)))
                                                                      #,n-req #,n-opt #,rest? 
                                                                      '#,req-kws '#,all-kws)))
                                           #,(quasisyntax/loc stx 
                                               (define #,core-id #,(core-wrap impl)))
                                           #,(quasisyntax/loc stx 
                                               (define #,unpack-id #,kwimpl))
                                           #,(quasisyntax/loc stx 
                                               (define proc #,wrap)))))))))])
        (syntax-case rhs (begin quote)
          [(lam-id . _)
           (can-opt? #'lam-id)
           (opt rhs values plain)]
          [(begin (quote sym) (lam-id . _))
           ;; looks like a compiler hint
           (and (can-opt? #'lam-id)
                (identifier? #'sym))
           (syntax-case rhs ()
             [(_ _ sub-rhs)
              (let ([wrap (lambda (stx) #`(begin (quote sym) #,stx))])
                (opt #'sub-rhs 
                     wrap
                     (lambda (rhs) (plain (wrap rhs)))))])]
          [_ (plain rhs)]))))
  
  ;; ----------------------------------------
  ;; `#%app' with keyword arguments
  
  (define-for-syntax (parse-app stx check-arity generate-direct)
    (let ([l (syntax->list stx)])
      (if (not (and l
                    (pair? (cdr l))
                    (not (keyword? (syntax-e (cadr l))))
                    (ormap (lambda (x) (keyword? (syntax-e x)))
                           l)))
          ;; simple or erroneous app:
          (if (identifier? stx)
              (raise-syntax-error
               #f
               "illegal use"
               stx)
              (if (and (pair? l)
                       (null? (cdr l)))
                  (raise-syntax-error
                   #f
                   "missing procedure expression;\n probably originally (), which is an illegal empty application"
                   stx)
                  (begin
                    (when l
                      (check-arity (- (length l) 2)))
                    (let ([args (cdr (syntax-e stx))])
                      (generate-direct 
                       (if l (cdr (if (pair? args) args (syntax-e args))) null) null #f
                       (quasisyntax/loc stx
                         (#%app . #,args)))))))
          ;; keyword app (maybe)
          (let ([exprs
                 (let ([kw-ht (make-hasheq)])
                   (let loop ([l (cddr l)])
                     (cond
                       [(null? l) null]
                       [(keyword? (syntax-e (car l)))
                        (when (hash-ref kw-ht (syntax-e (car l)) #f)
                          (raise-syntax-error
                           'application
                           "duplicate keyword in application"
                           stx
                           (car l)))
                        (hash-set! kw-ht (syntax-e (car l)) #t)        
                        (cond
                          [(null? (cdr l))
                           (raise-syntax-error
                            'application
                            "missing argument expression after keyword"
                            stx
                            (car l))]
                          [(keyword? (cadr l))
                           (raise-syntax-error
                            'application
                            "keyword in expression position (immediately after another keyword)"
                            stx
                            (cadr l))]
                          [else
                           (cons (cadr l)
                                 (loop (cddr l)))])]
                       [else
                        (cons (car l) (loop (cdr l)))])))])
            (let* ([name (syntax-local-infer-name stx #f)]
                   [ids (cons (if (and name (or (identifier? name) (symbol? name)))
                                  (if (syntax? name) name (datum->syntax #f name))
                                  (datum->syntax #f 'procedure))
                              (generate-temporaries exprs))])
              (let loop ([l (cdr l)]
                         [ids ids]
                         [bind-accum null]
                         [arg-accum null]
                         [kw-pairs null])
                (cond
                  [(null? l)
                   (let* ([args (reverse arg-accum)]
                          [sorted-kws (sort kw-pairs 
                                            (lambda (a b)
                                              (keyword<? (syntax-e (car a))
                                                         (syntax-e (car b)))))]
                          [cnt (+ 1 (length args))])
                     (check-arity (- cnt 2))
                     (syntax-protect
                      (quasisyntax/loc stx
                        (let #,(reverse bind-accum)
                          #,(generate-direct 
                             (cdr args) sorted-kws #t
                             (quasisyntax/loc stx
                               ((checked-procedure-check-and-extract struct:keyword-procedure
                                                                     #,(car args)
                                                                     keyword-procedure-extract 
                                                                     '#,(map car sorted-kws) 
                                                                     #,cnt)
                                '#,(map car sorted-kws)
                                (list #,@(map cdr sorted-kws))
                                . #,(cdr args))))))))]
                  [(keyword? (syntax-e (car l)))
                   (loop (cddr l)
                         (cdr ids)
                         (cons (list (car ids) (syntax-property (cadr l)
                                                                'inferred-name
                                                                ;; void hides binding name
                                                                (void)))
                               bind-accum)
                         arg-accum
                         (cons (cons (car l) (car ids))
                               kw-pairs))]
                  [else (loop (cdr l)
                              (cdr ids)
                              (cons (list (car ids) (car l)) bind-accum)
                              (cons (copy-properties (car ids) (car l)) arg-accum)
                              kw-pairs)])))))))

  (define-syntax (new-app stx)
    (parse-app stx void (lambda (args kw-args lifted? orig) orig)))

  (define-values-for-syntax (struct:kw-expander make-kw-expander kw-expander? kw-expander-ref kw-expander-set)
    (make-struct-type 'kw-expander #f 3 0 #f (list (cons prop:set!-transformer 0)) (current-inspector) 0))

  (define-for-syntax kw-expander-impl (make-struct-field-accessor kw-expander-ref 1 'impl))
  (define-for-syntax kw-expander-proc (make-struct-field-accessor kw-expander-ref 2 'proc))

  (define-for-syntax (syntax-procedure-converted-arguments-property stx) 
    (unless (syntax? stx)
      (raise-argument-error 'syntax-procedure-converted-arguments "syntax?" stx))
    (syntax-property stx kw-converted-arguments-variant-of))

  (define-for-syntax (make-keyword-syntax get-ids n-req n-opt rest? req-kws all-kws)
    (make-kw-expander
     (lambda (stx)
       (define-values (impl-id wrap-id) (get-ids))
       (syntax-case stx (set!)
         [(set! self rhs)
          (quasisyntax/loc stx (set! #,wrap-id rhs))]
         [(self arg ...)
          (let ([warning
                 (lambda (msg)
                   (let ([l (current-logger)])
                     (when (log-level? l 'warning)
                       (log-message
                        l
                        'warning
                        (format "~aexpansion detects ~a for: ~a"
                                (let ([s (syntax-source stx)]
                                      [l (syntax-line stx)]
                                      [c (syntax-column stx)]
                                      [p (syntax-position stx)])
                                  (if s
                                      (if l
                                          (format "~a:~a:~a: " s l c)
                                          (format "~a:::~a: " s p))
                                      ""))
                                msg
                                (syntax-e #'self))
                        (current-continuation-marks)))))]
                [impl-id/prop
                 (syntax-property impl-id kw-converted-arguments-variant-of 
                                  (cons (syntax-taint (syntax-local-introduce #'self))
                                        (syntax-taint (syntax-local-introduce impl-id))))]
                [wrap-id/prop
                 (syntax-property wrap-id alias-of 
                                  (cons (syntax-taint (syntax-local-introduce #'self))
                                        (syntax-taint (syntax-local-introduce wrap-id))))])
            (if (free-identifier=? #'new-app (datum->syntax stx '#%app))
                (parse-app (datum->syntax #f (cons #'new-app stx) stx)
                           (lambda (n)
                             (when (or (n . < . n-req)
                                       (and (not rest?)
                                            (n . > . (+ n-req n-opt))))
                               (warning "wrong number of by-position arguments")))
                           (lambda (args kw-args lifted? orig)
                             (let* ([args (syntax->list (datum->syntax #f args))]
                                    [n (length args)]
                                    [lift-args (lambda (k)
                                                 (if (not lifted?)
                                                     ;; caller didn't lift expresions out
                                                     (let ([ids (generate-temporaries args)])
                                                       #`(let #,(map list ids args)
                                                           #,(k ids)))
                                                     ;; caller already lifted expression:
                                                     (k args)))])
                               (or
                                (and (not (or (n . < . n-req)
                                              (and (not rest?)
                                                   (n . > . (+ n-req n-opt)))))
                                     (let loop ([kw-args kw-args] [req-kws req-kws] [all-kws all-kws])
                                       (cond
                                        [(null? kw-args) 
                                         (or (null? req-kws)
                                             (and
                                              (warning
                                               (format "missing required keyword ~a" (car req-kws)))
                                              #f))]
                                        [else (let* ([kw (syntax-e (caar kw-args))]
                                                     [all-kws (let loop ([all-kws all-kws])
                                                                (cond
                                                                 [(null? all-kws) null]
                                                                 [(keyword<? (car all-kws) kw)
                                                                  (loop (cdr all-kws))]
                                                                 [else all-kws]))])
                                                (cond
                                                 [(or (null? all-kws)
                                                      (not (eq? kw (car all-kws))))
                                                  (warning
                                                   (format "keyword ~a that is not accepted" kw))
                                                  #f]
                                                 [(and (pair? req-kws)
                                                       (eq? kw (car req-kws)))
                                                  (loop (cdr kw-args) (cdr req-kws) (cdr all-kws))]
                                                 [(and (pair? req-kws)
                                                       (keyword<? (car req-kws) (car all-kws)))
                                                  (warning
                                                   (format "missing required keyword ~a" (car req-kws)))
                                                  #f]
                                                 [else
                                                  (loop (cdr kw-args) req-kws (cdr all-kws))]))]))
                                     (syntax-protect
                                      (lift-args
                                       (lambda (args)
                                         (quasisyntax/loc stx
                                           (if (variable-reference-constant? (#%variable-reference #,wrap-id))
                                               (#,impl-id/prop
                                                ;; keyword arguments:
                                                #,@(let loop ([kw-args kw-args] [req-kws req-kws] [all-kws all-kws])
                                                     (cond
                                                      [(null? all-kws) null]
                                                      [(and (pair? kw-args)
                                                            (eq? (syntax-e (caar kw-args)) (car all-kws)))
                                                       (if (and (pair? req-kws)
                                                                (eq? (car req-kws) (car all-kws)))
                                                           (cons (cdar kw-args) 
                                                                 (loop (cdr kw-args) (cdr req-kws) (cdr all-kws)))
                                                           (list* (cdar kw-args) 
                                                                  #'#t
                                                                  (loop (cdr kw-args) req-kws (cdr all-kws))))]
                                                      [else
                                                       (list* #'#f
                                                              #'#f
                                                              (loop kw-args req-kws (cdr all-kws)))]))
                                                ;; required arguments:
                                                #,@(let loop ([i n-req] [args args])
                                                     (if (zero? i)
                                                         null
                                                         (cons (car args)
                                                               (loop (sub1 i) (cdr args)))))
                                                ;; optional arguments:
                                                #,@(let loop ([i n-opt] [args (list-tail args n-req)])
                                                     (cond
                                                      [(zero? i) null]
                                                      [(null? args) (cons #'#f (loop (sub1 i) null))]
                                                      [else
                                                       (cons (car args) (loop (sub1 i) (cdr args)))]))
                                                ;; booleans indicating whether optional argument are present:
                                                #,@(let loop ([i n-opt] [args (list-tail args n-req)])
                                                     (cond
                                                      [(zero? i) null]
                                                      [(null? args) (cons #'#f (loop (sub1 i) null))]
                                                      [else
                                                       (cons #'#t (loop (sub1 i) (cdr args)))]))
                                                ;; rest args:
                                                #,@(if rest?
                                                       #`((list #,@(list-tail args (min (length args) (+ n-req n-opt)))))
                                                       null))
                                               #,(if lifted?
                                                     orig
                                                     (quasisyntax/loc stx (#%app #,wrap-id/prop . #,args)))))))))
                                orig))))
                (datum->syntax stx (cons wrap-id/prop #'(arg ...)) stx stx)))]
         [self
          (syntax-property wrap-id alias-of (cons (syntax-taint (syntax-local-introduce #'self))
                                                     (syntax-taint (syntax-local-introduce wrap-id))))]))
     (lambda () (define-values (impl-id wrap-id) (get-ids)) impl-id)
     (lambda () (define-values (impl-id wrap-id) (get-ids)) wrap-id)))

  ;; Checks given kws against expected. Result is
  ;; (values missing-kw extra-kw), where both are #f if
  ;; the arguments are ok.
  (define (check-kw-args p kws)
    (let loop ([kws kws]
               [required (keyword-procedure-required p)]
               [allowed (keyword-procedure-allowed p)])
      (cond
        [(null? kws)
         (if (null? required)
             (values #f #f)
             (values (car required) #f))]
        [(and (pair? required)
              (eq? (car required) (car kws)))
         (loop (cdr kws) (cdr required) (and allowed (cdr allowed)))]
        [(not allowed) ; => all keywords are allowed
         (loop (cdr kws) required #f)]
        [(pair? allowed)
         (if (eq? (car allowed) (car kws))
             (loop (cdr kws) required (cdr allowed))
             (loop kws required (cdr allowed)))]
        [else (values #f (car kws))])))

  ;; Generates a keyword an arity checker dynamically:
  (define (make-keyword-checker req-kws allowed-kws arity)
    ;; If min-args is #f, then max-args is an arity value.
    ;; If max-args is #f, then >= min-args is accepted.
    (define-syntax (arity-check-lambda stx)
      (syntax-case stx ()
        [(_ (kws) kw-body)
         #'(cond
            [(integer? arity)
             (lambda (kws a) (and kw-body (= a arity)))]
            [(arity-at-least? arity)
             (let ([arity (arity-at-least-value arity)])
               (lambda (kws a) (and kw-body (a . >= . arity))))]
            [else
             (lambda (kws a) (and kw-body (arity-includes? arity a)))])]))
    (cond
     [(not allowed-kws)
      ;; All allowed
      (cond
       [(null? req-kws)
        ;; None required
        (arity-check-lambda (kws) #t)]
       [else
        (arity-check-lambda (kws) (subset? req-kws kws))])]
     [(null? allowed-kws)
      ;; None allowed
      (arity-check-lambda (kws) (null? kws))]
     [else
      (cond
       [(null? req-kws)
        ;; None required, just allowed
        (arity-check-lambda (kws) (subset? kws allowed-kws))]
       [else
        ;; Some required, some allowed
        (if (equal? req-kws allowed-kws)
            (arity-check-lambda 
             (kws)
             ;; All allowed are required, so check equality
             (let loop ([kws kws][req-kws req-kws])
               (if (null? req-kws)
                   (null? kws)
                   (if (null? kws)
                       #f
                       (and (eq? (car kws) (car req-kws))
                            (loop (cdr kws) (cdr req-kws)))))))
            (arity-check-lambda
             (kws) 
             ;; Required is a subset of allowed
             (subsets? req-kws kws allowed-kws)))])]))
  
  (define (arity-includes? arity a)
    (cond
     [(integer? arity) (= arity a)]
     [(arity-at-least? arity)
      (a . >= . (arity-at-least-value a))]
     [else
      (ormap (lambda (ar) (arity-includes? ar a)) arity)]))
  
  (define (subset? l1 l2)
    ;; l1 and l2 are sorted
    (cond
     [(null? l1) #t]
     [(null? l2) #f]
     [(eq? (car l1) (car l2)) (subset? (cdr l1) (cdr l2))]
     [else (subset? l1 (cdr l2))]))

  (define (subsets? l1 l2 l3)
    ;; l1, l2, and l3 are sorted, and l1 is a subset of l3
    (cond
     [(null? l1) (subset? l2 l3)]
     [(null? l2) #f]
     [(null? l3) #f]
     [else (let ([v2 (car l2)])
             (cond
              [(eq? (car l1) v2) (subsets? (cdr l1) (cdr l2) (cdr l3))]
              [(eq? v2 (car l3)) (subsets? l1 (cdr l2) (cdr l3))]
              [else (subsets? l1 l2 (cdr l3))]))]))

  ;; Extracts the procedure using the keyword-argument protocol.
  ;; If `p' doesn't accept keywords, make up a procedure that
  ;; reports an error.
  (define (keyword-procedure-extract/method kws n p method-n)
    (if (and (keyword-procedure? p)
             ((keyword-procedure-checker p) kws n))
        ;; Ok:
        (keyword-procedure-proc p)
        ;; Not ok, so far:
        (let ([p2 (and (not (keyword-procedure? p))
                       (procedure? p)
                       (or (and (new-procedure? p)
                                (let ([a (procedure-accessor-ref p)])
                                  (and a
                                       (a p))))
                           (procedure-extract-target p) ; integer supplied to `make-struct-type`
                           (and (new-procedure? p) 'method)))])
          (if p2
              ;; Maybe the target is ok:
              (if (eq? p2 'method)
                  ;; Build wrapper method:
                  (let ([p3 (keyword-procedure-extract/method
                             kws (add1 n) (new-procedure-ref p) (add1 method-n))])
                    (lambda (kws kw-args . args)
                      (apply p3 kws kw-args (cons p args))))
                  ;; Recur:
                  (keyword-procedure-extract/method kws n p2 method-n))
              ;; Not ok, period:
              (lambda (kws kw-args . args)
                (define-values (missing-kw extra-kw)
                  (if (keyword-procedure? p)
                      (check-kw-args p kws)
                      (values #f (car kws))))
                (let ([n (let ([method-n
                                (+ method-n
                                   (if (or (keyword-method? p) (okm? p)) 1 0))])
                           (if (n . >= . method-n) (- n method-n) n))]
                      [args-str
                       (if (and (null? args) (null? kws))
                           ""
                           ;; Format arguments:
                           (apply
                            string-append
                            "\n  arguments...:"
                            (append
                             (map (lambda (v)
                                    (format "\n   ~e" v))
                                  args)
                             (map (lambda (kw kw-arg)
                                    (format "\n   ~a ~e" kw kw-arg))
                                  kws kw-args))))]
                      [proc-name (lambda (p) (or (and (named-keyword-procedure? p)
                                                      (car (keyword-procedure-name+fail p)))
                                                 (object-name p)
                                                 p))])
                  (raise
                   (exn:fail:contract
                    (if extra-kw
                        (if (keyword-procedure? p)
                            (format
                             (string-append
                              "application: procedure does not expect an argument with given keyword\n"
                              "  procedure: ~a\n"
                              "  given keyword: ~a"
                              "~a")
                             (proc-name p) extra-kw args-str)
                            (if (procedure? p)
                                (format
                                 (string-append
                                  "application: procedure does not accept keyword arguments\n"
                                  "  procedure: ~a"
                                  "~a")
                                 (proc-name p) args-str)
                                (format
                                 (string-append
                                  "application: not a procedure;\n"
                                  " expected a procedure that can be applied to arguments\n"
                                  "  given: ~e"
                                  "~a")
                                 p args-str)))
                        (if missing-kw
                            (format
                             (string-append
                              "application: required keyword argument not supplied\n"
                              "  procedure: ~a\n"
                              "  required keyword: ~a"
                              "~a")
                             (proc-name p) missing-kw args-str)
                            (format
                             (string-append
                              "application: no case matching ~a non-keyword argument~a\n"
                              "  procedure: ~a"
                              "~a")
                             (- n 2) (if (= 1 (- n 2)) "" "s")
                             (proc-name p) args-str)))
                    (current-continuation-marks)))))))))
  (define (keyword-procedure-extract p kws n)
    (keyword-procedure-extract/method kws n p 0))

  ;; setting procedure arity
  (define (procedure-reduce-keyword-arity proc arity req-kw allowed-kw)
    (let* ([plain-proc (procedure-reduce-arity (if (okp? proc) 
                                                   (okp-ref proc 0)
                                                   proc)
                                               arity)])
      (define (sorted? kws)
        (let loop ([kws kws])
          (cond
           [(null? kws) #t]
           [(null? (cdr kws)) #t]
           [(keyword<? (car kws) (cadr kws)) (loop (cdr kws))]
           [else #f])))

      (unless (and (list? req-kw) (andmap keyword? req-kw)
                   (sorted? req-kw))
        (raise-argument-error 'procedure-reduce-keyword-arity "(and/c (listof? keyword?) sorted? distinct?)"
                              2 proc arity req-kw allowed-kw))
      (when allowed-kw
        (unless (and (list? allowed-kw) (andmap keyword? allowed-kw)
                     (sorted? allowed-kw))
          (raise-argument-error 'procedure-reduce-keyword-arity "(or/c (and/c (listof? keyword?) sorted? distinct?) #f)"
                                3 proc arity req-kw allowed-kw))
        (unless (subset? req-kw allowed-kw)
          (raise-arguments-error 'procedure-reduce-keyword-arity 
                                 "allowed-keyword list does not include all required keywords"
                                 "allowed-keyword list" allowed-kw
                                 "required keywords" req-kw)))
      (let-values ([(old-req old-allowed) (procedure-keywords proc)])
        (unless (subset? old-req req-kw)
          (raise-arguments-error 'procedure-reduce-keyword-arity
                                 "cannot reduce required keyword set"
                                 "required keywords" old-req
                                 "requested required keywords" req-kw))
        (when old-allowed
          (unless (subset? req-kw old-allowed)
            (raise-arguments-error 'procedure-reduce-keyword-arity
                                   "cannot require keywords not in original allowed set"
                                   "original allowed keywords" old-allowed
                                   "requested required keywords" req-kw))
          (unless (or (not allowed-kw)
                      (subset? allowed-kw old-allowed))
            (raise-arguments-error 'procedure-reduce-keyword-arity
                                   "cannot allow keywords not in original allowed set"
                                   "original allowed keywords" old-allowed
                                   "requested allowed keywords" allowed-kw))))
      (if (null? allowed-kw)
          plain-proc
          (let* ([inc-arity (lambda (arity delta)
                              (let loop ([a arity])
                                (cond
                                 [(integer? a) (+ a delta)]
                                 [(arity-at-least? a)
                                  (arity-at-least (+ (arity-at-least-value a) delta))]
                                 [else
                                  (map loop a)])))]
                 [new-arity (inc-arity arity 2)]
                 [kw-checker (make-keyword-checker req-kw allowed-kw new-arity)]
                 [proc (normalize-proc proc)]
                 [new-kw-proc (procedure-reduce-arity (keyword-procedure-proc proc)
                                                      new-arity)])
            (if (null? req-kw)
                ;; All keywords are optional:
                ((if (okm? proc)
                     make-optional-keyword-method
                     make-optional-keyword-procedure)
                 kw-checker
                 new-kw-proc
                 req-kw
                 allowed-kw
                 plain-proc)
                ;; Some keywords are required, so "plain" proc is
                ;;  irrelevant; we build a new one that wraps `missing-kws'.
                ((make-required (or (and (named-keyword-procedure? proc)
                                         (car (keyword-procedure-name+fail proc)))
                                    (object-name proc))
                                (procedure-reduce-arity
                                 missing-kw
                                 (inc-arity arity 1))
                                (or (okm? proc)
                                    (keyword-method? proc))
                                #f)
                 kw-checker
                 new-kw-proc
                 req-kw
                 allowed-kw))))))

  (define new:procedure-reduce-arity
    (let ([procedure-reduce-arity
           (lambda (proc arity)
             (if (and (procedure? proc)
                      (let-values ([(req allows) (procedure-keywords proc)])
                        (pair? req))
                      (not (null? arity)))
                 (raise-arguments-error 'procedure-reduce-arity
                                        "procedure has required keyword arguments"
                                        "procedure" proc)
                 (procedure-reduce-arity (if (okm? proc)
                                             (procedure->method proc)
                                             proc)
                                         arity)))])
      procedure-reduce-arity))
    
  (define new:procedure->method
    (let ([procedure->method
           (lambda (proc)
             (let ([proc (normalize-proc proc)])
               (if (keyword-procedure? proc)
                   (cond
                    [(okm? proc) proc]
                    [(keyword-method? proc) proc]
                    [(okp? proc) (make-optional-keyword-method
                                  (keyword-procedure-checker proc)
                                  (keyword-procedure-proc proc)
                                  (keyword-procedure-required proc)
                                  (keyword-procedure-allowed proc)
                                  (okp-ref proc 0))]
                    [else
                     ;; Constructor must be from `make-required', but not a method.
                     ;; Make a new variant that's a method:
                     (let* ([name+fail (keyword-procedure-name+fail proc)]
                            [mk (make-required (car name+fail) (cdr name+fail) #t #f)])
                       (mk
                        (keyword-procedure-checker proc)
                        (keyword-procedure-proc proc)
                        (keyword-procedure-required proc)
                        (keyword-procedure-allowed proc)))])
                   ;; Not a keyword-accepting procedure:
                   (procedure->method proc))))])
      procedure->method))

  (define new:procedure-rename
    (let ([procedure-rename 
           (lambda (proc name)
             (if (not (and (keyword-procedure? proc)
                           (symbol? name)))
                 (procedure-rename proc name)
                 ;; Rename a keyword procedure:
                 (cond
                  [(okp? proc)
                   ((if (okm? proc)
                        make-optional-keyword-procedure
                        make-optional-keyword-method)
                    (keyword-procedure-checker proc)
                    (keyword-procedure-proc proc)
                    (keyword-procedure-required proc)
                    (keyword-procedure-allowed proc)
                    (procedure-rename (okp-ref proc 0) name))]
                  [else
                   ;; Constructor must be from `make-required':
                   (let* ([name+fail (keyword-procedure-name+fail proc)]
                          [mk (make-required name (cdr name+fail) (keyword-method? proc) #f)])
                     (mk
                      (keyword-procedure-checker proc)
                      (keyword-procedure-proc proc)
                      (keyword-procedure-required proc)
                      (keyword-procedure-allowed proc)))])))])
      procedure-rename))

  (define new:chaperone-procedure
    (let ([chaperone-procedure
           (lambda (proc wrap-proc . props)
             (do-chaperone-procedure #f #f chaperone-procedure 'chaperone-procedure proc wrap-proc props))])
      chaperone-procedure))

  (define new:unsafe-chaperone-procedure
    (let ([unsafe-chaperone-procedure
           (lambda (proc wrap-proc . props)
             (do-unsafe-chaperone-procedure unsafe-chaperone-procedure 'unsafe-chaperone-procedure proc wrap-proc props))])
      unsafe-chaperone-procedure))

  (define new:impersonate-procedure
    (let ([impersonate-procedure
           (lambda (proc wrap-proc . props)
             (do-chaperone-procedure #t #f impersonate-procedure 'impersonate-procedure proc wrap-proc props))])
      impersonate-procedure))

  (define new:unsafe-impersonate-procedure
    (let ([unsafe-impersonate-procedure
           (lambda (proc wrap-proc . props)
             (do-unsafe-chaperone-procedure unsafe-impersonate-procedure 'unsafe-impersonate-procedure proc wrap-proc props))])
      unsafe-impersonate-procedure))

  (define new:chaperone-procedure*
    (let ([chaperone-procedure*
           (lambda (proc wrap-proc . props)
             (do-chaperone-procedure #f #t chaperone-procedure* 'chaperone-procedure proc wrap-proc props))])
      chaperone-procedure*))

  (define new:impersonate-procedure*
    (let ([impersonate-procedure*
           (lambda (proc wrap-proc . props)
             (do-chaperone-procedure #t #t impersonate-procedure* 'impersonate-procedure proc wrap-proc props))])
      impersonate-procedure*))

  (define (do-chaperone-procedure is-impersonator? self-arg? chaperone-procedure name proc wrap-proc props)
    (let ([n-proc (normalize-proc proc)]
          [n-wrap-proc (normalize-proc wrap-proc)])
      (if (or (not (keyword-procedure? n-proc))
              (not (procedure? wrap-proc))
              ;; if any bad prop, let `chaperone-procedure' complain
              (bad-props? props))
          (apply chaperone-procedure proc wrap-proc props)
          (begin
            (chaperone-arity-match-checking self-arg? name proc wrap-proc props)
            (let*-values ([(kw-chaperone)
                           (let ([p (keyword-procedure-proc n-wrap-proc)])
                             ;; `extra-arg ...` will be `self-proc` if `self-arg?`:
                             (define-syntax gen-wrapper
                               (syntax-rules ()
                                 [(_ extra-arg ...)
                                  (case-lambda 
                                    [(extra-arg ... kws args . rest)
                                     (call-with-values (lambda () (apply p kws args extra-arg ... rest))
                                       (lambda results
                                         (let* ([len (length results)]
                                                [alen (length rest)])
                                           (when (< len (+ alen 1))
                                             (raise-arguments-error
                                              '|keyword procedure chaperone|
                                              "wrong number of results from wrapper procedure"
                                              "expected minimum number of results" (+ alen 1)
                                              "received number of results" len
                                              "wrapper procedure" wrap-proc))
                                           (let ([num-extra (- len (+ alen 1))])
                                             (let ([new-args (list-ref results num-extra)])
                                               (unless (and (list? new-args)
                                                            (= (length new-args) (length args)))
                                                 (raise-arguments-error
                                                  '|keyword procedure chaperone|
                                                  (format
                                                   "expected a list of keyword-argument values as first result~a from wrapper procedure"
                                                   (if (= len alen)
                                                       ""
                                                       " (after the result-wrapper procedure or mark specifications)"))
                                                  "first result" new-args
                                                  "wrapper procedure" wrap-proc))
                                               (for-each
                                                (lambda (kw new-arg arg)
                                                  (unless is-impersonator?
                                                    (unless (chaperone-of? new-arg arg)
                                                      (raise-arguments-error
                                                       '|keyword procedure chaperone|
                                                       (format
                                                        "~a keyword result is not a chaperone of original argument from chaperoning procedure"
                                                        kw)
                                                       "result" new-arg
                                                       "wrapper procedure" wrap-proc))))
                                                kws
                                                new-args
                                                args))
                                             (case num-extra
                                               [(0) (apply values kws results)]
                                               [(1) (apply values (car results) kws (cdr results))]
                                               [else (apply values (let loop ([results results] [c num-extra])
                                                                     (if (zero? c)
                                                                         (cons kws results)
                                                                         (cons (car results) (loop (cdr results) (sub1 c))))))])))))]
                                    ;; The following case exists only to make sure that the arity of
                                    ;; any procedure passed to `make-keyword-args' is covered
                                    ;; by this procedure's arity.
                                    [other (error "shouldn't get here")])]))
                             (if self-arg?
                                 (gen-wrapper self-proc)
                                 (gen-wrapper)))]
                          [(new-proc chap-accessor)
                           (let wrap ([proc proc] [n-proc n-proc])
                             (cond
                              [(and (not (eq? n-proc proc))
                                    (new-procedure? proc))
                               (define v (new-procedure-ref proc))
                               (cond
                                [(exact-integer? v)
                                 ;; we have to chaperone the access to the field that
                                 ;; contains a procedure; the `new-procedure-accessor`
                                 ;; property gives us that accessor
                                 (define acc (procedure-accessor-ref proc))
                                 (values
                                  (chaperone-struct
                                   proc
                                   acc
                                   (lambda (self sub-proc)
                                     (define-values (f acc) (wrap sub-proc (normalize-proc sub-proc)))
                                     f))
                                  acc)]
                                [else
                                 (values
                                  (chaperone-struct
                                   proc
                                   new-procedure-ref
                                   (lambda (self proc)
                                     ;; This `proc` takes an extra argument, which is `self`:
                                     (chaperone-procedure
                                      proc
                                      (make-keyword-procedure 
                                       (let ()
                                         ;; `extra-arg ...` will be `self-proc` if `self-arg?`:
                                         (define-syntax gen-proc
                                           (syntax-rules ()
                                             [(_ extra-arg ...)
                                              (lambda (extra-arg ... kws kw-args self . args)
                                                ;; Chain to `kw-chaperone', pulling out the self
                                                ;; argument, and then putting it back:
                                                (define len (length args))
                                                (call-with-values
                                                    (lambda () (apply kw-chaperone extra-arg ... kws kw-args args))
                                                  (lambda results
                                                    (if (= (length results) (add1 len))
                                                        (apply values (car results) self (cdr results))
                                                        (apply values (car results) (cadr results) self (cddr results))))))]))
                                         (if self-arg?
                                             (gen-proc proc-self)
                                             (gen-proc)))))))
                                  new-procedure-ref)])]
                              [(okp? n-proc)
                               (values
                                (if is-impersonator?
                                    ;; Can't use `impersonate-struct` here (due to the immutable field);
                                    ;; create a new structure, but preserve properties
                                    ((if (okm? n-proc)
                                         make-optional-keyword-method-impersonator
                                         make-optional-keyword-procedure-impersonator)
                                     (keyword-procedure-checker n-proc)
                                     (chaperone-procedure (keyword-procedure-proc n-proc)
                                                          kw-chaperone)
                                     (keyword-procedure-required n-proc)
                                     (keyword-procedure-allowed n-proc)
                                     (chaperone-procedure (okp-ref n-proc 0)
                                                          (okp-ref n-wrap-proc 0))
                                     n-proc)
                                    (chaperone-struct
                                     proc
                                     keyword-procedure-proc
                                     (lambda (self proc)
                                       (chaperone-procedure proc kw-chaperone))
                                     (make-struct-field-accessor okp-ref 0)
                                     (lambda (self proc)
                                       (chaperone-procedure proc
                                                            (okp-ref n-wrap-proc 0)))))
                                keyword-procedure-proc)]
                              [else
                               (values
                                (if is-impersonator?
                                    ;; Constructor must be from `make-required':
                                    (let* ([name+fail (keyword-procedure-name+fail n-proc)]
                                           [mk (make-required (car name+fail) (cdr name+fail) (keyword-method? n-proc) #t)])
                                      (mk
                                       (keyword-procedure-checker n-proc)
                                       (chaperone-procedure (keyword-procedure-proc n-proc) kw-chaperone)
                                       (keyword-procedure-required n-proc)
                                       (keyword-procedure-allowed n-proc)
                                       n-proc))
                                    (chaperone-struct
                                     n-proc
                                     keyword-procedure-proc
                                     (lambda (self proc)
                                       (chaperone-procedure proc kw-chaperone))))
                                keyword-procedure-proc)]))])
              (if (null? props)
                  new-proc
                  (apply chaperone-struct new-proc 
                         ;; chaperone-struct insists on having at least one selector:
                         chap-accessor #f
                         props)))))))
  
  (define (do-unsafe-chaperone-procedure unsafe-chaperone-procedure name proc wrap-proc props)
    (let ([n-proc (normalize-proc proc)]
          [n-wrap-proc (normalize-proc wrap-proc)])
      (if (or (not (keyword-procedure? n-proc))
              (not (procedure? wrap-proc))
              ;; if any bad prop, let `unsafe-chaperone-procedure' complain
              (bad-props? props))
          (apply unsafe-chaperone-procedure proc wrap-proc props)
          (begin
            (chaperone-arity-match-checking #f name proc wrap-proc props)
            (apply unsafe-chaperone-procedure proc wrap-proc props)))))

  (define (bad-props? props)
    (let loop ([props props])
      (cond
        [(null? props) #f]
        [(impersonator-property? (car props))
         (let ([props (cdr props)])
           (or (null? props)
               (loop (cdr props))))]
        [else #t])))

  (define (chaperone-arity-match-checking self-arg? name proc wrap-proc props)
    (let-values ([(a) (procedure-arity proc)]
                 [(b) (procedure-arity wrap-proc)]
                 [(d) (if self-arg? 1 0)]
                 [(a-req a-allow) (procedure-keywords proc)]
                 [(b-req b-allow) (procedure-keywords wrap-proc)])
      (define (includes? a b)
        (cond
          [(number? b) (cond
                         [(number? a) (= b (+ a d))]
                         [(arity-at-least? a)
                          (b . >= . (+ (arity-at-least-value a) d))]
                         [else
                          (ormap (lambda (a) (includes? a b)) a)])]
          [(arity-at-least? b) (cond
                                 [(number? a) #f]
                                 [(arity-at-least? a)
                                  ((arity-at-least-value b) . >= . (+ (arity-at-least-value a) d))]
                                 [else (ormap (lambda (a) (includes? b a)) a)])]
          [else (andmap (lambda (b) (includes? a b)) b)]))
      
      (unless (includes? b a)
        ;; Let core report error:
        (apply chaperone-procedure proc wrap-proc props))
      (unless (subset? b-req a-req)
        (raise-arguments-error
         name
         "wrapper procedure requires more keywords than original procedure"
         "wrapper procedure" wrap-proc
         "original procedure" proc))
      (unless (or (not b-allow)
                  (and a-allow
                       (subset? a-allow b-allow)))
        (raise-arguments-error
         name
         "wrapper procedure does not accept all keywords of original procedure"
         "wrapper procedure" wrap-proc
         "original procedure" proc))
      (void)))
  
  (define (normalize-proc proc)
    ;; If `proc' gets keyword support through `new-prop:procedure',
    ;; then wrap it to normalize to to something that matches
    ;; `keyword-procedure?'.
    (cond
     [(keyword-procedure? proc) proc]
     [(new-procedure? proc)
      (let-values ([(req-kws allowed-kws) (procedure-keywords proc)])
        (if (null? allowed-kws)
            proc
            (make-optional-keyword-procedure
             (lambda (given-kws given-argc)
               (and (procedure-arity-includes? proc (- given-argc 2) #t)
                    (or (not allowed-kws)
                        (subset? given-kws allowed-kws))
                    (subset? req-kws given-kws)))
             (lambda (kws kw-args . vals)
               (keyword-apply proc kws kw-args vals))
             req-kws
             allowed-kws
             proc)))]
     [else proc]))

  ;; copy-properties : (or/c symbol? syntax?) syntax? -> syntax?
  ;; Return the first arg as a stx obj with the properties of the second
  (define-for-syntax (copy-properties to from)
    (datum->syntax to (syntax->datum to) to from)))
