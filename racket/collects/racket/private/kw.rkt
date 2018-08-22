(module kw '#%kernel
  (#%require "define.rkt"
             "small-scheme.rkt"
             "more-scheme.rkt"
             (only '#%unsafe
                   unsafe-chaperone-procedure
                   unsafe-impersonate-procedure
                   unsafe-undefined)
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
                         "kw-prop-key.rkt"
                         "immediate-default.rkt"))

  (#%provide new-lambda new-λ
             new-define
             new-app
             make-keyword-procedure
             keyword-apply
             procedure-keywords
             new:procedure-reduce-arity
             new:procedure-reduce-arity-mask
             procedure-reduce-keyword-arity
             procedure-reduce-keyword-arity-mask
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

  ;; A `lambda` with just optional arguments is expanded to a form
  ;; `case-lambda` that dispatches to a core `lambda`, where `core`
  ;; takes all arguments. Arguments that are not supplied to the
  ;; `case-lambda` wrapper are replaced by either `unsafe-undefined`
  ;; or an immediate default when the `core` function is called. See
  ;; "immediate-default.rkt" for the definition of immediate-default
  ;; expressions.
  ;;
  ;; If the original `lambda` has a "rest" argument, then it is passed
  ;; as a regular argument to the core `lambda`.
  ;;
  ;; For example,
  ;;
  ;;   (lambda (x [y (+ 1 2)] [z '3] . r)
  ;;     <body>)
  ;;
  ;; becomes
  ;;
  ;;   (let ([core (lambda (_x _y _z _r)
  ;;                 (let* ([x _x]
  ;;                        [y (if (eq? _y unsafe-undefined)
  ;;                               (+ 1 2)
  ;;                               _y)]
  ;;                        [z (if (#%expression #f) '3 _z)] ; `if` for TR
  ;;                        [r _r])
  ;;                   <body>))])
  ;;     (case-lambda
  ;;       [(x) (code x unsafe-undefined '3 null)]
  ;;       [(x y z . r) (code x y z r)]
  ;;       [(x y) (code x y '3 null)]))
  ;;
  ;; The "_"-prefixed argument names in the `core` `lambda` and the
  ;; `let*` sequence reflect the way that default-argument expressions
  ;; can refer only to earlier arguments. The order shown for the
  ;; `case-lambda` clauses reflects how the current expansion orders a
  ;; clause for just the required arguments first, and then it has
  ;; clauses for the optional arguments in reverse order.
  ;;
  ;; The use of `(if (#%expression #f) '3 _z)` instead of `_z` has no
  ;; effect on the compiled code, because the optimizer will simplify
  ;; it to `_z`, but the `(#%expression #f)` is annotated for Typed
  ;; Racket to ensure that the expression '3 contributes to type
  ;; checking of the function.
  ;;
  ;; For keyword arguments, a `core` `lambda` similarly receives all
  ;; arguments, with each keyword argument before all others and in
  ;; order of sorted keywords. In addition, there's an intermediate
  ;; `unpack` `lambda` that receives the keyword arguments in list
  ;; form as the first two arguments, with the remaining arguments
  ;; like the core; the job of the intermediate `unpack` `lambda` is
  ;; to parse the lists while exploiting the fact that the lists are
  ;; ordered.
  ;;
  ;; For example,
  ;;
  ;;  (lambda (x [y (+ 1 2)] #:b [b 'b] #:a [a (add1 b)] [z 3] . r)
  ;;    <body>)
  ;;
  ;; becomes
  ;;
  ;;  (let ([core (lambda (_a _b _x _y _z _r)
  ;;                (let* ([x _x]
  ;;                       [_y (if (eq? _y unsafe-undefined)
  ;;                               (+ 1 2)
  ;;                               _y)]
  ;;                       [b (if (#%expression #f) '3 _b)]
  ;;                       [a (if (eq? _a unsafe-undefined)
  ;;                              (add1 b)
  ;;                              _a2)]
  ;;                       [z (if (#%expression #f) '3 _z)]
  ;;                       [r _r])
  ;;                  <body>))])
  ;;    (let ([unpack (lambda (kws args _x _y _z _r)
  ;;                    (let* ([has-a? (and (pair? kws)
  ;;                                        (eq? '#:a (car kws)))]
  ;;                           [_a (if has-a? (car args) unsafe-undefined)]
  ;;                           [kws (if has-a? (cdr kws) kws)]
  ;;                           [args (if has-a? (cdr args) args)]
  ;;                           [has-b? (pair? args)]
  ;;                           [_b (if has-b? (car args) 'b)])
  ;;                      (core _a _b _x _y _z _r)))])
  ;;      (make-optional-keyword-procedure
  ;;       ...
  ;;       ;; Entry point when at least one keyword argument is provided:
  ;;       (case-lambda
  ;;         [(kw args x) (unpack kw args x unsafe-undefined '3 null)]
  ;;         [(kws args x y z . r) (unpack kws args x y z r)]
  ;;         [(kws args x y) (unpack kws args x y '3 null)])
  ;;       ...
  ;;       ;; Entry point when no keywords are provided:
  ;;       (case-lambda
  ;;         [(x) (unpack null null x unsafe-undefined '3 null)]
  ;;         [(x y z . r) (unpack null null x y z r)]
  ;;         [(x y) (unpack null null x y '3 null)]))))
  ;;
  ;; If the example is the right-hand side of `(define f ...)`, then
  ;; `core` is flattened into the definition context as described
  ;; further below, and some calls expand as follows:
  ;;
  ;;    (f 10) => (core unsafe-undefined 'b '10 unsafe-undefined '3 '())
  ;;    (f 10 #:a 'a) => (core 'a 'b '10 unsafe-undefined '3 '())
  ;;    (f 10 #:b bee #:a 'a) => (core 'a bee '10 unsafe-undefined '3 '())
  ;;    (f 10 11) => (core unsafe-undefined 'b '10 '11 '3 '())
  ;;    (f 10 11 12 13) => (core unsafe-undefined 'b '10 '11 '12 (list '13))
  ;;
  ;;
  ;; Another example, illustrating a mandatory keyword argument:
  ;;
  ;;  (lambda (#:x x #:y [y (add1 x)]) <body>)
  ;;
  ;; becomes
  ;;
  ;;  (let ([core (lambda (_x _y)
  ;;                (let* ([x _x]
  ;;                       [y (if (eq? _y unsafe-undefined)
  ;;                              (add1 x)
  ;;                              _y)])
  ;;                  <body>))])
  ;;    (let ([unpack
  ;;           (lambda (kws args)
  ;;             (let* ([_x (car args)] ; no check needed
  ;;                    [kws (cdr kws)]
  ;;                    [args (cdr args)]
  ;;                    [has-y? (pair? kws)]
  ;;                    [_y (if has-y? (car args) unsafe-undefined)])
  ;;               (core _x _y)))])
  ;;      (naming-constructor
  ;;       ...
  ;;       (case-lambda
  ;;         [(kws args) (unpack kw args)])
  ;;       ...)))
  ;;
  ;; Finally, `(define (f ...) <body>)` or `(define f (lambda (...)
  ;; <body>))` with keyword arguments expands to bind `f` as a macro,
  ;; and some `_f` is bound to the expansion illustrated above, except
  ;; that the `core` and `unpack` bindings are flattened into the
  ;; definition context. That way, uses of the `f` macro can typically
  ;; expand to a direct call to the corresponding `core` function,
  ;; statically parsing the supplied keyword arguments and passing
  ;; `unsafe-undefined` or an immediate default in place of unsupplied
  ;; arguments. This macro-binding approach is used only when `f` has
  ;; keyword arguments.

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
                                (cons prop:procedure-accessor values))
                               ;; Can impersonate:
                               #t))

  ;; ----------------------------------------
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
  ;; Functions and proxies with required keyword arguments
  
  (define-values (struct:keyword-procedure/arity-error make-kp/ae kp/ae? kp/ae-ref kp/ae-set!)
    (make-struct-type 'procedure
                      struct:keyword-procedure
                      0 0 #f
                      (list (cons prop:arity-string generate-arity-string)
                            (cons prop:incomplete-arity #t))))
  (define-values (struct:keyword-method/arity-error make-km/ae km/ae? km/ae-ref km/ae-set!)
    (make-struct-type 'procedure
                      struct:keyword-method
                      0 0 #f
                      (list (cons prop:arity-string generate-arity-string)
                            (cons prop:incomplete-arity #t))))
  (define-values (struct:keyword-procedure-impersonator/arity-error make-kpi/ae kpi/ae? kpi/ae-ref kpi/ae-set!)
    (make-struct-type 'procedure
                      struct:keyword-procedure-impersonator
                      0 0 #f
                      (list (cons prop:arity-string generate-arity-string)
                            (cons prop:incomplete-arity #t))))
  (define-values (struct:keyword-method-impersonator/arity-error make-kmi/ae kmi/ae? kmi/ae-ref kmi/ae-set!)
    (make-struct-type 'procedure
                      struct:keyword-method-impersonator
                      0 0 #f
                      (list (cons prop:arity-string generate-arity-string)
                            (cons prop:incomplete-arity #t))))

  ;; Constructor generator for a wrapper on a procedure with a required keyword.
  ;; The `procedure' property is a per-type method that has exactly
  ;;  the right arity, and that sends all arguments to `missing-kw'.
  (define (make-required name fail-proc method? impersonator?)
    (let-values ([(s: mk ? -ref -set!)
                  (make-struct-type (or name 'unknown)
                                    (if impersonator?
                                        (if method?
                                            struct:keyword-method-impersonator/arity-error
                                            struct:keyword-procedure-impersonator/arity-error)
                                        (if method?
                                            struct:keyword-method/arity-error
                                            struct:keyword-procedure/arity-error))
                                    0 0 #f
                                    (list (cons prop:named-keyword-procedure
                                                (cons name fail-proc)))
                                    (current-inspector)
                                    fail-proc)])
      mk))
  
  ;; Macro variant of `make-required`, used for lambda form with a required
  ;; keyword.  We use a macro so that the `make-struct-type` is visible
  ;; to the optimizer, which in turn allows it to determine that the first
  ;; result is a constructor that always succeeds.
  ;; >> Beware that `name` and `fail-proc` are duplicated in the macro expansion. <<
  ;; The `name` expresison is expected to be a quoted symbol, and `fail-proc` is
  ;; expected to be a small procedure, so that duplication is ok.
  ;; (This macro is used with lift-values-expression, so that the same constructor
  ;;  is used for each evaluation of a keyword lambda.)
  (define-syntax (make-required* stx)
    (syntax-case stx ()
      [(_ struct:km/ae name fail-proc)
       #'(make-struct-type name
                           struct:km/ae
                           0 0 #f
                           (list (cons prop:named-keyword-procedure
                                       (cons name fail-proc)))
                           (current-inspector)
                           fail-proc)]))

  ;; ----------------------------------------

  (define make-keyword-procedure
    (case-lambda 
     [(proc) (make-keyword-procedure 
              proc
              (lambda args
                (apply proc null null args)))]
     [(proc plain-proc)
      (make-optional-keyword-procedure
       (make-keyword-checker null #f (and (procedure? proc) ; reundant check helps purity inference
                                          (procedure-arity-mask proc)))
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
             #'(plain ([id default] . opt-ids) ([id default #:opt] . opts) kws need-kw rest))]
          [(kw id . rest)
           (and (identifier? #'id)
                (keyword? (syntax-e #'kw)))
           (begin
             (check-kw #'kw)
             (with-syntax ([(plain opt-ids opts kws need-kw rest) (loop #'rest needs-default?)])
               #'(plain opt-ids ([id #f #:kw-req] . opts) ([kw id #t #f] . kws) (kw . need-kw) rest)))]
          [(kw [id default] . rest)
           (and (identifier? #'id)
                (keyword? (syntax-e #'kw)))
           (begin
             (check-kw #'kw)
             (with-syntax ([(plain opt-ids opts kws need-kw rest) (loop #'rest needs-default?)])
               #'(plain opt-ids ([id default #:kw-opt] . opts) ([kw id #f default] . kws) need-kw rest)))]
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
                           ([opt-id pos-opt-expr] ...)
                           ([id opt-expr kind] ...)
                           ([kw kw-id kw-req kw-opt-expr] ...)
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
                    [kw-args (generate-temporaries kws)]     ; supplied value
                    [kw-arg?s (generate-temporaries kws)]    ; temporary to indicate whether supplied
                    [opt-args (generate-temporaries opts)]   ; supplied value
                    [get-not-supplieds (lambda (opt-exprs)
                                         (map (lambda (opt-expr)
                                                (if (immediate-default? opt-expr)
                                                    opt-expr
                                                    #'unsafe-undefined))
                                              opt-exprs))]
                    [opt-not-supplieds (get-not-supplieds (syntax->list #'(pos-opt-expr ...)))]
                    [kw-not-supplieds (get-not-supplieds (syntax->list #'(kw-opt-expr ...)))]
                    [needed-kws (sort (syntax->list #'need-kw)
                                      (lambda (a b) (keyword<? (syntax-e a) (syntax-e b))))]
                    [sorted-kws (sort (map list kws kw-args kw-arg?s kw-reqs kw-not-supplieds)
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
                                           [else
                                            (cons (cadar kws) (loop (cdr kws)))])))])
               (with-syntax ([(kw-arg ...) kw-args]
                             [kws-sorted sorted-kws]
                             [(opt-arg ...) opt-args]
                             [(opt-not-supplied ...) opt-not-supplieds]
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
                             [struct:kp/ae (if method?
                                               #'struct:keyword-method/arity-error
                                               #'struct:keyword-procedure/arity-error)]
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
                          ;; - optional kw; `unsafe-undefined` for not-supplied
                          ;; - mandatory kw
                          ;; - mandatory positional
                          ;; - optional positional; `unsafe-undefined` for not-supplied
                          ;; - rest arguments
                          (annotate-method
                           (quasisyntax/loc stx
                             (lambda (#,@(if kw-core?
                                             (flatten-keywords sorted-kws)
                                             null)
                                      new-plain-id ... 
                                      opt-arg ...
                                      . new-rest)
                               ;; sort out the arguments into the user-supplied bindings,
                               ;; evaluating default-value expressions as needed:
                               #,(syntax-property
                                  (quasisyntax/loc stx ; kw-opt profiler uses this srcloc
                                    (let-maybe ([id opt-expr kind] ... . rest)
                                               (kw-arg ...)
                                               (opt-arg ...)
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
                                      . new-rest)
                               ;; sort out the arguments into the user-supplied bindings,
                               ;; evaluating default-value expressions as needed:
                               #,(syntax-property
                                  (quasisyntax/loc stx ; kw-opt profiler uses this srcloc
                                    (let-kws given-kws given-args kws-sorted
                                             #,(syntax-property
                                                #`(core #,@(flatten-keywords sorted-kws)
                                                        new-plain-id ... opt-arg ...
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
                                        ([opt-id opt-arg opt-not-supplied] ...) (plain-id ...) 
                                        () ()
                                        (rest-empty rest-id . rest) ()))))]
                       [mk-with-kws
                        (lambda ()
                          ;; entry point with keywords:
                          (if (and (null? opts)
                                   (null? #'new-rest))
                              #'core
                              (annotate-method
                               (syntax/loc stx
                                 (opt-cases (unpack) ([opt-id opt-arg opt-not-supplied] ...) (given-kws given-args plain-id ...) 
                                            () ()
                                            (rest-empty rest-id . rest) ())))))]
                       [mk-kw-arity-stub
                        (lambda ()
                          ;; struct-type entry point for no keywords when a keyword is required
                          (annotate-method
                           (syntax/loc stx
                             (fail-opt-cases (missing-kw) (opt-id ...) (self plain-id ...) 
                                             ()
                                             (rest-id . fail-rest) ()))))]
                       [kw-k* (lambda (impl kwimpl wrap)
                                (kw-k impl kwimpl wrap #'core #'unpack
                                      (length plain-ids) opt-not-supplieds
                                      (not (null? (syntax-e #'rest)))
                                      needed-kws
                                      sorted-kws))])
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
                                    [(_ mk-id . _) (with-syntax ([n (or local-name
                                                                        (syntax-local-infer-name stx)
                                                                        'unknown)]
                                                                 [call-fail (mk-kw-arity-stub)])
                                                     (syntax-local-lift-values-expression
                                                      5
                                                      #'(make-required* struct:kp/ae 'n call-fail)))])
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
                  (lambda (impl kwimpl wrap core-id unpack-id n-req opt-not-supplieds rest? req-kws all-kws)
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
      [(_ kws kw-args ([kw arg arg? #f not-supplied-val]) . body)
       ;; last optional argument doesn't need to check as much or take as many cdrs
       (let ([arg? (pair? kws)])
         (let ([arg (if arg? (car kw-args) not-supplied-val)])
           . body))]
      [(_ kws kw-args ([kw arg arg? #f not-supplied-val] . rest) . body)
       (let ([arg? (and (pair? kws)
                        (eq? 'kw (car kws)))])
         (let ([arg (if arg? (car kw-args) not-supplied-val)]
               [kws (if arg? (cdr kws) kws)]
               [kw-args (if arg? (cdr kw-args) kw-args)])
           (let-kws kws kw-args rest . body)))]
      [(_ kws kw-args ([kw arg arg? #t _]) . body)
       ;; last required argument doesn't need to take cdrs
       (let ([arg (car kw-args)])
         . body)]
      [(_ kws kw-args ([kw arg arg? #t _] . rest) . body)
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
      [(_ (core ...) () (base ...)
          () ()
          (rest-empty rest-id . rest) ())
       ;; This case only happens when there are no optional arguments
       (case-lambda
         [(base ... . rest-id)
          (core ... base ... . rest)])]
      [(_ (core ...) ([opt-id opt-arg not-supplied-val]) (base ...)
          (done-id ...) (done-not-supplied ...)
          (rest-empty rest-id . rest) clauses)
       ;; Handle the last optional argument and the rest args (if any)
       ;; at the same time.
       (case-lambda
         [(base ...) (core ... base ... done-not-supplied ... not-supplied-val . rest-empty)]
         [(base ... done-id ... opt-arg . rest-id)
          (core ... base ... done-id ... opt-arg . rest)]
         . clauses)]
      [(_ (core ...) ([opt-id opt-arg not-supplied-val] [more-id more-arg more-not-supplied] ...) (base ...)
          (done-id ...) (done-not-supplied ...)
          (rest-empty rest-id . rest) clauses)
       ;; Handle just one optional argument, add it to the "done" sequence,
       ;; and continue generating clauses for the remaining optional arguments.
       (opt-cases (core ...) ([more-id more-arg more-not-supplied] ...) (base ...)
                  (done-id ... opt-id) (done-not-supplied ... not-supplied-val)
                  (rest-empty rest-id . rest)
                  ([(base ... done-id ... opt-arg)
                    (core ... base ... 
                          done-id ... opt-arg more-not-supplied ... . rest-empty)]
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
      [(_ () () () () . body)
       (syntax-property
        #'(let () . body)
        'feature-profile:kw-opt-protocol 'antimark)]
      [(_ ([id ignore #:plain] . more) kw-args opt-args (req-id . req-ids) . body)
       #'(let ([id req-id])
           (let-maybe more kw-args opt-args req-ids . body))]
      [(_ ([id expr #:opt] . more)  kw-args (opt-arg . opt-args) req-ids . body)
       #`(let ([id #,(wrap-default-check #'opt-arg #'expr)])
           (let-maybe more kw-args opt-args req-ids . body))]
      [(_ ([id expr #:kw-req] . more)  (kw-arg . kw-args) opt-args req-ids . body)
       #'(let ([id kw-arg])
           (let-maybe more kw-args opt-args req-ids . body))]
      [(_ ([id expr #:kw-opt] . more) (kw-arg . kw-args) opt-args req-ids . body)
       #`(let ([id #,(wrap-default-check #'kw-arg #'expr)])
           (let-maybe more kw-args opt-args req-ids . body))]
      [(_ (id) () () (req-id) . body)
       (syntax-property
        #'(let ([id req-id]) . body)
        'feature-profile:kw-opt-protocol 'antimark)]))

  (define-for-syntax (wrap-default-check arg-id expr)
    (with-syntax ([arg-id arg-id])
      (with-syntax ([tst (if (immediate-default? expr)
                             (syntax-property #'(#%expression #f)
                                              'typed-racket:ignore-type-information
                                              #t)
                             #'(eq? arg-id unsafe-undefined))]
                    [expr expr])
        #'(if tst
              expr
              arg-id))))

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
                                                n-req opt-not-supplieds rest? req-kws all-kws)
                                    (with-syntax ([proc (car (generate-temporaries (list id)))])
                                      (syntax-protect
                                       (quasisyntax/loc stx
                                         (begin
                                           #,(quasisyntax/loc stx
                                               (define-syntax #,id 
                                                 (make-keyword-syntax (lambda () 
                                                                        (values (quote-syntax #,core-id)
                                                                                (quote-syntax proc)))
                                                                      #,n-req '#,opt-not-supplieds #,rest? 
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

  (define-for-syntax (make-keyword-syntax get-ids n-req opt-not-supplieds rest? req-kws all-kws)
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
                                        (syntax-taint (syntax-local-introduce wrap-id))))]
                [n-opt (length opt-not-supplieds)])
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
                                                                 [(keyword<? (caar all-kws) kw)
                                                                  (loop (cdr all-kws))]
                                                                 [else all-kws]))])
                                                (cond
                                                 [(or (null? all-kws)
                                                      (not (eq? kw (caar all-kws))))
                                                  (warning
                                                   (format "keyword ~a that is not accepted" kw))
                                                  #f]
                                                 [(and (pair? req-kws)
                                                       (eq? kw (car req-kws)))
                                                  (loop (cdr kw-args) (cdr req-kws) (cdr all-kws))]
                                                 [(and (pair? req-kws)
                                                       (keyword<? (car req-kws) (caar all-kws)))
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
                                                #,@(let loop ([kw-args kw-args] [all-kws all-kws])
                                                     (cond
                                                      [(null? all-kws) null]
                                                      [(and (pair? kw-args)
                                                            (eq? (syntax-e (caar kw-args)) (caar all-kws)))
                                                       (cons (cdar kw-args) 
                                                             (loop (cdr kw-args) (cdr all-kws)))]
                                                      [else
                                                       (cons (list-ref (car all-kws) 4)
                                                             (loop kw-args (cdr all-kws)))]))
                                                ;; required arguments:
                                                #,@(let loop ([i n-req] [args args])
                                                     (if (zero? i)
                                                         null
                                                         (cons (car args)
                                                               (loop (sub1 i) (cdr args)))))
                                                ;; optional arguments:
                                                #,@(let loop ([opt-not-supplieds opt-not-supplieds] [args (list-tail args n-req)])
                                                     (cond
                                                      [(null? opt-not-supplieds) null]
                                                      [(null? args)
                                                       (cons (car opt-not-supplieds)
                                                             (loop (cdr opt-not-supplieds) null))]
                                                      [else
                                                       (cons (car args) (loop (cdr opt-not-supplieds) (cdr args)))]))
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
  (define (make-keyword-checker req-kws allowed-kws arity-mask)
    ;; If min-args is #f, then max-args is an arity value.
    ;; If max-args is #f, then >= min-args is accepted.
    (define-syntax (arity-check-lambda stx)
      (syntax-case stx ()
        [(_ (kws) kw-body)
         #'(lambda (kws a) (and kw-body (bitwise-bit-set? arity-mask a)))]))
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
        (if (and (list? req-kws) ; reundant, but helps inferences of no side effects
                 (list? allowed-kws) ; also for inference
                 (eq? (length req-kws) (length allowed-kws)))
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
  (define procedure-reduce-keyword-arity 
    (case-lambda
      [(proc arity req-kw allowed-kw name)
       (do-procedure-reduce-keyword-arity 'procedure-reduce-keyword-arity proc arity #f name req-kw allowed-kw)]
      [(proc arity req-kw allowed-kw)
       (do-procedure-reduce-keyword-arity 'procedure-reduce-keyword-arity proc arity #f #f req-kw allowed-kw)]))
  (define procedure-reduce-keyword-arity-mask
    (case-lambda
      [(proc mask req-kw allowed-kw name)
       (do-procedure-reduce-keyword-arity 'procedure-reduce-keyword-arity-mask proc #f mask name req-kw allowed-kw)]
      [(proc mask req-kw allowed-kw)
       (do-procedure-reduce-keyword-arity 'procedure-reduce-keyword-arity-mask proc #f mask #f req-kw allowed-kw)]))
  
  (define (do-procedure-reduce-keyword-arity who proc arity mask name req-kw allowed-kw)
    (let* ([plain-proc (let ([p (if (okp? proc) 
                                    (okp-ref proc 0)
                                    proc)])
                         (if arity
                             (procedure-reduce-arity p arity)
                             (procedure-reduce-arity-mask p mask name)))])
      (define (sorted? kws)
        (let loop ([kws kws])
          (cond
           [(null? kws) #t]
           [(null? (cdr kws)) #t]
           [(keyword<? (car kws) (cadr kws)) (loop (cdr kws))]
           [else #f])))

      (unless (and (list? req-kw) (andmap keyword? req-kw)
                   (sorted? req-kw))
        (raise-argument-error who "(and/c (listof? keyword?) sorted? distinct?)"
                              2 proc (or arity mask) req-kw allowed-kw))
      (when allowed-kw
        (unless (and (list? allowed-kw) (andmap keyword? allowed-kw)
                     (sorted? allowed-kw))
          (raise-argument-error who "(or/c (and/c (listof? keyword?) sorted? distinct?) #f)"
                                3 proc (or arity mask) req-kw allowed-kw))
        (unless (subset? req-kw allowed-kw)
          (raise-arguments-error who
                                 "allowed-keyword list does not include all required keywords"
                                 "allowed-keyword list" allowed-kw
                                 "required keywords" req-kw)))
      (let-values ([(old-req old-allowed) (procedure-keywords proc)])
        (unless (subset? old-req req-kw)
          (raise-arguments-error who
                                 "cannot reduce required keyword set"
                                 "required keywords" old-req
                                 "requested required keywords" req-kw))
        (when old-allowed
          (unless (subset? req-kw old-allowed)
            (raise-arguments-error who
                                   "cannot require keywords not in original allowed set"
                                   "original allowed keywords" old-allowed
                                   "requested required keywords" req-kw))
          (unless (or (not allowed-kw)
                      (subset? allowed-kw old-allowed))
            (raise-arguments-error who
                                   "cannot allow keywords not in original allowed set"
                                   "original allowed keywords" old-allowed
                                   "requested allowed keywords" allowed-kw))))
      (if (null? allowed-kw)
          plain-proc
          (let* ([mask (or mask (arity->mask arity))]
                 [new-mask (arithmetic-shift mask 2)]
                 [kw-checker (make-keyword-checker req-kw allowed-kw new-mask)]
                 [proc (normalize-proc proc)]
                 [new-kw-proc (procedure-reduce-arity-mask (keyword-procedure-proc proc)
                                                           new-mask)])
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
                                (procedure-reduce-arity-mask
                                 missing-kw
                                 (arithmetic-shift mask 1))
                                (or (okm? proc)
                                    (keyword-method? proc))
                                #f)
                 kw-checker
                 new-kw-proc
                 req-kw
                 allowed-kw))))))

  (define (arity->mask a)
    (cond
      [(exact-nonnegative-integer? a)
       (arithmetic-shift 1 a)]
      [(arity-at-least? a)
       (bitwise-xor -1 (sub1 (arithmetic-shift 1 (arity-at-least-value a))))]
      [(list? a)
       (let loop ([mask 0] [l a])
         (cond
           [(null? l) mask]
           [else
            (let ([a (car l)])
              (cond
                [(or (exact-nonnegative-integer? a)
                     (arity-at-least? a))
                 (loop (bitwise-ior mask (arity->mask a)) (cdr l))]
                [else #f]))]))]
      [else #f]))

  (define new:procedure-reduce-arity
    (let ([procedure-reduce-arity
           (case-lambda
             [(proc arity name)
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
                                          arity
                                          name))]
             [(proc arity)
              (new:procedure-reduce-arity proc arity #f)])])
      procedure-reduce-arity))

  (define new:procedure-reduce-arity-mask
    (let ([procedure-reduce-arity
           (case-lambda
             [(proc mask name)
              (if (and (procedure? proc)
                       (let-values ([(req allows) (procedure-keywords proc)])
                         (pair? req))
                       (not (eqv? mask 0)))
                  (raise-arguments-error 'procedure-reduce-arity
                                         "procedure has required keyword arguments"
                                         "procedure" proc)
                  (procedure-reduce-arity-mask (if (okm? proc)
                                                   (procedure->method proc)
                                                   proc)
                                               mask
                                               name))]
             [(proc mask)
              (new:procedure-reduce-arity-mask proc mask #f)])])
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
                        make-optional-keyword-method
                        make-optional-keyword-procedure)
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
                                             ;; Add back `kws` in the right place among the results:
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
                                 (let ([new-kw-proc
                                        ((if is-impersonator?
                                             impersonate-struct
                                             chaperone-struct)
                                         (if (okp? n-proc)
                                             ;; All keyword arguments are optional, so need to
                                             ;; chaperone as a plain procedure, too:
                                             (chaperone-procedure proc wrap-proc)
                                             ;; Some keyword is required:
                                             proc)
                                         new-procedure-ref
                                         (lambda (self proc)
                                           ;; This `proc` takes an extra argument, which is `self`:
                                           ((if is-impersonator?
                                                new:impersonate-procedure
                                                new:chaperone-procedure)
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
                                                          (define r-len (length results))
                                                          (define (list-take l n)
                                                            (if (zero? n) null (cons (car l) (list-take (cdr l) (sub1 n)))))
                                                          ;; Drop out `kws` result, add in `self`:
                                                          (if (and (null? '(extra-arg ...))
                                                                   (= r-len (+ 2 len)))
                                                              (apply values (cadr results) self (cddr results))
                                                              (apply values (let ([skip (- r-len len)])
                                                                              (append (list-take results (- skip 2))
                                                                                      (list (list-ref results (sub1 skip))
                                                                                            self)
                                                                                      (list-tail results skip))))))))]))
                                               (if self-arg?
                                                   (gen-proc proc-self)
                                                   (gen-proc)))))))])
                                   (values new-kw-proc
                                           new-procedure-ref))])]
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
