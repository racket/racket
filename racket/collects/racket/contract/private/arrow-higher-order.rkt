#lang racket/base
(require (for-syntax racket/base
                     "arr-util.rkt"
                     "helpers.rkt")
         "arity-checking.rkt"
         "kwd-info-struct.rkt"
         "blame.rkt"
         "misc.rkt"
         "prop.rkt"
         "guts.rkt"
         (prefix-in arrow: "arrow.rkt"))

(provide (for-syntax build-chaperone-constructor/real)
         procedure-arity-exactly/no-kwds
         ->-proj
         check-pre-cond
         check-post-cond
         pre-post/desc-result->string)

(define-for-syntax (build-chaperone-constructor/real this-args
                                                     mandatory-dom-projs 
                                                     optional-dom-projs
                                                     mandatory-dom-kwds
                                                     optional-dom-kwds
                                                     pre pre/desc
                                                     rest
                                                     rngs
                                                     post post/desc)
  (define (nvars n sym) (generate-temporaries (for/list ([i (in-range n)]) sym)))
  (with-syntax ([(mandatory-dom-proj ...) (generate-temporaries mandatory-dom-projs)]
                [(optional-dom-proj ...) (generate-temporaries optional-dom-projs)]
                [(mandatory-dom-kwd-proj ...) (nvars (length mandatory-dom-kwds) 'mandatory-dom-proj)]
                [(optional-dom-kwd-proj ...) (nvars (length optional-dom-kwds) 'optional-dom-proj)]
                [(rng-proj ...) (if rngs (generate-temporaries rngs) '())]
                [(rest-proj ...) (if rest (generate-temporaries '(rest-proj)) '())])
    #`(λ (blame f neg-party blame-party-info rng-ctcs
                mandatory-dom-proj ...  
                optional-dom-proj ... 
                rest-proj ...
                mandatory-dom-kwd-proj ... 
                optional-dom-kwd-proj ... 
                rng-proj ...)
        #,(create-chaperone
           #'blame #'neg-party #'blame-party-info #'f #'rng-ctcs
           this-args
           (syntax->list #'(mandatory-dom-proj ...))
           (syntax->list #'(optional-dom-proj ...))
           (map list 
                mandatory-dom-kwds
                (syntax->list #'(mandatory-dom-kwd-proj ...)))
           (map list 
                optional-dom-kwds
                (syntax->list #'(optional-dom-kwd-proj ...)))
           pre pre/desc
           (if rest (car (syntax->list #'(rest-proj ...))) #f)
           (if rngs (syntax->list #'(rng-proj ...)) #f)
           post post/desc))))


(define (check-pre-cond pre blame neg-party val)
  (with-contract-continuation-mark
   (cons blame neg-party)
   (unless (pre)
     (raise-blame-error (blame-swap blame)
                        #:missing-party neg-party
                        val "#:pre condition"))))

(define (check-post-cond post blame neg-party val)
  (with-contract-continuation-mark
   (cons blame neg-party)
   (unless (post)
     (raise-blame-error blame
                        #:missing-party neg-party
                        val "#:post condition"))))

(define (check-pre-cond/desc post blame neg-party val)
  (handle-pre-post/desc-string #t post blame neg-party val))
(define (check-post-cond/desc post blame neg-party val)
  (handle-pre-post/desc-string #f post blame neg-party val))
(define (handle-pre-post/desc-string pre? thunk blame neg-party val)
  (define condition-result (thunk))
  (cond
    [(equal? condition-result #t) 
     (void)]
    [else
     (define msg
       (pre-post/desc-result->string condition-result pre? '->*))
     (raise-blame-error (if pre? (blame-swap blame) blame)
                        #:missing-party neg-party
                        val "~a" msg)]))

(define (pre-post/desc-result->string condition-result pre? who)
  (cond
    [(equal? condition-result #f)
     (if pre?
         "#:pre condition"
         "#:post condition")]
    [(string? condition-result)
     condition-result]
    [(and (list? condition-result)
          (andmap string? condition-result))
     (apply
      string-append
      (let loop ([s condition-result])
        (cond
          [(null? s) '()]
          [(null? (cdr s)) s]
          [else (list* (car s)
                       "\n " 
                       (loop (cdr s)))])))]
    [else
     (error
      who
      "expected #:~a/desc to produce (or/c boolean? string? (listof string?)), got ~e"
      (if pre? "pre" "post")
      condition-result)]))

(define-for-syntax (create-chaperone blame neg-party blame-party-info
                                     val rng-ctcs
                                     this-args
                                     doms opt-doms
                                     req-kwds opt-kwds
                                     pre pre/desc
                                     dom-rest
                                     rngs
                                     post post/desc)
  (with-syntax ([blame blame]
                [val val])
    (with-syntax ([(pre ...) 
                   (cond
                     [pre
                      (list #`(check-pre-cond #,pre blame neg-party val))]
                     [pre/desc
                      (list #`(check-pre-cond/desc #,pre/desc blame neg-party val))]
                     [else null])]
                  [(post ...)
                   (cond
                     [post
                      (list #`(check-post-cond #,post blame neg-party val))]
                     [post/desc
                      (list #`(check-post-cond/desc #,post/desc blame neg-party val))]
                     [else null])])
      (with-syntax ([(this-param ...) this-args]
                    [(dom-ctc ...) doms]
                    [(dom-x ...) (generate-temporaries doms)]
                    [(opt-dom-ctc ...) opt-doms]
                    [(opt-dom-x ...) (generate-temporaries opt-doms)]
                    [(rest-ctc rest-x) (cons dom-rest (generate-temporaries '(rest)))]
                    [(req-kwd ...) (map car req-kwds)]
                    [(req-kwd-ctc ...) (map cadr req-kwds)]
                    [(req-kwd-x ...) (generate-temporaries (map car req-kwds))]
                    [(opt-kwd ...) (map car opt-kwds)]
                    [(opt-kwd-ctc ...) (map cadr opt-kwds)]
                    [(opt-kwd-x ...) (generate-temporaries (map car opt-kwds))]
                    [(rng-late-neg-projs ...) (if rngs rngs '())]
                    [(rng-x ...) (if rngs (generate-temporaries rngs) '())])
        (with-syntax ([(rng-checker-name ...)
                       (if rngs
                           (list (gen-id 'rng-checker))
                           null)]
                      [(rng-checker ...)
                       (if rngs
                           (list
                            (with-syntax ([rng-len (length rngs)])
                              (with-syntax ([rng-results
                                             #'(values (rng-late-neg-projs rng-x neg-party)
                                                       ...)])
                                #'(case-lambda
                                    [(rng-x ...)
                                     (with-contract-continuation-mark
                                      (cons blame neg-party)
                                      (let ()
                                        post ...
                                        rng-results))]
                                    [args
                                     (arrow:bad-number-of-results blame val rng-len args
                                                                  #:missing-party neg-party)]))))
                           null)])
          (let* ([min-method-arity (length doms)]
                 [max-method-arity (+ min-method-arity (length opt-doms))]
                 [min-arity (+ (length this-args) min-method-arity)]
                 [max-arity (+ min-arity (length opt-doms))]
                 [req-keywords (map (λ (p) (syntax-e (car p))) req-kwds)]
                 [opt-keywords (map (λ (p) (syntax-e (car p))) opt-kwds)]
                 [need-apply-values? (or dom-rest (not (null? opt-doms)))]
                 [no-rng-checking? (not rngs)])
            (with-syntax ([(dom-projd-args ...) #'((dom-ctc dom-x neg-party) ...)]
                          [basic-params
                           (cond
                             [dom-rest
                              #'(this-param ... 
                                 dom-x ...
                                 [opt-dom-x arrow:unspecified-dom] ...
                                 . 
                                 rest-x)]
                             [else
                              #'(this-param ... dom-x ... [opt-dom-x arrow:unspecified-dom] ...)])]
                          [opt+rest-uses
                           (for/fold ([i (if dom-rest #'(rest-ctc rest-x neg-party) #'null)])
                             ([o (in-list (reverse
                                           (syntax->list
                                            #'((opt-dom-ctc opt-dom-x neg-party) ...))))]
                              [opt-dom-x (in-list (reverse (syntax->list #'(opt-dom-x ...))))])
                             #`(let ([r #,i])
                                 (if (eq? arrow:unspecified-dom #,opt-dom-x) r (cons #,o r))))]
                          [(kwd-param ...)
                           (apply 
                            append
                            (map list
                                 (syntax->list #'(req-kwd ... opt-kwd ...))
                                 (syntax->list #'(req-kwd-x ... 
                                                  [opt-kwd-x arrow:unspecified-dom] ...))))]
                          [kwd-stx
                           (let* ([req-stxs
                                   (map (λ (s) (λ (r) #`(cons #,s #,r)))
                                        (syntax->list #'((req-kwd-ctc req-kwd-x neg-party) ...)))]
                                  [opt-stxs 
                                   (map (λ (x c) (λ (r) #`(maybe-cons-kwd #,c #,x #,r neg-party)))
                                        (syntax->list #'(opt-kwd-x ...))
                                        (syntax->list #'(opt-kwd-ctc ...)))]
                                  [reqs (map cons req-keywords req-stxs)]
                                  [opts (map cons opt-keywords opt-stxs)]
                                  [all-together-now (append reqs opts)]
                                  [put-in-reverse (sort all-together-now 
                                                        (λ (k1 k2) (keyword<? k2 k1))
                                                        #:key car)])
                             (for/fold ([s #'null])
                               ([tx (in-list (map cdr put-in-reverse))])
                               (tx s)))])
              (with-syntax ([kwd-lam-params
                             (if dom-rest
                                 #'(this-param ...
                                    dom-x ... 
                                    [opt-dom-x arrow:unspecified-dom] ...
                                    kwd-param ... . rest-x)
                                 #'(this-param ...
                                    dom-x ...
                                    [opt-dom-x arrow:unspecified-dom] ...
                                    kwd-param ...))]
                            [basic-return
                             (let ([inner-stx-gen
                                    (if need-apply-values?
                                        (λ (s) #`(apply values #,@s 
                                                        this-param ... 
                                                        dom-projd-args ... 
                                                        opt+rest-uses))
                                        (λ (s) #`(values 
                                                  #,@s 
                                                  this-param ...
                                                  dom-projd-args ...)))])
                               (if no-rng-checking?
                                   (inner-stx-gen #'())
                                   (arrow:check-tail-contract rng-ctcs
                                                              blame-party-info
                                                              neg-party
                                                              #'(rng-checker-name ...)
                                                              inner-stx-gen)))]
                            [kwd-return
                             (let* ([inner-stx-gen
                                     (if need-apply-values?
                                         (λ (s k) #`(apply values 
                                                           #,@s #,@k 
                                                           this-param ...
                                                           dom-projd-args ...
                                                           opt+rest-uses))
                                         (λ (s k) #`(values #,@s #,@k 
                                                            this-param ...
                                                            dom-projd-args ...)))]
                                    [outer-stx-gen
                                     (if (null? req-keywords)
                                         (λ (s)
                                           #`(if (null? kwd-results)
                                                 #,(inner-stx-gen s #'())
                                                 #,(inner-stx-gen s #'(kwd-results))))
                                         (λ (s)
                                           (inner-stx-gen s #'(kwd-results))))])
                               #`(let ([kwd-results kwd-stx])
                                   #,(if no-rng-checking?
                                         (outer-stx-gen #'())
                                         (arrow:check-tail-contract rng-ctcs
                                                                    blame-party-info
                                                                    neg-party
                                                                    #'(rng-checker-name ...)
                                                                    outer-stx-gen))))])
                (with-syntax ([basic-lambda-name (gen-id 'basic-lambda)]
                              [basic-lambda #'(λ basic-params
                                                ;; Arrow contract domain checking is instrumented
                                                ;; both here, and in `arity-checking-wrapper'.
                                                ;; We need to instrument here, because sometimes
                                                ;; a-c-w doesn't wrap, and just returns us.
                                                ;; We need to instrument in a-c-w to count arity
                                                ;; checking time.
                                                ;; Overhead of double-wrapping has not been
                                                ;; noticeable in my measurements so far.
                                                ;;  - stamourv
                                                (with-contract-continuation-mark
                                                 (cons blame neg-party)
                                                 (let ()
                                                   pre ... basic-return)))]
                              [kwd-lambda-name (gen-id 'kwd-lambda)]
                              [kwd-lambda #`(λ kwd-lam-params
                                              (with-contract-continuation-mark
                                               (cons blame neg-party)
                                               (let ()
                                                 pre ... kwd-return)))])
                  (with-syntax ([(basic-checker-name) (generate-temporaries '(basic-checker))])
                    (cond
                      [(and (null? req-keywords) (null? opt-keywords))
                       #`(let-values ([(rng-checker-name ...) (values rng-checker ...)])
                           (let ([basic-lambda-name basic-lambda])
                             (arrow:arity-checking-wrapper val 
                                                           blame neg-party
                                                           basic-lambda-name
                                                           void
                                                           #,min-method-arity
                                                           #,max-method-arity
                                                           #,min-arity
                                                           #,(if dom-rest #f max-arity)
                                                           '(req-kwd ...)
                                                           '(opt-kwd ...))))]
                      [(pair? req-keywords)
                       #`(let-values ([(rng-checker-name ...) (values rng-checker ...)])
                           (let ([kwd-lambda-name kwd-lambda])
                             (arrow:arity-checking-wrapper val
                                                           blame neg-party
                                                           void
                                                           kwd-lambda-name
                                                           #,min-method-arity
                                                           #,max-method-arity
                                                           #,min-arity
                                                           #,(if dom-rest #f max-arity)
                                                           '(req-kwd ...)
                                                           '(opt-kwd ...))))]
                      [else
                       #`(let-values ([(rng-checker-name ...) (values rng-checker ...)])
                           (let ([basic-lambda-name basic-lambda]
                                 [kwd-lambda-name kwd-lambda])
                             (arrow:arity-checking-wrapper val 
                                                           blame neg-party
                                                           basic-lambda-name
                                                           kwd-lambda-name
                                                           #,min-method-arity
                                                           #,max-method-arity
                                                           #,min-arity
                                                           #,(if dom-rest #f max-arity)
                                                           '(req-kwd ...)
                                                           '(opt-kwd ...))))])))))))))))

(define (maybe-cons-kwd c x r neg-party)
  (if (eq? arrow:unspecified-dom x)
      r
      (cons (c x neg-party) r)))

(define (->-proj chaperone-or-impersonate-procedure ctc
                 ;; fields of the 'ctc' struct
                 min-arity doms kwd-infos rest pre? rngs post?
                 plus-one-arity-function chaperone-constructor
                 late-neg?)
  (define optionals-length (- (length doms) min-arity))
  (define mtd? #f) ;; not yet supported for the new contracts
  (define okay-to-do-only-arity-check?
    (and (not rest)
         (not pre?)
         (not post?)
         (null? kwd-infos)
         (not rngs)
         (andmap any/c? doms)
         (= optionals-length 0)))
  (λ (orig-blame)
    (define rng-blame (arrow:blame-add-range-context orig-blame))
    (define swapped-domain (blame-add-context orig-blame "the domain of" #:swap? #t))

    (define partial-doms
      (for/list ([dom (in-list doms)]
                 [n (in-naturals 1)])
        ((get/build-late-neg-projection dom)
         (blame-add-context orig-blame 
                            (format "the ~a argument of" (n->th n))
                            #:swap? #t))))
    (define partial-rest (and rest
                              ((get/build-late-neg-projection rest)
                               (blame-add-context orig-blame "the rest argument of"
                                                  #:swap? #t))))
    (define partial-ranges
      (if rngs
          (for/list ([rng (in-list rngs)])
            ((get/build-late-neg-projection rng) rng-blame))
          '()))
    (define partial-kwds 
      (for/list ([kwd-info (in-list kwd-infos)]
                 [kwd (in-list kwd-infos)])
        ((get/build-late-neg-projection (kwd-info-ctc kwd-info))
         (blame-add-context orig-blame
                            (format "the ~a argument of" (kwd-info-kwd kwd))
                            #:swap? #t))))
    (define man-then-opt-partial-kwds
      (append (for/list ([partial-kwd (in-list partial-kwds)]
                         [kwd-info (in-list kwd-infos)]
                         #:when (kwd-info-mandatory? kwd-info))
                partial-kwd)
              (for/list ([partial-kwd (in-list partial-kwds)]
                         [kwd-info (in-list kwd-infos)]
                         #:unless (kwd-info-mandatory? kwd-info))
                partial-kwd)))
    
    (define the-args (append partial-doms
                             (if partial-rest (list partial-rest) '())
                             man-then-opt-partial-kwds
                             partial-ranges))
    (define plus-one-constructor-args
      (append partial-doms
              man-then-opt-partial-kwds
              partial-ranges
              (if partial-rest (list partial-rest) '())))
    (define blame-party-info (arrow:get-blame-party-info orig-blame))
    (define (successfully-got-the-right-kind-of-function val neg-party)
      (define chap/imp-func (apply chaperone-constructor
                                   orig-blame val
                                   neg-party blame-party-info
                                   rngs the-args))
      (cond
        [chap/imp-func
         (if (or post? (not rngs))
             (chaperone-or-impersonate-procedure
              val
              chap/imp-func
              impersonator-prop:contracted ctc
              impersonator-prop:blame (blame-add-missing-party orig-blame neg-party))
             (chaperone-or-impersonate-procedure
              val
              chap/imp-func
              impersonator-prop:contracted ctc
              impersonator-prop:blame (blame-add-missing-party orig-blame neg-party)
              impersonator-prop:application-mark
              (cons arrow:tail-contract-key (list* neg-party blame-party-info rngs))))]
        [else val]))
    (cond
      [late-neg?
       (define (arrow-higher-order:lnp val neg-party)
         (cond
           [(do-arity-checking orig-blame val doms rest min-arity kwd-infos)
            =>
            (λ (f)
              (f neg-party))]
           [else
            (successfully-got-the-right-kind-of-function val neg-party)]))
       (if okay-to-do-only-arity-check?
           (λ (val neg-party)
             (cond
               [(procedure-arity-exactly/no-kwds val min-arity) val]
               [else (arrow-higher-order:lnp val neg-party)]))
           arrow-higher-order:lnp)]
      [else
       (define (arrow-higher-order:vfp val)
         (wrapped-extra-arg-arrow 
          (cond
            [(do-arity-checking orig-blame val doms rest min-arity kwd-infos)
             =>
             values]
            [else
             (λ (neg-party)
               (successfully-got-the-right-kind-of-function val neg-party))])
          (apply plus-one-arity-function orig-blame val plus-one-constructor-args)))
       (if okay-to-do-only-arity-check?
           (λ (val)
             (cond
               [(procedure-arity-exactly/no-kwds val min-arity)
                (wrapped-extra-arg-arrow 
                 (λ (neg-party) val)
                 (apply plus-one-arity-function orig-blame val plus-one-constructor-args))]
               [else (arrow-higher-order:vfp val)]))
           arrow-higher-order:vfp)])))

(define (procedure-arity-exactly/no-kwds val min-arity)
  (and (procedure? val)
       (equal? (procedure-arity val) min-arity)
       (let-values ([(man opt) (procedure-keywords val)])
         (and (null? man)
              (null? opt)))))
