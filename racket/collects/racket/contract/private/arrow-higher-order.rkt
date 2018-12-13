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
         "list.rkt"
         (prefix-in arrow: "arrow-common.rkt")
         "arrow-collapsible.rkt"
         "collapsible-common.rkt"
         (submod "collapsible-common.rkt" properties)
         (only-in racket/unsafe/ops
                  unsafe-chaperone-procedure
                  unsafe-impersonate-procedure))

(provide (for-syntax build-chaperone-constructor/real)
         ->-proj
         check-pre-cond
         check-post-cond
         arity-checking-wrapper)

(define-for-syntax (build-chaperone-constructor/real ;; (listof (or/c #f stx))
                                                     ;; #f => syntactically known to be any/c
                                                     mandatory-dom-projs
                                                     
                                                     optional-dom-projs
                                                     mandatory-dom-kwds
                                                     optional-dom-kwds
                                                     pre pre/desc
                                                     rest
                                                     rngs
                                                     post post/desc
                                                     method?)
  (define (nvars n sym) (generate-temporaries (for/list ([i (in-range n)]) sym)))
  (with-syntax ([(mandatory-dom-proj ...) (generate-temporaries mandatory-dom-projs)]
                [(optional-dom-proj ...) (generate-temporaries optional-dom-projs)]
                [(mandatory-dom-kwd-proj ...) (nvars (length mandatory-dom-kwds) 'mandatory-dom-proj)]
                [(optional-dom-kwd-proj ...) (nvars (length optional-dom-kwds) 'optional-dom-proj)]
                [(rng-proj ...) (if rngs (generate-temporaries rngs) '())]
                [(rest-proj ...) (if rest (generate-temporaries '(rest-proj)) '())])
    #`(λ (blame f neg-party blame-party-info is-impersonator? rng-ctcs
                mandatory-dom-proj ...  
                optional-dom-proj ... 
                rest-proj ...
                mandatory-dom-kwd-proj ... 
                optional-dom-kwd-proj ... 
                rng-proj ...)
        (define blame+neg-party (cons blame neg-party))
        #,(create-chaperone
           #'blame #'neg-party #'blame+neg-party #'blame-party-info #'is-impersonator? #'f #'rng-ctcs
           (for/list ([id (in-list (syntax->list #'(mandatory-dom-proj ...)))]
                      [mandatory-dom-proj (in-list mandatory-dom-projs)])
             (and mandatory-dom-proj id))
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
           post post/desc
           method?))))


(define (check-pre-cond pre blame neg-party blame+neg-party val)
  (with-contract-continuation-mark
   blame+neg-party
   (unless (pre)
     (raise-blame-error (blame-swap blame)
                        #:missing-party neg-party
                        val "#:pre condition"))))

(define (check-post-cond post blame neg-party blame+neg-party val)
  (with-contract-continuation-mark
   blame+neg-party
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
       (arrow:pre-post/desc-result->string condition-result pre? '->*))
     (raise-blame-error (if pre? (blame-swap blame) blame)
                        #:missing-party neg-party
                        val "~a" msg)]))

(define-for-syntax (create-chaperone blame neg-party blame+neg-party blame-party-info is-impersonator?
                                     val rng-ctcs
                                     doms opt-doms
                                     req-kwds opt-kwds
                                     pre pre/desc
                                     dom-rest
                                     rngs
                                     post post/desc
                                     method?)
  (with-syntax ([blame blame]
                [blame+neg-party blame+neg-party]
                [val val]
                [is-impersonator? is-impersonator?])
    (with-syntax ([(pre ...) 
                   (cond
                     [pre
                      (list #`(check-pre-cond #,pre blame neg-party blame+neg-party val))]
                     [pre/desc
                      (list #`(check-pre-cond/desc #,pre/desc blame neg-party val))]
                     [else null])]
                  [(post ...)
                   (cond
                     [post
                      (list #`(check-post-cond #,post blame neg-party blame+neg-party val))]
                     [post/desc
                      (list #`(check-post-cond/desc #,post/desc blame neg-party val))]
                     [else null])])
      (with-syntax ([(dom-x ...) (generate-temporaries doms)]
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

        (define rng-checker
          (and rngs
               (with-syntax ([rng-len (length rngs)]
                             [rng-results #'(values (rng-late-neg-projs rng-x neg-party) ...)])
                 #'(case-lambda
                     [(rng-x ...)
                      (with-contract-continuation-mark
                       blame+neg-party
                       (let ()
                         post ...
                         rng-results))]
                     [args
                      (arrow:bad-number-of-results blame val rng-len args
                                                   #:missing-party neg-party)]))))
        (define (wrap-call-with-values-and-range-checking stx assume-result-values?)
          (if rngs
              (if assume-result-values?
                  #`(let-values ([(rng-x ...) #,stx])
                      (with-contract-continuation-mark
                       blame+neg-party
                       (let ()
                         post ...
                         (values (rng-late-neg-projs rng-x neg-party) ...))))
                  #`(call-with-values
                     (λ () #,stx)
                     #,rng-checker))
              stx))

          (let* ([min-arity (length doms)]
                 [max-arity (+ min-arity (length opt-doms))]
                 [req-keywords (map (λ (p) (syntax-e (car p))) req-kwds)]
                 [opt-keywords (map (λ (p) (syntax-e (car p))) opt-kwds)]
                 [need-apply? (or dom-rest (not (null? opt-doms)))])
            (with-syntax ([(dom-projd-args ...)
                           (for/list ([dom (in-list doms)]
                                      [dom-x (in-list (syntax->list #'(dom-x ...)))])
                             (if dom
                                 #`(#,dom #,dom-x neg-party)
                                 dom-x))]
                          [basic-params
                           (cond
                             [dom-rest
                              #'(dom-x ...
                                 [opt-dom-x arrow:unspecified-dom] ...
                                 . 
                                 rest-x)]
                             [else
                              #'(dom-x ... [opt-dom-x arrow:unspecified-dom] ...)])]
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
                                 #'(dom-x ... 
                                    [opt-dom-x arrow:unspecified-dom] ...
                                    kwd-param ... . rest-x)
                                 #'(dom-x ...
                                    [opt-dom-x arrow:unspecified-dom] ...
                                    kwd-param ...))]
                            [(basic-return basic-return/no-tail)
                             (let ()
                               (define inner-stx-gen
                                 (if need-apply?
                                     (λ (s) #`(apply values #,@s
                                                     dom-projd-args ...
                                                     opt+rest-uses))
                                     (λ (s) #`(values
                                               #,@s
                                               dom-projd-args ...))))
                               (list (if rngs
                                         (arrow:check-tail-contract rng-ctcs
                                                                    blame-party-info
                                                                    neg-party
                                                                    (list rng-checker)
                                                                    inner-stx-gen
                                                                    #'(cons blame neg-party))
                                         (inner-stx-gen #'()))
                                     (if rngs
                                         (inner-stx-gen (list rng-checker))
                                         (inner-stx-gen #'()))))]
                            [(basic-unsafe-return
                              basic-unsafe-return/result-values-assumed
                              basic-unsafe-return/result-values-assumed/no-tail)
                             (let ()
                               (define (inner-stx-gen stuff assume-result-values? do-tail-check?)
                                 (define arg-checking-expressions
                                   (if need-apply?
                                       #'(dom-projd-args ... opt+rest-uses)
                                       #'(dom-projd-args ...)))
                                 (define the-call/no-tail-mark
                                   (cond
                                     [(for/and ([dom (in-list doms)])
                                        (not dom))
                                      (if need-apply?
                                          #`(apply val #,@arg-checking-expressions)
                                          #`(val #,@arg-checking-expressions))]
                                     [else
                                      (with-syntax ([(tmps ...) (generate-temporaries
                                                                 arg-checking-expressions)])
                                        #`(let-values ([(tmps ...)
                                                        (with-contract-continuation-mark
                                                         blame+neg-party
                                                         (values #,@arg-checking-expressions))])
                                            #,(if need-apply?
                                                  #`(apply val tmps ...)
                                                  #`(val tmps ...))))]))
                                 (define the-call
                                   (if do-tail-check?
                                       #`(with-continuation-mark arrow:tail-contract-key
                                           (list* neg-party blame-party-info #,rng-ctcs)
                                           #,the-call/no-tail-mark)
                                       the-call/no-tail-mark))
                                 (cond
                                   [(null? (syntax-e stuff)) ;; surely there must a better way
                                    the-call/no-tail-mark]
                                   [else
                                    (wrap-call-with-values-and-range-checking
                                     the-call
                                     assume-result-values?)]))
                               (define (mk-return assume-result-values? do-tail-check?)
                                 (if do-tail-check?
                                     (if rngs
                                         (arrow:check-tail-contract
                                          rng-ctcs
                                          blame-party-info
                                          neg-party
                                          #'not-a-null
                                          (λ (x)
                                            (inner-stx-gen x assume-result-values? do-tail-check?))
                                          #'(cons blame neg-party))
                                         (inner-stx-gen #'() assume-result-values? do-tail-check?))
                                     (inner-stx-gen #'not-a-null assume-result-values?
                                                    do-tail-check?)))
                               (list (mk-return #f #t) (mk-return #t #t) (mk-return #t #f)))]
                            [(kwd-return kwd-return/no-tail)
                             (let* ([inner-stx-gen
                                     (if need-apply?
                                         (λ (s k) #`(apply values 
                                                           #,@s #,@k 
                                                           dom-projd-args ...
                                                           opt+rest-uses))
                                         (λ (s k) #`(values #,@s #,@k 
                                                            dom-projd-args ...)))]
                                    [outer-stx-gen
                                     (if (null? req-keywords)
                                         (λ (s)
                                           #`(if (null? kwd-results)
                                                 #,(inner-stx-gen s #'())
                                                 #,(inner-stx-gen s #'(kwd-results))))
                                         (λ (s)
                                           (inner-stx-gen s #'(kwd-results))))])
                               (list #`(let ([kwd-results kwd-stx])
                                         #,(if rngs
                                               (arrow:check-tail-contract rng-ctcs
                                                                          blame-party-info
                                                                          neg-party
                                                                          (list rng-checker)
                                                                          outer-stx-gen
                                                                          #'(cons blame neg-party))
                                               (outer-stx-gen #'())))
                                     #`(let ([kwd-results kwd-stx])
                                         #,(if rngs
                                               (outer-stx-gen (list rng-checker))
                                               (outer-stx-gen #'())))))])

                ;; Arrow contract domain checking is instrumented
                ;; both here, and in `arity-checking-wrapper'.
                ;; We need to instrument here, because sometimes
                ;; a-c-w doesn't wrap, and just returns us.
                ;; We need to instrument in a-c-w to count arity
                ;; checking time.
                ;; Overhead of double-wrapping has not been
                ;; noticeable in my measurements so far.
                ;;  - stamourv
                (with-syntax ([basic-lambda #'(λ basic-params
                                                (with-contract-continuation-mark
                                                 blame+neg-party
                                                 (let ()
                                                   pre ... basic-return)))]
                              [basic-lambda/no-tail
                               #'(λ basic-params
                                   (with-contract-continuation-mark
                                       blame+neg-party
                                     (let ()
                                       pre ... basic-return/no-tail)))]
                              [basic-unsafe-lambda
                               #'(λ basic-params
                                   (let ()
                                     pre ... basic-unsafe-return))]
                              [basic-unsafe-lambda/result-values-assumed
                               #'(λ basic-params
                                   (let ()
                                     pre ... basic-unsafe-return/result-values-assumed))]
                              [basic-unsafe-lambda/result-values-assumed/no-tail
                               #'(λ basic-params
                                   (let ()
                                     pre ... basic-unsafe-return/result-values-assumed/no-tail))]
                              [kwd-lambda-name (gen-id 'kwd-lambda)]
                              [kwd-lambda #`(λ kwd-lam-params
                                              (with-contract-continuation-mark
                                               blame+neg-party
                                               (let ()
                                                 pre ... kwd-return)))]
                              [kwd-lambda/no-tail #`(λ kwd-lam-params
                                                      (with-contract-continuation-mark
                                                          blame+neg-party
                                                        (let ()
                                                          pre ... kwd-return/no-tail)))])
                  (cond
                   [(and (null? req-keywords) (null? opt-keywords))
                    #`(arity-checking-wrapper val
                                              blame neg-party blame+neg-party
                                              basic-lambda basic-lambda/no-tail
                                              basic-unsafe-lambda
                                              basic-unsafe-lambda/result-values-assumed
                                              basic-unsafe-lambda/result-values-assumed/no-tail
                                              #,(and rngs (length rngs))
                                              void void
                                              #,min-arity
                                              #,(if dom-rest #f max-arity)
                                              '(req-kwd ...)
                                              '(opt-kwd ...)
                                              #,method?
                                              is-impersonator?)]
                   [(pair? req-keywords)
                    #`(arity-checking-wrapper val
                                              blame neg-party blame+neg-party
                                              void void #t #f #f #f
                                              kwd-lambda kwd-lambda/no-tail
                                              #,min-arity
                                              #,(if dom-rest #f max-arity)
                                              '(req-kwd ...)
                                              '(opt-kwd ...)
                                              #,method?
                                              is-impersonator?)]
                   [else
                    #`(arity-checking-wrapper val
                                              blame neg-party blame+neg-party
                                              basic-lambda basic-lambda/no-tail #t #f #f #f
                                              kwd-lambda kwd-lambda/no-tail
                                              #,min-arity
                                              #,(if dom-rest #f max-arity)
                                              '(req-kwd ...)
                                              '(opt-kwd ...)
                                              #,method?
                                              is-impersonator?)])))))))))

;; should we pass both the basic-lambda and the kwd-lambda?
;; if basic-unsafe-lambda is #f, returns only the one value,
;; namely the chaperone wrapper. Otherwise, returns two values,
;; a procedure and a boolean indicating it the procedure is the
;; basic-unsafe-lambda or not; note that basic-unsafe-lambda might
;; also be #t, but that happens only when we know that basic-lambda
;; can't be chosen (because there are keywords involved)
(define (arity-checking-wrapper val blame neg-party blame+neg-party basic-lambda basic-lambda/no-tail
                                basic-unsafe-lambda
                                basic-unsafe-lambda/result-values-assumed
                                basic-unsafe-lambda/result-values-assumed/no-tail
                                contract-result-val-count
                                kwd-lambda kwd-lambda/no-tail
                                min-arity max-arity
                                req-kwd opt-kwd
                                method?
                                is-impersonator?)
  ;; should not build this unless we are in the 'else' case (and maybe not at all)
  (cond
    [(arrow:matches-arity-exactly? val min-arity max-arity req-kwd opt-kwd)
     (if (and (null? req-kwd) (null? opt-kwd))
         (cond
           [is-impersonator?
            (if basic-unsafe-lambda
                (values basic-lambda/no-tail #f)
                basic-lambda/no-tail)]
           [(impersonator? val)
            (if basic-unsafe-lambda
                (values basic-lambda #f)
                basic-lambda)]
           [(and basic-unsafe-lambda
                 basic-unsafe-lambda/result-values-assumed
                 (equal? contract-result-val-count
                         (procedure-result-arity val)))
            (if (simple-enough? val)
                (values basic-unsafe-lambda/result-values-assumed/no-tail #t)
                (values basic-unsafe-lambda/result-values-assumed #t))]
           [basic-unsafe-lambda
            (values basic-unsafe-lambda #t)]
           [else basic-lambda])
         (cond
           [is-impersonator?
            (if basic-unsafe-lambda
                (values kwd-lambda/no-tail #f)
                kwd-lambda/no-tail)]
           [else
            (if basic-unsafe-lambda
                (values kwd-lambda #f)
                kwd-lambda)]))]
    [else
     (define-values (vr va) (procedure-keywords val))
     (define all-kwds (append req-kwd opt-kwd))
     (define (valid-number-of-args? args)
       (if max-arity
           (<= min-arity (length args) max-arity)
           (<= min-arity (length args))))
     (define kwd-checker
       (if (and (null? req-kwd) (null? opt-kwd))
           (λ (kwds kwd-args . args)
             (arrow:raise-no-keywords-arg blame #:missing-party neg-party val kwds))
           (λ (kwds kwd-args . args)
             (with-contract-continuation-mark
              blame+neg-party
              (let ()
             (define args-len (length args))
             (unless (valid-number-of-args? args)
               (arrow:raise-wrong-number-of-args-error
                blame #:missing-party neg-party val
                args-len min-arity max-arity method?))

             ;; these two for loops are doing O(n^2) work that could be linear
             ;; (since the keyword lists are sorted)
             (for ([req-kwd (in-list req-kwd)])
               (unless (memq req-kwd kwds)
                 (raise-blame-error (blame-swap blame) #:missing-party neg-party
                                    val
                                    '(expected "keyword argument ~a")
                                    req-kwd)))
             (for ([k (in-list kwds)])
               (unless (memq k all-kwds)
                 (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                                    '(received: "unexpected keyword argument ~a")
                                    k)))
             (keyword-apply kwd-lambda kwds kwd-args args))))))
     (define basic-checker-name
       (if (null? req-kwd)
           (λ args
             (with-contract-continuation-mark
              blame+neg-party
              (let ()
             (unless (valid-number-of-args? args)
               (define args-len (length args))
               (arrow:raise-wrong-number-of-args-error
                blame #:missing-party neg-party val
                args-len min-arity max-arity method?))
             (if is-impersonator?
                 (apply basic-lambda/no-tail args)
                 (apply basic-lambda args)))))
           (λ args
             (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                                "expected required keyword ~a"
                                (car req-kwd)))))
     (define proc
       (if (or (not va) (pair? vr) (pair? va))
           (make-keyword-procedure kwd-checker basic-checker-name)
           basic-checker-name))
     (if basic-unsafe-lambda
         (values proc #f)
         proc)]))

(define (simple-enough? f)
  (or (struct-accessor-procedure? f)
      (struct-constructor-procedure? f)
      (struct-predicate-procedure? f)
      (struct-mutator-procedure? f)))

(define (maybe-cons-kwd c x r neg-party)
  (if (eq? arrow:unspecified-dom x)
      r
      (cons (c x neg-party) r)))

(define (->-proj is-impersonator? ctc
                 ;; fields of the 'ctc' struct
                 min-arity doms kwd-infos rest pre? rngs post?
                 plus-one-arity-function chaperone-constructor method?
                 late-neg?)
  (define has-c-c-support?
    (->-contract-has-collapsible-support? ctc))
  (define chaperone? (not is-impersonator?))
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

    ;; if the ctc supports c-c mode, there are only positional args
    (define-values (partial-doms c-c-doms)
      (for/lists (projs ses)
                 ([dom (in-list doms)]
                  [n (in-naturals 1)])
        (define dom-blame
          (blame-add-context orig-blame
                             (nth-argument-of (if method? (sub1 n) n))
                             #:swap? #t))
        (define prepared (get/build-collapsible-late-neg-projection dom))
        (prepared dom-blame)))

    (define rest-blame
      (if (ellipsis-rest-arg-ctc? rest)
          (blame-swap orig-blame)
          (blame-add-context orig-blame "the rest argument of"
                             #:swap? #t)))
    (define partial-rest (and rest
                              ((get/build-late-neg-projection rest)
                               rest-blame)))
    (define-values (partial-ranges maybe-c-c-ranges)
      (cond
        [rngs
         (for/lists (proj c-c)
                    ([rng (in-list rngs)])
           (define prepared (get/build-collapsible-late-neg-projection rng))
           (prepared rng-blame))]
        [else (values '() #f)]))
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
    (define c-c-mergable
      (and has-c-c-support?
           (build-collapsible-arrow (car maybe-c-c-ranges) c-c-doms ctc orig-blame chaperone?)))
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
      (define old-c-c-prop (get-impersonator-prop:collapsible val #f))
      (define safe-for-c-c?
        (and has-c-c-support?
             (if old-c-c-prop
                 (and (collapsible-property? old-c-c-prop)
                      (eq? (collapsible-property-ref old-c-c-prop) val))
                 (val-has-arrow-collapsible-support? val))))
      (define wrapper-count
        (if (collapsible-count-property? old-c-c-prop)
            (collapsible-count-property-count old-c-c-prop)
            0))
      (define-values (chap/imp-func use-unsafe-chaperone-procedure?)
        (apply chaperone-constructor
               orig-blame val
               neg-party blame-party-info
               is-impersonator? rngs the-args))
      (define chaperone-or-impersonate-procedure
        (if use-unsafe-chaperone-procedure?
            (if is-impersonator? unsafe-impersonate-procedure unsafe-chaperone-procedure)
            (if is-impersonator? impersonate-procedure chaperone-procedure)))
      (cond
        [(not chap/imp-func)
         val]
        [(not safe-for-c-c?)
         (if (or post? (not rngs))
             (chaperone-or-impersonate-procedure
              val
              chap/imp-func
              impersonator-prop:contracted ctc
              impersonator-prop:blame (cons orig-blame neg-party))
             (chaperone-or-impersonate-procedure
              val
              chap/imp-func
              impersonator-prop:contracted ctc
              impersonator-prop:blame (cons orig-blame neg-party)
              impersonator-prop:application-mark
              (cons arrow:tail-contract-key (list* neg-party blame-party-info rngs))))]
        [(wrapper-count . >= . COLLAPSIBLE-LIMIT)
         (arrow-enter-collapsible-mode/collapse
          c-c-mergable
          val
          neg-party
          old-c-c-prop
          chaperone?)]
        [(collapsible-wrapper-property? old-c-c-prop)
         (arrow-enter-collapsible-mode/continue
          c-c-mergable
          val
          neg-party
          (collapsible-property-c-c old-c-c-prop)
          (collapsible-property-neg-party old-c-c-prop)
          (collapsible-wrapper-property-checking-wrapper old-c-c-prop)
          chaperone?)]
        [else
         (define c-c-prop
           (collapsible-count-property
            c-c-mergable
            neg-party
            #f
            (add1 wrapper-count)
            (or old-c-c-prop val)))
         (define wrapped
           (if (or post? (not rngs))
             (chaperone-or-impersonate-procedure
              val
              chap/imp-func
              impersonator-prop:collapsible c-c-prop)
             (chaperone-or-impersonate-procedure
              val
              chap/imp-func
              impersonator-prop:collapsible c-c-prop
              impersonator-prop:application-mark
              (cons arrow:tail-contract-key (list* neg-party blame-party-info rngs)))))
         (set-collapsible-property-ref! c-c-prop wrapped)
         wrapped]))
    (cond
      [late-neg?
       (define (arrow-higher-order:lnp val neg-party)
         (cond
           [(do-arity-checking orig-blame val doms rest min-arity kwd-infos method?)
            =>
            (λ (f)
              (f neg-party))]
           [else
            (successfully-got-the-right-kind-of-function val neg-party)]))
       (cond
         [okay-to-do-only-arity-check?
          (define lnp
            (λ (val neg-party)
              (cond
                [(arrow:procedure-arity-exactly/no-kwds val min-arity) val]
                [else (arrow-higher-order:lnp val neg-party)])))
          (values lnp (or c-c-mergable (build-collapsible-leaf lnp ctc orig-blame)))]
         [else
          (values
           arrow-higher-order:lnp
           (or c-c-mergable (build-collapsible-leaf arrow-higher-order:lnp ctc orig-blame)))])]
      [else
       (define (arrow-higher-order:vfp val)
         (define-values (normal-proc proc-with-no-result-checking expected-number-of-results)
           (apply plus-one-arity-function orig-blame val plus-one-constructor-args))
         (cond
           [(do-arity-checking orig-blame val doms rest min-arity kwd-infos method?)
            =>
            (λ (neg-party-acceptor)
              ;; probably don't need to include the wrapped-extra-arrow wrapper
              ;; here, but it is easier to reason about the contract-out invariant
              ;; with it here
              (wrapped-extra-arg-arrow neg-party-acceptor normal-proc))]
           [else
            (wrapped-extra-arg-arrow
             (λ (neg-party)
               (successfully-got-the-right-kind-of-function val neg-party))
             (if (equal? (procedure-result-arity val) expected-number-of-results)
                 proc-with-no-result-checking
                 normal-proc))]))
       (if okay-to-do-only-arity-check?
           (λ (val)
             (cond
               [(arrow:procedure-arity-exactly/no-kwds val min-arity)
                (define-values (normal-proc proc-with-no-result-checking expected-number-of-results)
                  (apply plus-one-arity-function orig-blame val plus-one-constructor-args))
                (wrapped-extra-arg-arrow 
                 (λ (neg-party) val)
                 normal-proc)]
               [else (arrow-higher-order:vfp val)]))
           arrow-higher-order:vfp)])))
