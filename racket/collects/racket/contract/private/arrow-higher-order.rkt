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
         (only-in racket/unsafe/ops
                  unsafe-chaperone-procedure
                  unsafe-impersonate-procedure))

(provide (for-syntax build-chaperone-constructor/real)
         procedure-arity-exactly/no-kwds
         ->-proj
         check-pre-cond
         check-post-cond
         pre-post/desc-result->string
         raise-wrong-number-of-args-error
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
    #`(λ (blame f neg-party blame-party-info rng-ctcs
                mandatory-dom-proj ...  
                optional-dom-proj ... 
                rest-proj ...
                mandatory-dom-kwd-proj ... 
                optional-dom-kwd-proj ... 
                rng-proj ...)
        (define blame+neg-party (cons blame neg-party))
        #,(create-chaperone
           #'blame #'neg-party #'blame+neg-party #'blame-party-info #'f #'rng-ctcs
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

(define-for-syntax (create-chaperone blame neg-party blame+neg-party blame-party-info
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
                [val val])
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
                            [basic-return
                             (let ([inner-stx-gen
                                    (if need-apply?
                                        (λ (s) #`(apply values #,@s 
                                                        dom-projd-args ... 
                                                        opt+rest-uses))
                                        (λ (s) #`(values 
                                                  #,@s 
                                                  dom-projd-args ...)))])
                               (if rngs
                                   (arrow:check-tail-contract rng-ctcs
                                                              blame-party-info
                                                              neg-party
                                                              (list rng-checker)
                                                              inner-stx-gen
                                                              #'(cons blame neg-party))
                                   (inner-stx-gen #'())))]
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
                                          (λ (x) (inner-stx-gen x assume-result-values? do-tail-check?))
                                          #'(cons blame neg-party))
                                         (inner-stx-gen #'() assume-result-values? do-tail-check?))
                                     (inner-stx-gen #'not-a-null assume-result-values? do-tail-check?)))
                               (list (mk-return #f #t) (mk-return #t #t) (mk-return #t #f)))]
                            [kwd-return
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
                               #`(let ([kwd-results kwd-stx])
                                   #,(if rngs
                                         (arrow:check-tail-contract rng-ctcs
                                                                    blame-party-info
                                                                    neg-party
                                                                    (list rng-checker)
                                                                    outer-stx-gen
                                                                    #'(cons blame neg-party))
                                         (outer-stx-gen #'()))))])

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
                                                 pre ... kwd-return)))])
                  (cond
                   [(and (null? req-keywords) (null? opt-keywords))
                    #`(arity-checking-wrapper val
                                              blame neg-party blame+neg-party
                                              basic-lambda
                                              basic-unsafe-lambda
                                              basic-unsafe-lambda/result-values-assumed
                                              basic-unsafe-lambda/result-values-assumed/no-tail
                                              #,(and rngs (length rngs))
                                              void
                                              #,min-arity
                                              #,(if dom-rest #f max-arity)
                                              '(req-kwd ...)
                                              '(opt-kwd ...)
                                              #,method?)]
                   [(pair? req-keywords)
                    #`(arity-checking-wrapper val
                                              blame neg-party blame+neg-party
                                              void #t #f #f #f
                                              kwd-lambda
                                              #,min-arity
                                              #,(if dom-rest #f max-arity)
                                              '(req-kwd ...)
                                              '(opt-kwd ...)
                                              #,method?)]
                   [else
                    #`(arity-checking-wrapper val
                                              blame neg-party blame+neg-party
                                              basic-lambda #t #f #f #f
                                              kwd-lambda
                                              #,min-arity
                                              #,(if dom-rest #f max-arity)
                                              '(req-kwd ...)
                                              '(opt-kwd ...)
                                              #,method?)])))))))))

;; should we pass both the basic-lambda and the kwd-lambda?
;; if basic-unsafe-lambda is #f, returns only the one value,
;; namely the chaperone wrapper. Otherwise, returns two values,
;; a procedure and a boolean indicating it the procedure is the
;; basic-unsafe-lambda or not; note that basic-unsafe-lambda might
;; also be #t, but that happens only when we know that basic-lambda
;; can't be chosen (because there are keywords involved)
(define (arity-checking-wrapper val blame neg-party blame+neg-party basic-lambda
                                basic-unsafe-lambda
                                basic-unsafe-lambda/result-values-assumed
                                basic-unsafe-lambda/result-values-assumed/no-tail
                                contract-result-val-count
                                kwd-lambda
                                min-arity max-arity
                                req-kwd opt-kwd
                                method?)
  ;; should not build this unless we are in the 'else' case (and maybe not at all)
  (cond
    [(arrow:matches-arity-exactly? val min-arity max-arity req-kwd opt-kwd)
     (if (and (null? req-kwd) (null? opt-kwd))
         (cond
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
         (if basic-unsafe-lambda
             (values kwd-lambda #f)
             kwd-lambda))]
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
               (raise-wrong-number-of-args-error
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
               (raise-wrong-number-of-args-error
                blame #:missing-party neg-party val
                args-len min-arity max-arity method?))
             (apply basic-lambda args))))
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

(define (raise-wrong-number-of-args-error
         blame #:missing-party [missing-party #f] val
         args-len pre-min-arity pre-max-arity method?)
  (define min-arity ((if method? sub1 values) pre-min-arity))
  (define max-arity ((if method? sub1 values) pre-max-arity))
  (define arity-string
    (if max-arity
        (cond
          [(= min-arity max-arity)
           (format "~a non-keyword argument~a" min-arity (if (= min-arity 1) "" "s"))]
          [(= (+ min-arity 1) max-arity)
           (format "~a or ~a non-keyword arguments" min-arity max-arity)]
          [else
           (format "~a to ~a non-keyword arguments" min-arity max-arity)])
        (format "at least ~a non-keyword argument~a" min-arity (if (= min-arity 1) "" "s"))))
  (raise-blame-error (blame-swap blame) val
                     #:missing-party missing-party
                     '(received: "~a argument~a" expected: "~a")
                     args-len (if (= args-len 1) "" "s") arity-string))

(define (maybe-cons-kwd c x r neg-party)
  (if (eq? arrow:unspecified-dom x)
      r
      (cons (c x neg-party) r)))

(define (->-proj chaperone? ctc
                 ;; fields of the 'ctc' struct
                 min-arity doms kwd-infos rest pre? rngs post?
                 plus-one-arity-function chaperone-constructor method?
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
                            (format "the ~a argument of" (n->th (if method? (sub1 n) n)))
                            #:swap? #t))))
    (define rest-blame
      (if (ellipsis-rest-arg-ctc? rest)
          (blame-swap orig-blame)
          (blame-add-context orig-blame "the rest argument of"
                             #:swap? #t)))
    (define partial-rest (and rest
                              ((get/build-late-neg-projection rest)
                               rest-blame)))
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
      (define-values (chap/imp-func use-unsafe-chaperone-procedure?)
        (apply chaperone-constructor
               orig-blame val
               neg-party blame-party-info
               rngs the-args))
      (define chaperone-or-impersonate-procedure
        (if use-unsafe-chaperone-procedure?
            (if chaperone? unsafe-chaperone-procedure unsafe-impersonate-procedure)
            (if chaperone? chaperone-procedure impersonate-procedure)))
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
           [(do-arity-checking orig-blame val doms rest min-arity kwd-infos method?)
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
               [(procedure-arity-exactly/no-kwds val min-arity)
                (define-values (normal-proc proc-with-no-result-checking expected-number-of-results)
                  (apply plus-one-arity-function orig-blame val plus-one-constructor-args))
                (wrapped-extra-arg-arrow 
                 (λ (neg-party) val)
                 normal-proc)]
               [else (arrow-higher-order:vfp val)]))
           arrow-higher-order:vfp)])))

(define (procedure-arity-exactly/no-kwds val min-arity)
  (and (procedure? val)
       (equal? (procedure-arity val) min-arity)
       (let-values ([(man opt) (procedure-keywords val)])
         (and (null? man)
              (null? opt)))))
