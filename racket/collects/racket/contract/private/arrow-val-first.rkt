#lang racket/base
(require (for-syntax racket/base
                     "arr-util.rkt")
         "blame.rkt"
         "misc.rkt"
         "prop.rkt"
         "guts.rkt"
         racket/stxparam
         (prefix-in arrow: "arrow.rkt"))

(provide ->2 ->*2
         (for-syntax ->2-handled?
                     ->*2-handled?)
         arity-as-string
         raw-arity-as-string)

(define-for-syntax (->2-handled? stx)
  (syntax-case stx (any values any/c)
    [(_ args ...)
     (syntax-parameter-value #'arrow:making-a-method)
     #f]
    [(_ any/c ... any)
     ;; should turn into a flat contract
     #f]
    [_ #t]))

(define-for-syntax (->*2-handled? stx)
  (syntax-case stx (any values any/c)
    [(_ args ...)
     (syntax-parameter-value #'arrow:making-a-method)
     #f]
    [_ #t]))

(define-for-syntax popular-keys
  '((0 0 () () #t 1)
    
    (2 0 () () #f #f)
    (1 0 () () #f #f)
    
    (3 0 () () #f 1) 
    (2 0 () () #f 1)
    (1 0 () () #f 1)
    (0 0 () () #f 1)))

(define-syntax (generate-popular-key-ids stx)
  #`(define-for-syntax #,(datum->syntax stx 'popular-key-ids)
      (list #,@(map (λ (x) #`(quote-syntax #,x))
                    (generate-temporaries (for/list ([e (in-list popular-keys)])
                                            'popular-key-id))))))
(generate-popular-key-ids)

(define-for-syntax (build-plus-one-arity-function
                    stx
                    regular-args
                    optional-args
                    mandatory-kwds
                    optional-kwds
                    pre
                    rest
                    rngs
                    post)
  (define key (and (not pre)
                   (not post)
                   (list (length regular-args)
                         (length optional-args)
                         (map syntax-e mandatory-kwds)
                         (map syntax-e optional-kwds)
                         (and rest #t)
                         (and rngs (if (syntax? rngs)
                                       (length (syntax->list rngs))
                                       (length rngs))))))
  (cond
    [(and key (member key popular-keys))
     =>
     (λ (l)
       (define index (- (length popular-keys) (length l)))
       (list-ref popular-key-ids index))]
    [else
     (build-plus-one-arity-function/real
      regular-args
      optional-args
      mandatory-kwds
      optional-kwds
      pre
      rest
      rngs
      post)]))

(define-syntax (build-populars stx)
  #`(begin
      #,@(for/list ([id (in-list popular-key-ids)]
                    [key (in-list popular-keys)])
           (define-values (regular-arg-count
                           optional-arg-count
                           mandatory-kwds
                           optional-kwds
                           rest
                           rngs)
             (apply values key))
           #`(define #,(syntax-local-introduce id)
               #,(build-plus-one-arity-function/real
                  (for/list ([x (in-range regular-arg-count)])
                    (string->symbol (format "man~a" x)))
                  (for/list ([x (in-range optional-arg-count)])
                    (string->symbol (format "opt~a" x)))
                  mandatory-kwds
                  optional-kwds
                  #f
                  (and rest)
                  (and rngs (for/list ([x (in-range rngs)])
                              (string->symbol (format "rng~a" x))))
                  #f)))))

(define-for-syntax (build-plus-one-arity-function/real
                    regular-args
                    optional-args
                    mandatory-kwds
                    optional-kwds
                    pre
                    rest
                    rngs
                    post)
  (with-syntax ([(regb ...) (generate-temporaries regular-args)]
                [(optb ...) (generate-temporaries optional-args)]
                [(kb ...) (generate-temporaries mandatory-kwds)]
                [(okb ...) (generate-temporaries optional-kwds)]
                [(rb ...) (generate-temporaries (or rngs '()))]
                [(arg-x ...) (generate-temporaries regular-args)]
                [(res-x ...) (generate-temporaries (or rngs '()))]
                [(kwd-arg-x ...) (generate-temporaries mandatory-kwds)])
    
    (with-syntax ([(formal-kwd-args ...)
                   (apply append (map list mandatory-kwds (syntax->list #'(kwd-arg-x ...))))]
                  [(kwd-arg-exps ...)
                   (apply append (map (λ (kwd kwd-arg-x kb) 
                                        (list kwd #`((#,kb #,kwd-arg-x) neg-party)))
                                      mandatory-kwds
                                      (syntax->list #'(kwd-arg-x ...))
                                      (syntax->list #'(kb ...))))]
                  [(letrec-bound-id) (generate-temporaries '(f))])
      
      (with-syntax ([(wrapper-args ...) #'(neg-party arg-x ... formal-kwd-args ...)]
                    [(the-call ...) #'(f ((regb arg-x) neg-party) ... kwd-arg-exps ...)]
                    [(pre-check ...)
                     (if pre 
                         (list #`(check-pre-condition blame neg-party f #,pre))
                         (list))]
                    [(post-check ...)
                     (if post
                         (list #`(check-post-condition blame neg-party f #,post))
                         (list))]
                    [(restb) (generate-temporaries '(rest-args))])
        (define body-proc
          (cond
            [(or (and (null? optional-args)
                      (null? optional-kwds))
                 (and (null? mandatory-kwds)
                      (null? optional-kwds)))
             (define case-lambda-clauses
               (let loop ([optional-args (reverse optional-args)]
                          [ob (reverse (syntax->list #'(optb ...)))]
                          [first? #t])
                 (define no-rest-call
                   #`(the-call ... #,@(for/list ([ob (in-list (reverse ob))]
                                                 [optional-arg (in-list (reverse optional-args))])
                                        #`((#,ob #,optional-arg) neg-party))))
                 (define full-call
                   (if (and first? rest)
                       #`(apply #,@no-rest-call ((restb rest-arg) neg-party))
                       no-rest-call))
                 (define the-args #`(wrapper-args ... 
                                     #,@(reverse optional-args)
                                     #,@(if (and first? rest)
                                            #'rest-arg
                                            '())))
                 (define the-clause
                   (if rngs
                       #`[#,the-args
                          pre-check ...
                          (define-values (failed res-x ...)
                            (call-with-values
                             (λ () #,full-call)
                             (case-lambda
                               [(res-x ...)
                                (values #f res-x ...)]
                               [args
                                (values args #,@(map (λ (x) #'#f) 
                                                     (syntax->list #'(res-x ...))))])))
                          (cond
                            [failed
                             (wrong-number-of-results-blame 
                              blame neg-party f
                              failed
                              #,(length 
                                 (syntax->list
                                  #'(res-x ...))))]
                            [else
                             post-check ...
                             (values ((rb res-x) neg-party) ...)])]
                       #`[#,the-args
                          pre-check ...
                          #,full-call]))
                 (cons the-clause
                       (cond
                         [(null? optional-args) '()]
                         [else (loop (cdr optional-args)
                                     (cdr ob)
                                     #f)]))))
             
             (cond
               [(null? (cdr case-lambda-clauses))
                ;; need to specialize this case because
                ;; there might be keyword arguments here
                #`(λ #,@(car case-lambda-clauses))]
               [else
                ;; (but there won't here)
                #`(case-lambda #,@case-lambda-clauses)])]
            [else
             #`(make-checking-proc f blame
                                   '(#,@mandatory-kwds) (list kb ...)
                                   '(#,@optional-kwds) (list okb ...) 
                                   #,(length regular-args) (list regb ... optb ...) 
                                   #,(if rest #'restb #'#f))]))
        #`(λ (blame f regb ... optb ... kb ... okb ... rb ... #,@(if rest (list #'restb) '()))
            #,body-proc)))))

(build-populars)

(define (make-checking-proc f blame  
                            original-mandatory-kwds kbs
                            original-optional-kwds okbs
                            minimum-arg-count rbs rest-ctc)
  (make-keyword-procedure
   (λ (actual-kwds actual-kwd-args neg-party . regular-args)
     (check-arg-count minimum-arg-count rbs regular-args f blame neg-party rest-ctc)
     (check-keywords original-mandatory-kwds original-optional-kwds actual-kwds f blame neg-party)
     (keyword-apply
      f
      actual-kwds
      (let loop ([kwds actual-kwds]
                 [kwd-args actual-kwd-args]
                 [mandatory-kwds original-mandatory-kwds]
                 [optional-kwds original-optional-kwds]
                 [kbs kbs]
                 [okbs okbs])
        (cond
          [(null? kwd-args) '()]
          [else
           (define kwd (car kwds))
           (define kwd-arg (car kwd-args))
           (cond
             [(and (pair? mandatory-kwds)
                   (equal? (car mandatory-kwds) kwd))
              (cons (((car kbs) kwd-arg) neg-party)
                    (loop (cdr kwds) 
                          (cdr kwd-args)
                          (cdr mandatory-kwds)
                          optional-kwds
                          (cdr kbs)
                          okbs))]
             [(and (pair? optional-kwds)
                   (equal? (car optional-kwds) kwd))
              (cons (((car okbs) kwd-arg) neg-party)
                    (loop (cdr kwds) 
                          (cdr kwd-args)
                          mandatory-kwds
                          (cdr optional-kwds)
                          kbs
                          (cdr okbs)))]
             [(pair? optional-kwds)
              (loop kwds kwd-args mandatory-kwds (cdr optional-kwds) kbs (cdr okbs))]
             [else
              (error 'arrow-val-first.rkt
                     (string-append
                      "internal error:\n  f ~s\n  actual-kwds ~s"
                      "\n  mandatory-kwds ~s\n  optional-kwds ~s\n  neg-party ~s")
                     f actual-kwds original-mandatory-kwds original-optional-kwds neg-party)])]))
      (let loop ([regular-args regular-args]
                 [rbs rbs])
        (cond
          [(null? regular-args) '()]
          [(null? rbs) ((rest-ctc regular-args) neg-party)]
          [else
           (cons (((car rbs) (car regular-args)) neg-party)
                 (loop (cdr regular-args) (cdr rbs)))]))))))

(define (check-arg-count minimum-arg-count rbs regular-args val blame neg-party rest-ctc)
  (define actual-count (length regular-args))
  (cond
    [(< actual-count minimum-arg-count)
     (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                        '(expected: "~a~a arguments")
                        (if (= (length rbs) minimum-arg-count)
                            ""
                            "at least ")
                        minimum-arg-count)]
    [(and (not rest-ctc) (< (length rbs) actual-count))
     (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                        '(expected: "~a~a arguments")
                        (if (= (length rbs) minimum-arg-count)
                            ""
                            "at most ")
                        (+ minimum-arg-count (length rbs)))]))

(define (check-keywords mandatory-kwds optional-kwds kwds val blame neg-party)
  (let loop ([mandatory-kwds mandatory-kwds]
             [optional-kwds optional-kwds]
             [kwds kwds])
    (cond
      [(null? kwds) 
       (unless (null? mandatory-kwds)
         (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                            '(expected: "keyword argument ~a")
                            (car mandatory-kwds)))]
      [else
       (define kwd (car kwds))
       (cond
         [(and (null? optional-kwds) (null? mandatory-kwds))
          (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                             '(expected: "no keyword argument ~a")
                             kwd)]
         [(and (pair? mandatory-kwds)
               (or (null? optional-kwds)
                   (keyword<? (car mandatory-kwds) (car optional-kwds))))
          (define man-kwd (car mandatory-kwds))
          (cond
            [(equal? kwd man-kwd)
             (loop (cdr mandatory-kwds) optional-kwds (cdr kwds))]
            [(keyword<? kwd man-kwd) 
             (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                                '(expected: "no keyword argument ~a")
                                kwd)]
            [(keyword<? man-kwd kwd) 
             (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                                '(expected: "keyword argument ~a")
                                man-kwd)])]
         [(and (pair? optional-kwds)
               (or (null? mandatory-kwds)
                   (keyword<? (car optional-kwds) (car mandatory-kwds))))
          (define opt-kwd (car optional-kwds))
          (cond
            [(equal? kwd opt-kwd)
             (loop mandatory-kwds (cdr optional-kwds) (cdr kwds))]
            [(keyword<? kwd opt-kwd)
             (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                                '(expected: "no keyword argument ~a")
                                kwd)]
            [(keyword<? opt-kwd kwd)
             (loop mandatory-kwds (cdr optional-kwds) kwds)])])])))

                   
(define (check-pre-condition blame neg-party val thunk)
  (unless (thunk)
    (raise-blame-error
     (blame-swap blame) #:missing-party neg-party val
     "#:pre condition failure")))

(define (check-post-condition blame neg-party val thunk)
  (unless (thunk)
    (raise-blame-error
     blame #:missing-party neg-party val
     "#:post condition failure")))

(define-for-syntax (parse-arrow-args stx args this->)
  (let loop ([args args]
             [regular-args '()]
             [kwds '()]
             [kwd-args '()]
             [let-bindings '()])
    (cond
      [(null? args) 
       (define sorted
         (sort (map cons kwds kwd-args)
               keyword<?
               #:key (compose syntax-e car)))
       (values (reverse regular-args)
               (map car sorted)
               (map cdr sorted)
               let-bindings)]
      [else
       (with-syntax ([(arg-x) (generate-temporaries (list (car args)))])
         (syntax-case (car args) ()
           [kwd
            (keyword? (syntax-e #'kwd))
            (begin
              (when (null? (cdr args))
                (raise-syntax-error '-> 
                                    "expected a contract to follow the keyword (plus the range)"
                                    stx
                                    (car args)))
              (loop (cddr args)
                    regular-args
                    (cons (car args) kwds)
                    (cons #'arg-x kwd-args)
                    (cons #`[arg-x #,(syntax-property (cadr args) 
                                                      'racket/contract:negative-position 
                                                      this->)]
                          let-bindings)))]
           [else
            (loop (cdr args)
                  (cons #'arg-x regular-args)
                  kwds
                  kwd-args
                  (cons #`[arg-x #,(syntax-property (car args) 
                                                    'racket/contract:negative-position 
                                                    this->)]
                        let-bindings))]))])))

(define-syntax (->2 stx)
  (syntax-case stx ()
    [(_ args ...)
     (not (->2-handled? stx))
     #'(arrow:-> args ...)]
    [(_ args ... rng)
     (let ()
       (define this-> (gensym 'this->))
       (define-values (regular-args kwds kwd-args let-bindings)
         (parse-arrow-args stx (syntax->list #'(args ...)) this->))
       (define (add-pos-obligations stxes)
         (for/list ([stx (in-list stxes)])
           (syntax-property stx 'racket/contract:positive-position this->)))
       (define rngs
         (syntax-case #'rng (any values)
           [any #f]
           [(values rng ...) (add-pos-obligations (syntax->list #'(rng ...)))]
           [rng (add-pos-obligations (list #'rng))]))
       (syntax-property
        #`(let #,let-bindings
            #,(quasisyntax/loc stx
                (build--> '->
                      (list #,@regular-args) '()
                      '(#,@kwds)
                      (list #,@kwd-args)
                      '() '()
                      #f
                      #f
                      #,(if rngs
                            #`(list #,@rngs)
                            #'#f)
                      #f
                      #,(build-plus-one-arity-function stx regular-args '() kwds '() #f #f rngs #f))))
        'racket/contract:contract
        (vector this->
                ;; the -> in the original input to this guy
                (list (car (syntax-e stx)))
                '())))]))

;; not quite the same as split-doms in arr-util.rkt, but similar idea.
(define-for-syntax (:split-doms stx name raw-doms)
  (let loop ([raw-doms raw-doms]
             [doms '()]
             [kwd-doms '()]
             [let-bindings '()])
    (syntax-case raw-doms ()
      [() (list (reverse doms)
                (sort-keywords stx kwd-doms)
                (reverse let-bindings))]
      [(kwd arg . rest)
       (and (keyword? (syntax-e #'kwd))
            (not (keyword? (syntax-e #'arg))))
       (with-syntax ([(x) (generate-temporaries #'(kwd))])
         (loop #'rest
               doms
               (cons #'(kwd x) kwd-doms)
               (cons #`[x arg] let-bindings)))]
      [(kwd arg . rest)
       (and (keyword? (syntax-e #'kwd))
            (keyword? (syntax-e #'arg)))
       (raise-syntax-error name
                           "cannot have two keywords in a row"
                           stx
                           #'kwd)]
      [(kwd)
       (keyword? (syntax-e #'kwd))
       (raise-syntax-error name
                           "cannot have a keyword at the end"
                           stx
                           #'kwd)]
      [(x . rest)
       (with-syntax ([(t) (generate-temporaries #'(x))])
         (loop #'rest 
               (cons #'t doms) 
               kwd-doms
               (cons #`[t x] let-bindings)))])))

(define-syntax (->*2 stx)
  (cond
    [(->*2-handled? stx)
     (syntax-case stx ()
       [(_ (raw-mandatory-dom ...) . other)
        (let ()
          (define-values (raw-optional-doms rest-ctc pre rng-ctcs post)
            (arrow:parse-leftover->* stx #'other))
          (with-syntax ([((mandatory-dom ...) 
                          ((mandatory-dom-kwd mandatory-dom-kwd-ctc) ...)
                          (mandatory-let-bindings ...))
                         (:split-doms stx '->* #'(raw-mandatory-dom ...))]
                        [((optional-dom ...)
                          ((optional-dom-kwd optional-dom-kwd-ctc) ...)
                          (optional-let-bindings ...))
                         (:split-doms stx '->* raw-optional-doms)]
                        [(pre-x post-x) (generate-temporaries '(pre-cond post-cond))])
            (with-syntax ([((kwd dom opt?) ...) #'((mandatory-dom-kwd mandatory-dom-kwd-ctc #f) ...
                                                   (optional-dom-kwd optional-dom-kwd-ctc #t) ...)]
                          [(pre-let-binding ...) (if pre
                                                     (list #`[pre-x (λ () #,pre)])
                                                     (list))]
                          [(post-let-binding ...) (if post
                                                      (list #`[post-x (λ () #,post)])
                                                      (list))])
              #`(let (mandatory-let-bindings ...
                      optional-let-bindings ... 
                      pre-let-binding ...
                      post-let-binding ...)
                  (build--> '->*
                            (list mandatory-dom ...)
                            (list optional-dom ...)
                            '(mandatory-dom-kwd ...)
                            (list mandatory-dom-kwd-ctc ...)
                            '(optional-dom-kwd ...)
                            (list optional-dom-kwd-ctc ...)
                            #,rest-ctc
                            #,(and pre #'pre-x)
                            #,(if rng-ctcs
                                  #`(list #,@rng-ctcs)
                                  #'#f)
                            #,(and post #'post-x)
                            #,(build-plus-one-arity-function
                               stx
                               (syntax->list #'(mandatory-dom ...))
                               (syntax->list #'(optional-dom ...))
                               (syntax->list #'(mandatory-dom-kwd ...))
                               (syntax->list #'(optional-dom-kwd ...))
                               (and pre #'pre-x)
                               rest-ctc
                               rng-ctcs
                               (and post #'post-x)))))))])]
    [else
     (syntax-case stx ()
       [(_ args ...)
        #'(arrow:->* args ...)])]))

(define ((mk-val-first-proj chaperone-or-impersonate-procedure) ->stct)
  (λ (blame)
    (define dbs (for/list ([v (in-list (base->-doms ->stct))]
                           [i (in-naturals 1)])
                  (define dom-proj (get/build-val-first-projection v))
                  (dom-proj
                   (blame-add-context blame
                                      (format "the ~a argument of"
                                              (n->th i))
                                      #:swap? #t))))
    
    (define kwd-dbs 
      (for/list ([kwd-info (in-list (base->-kwd-infos ->stct))])
        ((get/build-val-first-projection (kwd-info-ctc kwd-info))
         (blame-add-context blame
                            (format "the ~a argument of" (kwd-info-kwd kwd-info))
                            #:swap? #t))))
        
    (define opt-kwd-dbs (for/list ([kwd-db (in-list kwd-dbs)]
                                   [kwd-info (in-list (base->-kwd-infos ->stct))]
                                   #:unless (kwd-info-mandatory? kwd-info))
                          kwd-db))
    (define mandatory-kwd-dbs (for/list ([kwd-db (in-list kwd-dbs)]
                                         [kwd-info (in-list (base->-kwd-infos ->stct))]
                                         #:when (kwd-info-mandatory? kwd-info))
                                kwd-db))
    
    (define rst-b (and (base->-rest ->stct)
                       ((get/build-val-first-projection (base->-rest ->stct))
                        (blame-add-context blame 
                                           "the rest argument of"
                                           #:swap? #t))))
    
    (define just-one? (and (base->-rngs ->stct) (= 1 (length (base->-rngs ->stct)))))
    (define range-blame (blame-add-context blame "the range of"))
    (define rbs (for/list ([v (in-list (or (base->-rngs ->stct) '()))]
                           [i (in-naturals 1)])
                  ((get/build-val-first-projection v)
                   range-blame)))
    (define tail-mark-vals rbs)
    (define max-arity (if (base->-rest ->stct)
                          +inf.0
                          (length dbs)))
    (define min-arity (base->-min-arity ->stct))
    
    (define expected-values (length rbs))
    (λ (val)
      (define arity-checking (do-arity-checking blame val ->stct))
      (cond
        [arity-checking
         arity-checking]
        [else
         (define ctc-f-with-extra-neg-party-arg
           (apply (base->-proc ->stct) blame val 
                  (append dbs mandatory-kwd-dbs opt-kwd-dbs rbs (if rst-b (list rst-b) '()))))
         (wrapped-extra-arg-arrow
          (λ (neg-party)
            (chaperone-or-impersonate-procedure
             val
             (make-keyword-procedure
              (λ (supplied-kwds kwd-vals . args)
                (call-with-immediate-continuation-mark
                 arrow:contract-key
                 (λ (existing-tail-marks)
                   (unless (<= min-arity (length args) max-arity)
                     (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                                        '("received ~a argument~a" expected: "~a")
                                        (length args)
                                        (if (= 1 (length args)) "" "s")
                                        (cond
                                          [(= min-arity max-arity)
                                           (format "~a argument~a"
                                                   max-arity
                                                   (if (= 1 max-arity) "" "s"))]
                                          [else
                                           (format "between ~a and ~a arguments"
                                                   min-arity max-arity)])))
                   (define chap-regular-args
                     (let loop ([args args]
                                [dbs dbs])
                       (cond
                         [(null? dbs)
                          ;; out of contracts for individual args; switch to #:rest arg
                          (if rst-b
                              ((rst-b args) neg-party)
                              '())]
                         [(null? args)
                          ;; out of arguments; remaining dbs must be optional
                          '()]
                         [else
                          (cons (((car dbs) (car args)) neg-party)
                                (loop (cdr args) (cdr dbs)))])))
                   
                   (define (signal-missing-keyword-error kwd)
                     (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                                        '(expected: "keyword argument ~a")
                                        kwd))
                   (define (signal-extra-keyword-error kwd)
                     (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                                        '(expected: "no keyword argument ~a")
                                        kwd))
                   (define chap-keyword-args
                     (let loop ([supplied-kwds supplied-kwds]
                                [kwd-vals kwd-vals]
                                [kwd-dbs kwd-dbs]
                                [kwd-infos (base->-kwd-infos ->stct)])
                       (cond
                         [(and (null? supplied-kwds) (null? kwd-infos)) '()]
                         [(null? supplied-kwds)
                          (for ([kwd-info (in-list kwd-infos)])
                            (when (kwd-info-mandatory? kwd-info)
                              (signal-missing-keyword-error (kwd-info-kwd kwd-info))))
                          '()]
                         [(null? kwd-infos)
                          (signal-extra-keyword-error (car supplied-kwds))]
                         [else
                          (cond
                            [(equal? (kwd-info-kwd (car kwd-infos)) (car supplied-kwds))
                             (cons (((car kwd-dbs) (car kwd-vals)) neg-party)
                                   (loop (cdr supplied-kwds)
                                         (cdr kwd-vals)
                                         (cdr kwd-dbs)
                                         (cdr kwd-infos)))]
                            [(kwd-info-mandatory? (car kwd-infos))
                             (signal-missing-keyword-error (car supplied-kwds))]
                            [else
                             (loop supplied-kwds kwd-vals (cdr kwd-dbs) (cdr kwd-infos))])])))
                   
                   (when (base->-pre ->stct)
                     (check-pre-condition blame neg-party val (base->-pre ->stct)))
                   
                   (define chap-args 
                     (if (null? supplied-kwds)
                         chap-regular-args
                         (cons chap-keyword-args chap-regular-args)))
                   
                   (define chap-res
                     (if (and (base->-rngs ->stct)
                              (not (apply arrow:tail-marks-match? existing-tail-marks rbs)))
                         (list* (λ reses
                                  (define length-reses (length reses))
                                  (unless (= length-reses expected-values)
                                    (wrong-number-of-results-blame 
                                     blame neg-party val 
                                     reses expected-values))
                                  (define results
                                   (for/list ([res (in-list reses)]
                                              [rng-b (in-list rbs)])
                                     ((rng-b res) neg-party)))
                                  (when (base->-post ->stct)
                                    (check-post-condition blame neg-party val (base->-post ->stct)))
                                  (apply values results))
                                chap-args)
                         chap-args))
                   (apply values chap-res)))))
             impersonator-prop:contracted ->stct
             impersonator-prop:application-mark (cons arrow:contract-key tail-mark-vals)))
          ctc-f-with-extra-neg-party-arg)]))))

(define (wrong-number-of-results-blame blame neg-party val reses expected-values)
  (define length-reses (length reses))
  (raise-blame-error 
   blame #:missing-party neg-party val
   '("received ~a value~a" expected: "~a value~a")
   length-reses
   (if (= 1 length-reses) "" "s")
   expected-values
   (if (= 1 expected-values) "" "s")))

(define (do-arity-checking blame val ->stct)
  (let/ec k
    (unless (procedure? val)
      (maybe-err 
       k blame
       (λ (neg-party)
         (raise-blame-error blame #:missing-party neg-party val
                            '(expected: "a procedure" given: "~e")
                            val))))
     (define-values (actual-mandatory-kwds actual-optional-kwds) (procedure-keywords val))
     (define arity (if (list? (procedure-arity val))
                       (procedure-arity val)
                       (list (procedure-arity val))))
     (define expected-number-of-non-keyword-args (length (base->-doms ->stct)))
     (define matching-arity?
       (and (for/or ([a (in-list arity)])
              (or (equal? expected-number-of-non-keyword-args a)
                  (and (arity-at-least? a)
                       (>= expected-number-of-non-keyword-args (arity-at-least-value a)))))
            (if (base->-rest ->stct)
                (let ([lst (car (reverse arity))])
                  (and (arity-at-least? lst)
                       (<= (arity-at-least-value lst) (base->-min-arity ->stct))))
                #t)))
     (unless matching-arity?
       (maybe-err
        k blame
        (λ (neg-party)
          (raise-blame-error blame #:missing-party neg-party val
                             '(expected: 
                               "a procedure that accepts ~a non-keyword argument~a~a"
                               given: "~e"
                               "\n  ~a")
                             expected-number-of-non-keyword-args
                             (if (= expected-number-of-non-keyword-args 1) "" "s")
                             (if (base->-rest ->stct)
                                 " and arbitrarily many more"
                                 "")
                             val
                             (arity-as-string val)))))
    
    (define (should-have-supplied kwd)
      (maybe-err
       k blame
       (λ (neg-party)
         (raise-blame-error blame #:missing-party neg-party val
                            '(expected: 
                              "a procedure that accepts the ~a keyword argument"
                              given: "~e"
                              "\n  ~a")
                            kwd
                            val
                            (arity-as-string val)))))
    
    (define (should-not-have-supplied kwd)
      (maybe-err
       k blame
       (λ (neg-party)
         (raise-blame-error blame #:missing-party neg-party val
                            '(expected: 
                              "a procedure that does not require the ~a keyword argument"
                              given: "~e"
                              "\n  ~a")
                            kwd
                            val
                            (arity-as-string val)))))
    
    (when actual-optional-kwds ;; when all kwds are okay, no checking required
      (let loop ([mandatory-kwds actual-mandatory-kwds]
                 [all-kwds actual-optional-kwds]
                 [kwd-infos (base->-kwd-infos ->stct)])
        (cond
          [(null? kwd-infos)
           (unless (null? mandatory-kwds)
             (should-not-have-supplied (car mandatory-kwds)))]
          [else
           (define kwd-info (car kwd-infos))
           (define-values (mandatory? kwd new-mandatory-kwds new-all-kwds)
             (cond
               [(null? all-kwds)
                (should-have-supplied (kwd-info-kwd kwd-info))]
               [else
                (define mandatory? 
                  (and (pair? mandatory-kwds)
                       (equal? (car mandatory-kwds) (car all-kwds))))
                (values mandatory? 
                        (car all-kwds)
                        (if mandatory?
                            (cdr mandatory-kwds)
                            mandatory-kwds)
                        (cdr all-kwds))]))
           (cond
             [(equal? kwd (kwd-info-kwd kwd-info))
              (when (and (not (kwd-info-mandatory? kwd-info))
                         mandatory?)
                (maybe-err
                 k blame
                 (λ (neg-party)
                   (raise-blame-error 
                    blame #:missing-party neg-party val
                    '(expected:
                      "a procedure that optionally accepts the keyword ~a (this one is mandatory)"
                      given: "~e"
                      "\n  ~a")
                    val
                    kwd
                    (arity-as-string val)))))
              (loop new-mandatory-kwds new-all-kwds (cdr kwd-infos))]
             [(keyword<? kwd (kwd-info-kwd kwd-info))
              (when mandatory?
                (should-not-have-supplied kwd))
              (loop new-mandatory-kwds new-all-kwds kwd-infos)]
             [else
              (loop new-mandatory-kwds new-all-kwds kwd-infos)])])))
    
    #f))


(define (arity-as-string v)
  (define prefix (if (object-name v)
                     (format "~a accepts: " (object-name v))
                     (format "accepts: ")))
  (string-append prefix (raw-arity-as-string v)))

(define (raw-arity-as-string v)
  (define ar (procedure-arity v))
  (define (plural n) (if (= n 1) "" "s"))
  (define-values (man-kwds all-kwds) (procedure-keywords v))
  (define opt-kwds (if all-kwds (remove* man-kwds all-kwds) #f))
  (define normal-str (if (null? all-kwds) "" "normal "))
  (define normal-args
    (cond
      [(null? ar) "no arguments"]
      [(number? ar) (format "~a ~aargument~a" ar normal-str (plural ar))]
      [(arity-at-least? ar) (format "~a or arbitrarily many more ~aarguments"
                                    (arity-at-least-value ar)
                                    normal-str)]
      [else
       (define comma
         (if (and (= (length ar) 2)
                  (not (arity-at-least? (list-ref ar 1))))
             ""
             ","))
       (apply
        string-append
        (let loop ([ar ar])
          (cond
            [(null? (cdr ar))
             (define v (car ar))
             (cond
               [(arity-at-least? v)
                (list
                 (format "~a, or arbitrarily many more ~aarguments" 
                         (arity-at-least-value v)
                         normal-str))]
               [else
                (list (format "or ~a ~aarguments" v normal-str))])]
            [else 
             (cons (format "~a~a " (car ar) comma)
                   (loop (cdr ar)))])))]))
  (cond
    [(and (null? man-kwds) (null? opt-kwds)) 
     normal-args]
    [(and (null? man-kwds) (not opt-kwds))
     (string-append normal-args " and optionally any keyword")]
    [(and (null? man-kwds) (pair? opt-kwds))
     (string-append normal-args 
                    " and the optional keyword" 
                    (plural (length opt-kwds))
                    " "
                    (kwd-list-as-string opt-kwds))]
    [(and (pair? man-kwds) (not opt-kwds))
     (string-append normal-args
                    ", the mandatory keyword"
                    (plural (length man-kwds))
                    " "
                    (kwd-list-as-string man-kwds)
                    ", and optionally any keyword")]
    [(and (pair? man-kwds) (null? opt-kwds))
     (string-append normal-args
                    " and the mandatory keyword"
                    (plural (length man-kwds))
                    " "
                    (kwd-list-as-string man-kwds))]
    [(and (pair? man-kwds) (pair? opt-kwds))
     (string-append normal-args
                    ", the mandatory keyword"
                    (plural (length man-kwds))
                    " "
                    (kwd-list-as-string man-kwds)
                    ", and the optional keyword"
                    (plural (length opt-kwds))
                    " "
                    (kwd-list-as-string opt-kwds))]))

(define (kwd-list-as-string kwds)
  (cond
    [(null? (cdr kwds))
     (format "~a" (list-ref kwds 0))]
    [(null? (cddr kwds))
     (format "~a and ~a" (list-ref kwds 0) (list-ref kwds 1))]
    [else 
     (apply
      string-append
      (let loop ([kwds kwds])
        (cond
          [(null? (cdr kwds))
           (list (format "and ~a" (car kwds)))]
          [else
           (cons (format "~a, " (car kwds))
                 (loop (cdr kwds)))])))]))

(define (maybe-err k blame neg-accepter)
  (if (blame-original? blame)
      (neg-accepter #f)
      (k neg-accepter)))

(define (build--> who 
                  raw-regular-doms raw-optional-doms 
                  mandatory-kwds mandatory-raw-kwd-doms
                  optional-kwds optional-raw-kwd-doms
                  raw-rest-ctc
                  pre-cond raw-rngs post-cond
                  proc)
  (define regular-doms
    (for/list ([dom (in-list (append raw-regular-doms raw-optional-doms))])
      (coerce-contract who dom)))
  (define mandatory-kwd-infos 
    (for/list ([kwd (in-list mandatory-kwds)]
               [dom (in-list mandatory-raw-kwd-doms)])
      (kwd-info kwd (coerce-contract who dom) #t)))
  (define optional-kwd-infos 
    (for/list ([kwd (in-list optional-kwds)]
               [dom (in-list optional-raw-kwd-doms)])
      (kwd-info kwd (coerce-contract who dom) #f)))
  (define kwd-infos (sort (append optional-kwd-infos mandatory-kwd-infos)
                          keyword<?
                          #:key kwd-info-kwd))
  (define rest-ctc (and raw-rest-ctc (coerce-contract who raw-rest-ctc)))
  (define rngs
    (and raw-rngs
         (for/list ([rng (in-list raw-rngs)])
           (coerce-contract who rng))))
  (if (and (andmap chaperone-contract? regular-doms)
           (andmap (λ (x) (chaperone-contract? (kwd-info-ctc x))) kwd-infos)
           (andmap chaperone-contract? (or rngs '())))
      (make--> (length raw-regular-doms) 
               regular-doms kwd-infos rest-ctc pre-cond
               rngs post-cond proc)
      (make-impersonator-> (length raw-regular-doms)
                           regular-doms kwd-infos rest-ctc pre-cond
                           rngs post-cond proc)))

;; kwd : keyword?
;; ctc : contract?
;; mandatory? : boolean?
(define-struct kwd-info (kwd ctc mandatory?) #:transparent)

;; min-arity : nat
;; doms : (listof contract?)[len >= min-arity]
;;        includes optional arguments in list @ end
;; kwd-infos : (listof kwd-info)
;; pre : (or/c #f (-> void))
;; rngs : (listof contract?)
;; post : (or/c #f (-> void))
;; proc : procedure? -- special, +1 argument wrapper that accepts neg-party
(define-struct base-> (min-arity doms kwd-infos rest pre rngs post proc)
  #:property prop:custom-write custom-write-property-proc)

(define (base->-name ctc)
  (define rngs (base->-rngs ctc))
  (define rng-sexp
    (cond
      [(not rngs) 'any]
      [(= 1 (length rngs))
       (contract-name (car rngs))]
      [else
       `(values ,@(map contract-name rngs))]))
  (cond
    [(and (andmap kwd-info-mandatory? (base->-kwd-infos ctc))
          (= (base->-min-arity ctc)
             (length (base->-doms ctc)))
          (not (base->-rest ctc))
          (not (base->-pre ctc))
          (not (base->-post ctc)))
     `(-> ,@(map contract-name (base->-doms ctc))
          ,@(apply
             append
             (for/list ([kwd-info (base->-kwd-infos ctc)])
               (list (kwd-info-kwd kwd-info) 
                     (contract-name (kwd-info-ctc kwd-info)))))
          ,rng-sexp)]
    [else
     (define (take l n) (reverse (list-tail (reverse l) (- (length l) n))))
     (define mandatory-args
       `(,@(map contract-name (take (base->-doms ctc) (base->-min-arity ctc)))
         ,@(apply
            append
            (for/list ([kwd-info (base->-kwd-infos ctc)]
                       #:when (kwd-info-mandatory? kwd-info))
              (list (kwd-info-kwd kwd-info) 
                    (contract-name (kwd-info-ctc kwd-info)))))))
     
     (define optional-args
       `(,@(map contract-name (list-tail (base->-doms ctc) (base->-min-arity ctc)))
         ,@(apply
            append
            (for/list ([kwd-info (base->-kwd-infos ctc)]
                       #:when (not (kwd-info-mandatory? kwd-info)))
              (list (kwd-info-kwd kwd-info) 
                    (contract-name (kwd-info-ctc kwd-info)))))))
     
     `(->* ,mandatory-args 
           ,@(if (null? optional-args)
                 '()
                 (list optional-args))
           ,@(if (base->-rest ctc)
                 (list '#:rest (contract-name (base->-rest ctc)))
                 (list))
           ,@(if (base->-pre ctc)
                 (list '#:pre '...)
                 (list))
           ,rng-sexp
           ,@(if (base->-post ctc)
                 (list '#:post '...)
                 (list)))]))

(define ((->-first-order ctc) x)
  (define l (base->-min-arity ctc))
  (define man-kwds (for/list ([kwd-info (base->-kwd-infos ctc)]
                              #:when (kwd-info-mandatory? kwd-info))
                     (kwd-info-kwd kwd-info)))
  (define opt-kwds (for/list ([kwd-info (base->-kwd-infos ctc)]
                              #:unless (kwd-info-mandatory? kwd-info))
                     (kwd-info-kwd kwd-info)))
  (and (procedure? x) 
       (if (base->-rest ctc)
           (arrow:procedure-accepts-and-more? x l)
           (procedure-arity-includes? x l #t))
       (arrow:keywords-match man-kwds opt-kwds x)
       #t))

(define (make-property build-X-property chaperone-or-impersonate-procedure)
  (define proj (mk-val-first-proj chaperone-or-impersonate-procedure))
  (parameterize ([skip-projection-wrapper? #t])
    (build-X-property
     #:name base->-name 
     #:first-order ->-first-order
     #:projection
     (λ (this)
       (λ (blame)
         (λ (val)
           ((((proj this) blame) val) #f))))
     #:stronger
     (λ (this that) 
       (and (base->? that)
            (= (length (base->-doms that))
               (length (base->-doms this)))
            (= (base->-min-arity this) (base->-min-arity that))
            (andmap contract-stronger? (base->-doms that) (base->-doms this))
            (= (length (base->-kwd-infos this))
               (length (base->-kwd-infos that)))
            (for/and ([this-kwd-info (base->-kwd-infos this)]
                      [that-kwd-info (base->-kwd-infos that)])
              (and (equal? (kwd-info-kwd this-kwd-info)
                           (kwd-info-kwd that-kwd-info))
                   (contract-stronger? (kwd-info-ctc that-kwd-info)
                                       (kwd-info-ctc this-kwd-info))))
            (if (base->-rngs this)
                (and (base->-rngs that)
                     (andmap contract-stronger? (base->-rngs this) (base->-rngs that)))
                (not (base->-rngs that)))
            (not (base->-pre this))
            (not (base->-pre that))
            (not (base->-post this))
            (not (base->-post that))))
     #:val-first-projection 
     proj)))

(define-struct (-> base->) ()
  #:property
  prop:chaperone-contract
  (make-property build-chaperone-contract-property chaperone-procedure))

(define-struct (impersonator-> base->) ()
  #:property
  prop:contract
  (make-property build-contract-property impersonate-procedure))
