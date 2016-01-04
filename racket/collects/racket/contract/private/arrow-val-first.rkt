#lang racket/base
(require (for-syntax racket/base
                     "application-arity-checking.rkt"
                     "arr-util.rkt")
         "kwd-info-struct.rkt"
         "blame.rkt"
         "misc.rkt"
         "prop.rkt"
         "guts.rkt"
         "generate.rkt"
         "arrow-higher-order.rkt"
         racket/stxparam
         (prefix-in arrow: "arrow.rkt"))

(provide ->2 ->*2
         dynamic->*
         (for-syntax ->2-handled?
                     ->2-arity-check-only->?
                     ->*2-handled?
                     ->2*-arity-check-only->?
                     ->-valid-app-shapes
                     ->*-valid-app-shapes)
         (rename-out [-predicate/c predicate/c]))

(define-for-syntax (->2-handled? stx)
  (syntax-case stx (any values any/c boolean?)
    [(_ args ...)
     (syntax-parameter-value #'arrow:making-a-method)
     #f]
    [_ #t]))

(define-for-syntax (->2-arity-check-only->? stx)
  (syntax-case stx (any any/c)
    [(_ any/c ... any) (- (length (syntax->list stx)) 2)]
    [_ #f]))

(define-for-syntax (->*2-handled? stx)
  (syntax-case stx (any values any/c)
    [(_ args ...)
     (syntax-parameter-value #'arrow:making-a-method)
     #f]
    [_ #t]))

(define-for-syntax (->2*-arity-check-only->? stx)
  (syntax-case stx (any any/c)
    [(_ (any/c ...) any) (length (syntax->list (cadr (syntax->list stx))))]
    [(_ (any/c ...) () any) (length (syntax->list (cadr (syntax->list stx))))]
    [_ #f]))

(define-for-syntax popular-keys
  ;; of the 8417 contracts that get compiled during
  ;; 'raco setup' of the current tree, these are all
  ;; the ones that appear at least 50 times (the
  ;; number indicate how many times each appeared)
  `((0 0 () () #f 1)   ; 1260
    (0 0 () () #t 1)   ;   58
    (1 0 () () #f #f)  ;  116
    (1 0 () () #f 1)   ; 4140
    (1 0 () () #t 1)   ;   71
    (1 1 () () #f 1)   ;  186
    (1 2 () () #f 1)   ;  125
    (2 0 () () #f #f)  ;   99
    (2 0 () () #f 1)   ; 1345
    (2 1 () () #f 1)   ;   68
    (3 0 () () #f 1)   ;  423
    (4 0 () () #f 1)   ;  149
    (5 0 () () #f 1))) ;   74

(define-syntax (generate-popular-key-ids stx)
  (syntax-case stx ()
    [(_ popular-key-ids)
     #`(define-for-syntax popular-key-ids
         (list #,@(map (λ (x y) #`(list (quote-syntax #,x) (quote-syntax #,y)))
                       (generate-temporaries (for/list ([e (in-list popular-keys)])
                                               'popular-plus-one-key-id))
                       (generate-temporaries (for/list ([e (in-list popular-keys)])
                                               'popular-chaperone-key-id)))))]))
(generate-popular-key-ids popular-key-ids)

(define-for-syntax (build-plus-one-arity-function+chaperone-constructor
                    regular-args
                    optional-args
                    mandatory-kwds
                    optional-kwds
                    pre pre/desc
                    rest
                    rngs
                    post post/desc)
  (define key (and (not pre) (not pre/desc)
                   (not post) (not post/desc)
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
       (define ids (list-ref popular-key-ids index))
       (values (list-ref ids 0) (list-ref ids 1)))]
    [else
     (values (build-plus-one-arity-function/real
              regular-args
              optional-args
              mandatory-kwds
              optional-kwds
              pre pre/desc
              rest
              rngs
              post post/desc)
             (build-chaperone-constructor/real
              '() ;; this-args 
              regular-args
              optional-args
              mandatory-kwds
              optional-kwds
              pre pre/desc
              rest
              rngs
              post post/desc))]))

(define-syntax (build-populars stx)
  (syntax-case stx ()
    [(_ popular-chaperone-key-table)
     #`(begin
         #,@(for/list ([ids (in-list popular-key-ids)]
                       [key (in-list popular-keys)])
              (define plus-one-id (list-ref ids 0))
              (define chaperone-id (list-ref ids 1))
              (define-values (regular-arg-count
                              optional-arg-count
                              mandatory-kwds
                              optional-kwds
                              rest
                              rngs)
                (apply values key))
              (define mans (for/list ([x (in-range regular-arg-count)])
                             (string->symbol (format "man~a" x))))
              (define opts (for/list ([x (in-range optional-arg-count)])
                             (string->symbol (format "opt~a" x))))
              (define rng-vars (and rngs (for/list ([x (in-range rngs)])
                                           (string->symbol (format "rng~a" x)))))
              #`(begin
                  (define #,(syntax-local-introduce plus-one-id)
                    #,(build-plus-one-arity-function/real
                       mans opts
                       mandatory-kwds
                       optional-kwds
                       #f #f
                       rest
                       rng-vars
                       #f #f))
                  (define #,(syntax-local-introduce chaperone-id)
                    #,(build-chaperone-constructor/real
                       '() ;; this arg
                       mans opts
                       mandatory-kwds
                       optional-kwds
                       #f #f
                       rest
                       rng-vars
                       #f #f))))
         (define popular-chaperone-key-table
           (make-hash
            (list #,@(for/list ([id (in-list popular-key-ids)]
                                [key (in-list popular-keys)])
                       #`(cons '#,key #,(list-ref id 1)))))))]))

(define-for-syntax (build-plus-one-arity-function/real
                    regular-args
                    optional-args
                    mandatory-kwds
                    optional-kwds
                    pre pre/desc
                    rest
                    rngs
                    post post/desc)
  (with-syntax ([(regb ...) (generate-temporaries regular-args)]
                [(optb ...) (generate-temporaries optional-args)]
                [(kb ...) (generate-temporaries mandatory-kwds)]
                [(okb ...) (generate-temporaries optional-kwds)]
                [(rb ...) (generate-temporaries (or rngs '()))]
                [(arg-x ...) (generate-temporaries regular-args)]
                [(res-x ...) (generate-temporaries (or rngs '()))]
                [(kwd-arg-x ...) (generate-temporaries mandatory-kwds)])

    (define base-arg-expressions (reverse (syntax->list #'((regb arg-x neg-party) ...))))
    (define normal-arg-vars (generate-temporaries #'(arg-x ...)))
    (define base-arg-vars normal-arg-vars)

    (with-syntax ([(formal-kwd-args ...)
                   (apply append (map list mandatory-kwds (syntax->list #'(kwd-arg-x ...))))]
                  [(kwd-arg-exps ...)
                   (apply
                    append
                    (map (λ (kwd kwd-arg-x kb)
                           (set! base-arg-expressions
                                 (cons #`(#,kb #,kwd-arg-x neg-party)
                                       base-arg-expressions))
                           (set! base-arg-vars (cons (car (generate-temporaries (list kwd-arg-x)))
                                                     base-arg-vars))
                           (list kwd (car base-arg-vars)))
                         mandatory-kwds
                         (syntax->list #'(kwd-arg-x ...))
                         (syntax->list #'(kb ...))))]
                  [(letrec-bound-id) (generate-temporaries '(f))])
      
      (with-syntax ([(wrapper-args ...) #'(neg-party arg-x ... formal-kwd-args ...)]
                    [(the-call ...) #`(f #,@(reverse normal-arg-vars) kwd-arg-exps ...)]
                    [(pre-check ...)
                     (if pre 
                         (list #`(check-pre-cond #,pre blame neg-party f))
                         (list))]
                    [(post-check ...)
                     (if post
                         (list #`(check-post-cond #,post blame neg-party f))
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
                 (define args-expressions base-arg-expressions)
                 (define args-vars base-arg-vars)
                 (define no-rest-call
                   #`(the-call ...
                      #,@(for/list ([ob (in-list (reverse ob))]
                                    [optional-arg (in-list (reverse optional-args))])
                           (set! args-expressions
                                 (cons #`(#,ob #,optional-arg neg-party)
                                       args-expressions))
                           (set! args-vars
                                 (cons (car (generate-temporaries (list optional-arg)))
                                       args-vars))
                           (car args-vars))))
                 (define full-call
                   (cond
                     [(and first? rest)
                      (set! args-expressions (cons #'(restb rest-arg neg-party) args-expressions))
                      (set! args-vars (cons (car (generate-temporaries '(rest-args-arrow-contract)))
                                            args-vars))
                      #`(apply #,@no-rest-call #,(car args-vars))]
                     [else
                      no-rest-call]))
                 (define the-args #`(wrapper-args ... 
                                     #,@(reverse optional-args)
                                     #,@(if (and first? rest)
                                            #'rest-arg
                                            '())))
                 (define let-values-clause
                   #`[#,(reverse args-vars)
                      (with-contract-continuation-mark
                       blame+neg-party
                       (values #,@(reverse args-expressions)))])
                 
                 (define the-clause
                   (if rngs
                       #`[#,the-args
                          (let ([blame+neg-party (cons blame neg-party)])
                            pre-check ...
                            (define-values (failed res-x ...)
                              (call-with-values
                               (λ () (let-values (#,let-values-clause)
                                       #,full-call))
                               (case-lambda
                                 [(res-x ...)
                                  (values #f res-x ...)]
                                 [args
                                  (values args #,@(map (λ (x) #'#f) 
                                                       (syntax->list #'(res-x ...))))])))
                            (with-contract-continuation-mark
                              blame+neg-party
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
                                 (values
                                  (rb res-x neg-party)
                                  ...)])))]
                       #`[#,the-args
                          pre-check ...
                          (let ([blame+neg-party (cons blame neg-party)])
                            (let-values (#,let-values-clause)
                              #,full-call))]))
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
                                   #,(if pre pre #'#f)
                                   '(#,@mandatory-kwds) (list kb ...)
                                   '(#,@optional-kwds) (list okb ...) 
                                   #,(length regular-args) (list regb ... optb ...) 
                                   #,(if rest #'restb #'#f)
                                   #,(if post post #'#f)
                                   #,(if rngs #'(list rb ...) #'#f))]))
        #`(λ (blame f regb ... optb ... kb ... okb ... rb ... #,@(if rest (list #'restb) '()))
            (procedure-specialize
             #,body-proc))))))

(define (make-checking-proc f blame pre
                            original-mandatory-kwds kbs
                            original-optional-kwds okbs
                            minimum-arg-count rbs rest-ctc
                            post rngs)
  (make-keyword-procedure
   (λ (actual-kwds actual-kwd-args neg-party . regular-args)
     (check-arg-count minimum-arg-count (length rbs) regular-args f blame neg-party rest-ctc)
     (check-keywords original-mandatory-kwds original-optional-kwds actual-kwds f blame neg-party)
     (define (mk-call)
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
                (cons ((car kbs) kwd-arg neg-party)
                      (loop (cdr kwds) 
                            (cdr kwd-args)
                            (cdr mandatory-kwds)
                            optional-kwds
                            (cdr kbs)
                            okbs))]
               [(and (pair? optional-kwds)
                     (equal? (car optional-kwds) kwd))
                (cons ((car okbs) kwd-arg neg-party)
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
            [(null? rbs) (rest-ctc regular-args neg-party)]
            [else
             (cons ((car rbs) (car regular-args) neg-party)
                   (loop (cdr regular-args) (cdr rbs)))]))))
     (define complete-blame (blame-add-missing-party blame neg-party))
     (when pre (check-pre-cond pre blame neg-party f))
     (cond
       [rngs
        (define results (call-with-values mk-call list))
        (define rng-len (length rngs))
        (unless (= (length results) rng-len)
          (arrow:bad-number-of-results complete-blame f rng-len results))
        (when post (check-post-cond post blame neg-party f))
        (apply
         values
         (for/list ([result (in-list results)]
                    [rng (in-list rngs)])
           (rng result neg-party)))]
       [else
        (mk-call)]))))

(build-populars popular-chaperone-key-table)
(define (lookup-popular-chaperone-key regular-arg-count
                                      optional-arg-count
                                      mandatory-kwds
                                      optional-kwds
                                      rest
                                      rngs)
  (define key (list regular-arg-count
                    optional-arg-count
                    mandatory-kwds
                    optional-kwds
                    rest
                    rngs))
  (hash-ref popular-chaperone-key-table key #f))

(define (check-arg-count minimum-arg-count len-rbs regular-args val blame neg-party rest-ctc)
  (define actual-count (length regular-args))
  (cond
    [(< actual-count minimum-arg-count)
     (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                        '(expected: "~a~a arguments")
                        (if (= len-rbs minimum-arg-count)
                            ""
                            "at least ")
                        minimum-arg-count)]
    [(and (not rest-ctc) (< len-rbs actual-count))
     (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                        '(expected: "~a~a arguments")
                        (if (= len-rbs minimum-arg-count)
                            ""
                            "at most ")
                        len-rbs)]))

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

(define-for-syntax (->-valid-app-shapes stx)
  (syntax-case stx ()
    [(_ args ...)
     (let ()
       (define this-> (gensym 'this->))
       (define-values (regular-args kwds kwd-args let-bindings)
         (parse-arrow-args stx (syntax->list #'(args ...)) this->))
       (valid-app-shapes (list (- (length regular-args) 1))
                         (map syntax->datum kwds)
                         '()))]))

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
       (define-values (plus-one-arity-function chaperone-constructor)
         (build-plus-one-arity-function+chaperone-constructor 
          regular-args '() kwds '() #f #f #f rngs #f #f))
       (syntax-property
        #`(let #,let-bindings
            #,(quasisyntax/loc stx
                (build-simple-->
                 (list #,@regular-args)
                 '(#,@kwds)
                 (list #,@kwd-args)
                 #,(if rngs
                       #`(list #,@rngs)
                       #'#f)
                 #,plus-one-arity-function
                 #,chaperone-constructor)))
        'racket/contract:contract
        (vector this->
                ;; the -> in the original input to this guy
                (list (car (syntax-e stx)))
                '())))]))

;; not quite the same as split-doms in arr-util.rkt, but similar idea.
(define-for-syntax (:split-doms stx name raw-doms this->*)
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
               (cons #`[x #,(syntax-property #'arg 
                                             'racket/contract:negative-position 
                                             this->*)]
                     let-bindings)))]
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
               (cons #`[t #,(syntax-property #'x
                                             'racket/contract:negative-position 
                                             this->*)]
                     let-bindings)))])))

(define-for-syntax (parse->*2 stx this->*)
  (syntax-case stx ()
    [(_ (raw-mandatory-dom ...) . other)
     (let ()
       (define-values (raw-optional-doms rest-ctc pre pre/desc rng-ctcs post post/desc)
         (arrow:parse-leftover->* stx #'other))
       (with-syntax ([(man-dom
                       man-dom-kwds
                       man-lets)
                      (:split-doms stx '->* #'(raw-mandatory-dom ...) this->*)]
                     [(opt-dom
                       opt-dom-kwds
                       opt-lets)
                      (:split-doms stx '->* raw-optional-doms this->*)])
         (values
          #'man-dom
          #'man-dom-kwds
          #'man-lets
          #'opt-dom
          #'opt-dom-kwds
          #'opt-lets
          rest-ctc pre pre/desc rng-ctcs post post/desc)))]))

(define-for-syntax (->*-valid-app-shapes stx)
  (define this->* (gensym 'this->*))
  (define-values (man-dom man-dom-kwds man-lets
                          opt-dom opt-dom-kwds opt-lets
                          rest-ctc pre pre/desc rng-ctcs post post/desc)
    (parse->*2 stx this->*))
  (with-syntax ([((mandatory-dom-kwd mandatory-dom-kwd-ctc) ...) man-dom-kwds]
                [((optional-dom-kwd optional-dom-kwd-ctc) ...) opt-dom-kwds])
    (valid-app-shapes-from-man/opts (length (syntax->list man-dom))
                                    (length (syntax->list opt-dom))
                                    rest-ctc
                                    (syntax->datum #'(mandatory-dom-kwd ...))
                                    (syntax->datum #'(optional-dom-kwd ...)))))

(define-syntax (->*2 stx)
  (cond
    [(->*2-handled? stx)
     (define this->* (gensym 'this->*))
     (define-values (man-dom man-dom-kwds man-lets
                             opt-dom opt-dom-kwds opt-lets
                             rest-ctc pre pre/desc rng-ctcs post post/desc)
       (parse->*2 stx this->*))
     (with-syntax ([(mandatory-dom ...) man-dom]
                   [((mandatory-dom-kwd mandatory-dom-kwd-ctc) ...) man-dom-kwds]
                   [(mandatory-let-bindings ...) man-lets]
                   [(optional-dom ...) opt-dom]
                   [((optional-dom-kwd optional-dom-kwd-ctc) ...) opt-dom-kwds]
                   [(optional-let-bindings ...) opt-lets]
                   [(pre-x post-x) (generate-temporaries '(pre-cond post-cond))])
       (with-syntax ([((kwd dom opt?) ...) #'((mandatory-dom-kwd mandatory-dom-kwd-ctc #f) ...
                                              (optional-dom-kwd optional-dom-kwd-ctc #t) ...)]
                     [(pre-let-binding ...) (if (or pre pre/desc)
                                               (list #`[pre-x (λ () #,(or pre pre/desc))])
                                               (list))]
                     [(post-let-binding ...) (if (or post post/desc)
                                                 (list #`[post-x (λ () #,(or post post/desc))])
                                                 (list))])
         (define-values (plus-one-arity-function chaperone-constructor)
           (build-plus-one-arity-function+chaperone-constructor
            (syntax->list #'(mandatory-dom ...))
            (syntax->list #'(optional-dom ...))
            (syntax->list #'(mandatory-dom-kwd ...))
            (syntax->list #'(optional-dom-kwd ...))
            (and pre #'pre-x)
            (and pre/desc #'pre-x)
            rest-ctc
            rng-ctcs
            (and post #'post-x)
            (and post/desc #'post-x)))
         (syntax-property
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
                        #,(and pre #t)
                        #,(if rng-ctcs
                              #`(list #,@(for/list ([rng-ctc (in-list (syntax->list rng-ctcs))])
                                           (syntax-property rng-ctc
                                                            'racket/contract:positive-position
                                                            this->*)))
                              #'#f)
                        #,(and post #t)
                        #,plus-one-arity-function 
                        #,chaperone-constructor))
          
          'racket/contract:contract
          (vector this->*
                  ;; the -> in the original input to this guy
                  (list (car (syntax-e stx)))
                  '()))))]
    [else
     (syntax-case stx ()
       [(_ args ...)
        #'(arrow:->* args ...)])]))

(define (wrong-number-of-results-blame blame neg-party val reses expected-values)
  (define length-reses (length reses))
  (raise-blame-error 
   blame #:missing-party neg-party val
   '("received ~a value~a" expected: "~a value~a")
   length-reses
   (if (= 1 length-reses) "" "s")
   expected-values
   (if (= 1 expected-values) "" "s")))

(define (build-simple--> raw-regular-doms
                         mandatory-kwds mandatory-raw-kwd-doms
                         raw-rngs
                         plus-one-arity-function
                         chaperone-constructor)
  (build--> '->
            raw-regular-doms '() 
            mandatory-kwds mandatory-raw-kwd-doms
            '() '()
            #f
            #f raw-rngs #f
            plus-one-arity-function
            chaperone-constructor))

(define (build--> who 
                  raw-regular-doms raw-optional-doms 
                  mandatory-kwds mandatory-raw-kwd-doms
                  optional-kwds optional-raw-kwd-doms
                  raw-rest-ctc
                  pre-cond raw-rngs post-cond
                  plus-one-arity-function
                  chaperone-constructor)
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
  (cond
    ;; uncomment this to specialize (-> void) contract to a
    ;; more efficient wrapper (but there are no test cases for
    ;; that code, so add them before pushing this)
    #;
    [(and (null? regular-doms)
          (null? kwd-infos)
          (not rest-ctc)
          (not pre-cond)
          (not post-cond)
          (pair? rngs)
          (null? (cdr rngs))
          (flat-contract? (car rngs))
          (eq? void? (flat-contract-predicate (car rngs))))
     ->void-contract]
    [(and (pair? regular-doms)
          (null? (cdr regular-doms))
          (any/c? (car regular-doms))
          (null? kwd-infos)
          (not rest-ctc)
          (not pre-cond)
          (not post-cond)
          (pair? rngs)
          (null? (cdr rngs))
          (flat-contract? (car rngs))
          (eq? boolean? (flat-contract-predicate (car rngs))))
     any/c->boolean-contract]
    [(and (andmap chaperone-contract? regular-doms)
          (andmap (λ (x) (chaperone-contract? (kwd-info-ctc x))) kwd-infos)
          (andmap chaperone-contract? (or rngs '())))
     (make--> (length raw-regular-doms)
              regular-doms kwd-infos rest-ctc pre-cond
              rngs post-cond
              plus-one-arity-function
              chaperone-constructor)]
    [else
     (make-impersonator-> (length raw-regular-doms)
                          regular-doms kwd-infos rest-ctc pre-cond
                          rngs post-cond
                          plus-one-arity-function
                          chaperone-constructor)]))

(define (dynamic->* #:mandatory-domain-contracts [mandatory-domain-contracts '()]
                    #:optional-domain-contracts [optional-domain-contracts '()]
                    #:mandatory-keywords [unsorted-mandatory-keywords '()]
                    #:mandatory-keyword-contracts [unsorted-mandatory-keyword-contracts '()]
                    #:optional-keywords [unsorted-optional-keywords '()]
                    #:optional-keyword-contracts [unsorted-optional-keyword-contracts '()]
                    #:rest-contract [rest-contract #f]
                    #:range-contracts range-contracts)
  
  ;; leave these out for now
  (define pre-cond #f)
  (define post-cond #f)
  
  (define-syntax-rule (check-list e) (check-list/proc e 'e))
  (define (check-list/proc e name)
    (unless (list? e)
      (raise-argument-error 
       'dynamic->*
       (format "list? in the #:~a argument" name)
       e)))
  (define (check-list/kwds e name)
    (unless (andmap keyword? e)
      (raise-argument-error 
       'dynamic->*
       (format "(listof keyword?) in the #:~a argument" name)
       e)))
  (define (check-same-length l1 l2 name)
    (unless (= (length l1) (length l2))
      (error 'dynamic->*
             (string-append
              "expected the length of the #:~a-keywords argument"
              " to be the same as the length of the #:~a-keyword-contracts argument")
             name name)))
  (check-list mandatory-domain-contracts)
  (check-list optional-domain-contracts)
  (check-list unsorted-mandatory-keywords)
  (check-list/kwds unsorted-mandatory-keywords 'mandatory-keywords)
  (check-list unsorted-mandatory-keyword-contracts)
  (check-same-length unsorted-mandatory-keywords unsorted-mandatory-keyword-contracts 'mandatory)
  (check-list unsorted-optional-keywords)
  (check-list/kwds unsorted-optional-keywords 'optional-keywords)
  (check-list unsorted-optional-keyword-contracts)
  (check-same-length unsorted-optional-keywords unsorted-optional-keyword-contracts 'optional)
  (unless (or (not range-contracts)
              (list? range-contracts))
    (raise-argument-error 'dynamic->*
                          "(or/c (listof contract?) #f) in the #:range-contracts argument"
                          range-contracts))
  
  (define (sort-kwds unsorted-keywords unsorted-keyword-contracts)
    (define sorted
      (sort (map cons unsorted-keywords unsorted-keyword-contracts)
            keyword<?
            #:key car))
    (values (map car sorted) (map cdr sorted)))
  (define-values (mandatory-keywords mandatory-keyword-contracts)
    (sort-kwds unsorted-mandatory-keywords unsorted-mandatory-keyword-contracts))
  (define-values (optional-keywords optional-keyword-contracts)
    (sort-kwds unsorted-optional-keywords unsorted-optional-keyword-contracts))
  
  (define-syntax-rule 
    (define-next next args)
    (define (next n) 
      (let loop ([n n][_args args])
        (cond
          [(zero? n) (set! args _args) '()]
          [(null? _args) (error 'plug-one-arity-function-dynamic->* "internal error")]
          [else (cons (car _args) (loop (- n 1) (cdr _args)))]))))
  
  (define (plus-one-arity-function blame f . args)
    (make-keyword-procedure
     (λ (kwds kwd-args . regular-args)
       (error 'plus-one-arity-function "not implemented for dynamic->*"))))
  
  (define min-arity (length mandatory-domain-contracts))
  (define optionals (length optional-domain-contracts))
  (define rng-len (and range-contracts (length range-contracts)))
  (define max-arity (if rest-contract #f (+ min-arity optionals)))

  (define build-chaperone-constructor
    (or (lookup-popular-chaperone-key min-arity
                                      optionals
                                      mandatory-keywords
                                      optional-keywords
                                      (and rest-contract #t)
                                      rng-len)
        (λ (blame f neg-party blame-party-info rng-ctc-x . args)
          (define-next next args)
          (define mandatory-dom-projs (next min-arity))
          (define optional-dom-projs (next optionals))
          (define rest-proj (if rest-contract
                                (car (next 1))
                                #f))
          (define mandatory-dom-kwd-projs (next (length mandatory-keyword-contracts)))
          (define optional-dom-kwd-projs (next (length optional-keyword-contracts)))
          (define rng-projs (and rng-len (next rng-len)))
          (define mandatory+optional-dom-projs (append mandatory-dom-projs optional-dom-projs))
          (define kwd-table
            (make-hash
             (for/list ([kwd (in-list (append mandatory-keywords optional-keywords))]
                        [kwd-proj (in-list (append mandatory-dom-kwd-projs optional-dom-kwd-projs))])
               (cons kwd kwd-proj))))
          
          (define interposition-proc
            (make-keyword-procedure
             (λ (kwds kwd-args . args)
               
               (check-arg-count min-arity max-arity args f blame neg-party rest-contract)
               (check-keywords mandatory-keywords optional-keywords kwds f blame neg-party)
               
               (define kwd-results
                 (for/list ([kwd (in-list kwds)]
                            [kwd-arg (in-list kwd-args)])
                   ((hash-ref kwd-table kwd) kwd-arg neg-party)))
               (define regular-arg-results
                 (let loop ([args args]
                            [projs mandatory+optional-dom-projs])
                   (cond
                     [(and (null? projs) (null? args)) '()]
                     [(null? projs)
                      (rest-proj args neg-party)]
                     [(null? args) (error 'cant-happen::dynamic->*)]
                     [else (cons ((car projs) (car args) neg-party)
                                 (loop (cdr args) (cdr projs)))])))
               (define (result-checker . results)
                 (unless (= rng-len (length results))
                   (arrow:bad-number-of-results (blame-add-missing-party blame neg-party)
                                                f rng-len results))
                 (apply 
                  values
                  (for/list ([res (in-list results)]
                             [neg-party-proj (in-list rng-projs)])
                    (neg-party-proj res neg-party))))
               (define args-dealt-with
                 (if (null? kwds)
                     regular-arg-results
                     (cons kwd-results regular-arg-results)))
               (apply
                values
                (if range-contracts
                    (cons result-checker args-dealt-with)
                    args-dealt-with)))))
          
          (arrow:arity-checking-wrapper f blame neg-party
                                        interposition-proc interposition-proc
                                        min-arity max-arity
                                        min-arity max-arity 
                                        mandatory-keywords optional-keywords))))
  
  (build--> 'dynamic->*
            mandatory-domain-contracts optional-domain-contracts 
            mandatory-keywords mandatory-keyword-contracts
            optional-keywords optional-keyword-contracts
            rest-contract
            pre-cond range-contracts post-cond
            plus-one-arity-function
            build-chaperone-constructor))

;; min-arity : nat
;; doms : (listof contract?)[len >= min-arity]
;;        includes optional arguments in list @ end
;; kwd-infos : (listof kwd-info)
;; rest : (or/c #f contract?)
;; pre? : boolean?
;; rngs : (listof contract?)
;; post? : boolean?
;; plus-one-arity-function : procedure? -- special, +1 argument wrapper that accepts neg-party
;; chaperone-constructor ; procedure? -- function that builds a projection tailored to this arrow
(define-struct base-> (min-arity doms kwd-infos rest pre? rngs post?
                                 plus-one-arity-function chaperone-constructor)
  #:property prop:custom-write custom-write-property-proc)

(define (->-generate ctc)
  (cond
    [(and (equal? (length (base->-doms ctc))
                  (base->-min-arity ctc))
          (not (base->-rest ctc)))
     ;; only handle the case with no optional args and no rest args
     (define dom-ctcs (base->-doms ctc))
     (define doms-l (length dom-ctcs))
     (λ (fuel)
       (define dom-exers '())
       (define addl-available dom-ctcs)
       (for ([c (in-list (base->-doms ctc))])
         (define-values (exer ctcs) ((contract-struct-exercise c) fuel))
         (set! dom-exers (cons exer dom-exers))
         (set! addl-available (append ctcs addl-available)))
       (define rngs-gens 
         (if (base->-rngs ctc)
             (with-definitely-available-contracts
              addl-available
              (λ ()
                (for/list ([c (in-list (base->-rngs ctc))])
                  (contract-random-generate/choose c fuel))))
             '()))
       (cond
         [(for/and ([rng-gen (in-list rngs-gens)])
            rng-gen)
          (define env (contract-random-generate-get-current-environment))
          (λ ()
            (procedure-reduce-arity
             (λ args
               ; stash the arguments for use by other generators
               (for ([ctc (in-list dom-ctcs)]
                     [arg (in-list args)])
                 (contract-random-generate-stash env ctc arg))
               ; exercise the arguments
               (for ([arg (in-list args)]
                     [dom-exer (in-list dom-exers)])
                 (dom-exer arg))
               ; compute the results 
               (define results
                 (for/list ([rng-gen (in-list rngs-gens)])
                   (rng-gen)))
               ; return the results
               (apply values results))
             doms-l))]
         [else #f]))]
    [else (λ (fuel) #f)]))

(define (->-exercise ctc)
  (define rng-ctcs (base->-rngs ctc))
  (define dom-ctcs (for/list ([doms (in-list (base->-doms ctc))]
                              [i (in-range (base->-min-arity ctc))])
                     doms))
  (define dom-kwd-infos (for/list ([dom-kwd (in-list (base->-kwd-infos ctc))]
                                   #:when (kwd-info-mandatory? dom-kwd))
                          dom-kwd))
  (define dom-kwds (map kwd-info-kwd dom-kwd-infos))
  (cond
    [(not (base->-rest ctc))
     (λ (fuel)
       (define gens 
         (for/list ([dom-ctc (in-list dom-ctcs)])
           (contract-random-generate/choose dom-ctc fuel)))
       (define kwd-gens
         (for/list ([kwd-info (in-list dom-kwd-infos)])
           (contract-random-generate/choose (kwd-info-ctc kwd-info) fuel)))
       (define env (contract-random-generate-get-current-environment))
       (cond
         [(and (andmap values gens)
               (andmap values kwd-gens))
          (values 
           (λ (f)
             (call-with-values
              (λ ()
                (keyword-apply 
                 f
                 dom-kwds
                 (for/list ([kwd-gen (in-list kwd-gens)])
                   (kwd-gen))
                 (for/list ([gen (in-list gens)])
                   (gen))))
              (λ results 
                (when rng-ctcs
                  (for ([res-ctc (in-list rng-ctcs)]
                        [result (in-list results)])
                    (contract-random-generate-stash env res-ctc result))))))
           (or rng-ctcs '()))]
         [else
          (values void '())]))]
    [else
     (λ (fuel) (values void '()))]))

(define (base->-name ctc)
  (cond
    [(predicate/c? ctc) 'predicate/c]
    [else
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
             (not (base->-pre? ctc))
             (not (base->-post? ctc)))
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
              ,@(if (base->-pre? ctc)
                    (list '#:pre '...)
                    (list))
              ,rng-sexp
              ,@(if (base->-post? ctc)
                    (list '#:post '...)
                    (list)))])]))

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
  (define val-first-proj
    (λ (->stct)
      (maybe-warn-about-val-first ->stct)
      (->-proj chaperone-or-impersonate-procedure ->stct
               (base->-min-arity ->stct)
               (base->-doms ->stct)
               (base->-kwd-infos ->stct)
               (base->-rest ->stct)
               (base->-pre? ->stct)
               (base->-rngs ->stct)
               (base->-post? ->stct)
               (base->-plus-one-arity-function ->stct)
               (base->-chaperone-constructor ->stct)
               #f)))
  (define late-neg-proj
    (λ (->stct)
      (->-proj chaperone-or-impersonate-procedure ->stct
               (base->-min-arity ->stct)
               (base->-doms ->stct)
               (base->-kwd-infos ->stct)
               (base->-rest ->stct)
               (base->-pre? ->stct)
               (base->-rngs ->stct)
               (base->-post? ->stct)
               (base->-plus-one-arity-function ->stct)
               (base->-chaperone-constructor ->stct)
               #t)))
  (build-X-property
   #:name base->-name 
   #:first-order ->-first-order
   #:projection
   (λ (this)
     (define cthis (val-first-proj this))
     (λ (blame)
       (define cblame (cthis blame))
       (λ (val)
         ((cblame val) #f))))
   #:stronger ->-stronger
   #:generate ->-generate
   #:exercise ->-exercise
   #:val-first-projection val-first-proj
   #:late-neg-projection late-neg-proj))

(define (->-stronger this that)
  (and (base->? that)
       (= (length (base->-doms that))
          (length (base->-doms this)))
       (= (base->-min-arity this) (base->-min-arity that))
       (andmap contract-struct-stronger? (base->-doms that) (base->-doms this))
       (= (length (base->-kwd-infos this))
          (length (base->-kwd-infos that)))
       (for/and ([this-kwd-info (base->-kwd-infos this)]
                 [that-kwd-info (base->-kwd-infos that)])
         (and (equal? (kwd-info-kwd this-kwd-info)
                      (kwd-info-kwd that-kwd-info))
              (contract-struct-stronger? (kwd-info-ctc that-kwd-info)
                                         (kwd-info-ctc this-kwd-info))))
       (if (base->-rngs this)
           (and (base->-rngs that)
                (andmap contract-struct-stronger? (base->-rngs this) (base->-rngs that)))
           (not (base->-rngs that)))
       (not (base->-pre? this))
       (not (base->-pre? that))
       (not (base->-post? this))
       (not (base->-post? that))))
     
(define-struct (-> base->) ()
  #:property
  prop:chaperone-contract
  (make-property build-chaperone-contract-property chaperone-procedure))

(define-struct (predicate/c base->) ()
  #:property
  prop:chaperone-contract
  (make-property build-chaperone-contract-property chaperone-procedure))

(define-struct (impersonator-> base->) ()
  #:property
  prop:contract
  (make-property build-contract-property impersonate-procedure))

(define ->void-contract
  (let-syntax ([get-chaperone-constructor
                (λ (_)
                  (define desired-key '(0 0 () () #f 1))
                  (define expected-index 0)
                  (unless (equal? desired-key (list-ref popular-keys expected-index))
                    (error '->void-contract "expected the 0th key to be ~s" desired-key))
                  (define ids (list-ref popular-key-ids expected-index))
                  (list-ref ids 1))])
    (make--> 0 '() '() #f #f
             (list (coerce-contract 'whatever void?))
             #f
             (λ (blame f _ignored-rng-ctcs _ignored-rng-proj)
               (λ (neg-party)
                 (call-with-values
                  (λ () (f))
                  (case-lambda
                    [(rng)
                     (if (void? rng)
                         rng
                         (raise-blame-error blame #:missing-party neg-party rng
                                            '(expected: "void?" given: "~e")
                                            rng))]
                    [args
                     (wrong-number-of-results-blame blame neg-party f args 1)]))))
             (get-chaperone-constructor))))

(define (mk-any/c->boolean-contract constructor)
  (define (rng-checker f blame neg-party)
    (case-lambda
      [(rng)
       (if (boolean? rng)
           rng
           (raise-blame-error blame #:missing-party neg-party rng
                              '(expected: "boolean?" given: "~e")
                              rng))]
      [args
       (wrong-number-of-results-blame blame neg-party f args 1)]))
  (constructor 1 (list any/c) '() #f #f
               (list (coerce-contract 'whatever boolean?))
               #f
               (λ (blame f _ignored-dom-contract _ignored-rng-contract)
                 (λ (neg-party argument)
                   (call-with-values
                    (λ () (f argument))
                    (rng-checker f blame neg-party))))
               (λ (blame f neg-party
                         _ignored-blame-party-info
                         _ignored-rng-ctcs
                         _ignored-dom-contract
                         _ignored-rng-contract)
                 (unless (procedure? f)
                   (raise-blame-error
                    blame #:missing-party neg-party f
                    '(expected: "a procedure" given: "~e")
                    f))
                 (unless (procedure-arity-includes? f 1)
                   (raise-blame-error
                    blame #:missing-party neg-party f
                    '(expected: "a procedure that accepts 1 non-keyword argument"
                                given: "~e")
                    f))
                 (cond
                   [(and (struct-predicate-procedure? f)
                         (not (impersonator? f)))
                    #f]
                   [(and (equal? (procedure-arity f) 1)
                         (let-values ([(required mandatory) (procedure-keywords f)])
                           (and (null? required)
                                (null? mandatory))))
                    (λ (arg)
                      (values (rng-checker f blame neg-party) arg))]
                   [(procedure-arity-includes? f 1)
                    (make-keyword-procedure
                     (λ (kwds kwd-args . other)
                       (unless (null? kwds)
                         (arrow:raise-no-keywords-arg blame #:missing-party neg-party f kwds))
                       (unless (= 1 (length other))
                         (arrow:raise-wrong-number-of-args-error #:missing-party neg-party
                                                                 blame f (length other) 1 1 1))
                       (values (rng-checker f blame neg-party) (car other))))]))))

(define -predicate/c (mk-any/c->boolean-contract predicate/c))
(define any/c->boolean-contract (mk-any/c->boolean-contract make-->))
