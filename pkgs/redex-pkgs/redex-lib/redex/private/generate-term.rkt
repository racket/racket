#lang racket/base
(require "rg.rkt" 
         "jdg-gen.rkt"
         "error.rkt"
         "enum.rkt"
         "reduction-semantics.rkt"
         "struct.rkt"
         "term.rkt"
         "matcher.rkt"
         "judgment-form.rkt"
         "search.rkt"
         "term-fn.rkt"
         "pat-unify.rkt"
         racket/contract
         racket/match
         racket/pretty
         (for-syntax racket/base
                     racket/set
                     syntax/stx
                     setup/path-to-relative
                     "rewrite-side-conditions.rkt"
                     "term-fn.rkt"
                     "keyword-macros.rkt")
         math/base
         math/distributions)

(define-for-syntax (metafunc name)
  (and (identifier? name)
       (let ([tf (syntax-local-value name (λ () #f))])
         (and (term-fn? tf) (term-fn-get-id tf)))))

(define-for-syntax (metafunc/err name stx)
  (let ([m (metafunc name)])
    (if m m (raise-syntax-error #f "not a metafunction" stx name))))

(define-for-syntax (term-generator lang pattern what)
  (with-syntax ([pattern pattern])
    #`((compile #,lang '#,what) `pattern)))

(define (make-generator raw-generators form-name)
  (λ (size #:attempt-num [attempt-num 1] #:retries [retries default-retries])
    (define (check-arg arg)
      (unless (natural-number/c arg)
        (raise-argument-error form-name "natural number" arg)))
    (check-arg size)
    (check-arg attempt-num)
    (check-arg retries)
    (let-values ([(term _) ((match raw-generators
                              [(list g) g]
                              [_ (pick-from-list raw-generators)])
                            size attempt-num retries)])
      term)))

(define-for-syntax (show-message stx)
  (syntax-case stx ()
    [(what . _)
     (identifier? #'what)
     (with-syntax ([loc (if (and (path? (syntax-source stx))
                                 (syntax-line stx))
                            (format "~a:~a"
                                    (path->relative-string/library (syntax-source stx)) 
                                    (syntax-line stx))
                            #f)])
       #`(λ (msg)
           (fprintf 
            (current-output-port)
            "~a: ~a~a"
            'what (if loc (string-append loc "\n") "") msg)))]))

(define-for-syntax attempts-keyword
  (list '#:attempts #'(default-check-attempts)
        (list #'natural-number/c "#:attempts argument")))
(define-for-syntax source-keyword
  (list '#:source #f))
(define-for-syntax retries-keyword
  (list '#:retries #'default-retries 
        (list #'natural-number/c "#:retries argument")))
(define-for-syntax print?-keyword
  (list '#:print? #t))
(define-for-syntax attempt-size-keyword
  (list '#:attempt-size #'default-attempt-size 
        (list #'attempt-size/c "#:attempt-size argument")))
(define-for-syntax keep-going-keyword
  (list '#:keep-going? #'#f (list #'boolean? "#:keep-going? argument")))
(define-for-syntax (prepare-keyword lists?)
  (list '#:prepare #f 
        (list (if lists? #'(-> list? list?) #'(-> any/c any/c)) 
              "#:prepare argument")))

(define-for-syntax satisfying-disallowed-kws
  (set '#:source '#:retries))

(define-syntax (redex-check stx)
  (define valid-kws 
    (list* '#:satisfying
           '#:ad-hoc
           '#:enum
           '#:uniform-at-random
           '#:in-order
           (map car (list attempts-keyword
                          source-keyword
                          retries-keyword
                          print?-keyword
                          attempt-size-keyword
                          keep-going-keyword
                          (prepare-keyword #f)))))
  (define used-kws
    (for/fold ([kws (set)])
              ([maybe-kw-stx (in-list (syntax->list stx))])
      (define maybe-kw (syntax-e maybe-kw-stx))
      (cond 
        [(keyword? maybe-kw)
         (unless (member maybe-kw valid-kws)
           (raise-syntax-error 'redex-check "unknown keyword" stx maybe-kw-stx))
         (set-add kws maybe-kw)]
        [else kws])))
  (define bad-kws (set-intersect used-kws satisfying-disallowed-kws))
  (syntax-case stx (=)
    [(form lang #:satisfying (mf-id . args) = res property . kw-args)
     (unless (set-empty? bad-kws)
       (raise-syntax-error 'redex-check (format "~s cannot be used with #:satisfying" (car (set->list bad-kws))) stx))
     (redex-check/mf stx #'form #'lang #'mf-id #'args #'res #'property #'kw-args)]
    [(form lang #:satisfying (jform-id . pats) property . kw-args)
     (unless (set-empty? bad-kws)
       (raise-syntax-error 'redex-check (format "~s cannot be used with #:satisfying" (car (set->list bad-kws))) stx))
     (redex-check/jf stx #'form #'lang #'jform-id #'pats #'property #'kw-args)]
    [(form lang #:satisfying . rest)
     (raise-syntax-error 'redex-check "#:satisfying expected judgment form or metafunction syntax followed by a property" stx #'rest)]
    [(form lang pat #:enum biggest-e property . kw-args)
     (redex-check/pat stx #'form #'lang #'pat #'property #'biggest-e #f #f #f #'kw-args)]
    [(form lang pat #:uniform-at-random p-value property . kw-args)
     (redex-check/pat stx #'form #'lang #'pat #'property #f #'p-value #f #f #'kw-args)]
    [(form lang pat #:in-order property . kw-args)
     (redex-check/pat stx #'form #'lang #'pat #'property #f #f #t #f #'kw-args)]
    [(form lang pat #:ad-hoc property . kw-args)
     (redex-check/pat stx #'form #'lang #'pat #'property #f #f #f #t #'kw-args)]
    [(form lang pat property . kw-args)
     (redex-check/pat stx #'form #'lang #'pat #'property #f #f #f #f #'kw-args)]))

(define-struct gen-fail ())

(define-for-syntax (redex-check/jf orig-stx form lang jf-id pats property kw-args)
  (define-values (attempts-stx source-stx retries-stx print?-stx size-stx fix-stx keep-going-stx)
    (parse-redex-check-kw-args kw-args orig-stx form))
  (define nts (definition-nts lang orig-stx 'redex-check))
  (unless (judgment-form-id? jf-id)
    (raise-syntax-error 'redex-check "expected a judgment-form" jf-id))
  (define j-f (lookup-judgment-form-id jf-id))
  (define clauses (judgment-form-gen-clauses j-f))
  (define relation? (judgment-form-relation? j-f))
  (define args-stx (if relation?
                       (syntax/loc #'args #`(#,pats))
                       pats))
  (with-syntax* ([(syncheck-exp pattern (names ...) (names/ellipses ...))
                  (rewrite-side-conditions/check-errs lang 'redex-check #t args-stx)]
                 [show (show-message orig-stx)]
                 [res-term-stx #`(#,jf-id #,@args-stx)]
                 [property #`(bind-prop
                               (λ (bindings)
                                 #,(bind-pattern-names 'redex-check
                                                       #'(names/ellipses ...)
                                                       #'((lookup-binding bindings 'names) ...)
                                                       property)))])
                (quasisyntax/loc orig-stx
                  (let ([term-match (λ (generated)
                                      (cond [(test-match #,lang res-term-stx generated) => values]
                                            [else (redex-error 'redex-check "~s does not match ~s" generated 'res-term-stx)]))])
                    syncheck-exp
                    (let ([default-attempt-size (λ (s) (add1 (default-attempt-size s)))])
                      (parameterize ([attempt->size #,size-stx]
                                     [unsupported-pat-err-name 'redex-check])
                        (check-one
                         (λ (size _1 _2)
                           (values
                            (match ((make-jf-gen/proc '#,jf-id #,clauses #,lang 'pattern size))
                              [(and res (? values)) res]
                              [else (gen-fail)])
                            #f))
                         property #,attempts-stx 0 (and #,print?-stx show) #,fix-stx term-match
                         #,keep-going-stx)))))))

(define-for-syntax (redex-check/mf orig-stx form lang mf-id args-stx res-stx property kw-args)
  (define-values (attempts-stx source-stx retries-stx print?-stx size-stx fix-stx keep-going-stx)
    (parse-redex-check-kw-args kw-args orig-stx form))
  (define nts (definition-nts lang orig-stx 'redex-check))
  (define m (metafunc mf-id))
  (unless m (raise-syntax-error 'generate-term "expected a metafuction" mf-id))
  (define mf (syntax-local-value mf-id (λ () #f)))
  (with-syntax* ([(lhs-syncheck-exp lhs-pat (lhs-names ...) (lhs-names/ellipses ...))
                  (rewrite-side-conditions/check-errs lang 'redex-check #t args-stx)]
                 [(rhs-syncheck-exp rhs-pat (rhs-names ...) (rhs-names/ellipses ...))
                  (rewrite-side-conditions/check-errs lang 'redex-check #t res-stx)]
                 [res-term-stx #`((#,mf-id #,@args-stx) = #,res-stx)]
                 [mf-id (term-fn-get-id mf)]
                 [show (show-message orig-stx)]
                 [property #`(bind-prop
                               (λ (bindings)
                                 #,(bind-pattern-names 'redex-check
                                                       #'(lhs-names/ellipses ... 
                                                          rhs-names/ellipses ...)
                                                       #'((lookup-binding bindings 'lhs-names) ...
                                                          (lookup-binding bindings 'rhs-names) ...)
                                                       property)))])
                (quasisyntax/loc orig-stx
                  (let ([term-match (λ (generated)
                                      (cond [(test-match #,lang res-term-stx generated) => values]
                                            [else (redex-error 'redex-check "~s does not match ~s" generated 'res-term-stx)]))])
                    lhs-syncheck-exp
                    rhs-syncheck-exp
                    (let ([default-attempt-size (λ (s) (add1 (default-attempt-size s)))])
                      (parameterize ([attempt->size #,size-stx])
                        (check-one
                         (λ (size _1 _2)
                           (values
                            (match ((make-mf-gen/proc 'mf-id mf-id #,lang 'lhs-pat 'rhs-pat size))
                              [(and res (? values)) res]
                              [else (gen-fail)])
                            #f))
                         property #,attempts-stx 0 (and #,print?-stx show) #,fix-stx term-match
                         #,keep-going-stx)))))))

(define-for-syntax (redex-check/pat orig-stx form lang pat property 
                                    enum-bound-e enum-p-value in-order? ad-hoc?
                                    kw-args)
  (with-syntax ([(syncheck-exp pattern (name ...) (name/ellipses ...))
                 (rewrite-side-conditions/check-errs 
                  lang
                  'redex-check #t pat)]
                [show (show-message orig-stx)])
    (define-values (attempts-stx source-stx retries-stx print?-stx size-stx fix-stx keep-going-stx)
      (parse-redex-check-kw-args kw-args orig-stx form)) 
    (with-syntax ([property #`(bind-prop
                               (λ (bindings)
                                 #,(bind-pattern-names 'redex-check
                                                       #'(name/ellipses ...)
                                                       #'((lookup-binding bindings 'name) ...)
                                                       property)))])
      (quasisyntax/loc orig-stx
        (let ([att #,attempts-stx]
              [ret #,retries-stx]
              [print? #,print?-stx]
              [fix #,fix-stx]
              [keep-going? #,keep-going-stx]
              [term-match (λ (generated)
                            (cond [(test-match #,lang #,pat generated) => values]
                                  [else (redex-error 'redex-check "~s does not match ~s" generated '#,pat)]))])
          syncheck-exp
          (parameterize ([attempt->size #,size-stx])
            #,(cond
                [source-stx
                 (when enum-bound-e
                   (raise-syntax-error
                    #f
                    "#:enum cannot be used with the #:source keyword"
                    orig-stx))
                 (when enum-p-value
                   (raise-syntax-error
                    #f
                    "#:uniform-at-random cannot be used with the #:source keyword"
                    orig-stx))
                 (when in-order?
                   (raise-syntax-error
                    #f
                    "#:in-order cannot be used with the #:source keyword"
                    orig-stx))
                 #`(let-values ([(metafunc/red-rel num-cases) 
                                  #,(cond [(metafunc source-stx)
                                           => (λ (x) #`(values #,x (length (metafunc-proc-cases #,x))))]
                                          [else
                                           #`(let ([r #,(apply-contract #'reduction-relation? source-stx 
                                                                        "#:source argument" (syntax-e form))])
                                               (values r (length (reduction-relation-make-procs r))))])])
                      (check-lhs-pats
                       #,lang
                       metafunc/red-rel
                       property
                       (max 1 (floor (/ att num-cases)))
                       ret
                       'redex-check
                       (and print? show)
                       fix
                       keep-going?
                       #:term-match term-match))]
                [else
                 (cond
                   [in-order?
                    #`(check-one
                       (in-order-generator #,lang `pattern)
                       property att ret (and print? show) (or fix (λ (x) x)) term-match
                       keep-going?)]
                   [(or enum-bound-e enum-p-value)
                    #`(check-one
                       (ith-generator #,lang `pattern #,enum-bound-e #,enum-p-value)
                       property att ret (and print? show) (or fix (λ (x) x)) term-match
                       keep-going?)]
                   [ad-hoc?
                    #`(check-one
                       #,(term-generator lang #'pattern 'redex-check)
                       property att ret (and print? show) fix (and fix term-match)
                       keep-going?)]
                   [else
                    #`(check-one
                       (default-generator #,lang `pattern)
                       property att ret (and print? show) (or fix (λ (x) x)) term-match
                       keep-going?)])])))))))

(define (default-generator lang pat)
  (define ad-hoc-generator ((compile lang 'redex-check) pat))
  (define enum (pat-enumerator (compiled-lang-enum-table lang) pat))
  (cond
    [enum
     (define in-bounds (if (= +inf.0 (enum-size enum))
                           (λ (x) x)
                           (λ (x) (modulo x (enum-size enum)))))
     (define start-time (current-inexact-milliseconds))
     (define interleave-start-attempt #f)
     (define interleave-time (+ start-time (* 1000 10))) ;; 10 seconds later
     (define pure-random-start-attempt #f)
     (define pure-random-time (+ start-time (* 1000 60 10))) ;; 10 minutes later
     (λ (_size _attempt _retries)
       (define now (current-inexact-milliseconds))
       (cond
         [(<= now interleave-time)
          (values (enum-ith enum (in-bounds (- _attempt 1))) 'ignored)]
         [(<= now pure-random-time)
          (unless interleave-start-attempt (set! interleave-start-attempt _attempt))
          (define interleave-attempt (- _attempt interleave-start-attempt))
          (cond
            [(odd? interleave-attempt) 
             (ad-hoc-generator _size (/ (- interleave-attempt 1) 2) _retries)]
            [else
             (define enum-id (in-bounds (+ interleave-start-attempt (/ interleave-attempt 2) -1)))
             (values (enum-ith enum enum-id) 'ignored)])]
         [else
          (unless pure-random-start-attempt (set! pure-random-start-attempt _attempt))
          (ad-hoc-generator _size
                            (+ (- _attempt pure-random-start-attempt)
                               (quotient (- pure-random-start-attempt pure-random-start-attempt)
                                         2))
                            _retries)]))]
    [else
     ad-hoc-generator]))

(define (ith-generator lang pat enum-bound enum-p-value)
  (define enum-lang (compiled-lang-enum-table lang))
  (define enum (pat-enumerator enum-lang pat))
  (unless enum (error 'redex-check "cannot enumerate the pattern ~s" pat))
  (define the-size (enum-size enum))
  (cond
    [enum-bound
     (define bound (if (equal? the-size +inf.0)
                       enum-bound
                       (min enum-bound the-size)))
     (λ (_size _attempt _retries)
       (values (enum-ith enum (random-natural bound))
               'ignored))]
    [else
     (cond
       [(equal? the-size +inf.0)
        (λ (_size _attempt _retries)
          (values (enum-ith enum (pick-an-index enum-p-value))
                  'ignored))]
       [else
        (λ (_size _attempt _retries)
          (values (enum-ith enum (random-natural the-size))
                  'ignored))])]))

(define (in-order-generator lang pat)
  (define enum-lang (compiled-lang-enum-table lang))
  (define enum (pat-enumerator enum-lang pat))
  (unless enum (error 'redex-check "cannot enumerate the pattern ~s" pat))
  (define the-size (enum-size enum))
  (λ (_size _attempt _retries)
    (values (enum-ith enum (if (= +inf.0 the-size)
                               (- _attempt 1)
                               (modulo (- _attempt 1) the-size)))
            'ignored)))

;; pick-an-index : ([0,1] -> Nat) ∩ (-> Nat)
(define (pick-an-index [prob-of-zero 0.01])
  (max (random-natural/no-mean prob-of-zero)
       (random-natural/no-mean prob-of-zero)
       (random-natural/no-mean prob-of-zero)))

;; random-natural/no-mean : [0,1] -> Nat
(define (random-natural/no-mean prob-zero)
  (define x (sample (geometric-dist prob-zero)))
  (define m1 (expt 2 (exact-floor x)))
  (define m0 (quotient m1 2))
  (random-integer m0 m1))

(define-for-syntax (parse-redex-check-kw-args kw-args orig-stx form-name)
  (apply values
         (parse-kw-args (list attempts-keyword
                              source-keyword
                              retries-keyword
                              print?-keyword
                              attempt-size-keyword
                              (prepare-keyword #f)
                              keep-going-keyword)
                        kw-args
                        orig-stx
                        (syntax-e form-name))))

(define (format-attempts a)
  (format "~a attempt~a" a (if (= 1 a) "" "s")))

(define (check-one generator property attempts retries show term-fix term-match keep-going?)
  (define c (check generator property attempts retries show keep-going?
                   #:term-fix term-fix
                   #:term-match term-match))
  (cond 
    [(counterexample? c)
     (unless show c)] ; check printed it
    [show
     (show (format "no counterexamples in ~a\n"
                   (format-attempts attempts)))]
    [else
     #t]))

(define-struct (exn:fail:redex:test exn:fail:redex) (source term))
(define-struct counterexample (term) #:transparent)

(define-struct term-prop (pred))
(define-struct bind-prop (pred))

(define (check generator property attempts retries show keep-going?
               #:source [source #f]
               #:term-fix [term-fix #f]
               #:term-match [term-match #f]
               #:skip-term? [skip-term? (λ (x) #f)])
  (let loop ([remaining attempts])
    (cond 
      [(zero? remaining)
       #t]
      [else
       (define attempt (add1 (- attempts remaining)))
       (define-values (raw-term bindings) (generator ((attempt->size) attempt) attempt retries))
       (cond
         [(gen-fail? raw-term)
          (loop (sub1 remaining))]
         [else
          (define handler 
            (λ (action term)
              (λ (exn)
                (define msg (format "~a ~s raises an exception" action term))
                (when show (show (format "~a\n" msg)))
                (raise 
                 (if show
                     exn
                     (make-exn:fail:redex:test
                      (format "~a:\n~a" msg (exn-message exn))
                      (current-continuation-marks)
                      exn
                      term))))))
          (define term (with-handlers ([exn:fail? (handler "fixing" raw-term)])
                         (if term-fix (term-fix raw-term) raw-term)))
          (cond
            [(skip-term? term) (loop (- remaining 1))]
            [(if term-match
                 (let ([bindings (make-bindings 
                                  (match-bindings
                                   (pick-from-list (term-match term))))])
                   (with-handlers ([exn:fail? (handler "checking" term)])
                     (match property
                       [(term-prop pred) (pred term)]
                       [(bind-prop pred) (pred bindings)])))
                 (with-handlers ([exn:fail? (handler "checking" term)])
                   (match (cons property term-fix)
                     [(cons (term-prop pred) _) (pred term)]
                     [(cons (bind-prop pred) #f) (pred bindings)])))
             (loop (sub1 remaining))]
            [else
             (when show
               (show
                (format "counterexample found after ~a~a:\n"
                        (format-attempts attempt)
                        (if source (format " with ~a" source) "")))
               (pretty-write term (current-output-port)))
             (if keep-going?
                 (loop (sub1 remaining))
                 (make-counterexample term))])])])))

(define (check-lhs-pats lang mf/rr prop attempts retries what show term-fix keep-going?
                        #:term-match [term-match #f])
  (define lang-gen (compile lang what))
  (define-values (pats srcs skip-term?)
    (cond [(metafunc-proc? mf/rr)
           (values (map (λ (case) ((metafunc-case-lhs+ case) lang)) 
                        (metafunc-proc-cases mf/rr))
                   (metafunction-srcs mf/rr)
                   (compose not (metafunc-proc-in-dom? mf/rr)))]
          [(reduction-relation? mf/rr)
           (values (map (λ (rwp) ((rewrite-proc-lhs rwp) lang)) (reduction-relation-make-procs mf/rr))
                   (reduction-relation-srcs mf/rr)
                   (let ([pat (compile-pattern (reduction-relation-lang mf/rr)
                                               (reduction-relation-domain-pat mf/rr)
                                               #f)])
                     (λ (x) (not (match-pattern? pat x)))))]))
  
  (let loop ([pats pats] [srcs srcs])
    (if (and (null? pats) (null? srcs))
        (if show
            (show
             (format "no counterexamples in ~a (with each clause)\n"
                     (format-attempts attempts)))
            #t)
        (let ([c (with-handlers ([exn:fail:redex:generation-failure?
                                  ; Produce an error message that blames the LHS as a whole.
                                  (λ (_)
                                    (raise-gen-fail what (format "LHS of ~a" (car srcs)) retries))])
                   (check
                    (lang-gen (car pats))
                    prop
                    attempts
                    retries
                    show
                    keep-going?
                    #:skip-term? skip-term?
                    #:source (car srcs)
                    #:term-match term-match
                    #:term-fix term-fix))])
          (if (and (not keep-going?) (counterexample? c))
              (unless show c)
              (loop (cdr pats) (cdr srcs)))))))

(define-syntax (check-metafunction stx)
  (syntax-case stx ()
    [(form name property . kw-args)
     (let-values ([(attempts retries print? size fix keep-going?)
                   (apply values
                          (parse-kw-args (list attempts-keyword
                                               retries-keyword
                                               print?-keyword
                                               attempt-size-keyword
                                               (prepare-keyword #t)
                                               keep-going-keyword)
                                         (syntax kw-args)
                                         stx
                                         (syntax-e #'form)))]
                  [(m) (metafunc/err #'name stx)])
       (quasisyntax/loc stx
         (parameterize ([attempt->size #,size])
           (let ([att #,attempts]
                 [ret #,retries]
                 [fix #,fix])
             (check-lhs-pats 
              (metafunc-proc-lang #,m)
              #,m
              (term-prop #,(apply-contract #'(-> (listof any/c) any) #'property #f (syntax-e #'form)))
              att
              ret
              'check-metafunction
              (and #,print? #,(show-message stx))
              fix
              #,keep-going?)))))]))

(define (reduction-relation-srcs r)
  (map (λ (proc) (or (rewrite-proc-name proc)
                     (format "clause at ~a" (rewrite-proc-lhs-src proc))))
       (reduction-relation-make-procs r)))

(define (metafunction-srcs m)
  (map (λ (x) (format "clause at ~a" (metafunc-case-src-loc x)))
       (metafunc-proc-cases m)))

(define-syntax (check-reduction-relation stx)
  (syntax-case stx ()
    [(form relation property . kw-args)
     (let-values ([(attempts retries print? size fix keep-going?)
                   (apply values
                          (parse-kw-args (list attempts-keyword
                                               retries-keyword
                                               print?-keyword
                                               attempt-size-keyword
                                               (prepare-keyword #f)
                                               keep-going-keyword)
                                         (syntax kw-args)
                                         stx
                                         (syntax-e #'form)))])
       (quasisyntax/loc stx
         (parameterize ([attempt->size #,size])
           (let ([att #,attempts]
                 [ret #,retries]
                 [rel #,(apply-contract #'reduction-relation? #'relation #f (syntax-e #'form))]
                 [fix #,fix]
                 [keep-going? #,keep-going?])
             (check-lhs-pats
              (reduction-relation-lang rel)
              rel
              (term-prop #,(apply-contract #'(-> any/c any) #'property #f (syntax-e #'form)))
              att
              ret
              'check-reduction-relation
              (and #,print? #,(show-message stx))
              fix
              keep-going?)))))]))

(define-syntax (generate-term stx)
  (let ([l (cdr (syntax->list stx))])
    (for ([x (in-list l)])
      (define k (syntax-e x))
      (when (keyword? k)
        (unless (member k '(#:satisfying #:source #:attempt-num #:retries #:i-th))
          (raise-syntax-error 'generate-term "unknown keyword" stx x)))))
  
  (define (parse-keyword-args args)
    (define attempt-num #f)
    (define retries #f)
    
    (let loop ([args args])
      (syntax-case args ()
        [() 
         (values (or attempt-num #'1)
                 (or retries #'100))]
        [(#:attempt-num attempt-num-expr . rest)
         (let ()
           (when attempt-num
             (raise-syntax-error 'generate-term 
                                 "found multiple #:attempt-num keywords"
                                 stx
                                 (stx-car args)))
           (set! attempt-num #'(check-keyword-arg attempt-num-expr))
           (loop #'rest))]
        [(#:retries retries-expr . rest)
         (let ()
           (when retries
             (raise-syntax-error 'generate-term 
                                 "found multiple #:retries keywords"
                                 stx
                                 (stx-car args)))
           (set! retries #'(check-keyword-arg retries-expr))
           (loop #'rest))]
        [(x . y)
         (raise-syntax-error 'generate-term
                             "illegal syntax; expected #:attempt-num and #:retries keywords arguments"
                             #'x
                             stx)])))
  
  (define (check-size exp)
    (when (keyword? (syntax-e exp))
      (raise-syntax-error 'generate-term 
                          "expected a size expression"
                          stx
                          exp)))
  
  #`(#%expression
     #,(syntax-case stx (=)
         [(_ lang pat #:i-th i-expr)
          #'(generate-term/ith lang pat i-expr)]
         [(_ lang pat #:i-th)
          #'(generate-term/ith lang pat)]
         [(_ language #:satisfying (mf-id . args) = res)
          #'(generate-term/mf language (mf-id . args) res)]
         [(_ language #:satisfying (mf-id . mf-args) = res size)
          #'(generate-term/mf language (mf-id . mf-args) res size)]
         [(_ language #:satisfying (jf-id . jf-args))
          #`(generate-term/jf language (jf-id . jf-args))]
         [(_ language #:satisfying (jf-id . jf-args) size)
          #`(generate-term/jf language (jf-id . jf-args) size)]
         [(_ #:source metafunction/relation size . args)
          (let ()
            (check-size #'size)
            (define-values (attempt-num retries-expr) (parse-keyword-args #'args))
            #`(generate-term/source metafunction/relation size #,attempt-num #,retries-expr))]
         [(_ #:source metafunction/relation)
          #`(generate-term/source metafunction/relation)]
         [(_ lang pat size . args)
          (let ()
            (check-size #'size)
            (define-values (attempt-num retries-expr) (parse-keyword-args #'args))
            #`(generate-term/lang-pat lang pat size #,attempt-num #,retries-expr))]
         [(_ lang pat)
          #`(generate-term/lang-pat lang pat)])))

(define (check-keyword-arg expr)
  (unless (exact-nonnegative-integer? expr)
    (raise-argument-error 'generate-term
                          "natural number"
                          expr))
  expr)

(define-syntax (generate-term/ith stx)
  (syntax-case stx ()
    [(_ lang pat . rest)
     (with-syntax ([(syncheck-exp pattern (vars ...) (vars/ellipses ...)) 
                    (rewrite-side-conditions/check-errs 
                     #'lang
                     'generate-term #t #'pat)])
       (define fn-stx #'(begin syncheck-exp (generate-ith/proc lang `pattern)))
       (syntax-case #'rest ()
         [() fn-stx]
         [(i-expr) #`(#,fn-stx i-expr)]))]))

(define (generate-ith/proc lang pat)
  (define enum-lang (compiled-lang-enum-table lang))
  (define enum (pat-enumerator enum-lang pat))
  (unless enum (error 'generate-term "cannot enumerate ~s" pat))
  (define the-size (enum-size enum))
  (λ (i)
    (unless (exact-nonnegative-integer? i)
      (raise-argument-error 'generate-term
                            "exact-nonnegative-integer?"
                            i))
    (enum-ith enum (if (equal? the-size +inf.0)
                       i
                       (modulo i the-size)))))

(define-syntax (generate-term/mf stx)
  (syntax-case stx ()
    [(_ language (mf-id . args) res . size-maybe)
     (let ()
       (language-id-nts #'language 'generate-term) ;; for the error side-effect
       (define m (metafunc #'mf-id))
       (unless m (raise-syntax-error 'generate-term "expected a metafuction" #'mf-id))
       (define (body-code size)
         #`(generate-mf-pat language (mf-id . args) res #,size))
       (syntax-case #'size-maybe ()
         [() #`(λ (size) #,(body-code #'size))]
         [(size) (body-code #'size)]))]))

(define-syntax (generate-term/jf stx)
  (syntax-case stx ()
    [(_ language (jf-id . args) . size-maybe)
     (let ()
       (language-id-nts #'language 'generate-term) ;; for the error side-effect
       (unless (judgment-form-id? #'jf-id)
         (raise-syntax-error 'generate-term "expected a judgment-form" #'jf-id))
       (syntax-case #'size-maybe ()
         [() #`(λ (size) (generate-jf-pat language (jf-id . args) size))]
         [(size) #'(generate-jf-pat language (jf-id . args) size)]))]))

(define-syntax (generate-term/source stx)
  (syntax-case stx ()
    [(_ mf/rel . size+args-maybe)
     (let ()
       (define generator-stx
         (cond
           [(metafunc #'mf/rel)
            => (λ (f)
                 #`(make-generate-term/source/mf-generator 'mf/rel #,f))]
           [else
            #`(make-generate-term/source/rr-generator
               #,(apply-contract #'reduction-relation? #'mf/rel
                                 "#:source argument" 'generate-term))]))
       (syntax-case #'size+args-maybe ()
         [() generator-stx]
         [(size attempt-num retries-expr)
          #`(#,generator-stx size #:attempt-num attempt-num #:retries retries-expr)]))]))

(define (make-generate-term/source/mf-generator name f)
  (make-generator
   (let* ([L (metafunc-proc-lang f)]
          [compile-pat (compile L 'generate-term)]
          [cases (metafunc-proc-cases f)])
     (check-cases name cases)
     (map (λ (c) (compile-pat ((metafunc-case-lhs+ c) L))) 
          cases))
   'generate-term))

(define (make-generate-term/source/rr-generator r)
  (unless (reduction-relation? r)
    (raise-argument-error 'generate-term "reduction-relation?" r))
  (make-generator
   (let* ([L (reduction-relation-lang r)]
          [compile-pat (compile L 'orig-name)])
     (map (λ (p) (compile-pat ((rewrite-proc-lhs p) L)))
          (reduction-relation-make-procs r)))
   'generate-term))

(define-syntax (generate-term/lang-pat stx)
  (syntax-case stx ()
    [(_ lang pat . rest)
     (with-syntax ([(syncheck-exp pattern (vars ...) (vars/ellipses ...)) 
                    (rewrite-side-conditions/check-errs 
                     #'lang
                     'generate-term
                     #t #'pat)])
       (define generator-exp
         #`(begin syncheck-exp 
                  (generate-term/lang-pat/proc 
                   #,(term-generator #'lang #'pattern 'generate-term))))
       (syntax-case #'rest ()
         [() generator-exp]
         [(size attempt-num retries-expr)
          #`(#,generator-exp size #:attempt-num attempt-num #:retries retries-expr)]))]))

(define (generate-term/lang-pat/proc tg) (make-generator (list tg) 'generate-term))

(define (check-cases name cases)
  (when (null? cases)
    (raise-gen-fail 'generate-term (format "from ~a metafunction (it has no clauses)" name) 1)))

(define-syntax (generate-mf-pat stx)
  (syntax-case stx ()
    [(g-m-p lang-id (mf-name . lhs-pats) rhs-pat size)
     #`(parameterize ([unsupported-pat-err-name 'generate-term])
         ((make-redex-generator lang-id (mf-name . lhs-pats) = rhs-pat size)))]))

(define-syntax (generate-jf-pat stx)
  (syntax-case stx ()
    [(g-j-p lang-id (jf-name . pat-raw) size)
     #`(parameterize ([unsupported-pat-err-name 'generate-term])
         ((make-redex-generator lang-id (jf-name . pat-raw) size)))]))

(define-syntax (redex-generator stx)
  (syntax-case stx ()
    [(form-name args ...)
     #`(#%expression (make-redex-generator args ...))]))

(define-syntax (make-redex-generator stx)
  (syntax-case stx ()
    [(_ lang-id (jf/mf-id . args) . rest)
     (cond
       [(judgment-form-id? #'jf/mf-id)
        (syntax-case #'rest ()
          [(size)
           (let* ([j-f (lookup-judgment-form-id #'jf/mf-id)]
                  [clauses (judgment-form-gen-clauses j-f)]
                  [nts (definition-nts #'lang-id stx 'redex-generator)]
                  [relation? (judgment-form-relation? j-f)]
                  [args-stx (if relation?
                                (syntax/loc #'args (args))
                                #'args)]) 
             (with-syntax ([(syncheck-exp pat (names ...) (names/ellipses ...))
                            (rewrite-side-conditions/check-errs #'lang-id 'redex-generator #t args-stx)])
               #`(begin
                   syncheck-exp
                   #,(if relation?
                         #`(let ([gen-proc (make-jf-gen/proc 'jf/mf-id #,clauses lang-id 'pat size)])
                             (λ ()
                               (match (gen-proc)
                                 [`(,jf-name (,trms (... ...)))
                                  `(,jf-name ,@trms)]
                                 [#f #f])))
                         #`(make-jf-gen/proc 'jf/mf-id #,clauses lang-id 'pat size)))))]
          [_
           (raise-syntax-error 'redex-generator 
                               "expected an integer depth bound"
                               stx
                               #'rest)])]
       [(metafunc #'jf/mf-id)
        (syntax-case #'rest ()
          [(= res size)
           (and (identifier? #'=)
                (equal? '= (syntax->datum #'=)))
           (let ()
             (define mf (syntax-local-value #'jf/mf-id (λ () #f)))
             (define nts (definition-nts #'lang-id stx 'redex-generator))
             (with-syntax ([(lhs-syncheck-exp lhs-pat (lhs-names ...) (lhs-names/ellipses ...))
                            (rewrite-side-conditions/check-errs #'lang-id (syntax-e #'g-m-p) #t #'args)]
                           [(rhs-syncheck-exp rhs-pat (rhs-names ...) (rhs-names/ellipses ...))
                            (rewrite-side-conditions/check-errs #'lang-id (syntax-e #'g-m-p) #t #'res)]
                           [mf-id (term-fn-get-id mf)])
               #`(begin lhs-syncheck-exp rhs-syncheck-exp (make-mf-gen/proc 'mf-id mf-id lang-id 'lhs-pat 'rhs-pat size))))]
          [_
           (raise-syntax-error 'redex-generator 
                               "expected \"=\" followed by a result pattern and an integer depth bound"
                               stx
                               #'rest)])]
       [else
        (raise-syntax-error 'redex-generator
                            "expected either a metafunction or a judgment-form identifier"
                            stx
                            #'jf/mf-id)])]
    [(_ not-lang-id . rest)
     (not (identifier? #'not-lang-id))
     (raise-syntax-error 'redex-generator
                         "expected an identifier in the language position"
                         stx
                         #'not-lang-id)]))

(define (make-jf-gen/proc jf-id mk-clauses lang pat size)
  (define gen (search/next (mk-clauses) pat size lang))
  (define (termify search-res)
    (cond
      [(not-failed? search-res)
       (define exp (pat->term lang (p*e-p search-res) (p*e-e search-res)))
       (and (not-failed? exp)
            (cons jf-id exp))]
      [else #f]))
  (λ ()
    (parameterize ([current-logger generation-logger])
      (termify (gen)))))

(define (make-mf-gen/proc fn metafunc-proc lang lhs-pat rhs-pat size)
  (define gen (search/next ((metafunc-proc-gen-clauses metafunc-proc))
                           `(list ,lhs-pat ,rhs-pat)
                           size
                           lang))
  (define (termify res)
    (and (not-failed? res)
         (match res
           [(p*e lhs+rhs env)
            (define lhs+rhs-term (pat->term lang lhs+rhs env))
            (and (not-failed? lhs+rhs-term)
                 (match lhs+rhs-term
                   [(list lhs-term rhs-term)
                    `((,fn ,@lhs-term) = ,rhs-term)]))])))
  (λ ()
    (parameterize ([current-logger generation-logger])
      (termify (gen)))))

(provide redex-check
         generate-term
         check-reduction-relation
         check-metafunction
         enable-gen-trace!
         disable-gen-trace!
         last-gen-trace
         get-most-recent-trace
         update-gen-trace!
         exn:fail:redex:generation-failure?
         redex-generator
         (struct-out counterexample)
         (struct-out exn:fail:redex:test)
         pick-an-index)
