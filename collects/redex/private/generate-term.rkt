#lang racket/base
(require "rg.rkt" 
         "jdg-gen.rkt"
         "error.rkt"
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
                     syntax/stx
                     setup/path-to-relative
                     "rewrite-side-conditions.rkt"
                     "term-fn.rkt"
                     "keyword-macros.rkt"))

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
(define-for-syntax (prepare-keyword lists?)
  (list '#:prepare #f 
        (list (if lists? #'(-> list? list?) #'(-> any/c any/c)) 
              "#:prepare argument")))

(define-syntax (redex-check stx)
  (syntax-case stx ()
    [(form lang pat property . kw-args)
     (with-syntax ([(pattern (name ...) (name/ellipses ...))
                    (rewrite-side-conditions/check-errs 
                     (language-id-nts #'lang 'redex-check)
                     'redex-check #t #'pat)]
                   [show (show-message stx)])
     (let-values ([(attempts-stx source-stx retries-stx print?-stx size-stx fix-stx)
                   (apply values
                          (parse-kw-args (list attempts-keyword
                                               source-keyword
                                               retries-keyword
                                               print?-keyword
                                               attempt-size-keyword
                                               (prepare-keyword #f))
                                         (syntax kw-args)
                                         stx
                                         (syntax-e #'form)))])
         (with-syntax ([property (syntax
                                  (bind-prop
                                   (λ (bindings)
                                     (term-let ([name/ellipses (lookup-binding bindings 'name)] ...)
                                               property))))])
           (quasisyntax/loc stx
             (let ([att #,attempts-stx]
                   [ret #,retries-stx]
                   [print? #,print?-stx]
                   [fix #,fix-stx]
                   [term-match (λ (generated)
                                 (cond [(test-match lang pat generated) => values]
                                       [else (redex-error 'redex-check "~s does not match ~s" generated 'pat)]))])
               (parameterize ([attempt->size #,size-stx])
               #,(if source-stx
                     #`(let-values ([(metafunc/red-rel num-cases) 
                                     #,(cond [(metafunc source-stx)
                                              => (λ (x) #`(values #,x (length (metafunc-proc-cases #,x))))]
                                             [else
                                              #`(let ([r #,(apply-contract #'reduction-relation? source-stx 
                                                                           "#:source argument" (syntax-e #'form))])
                                                  (values r (length (reduction-relation-make-procs r))))])])
                         (check-lhs-pats
                          lang
                          metafunc/red-rel
                          property
                          (max 1 (floor (/ att num-cases)))
                          ret
                          'redex-check
                          (and print? show)
                          fix
                          #:term-match term-match))
                     #`(check-one
                        #,(term-generator #'lang #'pattern 'redex-check)
                        property att ret (and print? show) fix (and fix term-match)))))))))]))

(define (format-attempts a)
  (format "~a attempt~a" a (if (= 1 a) "" "s")))

(define (check-one generator property attempts retries show term-fix term-match) 
  (let ([c (check generator property attempts retries show 
                  #:term-fix term-fix
                  #:term-match term-match)])
    (if (counterexample? c)
        (unless show c) ; check printed it
        (if show
            (show (format "no counterexamples in ~a\n"
                          (format-attempts attempts)))
            #t))))

(define-struct (exn:fail:redex:test exn:fail:redex) (source term))
(define-struct counterexample (term) #:transparent)

(define-struct term-prop (pred))
(define-struct bind-prop (pred))

(define (check generator property attempts retries show
               #:source [source #f]
               #:term-fix [term-fix #f]
               #:term-match [term-match #f])
  (let loop ([remaining attempts])
    (if (zero? remaining)
        #t
        (let ([attempt (add1 (- attempts remaining))])
          (let-values ([(term bindings) (generator ((attempt->size) attempt) attempt retries)]
                       [(handler) 
                        (λ (action term)
                          (λ (exn)
                            (let ([msg (format "~a ~s raises an exception" action term)])
                              (when show (show (format "~a\n" msg)))
                              (raise 
                               (if show
                                   exn
                                   (make-exn:fail:redex:test
                                    (format "~a:\n~a" msg (exn-message exn))
                                    (current-continuation-marks)
                                    exn
                                    term))))))])
            (let ([term (with-handlers ([exn:fail? (handler "fixing" term)])
                          (if term-fix (term-fix term) term))])
              (if (if term-match
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
                  (loop (sub1 remaining))
                  (begin
                    (when show
                      (show
                       (format "counterexample found after ~a~a:\n"
                               (format-attempts attempt)
                               (if source (format " with ~a" source) "")))
                      (pretty-write term (current-output-port)))
                    (make-counterexample term)))))))))

(define (check-lhs-pats lang mf/rr prop attempts retries what show term-fix
                        #:term-match [term-match #f])
  (let ([lang-gen (compile lang what)])
    (let-values ([(pats srcs)
                  (cond [(metafunc-proc? mf/rr)
                         (values (map (λ (case) ((metafunc-case-lhs+ case) lang)) 
                                      (metafunc-proc-cases mf/rr))
                                 (metafunction-srcs mf/rr))]
                        [(reduction-relation? mf/rr)
                         (values (map (λ (rwp) ((rewrite-proc-lhs rwp) lang)) (reduction-relation-make-procs mf/rr))
                                 (reduction-relation-srcs mf/rr))])])
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
                        #:source (car srcs)
                        #:term-match term-match
                        #:term-fix term-fix))])
              (if (counterexample? c)
                  (unless show c)
                  (loop (cdr pats) (cdr srcs)))))))))

(define-syntax (check-metafunction stx)
  (syntax-case stx ()
    [(form name property . kw-args)
     (let-values ([(attempts retries print? size fix)
                   (apply values
                          (parse-kw-args (list attempts-keyword
                                               retries-keyword
                                               print?-keyword
                                               attempt-size-keyword
                                               (prepare-keyword #t))
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
              fix)))))]))

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
     (let-values ([(attempts retries print? size fix)
                   (apply values
                          (parse-kw-args (list attempts-keyword
                                               retries-keyword
                                               print?-keyword
                                               attempt-size-keyword
                                               (prepare-keyword #f))
                                         (syntax kw-args)
                                         stx
                                         (syntax-e #'form)))])
       (quasisyntax/loc stx
         (parameterize ([attempt->size #,size])
           (let ([att #,attempts]
                 [ret #,retries]
                 [rel #,(apply-contract #'reduction-relation? #'relation #f (syntax-e #'form))]
                 [fix #,fix])
             (check-lhs-pats
              (reduction-relation-lang rel)
              rel
              (term-prop #,(apply-contract #'(-> any/c any) #'property #f (syntax-e #'form)))
              att
              ret
              'check-reduction-relation
              (and #,print? #,(show-message stx))
              fix)))))]))

(define-syntax (generate-term stx)
  (syntax-case stx ()
    [(form-name args ...)
     #`(#%expression (generate-term/real form-name args ...))]))

(define-syntax (generate-term/real stx)
  (let ([l (cdr (syntax->list stx))])
    (when (list? l)
      (for ([x (in-list l)])
        (define k (syntax-e x))
        (when (keyword? k)
          (unless (member k '(#:satisfying #:source #:attempt-num #:retries))
            (raise-syntax-error 'generate-term "unknown keyword" stx x))))))
  (syntax-case stx ()
    [(_ orig-name language #:satisfying (jf/mf-id . args) . rest)
     (cond
       [(metafunc #'jf/mf-id)
        (let ()
          (define (signal-error whatever)
            (when (stx-pair? whatever)
              (define cr (syntax-e (stx-car whatever)))
              (when (keyword? cr)
                (raise-syntax-error 'generate-term 
                                    "#:satisfying does not yet support additional keyword arguments"
                                    stx 
                                    (stx-car whatever))))
            (raise-syntax-error 'generate-term
                                "expected a metafunction result and a size"
                                stx))
          (let ([body-code 
                 (λ (res size)
                   #`(generate-mf-pat language (jf/mf-id . args) #,res #,size))])
            (syntax-case #'rest (=)
              [(= res) 
               #`(λ (size) 
                   #,(body-code #'res #'size))]
              [(= res size)
               (body-code #'res #'size)]
              [(x . y)
               (or (not (identifier? #'x))
                   (not (free-identifier=? #'= #'x)))
               (raise-syntax-error 'generate-term
                                   "expected to find ="
                                   stx
                                   #'x)]
              [whatever
               (signal-error #'whatever)])))]
       [(judgment-form-id? #'jf/mf-id)
        (syntax-case #'rest ()
          [() 
           #`(λ (size) 
               (generate-jf-pat language (jf/mf-id . args) size))]
          [(size)
           #'(generate-jf-pat language (jf/mf-id . args) size)]
          [(x . y) (raise-syntax-error 'generate-term 
                                       "#:satisfying does not yet support additional keyword arguments"
                                       stx #'x)])]
       [else
        (raise-syntax-error 'generate-term 
                            "expected either a metafunction or a judgment-form identifier"
                            stx
                            #'jf/mf-id)])]
    [(_ orig-name actual-stx ...)
     (let ()
       (define form-name (syntax-e #'orig-name))
       (define-values (raw-generators args)
         (syntax-case #'(actual-stx ...) ()
           [(#:source src . rest)
            (values 
             (cond [(metafunc #'src) 
                    => (λ (f)
                         #`(let* ([f #,f]
                                  [L (metafunc-proc-lang f)]
                                  [compile-pat (compile L '#,form-name)])
                             (map (λ (c) (compile-pat ((metafunc-case-lhs+ c) L))) 
                                  (metafunc-proc-cases f))))]
                   [else
                    #`(let* ([r #,(apply-contract #'reduction-relation?  #'src "#:source argument" form-name)]
                             [L (reduction-relation-lang r)]
                             [compile-pat (compile L '#,form-name)])
                        (map (λ (p) (compile-pat ((rewrite-proc-lhs p) L)))
                             (reduction-relation-make-procs r)))])
             #'rest)]
           [(lang pat . rest)
            (with-syntax ([(pattern (vars ...) (vars/ellipses ...)) 
                           (rewrite-side-conditions/check-errs 
                            (language-id-nts #'lang form-name)
                            form-name #t #'pat)])
              (values #`(list #,(term-generator #'lang #'pattern form-name))
                      #'rest))]))
       (define generator-syntax
         #`(make-generator #,raw-generators '#,form-name))
       (syntax-case args ()
         [()
          generator-syntax]
         [(size . kw-args)
          (quasisyntax/loc stx
            (#,generator-syntax size . kw-args))]))]))

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
             (with-syntax ([(pat (names ...) (names/ellipses ...))
                            (rewrite-side-conditions/check-errs nts 'redex-generator #t args-stx)])
               (if relation?
                   #`(let ([gen-proc (make-jf-gen/proc 'jf/mf-id #,clauses lang-id 'pat size)])
                       (λ ()
                         (match (gen-proc)
                           [`(,jf-name (,trms (... ...)))
                            `(,jf-name ,@trms)]
                           [#f #f])))
                   #`(make-jf-gen/proc 'jf/mf-id #,clauses lang-id 'pat size))))]
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
             (with-syntax ([(lhs-pat (lhs-names ...) (lhs-names/ellipses ...))
                            (rewrite-side-conditions/check-errs nts (syntax-e #'g-m-p) #t #'args)]
                           [(rhs-pat (rhs-names ...) (rhs-names/ellipses ...))
                            (rewrite-side-conditions/check-errs nts (syntax-e #'g-m-p) #t #'res)]
                           [mf-id (term-fn-get-id mf)])
               #`(make-mf-gen/proc 'mf-id mf-id lang-id 'lhs-pat 'rhs-pat size)))]
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
      [search-res
       (define exp (pat->term lang (p*e-p search-res) (p*e-e search-res)))
       (and exp
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
    (and res
         (match res
           [(p*e lhs+rhs env)
            (define lhs+rhs-term (pat->term lang lhs+rhs env))
            (and lhs+rhs-term
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
         (struct-out exn:fail:redex:test))
