#lang racket/base

(require "matcher.rkt"
         "struct.rkt"
         "term.rkt"
         "fresh.rkt"
         "loc-wrapper.rkt"
         "error.rkt"
         (for-syntax "cycle-check.rkt")
         racket/trace
         racket/contract
         racket/list
         mzlib/etc)

(require (for-syntax syntax/name
                     "loc-wrapper-ct.rkt"
                     "rewrite-side-conditions.rkt"
                     "term-fn.rkt"
                     "underscore-allowed.rkt"
                     syntax/boundmap
                     syntax/id-table
                     racket/base
                     racket/list
                     racket/match
                     racket/syntax
                     syntax/parse
                     syntax/parse/experimental/contract
                     syntax/name))

(define (language-nts lang)
  (hash-map (compiled-lang-ht lang) (λ (x y) x)))

(define-for-syntax (prune-syntax stx)
  (datum->syntax
   (identifier-prune-lexical-context #'whatever '(#%app #%datum))
   (let loop ([stx stx])
     (syntax-case stx ()
       [(a . b)
        (datum->syntax (identifier-prune-lexical-context #'whatever '(#%app))
                       (cons (loop #'a) (loop #'b))
                       stx)]
       [x 
        (identifier? #'x)
        (identifier-prune-lexical-context #'x)]
       [() (datum->syntax #f '() stx)]
       [_ (datum->syntax (identifier-prune-lexical-context #'whatever '(#%datum))
                         (syntax->datum stx) stx)]))))

(define-for-syntax (term-matcher orig-stx make-matcher)
  (syntax-case orig-stx ()
    [(form-name lang [pattern rhs] ...)
     (begin
       (unless (identifier? #'lang)
         (raise-syntax-error (syntax-e #'form-name) "expected an identifier in the language position" orig-stx #'lang))
       (let ([lang-nts (language-id-nts #'lang (syntax-e #'form-name))])
         (with-syntax ([((side-conditions-rewritten (names ...) (names/ellipses ...)) ...)
                        (map (λ (x) (rewrite-side-conditions/check-errs lang-nts (syntax-e #'form-name) #t x))
                             (syntax->list (syntax (pattern ...))))]
                       [(cp-x ...) (generate-temporaries #'(pattern ...))]
                       [make-matcher make-matcher])
           #'(make-matcher
              'form-name lang 
              (list 'pattern ...)
              (list (compile-pattern lang `side-conditions-rewritten #t) ...)
              (list (λ (match)
                      (term-let/error-name 
                       form-name
                       ([names/ellipses (lookup-binding (mtch-bindings match) 'names)] ...)
                       rhs)) ...)))))]))

(define-syntax (term-match/single stx)
  (term-matcher stx #'term-match/single/proc))
(define-syntax (term-match stx)
  (term-matcher stx #'term-match/proc))

(define ((term-match/proc form-name lang ps cps rhss) term)
  (append-map
   (λ (cp rhs)
     (let ([matches (match-pattern cp term)])
       (if matches
           (map rhs matches)
           '())))
   cps rhss))

(define ((term-match/single/proc form-name lang ps0 cps rhss) term)
  (let loop ([ps ps0] [cps cps] [rhss rhss])
    (if (null? ps)
        (redex-error form-name 
                     (if (null? (cdr ps0))
                         (format "term ~s does not match pattern ~s" term (car ps0))
                         (format "no patterns matched ~s" term)))
        (let ([match (match-pattern (car cps) term)])
          (if match
              (begin
                (unless (null? (cdr match))
                  (redex-error
                   form-name
                   "pattern ~s matched term ~s multiple ways"
                   (car ps)
                   term))
                ((car rhss) (car match)))
              (loop (cdr ps) (cdr cps) (cdr rhss)))))))

(define-syntaxes (redex-let redex-let*)
  (let ()
    (define-syntax-class binding
      #:description "binding clause"
      (pattern (lhs:expr rhs:expr)))
    (define-syntax-class (bindings extract)
      #:description (if extract
                        "sequence of disjoint binding clauses"
                        "sequence of binding clauses")
      (pattern (b:binding ...)
               #:fail-when (and extract
                                (check-duplicate-identifier
                                 (apply append (map extract (syntax->list #'(b.lhs ...))))))
               "duplicate pattern variable"
               #:with (lhs ...) #'(b.lhs ...)
               #:with (rhs ...) #'(b.rhs ...)))
    
    (define (redex-let stx)
      (define-values (form-name nts)
        (syntax-case stx ()
          [(name lang . _) 
           (values (syntax-e #'name)
                   (language-id-nts #'lang (syntax-e #'name)))]))
      (define (pattern-variables pattern)
        (let-values ([(names _) (extract-names nts form-name #t pattern)])
          names))
      (syntax-parse stx
        [(name lang (~var bs (bindings pattern-variables)) body ...+)
         (with-syntax ([(t ...) (generate-temporaries #'bs)])
           #`(let ([t bs.rhs] ...)
               #,(nested-lets #'lang #'([bs.lhs t] ...) #'(body ...) #'name)))]))
    
    (define (redex-let* stx)
      (syntax-parse stx
        [(name lang (~var bs (bindings #f)) body ...+)
         (nested-lets #'lang #'bs #'(body ...) #'name)]))
    
    (define (nested-lets lang bindings bodies name)
      (syntax-case bindings ()
        [()
         #`(let () #,@bodies)]
        [([lhs rhs] . bindings)
         (with-syntax ([rest-lets (nested-lets lang #'bindings bodies name)])
           #`(#,(term-matcher #`(#,name #,lang [lhs rest-lets]) 
                              #'term-match/single/proc) 
              rhs))]))
    
    (values redex-let redex-let*)))

(define-syntax (compatible-closure stx)
  (syntax-case stx ()
    [(_ red lang nt)
     (identifier? (syntax nt))
     (with-syntax ([(side-conditions-rewritten (names ...) (names/ellipses ...))
                    (rewrite-side-conditions/check-errs (language-id-nts #'lang 'compatible-closure)
                                                        'compatible-closure 
                                                        #t
                                                        (syntax (cross nt)))])
       (syntax (do-context-closure red lang `side-conditions-rewritten 'compatible-closure)))]
    [(_ red lang nt)
     (raise-syntax-error 'compatible-closure "expected a non-terminal as last argument" stx (syntax nt))]))

(define-syntax (context-closure stx)
  (syntax-case stx ()
    [(_ red lang pattern)
     (with-syntax ([(side-conditions-rewritten (names ...) (names/ellipses ...))
                    (rewrite-side-conditions/check-errs (language-id-nts #'lang 'context-closure)
                                                        'context-closure
                                                        #t
                                                        (syntax pattern))])
       (syntax
        (do-context-closure
         red
         lang
         `side-conditions-rewritten
         'context-closure)))]))

(define (do-context-closure red lang pat name)
  (unless (reduction-relation? red)
    (error name "expected <reduction-relation> as first argument, got ~e" red))
  (unless (compiled-lang? lang)
    (error name "expected <lang> as second argument, got ~e" lang))
  (let ([cp (compile-pattern
             lang
             `(in-hole (name ctxt ,pat)
                       (name exp any))
             #f)])
    (build-reduction-relation
     #f
     lang
     (map
      (λ (make-proc)
        (make-rewrite-proc
         (λ (lang)
           (let ([f (make-proc lang)])
             (λ (main-exp exp extend acc)
               (let loop ([ms (or (match-pattern cp exp) '())]
                          [acc acc])
                 (cond
                   [(null? ms) acc]
                   [else
                    (let* ([mtch (car ms)]
                           [bindings (mtch-bindings mtch)])
                      (loop (cdr ms)
                            (f main-exp
                               (lookup-binding bindings 'exp)
                               (λ (x) (extend (plug (lookup-binding bindings 'ctxt) x)))
                               acc)))])))))
         (rewrite-proc-name make-proc)
         (rewrite-proc-lhs make-proc)
         (rewrite-proc-lhs-src make-proc)
         (rewrite-proc-id make-proc)))
      (reduction-relation-make-procs red))
     (reduction-relation-rule-names red)
     (reduction-relation-lws red)
     `any)))

(define-syntax (--> stx) (raise-syntax-error '--> "used outside of reduction-relation"))
(define-syntax (fresh stx) (raise-syntax-error 'fresh "used outside of reduction-relation"))
(define-syntax (with stx) (raise-syntax-error 'with "used outside of reduction-relation"))

(define (apply-reduction-relation/tagged p v)
  (let loop ([procs (reduction-relation-procs p)]
             [acc '()])
    (cond
      [(null? procs) acc]
      [else 
       (loop (cdr procs)
             ((car procs) v acc))])))

(define (apply-reduction-relation/tag-with-names p v) (map cdr (apply-reduction-relation/tagged p v)))
(define (apply-reduction-relation p v) (map caddr (apply-reduction-relation/tagged p v)))

(define-for-syntax (extract-pattern-binds lhs)
  (let loop ([lhs lhs])
    (syntax-case* lhs (name) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
      [(name id expr)
       (identifier? #'id)
       (cons (cons #'id #'expr) (loop #'expr))]
      ;; FIXME: should follow the grammar of patterns!
      [(a . b)
       (append (loop #'a) (loop #'b))]
      [_else null])))

(define-for-syntax (extract-term-let-binds lhs)
  (let loop ([lhs lhs])
    (syntax-case* lhs (term-let) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
      [(term-let ((x e1) ...) e2 ...)
       (append (map cons
                    (syntax->list #'(x ...))
                    (syntax->list #'(e1 ...)))
               (loop #'(e2 ...)))]
      ;; FIXME: should follow the grammar of patterns!
      [(a . b)
       (append (loop #'a) (loop #'b))]
      [_else null])))

(define-syntax (-reduction-relation stx)
  (syntax-case stx ()
    [(_ lang args ...)
     (with-syntax ([orig-stx stx])
       (syntax/loc stx (do-reduction-relation orig-stx reduction-relation empty-reduction-relation #f lang args ...)))]))

(define-syntax (extend-reduction-relation stx)
  (syntax-case stx ()
    [(_ orig-reduction-relation lang args ...)
     (with-syntax ([orig-stx stx])
       (syntax/loc stx (do-reduction-relation orig-stx extend-reduction-relation orig-reduction-relation #t lang args ...)))]))

(define-for-syntax (where-keyword? id)
  (or (free-identifier=? id #'where)
      (free-identifier=? id #'where/hidden)))

(define-for-syntax (split-by-mode xs mode)
  (for/fold ([ins '()] [outs '()])
            ([x (reverse xs)]
             [m (reverse mode)])
            (match m
              ['I (values (cons x ins) outs)]
              ['O (values ins (cons x outs))])))

(define-for-syntax (generate-binding-constraints names names/ellipses bindings syn-err-name)
  (define (id/depth stx)
    (syntax-case stx ()
      [(s (... ...))
       (let ([r (id/depth #'s)])
         (make-id/depth (id/depth-id r) (add1 (id/depth-depth r)) (id/depth-mismatch? r)))]
      [s (make-id/depth #'s 0 #f)]))
  (define temporaries (generate-temporaries names))
  (values
   (for/fold ([cs '()])
     ([n names]
      [w/e names/ellipses]
      [x temporaries])
     (cond [(hash-ref bindings (syntax-e n) #f)
            => (λ (b) 
                 (let ([b-id/depth (id/depth b)]
                       [n-id/depth (id/depth w/e)])
                   (if (= (id/depth-depth b-id/depth) (id/depth-depth n-id/depth))
                       (cons #`(equal? #,x (term #,b)) cs)
                       (raise-ellipsis-depth-error
                        syn-err-name
                        (id/depth-id n-id/depth) (id/depth-depth n-id/depth)
                        (id/depth-id b-id/depth) (id/depth-depth b-id/depth)))))]
           [else cs]))
   temporaries
   (for/fold ([extended bindings])
     ([name names] 
      [w/ellipses names/ellipses])
     (hash-set extended (syntax-e name) w/ellipses))))

(define-for-syntax (ellipsis? stx)
  (and (identifier? stx)
       (free-identifier=? stx (quote-syntax ...))))

;; the withs, freshs, and side-conditions come in backwards order
(define-for-syntax (bind-withs orig-name main lang lang-nts stx where-mode body names w/ellipses)
  (with-disappeared-uses
   (let loop ([stx stx]
              [to-not-be-in main]
              [env (make-immutable-hash
                    (map (λ (x e) (cons (syntax-e x) e))
                         names w/ellipses))])
     (syntax-case stx (fresh judgment-holds)
       [() body]
       [((-where x e) y ...)
        (where-keyword? #'-where)
        (let ()
          (with-syntax ([(side-conditions-rewritten (names ...) (names/ellipses ...))
                         (rewrite-side-conditions/check-errs
                          lang-nts
                          'reduction-relation
                          #t
                          #'x)])
            (define-values (binding-constraints temporaries env+)
              (generate-binding-constraints (syntax->list #'(names ...))
                                            (syntax->list #'(names/ellipses ...))
                                            env
                                            orig-name))
            (with-syntax ([(binding-constraints ...) binding-constraints]
                          [(x ...) temporaries])
              (define rest-body (loop #'(y ...) #`(list x ... #,to-not-be-in) env+))
              #`(#,(case where-mode
                     [(flatten)
                      #'combine-where-results/flatten]
                     [(predicate)
                      #'combine-where-results/predicate]
                     [else (error 'unknown-where-mode "~s" where-mode)])
                 (match-pattern (compile-pattern #,lang `side-conditions-rewritten #t) (term e))
                 (λ (bindings)
                   (let ([x (lookup-binding bindings 'names)] ...)
                     (and binding-constraints ...
                          (term-let ([names/ellipses x] ...) 
                                    #,rest-body))))))))]
       [((-side-condition s ...) y ...)
        (or (free-identifier=? #'-side-condition #'side-condition)
            (free-identifier=? #'-side-condition #'side-condition/hidden))
        #`(and s ... #,(loop #'(y ...) to-not-be-in env))]
       [((fresh x) y ...)
        (identifier? #'x)
        #`(term-let ([x (variable-not-in #,to-not-be-in 'x)]) 
                    #,(loop #'(y ...) #`(list (term x) #,to-not-be-in) env))]
       [((fresh x name) y ...)
        (identifier? #'x)
        #`(term-let ([x (let ([the-name (term name)])
                          (verify-name-ok '#,orig-name the-name)
                          (variable-not-in #,to-not-be-in the-name))])
                    #,(loop #'(y ...) #`(list (term x) #,to-not-be-in) env))]
       [((fresh (y) (x ...)) z ...)
        #`(term-let ([(y #,'...)
                      (variables-not-in #,to-not-be-in 
                                        (map (λ (_ignore_) 'y)
                                             (term (x ...))))])
                    #,(loop #'(z ...) #`(list (term (y #,'...)) #,to-not-be-in) env))]
       [((fresh (y) (x ...) names) z ...)
        #`(term-let ([(y #,'...)
                      (let ([the-names (term names)]
                            [len-counter (term (x ...))])
                        (verify-names-ok '#,orig-name the-names len-counter)
                        (variables-not-in #,to-not-be-in the-names))])
                    #,(loop #'(z ...) #`(list (term (y #,'...)) #,to-not-be-in) env))]
       [((judgment-holds j) . after)
        (loop (cons #'j #'after) to-not-be-in env)]
       [((form-name . pats) . after)
        (judgment-form-id? #'form-name)
        (let*-values ([(premise) (syntax-case stx () [(p . _) #'p])]
                      [(rest-clauses under-ellipsis?)
                       (syntax-case #'after ()
                         [(maybe-ellipsis . more)
                          (ellipsis? #'maybe-ellipsis)
                          (values #'more #t)]
                         [_ (values #'after #f)])]
                      [(judgment-form) (syntax-local-value/record #'form-name (λ (_) #t))]
                      [(mode) (judgment-form-mode judgment-form)]
                      [(judgment-proc) (judgment-form-proc judgment-form)]
                      [(input-template output-pre-pattern) 
                       (let-values ([(in out) (split-by-mode (syntax->list #'pats) mode)])
                         (if under-ellipsis?
                             (let ([ellipsis (syntax/loc premise (... ...))])
                               (values #`(#,in #,ellipsis) #`(#,out #,ellipsis)))
                             (values in out)))]
                      [(output-pattern output-names output-names/ellipses)
                       (with-syntax ([(output names names/ellipses)
                                      (rewrite-side-conditions/check-errs lang-nts orig-name #t output-pre-pattern)])
                         (values #'output
                                 (syntax->list #'names)
                                 (syntax->list #'names/ellipses)))]
                      [(binding-constraints temporaries env+)
                       (generate-binding-constraints output-names output-names/ellipses env orig-name)]
                      [(rest-body) (loop rest-clauses #`(list judgment-output #,to-not-be-in) env+)]
                      [(call)
                       (let ([input (quasisyntax/loc premise (term #,input-template))])
                         (define (make-traced input)
                           (quasisyntax/loc premise
                             (call-judgment-form 'form-name #,judgment-proc '#,mode #,input)))
                         (if under-ellipsis?
                             #`(repeated-premise-outputs #,input (λ (x) #,(make-traced #'x)))
                             (make-traced input)))])
          (with-syntax ([(output-name ...) output-names]
                        [(output-name/ellipsis ...) output-names/ellipses]
                        [(temp ...) temporaries]
                        [(binding-constraint ...) binding-constraints])
            #`(begin
                (void #,(defined-check judgment-proc "judgment form" #:external #'form-name))
                (for/fold ([outputs '()]) ([sub-output #,call])
                  (define mtchs
                    (match-pattern (compile-pattern #,lang `#,output-pattern #t) sub-output))
                  (if mtchs
                      (for/fold ([outputs outputs]) ([mtch mtchs])
                        (let ([temp (lookup-binding (mtch-bindings mtch) 'output-name)] ...)
                          (define mtch-outputs
                            (and binding-constraint ...
                                 (term-let ([output-name/ellipsis temp] ...) 
                                           #,rest-body)))
                          (if mtch-outputs
                              (append mtch-outputs outputs)
                              outputs)))
                      outputs)))))]))))

(define (combine-where-results/flatten mtchs result)
  (and mtchs
       (for/fold ([r '()]) ([m mtchs])
         (let ([s (result (mtch-bindings m))])
           (if s (append s r) r)))))

(define (combine-where-results/predicate mtchs result)
  (and mtchs 
       (for/or ([mtch mtchs])
         (result (mtch-bindings mtch)))))

(define (repeated-premise-outputs inputs premise)
  (if (null? inputs)
      '(())
      (let ([output (premise (car inputs))])
        (if (null? output)
            '()
            (for*/list ([o output] [os (repeated-premise-outputs (cdr inputs) premise)])
                       (cons o os))))))

(define (call-judgment-form form-name form-proc mode input)
  (define traced (current-traced-metafunctions))
  (if (or (eq? 'all traced) (memq form-name traced))
      (let ([outputs #f])
        (define spacers
          (for/fold ([s '()]) ([m mode])
                    (case m [(I) s] [(O) (cons '_ s)])))
        (define (assemble inputs outputs)
          (let loop ([ms mode] [is inputs] [os outputs])
            (if (null? ms)
                '()
                (case (car ms)
                  [(I) (cons (car is) (loop (cdr ms) (cdr is) os))]
                  [(O) (cons (car os) (loop (cdr ms) is (cdr os)))]))))
        (define (wrapped . _)
          (set! outputs (form-proc input))
          (for/list ([output outputs])
            (cons form-name (assemble input output))))
        (apply trace-call form-name wrapped (assemble input spacers))
        outputs)
      (form-proc input)))

(define-for-syntax (name-pattern-lws pat)
  (map (λ (x) (cons (to-lw/proc (car x)) (to-lw/proc (cdr x))))
       (extract-pattern-binds pat)))

(define-for-syntax (check-judgment-arity stx judgment)
  (syntax-case judgment ()
    [(form-name pat ...)
     (judgment-form-id? #'form-name)
     (let ([expected (length (judgment-form-mode (syntax-local-value #'form-name)))]
           [actual (length (syntax->list #'(pat ...)))])
       (unless (= actual expected)
         (raise-syntax-error 
          #f 
          (format "mode specifies a ~a-ary relation but use supplied ~a term~a" 
                  expected actual (if (= actual 1) "" "s"))
          judgment)))]
    [(form-name pat ...)
     (raise-syntax-error #f "expected a judgment form name" stx #'form-name)]))

(define-syntax-set (do-reduction-relation)
  (define (do-reduction-relation/proc stx)
    (syntax-case stx ()
      [(_ orig-stx id orig-reduction-relation allow-zero-rules? lang . w/domain-args)
       (identifier? #'lang)
       (prune-syntax
        (let-values ([(domain-pattern main-arrow args)
                      (parse-keywords #'orig-stx #'id #'w/domain-args)])
          (with-syntax ([(rules ...) (before-with args)]
                        [(shortcuts ...) (after-with args)])
            (with-syntax ([(lws ...) (map rule->lws (syntax->list #'(rules ...)))])
              (reduction-relation/helper 
               #'orig-stx
               (syntax-e #'id)
               #'orig-reduction-relation
               (syntax lang)
               (syntax->list (syntax (rules ...)))
               (syntax->list (syntax (shortcuts ...)))
               #'(list lws ...)
               (syntax-e #'allow-zero-rules?)
               domain-pattern
               main-arrow)))))]
      [(_ orig-stx id orig-reduction-relation allow-zero-rules? lang args ...)
       (raise-syntax-error (syntax-e #'id) 
                           "expected an identifier for the language name"
                           #'lang)]))
  
  (define default-arrow #'-->)
  
  (define (parse-keywords stx id args)
    (let ([domain-contract #'any]
          [main-arrow default-arrow])
      
      ;; ensure no duplicate keywords
      (let ([ht (make-hash)]
            [known-keywords '(#:arrow #:domain)])  ;; #:arrow not yet implemented
        (for-each (λ (kwd/stx)  ;; (not necc a keyword)
                    (let ([kwd (syntax-e kwd/stx)])
                      (when (keyword? kwd)
                        (unless (member kwd known-keywords)
                          (raise-syntax-error (syntax-e id)
                                              "unknown keyword"
                                              stx
                                              kwd/stx))
                        (when (hash-ref ht kwd #f)
                          (raise-syntax-error (syntax-e id)
                                              "duplicate keywords"
                                              stx
                                              kwd/stx
                                              (list (hash-ref ht kwd))))
                        (hash-set! ht kwd kwd/stx))))
                  (syntax->list args)))
      
      (let loop ([args args])
        (syntax-case args ()
          [(#:domain pat args ...)
           (begin (set! domain-contract #'pat)
                  (loop #'(args ...)))]
          [(#:domain)
           (raise-syntax-error (syntax-e id) 
                               "expected a domain after #:domain"
                               stx)]
          [(#:arrow arrow . args)
           (identifier? #'arrow)
           (begin (set! main-arrow #'arrow)
                  (loop #'args))]
          [(#:arrow arrow . args)
           (raise-syntax-error (syntax-e id) 
                               "expected an arrow after #:arrow, not a compound expression"
                               stx
                               #'arrow)]
          [(#:arrow)
           (raise-syntax-error (syntax-e id) 
                               "expected an arrow after #:arrow"
                               stx)]
          [_
           (begin
             (values domain-contract main-arrow args))]))))

  
  (define (before-with stx)
    (let loop ([lst (syntax->list stx)])
      (cond
        [(null? lst) null]
        [else
         (let ([fst (car lst)])
           (syntax-case (car lst) (with)
             [with null]
             [else (cons (car lst) (loop (cdr lst)))]))])))
  
  (define (after-with stx)
    (let loop ([lst (syntax->list stx)])
      (cond
        [(null? lst) null]
        [else
         (let ([fst (car lst)])
           (syntax-case (car lst) (with)
             [with (cdr lst)]
             [else (loop (cdr lst))]))])))
  
  (define (name-pattern-lws/rr pat)
    (for/list ([lw-pair (name-pattern-lws pat)])
      (match lw-pair
        [(cons l r) #`(cons #,l #,r)])))
  
  (define (rule->lws rule)
    (syntax-case rule ()
      [(arrow lhs rhs stuff ...)
       (let-values ([(label computed-label scs/withs fvars)
                     (let loop ([stuffs (syntax->list #'(stuff ...))]
                                [label #f]
                                [computed-label #f]
                                [scs/withs null]
                                [fvars null])
                       (cond
                         [(null? stuffs) (values label computed-label (reverse scs/withs) (reverse fvars))]
                         [else
                          (syntax-case (car stuffs) (where where/hidden
                                                           side-condition side-condition/hidden
                                                           fresh variable-not-in
                                                           computed-name
                                                           judgment-holds)
                            [(fresh xs ...) 
                             (loop (cdr stuffs)
                                   label
                                   computed-label
                                   scs/withs
                                   (append 
                                    (reverse (map (λ (x)
                                                    (to-lw/proc
                                                     (syntax-case x ()
                                                       [x
                                                        (identifier? #'x)
                                                        #'x]
                                                       [(x whatever)
                                                        (identifier? #'x)
                                                        #'x]
                                                       [((y dots) (x dots2))
                                                        (datum->syntax 
                                                         #f 
                                                         `(,(syntax->datum #'y) ...) 
                                                         #'y)]
                                                       [((y dots) (x dots2) whatever)
                                                        (datum->syntax 
                                                         #f 
                                                         `(,(syntax->datum #'y) ...) 
                                                         #'y)])))
                                                  (syntax->list #'(xs ...))))
                                    fvars))]
                            [(where x e)
                             (loop (cdr stuffs)
                                   label
                                   computed-label
                                   (cons #`(cons #,(to-lw/proc #'x) #,(to-lw/proc #'e))
                                         (append (name-pattern-lws/rr #'x) scs/withs))
                                   fvars)]
                            [(where/hidden x e)
                             (loop (cdr stuffs) label computed-label scs/withs fvars)]
                            [(side-condition sc)
                             (loop (cdr stuffs)
                                   label
                                   computed-label
                                   (cons (to-lw/uq/proc #'sc) scs/withs)
                                   fvars)]
                            [(side-condition/hidden sc)
                             (loop (cdr stuffs) label computed-label scs/withs fvars)]
                            [x
                             (identifier? #'x)
                             (loop (cdr stuffs)
                                   #''x
                                   computed-label
                                   scs/withs
                                   fvars)]
                            [x
                             (string? (syntax-e #'x))
                             (loop (cdr stuffs)
                                   #'(string->symbol x)
                                   computed-label
                                   scs/withs
                                   fvars)]
                            [(computed-name e)
                             (loop (cdr stuffs)
                                   label
                                   #'e
                                   scs/withs
                                   fvars)]
                            [(judgment-holds (form-name . pieces))
                             (judgment-form-id? #'form-name)
                             (loop (cdr stuffs)
                                   label
                                   computed-label
                                   (let*-values ([(mode) (judgment-form-mode (syntax-local-value #'form-name))]
                                                 [(_ outs) (split-by-mode (syntax->list #'pieces) mode)])
                                     (cons (to-lw/proc #'(form-name . pieces))
                                           (for/fold ([binds scs/withs]) ([out outs])
                                             (append (name-pattern-lws/rr out) binds))))
                                   fvars)]
                            [_
                             ;; just skip over junk here, and expect a syntax error to be raised elsewhere
                             (loop (cdr stuffs) label computed-label scs/withs fvars)])]))])
         (with-syntax ([(scs/withs ...) scs/withs]
                       [(fvars ...) fvars]
                       [((bind-id . bind-pat) ...) 
                        (extract-pattern-binds #'lhs)]
                       [((tl-id . tl-pat) ...)
                       (extract-term-let-binds #'rhs)])
           #`(make-rule-pict 'arrow
                             #,(to-lw/proc #'lhs)
                             #,(to-lw/proc #'rhs)
                             #,label
                             #,(and computed-label 
                                    (to-lw/proc #`,#,computed-label))
                             (list scs/withs ...
                                   #,@(map (λ (bind-id bind-pat)
                                             #`(cons #,(to-lw/proc bind-id)
                                                     #,(to-lw/proc bind-pat)))
                                           (syntax->list #'(bind-id ...))
                                           (syntax->list #'(bind-pat ...)))
                                   #,@(map (λ (tl-id tl-pat)
                                             #`(cons #,(to-lw/proc tl-id)
                                                     #,(to-lw/uq/proc tl-pat)))
                                           (syntax->list #'(tl-id ...))
                                           (syntax->list #'(tl-pat ...))))
                             (list fvars ...))))]))
  
  (define (reduction-relation/helper stx orig-name orig-red-expr lang-id rules shortcuts 
                                     lws 
                                     allow-zero-rules?
                                     domain-pattern
                                     main-arrow)
    (let ([ht (make-module-identifier-mapping)]
          [all-top-levels '()]
          [withs (make-module-identifier-mapping)])
      (for-each (λ (shortcut)
                  (syntax-case shortcut ()
                    [((rhs-arrow rhs-from rhs-to)
                      (lhs-arrow a b))
                     (not (identifier? #'a))
                     (raise-syntax-error
                      orig-name
                      "malformed shortcut, expected identifier"
                      shortcut #'a)]
                    [((rhs-arrow rhs-from rhs-to)
                      (lhs-arrow a b))
                     (not (identifier? #'b))
                     (raise-syntax-error
                      orig-name
                      "malformed shortcut, expected identifier"
                      shortcut #'b)]
                    [((rhs-arrow rhs-from rhs-to)
                      (lhs-arrow lhs-from lhs-to))
                     (begin
                       (table-cons! withs #'lhs-arrow #'rhs-arrow)
                       (table-cons! ht (syntax rhs-arrow) shortcut))]
                    [((a b c) d)
                     (raise-syntax-error 
                      orig-name
                      "malformed shortcut, expected right-hand side to have three sub-expressions"
                      stx (syntax d))]
                    [(a b)
                     (raise-syntax-error 
                      orig-name
                      "malformed shortcut, expected left-hand side to have three sub-expressions"
                      stx (syntax a))]
                    [(a b c d ...)
                     (raise-syntax-error orig-name
                                         "malformed shortcut, expected only two subparts for a shortcut definition, found an extra one"
                                         stx
                                         (syntax c))]
                    [_ (raise-syntax-error orig-name
                                           "malformed shortcut"
                                           stx shortcut)]))
                shortcuts)
            
      (for-each (λ (rule)
                  (syntax-case rule ()
                    [(arrow . rst)
                     (begin
                       (set! all-top-levels (cons #'arrow all-top-levels))
                       (table-cons! ht (syntax arrow) rule))]))
                rules)
      
      ;; signal a syntax error if there are shortcuts defined, but no rules that use them
      (unless (null? shortcuts)
        (unless (module-identifier-mapping-get ht main-arrow (λ () #f))
          (raise-syntax-error orig-name 
                              (format "no ~a rules" (syntax-e main-arrow))
                              stx)))
      
      (for-each (λ (tl)
                  (let loop ([id tl])
                    (unless (free-identifier=? main-arrow id)
                      (let ([nexts
                             (module-identifier-mapping-get
                              withs id 
                              (λ () 
                                (raise-syntax-error 
                                 orig-name
                                 (format "the ~s relation is not defined"
                                         (syntax->datum id))
                                 stx
                                 id)))])
                        (for-each loop nexts)))))
                all-top-levels)
      
      (let ([name-table (make-hasheq)]
            [lang-nts (language-id-nts lang-id orig-name)])
        (hash-set! name-table #f 0)
        ;; name table maps symbols for the rule names to their syntax objects and to a counter indicating what
        ;; order the names were encountered in. The current value of the counter is stored in the table at key '#f'.
        (with-syntax ([lang-id lang-id]
                      [(top-level ...) (get-choices stx orig-name ht lang-id main-arrow
                                                    name-table lang-id allow-zero-rules?)]
                      [(rule-names ...) 
                       (begin
                         (hash-remove! name-table #f)
                         (map car (sort (hash-map name-table (λ (k v) (list k (list-ref v 1)))) < #:key cadr)))]
                      [lws lws]
                      
                      [(domain-pattern-side-conditions-rewritten (names ...) (names/ellipses ...))
                       (rewrite-side-conditions/check-errs
                        lang-nts
                        orig-name
                        #f
                        domain-pattern)])
                      
          #`(build-reduction-relation
             #,orig-red-expr
             lang-id
             (list top-level ...)
             '(rule-names ...)
             lws
             `domain-pattern-side-conditions-rewritten)))))
  
  #|    
    ;; relation-tree = 
    ;;   leaf
    ;;  (make-node id[frm] pat[frm] id[to] pat[to] (listof relation-tree))
    (define-struct node (frm-id frm-pat to-id to-pat))
    (define-struct leaf (frm-pat to-pat))
  |#  
  ;; get-choices : stx[original-syntax-object] bm lang identifier ht[sym->syntax] identifier[language-name] -> (listof relation-tree)
  (define (get-choices stx orig-name bm lang id name-table lang-id allow-zero-rules?)
    (reverse
     (apply 
      append
      (map (λ (x) (get-tree stx orig-name bm lang x name-table lang-id allow-zero-rules?))
           (module-identifier-mapping-get 
            bm id
            (λ ()
              (if allow-zero-rules?
                  '()
                  (raise-syntax-error orig-name 
                                      (format "no rules use ~a" (syntax->datum id))
                                      stx 
                                      (if (equal? id default-arrow) #f id)))))))))
  
  (define (get-tree stx orig-name bm lang case-stx name-table lang-id allow-zero-rules?)
    (syntax-case case-stx ()
      [(arrow from to extras ...)
       (list (do-leaf stx 
                      orig-name
                      lang 
                      name-table
                      (syntax from) 
                      (syntax to) 
                      (syntax->list (syntax (extras ...)))
                      lang-id))]
      [((rhs-arrow rhs-from rhs-to) (lhs-arrow lhs-frm-id lhs-to-id))
       (let* ([lang-nts (language-id-nts lang-id orig-name)]
              [rewrite-side-conds
               (λ (pat) (rewrite-side-conditions/check-errs lang-nts orig-name #t pat))])
         (with-syntax ([(side-conditions-rewritten (names ...) (names/ellipses ...))
                        (rewrite-side-conds
                         (rewrite-node-pat (syntax-e (syntax lhs-frm-id))
                                           (syntax rhs-from)))]
                       [(fresh-rhs-from (fresh-names ...) (fresh-names/ellipses ...)) 
                        (rewrite-side-conds 
                         (freshen-names #'rhs-from #'lhs-frm-id lang-nts orig-name))]
                       [lang lang])
           (map
            (λ (child-proc)
              #`(do-node-match
                 'lhs-frm-id
                 'lhs-to-id
                 `side-conditions-rewritten
                 (λ (bindings rhs-binder)
                   (term-let ([lhs-to-id rhs-binder]
                              [names/ellipses (lookup-binding bindings 'names)] ...)
                             (term rhs-to)))
                 #,child-proc
                 `fresh-rhs-from))
            (get-choices stx orig-name bm #'lang
                         (syntax lhs-arrow) 
                         name-table lang-id 
                         allow-zero-rules?))))]))
  (define (rewrite-node-pat id term)
    (let loop ([t term])
      (syntax-case t (side-condition)
        [(side-condition p c)
         #`(side-condition #,(loop #'p) c)]
        [(p ...)
         (map loop (syntax->list #'(p ...)))]
        [else 
         (if (and (identifier? t) (eq? id (syntax-e t)))
             `(name ,id any)
             t)])))
  
  (define (freshen-names pat hole-id nts what)
    (define (fresh x)
      (gensym
       (if (or (memq x nts) (memq x underscore-allowed))
           (string-append (symbol->string x) "_")
           x)))
    (let-values ([(bound _) (extract-names nts what #t pat 'binds-anywhere)])
      (let ([renames (make-bound-identifier-mapping)])
        (for-each 
         (λ (x)
           (unless (bound-identifier=? x hole-id)
             (bound-identifier-mapping-put! renames x (fresh (syntax-e x)))))
         bound)
        (let recur ([p pat])
          (syntax-case p (side-condition)
            [(side-condition p c)
             #`(side-condition 
                #,(recur #'p)
                (term-let (#,@(bound-identifier-mapping-map renames (λ (x y) #`(#,x (term #,y)))))
                          c))]
            [(p ...)
             #`(#,@(map recur (syntax->list #'(p ...))))]
            [else
             (if (identifier? p)
                 (bound-identifier-mapping-get renames p (λ () p))
                 p)])))))
  
  (define (do-leaf stx orig-name lang name-table from to extras lang-id)
    (let* ([lang-nts (language-id-nts lang-id orig-name)]
           [rw-sc (λ (pat) (rewrite-side-conditions/check-errs lang-nts orig-name #t pat))])
      (let-values ([(name computed-name sides/withs/freshs) (process-extras stx orig-name name-table extras)])
        (with-syntax ([(side-conditions-rewritten (names ...) (names/ellipses ...)) (rw-sc from)])
          (define body-code
            (bind-withs orig-name 
                        #'main-exp
                        lang
                        lang-nts
                        sides/withs/freshs
                        'flatten
                        #`(list (cons #,(or computed-name #'none)
                                      (term #,to)))
                        (syntax->list #'(names ...))
                        (syntax->list #'(names/ellipses ...))))
          (define test-case-body-code
            ;; this contains some redundant code
            (bind-withs orig-name
                        #'#t 
                        #'lang-id2
                        lang-nts
                        sides/withs/freshs
                        'predicate
                        #'#t
                        (syntax->list #'(names ...))
                        (syntax->list #'(names/ellipses ...))))
          (with-syntax ([(lhs-w/extras (w/extras-names ...) (w/extras-names/ellipses ...))
                         (rw-sc #`(side-condition #,from #,test-case-body-code))]
                        [lhs-source (format "~a:~a:~a"
                                            (syntax-source from)
                                            (syntax-line from)
                                            (syntax-column from))]
                        [name name]
                        [lang lang]
                        [body-code body-code])
            #`
            (build-rewrite-proc/leaf `side-conditions-rewritten
                                     (λ (main-exp bindings)
                                       (term-let ([names/ellipses (lookup-binding bindings 'names)] ...)
                                                 body-code))
                                     lhs-source
                                     name
                                     (λ (lang-id2) `lhs-w/extras)))))))
  
  (define (process-extras stx orig-name name-table extras)
    (let* ([the-name #f]
           [the-name-stx #f]
           [computed-name-stx #f]
           [sides/withs/freshs 
            (let loop ([extras extras])
              (cond
                [(null? extras) '()]
                [else
                 (syntax-case (car extras) (fresh computed-name judgment-holds)
                   [name 
                    (or (identifier? (car extras))
                        (string? (syntax-e (car extras))))
                    (begin
                      (let* ([raw-name (syntax-e (car extras))]
                             [name-sym
                              (if (symbol? raw-name)
                                  raw-name
                                  (string->symbol raw-name))])
                        (when (hash-ref name-table name-sym #f)
                          (raise-syntax-errors orig-name 
                                               "same name on multiple rules"
                                               stx
                                               (list (car (hash-ref name-table name-sym))
                                                     (syntax name))))
                        (let ([num (hash-ref name-table #f)])
                          (hash-set! name-table #f (+ num 1))
                          (hash-set! name-table name-sym (list (syntax name) num)))
                        
                        (when the-name
                          (raise-syntax-errors orig-name
                                               "expected only a single name" 
                                               stx
                                               (list the-name-stx (car extras))))
                        (set! the-name (if (symbol? raw-name)
                                           (symbol->string raw-name)
                                           raw-name))
                        (set! the-name-stx (car extras))
                        (loop (cdr extras))))]
                   [(fresh var ...)
                    (append (map (λ (x)
                                   (syntax-case x ()
                                     [x
                                      (identifier? #'x)
                                      #'(fresh x)]
                                     [(x name)
                                      (identifier? #'x)
                                      #'(fresh x name)]
                                     [((ys dots2) (xs dots1))
                                      (and (eq? (syntax-e #'dots1) (string->symbol "..."))
                                           (eq? (syntax-e #'dots2) (string->symbol "...")))
                                      #'(fresh (ys) (xs dots1))]
                                     [((ys dots2) (xs dots1) names)
                                      (and (eq? (syntax-e #'dots1) (string->symbol "..."))
                                           (eq? (syntax-e #'dots2) (string->symbol "...")))
                                      #'(fresh (ys) (xs dots1) names)]
                                     [x
                                      (raise-syntax-error orig-name 
                                                          "malformed fresh variable clause"
                                                          stx
                                                          #'x)]))
                                 (syntax->list #'(var ...)))
                            (loop (cdr extras)))]
                   [(-side-condition exp ...)
                    (or (free-identifier=? #'-side-condition #'side-condition)
                        (free-identifier=? #'-side-condition #'side-condition/hidden))
                    (cons (car extras) (loop (cdr extras)))]
                   [(-where x e)
                    (where-keyword? #'-where)
                    (cons (car extras) (loop (cdr extras)))]
                   [(-where . x)
                    (where-keyword? #'-where)
                    (raise-syntax-error orig-name "malformed where clause" stx (car extras))]
                   [(computed-name e)
                    (if computed-name-stx
                        (raise-syntax-errors orig-name "expected at most one computed-name clause"
                                             stx (list computed-name-stx #'e))
                        (set! computed-name-stx #'e))
                    (loop (cdr extras))]
                   [(computed-name . _)
                    (raise-syntax-error orig-name "malformed computed-name clause" stx (car extras))]
                   [(judgment-holds judgment)
                    (begin
                      (check-judgment-arity stx #'judgment)
                      (cons #'judgment (loop (cdr extras))))]
                   [_
                    (raise-syntax-error orig-name "unknown extra" stx (car extras))])]))])
      (values the-name computed-name-stx sides/withs/freshs)))
  
  ;; table-cons! hash-table sym any -> void
  ;; extends ht at key by `cons'ing hd onto whatever is alrady bound to key (or the empty list, if nothing is)
  (define (table-cons! ht key hd)
    (module-identifier-mapping-put! ht key (cons hd (module-identifier-mapping-get ht key (λ () '())))))
  
  (define (raise-syntax-errors sym str stx stxs)
    (raise (make-exn:fail:syntax 
            (string->immutable-string (format "~a: ~a~a" 
                                              sym 
                                              str
                                              (if (error-print-source-location)
                                                  (string-append ":" (stxs->list stxs))
                                                  "")))
            (current-continuation-marks)
            stxs)))
  
  (define (stxs->list stxs)
    (apply
     string-append
     (let loop ([stxs stxs])
       (cond
         [(null? stxs) '()]
         [else 
          (cons (format " ~s" (syntax->datum (car stxs)))
                (loop (cdr stxs)))])))))

(define (build-rewrite-proc/leaf side-conditions-rewritten 
                                 build-really-matched 
                                 lhs-source
                                 name
                                 lhs-w/extras-proc)
  (let ([case-id (gensym)])
    (make-rewrite-proc
     (λ (lang-id)
       (let ([cp (compile-pattern lang-id side-conditions-rewritten #t)])
         (λ (main-exp exp f other-matches)
           (let ([mtchs (match-pattern cp exp)])
             (if mtchs
                 (let loop ([mtchs mtchs]
                            [acc other-matches])
                   (cond
                     [(null? mtchs) acc]
                     [else 
                      (let* ([mtch (car mtchs)]
                             [bindings (mtch-bindings mtch)]
                             [really-matched (build-really-matched main-exp bindings)])
                        (cond
                          [really-matched
                           (for-each
                            (λ (c)
                              (let ([r (coverage-relation c)])
                                (when (and (reduction-relation? r)
                                           (memf (λ (r) (eq? case-id (rewrite-proc-id r)))
                                                 (reduction-relation-make-procs r)))
                                  (cover-case case-id c))))
                            (relation-coverage))
                           (loop (cdr mtchs) 
                                 (map/mt (λ (x) (list name
                                                      (if (none? (car x)) 
                                                          name
                                                          (format "~a" (car x)))
                                                      (f (cdr x)))) 
                                         really-matched acc))]
                          [else 
                           (loop (cdr mtchs) acc)]))]))
                 other-matches)))))
     name
     lhs-w/extras-proc
     lhs-source
     case-id)))

(define (substitute from to pat)
  (let recur ([p pat])
    (syntax-case p (side-condition)
      [(side-condition p c)
       #`(side-condition #,(recur #'p) c)]
      [(p ...)
       #`(#,@(map recur (syntax->list #'(p ...))))]
      [else
       (if (and (identifier? p) (bound-identifier=? p from))
           to
           p)])))

(define (verify-name-ok orig-name the-name)
  (unless (symbol? the-name)
    (error orig-name "expected a single name, got ~s" the-name)))

(define (verify-names-ok orig-name the-names len-counter)
  (unless (and (list? the-names)
               (andmap symbol? the-names))
    (error orig-name
           "expected a sequence of names, got ~s"
           the-names))
  (unless (= (length len-counter)
             (length the-names))
    (error orig-name
           "expected the length of the sequence of names to be ~a, got ~s"
           (length len-counter)
           the-names)))

(define (union-reduction-relations fst snd . rst)
  (let ([name-ht (make-hasheq)]
        [counter 0]
        [lst (list* fst snd rst)]
        [first-lang (reduction-relation-lang fst)])
    (for-each
     (λ (red)
       (unless (eq? first-lang (reduction-relation-lang red))
         (error 'union-reduction-relations 
                "expected all of the reduction relations to use the same language"))
       (for-each (λ (name)
                   (when (hash-ref name-ht name #f)
                     (error 'union-reduction-relations "multiple rules with the name ~s" name))
                   (hash-set! name-ht name counter)
                   (set! counter (+ counter 1)))
                 (reduction-relation-rule-names red)))
     (reverse lst)) ;; reverse here so the names get put into the hash in the proper (backwards) order
    (make-reduction-relation
     first-lang
     (reverse (apply append (map reduction-relation-make-procs lst)))
     (map car (sort (hash-map name-ht list) < #:key cadr))
     (apply append (map reduction-relation-lws lst)) 
     (reverse (apply append (map reduction-relation-procs lst))))))

(define (do-node-match lhs-frm-id lhs-to-id pat rhs-proc child-make-proc rhs-from)
  (define (subst from to in)
    (let recur ([p in])
      (cond [(eq? from p) to]
            [(pair? p) (map recur p)]
            [else p])))
  ;; need call to make-rewrite-proc
  ;; also need a test case here to check duplication of names.
  (make-rewrite-proc
   (λ (lang)
     (let ([cp (compile-pattern lang pat #t)]
           [child-proc (child-make-proc lang)])
       (λ (main-exp exp f other-matches)
         (let ([mtchs (match-pattern cp exp)])
           (if mtchs
               (let o-loop ([mtchs mtchs]
                            [acc other-matches])
                 (cond
                   [(null? mtchs) acc]
                   [else
                    (let ([sub-exp (lookup-binding (mtch-bindings (car mtchs)) lhs-frm-id)])
                      (o-loop (cdr mtchs) 
                              (child-proc main-exp
                                          sub-exp
                                          (λ (x) (f (rhs-proc (mtch-bindings (car mtchs)) x)))
                                          acc)))]))
               other-matches)))))
   (rewrite-proc-name child-make-proc)
   (λ (lang) (subst lhs-frm-id ((rewrite-proc-lhs child-make-proc) lang) rhs-from))
   (rewrite-proc-lhs-src child-make-proc)
   (rewrite-proc-id child-make-proc)))

(define relation-coverage (make-parameter null))

(define (cover-case id cov)
  (hash-update! (coverage-counts cov) id 
                (λ (c) (cons (car c) (add1 (cdr c))))))

(define (covered-cases cov)
  (hash-map (coverage-counts cov) (λ (k v) v)))

(define-struct coverage (relation counts))

(define-syntax (fresh-coverage stx)
  (syntax-case stx ()
    [(name subj-stx)
     (with-syntax ([subj
                    (cond [(and (identifier? (syntax subj-stx))
                                (let ([tf (syntax-local-value (syntax subj-stx) (λ () #f))])
                                  (and (term-fn? tf) (term-fn-get-id tf))))
                           => values]
                          [else (syntax (let ([r subj-stx])
                                          (if (reduction-relation? r)
                                              r
                                              (raise-type-error 'name "reduction-relation" r))))])])
       (syntax
        (let ([h (make-hasheq)])
          (cond [(metafunc-proc? subj)
                 (for-each
                  (λ (c) (hash-set! h (metafunc-case-id c) (cons (metafunc-case-src-loc c) 0)))
                  (metafunc-proc-cases subj))]
                [(reduction-relation? subj)
                 (for-each 
                  (λ (rwp) 
                    (hash-set! h (rewrite-proc-id rwp) (cons (or (rewrite-proc-name rwp) (rewrite-proc-lhs-src rwp)) 0)))
                  (reduction-relation-make-procs subj))])
          (make-coverage subj h))))]))

(define-syntax (test-match stx)
  (syntax-case stx ()
    [(form-name lang-exp pattern)
     (identifier? #'lang-exp)
     (let*-values ([(what) (syntax-e #'form-name)]
                   [(nts) (language-id-nts #'lang-exp what)])
       (with-syntax ([(side-condition-rewritten (vars ...) (ids/depths ...))
                      (rewrite-side-conditions/check-errs nts what #t #'pattern)])
         (with-syntax ([binders (map syntax-e (syntax->list #'(vars ...)))]
                       [name (syntax-local-infer-name stx)])
           (syntax 
            (do-test-match lang-exp `side-condition-rewritten 'binders 'name)))))]
    [(form-name lang-exp pattern expression)
     (identifier? #'lang-exp)
     (syntax 
      ((form-name lang-exp pattern) expression))]
    [(_ a b c)
     (raise-syntax-error 'redex-match "expected an identifier (bound to a language) as first argument" stx #'a)]
    [(_ a b)
     (raise-syntax-error 'redex-match "expected an identifier (bound to a language) as first argument" stx #'a)]))

(define-struct match (bindings) #:inspector #f)

(define (do-test-match lang pat binders context-name)
  (unless (compiled-lang? lang)
    (error 'redex-match "expected first argument to be a language, got ~e" lang))
  (define name (or context-name
                   (and (symbol? pat)
                        pat)))
  (define cpat (compile-pattern lang pat #t))
  (define redex-match-proc
    (λ (exp)
      (let ([ans (match-pattern cpat exp)])
        (and ans
             (map (λ (m) (make-match (sort-bindings 
                                      (filter (λ (x) (memq (bind-name x) binders))
                                              (bindings-table (mtch-bindings m))))))
                  ans)))))
  (if name
      (procedure-rename redex-match-proc name)
      redex-match-proc))

(define (sort-bindings bnds)
  (sort
   bnds
   (λ (x y) (string-ci<=? (symbol->string (bind-name x))
                          (symbol->string (bind-name y))))))

(define-values (struct:metafunc-proc make-metafunc-proc metafunc-proc? metafunc-proc-ref metafunc-proc-set!)
  (make-struct-type 'metafunc-proc #f 9 0 #f null (current-inspector) 0))
(define metafunc-proc-pict-info (make-struct-field-accessor metafunc-proc-ref 1))
(define metafunc-proc-lang (make-struct-field-accessor metafunc-proc-ref 2))
(define metafunc-proc-multi-arg? (make-struct-field-accessor metafunc-proc-ref 3))
(define metafunc-proc-name (make-struct-field-accessor metafunc-proc-ref 4))
(define metafunc-proc-in-dom? (make-struct-field-accessor metafunc-proc-ref 5))
(define metafunc-proc-dom-pat (make-struct-field-accessor metafunc-proc-ref 6))
(define metafunc-proc-cases (make-struct-field-accessor metafunc-proc-ref 7))
(define metafunc-proc-relation? (make-struct-field-accessor metafunc-proc-ref 8))

(define-struct metafunction (proc))

(define-struct metafunc-case (lhs rhs lhs+ src-loc id))

;; Intermediate structures recording clause "extras" for typesetting.
(define-struct metafunc-extra-side-cond (expr))
(define-struct metafunc-extra-where (lhs rhs))
(define-struct metafunc-extra-fresh (vars))

(define-syntax (in-domain? stx)
  (syntax-case stx ()
    [(_ (name exp ...))
     (begin
       (unless (identifier? #'name)
         (raise-syntax-error #f "expected an identifier" stx #'name))
       #'(in-domain?/proc (metafunction-form name) (term (exp ...))))]))

(define (in-domain?/proc mf exp)
  (let ([mp (metafunction-proc mf)])
    ((metafunc-proc-in-dom? mp)
     exp)))

(define-for-syntax (definition-nts lang orig-stx syn-error-name)
  (unless (identifier? lang)
    (raise-syntax-error #f "expected an identifier in the language position" orig-stx lang))
  (language-id-nts lang syn-error-name))

(define-for-syntax (lhs-lws clauses)
  (with-syntax ([((lhs-for-lw _ ...) ...) clauses])
    (map (λ (x) (to-lw/proc (datum->syntax #f (cdr (syntax-e x)) x)))
         (syntax->list #'(lhs-for-lw ...)))))

;                                                                                                          
;                                                                                                          
;                                                                                                          
;                            ;              ;;;                                 ;    ;;                    
;                           ;;             ;;;;                                ;;    ;;                    
;  ;;;;;;; ;;;;    ;;;    ;;;;; ;;;;;;;   ;;;;; ;;;; ;;;; ;;;; ;;;    ;;;;;  ;;;;;        ;;;;   ;;;; ;;;  
;  ;;;;;;;;;;;;;  ;;;;;  ;;;;;; ;;;;;;;;  ;;;;  ;;;; ;;;; ;;;;;;;;;  ;;;;;; ;;;;;; ;;;;  ;;;;;;  ;;;;;;;;; 
;  ;;;; ;;; ;;;; ;;;; ;;  ;;;;      ;;;; ;;;;;; ;;;; ;;;; ;;;; ;;;; ;;;;;;;  ;;;;  ;;;; ;;;;;;;; ;;;; ;;;; 
;  ;;;; ;;; ;;;; ;;;;;;;  ;;;;   ;;;;;;; ;;;;;; ;;;; ;;;; ;;;; ;;;; ;;;;     ;;;;  ;;;; ;;;; ;;; ;;;; ;;;; 
;  ;;;; ;;; ;;;; ;;;;;    ;;;;; ;;  ;;;;  ;;;;  ;;;; ;;;; ;;;; ;;;; ;;;;;;;  ;;;;; ;;;; ;;;;;;;; ;;;; ;;;; 
;  ;;;; ;;; ;;;;  ;;;;;;  ;;;;; ;;;;;;;;  ;;;;  ;;;;;;;;; ;;;; ;;;;  ;;;;;;  ;;;;; ;;;;  ;;;;;;  ;;;; ;;;; 
;  ;;;; ;;; ;;;;   ;;;;    ;;;;  ;; ;;;;  ;;;;   ;;; ;;;; ;;;; ;;;;   ;;;;;   ;;;; ;;;;   ;;;;   ;;;; ;;;; 
;                                                                                                          
;                                                                                                          
;                                                                                                          

(define-syntax (define-metafunction stx)
  (syntax-case stx ()
    [(_ . rest)
     (internal-define-metafunction stx #f #'rest #f)]))

(define-syntax (define-relation stx)
  (syntax-case stx ()
    [(_ . rest)
     ;; need to rule out the contracts for this one
     (internal-define-metafunction stx #f #'rest #t)]))

(define-syntax (define-metafunction/extension stx)
  (syntax-case stx ()
    [(_ prev . rest)
     (identifier? #'prev)
     (internal-define-metafunction stx #'prev #'rest #f)]))

(define-for-syntax (internal-define-metafunction orig-stx prev-metafunction stx relation?)
  (not-expression-context orig-stx)
  (syntax-case stx ()
    [(lang . rest)
     (let ([syn-error-name (if relation?
                               'define-relation
                               (if prev-metafunction
                                   'define-metafunction/extension
                                   'define-metafunction))])
       ;; keep this near the beginning, so it signals the first error (PR 10062)
       (definition-nts #'lang orig-stx syn-error-name)
       (when (null? (syntax-e #'rest))
         (raise-syntax-error syn-error-name "no clauses" orig-stx))
       (when prev-metafunction
         (syntax-local-value 
          prev-metafunction
          (λ ()
            (raise-syntax-error syn-error-name "expected a previously defined metafunction" orig-stx prev-metafunction))))
       (let*-values ([(contract-name dom-ctcs codom-contracts pats)
                      (split-out-contract orig-stx syn-error-name #'rest relation?)]
                     [(name _) (defined-name (list contract-name) pats orig-stx)])
         (when (and prev-metafunction (eq? (syntax-e #'name) (syntax-e prev-metafunction)))
           (raise-syntax-error syn-error-name "the extended and extending metafunctions cannot share a name" orig-stx prev-metafunction))
         (with-syntax ([(name2 name-predicate) (generate-temporaries (list name name))]
                       [name name])
           (with-syntax ([defs #`(begin
                                   (define-values (name2 name-predicate)
                                     (generate-metafunction #,orig-stx
                                                            lang
                                                            #,prev-metafunction
                                                            name
                                                            name-predicate
                                                            #,dom-ctcs
                                                            #,codom-contracts
                                                            #,pats
                                                            #,relation?
                                                            #,syn-error-name))
                                   (term-define-fn name name2))])
             (if (eq? 'top-level (syntax-local-context))
                 ; Introduce the names before using them, to allow
                 ; metafunction definition at the top-level.
                 (syntax 
                  (begin 
                    (define-syntaxes (name2 name-predicate) (values))
                    defs))
                 (syntax defs))))))]))

(define-for-syntax (relation-split-out-rhs raw-rhsss orig-stx)
  (for/list ([rhss (in-list (syntax->list raw-rhsss))])
    (define rhses '())
    (define sc/wheres '())
    (for ([rhs (in-list (syntax->list rhss))])
      (define (found-one) 
        (set! sc/wheres (cons rhs sc/wheres)))
      (syntax-case rhs (side-condition side-condition/hidden where where/hidden judgment-holds)
        [(side-condition . stuff) (found-one)]
        [(side-condition/hidden . stuff) (found-one)]
        [(where . stuff) (found-one)]
        [(where/hidden . stuff) (found-one)]
        [(judgment-holds . stuff) (found-one)]
        [_ 
         (cond
           [(null? sc/wheres)
            (set! rhses (cons rhs rhses))]
           [else
            (raise-syntax-error 'define-relation
                                (format "found a '~a' clause not at the end; followed by a normal, right-hand side clause"
                                        (syntax-e (car (syntax-e (car sc/wheres)))))
                                (last sc/wheres)
                                #f
                                (list  rhs))])]))
    (list (reverse rhses)
          (reverse sc/wheres))))

(define-syntax (generate-metafunction stx)
  (syntax-case stx ()
    [(_ orig-stx lang prev-metafunction name name-predicate dom-ctcs codom-contracts pats relation? syn-error-name)
     (let ([prev-metafunction (and (syntax-e #'prev-metafunction) #'prev-metafunction)]
           [dom-ctcs (syntax-e #'dom-ctcs)]
           [codom-contracts (syntax-e #'codom-contracts)]
           [pats (syntax-e #'pats)]
           [relation? (syntax-e #'relation?)]
           [syn-error-name (syntax-e #'syn-error-name)])
       (define lang-nts
         (definition-nts #'lang #'orig-stx syn-error-name))
       (with-syntax ([(((original-names lhs-clauses ...) raw-rhses ...) ...) pats]
                     [(lhs-for-lw ...) (lhs-lws pats)])
         (with-syntax ([((rhs stuff ...) ...) (if relation?
                                                  (with-syntax ([(((rhses ...) (where/sc ...)) ...) 
                                                                 (relation-split-out-rhs #'((raw-rhses ...) ...)
                                                                                         #'orig-stx)])
                                                    #'(((AND rhses ...) where/sc ...) ...))
                                                  #'((raw-rhses ...) ...))]
                       [(lhs ...) #'((lhs-clauses ...) ...)])
           (parse-extras #'((stuff ...) ...))
           (with-syntax ([((side-conditions-rewritten lhs-names lhs-namess/ellipses) ...) 
                          (map (λ (x) (rewrite-side-conditions/check-errs
                                       lang-nts
                                       syn-error-name
                                       #t
                                       x))
                               (syntax->list (syntax (lhs ...))))])
             (with-syntax ([(rhs/wheres ...)
                            (map (λ (sc/b rhs names names/ellipses)
                                   (bind-withs
                                    syn-error-name '()  
                                    #'effective-lang lang-nts
                                    sc/b 'flatten
                                    #`(list (term #,rhs))
                                    (syntax->list names) 
                                    (syntax->list names/ellipses)))
                                 (syntax->list #'((stuff ...) ...))
                                 (syntax->list #'(rhs ...))
                                 (syntax->list #'(lhs-names ...))
                                 (syntax->list #'(lhs-namess/ellipses ...)))]
                           [(rg-rhs/wheres ...)
                            (map (λ (sc/b rhs names names/ellipses) 
                                   (bind-withs
                                    syn-error-name '()  
                                    #'effective-lang lang-nts
                                    sc/b 'predicate
                                    #`#t
                                    (syntax->list names)
                                    (syntax->list names/ellipses)))
                                 (syntax->list #'((stuff ...) ...))
                                 (syntax->list #'(rhs ...))
                                 (syntax->list #'(lhs-names ...))
                                 (syntax->list #'(lhs-namess/ellipses ...)))])
               (with-syntax ([((rg-side-conditions-rewritten rg-names rg-names/ellipses ...) ...)
                              (map (λ (x) (rewrite-side-conditions/check-errs
                                           lang-nts
                                           syn-error-name
                                           #t
                                           x))
                                   (syntax->list (syntax ((side-condition lhs rg-rhs/wheres) ...))))]
                             [(clause-src ...)
                              (map (λ (lhs)
                                     (format "~a:~a:~a"
                                             (syntax-source lhs)
                                             (syntax-line lhs)
                                             (syntax-column lhs)))
                                   pats)]
                             [(dom-side-conditions-rewritten dom-names dom-names/ellipses)
                              (if dom-ctcs
                                  (rewrite-side-conditions/check-errs
                                   lang-nts
                                   syn-error-name
                                   #f
                                   dom-ctcs)
                                  #'(any () ()))]
                             [((codom-side-conditions-rewritten codom-names codom-names/ellipses) ...)
                              (map (λ (codom-contract)
                                     (rewrite-side-conditions/check-errs
                                      lang-nts
                                      syn-error-name
                                      #f
                                      codom-contract))
                                   codom-contracts)]
                             [(rhs-fns ...)
                              (map (λ (names names/ellipses rhs/where)
                                     (with-syntax ([(names ...) names]
                                                   [(names/ellipses ...) names/ellipses]
                                                   [rhs/where rhs/where])
                                       (syntax
                                        (λ (name bindings)
                                          (term-let-fn ((name name))
                                                       (term-let ([names/ellipses (lookup-binding bindings 'names)] ...)
                                                                 rhs/where))))))
                                   (syntax->list #'(lhs-names ...))
                                   (syntax->list #'(lhs-namess/ellipses ...))
                                   (syntax->list (syntax (rhs/wheres ...))))])
                 (syntax-property
                  (prune-syntax
                   #`(let ([sc `(side-conditions-rewritten ...)]
                           [dsc `dom-side-conditions-rewritten])
                       (let ([cases (map (λ (pat rhs-fn rg-lhs src)
                                           (make-metafunc-case
                                            (λ (effective-lang) (compile-pattern effective-lang pat #t))
                                            rhs-fn
                                            rg-lhs src (gensym)))
                                         sc
                                         (list (λ (effective-lang) rhs-fns) ...)
                                         (list (λ (effective-lang) `rg-side-conditions-rewritten) ...)
                                         `(clause-src ...))]
                             [parent-cases 
                              #,(if prev-metafunction
                                    #`(metafunc-proc-cases #,(term-fn-get-id (syntax-local-value prev-metafunction)))
                                    #'null)])
                         (build-metafunction 
                          lang
                          cases
                          parent-cases
                          (λ (f/dom)
                            (make-metafunc-proc
                             (let ([name (lambda (x) (f/dom x))]) name)
                             (generate-lws #,relation?
                                           (lhs ...)
                                           (lhs-for-lw ...)
                                           ((stuff ...) ...)
                                           #,(if relation?
                                                 #'((raw-rhses ...) ...)
                                                 #'(rhs ...)))
                             lang
                             #t ;; multi-args?
                             'name
                             (let ([name (lambda (x) (name-predicate x))]) name)
                             dsc
                             (append cases parent-cases)
                             #,relation?))
                          #,(if dom-ctcs #'dsc #f)
                          `(codom-side-conditions-rewritten ...)
                          'name
                          #,relation?))))
                  'disappeared-use
                  (map syntax-local-introduce 
                       (syntax->list #'(original-names ...))))))))))]))

(define-syntax (define-judgment-form stx)
  (not-expression-context stx)
  (syntax-case stx ()
    [(def-form-id lang . body)
     (let ([lang #'lang]
           [syn-err-name (syntax-e #'def-form-id)])
       (define nts (definition-nts lang stx syn-err-name))
       (define-values (judgment-form-name dup-form-names mode position-contracts clauses)
         (parse-judgment-form-body #'body syn-err-name stx))
       (define definitions
         #`(begin
             (define-syntax #,judgment-form-name 
               (judgment-form '#,judgment-form-name '#,mode #'judgment-form-proc #'#,lang #'judgment-form-lws))
             (define judgment-form-proc
               (compile-judgment-form-proc #,judgment-form-name #,lang #,mode #,clauses #,position-contracts #,stx #,syn-err-name))
             (define judgment-form-lws
               (compiled-judgment-form-lws #,clauses))))
       (syntax-property
        (prune-syntax
         (if (eq? 'top-level (syntax-local-context))
             ; Introduce the names before using them, to allow
             ; judgment form definition at the top-level.
             #`(begin 
                 (define-syntaxes (judgment-form-proc judgment-form-lws) (values))
                 #,definitions)
             definitions))
        'disappeared-use
        (map syntax-local-introduce dup-form-names)))]))

(define-for-syntax (parse-judgment-form-body body syn-err-name full-stx)
  (define-syntax-class pos-mode
    #:literals (I O)
    (pattern I)
    (pattern O))
  (define-syntax-class mode-spec
    #:description "mode specification"
    (pattern (_:id _:pos-mode ...)))
  (define-syntax-class contract-spec
    #:description "contract specification"
    (pattern (_:id _:expr ...)))
  (define (horizontal-line? id)
    (regexp-match? #rx"^-+$" (symbol->string (syntax-e id))))
  (define-syntax-class horizontal-line
    (pattern x:id #:when (horizontal-line? #'x)))
  (define (parse-rules rules)
    (for/list ([rule rules])
      (syntax-parse rule
        [(prem ... _:horizontal-line conc)
         #'(conc prem ...)]
        [_ rule])))
  (define-values (name/mode mode name/contract contract rules)
    (syntax-parse body #:context full-stx
      [((~or (~seq #:mode ~! mode:mode-spec)
             (~seq #:contract ~! contract:contract-spec))
        ...
        rule:expr ...)
       (let-values ([(name/mode mode)
                     (syntax-parse #'(mode ...)
                       [((name . mode)) (values #'name (syntax->list #'mode))]
                       [_ (raise-syntax-error 
                           #f "expected definition to include a mode specification"
                           full-stx)])]
                    [(name/ctc ctc)
                     (syntax-parse #'(contract ...)
                       [() (values #f #f)]
                       [((name . contract)) (values #'name (syntax->list #'contract))]
                       [(_ . dups)
                        (raise-syntax-error 
                         syn-err-name "expected at most one contract specification"
                         #f #f (syntax->list #'dups))])])
         (values name/mode mode name/ctc ctc
                 (parse-rules (syntax->list #'(rule ...)))))]))
  (check-clauses full-stx syn-err-name rules #t)
  (check-arity-consistency mode contract full-stx)
  (define-values (form-name dup-names)
    (syntax-case rules ()
      [() (raise-syntax-error #f "expected at least one rule" full-stx)]
      [_ (defined-name (list name/mode name/contract) rules full-stx)]))
  (values form-name dup-names mode contract rules))

(define-for-syntax (check-arity-consistency mode contracts full-def)
  (when (and contracts (not (= (length mode) (length contracts))))
    (raise-syntax-error 
     #f "mode and contract specify different numbers of positions" full-def)))

(define-for-syntax (defined-name declared-names clauses orig-stx)
  (with-syntax ([(((used-names _ ...) _ ...) ...) clauses])
    (define-values (the-name other-names)
      (let ([present (filter values declared-names)])
        (if (null? present)
            (values (car (syntax->list #'(used-names ...)))
                    (cdr (syntax->list #'(used-names ...))))
            (values (car present) 
                    (append (cdr present) (syntax->list #'(used-names ...)))))))
    (let loop ([others other-names])
      (cond
        [(null? others) (values the-name other-names)]
        [else
         (unless (eq? (syntax-e the-name) (syntax-e (car others)))
           (raise-syntax-error 
            #f
            "expected the same name in both positions"
            orig-stx
            the-name (list (car others))))
         (loop (cdr others))]))))

(define-for-syntax (split-out-contract stx syn-error-name rest relation?)
  ;; initial test determines if a contract is specified or not
  (cond
    [(pair? (syntax-e (car (syntax->list rest))))
     (values #f #f (list #'any) (check-clauses stx syn-error-name (syntax->list rest) relation?))]
    [else
     (syntax-case rest ()
       [(id separator more ...)
        (identifier? #'id)
        (cond
          [relation?
           (let-values ([(contract clauses) 
                         (parse-relation-contract #'(separator more ...) syn-error-name stx)])
             (when (null? clauses)
               (raise-syntax-error syn-error-name 
                                   "expected clause definitions to follow domain contract"
                                   stx))
             (values #'id contract (list #'any) (check-clauses stx syn-error-name clauses #t)))]
          [else
           (unless (eq? ': (syntax-e #'separator))
             (raise-syntax-error syn-error-name "expected a colon to follow the meta-function's name" stx #'separator))
           (let loop ([more (syntax->list #'(more ...))]
                      [dom-pats '()])
             (cond
               [(null? more)
                (raise-syntax-error syn-error-name "expected an ->" stx)]
               [(eq? (syntax-e (car more)) '->)
                (define-values (raw-clauses rev-codomains)
                  (let loop ([prev (car more)]
                             [more (cdr more)]
                             [codomains '()])
                    (cond
                      [(null? more)
                       (raise-syntax-error syn-error-name "expected a range contract to follow" stx prev)]
                      [else
                       (define after-this-one (cdr more))
                       (cond
                         [(null? after-this-one)
                          (values null (cons (car more) codomains))]
                         [else
                          (define kwd (cadr more))
                          (cond
                            [(member (syntax-e kwd) '(or ∨ ∪))
                             (loop kwd 
                                   (cddr more)
                                   (cons (car more) codomains))]
                            [else
                             (values (cdr more)
                                     (cons (car more) codomains))])])])))
                (let ([doms (reverse dom-pats)]
                      [clauses (check-clauses stx syn-error-name raw-clauses relation?)])
                  (values #'id doms (reverse rev-codomains) clauses))]
               [else
                (loop (cdr more) (cons (car more) dom-pats))]))])]
       [_
        (raise-syntax-error
         syn-error-name
         (format "expected the name of the ~a, followed by its contract (or no name and no contract)"
                 (if relation? "relation" "meta-function"))
         stx
         rest)])]))

(define-for-syntax (check-clauses stx syn-error-name rest relation?)
  (syntax-case rest ()
    [([(lhs ...) roc1 roc2 ...] ...)
     rest]
    [([(lhs ...) rhs ...] ...)
     (if relation?
         rest
         (begin
           (for-each 
            (λ (clause)
              (syntax-case clause ()
                [(a b) (void)]
                [x (raise-syntax-error syn-error-name "expected a pattern and a right-hand side" stx clause)]))
            rest)
           (raise-syntax-error syn-error-name "error checking failed.3" stx)))]
    [([x roc ...] ...)
     (begin
       (for-each 
        (λ (x)
          (syntax-case x ()
            [(lhs ...) (void)]
            [x (raise-syntax-error syn-error-name "expected a function prototype" stx #'x)]))
        (syntax->list #'(x ...)))
       (raise-syntax-error syn-error-name "error checking failed.1" stx))]
    [(x ...)
     (begin
       (for-each 
        (λ (x)
          (syntax-case x ()
            [(stuff stuff2 ...) (void)]
            [x (raise-syntax-error syn-error-name "expected a clause" stx #'x)]))
        (syntax->list #'(x ...)))
       (raise-syntax-error syn-error-name "error checking failed.2" stx))]))

(define-for-syntax (parse-extras extras)
  (for-each
   (λ (stuffs)
     (for-each
      (λ (stuff)
        (syntax-case stuff (where side-condition where/hidden side-condition/hidden judgment-holds)
          [(side-condition tl-side-conds ...) 
           (void)]
          [(side-condition/hidden tl-side-conds ...) 
           (void)]
          [(where x e)
           (void)]
          [(where/hidden x e)
           (void)]
          [(where . args)
           (raise-syntax-error 'define-metafunction 
                               "malformed where clause"
                               stuff)]
          [(where/hidden . args)
           (raise-syntax-error 'define-metafunction 
                               "malformed where/hidden clause"
                               stuff)]
          [(judgment-holds (form-name . _))
           (unless (judgment-form-id? #'form-name)
             (raise-syntax-error 'define-metafunction
                                 "expected the name of a judgment-form"
                                 #'form-name))]
          [_
           (raise-syntax-error 'define-metafunction 
                               "expected a side-condition or where clause"
                               stuff)]))
      (syntax->list stuffs)))
   (syntax->list extras)))

(define-for-syntax (parse-relation-contract after-name syn-error-name orig-stx)
  (syntax-case after-name ()
    [(subset . rest-pieces)
     (unless (memq (syntax-e #'subset) '(⊂ ⊆))
       (raise-syntax-error syn-error-name
                           "expected ⊂ or ⊆ to follow the relation's name"
                           orig-stx #'subset))
     (let ([more (syntax->list #'rest-pieces)])
       (when (null? more)
         (raise-syntax-error syn-error-name 
                             (format "expected a sequence of patterns separated by x or × to follow ~a" 
                                     (syntax-e #'subset))
                             orig-stx
                             #'subset))
       (let loop ([more (cdr more)]
                  [arg-pats (list (car more))])
         (cond
           [(and (not (null? more)) (memq (syntax-e (car more)) '(x ×)))
            (when (null? (cdr more))
              (raise-syntax-error syn-error-name 
                                  (format "expected a pattern to follow ~a" (syntax-e (car more)))
                                  orig-stx (car more)))
            (loop (cddr more)
                  (cons (cadr more) arg-pats))]
           [else (values (reverse arg-pats) more)])))]))

(define-syntax (judgment-holds stx)
  (syntax-case stx ()
    [(j-h judgment)
     #`(not (null? #,(syntax/loc stx (j-h judgment #t))))]
    [(j-h (form-name . pats) tmpl)
     (judgment-form-id? #'form-name)
     (let* ([syn-err-name (syntax-e #'j-h)]
            [lang (judgment-form-lang (syntax-local-value #'form-name))]
            [nts (definition-nts lang stx syn-err-name)]
            [judgment (syntax-case stx () [(_ judgment _) #'judgment])])
       (check-judgment-arity stx judgment)
       #`(sort #,(bind-withs syn-err-name '() lang nts (list judgment)
                             'flatten #`(list (term #,#'tmpl)) '() '())
               string<=?
               #:key (λ (x) (format "~s" x))))]
    [(_ (not-form-name . _) . _)
     (not (judgment-form-id? #'form-name))
     (raise-syntax-error #f "expected a judgment form name" stx #'not-form-name)]))

(define-for-syntax (do-compile-judgment-form-proc name mode clauses contracts nts lang syn-error-name)
  (define (compile-clause clause)
    (syntax-case clause ()
      [((_ . conc-pats) . prems)
       (let-values ([(input-pats output-pats) (split-by-mode (syntax->list #'conc-pats) mode)])
         (define ((rewrite-pattern binds?) pat)
           (rewrite-side-conditions/check-errs nts syn-error-name binds? pat))
         (with-syntax ([(lhs (names ...) (names/ellipses ...)) ((rewrite-pattern #t) input-pats)])
           (define (contracts-compilation ctcs)
             (and ctcs
                  (with-syntax ([(ctc ...) ctcs])
                    #`(list (compile-pattern #,lang `ctc #f) ...))))
           (define-values (input-contracts output-contracts)
             (syntax-case contracts ()
               [#f (values #f #f)]
               [(p ...) 
                (let-values ([(ins outs) (split-by-mode (syntax->list #'(p ...)) mode)])
                  (with-syntax ([((in-pat in-names in-names/ellipses) ...)
                                 (map (rewrite-pattern #f) ins)]
                                [((out-pat out-names out-names/ellipses) ...)
                                 (map (rewrite-pattern #f) outs)])
                  (values #'(in-pat ...)
                          #'(out-pat ...))))]))
           (define body
             (bind-withs syn-error-name '() lang nts (syntax->list #'prems) 
                         'flatten #`(list (term (#,@output-pats))) 
                         (syntax->list #'(names ...))
                         (syntax->list #'(names/ellipses ...))))
           #`(let ([compiled-lhs (compile-pattern #,lang `lhs #t)]
                   [compiled-input-ctcs #,(contracts-compilation input-contracts)]
                   [compiled-output-ctcs #,(contracts-compilation output-contracts)])
               (λ (input)
                 (check-judgment-form-contract `#,name input compiled-input-ctcs 'I '#,mode)
                 (combine-judgment-rhses
                  compiled-lhs
                  input
                  (λ (m)
                    (term-let ([names/ellipses (lookup-binding (mtch-bindings m) 'names)] ...)
                              #,body))
                  (λ (output)
                    (check-judgment-form-contract `#,name output compiled-output-ctcs 'O '#,mode)))))))]))
  (with-syntax ([(clause-proc ...) (map compile-clause clauses)]
                [(clause-proc-ids ...) (generate-temporaries clauses)])
    (with-syntax ([(backwards-ids ...) (reverse (syntax->list #'(clause-proc-ids ...)))])
      #'(let ([clause-proc-ids clause-proc] ...)
          (λ (input)
            (append (backwards-ids input) ...))))))

(define (combine-judgment-rhses compiled-lhs input rhs check-output)
  (define mtchs (match-pattern compiled-lhs input))
  (cond
    [mtchs
     (define output-table (make-hash))
     (for ([m (in-list mtchs)])
       (define os (rhs m))
       (when os
         (for ([x (in-list os)])
           (hash-set! output-table x #t))))
     (define outputs (hash-map output-table (λ (k v) k)))
     (for ([output (in-list outputs)])
       (check-output output))
     outputs]
    [else '()]))

(define-for-syntax (do-compile-judgment-form-lws clauses)
  (syntax-case clauses ()
    [(((_ . conc-body) . prems) ...)
     (let ([rev-premss
            ; for consistency with metafunction extras
            (for/list ([prems (syntax->list #'(prems ...))])
              (reverse (syntax->list prems)))]
           [no-rhss (map (λ (_) '()) clauses)])
       #`(generate-lws #t (conc-body ...) #,(lhs-lws clauses) #,rev-premss #,no-rhss))]))

(define (check-judgment-form-contract form-name terms contracts mode modes)
  (define description
    (case mode
      [(I) "input"]
      [(O) "output"]))
  (when contracts
    (let loop ([rest-modes modes] [rest-terms terms] [rest-ctcs contracts] [pos 1])
      (unless (null? rest-modes)
        (if (eq? mode (car rest-modes))
            (if (match-pattern (car rest-ctcs) (car rest-terms))
                (loop (cdr rest-modes) (cdr rest-terms) (cdr rest-ctcs) (+ 1 pos))
                (redex-error form-name "~a ~s at position ~s does not match its contract"
                             description (car rest-terms) pos))
            (loop (cdr rest-modes) rest-terms rest-ctcs (+ 1 pos)))))))

(define-for-syntax (mode-check mode clauses nts syn-err-name orig-stx)
  (define ((check-template bound-anywhere) temp bound)
    (let check ([t temp])
      (syntax-case t (unquote)
        [(unquote . _)
         (raise-syntax-error syn-err-name "unquote unsupported" t)]
        [x
         (identifier? #'x)
         (unless (cond [(free-id-table-ref bound-anywhere #'x #f)
                        (free-id-table-ref bound #'x #f)]
                       [(id-binds? nts #t #'x)
                        (term-fn? (syntax-local-value #'x (λ () #f)))]
                       [else #t])
           (raise-syntax-error syn-err-name "unbound pattern variable" #'x))]
        [(u ...)
         (for-each check (syntax->list #'(u ...)))]
        [_ (void)])))
  (define ((bind kind) pat bound)
    (define-values (ids _)
      (extract-names nts syn-err-name #t pat kind))
    (for/fold ([b bound]) ([x ids])
              (free-id-table-set b x #t)))
  (define (split-body judgment)
    (syntax-case judgment ()
      [(form-name . body)
       (split-by-mode (syntax->list #'body) (judgment-form-mode (syntax-local-value #'form-name)))]))
  (define (drop-ellipses prems)
    (syntax-case prems ()
      [() '()]
      [(prem maybe-ellipsis . remaining)
       (ellipsis? #'maybe-ellipsis)
       (syntax-case #'prem ()
         [(form-name . _)
          (judgment-form-id? #'form-name)
          (cons #'prem (drop-ellipses #'remaining))]
         [_ (raise-syntax-error syn-err-name "ellipses must follow judgment form uses" #'maybe-ellipsis)])]
      [(prem . remaining)
       (cons #'prem (drop-ellipses #'remaining))]))
  (define (fold-clause pat-pos tmpl-pos acc-init clause)
    (syntax-case clause ()
      [(conc . prems)
       (let-values ([(conc-in conc-out) (split-body #'conc)])
         (check-judgment-arity orig-stx #'conc)
         (define acc-out
           (for/fold ([acc (foldl pat-pos acc-init conc-in)])
                     ([prem (drop-ellipses #'prems)])
                     (syntax-case prem ()
                       [(-where pat tmpl)
                        (where-keyword? #'-where)
                        (begin
                          (tmpl-pos #'tmpl acc)
                          (pat-pos #'pat acc))]
                       [(form-name . _)
                        (if (judgment-form-id? #'form-name)
                            (let-values ([(prem-in prem-out) (split-body prem)])
                              (check-judgment-arity orig-stx prem)
                              (for ([pos prem-in]) (tmpl-pos pos acc))
                              (foldl pat-pos acc prem-out))
                            (raise-syntax-error syn-err-name "expected judgment form name" #'form-name))]
                       [_ (raise-syntax-error syn-err-name "malformed premise" prem)])))
         (for ([pos conc-out]) (tmpl-pos pos acc-out))
         acc-out)]))
  (for ([clause clauses])
       (define do-tmpl
         (check-template
          (fold-clause (bind 'rhs-only) void (make-immutable-free-id-table) clause)))
       (fold-clause (bind 'rhs-only) do-tmpl (make-immutable-free-id-table) clause)))

(define-syntax (generate-lws stx)
  (syntax-case stx ()
    [(_ relation? seq-of-lhs seq-of-lhs-for-lw seq-of-tl-side-cond/binds seq-of-rhs)
     (with-syntax
         ([(rhs/lw ...) 
           (syntax-case #'relation? ()
             [#t (map (λ (x) #`(list #,@(map to-lw/proc (syntax->list x))))
                      (syntax->list #'seq-of-rhs))]
             [#f (map to-lw/proc (syntax->list #'seq-of-rhs))])]
          [(((bind-id/lw . bind-pat/lw) ...) ...)
           ;; Also for pict, extract pattern bindings
           (map name-pattern-lws (syntax->list #'seq-of-lhs))]
          [((where/sc/lw ...) ...)
           ;; Also for pict, extract where bindings
           (map (λ (hm)
                  (map
                   (λ (lst)
                     (syntax-case lst (unquote side-condition where)
                       [(form-name . _)
                        (judgment-form-id? #'form-name)
                        #`(make-metafunc-extra-side-cond #,(to-lw/proc lst))]
                       [(form-name . _)
                        (judgment-form-id? #'form-name)
                        #`(make-metafunc-extra-side-cond #,(to-lw/proc lst))]
                       [(where pat (unquote (f _ _)))
                        (and (or (identifier? #'pat)
                                 (andmap identifier? (syntax->list #'pat)))
                             (or (free-identifier=? #'f #'variable-not-in)
                                 (free-identifier=? #'f #'variables-not-in)))
                        (with-syntax ([(ids ...)
                                       (map to-lw/proc
                                            (if (identifier? #'pat)
                                                (list #'pat)
                                                (syntax->list #'pat)))])
                          #`(make-metafunc-extra-fresh
                             (list ids ...)))]
                       [(where pat exp)
                        #`(make-metafunc-extra-where
                           #,(to-lw/proc #'pat) #,(to-lw/proc #'exp))]
                       [(side-condition x)
                        #`(make-metafunc-extra-side-cond
                           #,(to-lw/uq/proc #'x))]
                       [maybe-ellipsis
                        (ellipsis? #'maybe-ellipsis)
                        (to-lw/proc #'maybe-ellipsis)]))
                   (visible-extras hm)))
                (syntax->list #'seq-of-tl-side-cond/binds))]
          [(((where-bind-id/lw . where-bind-pat/lw) ...) ...)
           (map (λ (clauses)
                  (for/fold ([binds '()]) ([clause (visible-extras clauses)])
                            (syntax-case clause (where)
                              [(form-name . pieces)
                               (judgment-form-id? #'form-name)
                               (let*-values ([(mode) (judgment-form-mode (syntax-local-value #'form-name))]
                                             [(_ outs) (split-by-mode (syntax->list #'pieces) mode)])
                                 (for/fold ([binds binds]) ([out outs])
                                           (append (name-pattern-lws out) binds)))]
                              [(where lhs rhs) (append (name-pattern-lws #'lhs) binds)]
                              [_ binds])))
                (syntax->list #'seq-of-tl-side-cond/binds))]
          [(((rhs-bind-id/lw . rhs-bind-pat/lw/uq) ...) ...)
           ;; Also for pict, extract pattern bindings
           (map (λ (x) (map (λ (x) (cons (to-lw/proc (car x)) (to-lw/uq/proc (cdr x))))
                            (extract-term-let-binds x)))
                (syntax->list #'seq-of-rhs))]
          
          [(x-lhs-for-lw ...) #'seq-of-lhs-for-lw])
       #'(list (list x-lhs-for-lw
                     (list (make-metafunc-extra-where bind-id/lw bind-pat/lw) ...
                           (make-metafunc-extra-where where-bind-id/lw where-bind-pat/lw) ...
                           (make-metafunc-extra-where rhs-bind-id/lw rhs-bind-pat/lw/uq) ...
                           where/sc/lw ...)
                     rhs/lw)
               ...))]))

(define-for-syntax (visible-extras extras)
  (for/fold ([visible empty]) ([extra (syntax->list extras)])
            (syntax-case extra (where/hidden
                                side-condition/hidden
                                judgment-holds)
              [(where/hidden pat exp) visible]
              [(side-condition/hidden x) visible]
              [(judgment-holds judgment)
               (cons #'judgment visible)]
              [_ (cons extra visible)])))

(define-syntax (compile-judgment-form-proc stx)
  (syntax-case stx ()
    [(_ judgment-form-name lang mode clauses ctcs full-def syn-err-name)
     (let ([nts (definition-nts #'lang #'full-def (syntax-e #'syn-err-name))])
       (mode-check (syntax->datum #'mode) (syntax->list #'clauses) nts (syntax-e #'syn-err-name) stx)
       (do-compile-judgment-form-proc
        (syntax-e #'judgment-form-name) 
        (syntax->datum #'mode) 
        (syntax->list #'clauses) 
        #'ctcs
        nts
        #'lang 
        (syntax-e #'syn-err-name)))]))

(define-syntax (compiled-judgment-form-lws stx)
  (syntax-case stx ()
    [(_ clauses)
     (do-compile-judgment-form-lws (syntax->list #'clauses))]))

(define (build-metafunction lang cases parent-cases wrap dom-contract-pat codom-contract-pats name relation?)
  (let* ([dom-compiled-pattern (and dom-contract-pat (compile-pattern lang dom-contract-pat #f))]
         [codom-compiled-patterns (map (λ (codom-contract-pat) (compile-pattern lang codom-contract-pat #f))
                                       codom-contract-pats)]
         [all-cases (append cases parent-cases)]
         [lhss-at-lang (map (λ (case) ((metafunc-case-lhs case) lang)) all-cases)]
         [rhss-at-lang (map (λ (case) ((metafunc-case-rhs case) lang)) all-cases)]
         [ids (map metafunc-case-id all-cases)])
    (values
     (wrap
      (letrec ([cache (make-hash)]
               [cache-entries 0]
               [not-in-cache (gensym)]
               [cache-result (λ (arg res case)
                               (when (caching-enabled?)
                                 (when (>= cache-entries cache-size)
                                   (set! cache (make-hash))
                                   (set! cache-entries 0))
                                 (hash-set! cache arg (cons res case))
                                 (set! cache-entries (add1 cache-entries))))]
               [log-coverage (λ (id)
                               (when id
                                 (for-each 
                                  (λ (c)
                                    (let ([r (coverage-relation c)])
                                      (when (and (metafunc-proc? r)
                                                 (findf (λ (c) (eq? id (metafunc-case-id c)))
                                                        (metafunc-proc-cases r)))
                                        (cover-case id c))))
                                  (relation-coverage))))]
               [metafunc
                (λ (exp)
                  (let ([cache-ref (hash-ref cache exp not-in-cache)])
                    (cond
                      [(or (not (caching-enabled?)) (eq? cache-ref not-in-cache))
                       (when dom-compiled-pattern
                         (unless (match-pattern dom-compiled-pattern exp)
                           (redex-error name
                                        "~s is not in my domain"
                                        `(,name ,@exp))))
                       (let loop ([ids ids]
                                  [lhss lhss-at-lang]
                                  [rhss rhss-at-lang]
                                  [num (- (length parent-cases))])
                         (cond
                           [(null? ids) 
                            (if relation?
                                (begin 
                                  (cache-result exp #f #f)
                                  #f)
                                (redex-error name "no clauses matched for ~s" `(,name . ,exp)))]
                           [else
                            (let ([pattern (car lhss)]
                                  [rhs (car rhss)]
                                  [id (car ids)]
                                  [continue (λ () (loop (cdr ids) (cdr lhss) (cdr rhss) (+ num 1)))])
                              (let ([mtchs (match-pattern pattern exp)])
                                (cond
                                  [(not mtchs) (continue)]
                                  [relation? 
                                   (let ([ans
                                          (ormap (λ (mtch) 
                                                   (define rhs-ans (rhs traced-metafunc (mtch-bindings mtch)))
                                                   (and rhs-ans (ormap values rhs-ans)))
                                                 mtchs)])
                                     (unless (ormap (λ (codom-compiled-pattern) (match-pattern codom-compiled-pattern ans))
                                                    codom-compiled-patterns)
                                       (redex-error name "codomain test failed for ~s, call was ~s" ans `(,name ,@exp)))
                                     (cond
                                       [ans 
                                        (cache-result exp #t id)
                                        (log-coverage id)
                                        #t]
                                       [else
                                        (continue)]))]
                                  [else
                                   (let ([anss (apply append
                                                      (filter values
                                                              (map (λ (mtch) (rhs traced-metafunc (mtch-bindings mtch)))
                                                                   mtchs)))]
                                         [ht (make-hash)])
                                     (for-each (λ (ans) (hash-set! ht ans #t)) anss)
                                     (cond
                                       [(null? anss)
                                        (continue)]
                                       [(not (= 1 (hash-count ht)))
                                        (redex-error name "~a matched ~s ~a different ways and returned different results" 
                                                     (if (< num 0)
                                                         "a clause from an extended metafunction"
                                                         (format "clause #~a (counting from 0)" num))
                                                     `(,name ,@exp)
                                                     (length mtchs))]
                                       [else
                                        (let ([ans (car anss)])
                                          (unless (ormap (λ (codom-compiled-pattern)
                                                           (match-pattern codom-compiled-pattern ans))
                                                         codom-compiled-patterns)
                                            (redex-error name
                                                         "codomain test failed for ~s, call was ~s"
                                                         ans 
                                                         `(,name ,@exp)))
                                          (cache-result exp ans id)
                                          (log-coverage id)
                                          ans)]))])))]))]
                      [else 
                       (log-coverage (cdr cache-ref))
                       (car cache-ref)])))]
               [ot (current-trace-print-args)]
               [otr (current-trace-print-results)]
               [traced-metafunc (lambda (exp)
                                  (if (or (eq? (current-traced-metafunctions) 'all)
                                          (memq name (current-traced-metafunctions)))
                                      (parameterize ([current-trace-print-args
                                                      (λ (name args kws kw-args level)
                                                        (if (or (not (caching-enabled?))
                                                                (eq? not-in-cache (hash-ref cache exp not-in-cache)))
                                                            (display " ")
                                                            (display "c"))
                                                        (ot name (car args) kws kw-args level))]
                                                     [current-trace-print-results
                                                      (λ (name results level)
                                                        (display " ")
                                                        (otr name results level))]
                                                     [print-as-expression #f])
                                        (trace-call name metafunc exp))
                                      (metafunc exp)))])
        traced-metafunc))
     (if dom-compiled-pattern
         (λ (exp) (and (match-pattern dom-compiled-pattern exp) #t))
         (λ (exp) (and (ormap (λ (lhs) (match-pattern lhs exp)) lhss-at-lang) 
                       #t))))))

(define current-traced-metafunctions (make-parameter '()))

(define-syntax (metafunction-form stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (let ([v (syntax-local-value #'id (lambda () #f))])
       (if (term-fn? v)
           (syntax-property
            #`(make-metafunction #,(term-fn-get-id v))
            'disappeared-use
            (list #'id))
           (raise-syntax-error
            #f
            "not bound as a metafunction"
            stx
            #'id)))]))

(define-for-syntax (mode-keyword stx)
  (raise-syntax-error #f "keyword invalid outside of mode specification" stx))
(define-syntax I mode-keyword)
(define-syntax O mode-keyword)

(define-syntax (::= stx)
  (raise-syntax-error #f "cannot be used outside a language definition" stx))

(define-for-syntax (parse-non-terminals nt-defs stx)
  (define (parse-non-terminal def)
    (define (delim? stx)
      (and (identifier? stx) (free-identifier=? stx #'::=)))
    (define-values (left delim right)
      (syntax-case def ()
        [(_ _ ...)
         (let split ([xs def])
           (syntax-case xs (::=)
             [() (values '() #f '())]
             [(x . prods)
              (delim? #'x)
              (values '() #'x (syntax->list #'prods))]
             [(x . xs)
              (let-values ([(l d r) (split #'xs)])
                (values (cons #'x l) d r))]))]
        [_ (raise-syntax-error #f "expected non-terminal definition" stx def)]))
    (define (check-each xs bad? msg)
      (define x (findf bad? xs))
      (when x (raise-syntax-error #f msg stx x)))
    (define-values (names prods)
      (if delim
          (begin
            (when (null? left)
              (raise-syntax-error #f "expected preceding non-terminal names" stx delim))
            (values left right))
          (values (syntax-case (car left) ()
                    [(x ...) (syntax->list #'(x ...))]
                    [x (list #'x)])
                  (cdr left))))
    
    (check-each names (λ (x) (not (identifier? x)))
                "expected non-terminal name")
    (check-each names (λ (x) (memq (syntax-e x) (cons 'name underscore-allowed)))
                "cannot use pattern language keyword as a non-terminal name")
    (check-each names (λ (x) (regexp-match? #rx"_" (symbol->string (syntax-e x))))
                "cannot use _ in a non-terminal name")
    
    (when (null? prods)
      (raise-syntax-error #f "expected at least one production to follow" 
                          stx (or delim (car left))))
    (check-each prods delim? "expected production")
    (cons names prods))
  (define parsed (map parse-non-terminal (syntax->list nt-defs)))
  (define defs (make-hash))
  (for ([p parsed])
    (define ns (car p))
    (for ([n ns])
      (define m (hash-ref defs (syntax-e n) #f))
      (if m
          (raise-syntax-error #f "same non-terminal defined twice"
                              stx n (list m))
          (hash-set! defs (syntax-e n) n))))
  parsed)

(define-syntax (define-language stx)
  (not-expression-context stx)
  (syntax-case stx ()
    [(form-name lang-name . nt-defs)
     (begin
       (unless (identifier? #'lang-name)
         (raise-syntax-error #f "expected an identifier" stx #'lang-name))
       (with-syntax ([(define-language-name) (generate-temporaries #'(lang-name))])
         (let ([non-terms (parse-non-terminals #'nt-defs stx)])
           (with-syntax ([((names prods ...) ...) non-terms]
                         [(all-names ...) (apply append (map car non-terms))])
             (quasisyntax/loc stx
               (begin
                 (define-syntax lang-name
                   (make-set!-transformer
                    (make-language-id
                     (λ (stx)
                       (syntax-case stx (set!)
                         [(set! x e) (raise-syntax-error (syntax-e #'form-name) "cannot set! identifier" stx #'e)]
                         [(x e (... ...))
                          #'(define-language-name e (... ...))]
                         [x 
                          (identifier? #'x)
                          #'define-language-name]))
                     '(all-names ...))))
                 (define define-language-name
                   #,(syntax/loc stx (language form-name lang-name (all-names ...) (names prods ...) ...)))))))))]))

(define-struct binds (source binds))

  
(define-syntax (language stx)
  (syntax-case stx ()
    [(_ form-name lang-id (all-names ...) (name rhs ...) ...)
     (prune-syntax
      (let ()
        (let ([all-names (syntax->list #'(all-names ...))])
          (with-syntax ([(((r-rhs r-names r-names/ellipses) ...) ...) 
                         (map (lambda (rhss) 
                                (map (lambda (rhs)
                                       (rewrite-side-conditions/check-errs
                                        (map syntax-e all-names)
                                        (syntax-e #'form-name)
                                        #f
                                        rhs)) 
                                     (syntax->list rhss)))
                              (syntax->list (syntax ((rhs ...) ...))))]
                        [((rhs/lw ...) ...) 
                         (map (lambda (rhss) (map to-lw/proc (syntax->list rhss)))
                              (syntax->list (syntax ((rhs ...) ...))))]
                        [(refs ...)
                         (let loop ([stx (syntax ((rhs ...) ...))])
                           (cond
                             [(identifier? stx)
                              (if (ormap (λ (x) (bound-identifier=? x stx)) 
                                         all-names)
                                  (list stx)
                                  '())]
                             [(syntax? stx)
                              (loop (syntax-e stx))]
                             [(pair? stx)
                              (append (loop (car stx))
                                      (loop (cdr stx)))]
                             [else '()]))])
            (check-for-cycles stx #'(name ...) #'((r-rhs ...) ...))
            (with-syntax ([(the-stx ...) (cdr (syntax-e stx))]
                          [(all-names ...) all-names]
                          [((uniform-names ...) ...)
                           (map (λ (x) (if (identifier? x) (list x) x))
                                (syntax->list (syntax (name ...))))]
                          [(first-names ...)
                           (map (λ (x) (if (identifier? x) x (car (syntax->list x))))
                                (syntax->list (syntax (name ...))))]
                          [((new-name orig-name) ...)
                           (apply
                            append
                            (map (λ (name-stx)
                                   (if (identifier? name-stx)
                                       '()
                                       (let ([l (syntax->list name-stx)])
                                         (map (λ (x) (list x (car l)))
                                              (cdr l)))))
                                 (syntax->list #'(name ...))))])
              
              ;; note: when there are multiple names for a single non-terminal,
              ;; we build equivalent non-terminals by redirecting all except the
              ;; first non-terminal to the first one, and then make the first one
              ;; actually have all of the productions. This should produce better
              ;; caching behavior and should compile faster than duplicating the
              ;; right-hand sides.
              (syntax/loc stx
                (begin
                  (let ([all-names 1] ...)
                    (begin (void) refs ...))
                  (compile-language (list (list '(uniform-names ...) rhs/lw ...) ...)
                                    (list (make-nt 'first-names (list (make-rhs `r-rhs) ...)) ...
                                          (make-nt 'new-name (list (make-rhs '(nt orig-name)))) ...)
                                    '((uniform-names ...) ...)))))))))]))

(define-syntax (define-extended-language stx)
  (syntax-case stx ()
    [(_ name orig-lang . nt-defs)
     (begin
       (unless (identifier? (syntax name))
         (raise-syntax-error 'define-extended-language "expected an identifier" stx #'name))
       (unless (identifier? (syntax orig-lang))
         (raise-syntax-error 'define-extended-language "expected an identifier" stx #'orig-lang))
       (let ([old-names (language-id-nts #'orig-lang 'define-extended-language)]
             [non-terms (parse-non-terminals #'nt-defs stx)])
         (with-syntax ([((names prods ...) ...) non-terms]
                       [(all-names ...) (apply append old-names (map car non-terms))]
                       [(define-language-name) (generate-temporaries #'(name))])
           #'(begin
               (define define-language-name (extend-language orig-lang (all-names ...) (names prods ...) ...))
               (define-syntax name
                 (make-set!-transformer
                  (make-language-id
                   (λ (stx)
                     (syntax-case stx (set!)
                       [(set! x e) (raise-syntax-error 'define-extended-language "cannot set! identifier" stx #'e)]
                       [(x e (... ...)) #'(define-language-name e (... ...))]
                       [x 
                        (identifier? #'x)
                        #'define-language-name]))
                   '(all-names ...))))))))]))

(define-syntax (extend-language stx)
  (syntax-case stx ()
    [(_ lang (all-names ...) (name rhs ...) ...)
     (with-syntax ([(((r-rhs r-names r-names/ellipses) ...) ...)
                    (map (lambda (rhss) (map (λ (x) (rewrite-side-conditions/check-errs
                                                     (append (language-id-nts #'lang 'define-extended-language)
                                                             (map syntax-e
                                                                  (syntax->list #'(all-names ...))))
                                                     'define-extended-language
                                                     #f
                                                     x))
                                             (syntax->list rhss)))
                         (syntax->list (syntax ((rhs ...) ...))))]
                   [((rhs/lw ...) ...) (map (lambda (rhss) (map to-lw/proc (syntax->list rhss)))
                                            (syntax->list (syntax ((rhs ...) ...))))]
                   [((uniform-names ...) ...)
                    (map (λ (x) (if (identifier? x) (list x) x))
                         (syntax->list (syntax (name ...))))])
       (syntax/loc stx
         (do-extend-language lang 
                             (list (make-nt '(uniform-names ...) (list (make-rhs `r-rhs) ...)) ...)
                             (list (list '(uniform-names ...) rhs/lw ...) ...))))]))

(define extend-nt-ellipses '(....))

;; do-extend-language : compiled-lang (listof (listof nt)) ? -> compiled-lang
;; note: the nts that come here are an abuse of the `nt' struct; they have
;; lists of symbols in the nt-name field.
(define (do-extend-language old-lang new-nts new-pict-infos)
  (unless (compiled-lang? old-lang)
    (error 'define-extended-language "expected a language as first argument, got ~e" old-lang))
  
  (let ([old-nts (compiled-lang-lang old-lang)]
        [old-ht (make-hasheq)]
        [new-ht (make-hasheq)])
    
    
    (for-each (λ (nt) 
                (hash-set! old-ht (nt-name nt) nt)
                (hash-set! new-ht (nt-name nt) nt))
              old-nts)
    
    (for-each (λ (raw-nt)
                (let* ([names (nt-name raw-nt)]
                       [rhs (nt-rhs raw-nt)]
                       [primary-names (map (λ (name) (find-primary-nt name old-lang)) names)]
                       [main-primary (car primary-names)])
                  
                  ;; error checking
                  (when (and (ormap not primary-names)
                             (ormap symbol? primary-names))
                    (error 'define-extended-language "new language extends old non-terminal ~a and also adds new shortcut ~a"
                           (ormap (λ (x y) (and (symbol? x) y)) primary-names names)
                           (ormap (λ (x y) (and (not x) y)) primary-names names)))
                  
                  ;; error checking
                  (when (andmap symbol? primary-names)
                    (let ([main-orig (car names)])
                      (let loop ([primary-names (cdr primary-names)]
                                 [names (cdr names)])
                        (cond
                          [(null? primary-names) void]
                          [else 
                           (unless (eq? main-primary (car primary-names))
                             (error 'define-extended-language
                                    (string-append 
                                     "new language does not have the same non-terminal aliases as the old,"
                                     " non-terminal ~a was not in the same group as ~a in the old language")
                                    (car names)
                                    main-orig))
                           (loop (cdr primary-names) (cdr names))]))))
                                  
                  
                  ;; rebind original nt
                  (let ([nt (make-nt (or main-primary (car names)) rhs)])
                    (cond
                      [(ormap (λ (rhs) (member (rhs-pattern rhs) extend-nt-ellipses))
                              (nt-rhs nt))
                       (unless (hash-ref old-ht (nt-name nt) #f)
                         (error 'define-extended-language
                                "the language extends the ~s non-terminal, but that non-terminal is not in the old language"
                                (nt-name nt)))
                       (hash-set! new-ht 
                                  (nt-name nt)
                                  (make-nt
                                   (nt-name nt)
                                   (append (nt-rhs (hash-ref old-ht (nt-name nt)))
                                           (filter (λ (rhs) (not (member (rhs-pattern rhs) extend-nt-ellipses)))
                                                   (nt-rhs nt)))))]
                      [else
                       (hash-set! new-ht (nt-name nt) nt)]))
                  
                  ;; add new shortcuts (if necessary)
                  (unless main-primary
                    (for-each (λ (shortcut-name)
                                (hash-set! new-ht 
                                           shortcut-name 
                                           (make-nt shortcut-name (list (make-rhs `(nt ,(car names)))))))
                              (cdr names)))))
                  
              new-nts)
    
    (compile-language (vector (compiled-lang-pict-builder old-lang)
                              new-pict-infos)
                      (hash-map new-ht (λ (x y) y))
                      (compiled-lang-nt-map old-lang))))

;; find-primary-nt : symbol lang -> symbol or #f
;; returns the primary non-terminal for a given nt, or #f if `nt' isn't bound in the language.
(define (find-primary-nt nt lang)
  (let ([combined (find-combined-nts nt lang)])
    (and combined
         (car combined))))

;; find-combined-nts : symbol lang -> (listof symbol) or #f
;; returns the combined set of non-terminals for 'nt' from lang
(define (find-combined-nts nt lang)
  (ormap (λ (nt-line)
           (and (member nt nt-line)
                nt-line))
         (compiled-lang-nt-map lang)))

(define (apply-reduction-relation* reductions exp 
                                   #:cache-all? [cache-all? (current-cache-all?)]
                                   #:stop-when [stop-when (λ (x) #f)])
  (let-values ([(results cycle?) (traverse-reduction-graph reductions exp
                                                           #:cache-all? cache-all?
                                                           #:stop-when stop-when)])
    results))

(struct search-success ())
(struct search-failure (cutoff?))

;; traverse-reduction-graph : 
;;  reduction-relation term #:goal (-> any boolean?) #:steps number? #:visit (-> any/c void?) -> (or/c search-success? search-failure?)
;;  reduction-relation term #:goal #f                #:steps number? #:visit (-> any/c void?) -> (values (listof any/c) boolean?)
(define (traverse-reduction-graph reductions start #:goal [goal? #f] #:steps [steps +inf.0] #:visit [visit void] 
                                  #:cache-all? [cache-all? (current-cache-all?)]
                                  #:stop-when [stop-when (λ (x) #f)])
  (define visited (and cache-all? (make-hash)))
  (let/ec return
    (let ([answers (make-hash)]
          [cycle? #f]
          [cutoff? #f])
      (let loop ([term start]
                 ;; It would be better to record all visited terms, to avoid traversing
                 ;; any part of the graph multiple times. Results from 
                 ;;    collects/redex/trie-experiment
                 ;; in commit
                 ;;    152084d5ce6ef49df3ec25c18e40069950146041
                 ;; suggest that a hash works better than a trie.
                 [path (make-immutable-hash '())]
                 [more-steps steps])
        (if (and goal? (goal? term))
            (return (search-success))
            (cond
              [(hash-ref path term #f)
               (set! cycle? #t)]
              [else
               (visit term)
               (cond
                 [(stop-when term)
                  (unless goal? 
                    (hash-set! answers term #t))]
                 [else
                  (define nexts (apply-reduction-relation reductions term))
                  (cond
                    [(null? nexts) 
                     (unless goal? 
                       (hash-set! answers term #t))]
                    [else (if (zero? more-steps)
                              (set! cutoff? #t)
                              (for ([next (in-list (remove-duplicates nexts))])
                                (when (or (not visited)
                                          (not (hash-ref visited next #f)))
                                  (when visited (hash-set! visited next #t))
                                  (loop next 
                                        (hash-set path term #t) 
                                        (sub1 more-steps)))))])])])))
      (if goal?
          (search-failure cutoff?)
          (values (sort (hash-map answers (λ (x y) x))
                        string<=?
                        #:key (λ (x) (format "~s" x)))
                  cycle?)))))

(define current-cache-all? (make-parameter #f))

;; map/mt : (a -> b) (listof a) (listof b) -> (listof b)
;; map/mt is like map, except
;;  a) it uses the last argument instead of the empty list
;;  b) if `f' returns #f, that is not included in the result
(define (map/mt f l mt-l)
  (let loop ([l l])
    (cond
      [(null? l) mt-l]
      [else
       (let ([this-one (f (car l))])
         (if this-one
             (cons this-one (loop (cdr l)))
             (loop (cdr l))))])))

(define (reduction-relation->rule-names x) 
  (reverse (reduction-relation-rule-names x)))


;                                                                               
;                                                                               
;                                                                               
;      ;                     ;                         ;;     ;                 
;     ;;                    ;;                         ;;    ;;                 
;   ;;;;;   ;;;    ;;;;;  ;;;;;      ;;;;; ;;;; ;;;;       ;;;;;   ;;;    ;;;;; 
;  ;;;;;;  ;;;;;  ;;;;;; ;;;;;;     ;;;;;; ;;;; ;;;; ;;;; ;;;;;;  ;;;;;  ;;;;;; 
;   ;;;;  ;;;; ;; ;;;;    ;;;;      ;;;;   ;;;; ;;;; ;;;;  ;;;;  ;;;; ;; ;;;;   
;   ;;;;  ;;;;;;;  ;;;;   ;;;;       ;;;;  ;;;; ;;;; ;;;;  ;;;;  ;;;;;;;  ;;;;  
;   ;;;;; ;;;;;     ;;;;  ;;;;;       ;;;; ;;;; ;;;; ;;;;  ;;;;; ;;;;;     ;;;; 
;   ;;;;;  ;;;;;; ;;;;;;  ;;;;;     ;;;;;; ;;;;;;;;; ;;;;  ;;;;;  ;;;;;; ;;;;;; 
;    ;;;;   ;;;;  ;;;;;    ;;;;     ;;;;;   ;;; ;;;; ;;;;   ;;;;   ;;;;  ;;;;;  
;                                                                               
;                                                                               
;                                                                               

(define tests 0)
(define test-failures 0)
(define (inc-failures) (set! test-failures (+ test-failures 1)))
(define (inc-tests) (set! tests (+ tests 1)))

(define (test-results)
  (cond
    [(= tests 0)
     (printf "No tests run.\n")]
    [(= test-failures 0)
     (cond
       [(= tests 1)
        (printf "One test passed.\n")]
       [(= tests 2)
        (printf "Both tests passed.\n")]
       [else
        (printf "All ~a tests passed.\n" tests)])]
    [else
     (printf "~a test~a failed (out of ~a total).\n"
             test-failures
             (if (= test-failures 1) "" "s")
             tests)])
  (set! tests 0)
  (set! test-failures 0))

(define-for-syntax (get-srcloc stx)
  #`(list 
     '#,(syntax-source stx)
     '#,(syntax-line stx)
     '#,(syntax-column stx)
     '#,(syntax-position stx)))

(define-for-syntax test-equiv-ctc
  #'(-> any/c any/c any/c))
(define-for-syntax test-equiv-name
  "#:equiv argument")
(define-for-syntax test-equiv-default
  #'equal?)

(define-syntax (test-->> stx)
  (syntax-parse stx
    [(form red:expr
           (~or (~optional (~seq (~and #:cycles-ok (~bind [cycles-ok? #t])))
                           #:defaults ([cycles-ok? #f])
                           #:name "#:cycles-ok argument")
                (~optional (~seq #:equiv equiv?)
                           #:defaults ([equiv?.c test-equiv-default])
                           #:name test-equiv-name)
                (~optional (~seq #:pred pred)
                           #:defaults ([pred #f])
                           #:name "#:pred argument"))
           ...
           e1:expr
           e2:expr ...)
     #:declare equiv? (expr/c test-equiv-ctc #:name test-equiv-name)
     #`(test-->>/procs 'test-->> red e1 (list e2 ...) traverse-reduction-graph #,(attribute cycles-ok?) equiv?.c #,(attribute pred) #,(get-srcloc stx))]))

(define-syntax (test--> stx)
  (syntax-parse stx
    [(form red:expr
           (~optional (~seq #:equiv equiv?)
                      #:defaults ([equiv?.c test-equiv-default]))
           e1:expr
           e2:expr ...)
     #:declare equiv? (expr/c test-equiv-ctc #:name test-equiv-name)
     #`(test-->>/procs 'test--> red e1 (list e2 ...) apply-reduction-relation/dummy-second-value #t equiv?.c #f #,(get-srcloc stx))]))

(define (apply-reduction-relation/dummy-second-value red arg #:visit visit)
  (values (apply-reduction-relation red arg) #f))

(define (test-->>/procs name red arg expected apply-red cycles-ok? equiv? pred srcinfo)
  (unless (reduction-relation? red)
    (error name "expected a reduction relation as first argument, got ~e" red))
  (when pred
    (unless (and (procedure? pred)
                 (procedure-arity-includes? pred 1))
      (error 'test-->> "expected a procedure that accepted one argument for the #:pred, got ~e" pred)))
  (inc-tests)
  (define visit-already-failed? #f)
  (define (visit t)
    (when pred
      (unless visit-already-failed?
        (unless (pred t)
          (set! visit-already-failed? #t)
          (inc-failures)
          (print-failed srcinfo)
          (eprintf "found a term that failed #:pred: ~v\n" t)))))
  (let-values ([(got got-cycle?) (apply-red red arg #:visit visit)])
    
    (cond
      [(and got-cycle?
            (not cycles-ok?))
       (inc-failures)
       (print-failed srcinfo)
       (eprintf "found a cycle in the reduction graph\n")]
      [else
       (unless visit-already-failed?
         (let* ([⊆ (λ (s1 s2) (andmap (λ (x1) (memf (λ (x) (equiv? x1 x)) s2)) s1))]
                [set-equal? (λ (s1 s2) (and (⊆ s1 s2) (⊆ s2 s1)))])
           (unless (set-equal? expected got)
             (inc-failures)
             (print-failed srcinfo)
             (for-each
              (λ (v2) (eprintf "expected: ~v\n" v2))
              expected)
             (if (empty? got)
                 (eprintf "got nothing\n")
                 (for-each
                  (λ (v1) (eprintf "  actual: ~v\n" v1))
                  got)))))])))

(define-syntax (test-->>∃ stx)
  (syntax-parse stx
    [(form (~optional (~seq #:steps steps) #:defaults ([steps.c #'1000]))
           relation
           start:expr
           goal)
     #:declare relation (expr/c #'reduction-relation? 
                                #:name "reduction relation expression")
     #:declare goal (expr/c #'(or/c (-> any/c any/c) (not/c procedure?)) 
                            #:name "goal expression")
     #:declare steps (expr/c #'(or/c natural-number/c +inf.0) 
                             #:name "steps expression")
     #`(test-->>∃/proc relation.c start goal.c steps.c #,(get-srcloc stx))]))

(define (test-->>∃/proc relation start goal steps srcinfo)
  (let ([result (traverse-reduction-graph 
                 relation
                 start 
                 #:goal (if (procedure? goal) goal (λ (x) (equal? goal x)))
                 #:steps steps)])
    (inc-tests)
    (when (search-failure? result)
      (print-failed srcinfo)
      (inc-failures)
      (begin
        (if (procedure? goal)
            (eprintf "no term satisfying ~a reachable from ~a" goal start)
            (eprintf "term ~a not reachable from ~a" goal start))
        (when (search-failure-cutoff? result)
          (eprintf " (within ~a steps)" steps))
        (newline (current-error-port))))))

(define-syntax (test-predicate stx)
  (syntax-case stx ()
    [(_ p arg)
     #`(test-predicate/proc p arg #,(get-srcloc stx))]))

(define (test-predicate/proc pred arg srcinfo)
  (inc-tests)
  (unless (pred arg)
    (inc-failures)
    (print-failed srcinfo)
    (eprintf "  ~v does not hold for\n  ~v\n" 
             pred arg)))

(define-syntax (test-equal stx)
  (syntax-case stx ()
    [(_ e1 e2)
     #`(test-equal/proc e1 e2 #,(get-srcloc stx))]))

(define (test-equal/proc v1 v2 srcinfo)
  (inc-tests)
  (unless (equal? v1 v2)
    (inc-failures)
    (print-failed srcinfo)
    (eprintf "  actual: ~v\n" v1)
    (eprintf "expected: ~v\n" v2)))

(define (print-failed srcinfo)
  (let ([file (list-ref srcinfo 0)]
        [line (list-ref srcinfo 1)]
        [column (list-ref srcinfo 2)]
        [pos (list-ref srcinfo 3)])
    (eprintf "FAILED ~a~a\n"
             (cond
               [(path? file) 
                (let-values ([(base name dir) (split-path file)])
                  (path->string name))]
               [else ""])
             (cond
               [(and line column)
                (format ":~a.~a" line column)]
               [pos 
                (format "::~a" pos)]
               [else #f]))))

(provide (rename-out [-reduction-relation reduction-relation])
         --> fresh with ::= I O ;; macro keywords
         reduction-relation->rule-names
         extend-reduction-relation
         reduction-relation?
         union-reduction-relations
         
         compatible-closure
         context-closure
         
         define-language
         define-extended-language
         
         define-metafunction
         define-metafunction/extension
         define-relation
         define-judgment-form 
         judgment-holds
         
         (rename-out [metafunction-form metafunction])
         metafunction? metafunction-proc
         in-domain?
         current-traced-metafunctions
         metafunc-proc-lang
         metafunc-proc-pict-info
         metafunc-proc-name
         metafunc-proc-multi-arg?
         metafunc-proc-in-dom?
         metafunc-proc-dom-pat
         metafunc-proc-cases
         metafunc-proc-relation?
         metafunc-proc?
         (struct-out metafunc-case)
         
         (struct-out metafunc-extra-side-cond)
         (struct-out metafunc-extra-where)
         (struct-out metafunc-extra-fresh)
         
         (struct-out binds))

(provide test-match
         term-match
         term-match/single
         redex-let 
         redex-let*
         make-bindings bindings-table bindings?
         match? match-bindings
         make-bind bind? bind-name bind-exp
         make-match
         
         test-equal
         test-->>
         test-->
         test-->>∃ (rename-out [test-->>∃ test-->>E])
         test-predicate
         test-results)


(provide language-nts
         apply-reduction-relation
         apply-reduction-relation/tag-with-names
         apply-reduction-relation/tagged
         apply-reduction-relation*
         current-cache-all?
         variable-not-in
         variables-not-in)

(provide relation-coverage
         covered-cases
         (rename-out [fresh-coverage make-coverage])
         coverage?)

;; the AND metafunction is defined here to be used
;; in define-relation so that ellipses work properly
;; across clauses in relations
(define-language L)
(define-metafunction L
  AND : any ... -> any
  [(AND any ...) 
   ,(andmap values (term (any ...)))])
