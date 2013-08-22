#lang racket/base

(require "matcher.rkt"
         "term.rkt"
         "fresh.rkt"
         "error.rkt"
         "search.rkt"
         racket/trace
         racket/stxparam
         "term-fn.rkt"
         "rewrite-side-conditions.rkt"
         (prefix-in pu: "pat-unify.rkt"))

(require
 (for-syntax "rewrite-side-conditions.rkt"
             "term.rkt"
             "term-fn.rkt"
             "loc-wrapper-ct.rkt"
             racket/stxparam-exptime
             racket/base
             racket/syntax
             syntax/id-table
             racket/list
             syntax/parse
             syntax/stx))

(require
 (for-template "term.rkt"))

(struct derivation (term name subs) 
  #:transparent
  #:guard (λ (term name subs struct-name)
            (unless (or (not name) (string? name))
              (raise-argument-error struct-name "(or/c string? #f)" 1 term name subs))
            (unless (and (list? subs)
                         (andmap derivation? subs))
              (raise-argument-error struct-name "derivation?" 2 term name subs))
            (values term name subs)))

;; structs that hold intermediate results when building a derivation
(struct derivation-subs-acc (subs-so-far this-output) #:transparent)
(struct derivation-with-output-only (output name subs) #:transparent)

;; Intermediate structures recording clause "extras" for typesetting.
(define-struct metafunc-extra-side-cond (expr))
(define-struct metafunc-extra-where (lhs rhs))
(define-struct metafunc-extra-fresh (vars))

(begin-for-syntax
  ;; pre: (judgment-form-id? stx) holds
  (define (lookup-judgment-form-id stx)
    (define jf-pe (judgment-form-pending-expansion))
    (if (and jf-pe
             (free-identifier=? (car jf-pe) stx))
        (cdr jf-pe)
        (syntax-local-value stx)))
  (define judgment-form-pending-expansion (make-parameter #f)))

(define-for-syntax (prune-syntax stx)
  (datum->syntax
   (identifier-prune-lexical-context #'whatever '(#%app #%datum))
   (let loop ([stx stx])
     (syntax-case stx ()
       [(a . b)
        (datum->syntax (identifier-prune-lexical-context #'whatever '(#%app))
                       (cons (loop #'a) (loop #'b))
                       stx
                       stx)]
       [x 
        (identifier? #'x)
        (identifier-prune-lexical-context #'x (list (syntax-e #'x) '#%top))]
       [() (datum->syntax #f '() stx)]
       [_ (datum->syntax (identifier-prune-lexical-context #'whatever '(#%datum))
                         (syntax->datum stx)
                         stx
                         stx)]))))

(define-syntax (--> stx) (raise-syntax-error '--> "used outside of reduction-relation"))
(define-syntax (fresh stx) (raise-syntax-error 'fresh "used outside of reduction-relation"))
(define-syntax (with stx) (raise-syntax-error 'with "used outside of reduction-relation"))

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

;; the withs, freshs, and side-conditions come in backwards order
;; rt-lang is an identifier that will be bound to a (runtime) language,
;;   not necc bound via define-language. 
;; ct-lang is an identifier  guaranteed to be bound by define-language
;;   that can be used to call rewrite-side-conditions/check-errs, but
;;   some extension of that language may end up being bound to rt-lang
(define-for-syntax (bind-withs orig-name main rt-lang lang-nts ct-lang stx where-mode body names w/ellipses side-condition-unquoted? jf-results-id)
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
          (with-syntax ([(syncheck-exp side-conditions-rewritten (names ...) (names/ellipses ...))
                         (rewrite-side-conditions/check-errs
                          ct-lang
                          'reduction-relation
                          #t
                          #'x)]
                        [lang-stx rt-lang])
            (define-values (binding-constraints temporaries env+)
              (generate-binding-constraints (syntax->list #'(names ...))
                                            (syntax->list #'(names/ellipses ...))
                                            env
                                            orig-name))
            (with-syntax ([(binding-constraints ...) binding-constraints]
                          [(x ...) temporaries])
              (define rest-body (loop #'(y ...) #`(list x ... #,to-not-be-in) env+))
              #`(begin
                  syncheck-exp
                  (#,(case where-mode
                     [(flatten)
                      #'combine-where-results/flatten]
                     [(predicate)
                      #'combine-where-results/predicate]
                     [else (error 'unknown-where-mode "~s" where-mode)])
                 (match-pattern (compile-pattern #,rt-lang `side-conditions-rewritten #t) (term e #:lang #,ct-lang))
                 (λ (bindings)
                   (let ([x (lookup-binding bindings 'names)] ...)
                     (and binding-constraints ...
                          (term-let ([names/ellipses x] ...) 
                                    #,rest-body)))))))))]
       [((-side-condition s ...) y ...)
        (side-condition-keyword? #'-side-condition)
        (if side-condition-unquoted?
            #`(and s ... #,(loop #'(y ...) to-not-be-in env))
            #`(and (term s) ... #,(loop #'(y ...) to-not-be-in env)))]
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
       [((form-name pats ...) . after)
        (judgment-form-id? #'form-name)
        (let*-values ([(premise) (syntax-case stx () [(p . _) #'p])]
                      [(rest-clauses under-ellipsis?)
                       (syntax-case #'after ()
                         [(maybe-ellipsis . more)
                          (ellipsis? #'maybe-ellipsis)
                          (values #'more #t)]
                         [_ (values #'after #f)])]
                      [(judgment-form) (lookup-judgment-form-id #'form-name)]
                      [(mode) (judgment-form-mode judgment-form)]
                      [(judgment-proc) (judgment-form-proc judgment-form)]
                      [(input-template output-pre-pattern) 
                       (let-values ([(in out) (split-by-mode (syntax->list #'(pats ...)) mode)])
                         (if under-ellipsis?
                             (let ([ellipsis (syntax/loc premise (... ...))])
                               (values #`(#,in #,ellipsis) #`(#,out #,ellipsis)))
                             (values in out)))]
                      [(syncheck-exp output-pattern output-names output-names/ellipses)
                       (with-syntax ([(syncheck-exp output names names/ellipses)
                                      (rewrite-side-conditions/check-errs ct-lang orig-name #t output-pre-pattern)])
                         (values #'syncheck-exp
                                 #'output
                                 (syntax->list #'names)
                                 (syntax->list #'names/ellipses)))]
                      [(binding-constraints temporaries env+)
                       (generate-binding-constraints output-names output-names/ellipses env orig-name)]
                      [(rest-body) (loop rest-clauses #`(list judgment-output #,to-not-be-in) env+)]
                      [(call)
                       (let ([input (quasisyntax/loc premise (term #,input-template #:lang #,ct-lang))])
                         (define (make-traced input)
                           (quasisyntax/loc premise
                             (call-judgment-form 'form-name #,judgment-proc '#,mode #,input #,(if jf-results-id #''() #f))))
                         (if under-ellipsis?
                             #`(repeated-premise-outputs #,input (λ (x) #,(make-traced #'x)))
                             (make-traced input)))])
          (record-disappeared-uses (list #'form-name))
          (with-syntax ([(output-name ...) output-names]
                        [(output-name/ellipsis ...) output-names/ellipses]
                        [(temp ...) temporaries]
                        [(binding-constraint ...) binding-constraints])
            #`(begin
                #,syncheck-exp
                (void #,(defined-check judgment-proc "judgment form" #:external #'form-name))
                (judgment-form-bind-withs/proc
                 #,rt-lang
                 `#,output-pattern
                 #,call
                 #,under-ellipsis?
                 #,jf-results-id
                 (λ (bindings #,(if jf-results-id jf-results-id '_ignored))
                   (let ([temp (lookup-binding bindings 'output-name)] ...)
                     (and binding-constraint ...
                          (term-let ([output-name/ellipsis temp] ...)
                                    #,rest-body))))))))]))))

(define (judgment-form-bind-withs/proc lang output-pattern output under-ellipsis? old-maps do-something)
  (let ([compiled-pattern (compile-pattern lang output-pattern #t)])
    (for/fold ([outputs '()]) ([sub-output (in-list output)])
      (define sub-tree (if under-ellipsis?
                           (map derivation-subs-acc-subs-so-far sub-output)
                           (derivation-subs-acc-subs-so-far sub-output)))
      (define term (if under-ellipsis?
                       (map derivation-subs-acc-this-output sub-output)
                       (derivation-subs-acc-this-output sub-output)))
      (define mtchs (match-pattern compiled-pattern term))
      (if mtchs
          (for/fold ([outputs outputs]) ([mtch (in-list mtchs)])
            (define mtch-outputs (do-something (mtch-bindings mtch) 
                                               (and old-maps
                                                    (if under-ellipsis?
                                                        (append (reverse sub-tree) old-maps)
                                                        (cons sub-tree old-maps)))))
            (if mtch-outputs
                (append mtch-outputs outputs)
                outputs))
          outputs))))

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

(define (call-judgment-form form-name form-proc mode input derivation-init)
  (define traced (current-traced-metafunctions))
  (define vecs
    (if (or (eq? 'all traced) (memq form-name traced))
        (let ([outputs #f])
          (define spacers
            (for/fold ([s '()]) ([m mode])
              (case m [(I) s] [(O) (cons '_ s)])))
          (define (wrapped . _)
            (set! outputs (form-proc form-proc input derivation-init))
            (for/list ([output (in-list outputs)])
              (cons form-name (assemble mode input (derivation-with-output-only-output output)))))
          (apply trace-call form-name wrapped (assemble mode input spacers))
          outputs)
        (form-proc form-proc input derivation-init)))
  (for/list ([v (in-list vecs)])
    (define subs (derivation-with-output-only-subs v))
    (define rulename (derivation-with-output-only-name v))
    (define this-output (derivation-with-output-only-output v))
    (derivation-subs-acc 
     (and subs (derivation (cons form-name (assemble mode input this-output))
                           rulename
                           (reverse subs)))
     this-output)))

(define (assemble mode inputs outputs)
  (let loop ([ms mode] [is inputs] [os outputs])
    (if (null? ms)
        '()
        (case (car ms)
          [(I) (cons (car is) (loop (cdr ms) (cdr is) os))]
          [(O) (cons (car os) (loop (cdr ms) is (cdr os)))]))))

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

(define current-traced-metafunctions (make-parameter '()))

(define-for-syntax (mode-keyword stx)
  (raise-syntax-error #f "keyword invalid outside of mode specification" stx))
(define-syntax I mode-keyword)
(define-syntax O mode-keyword)

(define-for-syntax (check-judgment-arity stx judgment)
  (syntax-case judgment ()
    [(form-name pat ...)
     (judgment-form-id? #'form-name)
     (unless (judgment-form-relation? (lookup-judgment-form-id #'form-name))
       (let ([expected (length (judgment-form-mode (lookup-judgment-form-id #'form-name)))]
             [actual (length (syntax->list #'(pat ...)))])
         (unless (= actual expected)
           (raise-syntax-error 
            #f 
            (format "mode specifies a ~a-ary relation but use supplied ~a term~a" 
                    expected actual (if (= actual 1) "" "s"))
            judgment))))]
    [(form-name pat ...)
     (raise-syntax-error #f "expected a judgment form name" stx #'form-name)]))

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

(define-for-syntax (definition-nts lang orig-stx syn-error-name)
  (unless (identifier? lang)
    (raise-syntax-error #f "expected an identifier in the language position" orig-stx lang))
  (language-id-nts lang syn-error-name))

(define-for-syntax (lhs-lws clauses)
  (with-syntax ([((lhs-for-lw _ ...) ...) clauses])
    (map (λ (x) (to-lw/proc (datum->syntax #f (cdr (syntax-e x)) x)))
         (syntax->list #'(lhs-for-lw ...)))))

(define-syntax (define-judgment-form stx)
  (not-expression-context stx)
  (syntax-case stx ()
    [(def-form-id lang . body)
     (do-extended-judgment-form #'lang (syntax-e #'def-form-id) #'body #f stx #f)]))

(define-syntax (define-extended-judgment-form stx)
  (not-expression-context stx)
  (syntax-case stx ()
    [(def-form-id lang original-id . body)
     (begin
       (unless (judgment-form-id? #'original-id)
         (raise-syntax-error 'define-extended-judgment-form 
                             "expected a judgment form"
                             stx
                             #'original-id))
       (do-extended-judgment-form #'lang 'define-extended-judgment-form #'body #'original-id stx #f))]))

(define-syntax (define-relation stx)
  (syntax-case stx ()
    [(def-form-id lang . body)
     (begin
       (unless (identifier? #'lang)
         (raise-syntax-error #f "expected an identifier in the language position" stx #'lang))
       (define-values (contract-name dom-ctcs codom-contracts pats)
         (split-out-contract stx (syntax-e #'def-form-id) #'body #t))
       (with-syntax* ([((name trms ...) rest ...) (car pats)]
                      [(mode-stx ...) #`(#:mode (name I))]
                      [(ctc-stx ...) (if dom-ctcs 
                                         #`(#:contract (name #,dom-ctcs)) 
                                         #'())]
                      [(clauses ...) pats]
                      [new-body #`(mode-stx ... ctc-stx ... clauses ...)])
                     (do-extended-judgment-form #'lang (syntax-e #'def-form-id) #'new-body #f stx #t)))]))

;; if relation? is true, then the contract is a list of redex patterns
;; if relation? is false, then the contract is a single redex pattern
;;   (meant to match the actual argument as a sequence)
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
                (define-values (raw-clauses rev-codomains pre-condition)
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
                          (values null (cons (car more) codomains) #t)]
                         [else
                          (define kwd (cadr more))
                          (cond
                            [(member (syntax-e kwd) '(or ∨ ∪))
                             (loop kwd 
                                   (cddr more)
                                   (cons (car more) codomains))]
                            [(and (not relation?) (equal? (syntax-e kwd) '#:pre))
                             (when (null? (cddr more)) 
                               (raise-syntax-error 'define-metafunction 
                                                   "expected an expression to follow #:pre keyword"
                                                   kwd))
                             (values (cdddr more)
                                     (cons (car more) codomains)
                                     (caddr more))]
                            [else
                             (values (cdr more)
                                     (cons (car more) codomains)
                                     #t)])])])))
                (let ([doms (reverse dom-pats)]
                      [clauses (check-clauses stx syn-error-name raw-clauses relation?)])
                  (values #'id 
                          (if relation?
                              doms
                              #`(side-condition #,doms (term #,pre-condition)))
                          (reverse rev-codomains)
                          clauses))]
               [else
                (loop (cdr more) (cons (car more) dom-pats))]))])]
       [_
        (raise-syntax-error
         syn-error-name
         (format "expected the name of the ~a, followed by its contract (or no name and no contract)"
                 (if relation? "relation" "meta-function"))
         stx
         rest)])]))

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

(define-for-syntax (do-extended-judgment-form lang syn-err-name body orig stx is-relation?)
  (define nts (definition-nts lang stx syn-err-name))
  (define-values (judgment-form-name dup-form-names mode position-contracts clauses rule-names)
    (parse-judgment-form-body body syn-err-name stx (identifier? orig) is-relation?))
  (define definitions
    #`(begin
        (define-syntax #,judgment-form-name 
          (judgment-form '#,judgment-form-name '#,(cdr (syntax->datum mode)) #'judgment-form-runtime-proc
                         #'mk-judgment-form-proc #'#,lang #'jf-lws
                         '#,rule-names #'judgment-runtime-gen-clauses #'mk-judgment-gen-clauses #'jf-term-proc #,is-relation?))
        (define-values (mk-judgment-form-proc mk-judgment-gen-clauses)
          (compile-judgment-form #,judgment-form-name #,mode #,lang #,clauses #,rule-names #,position-contracts 
                                 #,orig #,stx #,syn-err-name judgment-runtime-gen-clauses))
        (define judgment-form-runtime-proc (mk-judgment-form-proc #,lang))
        (define jf-lws (compiled-judgment-form-lws #,clauses #,judgment-form-name #,stx))
        (define judgment-runtime-gen-clauses (mk-judgment-gen-clauses #,lang (λ () (judgment-runtime-gen-clauses))))
        (define jf-term-proc (make-jf-term-proc #,judgment-form-name #,syn-err-name #,lang #,nts #,mode))))
  (syntax-property
   (values ;prune-syntax
    (if (eq? 'top-level (syntax-local-context))
        ; Introduce the names before using them, to allow
        ; judgment form definition at the top-level.
        #`(begin 
            (define-syntaxes (judgment-form-runtime-proc judgment-runtime-gen-clauses jf-term-proc jf-lws) (values))
            #,definitions)
        definitions))
   'disappeared-use
   (map syntax-local-introduce dup-form-names)))

(define-for-syntax (jf-is-relation? jf-id)
  (judgment-form-relation? (lookup-judgment-form-id jf-id)))

(define-for-syntax (parse-judgment-form-body body syn-err-name full-stx extension? is-relation?)
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
  (define-syntax-class name
    (pattern x #:when (and (not is-relation?)
                           (or (and (symbol? (syntax-e #'x))
                                    (not (horizontal-line? #'x))
                                    (not (eq? '... (syntax-e #'x))))
                               (string? (syntax-e #'x))))))
  (define (parse-rules rules)
    (define-values (backward-rules backward-names)
      (for/fold ([parsed-rules '()]
                 [names '()])
        ([rule rules])
        (syntax-parse rule
          [(prem ... _:horizontal-line n:name conc)
           (values (cons #'(conc prem ...) parsed-rules)
                   (cons #'n names))]
          [(prem ... _:horizontal-line conc)
           (values (cons #'(conc prem ...) parsed-rules)
                   (cons #f names))]
          [(conc prem ... n:name)
           (values (cons #'(conc prem ...) parsed-rules)
                   (cons #'n names))]
          [else
           (values (cons rule parsed-rules)
                   (cons #f names))])))
    (values (reverse backward-rules)
            (reverse backward-names)))
  (define-values (name/mode mode-stx name/contract contract rules rule-names)
    (syntax-parse body #:context full-stx
      [((~or (~seq #:mode ~! mode:mode-spec)
             (~seq #:contract ~! contract:contract-spec))
        ...
        rule:expr ...)
       (let-values ([(name/mode mode)
                     (syntax-parse #'(mode ...)
                                   [((name the-mode ...)) (values #'name (car (syntax->list #'(mode ...))))]
                                   [_ 
                                    (raise-syntax-error 
                                     #f
                                     (if (null? (syntax->list #'(mode ...)))
                                         "expected definition to include a mode specification"
                                         "expected definition to include only one mode specification")
                                     full-stx)])]
                    [(name/ctc ctc)
                     (syntax-parse #'(contract ...)
                                   [() (values #f #f)]
                                   [((name . contract)) (values #'name (syntax->list #'contract))]
                                   [(_ . dups)
                                    (raise-syntax-error 
                                     syn-err-name "expected at most one contract specification"
                                     #f #f (syntax->list #'dups))])])
         (define-values (parsed-rules rule-names) (parse-rules (syntax->list #'(rule ...)))) 
         (values name/mode mode name/ctc ctc parsed-rules rule-names))]))
  (check-clauses full-stx syn-err-name rules #t)
  (check-dup-rule-names full-stx syn-err-name rule-names)
  (check-arity-consistency mode-stx contract full-stx)
  (define-values (form-name dup-names)
    (syntax-case rules ()
      [() 
       (not extension?)
       (raise-syntax-error #f "expected at least one rule" full-stx)]
      [_ (defined-name (list name/mode name/contract) rules full-stx)]))
  (define string-rule-names
    (for/list ([name (in-list rule-names)])
      (cond
        [(not name) name]
        [(symbol? (syntax-e name))
         (symbol->string (syntax-e name))]
        [else (syntax-e name)])))
  (values form-name dup-names mode-stx contract rules string-rule-names))

;; names : (listof (or/c #f syntax[string]))
(define-for-syntax (check-dup-rule-names full-stx syn-err-name names)
  (define tab (make-hash))
  (for ([name (in-list names)])
    (when (syntax? name)
      (define k (if (symbol? (syntax-e name))
                    (symbol->string (syntax-e name))
                    (syntax-e name)))
      (hash-set! tab k (cons name (hash-ref tab k '())))))
  (for ([(k names) (in-hash tab)])
    (unless (= 1 (length names))
      (raise-syntax-error syn-err-name
                          "duplicate rule names"
                          (car names) #f (cdr names)))))
                          
(define-for-syntax (check-arity-consistency mode-stx contracts full-def)
  (when (and contracts (not (= (length (cdr (syntax->datum mode-stx)))
                               (length contracts))))
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

(define-syntax (make-jf-term-proc stx)
  (syntax-case stx ()
    [(_  jdg-name syn-err-name lang nts mode)
     (if (member 'O (syntax->datum #'mode))
         #'(λ (_)
             (error 'syn-err-name "judgment forms with output mode positions cannot currently be used in term"))
         (with-syntax* ([(binding ...) (generate-temporaries (cdr (syntax->list #'mode)))]
                        [(input) (generate-temporaries (list #'input))]
                        [body-stx (bind-withs #'syn-err-name '() #'lang (syntax->datum #'nts) #'lang
                                              (if (jf-is-relation? #'jdg-name)
                                                  (list #'(jdg-name ((unquote-splicing input))))
                                                  (list #'(jdg-name (unquote binding) ...)))
                                              'flatten
                                              #`(list #t)
                                              '()
                                              '()
                                              #f
                                              #f)])
                       (if (jf-is-relation? #'jdg-name)
                           #`(λ (input)
                               (not (null? body-stx)))
                           #`(λ (input)
                               (call-with-values 
                                (λ () (apply values input))
                                (λ (binding ...)
                                  (not (null? body-stx))))))))]))

(define-syntax (judgment-holds/derivation stx)
  (syntax-case stx ()
    [(_ stx-name derivation? judgment)
     #`(not (null? #,(syntax/loc stx (judgment-holds/derivation stx-name derivation? judgment #t))))]
    [(_ stx-name derivation? (form-name . pats) tmpl)
     (and (judgment-form-id? #'form-name)
          (when (jf-is-relation? #'form-name)
            (raise-syntax-error (syntax-e #'stx-name) "relations not allowed" #'form-name)))
     (let* ([syn-err-name (syntax-e #'stx-name)]
            [lang (judgment-form-lang (lookup-judgment-form-id #'form-name))]
            [nts (definition-nts lang stx syn-err-name)]
            [judgment (syntax-case stx () [(_ _ _ judgment _) #'judgment])]
            [derivation? (syntax-e #'derivation?)]
            [id-or-not (if derivation?
                           (car (generate-temporaries '(jf-derivation-lst)))
                           #f)]
            [main-stx
             (bind-withs syn-err-name '() lang nts lang
                         (list judgment)
                         'flatten
                         (if derivation?
                             id-or-not
                             #`(list (term #,#'tmpl #:lang #,lang)))
                         '()
                         '()
                         #f
                         id-or-not)])
       (check-judgment-arity stx judgment)
       (syntax-property
        (if id-or-not
            #`(let ([#,id-or-not '()])
                #,main-stx)
            #`(sort #,main-stx
                    string<=?
                    #:key (λ (x) (format "~s" x))))
        'disappeared-use
        (syntax-local-introduce #'form-name)))]
    [(_ stx-name derivation? (not-form-name . _) . _)
     (not (judgment-form-id? #'form-name))
     (raise-syntax-error (syntax-e #'stx-name) "expected a judgment form name" #'not-form-name)]
    [(_ stx-name . whatever)
     (raise-syntax-error (syntax-e #'stx-name)
                         "bad syntax"
                         stx)]))

(define-syntax (judgment-holds stx)
  (syntax-case stx ()
    [(_  (jf-id . args))
     #`(#%expression (judgment-holds/derivation judgment-holds #f #,(stx-car (stx-cdr stx))))]
    [(_  (jf-id . args) trm)
     #`(#%expression (judgment-holds/derivation judgment-holds #f #,(stx-car (stx-cdr stx)) trm))]))

(define-syntax (build-derivations stx)
  (syntax-case stx ()
    [(_  jf-expr)
     #'(#%expression (judgment-holds/derivation build-derivations #t jf-expr any))]))

(define-for-syntax (do-compile-judgment-form-proc name mode-stx clauses rule-names contracts nts orig lang stx syn-error-name)
  (with-syntax ([(init-jf-derivation-id) (generate-temporaries '(init-jf-derivation-id))])
    (define mode (cdr (syntax->datum mode-stx)))
    (define-values (input-contracts output-contracts)
      (if contracts
          (let-values ([(ins outs) (split-by-mode contracts mode)])
            (values ins outs))
          (values #f #f)))
    (define (compile-clause clause clause-name)
      (syntax-case clause ()
        [((_ . conc-pats) . prems)
         (let-values ([(input-pats output-pats) (split-by-mode (syntax->list #'conc-pats) mode)])
           (with-syntax ([(lhs-syncheck-exp lhs (names ...) (names/ellipses ...)) (rewrite-side-conditions/check-errs lang syn-error-name #t input-pats)]
                         [(jf-derivation-id) (generate-temporaries '(jf-derivation-id))])
             (define (contracts-compilation ctcs)
               (with-syntax ([(ctc ...) ctcs])
                 #`(list (compile-pattern lang `ctc #f) ...)))
             (define body
               (parameterize ([judgment-form-pending-expansion
                               (cons name
                                     (struct-copy judgment-form (lookup-judgment-form-id name)
                                                  [proc #'recur]))])
                 (bind-withs syn-error-name '() lang nts lang
                             (syntax->list #'prems) 
                             'flatten #`(list (derivation-with-output-only (term (#,@output-pats) #:lang #,lang)
                                                                           #,clause-name
                                                                           jf-derivation-id))
                             (syntax->list #'(names ...))
                             (syntax->list #'(names/ellipses ...))
                             #f
                             #'jf-derivation-id)))
             (with-syntax ([(compiled-lhs compiled-input-ctcs compiled-output-ctcs)
                            (generate-temporaries '(compiled-lhs compiled-input-ctcs compiled-output-ctcs))])
               
               #`(
                  ;; pieces of a 'let' expression to be combined: first some bindings
                  ([compiled-lhs (compile-pattern lang `lhs #t)]
                   #,@(if input-contracts
                          (list #`[compiled-input-ctcs #,(contracts-compilation input-contracts)])
                          (list))
                   #,@(if output-contracts
                          (list #`[compiled-output-ctcs #,(contracts-compilation output-contracts)])
                          (list)))
                  ;; and then the body of the let, but expected to be behind a (λ (input) ...).
                  (let ([jf-derivation-id init-jf-derivation-id])
                    (begin
                      lhs-syncheck-exp
                      #,@(if input-contracts
                             (list #`(check-judgment-form-contract '#,name input compiled-input-ctcs 'I '#,mode))
                             (list))
                      (combine-judgment-rhses
                       compiled-lhs
                       input
                       (λ (bnds)
                         (term-let ([names/ellipses (lookup-binding bnds 'names)] ...)
                                   #,body))
                       #,(if output-contracts
                             #`(λ (output)
                                 (check-judgment-form-contract '#,name output compiled-output-ctcs 'O '#,mode))
                             #`void))))))))]))
  
    (when (identifier? orig)
      (define orig-mode (judgment-form-mode (lookup-judgment-form-id orig)))
      (unless (equal? mode orig-mode)
        (raise-syntax-error syn-error-name
                            (format 
                             "mode for extended judgment form does not match original mode; got ~s for the original and ~s for the extension"
                             `(,(syntax-e orig) ,@orig-mode)
                             `(,(syntax-e name) ,@mode))
                            stx
                            mode-stx)))
    
    (with-syntax ([(((clause-proc-binding ...) clause-proc-body) ...) (map compile-clause clauses rule-names)])
      (with-syntax ([(clause-proc-body-backwards ...) (reverse (syntax->list #'(clause-proc-body ...)))])
        (if (identifier? orig)
            (with-syntax ([orig-mk (judgment-form-mk-proc (lookup-judgment-form-id orig))])
              #`(λ (lang)
                  (let (clause-proc-binding ... ...)
                    (let ([prev (orig-mk lang)])
                      (λ (recur input init-jf-derivation-id)
                        (append (prev recur input init-jf-derivation-id)
                                clause-proc-body-backwards ...))))))
            #`(λ (lang)
                (let (clause-proc-binding ... ...)
                  (λ (recur input init-jf-derivation-id)
                    (append clause-proc-body-backwards ...)))))))))

(define (combine-judgment-rhses compiled-lhs input rhs check-output)
  (define mtchs (match-pattern compiled-lhs input))
  (cond
    [mtchs
     (define output-table (make-hash))
     (for ([m (in-list mtchs)])
       (define os (rhs (mtch-bindings m)))
       (when os
         (for ([x (in-list os)])
           (hash-set! output-table x #t))))
     (define outputs (hash-map output-table (λ (k v) k)))
     (for ([output (in-list outputs)])
       (check-output output))
     outputs]
    [else '()]))

(define-for-syntax (do-compile-judgment-form-lws clauses jf-name-stx full-def)
  (syntax-case clauses ()
    [(((_ . conc-body) prems ...) ...)
     (with-syntax ([((rhss ...) (sc/ws ...)) (if (jf-is-relation? jf-name-stx)
                                                 (with-syntax ([(((rhses ...) (where/sc ...)) ...)
                                                                (relation-split-out-rhs #'((prems ...) ...) full-def)])
                                                   #'(((rhses ...) ...) ((where/sc ...) ...)))
                                                 (let ([rev-premss
                                                        ; for consistency with metafunction extras
                                                        (for/list ([prems (syntax->list #'((prems ...) ...))])
                                                          (reverse (syntax->list prems)))]
                                                       [no-rhss (map (λ (_) '()) clauses)])
                                                   (list no-rhss rev-premss)))])
       #`(generate-lws #t (conc-body ...) #,(lhs-lws clauses) (sc/ws ...) (rhss ...) #f))]))

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

(define (check-judgment-form-contract form-name term+trees contracts mode modes)
  (define terms (if (eq? mode 'O)
                    (derivation-with-output-only-output term+trees)
                    term+trees))
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
       (if (judgment-form-relation? (lookup-judgment-form-id #'form-name))
           (values (list) (list #'(body)))
           (split-by-mode (syntax->list #'body) 
                      (judgment-form-mode
                       (lookup-judgment-form-id #'form-name))))]))
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
               [(-side-condition tmpl)
                (side-condition-keyword? #'-side-condition)
                (begin (tmpl-pos #'tmpl acc)
                       acc)]
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
    [(_ relation? seq-of-lhs seq-of-lhs-for-lw seq-of-tl-side-cond/binds seq-of-rhs side-condition-unquoted?)
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
                                 (let ([l (syntax->list #'pat)])
                                   (and l (andmap identifier? (syntax->list #'pat)))))
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
                           #,(if (syntax-e #'side-condition-unquoted?)
                                 (to-lw/uq/proc #'x)
                                 (to-lw/proc #'x)))]
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
                       (let*-values ([(mode) (judgment-form-mode (lookup-judgment-form-id #'form-name))]
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

(define-syntax (compile-judgment-form stx)
  (syntax-case stx ()
    [(_ judgment-form-name mode-arg lang raw-clauses rule-names ctcs orig full-def syn-err-name judgment-form-runtime-gen-clauses)
     (let ([nts (definition-nts #'lang #'full-def (syntax-e #'syn-err-name))]
           [rule-names (syntax->datum #'rule-names)]
           [syn-err-name (syntax-e #'syn-err-name)]
           [clauses (rewrite-relation-prems
                     (if (jf-is-relation? #'judgment-form-name)
                         (fix-relation-clauses (syntax-e #'judgment-form-name) (syntax->list #'raw-clauses))
                         (syntax->list #'raw-clauses)))]
           [mode (cdr (syntax->datum #'mode-arg))])
       (unless (jf-is-relation? #'judgment-form-name) 
         (mode-check (cdr (syntax->datum #'mode-arg)) clauses nts syn-err-name stx))
       (define-values (syncheck-exprs contracts)
         (syntax-case #'ctcs ()
           [#f (values '() #f)]
           [(p ...)
            (let loop ([pats (syntax->list #'(p ...))]
                       [ctcs '()]
                       [syncheck-exps '()])
              (cond
                [(null? pats) (values syncheck-exps (reverse ctcs))]
                [else
                 (define pat (car pats))
                 (with-syntax ([(syncheck-exp pat (names ...) (names/ellipses ...)) 
                                (rewrite-side-conditions/check-errs #'lang #'syn-error-name #f pat)])
                   (loop (cdr pats)
                         (cons #'pat ctcs)
                         (cons #'syncheck-exp syncheck-exps)))]))]))
       (define proc-stx (do-compile-judgment-form-proc #'judgment-form-name
                                                       #'mode-arg
                                                       clauses
                                                       rule-names
                                                       contracts
                                                       nts
                                                       #'orig
                                                       #'lang
                                                       #'full-def
                                                       syn-err-name))
       (define gen-stx (with-syntax* ([(comp-clauses ...) 
                                       (map (λ (c) (compile-gen-clause c mode syn-err-name 
                                                                       #'judgment-form-name #'lang)) 
                                            clauses)])
                                     (if (identifier? #'orig)
                                         (with-syntax ([prev-mk (judgment-form-mk-gen-clauses (lookup-judgment-form-id #'orig))])
                                           #`(λ (effective-lang judgment-form-runtime-gen-clauses)
                                               (define mk-prev-clauses (prev-mk effective-lang judgment-form-runtime-gen-clauses))
                                               (λ ()
                                                 (append 
                                                  (mk-prev-clauses)
                                                  #,(check-pats
                                                     #'(list comp-clauses ...))))))
                                         #`(λ (effective-lang judgment-form-runtime-gen-clauses)
                                             (λ ()
                                               #,(check-pats
                                                  #'(list comp-clauses ...)))))))
       #`(begin #,@syncheck-exprs (values #,proc-stx #,gen-stx)))]))

(define-for-syntax (rewrite-relation-prems clauses)
  (map (λ (c)
         (syntax-case c ()
           [(conc prems ...)
            (with-syntax ([(new-prems ...) (map (λ (p)
                                                  (syntax-case p ()
                                                    [(r-name rest ...)
                                                     (and (identifier? #'r-name)
                                                          (judgment-form-id? #'r-name)
                                                          (jf-is-relation? #'r-name))
                                                     #'(r-name (rest ...))]
                                                    [else
                                                     p]))
                                                (syntax->list #'(prems ...)))])
              #'(conc new-prems ...))]))
       clauses))

(define-for-syntax (fix-relation-clauses name raw-clauses)
  (map (λ (clause-stx)
         (define (fix-rule rule-stx)
           (syntax-case rule-stx ()
             [(rule-name rest ...)
              (and (identifier? #'rule-name)
                   (judgment-form-id? #'rule-name))
              #'(rule-name rest ...)]
             [rule
              #'(side-condition rule)]))
         (let loop ([c-stx clause-stx]
                    [new-c-stx '()]
                    [extra-stx '()])
           (syntax-case c-stx ()
             [()
              (let* ([c-rev (reverse new-c-stx)]
                     [conclusion (syntax-case (car c-rev) ()
                                   [(r-name rest ...)
                                    #'(r-name (rest ...))])])
                (with-syntax ([(cls ...) (cons conclusion (append (reverse extra-stx) (cdr c-rev)))])
                  #'(cls ...)))]
             [((where ext-rest ...) rest ...)
              (where-keyword? #'where)
              (loop #'(rest ...)
                    new-c-stx
                    (cons #'(where ext-rest ...) extra-stx))]
             [((side-con ext-rest ...) rest ...)
              (side-condition-keyword? #'side-con)
              (loop #'(rest ...)
                    new-c-stx
                    (cons #'(side-con (unquote ext-rest ...)) extra-stx))]
             [(rule ellipsis rest ...)
              (ellipsis? #'ellipsis)
              (loop #'(rest ...)
                    (cons #'ellipsis (cons (fix-rule #'rule) new-c-stx))
                    extra-stx)]
             [(rule rest ...)
              (loop #'(rest ...)
                    (cons (fix-rule #'rule) new-c-stx)
                    extra-stx)])))
       raw-clauses))

(define-syntax (compile-judgment-gen-clauses stx)
  (syntax-case stx ()
    [(_ judgment-form-name mode-arg lang clauses ctcs orig full-def syn-err-name judgment-form-runtime-gen-clauses)
     (let ([clauses (syntax->list #'clauses)]
           [nts (definition-nts #'lang #'full-def (syntax-e #'syn-err-name))]
           [syn-err-name (syntax-e #'syn-err-name)]
           [mode (cdr (syntax->datum #'mode-arg))])
       (with-syntax* ([(comp-clauses ...) 
                       (map (λ (c) (compile-gen-clause c mode syn-err-name 
                                                       #'judgment-form-name #'lang)) 
                            clauses)])
                     (if (identifier? #'orig)
                         (with-syntax ([prev-mk (judgment-form-mk-gen-clauses (lookup-judgment-form-id #'orig))])
                           #`(λ (effective-lang judgment-form-runtime-gen-clauses)
                               (define mk-prev-clauses (prev-mk effective-lang judgment-form-runtime-gen-clauses))
                               (λ ()
                                 (append 
                                  (mk-prev-clauses)
                                  #,(check-pats
                                     #'(list comp-clauses ...))))))
                         #`(λ (effective-lang judgment-form-runtime-gen-clauses)
                             (λ ()
                               #,(check-pats
                                  #'(list comp-clauses ...)))))))]))

(define-syntax (compiled-judgment-form-lws stx)
  (syntax-case stx ()
    [(_ clauses name def-stx)
     (do-compile-judgment-form-lws (syntax->list #'clauses) #'name #'def-stx)]))

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



(define-for-syntax (name-pattern-lws pat)
  (map (λ (x) (cons (to-lw/proc (car x)) (to-lw/proc (cdr x))))
       (extract-pattern-binds pat)))

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

(define-for-syntax (split-by-mode xs mode)
  (for/fold ([ins '()] [outs '()])
    ([x (reverse xs)]
     [m (reverse mode)])
    (case m
      [(I) (values (cons x ins) outs)]
      [(O) (values ins (cons x outs))]
      [else (error 'split-by-mode "ack ~s" m)])))

(define-for-syntax (fuse-by-mode ins outs mode)
  (let loop ([is (reverse ins)]
             [os (reverse outs)]
             [ms (reverse mode)]
             [res '()])
    (define err (λ () (error 'fuse-by-mode "mismatched mode and split: ~s ~s ~s" ins outs mode)))
    (cond
      [(and (empty? ms)
            (empty? is)
            (empty? os))
       res]
      [(empty? ms)
       (err)]
      [else
       (case (car ms)
         [(I) (if (empty? is)
                  (err)
                  (loop (cdr is) os (cdr ms)
                        (cons (car is) res)))]
         [(O) (if (empty? os)
                  (err)
                  (loop is (cdr os) (cdr ms)
                        (cons (car os) res)))]
         [else (error 'fuse-by-mode "ack ~s" (car ms))])])))

(define-for-syntax (ellipsis? stx)
  (and (identifier? stx)
       (free-identifier=? stx (quote-syntax ...))))

(define-for-syntax (where-keyword? id)
  (and (identifier? id)
       (or (free-identifier=? id #'where)
           (free-identifier=? id #'where/hidden))))
(define-for-syntax (side-condition-keyword? id)
  (and (identifier? id)
       (or (free-identifier=? id #'side-condition)
           (free-identifier=? id #'side-condition/hidden))))
;                                                                        
;                                                                        
;                                                      ;                 
;                                              ;                         
;    ;; ;;  ;;;  ;; ;;    ;;;   ;; ;;   ;;;   ;;;;;  ;;;     ;;;  ;; ;;  
;   ;  ;;  ;   ;  ;;  ;  ;   ;   ;;    ;   ;   ;       ;    ;   ;  ;;  ; 
;   ;   ;  ;;;;;  ;   ;  ;;;;;   ;      ;;;;   ;       ;    ;   ;  ;   ; 
;   ;   ;  ;      ;   ;  ;       ;     ;   ;   ;       ;    ;   ;  ;   ; 
;   ;   ;  ;      ;   ;  ;       ;     ;   ;   ;   ;   ;    ;   ;  ;   ; 
;    ;;;;   ;;;; ;;; ;;;  ;;;;  ;;;;;   ;;;;;   ;;;  ;;;;;   ;;;  ;;; ;;;
;       ;                                                                
;    ;;;                                                                 
;                                                                        
;                                                                        




(define-for-syntax (compile-gen-clause clause-stx mode syn-error-name jdg-name lang)
  (syntax-case clause-stx ()
    [((conc-name . conc-body-raw) prems ...)
     (let*-values ([(conc/+ conc/-) (split-by-mode (syntax->list #'conc-body-raw) mode)]
                   [(syncheck-exps conc/+rw names) (rewrite-pats conc/+ lang)]
                   [(ps-rw eqs p-names) (rewrite-prems #t (syntax->list #'(prems ...)) names lang 'define-judgment-form)]
                   [(conc/-rw conc-mfs) (rewrite-terms conc/- p-names)]
                   [(conc) (fuse-by-mode conc/+rw conc/-rw mode)])
       (with-syntax ([(c ...) conc]
                     [(c-mf ...) conc-mfs]
                     [(eq ...) eqs]
                     [(prem-bod ...) (reverse ps-rw)])
         #`(begin #,@syncheck-exps
                  (clause '(list c ...) (list eq ...) (list c-mf ... prem-bod ...) effective-lang '#,jdg-name))))]))


(define-for-syntax (rewrite-prems in-judgment-form? prems names lang what)
  (define (rewrite-jf prem-name prem-body ns ps-rw eqs)
    (define p-form (lookup-judgment-form-id prem-name))
    (define p-mode (judgment-form-mode p-form))
    (define p-clauses (judgment-form-gen-clauses p-form))
    (define-values (p/-s p/+s) (split-by-mode (syntax->list prem-body) p-mode))
    (define-values (p/-rws mf-apps) (rewrite-terms p/-s ns in-judgment-form?))
    (define-values (syncheck-exps p/+rws new-names) (rewrite-pats p/+s lang))
    (define p-rw (fuse-by-mode p/-rws p/+rws p-mode))
    (with-syntax ([(p ...) p-rw])
      (values (cons #`(begin
                        #,@syncheck-exps
                        (prem #,p-clauses '(list p ...)))
                    (append mf-apps ps-rw))
              eqs
              (append ns new-names))))
  (define-values (prems-rev new-eqs new-names)
    (for/fold ([ps-rw '()] 
               [eqs '()]
               [ns names])
      ([prem prems])
      (syntax-case prem ()
        [(-where pat term)
         (where-keyword? #'-where)
         (let-values ([(term-rws mf-cs) (rewrite-terms (list #'term) ns in-judgment-form?)])
           (with-syntax ([(syncheck-exp pat-rw new-names) (rewrite/pat #'pat lang)])
             (values (append mf-cs ps-rw)
                     (cons #`(begin syncheck-exp (eqn 'pat-rw '#,(car term-rws))) eqs)
                     (append (syntax->datum #'new-names) ns))))]
        [(side-cond rest)
         (side-condition-keyword? #'side-cond)
         (if in-judgment-form?
             (let-values ([(term-rws mf-cs) (rewrite-terms (list #'rest) ns in-judgment-form?)])
               (values (append mf-cs ps-rw)
                       (cons #`(dqn '() #f '#,(car term-rws)) eqs)
                       ns))
             (values ps-rw eqs ns))]
        [(prem-name . prem-body)
         (and (judgment-form-id? #'prem-name) in-judgment-form?)
         (rewrite-jf #'prem-name #'prem-body ns ps-rw eqs)]
        [(judgment-holds (prem-name . prem-body))
         (and (judgment-form-id? #'prem-name) (not in-judgment-form?))
         (rewrite-jf #'prem-name #'prem-body ns ps-rw eqs)]
        [var
         (eq? '... (syntax-e #'var))
         ;; TODO - fix when implementing ellipses
         (values ps-rw eqs ns)]
        [else (raise-syntax-error what "malformed premise" prem)])))
  (values prems-rev new-eqs new-names))

(define-for-syntax (rewrite-pats pats lang)
  (with-syntax ([((syncheck-exp pat-rw (names ...)) ...) 
                 (map (λ (p) (rewrite/pat p lang))
                      pats)])
    (values #'(syncheck-exp ...)
            (syntax->list #'(pat-rw ...))
            (remove-duplicates (syntax->datum #'(names ... ...))))))
                  

(define-for-syntax (rewrite/pat pat lang)
  (with-syntax ([(syncheck-exp body (names ...) (names/ellipses ...))
                 (rewrite-side-conditions/check-errs lang #'rewrite/pat #t pat)])
    #'(syncheck-exp body (names ...))))

(define-for-syntax (rewrite-terms terms names [reverse-mfs? #f])
  (define maybe-rev (if reverse-mfs? reverse values))
  (with-syntax* ([((term-pattern ((res-pat ((metafunc f) args-pat)) ...) body-pat) ...)
                  (map (λ (t) (term-rewrite t names)) terms)]
                 [((mf-clauses ...) ...) (map (λ (fs) 
                                                (map (λ (f-id)
                                                       (with-syntax ([f-id f-id])
                                                         (if (judgment-form-id? #'f-id)
                                                             #'(error 'generate-term "generation disabled for relations in term positions")
                                                             #'(metafunc-proc-gen-clauses f-id))))
                                                     (syntax->list fs)))
                                              (syntax->list #'((f ...) ...)))])
                (values (syntax->list #'(body-pat ...))
                        (maybe-rev (syntax->list #'((prem mf-clauses '(list args-pat res-pat)) ... ...))))))

(define unsupported-pat-err-name (make-parameter #f))

(define-for-syntax (check-pats stx)
  (cond
    [(has-unsupported-pat? stx) 
     =>
     (λ (bad-stx)
       #`(error (unsupported-pat-err-name) "generation failed at unsupported pattern: ~s" #,bad-stx))]
    [else
     stx]))

(define-for-syntax (has-unsupported-pat? stx)
  (syntax-case stx (repeat side-condition in-hole undatum-splicing)
    [(repeat . rest)
     (and (identifier? #'repeat)
          (eq? (syntax-e #'repeat) 'repeat))
     #''(repeat . rest)]
    [(side-condition . rest)
     (and (identifier? #'side-condition)
          (eq? (syntax-e #'side-condition) 'side-condition))
     #''(side-condition . rest)]
    [(in-hole . rest)
     (and (identifier? #'in-hole)
          (eq? (syntax-e #'in-hole) 'in-hole))
     #''(in-hole . rest)]
    [(undatum . rest)
     (and (identifier? #'undatum)
          (eq? (syntax-e #'undatum) 'undatum))
     #''(undatum . rest)]
    [(undatum-splicing .rest)
     (and (identifier? #'undatum-splicing)
          (eq? (syntax-e #'undatum-splicing) 'undatum-splicing))
     #''(undatum-splicing .rest)]
    [(elems ...)
     (for/or ([e (in-list (syntax->list #'(elems ...)))])
       (has-unsupported-pat? e))]
    [_
     #f]))

(provide define-judgment-form 
         define-relation
         define-extended-judgment-form
         judgment-holds
         build-derivations
         generate-lws
         (struct-out derivation)
         (for-syntax extract-term-let-binds
                     name-pattern-lws
                     extract-pattern-binds
                     check-clauses
                     split-by-mode
                     where-keyword?
                     side-condition-keyword?
                     ellipsis?
                     visible-extras
                     judgment-form-id?
                     lookup-judgment-form-id
                     split-out-contract))


(provide --> fresh with I O ;; macro keywords
         (for-syntax prune-syntax
                     side-condition-keyword?
                     bind-withs
                     split-by-mode)
         current-traced-metafunctions
         
         (struct-out metafunc-extra-side-cond)
         (struct-out metafunc-extra-where)
         (struct-out metafunc-extra-fresh))

(provide unsupported-pat-err-name
         (for-syntax rewrite-terms
                     currently-expanding-term-fn
                     rewrite-prems
                     with-syntax*
                     definition-nts
                     check-pats
                     relation-split-out-rhs))
