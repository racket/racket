#lang racket/base

(require "matcher.rkt"
         "term.rkt"
         "fresh.rkt"
         "error.rkt"
         racket/trace)

(require
 (for-syntax "rewrite-side-conditions.rkt"
             "term-fn.rkt"
             "loc-wrapper-ct.rkt"
             racket/base
             racket/syntax
             syntax/id-table
             racket/list
             syntax/parse))

;; Intermediate structures recording clause "extras" for typesetting.
(define-struct metafunc-extra-side-cond (expr))
(define-struct metafunc-extra-where (lhs rhs))
(define-struct metafunc-extra-fresh (vars))

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
        (identifier-prune-lexical-context #'x (list (syntax-e #'x) '#%top))]
       [() (datum->syntax #f '() stx)]
       [_ (datum->syntax (identifier-prune-lexical-context #'whatever '(#%datum))
                         (syntax->datum stx) stx)]))))

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
(define-for-syntax (bind-withs orig-name main lang lang-nts stx where-mode body names w/ellipses side-condition-unquoted?)
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
                             'flatten #`(list (term #,#'tmpl)) '() '() #f)
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
                         (syntax->list #'(names/ellipses ...))
                         #f))
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
       #`(generate-lws #t (conc-body ...) #,(lhs-lws clauses) #,rev-premss #,no-rhss #f))]))

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

(define-for-syntax (ellipsis? stx)
  (and (identifier? stx)
       (free-identifier=? stx (quote-syntax ...))))


(define-for-syntax (where-keyword? id)
  (or (free-identifier=? id #'where)
      (free-identifier=? id #'where/hidden)))
(define-for-syntax (side-condition-keyword? id)
  (or (free-identifier=? id #'side-condition)
      (free-identifier=? id #'side-condition/hidden)))

(provide define-judgment-form 
         judgment-holds
         generate-lws
         (for-syntax extract-term-let-binds
                     name-pattern-lws
                     extract-pattern-binds
                     check-clauses
                     split-by-mode
                     where-keyword?
                     side-condition-keyword?
                     ellipsis?
                     visible-extras))


(provide --> fresh with I O ;; macro keywords
         (for-syntax prune-syntax
                     side-condition-keyword?
                     bind-withs
                     split-by-mode)
         current-traced-metafunctions
         
         (struct-out metafunc-extra-side-cond)
         (struct-out metafunc-extra-where)
         (struct-out metafunc-extra-fresh))
