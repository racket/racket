#lang racket/base
(require racket/list
         racket/match
         racket/string
         "../host/correlate.rkt"
         "../common/set.rkt"
         "../compile/side-effect.rkt"
         "../compile/known.rkt"
         "../run/status.rkt"
         (prefix-in bootstrap: "../run/linklet.rkt")
         "symbol.rkt"
         "defn.rkt"
         "defn-known.rkt"
         "known-primitive.rkt")

(provide simplify-definitions
         simplify-expr)

(define (union-all . args)
  (if (null? args)
      (seteq)
      (set-union (car args) (apply union-all (cdr args)))))

;; Compute the variables that are the target of a set! in e. Note
;; that, at this point, the linklet has been modified so all bindings
;; have unique identifiers, so we can equate the binding with the
;; identifier.
;;
;; In some sense, this is only used for performance optimizations;
;; if `mutated-vars` said "everything is potentially mutated!", that
;; would not change the meaning of programs. However, some of those
;; optimizations are actually quite important in practice. Specifically,
;; knowing that a variable is not mutated enables (soundly) folding
;; `(variable-reference-constant? (#%variable-reference v))` into #t,
;; which, after some further optimizations, allows keyword procedures
;; to use the optimized calling convention. In fact, the analysis is
;; sufficient for *all* keyword procedures to get optimized this way
;; for the code that we need to schemify, which then enables dropping
;; all of the keyword procedure handling as unused.
(define (mutated-vars e)
  (match e
    [`(set! ,i ,e) (set-add (mutated-vars e) i)]
    [`(let-values ([(,name ...) ,es] ...) ,e)
     (apply union-all (mutated-vars e) (map mutated-vars es))]
    [`(letrec-values ([(,name* ...) ,bound-exprs] ...) ,body-expr)
     (define names (apply union-all (map list->seteq name*)))
     (define names-because-implicit-assignments/or-false
       (for/or ([bound-expr (in-list bound-exprs)])
         (match bound-expr
           [`(lambda ,_ ,_) #f]
           [`(case-lambda ,_ ...) #f]
           [`(let-values ([(,_ ...) (lambda ,_ ,_)] ...) (case-lambda ,_ ...))
            ; ^-- desugared form of a lambda with optional args
            #f]
           [els #:when (set-empty? (set-intersect (frees els) names)) #f]
           [els
            (reject-suspect-letrec e)
            ;; Using all of `names` here is sound, and a more precise analysis
            ;; would not be useful. See comments on GH PR 5272
            names])))
     (apply union-all
            (mutated-vars body-expr)
            (or names-because-implicit-assignments/or-false (seteq))
            (map mutated-vars bound-exprs))]
    [`(lambda ,args ,e) (mutated-vars e)]
    [`(case-lambda [,args ,e] ...) (apply union-all (map mutated-vars e))]
    [`(,sym ,e ...)
     #:when (memq sym '(begin begin0 with-continuation-mark if))
     (apply union-all (map mutated-vars e))]
    [(? symbol? e) (seteq)]
    [`(quote ,_) (seteq)]
    [e #:when (or (boolean? e) (number? e) (string? e) (bytes? e))
       (seteq)]
    [(list app ...) (apply union-all (map mutated-vars app))]))

;; compute the free variables of e
(define (frees e)
  (match e
    [(? symbol?) (set e)]
    [`(let-values ,cl ,e)
     (define cl* (map (lambda (c)  (frees (cadr c))) cl))
     (define binds (apply seteq (apply append (map car cl))))
     (set-union (apply union-all cl*) (set-remove (frees e) binds))]
    [`(letrec-values ,cl ,e)
     (define cl* (map (lambda (c)  (frees (cadr c))) cl))
     (define binds (apply seteq (apply append (map car cl))))
     (set-remove (set-union (frees e) (apply union-all cl*)) binds)]
    [`(lambda (,args ...) ,e) (set-remove (frees e) (apply seteq args))]
    [`(lambda ,args ,e) (frees e)]
    [`(case-lambda [,args ,e] ...) (apply union-all (map frees e))]
    [`(,sym ,e ...)
     #:when (memq sym '(begin begin0 with-continuation-mark if set!))
     (apply union-all (map frees e))]
    [`(quote ,_) (seteq)]
    [e #:when (or (hash? e) (boolean? e) (number? e) (string? e) (bytes? e))
       (seteq)]
    [(list app ...) (apply union-all (map frees app))]))

(define (simplify-expr e           ; expression to simplify
                       vars        ; set of all mutated variables (for variable-reference-constant?)
                       safe-ref?   ; predicate for whether referencing a variable is safe
                       seen-defns) ; known definitions
  (define (simp e) (simplify-expr e vars safe-ref? seen-defns))
  (match e
    [`(if ,e0 ,e1 ,e2)
     (define e0* (simp e0))
     (case e0*
       [(#t) (simp e1)]
       [(#f) (simp e2)]
       [else `(if ,e0* ,(simp e1) ,(simp e2))])]
    [`(let-values ,cl ,e)
     (define names (apply append (map car cl)))
     (define simp-body (simplify-expr e vars (lambda (e) (or (memq e names) (safe-ref? e))) seen-defns))
     (define body-frees (frees simp-body))
     (define cl* (filter-map
                  (lambda (c)
                    (define vars (car c))
                    (define rhs (simp (cadr c)))
                    (cond
                      [(and (for/and ([v (in-list vars)]) (not (set-member? body-frees v)))
                            (or
                             (not (any-side-effects? rhs (length vars) #:known-defns seen-defns
                                                     #:ready-variable? safe-ref?))
                             ;; UNSOUND --- assume that variables are defined before use
                             (symbol? rhs)))
                       #f]
                      [else (list vars rhs)]))
                  cl))
     `(let-values ,cl* ,simp-body)]
    [`(letrec-values ,cl ,e)
     (define names (apply append (map car cl)))
     (define cl* (map (lambda (c) (list (car c) (simp (cadr c)))) cl))
     `(letrec-values ,cl* ,(simplify-expr e vars (lambda (e) (or (memq e names) (safe-ref? e))) seen-defns))]
    [`(lambda (,args ...) ,e) `(lambda ,args ,(simplify-expr e vars (lambda (e) (or (memq e args) (safe-ref? e))) seen-defns))]
    [`(lambda ,args ,e) `(lambda ,args ,(simp e))]
    [`(case-lambda ,cl ...)
     (cons 'case-lambda (for/list ([c (in-list cl)])
                          (list (car c)
                                (simp (cadr c)))))]
    [`(variable-reference-constant? (#%variable-reference ,x))
     ;; UNSOUND --- assume that variables are defined before use
     (not (hash-ref vars x #f))]
    [`(begin ,e)
     (simp e)]
    [`(begin (void) ,e ...)
     (simp `(begin ,@e))]
    [`(,sym ,e ...)
     #:when (memq sym '(begin begin0 with-continuation-mark set!))
     `(,sym ,@(map simp e))]
    [(? symbol? e) e]
    [`(quote ,_) e]
    [e #:when (or (boolean? e) (number? e) (string? e) (bytes? e))
     e]
    [(list app ...) (map simp app)]))

;; We can rely on each binding within the linklet having a distinct
;; symbolic name
(define (simplify-definitions linklet-expr)
  (log-status "Simplifying definitions...")
  (define body (bootstrap:s-expr-linklet-body linklet-expr))

  (define all-mutated-vars
    (for/fold ([s (seteq)]) ([e (in-list body)])
      (cond [(defn? e)
             (set-union s (mutated-vars (defn-rhs e)))]
            [else (set-union s (mutated-vars e))])))

  (define seen-defns (make-hasheq))
  (register-known-primitives! seen-defns)

  (define (safe-defn-or-expr? e)
    (if (defn? e)
        (not (any-side-effects? (defn-rhs e) (length (defn-syms e)) #:known-defns seen-defns))
        (not (any-side-effects? e #f #:known-defns seen-defns))))

  (define (safe-ref? s) (hash-ref seen-defns s #f))

  (define new-body
    (let loop ([body body])
      (cond [(null? body) null]
            [(defn? (car body))
             (for* ([d (in-list body)]
                    #:break (and (defn? d)
                                 (hash-ref seen-defns (car (defn-syms d)) #f))
                    #:break (not (safe-defn-or-expr? d))
                    #:when (defn? d))
               (add-defn-known! seen-defns all-mutated-vars (defn-syms d) (defn-rhs d)))
             (define e (car body))
             (define new-defn
               (list 'define-values (defn-syms e) (simplify-expr (defn-rhs e) all-mutated-vars safe-ref? seen-defns)))
             (add-defn-known! seen-defns all-mutated-vars (defn-syms e) (defn-rhs e))
             (cons new-defn (loop (cdr body)))]
            [else
             (define e
               (simplify-expr (car body) all-mutated-vars safe-ref? seen-defns))
             (if (equal? e '(void))
                 (loop (cdr body))
                 (cons e
                       (loop (cdr body))))])))

  (append (take linklet-expr 3)
          new-body))

;; This function gets called when encountering a 'suspect letrec': some
;; letrec expression where the bound variable is used somewhere inside
;; the bound expression, and we can't prove (given the information
;; available here) that the use is safe: that the implicit mutation that
;; happens as the letrec defines the variables is not observable.
;; Specifically, such a variable is used in a bound expression that is
;; not a `lambda` or `case-lambda` form.
;;
;; We reject that case with an error, as it's possible
;; that we stumbled on a bug (e.g. the bug fixed in git commit
;;     b9b017261b (io: repair wrong-variable use in checking for special Windows paths, 2025-06-03)
;; although that code unreachable at present). This means that extracted
;; code must be written in a more restricted language than plain Racket.
;; Definition forms that would not be recognized here must be written
;; with `set!` to highlight the need for extra scrutiny. (There's a more detailed
;; discussion of why in GH PR https://github.com/racket/racket/pull/5272
;; "extracter: rewrite, bugfix, and test mutated-vars".)
;
;; If you've found your way to this comment because an error showed up
;; and you're investigating :) then you need fix the code.
(define (reject-suspect-letrec letrec-expr)
  (error 'mutated-vars
         "unable to show that implicit assignment in letrec is unobservable: ~a"
         letrec-expr))

(module+ test
  (require (for-syntax racket/base racket/syntax-srcloc))

  (struct exn:fail:test exn (test-srcloc)
    #:property prop:exn:srclocs
    (lambda (self)
      (list (exn:fail:test-test-srcloc self))))

  (define (raise-test-error msg #:srcloc srcloc)
    (raise (exn:fail:test msg (current-continuation-marks) srcloc)))

  (define (check-mutated-vars* expr expected [desc #f] #:error [error-rx #f] #:loc loc)
    (define mut-vars
      (with-handlers ([exn:fail? (lambda (exn) (exn-message exn))])
        (mutated-vars expr)))
    (cond
      [(string? mut-vars)
       (define out-str mut-vars)
       (unless (and error-rx (regexp-match? error-rx out-str))
         (raise-test-error (format "test failure;~n  test description: ~a~n  actual output: ~n   ~a~n  expected to ~a: ~a~n  test soure: ~a"
                                   (or desc "(none provided)")
                                   out-str
                                   (if error-rx "match" "not match")
                                   error-rx
                                   (srcloc->string loc))
                           #:srcloc loc))]
      [else
       (define actual-sorted (sort (set->list mut-vars) symbol<?))
       (define expected-sorted (sort (set->list (list->seteq expected)) symbol<?))
       (unless (equal? actual-sorted expected-sorted)
         (raise-test-error (format "test failure;~n  test description: ~a~n  expected result: ~a~n  actual result: ~a~n  test soure: ~a"
                                   (or desc "(none provided)")
                                   expected-sorted
                                   actual-sorted
                                   (srcloc->string loc))
                           #:srcloc loc))]))

  (define-syntax (check-mutated-vars stx)
    (syntax-case stx ()
      [(_ arg ...)
       (quasisyntax/loc stx
         (check-mutated-vars* arg ... #:loc (quote #,(syntax-srcloc stx))))]))


  ; Essential base cases
  (check-mutated-vars 'id '() "ignores lone identifiers")
  (check-mutated-vars '(set! i e) '(i) "set! at top-level")
  (check-mutated-vars '(set! i (boop e)) '(i) "set! at top-level with non-trivial expr")

  ; set! with procedure application
  (check-mutated-vars '(proc) '() "ignores identifiers in rator position")
  (check-mutated-vars '(proc arg1 arg1) '() "ignores identifiers in rand position")
  (check-mutated-vars '((set! i exp)) '(i) "set! in rator position with no args")
  (check-mutated-vars '((set! i exp) a b) '(i) "set! in rator position with args")
  (check-mutated-vars '(proc (set! i exp)) '(i) "set! in rand position, only argument")
  (check-mutated-vars '(proc (set! i exp) a b) '(i) "set! in rand position, first arg")
  (check-mutated-vars '(proc a b (set! i exp)) '(i) "set! in rand position, last arg")
  (check-mutated-vars '(proc a (set! i exp) c) '(i) "set! in rand position, middle arg")

  ; multiple set! calls
  (check-mutated-vars '(proc (set! i expr1) (set! j expr2)) '(i j) "multiple set! rands")
  (check-mutated-vars '(proc a (set! i expr1) b (set! j expr1) (set! k expr1))
                      '(i j k) "multiple set! rands with mixed args")
  (check-mutated-vars '(proc a (set! i expr1) b (set! j expr1) (set! i expr1))
                      '(i j) "multiple set! rands with mixed args, repeated ident")

  ; set! within set!
  (check-mutated-vars '(proc (set! i (set! j e))) '(i j) "recurs through expr position of set!")
  (check-mutated-vars '(proc (set! i (p2 (set! j e)))) '(i j) "more nesting")
  (check-mutated-vars '(set! i (set! j (proc a (set! i (set! k e)) b))) '(i j k) "more nesting")

  ; literals
  (check-mutated-vars '#t '() "ignores literals (top-level)")
  (check-mutated-vars '#f '() "ignores literals (top-level)")
  (check-mutated-vars 0 '() "ignores literals (top-level)")
  (check-mutated-vars '"foop" '() "ignores literals (top-level)")
  (check-mutated-vars '#"boop" '() "ignores literals (top-level)")
  (check-mutated-vars '(p #t) '() "ignores literals (as nested expr)")
  (check-mutated-vars '(p #f) '() "ignores literals (as nested expr)")
  (check-mutated-vars '(p 42) '() "ignores literals (as nested expr)")
  (check-mutated-vars '(p "foop") '() "ignores literals (as nested expr)")
  (check-mutated-vars '(p #"boop") '() "ignores literals (as nested expr)")

  ; quote
  (check-mutated-vars '(quote (set! j e)) '() "set! in quote ignored")
  (check-mutated-vars '(proc (quote (set! j e))) '() "set! in nested quote ignored")
  (check-mutated-vars '(quote (proc (set! i e))) '() "set! nested in quote ignored")

  ; begin...
  (check-mutated-vars '(begin) '() "empty begin")
  (check-mutated-vars '(begin a) '() "begin, boring body")
  (check-mutated-vars '(begin (set! i j)) '(i) "begin, set! as body")
  (check-mutated-vars '(begin (set! i j) a b) '(i) "begin, set! first body form")
  (check-mutated-vars '(begin a (set! i j) b) '(i) "begin, set! middle body form")
  (check-mutated-vars '(begin a b (set! i j)) '(i) "begin, set! last body form")
  (check-mutated-vars '(begin (set! i j) (set! k z)) '(i k) "begin, multiple")
  (check-mutated-vars '(begin a (proc (set! i j)) b) '(i) "begin, nesting")
  (check-mutated-vars '(proc (begin a (proc (begin (set! i j))))) '(i) "begin, deep nesting")

  ; ...and begin0
  (check-mutated-vars '(begin0) '() "empty begin0")
  (check-mutated-vars '(begin0 a) '() "begin0, boring body")
  (check-mutated-vars '(begin0 (set! i j)) '(i) "begin0, set! as body")
  (check-mutated-vars '(begin0 (set! i j) a b) '(i) "begin0, set! first body form")
  (check-mutated-vars '(begin0 a (set! i j) b) '(i) "begin0, set! middle body form")
  (check-mutated-vars '(begin0 a b (set! i j)) '(i) "begin0, set! last body form")
  (check-mutated-vars '(begin0 (set! i j) (set! k z)) '(i k) "begin0, multiple")
  (check-mutated-vars '(begin0 a (proc (set! i j)) b) '(i) "begin0, nesting")
  (check-mutated-vars '(proc (begin0 a (proc (begin0 (set! i j))))) '(i) "begin0, deep nesting")

  ; with-continuation-mark
  (check-mutated-vars '(with-continuation-mark a b c) '() "with-continuation-mark, no set!")
  (check-mutated-vars '(with-continuation-mark (set! i j) b c) '(i) "with-continuation-mark, set! in mark-key position")
  (check-mutated-vars '(with-continuation-mark a (set! i j) c) '(i) "with-continuation-mark, set! in mark-value position")
  (check-mutated-vars '(with-continuation-mark a b (set! i j)) '(i) "with-continuation-mark, set! in body-expr position")
  (check-mutated-vars '(with-continuation-mark (set! i e) (set! j e2) (set! k e3))
                      '(i j k) "with-continuation-mark, set! in every position")
  (check-mutated-vars '(with-continuation-mark (p1 (set! i e)) (p2 (set! j e2)) (p3 (set! k e3)))
                      '(i j k) "with-continuation-mark, set! in every position with nesting")
  (check-mutated-vars '(with-continuation-mark (p1 (set! i e)) (p2 (set! j e2)) (p3 (set! i e3)))
                      '(i j) "with-continuation-mark, set! in every position with nesting and repeated vars")
  (check-mutated-vars '(proc (with-continuation-mark (p1 (set! i e)) (p2 (set! j e2)) (p3 (set! k e3))))
                      '(i j k) "with-continuation-mark when it's nested")

  ; if
  (check-mutated-vars '(if a b c) '() "if, no set!")
  (check-mutated-vars '(if (set! i j) b c) '(i) "if, set! in cond position")
  (check-mutated-vars '(if a (set! i j) c) '(i) "if, set! in when-true position")
  (check-mutated-vars '(if a b (set! i j)) '(i) "if, set! in when-false position")
  (check-mutated-vars '(if (set! i e) (set! j e2) (set! k e3))
                      '(i j k) "if, set! in every position")
  (check-mutated-vars '(if (p1 (set! i e)) (p2 (set! j e2)) (p3 (set! k e3)))
                      '(i j k) "if, set! in every position with nesting")
  (check-mutated-vars '(if (p1 (set! i e)) (p2 (set! j e2)) (p3 (set! i e3)))
                      '(i j) "if, set! in every position with nesting and repeated vars")
  (check-mutated-vars '(proc (if (p1 (set! i e)) (p2 (set! j e2)) (p3 (set! k e3))))
                      '(i j k) "if when it's nested")

  ; lambda
  (check-mutated-vars '(lambda (a) (set! i e)) '(i) "lambda with set! body")
  (check-mutated-vars '(lambda (a) (proc (set! i e))) '(i) "lambda with set! nested in body")
  (check-mutated-vars '(lambda (a1 a2) (proc (set! i e))) '(i) "lambda, multiple formal args")
  (check-mutated-vars '(lambda (a1 . rest) (proc (set! i e))) '(i) "lambda, rest args")
  (check-mutated-vars '(lambda args (proc (set! i e))) '(i) "lambda, varargs")
  (check-mutated-vars '(lambda (i) (set! i e)) '(i) "names bound by lambda are still in result")
  (check-mutated-vars '(lambda (i) (proc (set! i e))) '(i) "names bound by lambda are still in result (nested)")
  (check-mutated-vars '(lambda (x . i) (proc (set! i e))) '(i) "names bound by lambda's rest-arg formal are still in result")
  (check-mutated-vars '(lambda i (proc (set! i e))) '(i) "names bound by lambda's all-args formal are still in result")
  (check-mutated-vars '(proc (lambda (a b) (begin (set! i e) a))) '(i) "lambda, misc. nesting")
  (check-mutated-vars '(proc (lambda (set! i e) (set! j k)))  ; <-- N.B. this is illegal input, but we check anyway
                      '(j) "doesn't recur into lambda's formal params")

  ; case-lambda
  (check-mutated-vars '(case-lambda [(i) (proc (set! j e))]) '(j) "case-lambda, set! in only case")
  (check-mutated-vars '(case-lambda [(i) (proc (set! j e))]) '(j) "case-lambda, set! in only case")
  (check-mutated-vars '(case-lambda [(i) (proc (set! j e))]
                                    [(a) (boop)]
                                    [(b) (burp)])
                      '(j) "case-lambda, set! in first case")
  (check-mutated-vars '(case-lambda [(a) (boop)]
                                    [(i) (proc (set! j e))]
                                    [(b) (burp)])
                      '(j) "case-lambda, set! in middle case")
  (check-mutated-vars '(case-lambda [(a) (boop)]
                                    [(b) (burp)]
                                    [(i) (proc (set! j e))])
                      '(j) "case-lambda, set! in last case")
  (check-mutated-vars '(case-lambda [(a) (proc (set! i1 e))]
                                    [(b) (burp (set! i2 e))]
                                    [(i) (proc (set! i3 e))])
                      '(i1 i2 i3) "case-lambda, set! in multiple cases")
  (check-mutated-vars '(case-lambda [(a) (proc (set! i1 e))]
                                    [(b) (burp (set! i1 e))]
                                    [(i) (proc (set! i3 e))])
                      '(i1 i3) "case-lambda, set! in multiple cases, repeated id")
  (check-mutated-vars '(case-lambda [(a b c) (proc (set! i1 e))]
                                    [(a b . c) (burp (set! i2 e))]
                                    [a (proc (set! i3 e))])
                      '(i1 i2 i3) "case-lambda, vararg combinations")
  (check-mutated-vars '(case-lambda [(i) (blurp (set! i e))])
                      '(i) "case-lambda, bound as formal arg still included in result")
  (check-mutated-vars '(case-lambda [(a b . i) (blurp (set! i e))])
                      '(i) "case-lambda, bound as formal rest-arg still included in result")
  (check-mutated-vars '(case-lambda [i (blurp (set! i e))])
                      '(i) "case-lambda, bound as formal all-arg still included in result")
  (check-mutated-vars '(case-lambda [(set! j e2) (blurp (set! i e))]) ; <-- N.B. this is illegal input, but we check anyway
                      '(i) "case-lambda, doesn't recur into arg list")

  ; let-values
  (check-mutated-vars '(let-values ([(i j) (exp)])
                         (set! k (blurp)))
                      '(k) "set! in let-values body, only expr")
  (check-mutated-vars '(let-values ([(i j) (exp)])
                         (set! k (blurp))
                         (burp)
                         (bloop))
                      '(k) "set! in let-values body, first expr")
  (check-mutated-vars '(let-values ([(i j) (exp)])
                         (burp)
                         (set! k (blurp))
                         (bloop))
                      '(k) "set! in let-values body, middle expr")
  (check-mutated-vars '(let-values ([(i j) (exp)])
                         (burp)
                         (bloop)
                         (set! k (blurp)))
                      '(k) "set! in let-values body, final expr")
  (check-mutated-vars '(let-values () (set! k e)) '(k) "let-values with no clauses")
  (check-mutated-vars '(let-values ([(i) (set! j e)]) e2) '(j) "let-values recurs into clauses")
  (check-mutated-vars '(let-values ([(i1 i2) (set! j1 e)]
                                    [(i3) (proc (set! j2 e2))]
                                    [() (set! j3 (proc2 (set! j4 e)))])
                         (foo)
                         (bar (set! j4 (baz (set! j5 quux)))))
                      '(j1 j2 j3 j4 j5)
                      "let-values with a bunch of messy nesting")
  (check-mutated-vars '(letrec-values ([(i1 i2) 4]
                                       [(i3) 5]
                                       [() 6])
                         (set! i3 4))
                      '(i3)
                      "let-values; body modifies a binding")

  ; letrec-values without rec
  (check-mutated-vars '(letrec-values ([(i j) (exp)])
                         (set! k (blurp)))
                      '(k) "set! in letrec-values body, only expr")
  (check-mutated-vars '(letrec-values ([(i j) (exp)])
                         (set! k (blurp))
                         (burp)
                         (bloop))
                      '(k) "set! in letrec-values body, first expr")
  (check-mutated-vars '(letrec-values ([(i j) (exp)])
                         (burp)
                         (set! k (blurp))
                         (bloop))
                      '(k) "set! in letrec-values body, middle expr")
  (check-mutated-vars '(letrec-values ([(i j) (exp)])
                         (burp)
                         (bloop)
                         (set! k (blurp)))
                      '(k) "set! in letrec-values body, final expr")
  (check-mutated-vars '(letrec-values () (set! k e)) '(k) "letrec-values with no clauses")
  (check-mutated-vars '(letrec-values ([(i) (set! j e)]) e2) '(j) "letrec-values recurs into clauses")
  (check-mutated-vars '(letrec-values ([(i1 i2) (set! j1 e)]
                                       [(i3) (proc (set! j2 e2))]
                                       [() (set! j3 (proc2 (set! j4 e)))])
                         (foo)
                         (bar (set! j4 (baz (set! j5 quux)))))
                      '(j1 j2 j3 j4 j5)
                      "letrec-values with a bunch of messy nesting")
  (check-mutated-vars '(letrec-values ([(i1 i2) 4]
                                       [(i3) 5]
                                       [() 6])
                         (set! i3 4))
                      '(i3)
                      "let-values; body modifies a binding")

  ; letrec-values using the recursive bindings
  (check-mutated-vars '(letrec-values ([(x) (lambda (a) x)]
                                       [(y) (lambda (a) y)])
                         body)
                      '()
                      "letrec-values; reference inside lambda")
  (check-mutated-vars '(letrec-values ([(x) (lambda (a) (set! x a))]
                                       [(y) (lambda (a) y)])
                         body)
                      '(x)
                      "letrec-values; modification with set! in its binding")
  (check-mutated-vars '(letrec-values ([(x) (lambda (a) (set! y a))]
                                       [(y) (lambda (a) y)])
                         body)
                      '(y)
                      "letrec-values; modification with set! of a subsequent binding")
  (check-mutated-vars '(letrec-values ([(x) (lambda (a) a)]
                                       [(y) (lambda (a) (set! x a))])
                         body)
                      '(x)
                      "letrec-values; modification with set! of a subsequent binding")
  (check-mutated-vars '(letrec-values ([(vvv) (proc vvv)])
                         body)
                      '(vvv)
                      #:error #rx".*implicit assignment.*vvv"
                      "letrec-values; observation of implicit modification")
  (check-mutated-vars '(letrec-values ([(vvv) (proc (lambda () vvv))])
                         body)
                      '(vvv)
                      #:error #rx".*implicit assignment.*vvv"
                      "letrec-values; observation of implicit modification in used lambda")
  (check-mutated-vars '(letrec-values ([(vvv) (lambda () vvv)]
                                       [(zzz) (proc vvv)])
                         body)
                      '(vvv zzz)
                      #:error #rx".*implicit assignment.*(vvv.*zzz|zzz.*vvv)"
                      "letrec-values; potential observation of implicit modification")
  (check-mutated-vars '(letrec-values ([(vvv) (proc vvv)]
                                       [(zzz) (lambda () vvv)])
                         body)
                      '(vvv zzz)
                      #:error #rx".*implicit assignment.*(vvv.*zzz|zzz.*vvv)"
                      "letrec-values; actual observation of implicit modification")
  (check-mutated-vars '(letrec-values ([(vvv) (lambda () vvv)])
                         body)
                      '()
                      "letrec-values; no problem if recursive use is in lambda")
  (check-mutated-vars '(letrec-values ([(vvv) (case-lambda
                                                [() vvv]
                                                [(a) vvv]
                                                [(a b . c) vvv]
                                                [d vvv])])
                         body)
                      '()
                      "letrec-values; no problem if recursive use is in case-lambda")
  (check-mutated-vars '(letrec-values ([(uuu www zzz mmm nnn) (values 4 4 4 4 4)]
                                       [(vvv) (case-lambda
                                                [() (set! uuu 4)]
                                                [(a) (set! vvv 5)]
                                                [(a b . c) www]
                                                [d (set! zzz 6)])])
                         body)
                      '(uuu vvv zzz)
                      "letrec-values; correctly handles mutation inside case-lambda bodies")
  (check-mutated-vars '(letrec-values ([(vvv) (proc (case-lambda [(a) vvv]))])
                         body)
                      '(vvv)
                      #:error #rx".*implicit assignment.*vvv"
                      "letrec-values; observation of implicit modification in used case-lambda")
  (check-mutated-vars '(letrec-values ([(vvv) (let-values ([(uuu) (lambda (a) vvv)])
                                                (case-lambda [() vvv]
                                                             [a vvv]))])
                         body)
                      '()
                      "letrec-values; use in body of desugared optional args lambda is ok")
  (check-mutated-vars '(letrec-values ([(vvv) (proc (let-values ([(uuu) (lambda (a) vvv)])
                                                      (case-lambda [() vvv]
                                                                   [a vvv])))])
                         body)
                      '(vvv)
                      #:error #rx".*implicit assignment.*vvv"
                      "letrec-values; potential implicit use detected in used optional-arg lambda")
  (check-mutated-vars '(letrec-values ([(vvv) (let-values ([(uuu) (lambda (a) (set! vvv 4))])
                                                (case-lambda [() vvv]
                                                             [a vvv]))])
                         body)
                      '(vvv)
                      "letrec-values; modification in body of desugared optional args still detected")
  (check-mutated-vars '(letrec-values ([(vvv) (let-values ([(uuu) vvv])
                                                (case-lambda [() vvv]
                                                             [a vvv]))])
                         body)
                      '(vvv)
                      #:error #rx".*implicit assignment.*vvv"
                      "letrec-values; thing that looks kinda like an optional-arg lambda but isn't")
  (check-mutated-vars '(letrec-values ([(vvv) (let-values ([(uuu) (lambda () bleh)])
                                                (proc (case-lambda [() vvv]
                                                                   [a vvv])))])
                         body)
                      '(vvv)
                      #:error #rx".*implicit assignment.*vvv"
                      "letrec-values; thing that looks kinda like an optional-arg lambda but isn't")
  (check-mutated-vars '(letrec-values ([(aaa) 4]  ; <-- inclusion in result is conservative
                                       [(bbb) 5]  ; <-- inclusion in result is conservative
                                       [(ccc) (lambda () ddd)]  ; looks safe (reference guarded by lambda), but isn't
                                       [(ddd) (proc (ccc))]     ; looks safe (only references are backwards), but isn't
                                       [(eee) 6])  ; <-- inclusion in result is conservative
                         body)
                      '(aaa bbb ccc ddd eee)
                      #:error #rx".*implicit assignment"
                      "letrec-values; implicit assignment (conservatively) includes *all* letrec bindings")

  'tests-pass)
