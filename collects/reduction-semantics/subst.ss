(module subst mzscheme
  (require (lib "match.ss")
           (prefix plt: (lib "plt-match.ss"))
           (lib "list.ss"))
  
  (provide plt-subst subst
           all-vars variable subterm subterms constant build
           subst/proc alpha-rename free-vars/memoize)
  
  (define-syntax (all-vars stx) (raise-syntax-error 'subst "all-vars out of context" stx))
  (define-syntax (variable stx) (raise-syntax-error 'subst "variable out of context" stx))
  (define-syntax (subterm stx) (raise-syntax-error 'subst "subterm out of context" stx))
  (define-syntax (subterms stx) (raise-syntax-error 'subst "subterms out of context" stx))
  (define-syntax (constant stx) (raise-syntax-error 'subst "constant out of context" stx))
  (define-syntax (build stx) (raise-syntax-error 'subst "build out of context" stx))
  
  (define-syntax (make-subst stx)
    (syntax-case stx ()
      [(_ subst match)
       (syntax
        (define-syntax (subst stx)
          (syntax-case stx ()
            [(_ (pat rhs (... ...)) (... ...))
             (with-syntax ([term/arg #'term/arg]
                           [constant/arg #'constant/arg]
                           [variable/arg #'variable/arg]
                           [combine/arg #'combine/arg]
                           [sub-piece/arg #'subpiece/arg])
               (define (handle-rhs rhs-stx)
                 (syntax-case rhs-stx (all-vars build subterm subterms variable constant)
                   [((all-vars all-vars-exp) (build build-exp) sub-pieces (... ...))
                    (with-syntax ([(sub-pieces (... ...))
                                   (map (lambda (subterm-stx)
                                          (syntax-case subterm-stx (subterm subterms)
                                            [(subterm vars body) (syntax (list (sub-piece/arg vars body)))]
                                            [(subterms vars terms) 
                                             (syntax 
                                              (let ([terms-var terms])
                                                (unless (list? terms-var)
                                                  (error 'subst
                                                         "expected a list of terms for `subterms' subclause, got: ~e"
                                                         terms-var))
                                                (map (lambda (x) (sub-piece/arg vars x))
                                                     terms-var)))]
                                            [else (raise-syntax-error 
                                                   'subst 
                                                   "unknown all-vars subterm"
                                                   stx
                                                   subterm-stx)]))
                                        (syntax->list (syntax (sub-pieces (... ...)))))])
                      (syntax
                       (apply combine/arg
                              build-exp
                              all-vars-exp
                              (append sub-pieces (... ...)))))]
                   [((all-vars) sub-pieces (... ...))
                    (raise-syntax-error 'subst "expected all-vars must have an argument" stx rhs-stx)]
                   [((all-vars all-vars-exp) not-build-clause anything (... ...))
                    (raise-syntax-error 'subst "expected build clause" (syntax not-build-clause))]
                   [((all-vars all-vars-exp))
                    (raise-syntax-error 'subst "missing build clause" (syntax (all-vars all-vars-exp)))]
                   [((constant)) 
                    (syntax (constant/arg term/arg))]
                   [((variable))
                    (syntax (variable/arg (lambda (x) x) term/arg))]
                   [(unk unk-more (... ...))
                    (raise-syntax-error 'subst "unknown clause" (syntax unk))]))
               (with-syntax ([(expanded-rhs (... ...))
                              (map handle-rhs (syntax->list (syntax ((rhs (... ...)) (... ...)))))])
                 (syntax
                  (let ([separate
                         (lambda (term/arg constant/arg variable/arg combine/arg sub-piece/arg)
                           (match term/arg
                             [pat expanded-rhs] (... ...)
                             [else (error 'subst "no matching clauses for ~s\n" term/arg)]))])
                    (lambda (var val exp)
                      (subst/proc var val exp separate))))))])))]))
  
  (make-subst subst match)
  (make-subst plt-subst plt:match)
  
  (define (subst/proc var val exp separate)
    (let* ([free-vars-cache (make-hash-table)]
           [fv-val (free-vars/memoize free-vars-cache val separate)])
      (let loop ([exp exp])
        (let ([fv-exp (free-vars/memoize free-vars-cache exp separate)]
              [handle-constant
               (lambda (x) x)]
              [handle-variable
               (lambda (rebuild var-name)
                 (if (eq? var-name var)
                     val
                     (rebuild var-name)))]
              [handle-complex
               (lambda (maker vars . subpieces)
                 (cond
                   [(ormap (lambda (var) (memq var fv-val)) vars)
                    =>
                    (lambda (to-be-renamed-l)
                      (let ([to-be-renamed (car to-be-renamed-l)])
                        (loop
                         (alpha-rename
                          to-be-renamed
                          (pick-new-name to-be-renamed (cons to-be-renamed fv-val))
                          exp
                          separate))))]
                   [else
                    (apply maker 
                           vars
                           (map (lambda (subpiece)
                                  (let ([sub-term-binders (subpiece-binders subpiece)]
                                        [sub-term (subpiece-term subpiece)])
                                    (if (memq var sub-term-binders)
                                        sub-term
                                        (loop sub-term))))
                                subpieces))]))])
          (if (memq var fv-exp)
              (separate
               exp
               handle-constant
               handle-variable
               handle-complex
               make-subpiece)
              exp)))))
  
  (define-struct subpiece (binders term) (make-inspector))
  
  ;; alpha-rename : symbol symbol term separate -> term
  ;; renames the occurrences of to-be-renamed that are 
  ;; bound in the "first level" of exp.
  (define (alpha-rename to-be-renamed new-name exp separate)
    (define (first exp)
      (separate exp
                first-handle-constant
                first-handle-variable
                first-handle-complex
                first-handle-complex-subpiece))
    (define (first-handle-constant x) x)
    (define (first-handle-variable rebuild var) (rebuild var))
    (define (first-handle-complex maker vars . subpieces)
      (let ([replaced-vars
             (map (lambda (x) (if (eq? x to-be-renamed) new-name x))
                  vars)])
        (apply maker replaced-vars subpieces)))
    (define (first-handle-complex-subpiece binders subterm)
      (if (memq to-be-renamed binders)
          (beyond-first subterm)
          subterm))
    
    (define (beyond-first exp)
      (define (handle-constant x) x)
      (define (handle-variable rebuild var)
        (if (eq? var to-be-renamed)
            (rebuild new-name)
            (rebuild var)))
      (define (handle-complex maker vars . subpieces)
        (apply maker vars subpieces))
      (define (handle-complex-subpiece binders subterm)
        (if (memq to-be-renamed binders)
            subterm
            (beyond-first subterm)))
      (separate
       exp
       handle-constant
       handle-variable
       handle-complex
       handle-complex-subpiece))
    
    (first exp))
  
  ;; free-vars/memoize : hash-table[sexp -o> (listof symbols)] sexp separate -> (listof symbols)
  ;; doesn't cache against separate -- if it changes, a new hash-table must be passed in,
  ;; or the caching will be wrong
  (define (free-vars/memoize cache exp separate)
    (hash-table-get
     cache
     exp
     (lambda ()
       (let ([res (free-vars/compute cache exp separate)])
         (hash-table-put! cache exp res)
         res))))
  
  ;; free-vars/memoize : hash-table[sexp -o> (listof symbols)] sexp separate -> (listof symbols)
  (define (free-vars/compute cache exp separate)
    (let ([handle-constant (lambda (x) '())]
          [handle-variable (lambda (rebuild var) (list var))]
          [handle-complex
           (lambda (maker vars . subpieces)
             (apply append subpieces))]
          [handle-complex-subpiece
           (lambda (binders subterm)
             (foldl remove-all
                    (free-vars/memoize cache subterm separate)
                    binders))])
      (separate
       exp
       handle-constant
       handle-variable
       handle-complex
       handle-complex-subpiece)))
  
  (define (remove-all var lst)
    (let loop ([lst lst]
               [ans '()])
      (cond
        [(null? lst) ans]
        [else (if (eq? (car lst) var)
                  (loop (cdr lst) ans)
                  (loop (cdr lst) (cons (car lst) ans)))])))
  
  (define (lc-direct-subst var val exp)
    (let ([fv-exp (lc-direct-free-vars exp)])
      (if (memq var fv-exp)
          (match exp
            [`(lambda ,vars ,body)
             (if (memq var vars)
                 exp
                 (let* ([fv-val (lc-direct-free-vars val)]
                        [vars1 (map (lambda (var) (pick-new-name var fv-val)) vars)])
                   `(lambda ,vars1 ,(lc-direct-subst
                                     var 
                                     val
                                     (lc-direct-subst/l vars 
                                                        vars1
                                                        body)))))]
            [`(let (,l-var ,exp) ,body)
             (if (eq? l-var var)
                 `(let (,l-var ,(lc-direct-subst var val exp)) ,body)
                 (let* ([fv-val (lc-direct-free-vars val)]
                        [l-var1 (pick-new-name l-var fv-val)])
                   `(let (,l-var1 ,(lc-direct-subst var val exp))
                      ,(lc-direct-subst
                        var 
                        val
                        (lc-direct-subst
                         l-var 
                         l-var1
                         body)))))]
            [(? number?) exp]
            [(and var1 (? symbol?))
             (if (eq? var1 var)
                 val
                 var1)]
            [`(,@(args ...))
             `(,@(map (lambda (arg) (lc-direct-subst var val arg)) args))])
          exp)))
  
  ;; lc-direct-subst/l : (listof symbol) (listof symbol) (listof symbol) sexp -> exp
  ;; substitutes each of vars with vals in exp
  ;; [assume vals don't contain any vars]
  (define (lc-direct-subst/l vars vals exp)
    (foldr (lambda (var val exp) (lc-direct-subst var val exp))
           exp
           vars
           vals))
  
  ;; lc-direct-free-vars : sexp -> (listof symbol)
  ;; returns the free variables in exp
  (define (lc-direct-free-vars exp)
    (let ([ht (make-hash-table)])
      (let loop ([exp exp]
                 [binding-vars null])
        (match exp
          [(? symbol?) 
           (unless (memq exp binding-vars)
             (hash-table-put! ht exp #t))]
          [(? number?)
           (void)]
          [`(lambda ,vars ,body)
           (loop body (append vars binding-vars))]
          [`(let (,var ,exp) ,body)
           (loop exp binding-vars)
           (loop body (cons var binding-vars))]
          [`(,@(args ...))
           (for-each (lambda (arg) (loop arg binding-vars)) args)]))
      (hash-table-map ht (lambda (x y) x))))
  
  ;; pick-new-name : symbol (listof symbol) -> symbol
  ;; returns a primed version of `var' that does
  ;; not occur in vars (possibly with no primes)
  (define (pick-new-name var vars)
    (if (member var vars)
        (pick-new-name (prime var) vars)
        var))
  
  ;; prime : symbol -> symbol
  ;; adds an @ at the end of the symbol
  (define (prime var)
    (string->symbol
     (string-append
      (symbol->string var)
      "@"))))