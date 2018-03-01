#lang racket/base
(require racket/list
         racket/match
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

;; compute the variables that are the target of a set! in e
(define (mutated-vars e)
  (match e
    [`(set! ,i ,e) (set-add (mutated-vars e) i)]
    [`(let-values ,cl ,e) 
     (define cl* (map (lambda (c)  (mutated-vars (cadr c))) cl))
     (define binds (apply seteq (apply append (map car cl))))
     (set-union (apply union-all (map mutated-vars cl*)) (set-remove (mutated-vars e) binds))]
    [`(letrec-values ,cl ,e)
     ;; UNSOUND --- assume that variables are defined before use
     ;; (i.e., no visible implicit assignment)
     (define cl* (map (lambda (c) (mutated-vars (cadr c))) cl))
     (define binds (apply seteq (apply append (map car cl))))
     (set-remove (set-union (mutated-vars e) (apply union-all (map mutated-vars cl*))) binds)]
    [`(lambda ,args ,e) (mutated-vars e)]
    [`(case-lambda [,args ,e] ...) (apply union-all (map mutated-vars e))]
    [`(,sym ,e ...)
     #:when (memq sym '(begin begin0 with-continuation-mark if))
     (apply union-all (map mutated-vars e))]
    [(? symbol? e) (seteq)]
    [`(quote ,_) (seteq)]
    [e #:when (or (boolean? e) (number? e) (string? e) (bytes? e))
       (seteq)]
    [(list app ...) (apply union-all (map mutated-vars app))]
    [(? hash?) (seteq)]))

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
    [`(,sym ,e ...)
     #:when (memq sym '(begin begin0 with-continuation-mark set!))
     `(,sym ,@(map simp e))]
    [(? symbol? e) e]
    [`(quote ,_) e]
    [e #:when (or (boolean? e) (number? e) (string? e) (bytes? e))
     e]
    [(list app ...) (map simp app)]))

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
               (add-defn-known! seen-defns (defn-syms d) (defn-rhs d)))
             (define e (car body))
             (define new-defn 
               (list 'define-values (defn-syms e) (simplify-expr (defn-rhs e) all-mutated-vars safe-ref? seen-defns)))
             (add-defn-known! seen-defns (defn-syms e) (defn-rhs e))
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
