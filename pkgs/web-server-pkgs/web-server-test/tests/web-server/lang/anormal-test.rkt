#lang racket/base
(require rackunit
         web-server/lang/anormal
         web-server/lang/util)
(provide anormal-tests)

(define (empty-env var)
  (error "empty environment"))

(define (extend env vars vals)
  (lambda (var0)
    (let loop ([vars vars]
               [vals vals])
      (cond
        [(null? vars) (env var0)]
        [(eqv? var0 (car vars))
         (car vals)]
        [else (loop (cdr vars) (cdr vals))]))))

;; alpha=/env: environment target-expr target-expr -> boolean
;; are two target expressions alpha-equivalent?
(define (alpha=/env env1 env2 expr1 expr2)
  (syntax-case expr1 (if #%plain-app)
    [(if tst1 csq1 alt1)
     (syntax-case expr2 (if)
       [(if tst2 csq2 alt2) (and (alpha=/env env1 env2 #'tst1 #'tst2)
                                 (alpha=/env env1 env2 #'csq1 #'csq2)
                                 (alpha=/env env1 env2 #'alt1 #'alt2))]
       [_else #f])]
    [(#%plain-app rator1 rands1 ...)
     (syntax-case expr2 (#%plain-app)
       [(#%plain-app rator2 rands2 ...)
        (and (alpha=/env env1 env2 #'rator1 #'rator2)
             (let loop ([rs1 (syntax->list #'(rands1 ...))]
                        [rs2 (syntax->list #'(rands2 ...))])
               (or (and (null? rs1)
                        (null? rs2))
                   (and (alpha=/env env1 env2 (car rs1) (car rs2))
                        (loop (cdr rs1) (cdr rs2))))))]
       [_else 
        (raise-syntax-error 'alpha=/env "Dropped through on #%plain-app:" expr2)
        #f])]
    [_else (w-alpha=/env env1 env2 expr1 expr2)]))

;; w-alpha=/env: env target-expr target-expr -> boolean
;; are two target vars or vals alpha-equivalent?
(define (w-alpha=/env env1 env2 expr1 expr2)
  (syntax-case expr1 (#%top #%plain-lambda quote #%expression)
    [(#%top . var1)
     (syntax-case expr2 (#%top)
       [(#%top . var2)
        (eqv? (syntax->datum #'var1)
              (syntax->datum #'var2))]
       [_else #f])]
    [(quote datum1)
     (syntax-case expr2 (quote)
       [(quote datum2)
        (let ([dat1 (syntax->datum #'datum1)]
              [dat2 (syntax->datum #'datum2)])
          (equal? dat1 dat2))]
       [_else #f])]
    [(#%plain-lambda formals1 body1)
     (syntax-case expr2 (#%plain-lambda)
       [(#%plain-lambda formals2 body2)
        (let ([syms (map gensym (syntax->symbols (formals-list #'formals1)))])
          (and (= (length syms) (length (formals-list #'formals2)))
               (alpha=/env
                (extend env1 (syntax->symbols (formals-list #'formals1)) syms)
                (extend env2 (syntax->symbols (formals-list #'formals2)) syms)
                #'body1 #'body2)))]
       [_else #f])]
    [(#%expression e1)
     (syntax-case expr2 (#%expression)
       [(#%expression e2)
        (w-alpha=/env env1 env2 #'e1 #'e2)]
       [_else #f])]
    [x1 (symbol? (syntax->datum #'x1))
        (syntax-case expr2 ()
          [x2 (symbol? (syntax->datum #'x2))
              (or (free-identifier=? #'x1 #'x2)
                  (eqv? (env1 (syntax->datum #'x1))
                        (env2 (syntax->datum #'x2))))]
          [_else #f])]
    [_else 
     (raise-syntax-error 'alpha= "Dropped through:" expr1)
     #f]))

;; convert syntax into a list of symbols
(define (syntax->symbols stx)
  (syntax-case stx ()
    [(vars ...)
     (map
      (lambda (s)
        (syntax->datum s))
      (syntax->list #'(vars ...)))]))

;; alph=: target-expr target-expr -> boolean
;; are two target expressions alpha-equivalent?
(define (alpha= expr1 expr2)
  (define r (alpha=/env empty-env empty-env expr1 expr2))
  (unless r
    (error 'alpha= "Not alpha=:\t~S\n\t~S\n" (syntax->datum expr1) (syntax->datum expr2)))
  r)

(define normalize-term (make-anormal-term (lambda _ (error 'anormal "No elim-letrec given."))))

(define anormal-tests
  (test-suite
   "Anormalization"
   (test-suite
    "Base Cases"
    
    (test-case
     "Top level identifier"
     (check alpha= (normalize-term (expand-syntax (syntax car)))
            (expand-syntax (syntax car))))
    
    (test-case
     "Simple arithmetic expression"
     (check alpha= (normalize-term (expand-syntax (syntax (+ 1 1))))
            (expand-syntax (syntax (+ 1 1)))))
    
    (test-case
     "lambda-expression with constant body"
     (check alpha= (normalize-term (expand-syntax (syntax (lambda (x) 3))))
            (expand-syntax (syntax (lambda (x) 3)))))
    
    (test-case
     "lambda-expression with var-ref body"
     (check alpha= (normalize-term (expand-syntax (syntax (lambda (x) x))))
            (expand-syntax (syntax (lambda (x) x)))))
    
    (test-case
     "lambda-expression/constant-body/multiple formals"
     (check alpha= (normalize-term (expand-syntax (syntax (lambda (x y z) 3))))
            (expand-syntax (syntax (lambda (x y z) 3)))))       
    
    (test-case
     "two-armed-if"
     (check alpha= (normalize-term (expand-syntax (syntax (if #t 1 2))))
            (expand-syntax (syntax (if #t 1 2)))))
    
    (test-case
     "let/var-ref in body"
     (check alpha= (normalize-term (expand-syntax (syntax (let ([x 1]) x))))
            (expand-syntax (syntax ((lambda (x) x) 1)))))
    
    (test-case
     "call to void"
     (check alpha= (normalize-term (expand-syntax (syntax (void))))
            (expand-syntax (syntax (void)))))
    
    (test-case
     "primitive application/multiple arguments"
     (check alpha= (normalize-term (expand-syntax (syntax (+ 1 2 3))))
            (expand-syntax (syntax (+ 1 2 3)))))
    
    #;(test-case
     "empty-list"
     (check alpha= (normalize-term (expand-syntax (syntax ())))
            (expand-syntax (syntax ()))))
    
    (test-case
     "quoted list of constants"
     (check alpha= (normalize-term (expand-syntax (syntax '(1 2 3))))
            (expand-syntax (syntax '(1 2 3))))))
   
   (test-suite
    "Inductive Cases"
    
    (test-case
     "nested primitive applications with multiple arguments"
     (check alpha= (normalize-term (expand-syntax (syntax (* (+ 1 2) 3))))
            (expand-syntax (syntax ((lambda (x) (* x 3)) (+ 1 2))))))
        
    (test-case
     "two-armed if with prim-app in test posn"
     (check alpha= (normalize-term (expand-syntax (syntax (if (+ 1 2) 3 4))))
            (expand-syntax (syntax ((lambda (x) (if x 3 4)) (+ 1 2))))))
    
    (test-case
     "nested single argument primitive applications"
     (check alpha= (normalize-term (expand-syntax (syntax (* (+ 1)))))
            (expand-syntax (syntax ((lambda (x0) (* x0)) (+ 1))))))
    
    (test-case
     "deeply nested primitive applications"
     (check alpha= (normalize-term (expand-syntax (syntax (* (+ (+ (+ 1 2) 3) 4) (+ 5 6)))))
            (expand-syntax (syntax ((lambda (x0)
                               ((lambda (x1)
                                  ((lambda (x2)
                                     ((lambda (x3) (* x2 x3))
                                      (+ 5 6)))
                                   (+ x1 4)))
                                (+ x0 3)))
                             (+ 1 2))))))
    
    (test-case
     "deeply nested primitive applications"
     (check alpha= (normalize-term (expand-syntax (syntax (* (+ 1 2) (+ 1 (+ 2 (+ 3 4)))))))
            (expand-syntax (syntax ((lambda (x0)
                               ((lambda (x1)
                                  ((lambda (x2)
                                     ((lambda (x3)
                                        (* x0 x3))
                                      (+ 1 x2)))
                                   (+ 2 x1)))
                                (+ 3 4)))
                             (+ 1 2))))))
    
    (test-case
     "if nested in test position"
     (check alpha= (normalize-term (expand-syntax (syntax (if (if #t #f #t) #t #t))))
            (expand-syntax (syntax ((lambda (x) (if x #t #t)) (if #t #f #t))))))
    
    (test-case
     "procedure/body has nested if"
     (check alpha= (normalize-term (expand-syntax (syntax (lambda (x) (if (if x 1 2) 3 4)))))
            (expand-syntax (syntax (lambda (x)
                              ((lambda (y0) (if y0 3 4))
                               (if x 1 2)))))))
    
    (test-case
     "constant 0-arg procedure application"
     (check alpha= (normalize-term (expand-syntax (syntax ((lambda () 3)))))
            (expand-syntax (syntax ((lambda () 3))))))
    
    (test-case
     "if with function application in test"
     (check alpha= (normalize-term (expand-syntax (syntax (if ((lambda () 7)) 1 2))))
            (expand-syntax (syntax ((lambda (x) (if x 1 2))
                             ((lambda () 7)))))))
    
    (test-case
     "if with lambda-expression in consequent and alternative"
     (check alpha= (normalize-term (expand-syntax (syntax ((if #t (lambda () 1) (lambda () 2))))))
            (expand-syntax (syntax ((lambda (x) (x)) (if #t (lambda () 1) (lambda () 2)))))))
    
    (test-case
     "call/cc with value argument"
     (check alpha= (normalize-term (expand-syntax (syntax (call/cc (lambda (x) x)))))
            (expand-syntax (syntax (call/cc (lambda (x) x))))))
    
    (test-case
     "call/cc with complex expression in argument"
     (check alpha= (normalize-term (expand-syntax (syntax (call/cc (f (g 7))))))
            (expand-syntax (syntax ((lambda (x0)
                               ((lambda (x1) (call/cc x1))
                                (f x0)))
                             (g 7)))))))
   
   (test-suite
    "Additional tests"
    
    (test-case
     "multiple body expressions in lambda"
     (check alpha= (normalize-term (expand-syntax (syntax (lambda (x y z) 3 4))))
            (expand-syntax (syntax (lambda (x y z) 
                              (call-with-values (lambda () 3)
                                                (lambda throw-away 4)))))))
    
    (test-case
     "zero-or-more argument lambda"
     (check alpha= (normalize-term (expand-syntax (syntax (lambda x x))))
            (expand-syntax (syntax (lambda x x)))))
    
    (test-case
     "multi-valued let-values"
     (check alpha= (normalize-term (expand-syntax (syntax (let-values ([(x y) (values 1 2)]) (+ x y)))))
            (expand-syntax (syntax (call-with-values (lambda () (values 1 2))
                                              (lambda (x y) (+ x y)))))))
    (test-case
     "let/multiple clauses before body"
     (check alpha= (normalize-term (expand-syntax (syntax (let ([x 1] [y 2]) (+ x y)))))
            (expand-syntax (syntax ((lambda (x)
                               ((lambda (y)
                                  (+ x y))
                                2))
                             1))))))
   
   (test-suite
    "Miscellaneous tests"
    
    #;(test-case
     "empty begin"
     (check alpha= (normalize-term (expand-syntax (syntax (begin))))
            (expand-syntax (syntax (void)))))
    
    (test-case
     "begin with one expression"
     (check alpha= (normalize-term (expand-syntax (syntax (begin 1))))
            (expand-syntax (syntax (quote 1)))))
    
    (test-case
     "begin with multiple expressions"
     (check alpha= (normalize-term (expand-syntax (syntax (begin 1 2 3))))
            (normalize-term (expand-syntax (syntax (call-with-values
                                             (lambda () 1)
                                             (lambda throw-away
                                               (call-with-values
                                                (lambda () 2)
                                                (lambda throw-away
                                                  3)))))))))
    
    (test-case
     "cond expression"
     (check-true
      (and
       (with-handlers ([(lambda (x) #t)
                        (lambda (the-exn) #f)])
         (normalize-term
          (expand-syntax
           (syntax
            (cond
              [(null? l) 1]
              [(zero? (car l)) (k 0)]
              [else
               (* (car l) (cdr l))])))))
       #t)))
    
    ; XXX Anormal only works on expressions
    #;(test-not-exn "define-struct" 
                  (lambda () (normalize-term (expand-syntax (syntax (define-struct posn (x y)))))))
    (test-not-exn "quote-syntax: #f" 
                  (lambda () (parameterize ([transformer? #f])
                               (normalize-term (expand-syntax (syntax #'provide/contract-id-set-a-date-day!))))))
    ; XXX I don't know if this SHOULD work
    #;(test-not-exn "quote-syntax: #t"
                  (lambda () (parameterize ([transformer? #t])
                               (normalize-term (expand-syntax (syntax #'provide/contract-id-set-a-date-day!))))))
    )))
