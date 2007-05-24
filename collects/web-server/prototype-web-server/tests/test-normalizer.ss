(module test-normalizer mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           "../normalizer.ss")
  (provide test-normalizer-suite)
  
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
    (syntax-case expr1 (if #%app)
      [(if tst1 csq1)
       (syntax-case expr2 (if)
         [(if tst2 csq2) (and (alpha=/env env1 env2 #'tst1 #'tst2)
                              (alpha=/env env1 env2 #'csq1 #'csq2))]
         [_else #f])]
      [(if tst1 csq1 alt1)
       (syntax-case expr2 (if)
         [(if tst2 csq2 alt2) (and (alpha=/env env1 env2 #'tst1 #'tst2)
                                   (alpha=/env env1 env2 #'csq1 #'csq2)
                                   (alpha=/env env1 env2 #'alt1 #'alt2))]
         [_else #f])]
      [(#%app rator1 rands1 ...)
       (syntax-case expr2 (#%app)
         [(#%app rator2 rands2 ...)
          (and (alpha=/env env1 env2 #'rator1 #'rator2)
               (let loop ([rs1 (syntax->list #'(rands1 ...))]
                          [rs2 (syntax->list #'(rands2 ...))])
                 (or (and (null? rs1)
                          (null? rs2))
                     (and (alpha=/env env1 env2 (car rs1) (car rs2))
                          (loop (cdr rs1) (cdr rs2))))))]
         [_else #f])]
      [_else (w-alpha=/env env1 env2 expr1 expr2)]))
  
  ;; w-alpha=/env: env target-expr target-expr -> boolean
  ;; are two target vars or vals alpha-equivalent?
  (define (w-alpha=/env env1 env2 expr1 expr2)
    (syntax-case expr1 (#%top #%datum lambda quote)
      [(#%top . var1)
       (syntax-case expr2 (#%top)
         [(#%top . var2)
          (eqv? (syntax-object->datum #'var1)
                (syntax-object->datum #'var2))]
         [_else #f])]
      [(#%datum . datum1)
       (syntax-case expr2 (#%datum)
         [(#%datum . datum2)
          (let ([dat1 (syntax-object->datum #'datum1)]
                [dat2 (syntax-object->datum #'datum2)])
            (eqv? dat1 dat2))]
         [_else #f])]
      [(quote datum1)
       (syntax-case expr2 (quote)
         [(quote datum2)
          (let ([dat1 (syntax-object->datum #'datum1)]
                [dat2 (syntax-object->datum #'datum2)])
            (equal? dat1 dat2))]
         [_else #f])]
      [(lambda (formals1 ...) body1)
       (syntax-case expr2 (lambda)
         [(lambda (formals2 ...) body2)
          (let ([syms (map gensym (syntax->symbols #'(formals1 ...)))])
            (and (= (length syms) (length (syntax->list #'(formals2 ...))))
                 (alpha=/env
                  (extend env1 (syntax->symbols #'(formals1 ...)) syms)
                  (extend env2 (syntax->symbols #'(formals2 ...)) syms)
                  #'body1 #'body2)))]
         [_else #f])]
      [x1 (symbol? (syntax-object->datum #'x1))
          (syntax-case expr2 ()
            [x2 (symbol? (syntax-object->datum #'x2))
                (or (module-identifier=? #'x1 #'x2)
                    (eqv? (env1 (syntax-object->datum #'x1))
                          (env2 (syntax-object->datum #'x2))))]
            [_else #f])]
      [_else #f]))
  
  ;; convert syntax into a list of symbols
  (define (syntax->symbols stx)
    (syntax-case stx ()
      [(vars ...)
       (map
        (lambda (s)
          (syntax-object->datum s))
        (syntax->list #'(vars ...)))]))
  
  ;; alph=: target-expr target-expr -> boolean
  ;; are two target expressions alpha-equivalent?
  (define (alpha= expr1 expr2)
    (alpha=/env empty-env empty-env expr1 expr2))
  
  (define-syntax (check-unsupported-lambda stx)
    (syntax-case stx ()
      [(_ expr)
       #'(with-handlers ([(lambda (x) #t)
                          (lambda (the-exn)
                            (string=? "lambda: Not all lambda-expressions supported"
                                      (exn-message the-exn)))])
           expr)]))
  
  (define-syntax (check-unsupported-let stx)
    (syntax-case stx ()
      [(_ expr)
       #'(with-handlers ([(lambda (x) #t)
                          (lambda (the-exn)
                            (string=? "let-values: Not all let-values-expressions supported"
                                      (exn-message the-exn)))])
           expr)]))
  
  ;; **************************************************
  ;; **************************************************
  ;; ACTUAL TESTS
  
  (define test-normalizer-suite
    (make-test-suite
     "Tests for Normalization Phase"
     (make-test-suite
      "Base Cases"
      
      (make-test-case
       "Top level identifier"
       (assert alpha= (normalize-term (expand (syntax car)))
               (expand (syntax car))))
      
      (make-test-case
       "Simple arithmetic expression"
       (assert alpha= (normalize-term (expand (syntax (+ 1 1))))
               (expand (syntax (+ 1 1)))))
      
      (make-test-case
       "lambda-expression with constant body"
       (assert alpha= (normalize-term (expand (syntax (lambda (x) 3))))
               (expand (syntax (lambda (x) 3)))))
      
      (make-test-case
       "lambda-expression with var-ref body"
       (assert alpha= (normalize-term (expand (syntax (lambda (x) x))))
               (expand (syntax (lambda (x) x)))))
      
      (make-test-case
       "lambda-expression/constant-body/multiple formals"
       (assert alpha= (normalize-term (expand (syntax (lambda (x y z) 3))))
               (expand (syntax (lambda (x y z) 3)))))
      
      (make-test-case
       "one-armed-if"
       (assert alpha= (normalize-term (expand (syntax (if #t 1))))
               (expand (syntax (if #t 1)))))
      
      
      (make-test-case
       "two-armed-if"
       (assert alpha= (normalize-term (expand (syntax (if #t 1 2))))
               (expand (syntax (if #t 1 2)))))
      
      (make-test-case
       "let/var-ref in body"
       (assert alpha= (normalize-term (expand (syntax (let ([x 1]) x))))
               (expand (syntax ((lambda (x) x) 1)))))
      
      (make-test-case
       "call to void"
       (assert alpha= (normalize-term (expand (syntax (void))))
               (expand (syntax (void)))))
      
      (make-test-case
       "primitive application/multiple arguments"
       (assert alpha= (normalize-term (expand (syntax (+ 1 2 3))))
               (expand (syntax (+ 1 2 3)))))
      
      (make-test-case
       "empty-list"
       (assert alpha= (normalize-term (expand (syntax ())))
               (expand (syntax ()))))
      
      (make-test-case
       "qoted list of constants"
       (assert alpha= (normalize-term (expand (syntax '(1 2 3))))
               (expand (syntax '(1 2 3))))))
     
     (make-test-suite
      "Inductive Cases"
      
      (make-test-case
       "nested primitive applications with multiple arguments"
       (assert alpha= (normalize-term (expand (syntax (* (+ 1 2) 3))))
               (expand (syntax ((lambda (x) (* x 3)) (+ 1 2))))))
      
      (make-test-case
       "one-armed if with prim-app in test posn"
       (assert alpha= (normalize-term (expand (syntax (if (+ 1 2) 3))))
               (expand (syntax ((lambda (x) (if x 3)) (+ 1 2))))))
      
      (make-test-case
       "two-armed if with prim-app in test posn"
       (assert alpha= (normalize-term (expand (syntax (if (+ 1 2) 3 4))))
               (expand (syntax ((lambda (x) (if x 3 4)) (+ 1 2))))))
      
      (make-test-case
       "nested single argument primitive applications"
       (assert alpha= (normalize-term (expand (syntax (* (+ 1)))))
               (expand (syntax ((lambda (x0) (* x0)) (+ 1))))))
      
      (make-test-case
       "deeply nested primitive applications"
       (assert alpha= (normalize-term (expand (syntax (* (+ (+ (+ 1 2) 3) 4) (+ 5 6)))))
               (expand (syntax ((lambda (x0)
                                  ((lambda (x1)
                                     ((lambda (x2)
                                        ((lambda (x3) (* x2 x3))
                                         (+ 5 6)))
                                      (+ x1 4)))
                                   (+ x0 3)))
                                (+ 1 2))))))
      
      (make-test-case
       "deeply nested primitive applications"
       (assert alpha= (normalize-term (expand (syntax (* (+ 1 2) (+ 1 (+ 2 (+ 3 4)))))))
               (expand (syntax ((lambda (x0)
                                  ((lambda (x1)
                                     ((lambda (x2)
                                        ((lambda (x3)
                                           (* x0 x3))
                                         (+ 1 x2)))
                                      (+ 2 x1)))
                                   (+ 3 4)))
                                (+ 1 2))))))
      
      (make-test-case
       "if nested in test position"
       (assert alpha= (normalize-term (expand (syntax (if (if #t #f #t) #t #t))))
               (expand (syntax ((lambda (x) (if x #t #t)) (if #t #f #t))))))
      
      (make-test-case
       "procedure/body has nested if"
       (assert alpha= (normalize-term (expand (syntax (lambda (x) (if (if x 1 2) 3 4)))))
               (expand (syntax (lambda (x)
                                 ((lambda (y0) (if y0 3 4))
                                  (if x 1 2)))))))
      
      (make-test-case
       "constant 0-arg procedure application"
       (assert alpha= (normalize-term (expand (syntax ((lambda () 3)))))
               (expand (syntax ((lambda () 3))))))
      
      (make-test-case
       "if with function application in test"
       (assert alpha= (normalize-term (expand (syntax (if ((lambda () 7)) 1 2))))
               (expand (syntax ((lambda (x) (if x 1 2))
                                ((lambda () 7)))))))
      
      (make-test-case
       "if with lambda-expression in consequent and alternative"
       (assert alpha= (normalize-term (expand (syntax ((if #t (lambda () 1) (lambda () 2))))))
               (expand (syntax ((lambda (x) (x)) (if #t (lambda () 1) (lambda () 2)))))))
      
      (make-test-case
       "call/cc with value argument"
       (assert alpha= (normalize-term (expand (syntax (call/cc (lambda (x) x)))))
               (expand (syntax (call/cc (lambda (x) x))))))
      
      (make-test-case
       "call/cc with complex expression in argument"
       (assert alpha= (normalize-term (expand (syntax (call/cc (f (g 7))))))
               (expand (syntax ((lambda (x0)
                                  ((lambda (x1) (call/cc x1))
                                   (f x0)))
                                (g 7)))))))
     
     (make-test-suite
      "Check that certain errors are raised"
      
      (make-test-case
       "multiple body expressions in lambda"
       (assert-true (check-unsupported-lambda
                     (normalize-term (expand (syntax (lambda (x y z) 3 4)))))))
      
      (make-test-case
       "zero-or-more argument lambda"
       (assert-true (check-unsupported-lambda
                     (normalize-term (expand (syntax (lambda x x)))))))
      
      ; this is supported now
      #; (make-test-case
          "multi-valued let-values"
          (assert-true (check-unsupported-let
                        (normalize-term (expand (syntax (let-values ([(x y) (values 1 2)]) (+ x y))))))))
      ; this is supported now
      #; (make-test-case
       "let/multiple clauses before body"
       (assert-true (check-unsupported-let
                     (normalize-term (expand (syntax (let ([x 1] [y 2]) (+ x y)))))))))
     
     (make-test-suite
      "Miscellaneous tests"
      
      (make-test-case
       "empty begin"
       (assert alpha= (normalize-term (expand (syntax (begin))))
               (syntax (#%app (#%top . void)))))
      
      (make-test-case
       "begin with one expression"
       (assert alpha= (normalize-term (expand (syntax (begin 1))))
               (syntax (#%datum . 1))))
      
      (make-test-case
       "begin with multiple expressions"
       (assert alpha= (normalize-term (expand (syntax (begin 1 2 3))))
               (normalize-term (expand (syntax (let ([throw-away 1])
                                                 (let ([throw-away 2])
                                                   3)))))))
      
      (make-test-case
       "cond expression"
       (assert-true
        (and
         (with-handlers ([(lambda (x) #t)
                          (lambda (the-exn) #f)])
           (normalize-term
            (expand
             (syntax
              (cond
                [(null? l) 1]
                [(zero? (car l)) (k 0)]
                [else
                 (* (car l) (cdr l))])))))
         #t)))))))
