(module match-tests mzscheme
  (require mzlib/match)
  
  (require (planet "test-compat2.ss" ("schematics" "schemeunit.plt" 2 10)))

  (provide match-tests)
  
  (define match-expander-tests
    (make-test-suite
     "Tests for define-match-expander"
     (make-test-case "Trivial expander"
                     (let ()
                       (define-match-expander bar #f (lambda (x) #'_) +)
                       (assert = 4 (match 3 [(= add1 x) x])) ; other stuff still works
                       (assert-true (match 3 [(bar) #t])) ; (bar) matches anything
                       (assert = 12 (bar 3 4 5))
                       (assert = 12 (apply bar '(3 4 5))))) ; bar works like +     
     (make-test-case "Trivial expander w/ keywords"
                     (let ()
                       (define-match-expander bar  #:match (lambda (x) #'_) #:expression +)
                       (assert = 4 (match 3 [(= add1 x) x])) ; other stuff still works
                       (assert-true (match 3 [(bar) #t])) ; (bar) matches anything
                       (assert = 12 (bar 3 4 5))
                       (assert = 12 (apply bar '(3 4 5))))) ; bar works like +        
     ))
  
 
  
  (define simple-tests 
    (make-test-suite
     "Some Simple Tests"
     (make-test-case "Trivial"
                     (assert = 3 (match 3 [x x])))
     (make-test-case "= pattern"
                     (assert = 4 (match 3 [(= add1 y) y])))
     (make-test-case "struct patterns"
                     (let ()
                       (define-struct point (x y))
                       (define (origin? pt)
                         (match pt
                           (($ point 0 0) #t)
                           (else #f)))
                       (assert-true (origin? (make-point 0 0)))
                       (assert-false (origin? (make-point 1 1)))))
     ))
  
  (define nonlinear-tests
    (make-test-suite 
     "Non-linear patterns"
     (make-test-case "Very simple"
                     (assert = 3 (match '(3 3) [(a a) a])))
     (make-test-case "Fails"
                     (assert-exn exn:misc:match? (lambda () (match '(3 4) [(a a) a]))))
     (make-test-case "Use parameter"
                     (parameterize ([match-equality-test eq?])
                       (assert = 5 (match '((3) (3)) [(a a) a] [_ 5]))))
     (make-test-case "Uses equal?"
                     (assert equal? '(3) (match '((3) (3)) [(a a) a] [_ 5])))))
    
  
  (define doc-tests
    (make-test-suite 
     "Tests from Help Desk Documentation"
     (make-test-case "match-let"
                     (assert = 6 (match-let ([(x y z) (list 1 2 3)]) (+ x y z))))
     #;
     (make-test-case "set! pattern"
                     (let ()
                       (define x (list 1 (list 2 3)))
                       (match x [(_ (_ (set! setit)))  (setit 4)])
                       (assert-equal? x '(1 (2 4)))))
     (make-test-case "lambda calculus"
                     (let ()
                       (define-struct Lam (args body))
                       (define-struct Var (s))
                       (define-struct Const (n))
                       (define-struct App (fun args))
                       
                       (define parse
                         (match-lambda
                           [(and s (? symbol?) (not 'lambda))
                            (make-Var s)]
                           [(? number? n)
                            (make-Const n)]
                           [('lambda (and args ((? symbol?) ...) (not (? repeats?))) body)
                            (make-Lam args (parse body))]
                           [(f args ...)
                            (make-App
                             (parse f)
                             (map parse args))]
                           [x (error 'syntax "invalid expression")]))
                       
                       (define repeats?
                         (lambda (l)
                           (and (not (null? l))
                                (or (memq (car l) (cdr l)) (repeats? (cdr l))))))
                       
                       (define unparse
                         (match-lambda
                           [($ Var s) s]
                           [($ Const n) n]
                           [($ Lam args body) `(lambda ,args ,(unparse body))]
                           [($ App f args) `(,(unparse f) ,@(map unparse args))]))
                       
                       (assert equal? '(lambda (x y) x) (unparse (parse '(lambda (x y) x))))))
     
     (make-test-case "counter : match-define"
                     (let ()
                       (match-define (inc value reset)
                                     (let ([val 0])
                                       (list
                                        (lambda () (set! val (add1 val)))
                                        (lambda () val)
                                        (lambda () (set! val 0)))))
                       (inc)
                       (inc)
                       (assert =  2 (value))
                       (inc)
                       (assert = 3 (value))
                       (reset)
                       (assert =  0 (value))))
                     
     ))
  
  (define match-tests
    (make-test-suite "Tests for match.ss"
                     doc-tests
                     simple-tests
                     nonlinear-tests
                     match-expander-tests))
  )