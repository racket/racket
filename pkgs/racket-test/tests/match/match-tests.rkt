(module match-tests racket/base
  (require racket/match rackunit
           (for-syntax racket/base))
  
  (provide match-tests)
  
  (define match-expander-tests
    (test-suite
     "Tests for define-match-expander"
     (test-case "Trivial expander"
                     (let ()
                       (define-match-expander bar
                         (lambda (x) #'_)
                         (lambda (stx)
                           (syntax-case stx ()
                             [(_ x ...) #'(+ x ...)]
                             [_
                              (identifier? stx)
                              #'+])))
                       (check = 4 (match 3 [(app add1 x) x])) ; other stuff still works
                       (check-true (match 3 [(bar) #t])) ; (bar) matches anything
                       (check = 12 (bar 3 4 5))
                       (check = 12 (apply bar '(3 4 5))))) ; bar works like +     
     ))
  
 
  
  (define simple-tests 
    (test-suite
     "Some Simple Tests"
     (test-case "Trivial"
                     (check = 3 (match 3 [x x])))
     (test-case "app pattern"
                     (check = 4 (match 3 [(app add1 y) y])))
     (test-case "struct patterns"
                     (let ()
                       (define-struct point (x y))
                       (define (origin? pt)
                         (match pt
                           ((struct point (0 0)) #t)
                           (_ #f)))
                       (check-true (origin? (make-point 0 0)))
                       (check-false (origin? (make-point 1 1)))))
     (test-case "empty hash-table pattern bug"
       (check-equal? (match #hash((1 . 2))
                       [(hash-table) "empty"]
                       [_ "non-empty"])
                     "non-empty")
       (check-equal? (match #hash()
                       [(hash-table) "empty"]
                       [_ "non-empty"])
                     "empty"))
     ))

  (define hash-table-no-rep-tests
    (test-suite "hash-table patterns (no ..k)"
      (test-case "literal keys"
        (check-equal? (match (hash 1 2 3 4)
                        [(hash-table (1 a) (3 b))
                         (list a b)])
                      '(2 4))

        (check-equal? (match (hash 'a 'b 'c 'd)
                        [(hash-table ('c x) ('a y))
                         (list x y)])
                      '(d b))

        (check-true (match (hash)
                      [(hash-table) #t]))

        (check-equal? (match (hash "a" "b" "c" "d" "e" "f")
                        [(hash-table ("e" x) ("a" "b") ("c" y))
                         (list x y)])
                      '("f" "d")))

      (test-case "literal keys predicate"
        (check-equal? (match (hash 'a 'b 'c 'd)
                        [(hash-table ('a x)) 1]
                        [(hash-table ('a x) ('c y)) 2])
                      2)

        (check-equal? (match (hash 1 2 3 4)
                        [(hash-table) 1]
                        [(hash-table (1 a)) 2]
                        [(hash-table (2 a)) 3]
                        [(hash-table (1 a) (3 b) (5 c)) 4]
                        [(hash-table (1 a) (2 b)) 5]
                        [(hash-table (1 10) (3 b)) 6]
                        [(hash-table (1 a) (3 b)) 7]
                        [(hash-table (1 a) (3 4)) 8])
                      7)

        ;; Duplicate keys
        (check-equal? (match (hash "a" "b" "c" "d")
                        [(hash-table ("a" x) ("a" y))
                         (list x y)]
                        [(hash-table _ _) 42])
                      42)

        (check-equal? (match (hash "a" "b")
                        [(hash-table ("a" x) ("a" y))
                         (list x y)]
                        [(hash-table _) 42])
                      42)
        (check-equal? (match (hash "a" "b")
                        [(hash-table ("a" x) ("a" y))
                         (list x y)]
                        [(hash-table _) 42])
                      (match (hash "a" "b")
                        [(hash-table ((== "a") x) ("a" y))
                         (list x y)]
                        [(hash-table _) 42])))

      (test-case "non literal keys"
        (check-equal? (match (hash (list 1 2) 'b (list 3 4) 'd)
                        [(hash-table ((list 1 2) x) ((list 3 4) y)) (list x y)])
                      '(b d))

        (check-equal? (match (hash (list 1 2) 'b (list 3 4) 'd)
                        [(hash-table (c d) (a 'b)) (list a c d)])
                      '((1 2) (3 4) d))

        (check-equal? (match (hash (list 1 2) 'b (list 3 4) 'd)
                        [(hash-table p (a 'b)) (list a p)])
                      '((1 2) ((3 4) d)))

        (check-equal? (match (hash (list 1 2) 'x (list 3 4) 'x)
                        [(hash-table _ q) (cadr q)])
                      'x))

      (test-case "non literal keys predicate"
        (check-equal? (match (hash (list 1 2) 'b (list 3 4) 'd)
                        [(hash-table) 1]
                        [(hash-table a) 2]
                        [(hash-table a b c) 3]
                        [(hash-table a b) 4]
                        [(hash-table (p 'd) _) p])
                      4)

        (check-equal? (match (hash (list 1 2) 'b (list 3 4) 'd)
                        [(hash-table) 1]
                        [(hash-table a) 2]
                        [(hash-table a b c) 3]
                        [(hash-table (p 'd) _) p]
                        [(hash-table a b) 4])
                      (list 3 4))

        (check-equal? (match (hash 1 2)
                        [(hash-table (a b) (c d)) 1]
                        [_ 42])
                      42))))

  (define hash-table-rep-tests
    (test-suite "hash-table patterns (with ..k)"
      (test-case "literal keys"
        (check-equal? (match (hash 1 2 3 4 5 6)
                        [(hash-table (3 b) (5 c) (1 a) _ ...)
                         (list a b c)])
                      '(2 4 6))

        (check-equal? (match (hash 1 2 3 4 5 6)
                        [(hash-table (3 a) (5 b) _ ...)
                         (list a b)])
                      '(4 6))

        (check-equal? (match (hash 1 2 3 4 5 6)
                        [(hash-table (3 a) (5 b) (_ _) ...)
                         (list a b)])
                      '(4 6))

        ;; Duplicate keys
        (check-equal? (match (hash 1 2 3 4 5 6)
                        [(hash-table (3 a) (3 b) _ ...)
                         (list a b)]
                        [_ 42])
                      42)
        (check-equal? (match (hash 1 2 3 4 5 6)
                        [(hash-table (3 a) (3 b) _ ...)
                         (list a b)]
                        [_ 42])
                      (match (hash 1 2 3 4 5 6)
                        [(hash-table ((== 3) a) (3 b) _ ...)
                         (list a b)]
                        [_ 42]))
        (check-equal? (match (hash 3 4)
                        [(hash-table (3 a) (3 b) _ ...)
                         (list a b)]
                        [_ 42])
                      42)

        (check-true (match (hash 1 2 3 4 5 6)
                      [(hash-table _ ...) #t]))

        (check-true (match (hash 1 2 3 4 5 6)
                      [(hash-table (1 2) _ ...) #t])))

      (test-case "literal keys predicate"
        (check-equal? (match (hash 1 2 3 4 5 6)
                        [(hash-table (3 42) (5 b) _ ...) #f]
                        [(hash-table (5 b) _ ...) b])
                      6)

        (check-equal? (match (hash 1 2 3 4 5 6)
                        [(hash-table (1 _) (3 _) (5 _) (7 _) _ ...) #f]
                        [(hash-table (2 _) _ ...) #f]
                        [(hash-table (5 b) _ ...) b])
                      6))

      (test-case "non literal keys"
        (check-equal? (match (hash 1 2 3 4 5 6)
                        [(hash-table (3 42) (5 b) _ ...) #f]
                        [(hash-table (5 b) _ ...) b])
                      6)

        (check-equal? (match (hash 1 2 3 4 5 6)
                        [(hash-table (a 2) _ ...) a])
                      1)

        (check-true (match (hash 1 2 3 4)
                      [(hash-table p _ _ ...) #t]))


        (check-equal? (match (hash 1 2 3 4)
                        [(hash-table (1 x) _ ...) x])
                      2)

        (check-equal? (match (hash 1 2 3 4)
                        [(hash-table (1 x) _ ..0) x])
                      2)

        (check-equal? (match (hash 1 2 3 4)
                        [(hash-table (1 x) _ ..1) x])
                      2)

        (check-equal? (match (hash 1 2 3 4)
                        [(hash-table (1 x) p ...) p])
                      (list (list 3 4)))

        (check-equal? (match (hash 1 2)
                        [(hash-table (1 x) p ...) p])
                      '()))

      (test-case "non literal keys predicate"
        (check-equal? (match (hash 1 2 3 4 5 6)
                        [(hash-table _ ..4) 1]
                        [(hash-table _ ..3) 2])
                      2)

        (check-equal? (match (hash 1 2 3 4 5 6)
                        [(hash-table a b c d _ ...) 1]
                        [(hash-table a b c _ ...) 2])
                      2)

        (check-equal? (match (hash 1 2 3 4 5 6)
                        [(hash-table (_ 10) b c _ ...) 1]
                        [(hash-table a b c _ ...) 2])
                      2))))

  (define nonlinear-tests
    (test-suite 
     "Non-linear patterns"
     (test-case "Very simple"
                     (check = 3 (match '(3 3) [(list a a) a])))
     (test-case "Fails"
                     (check-exn exn:misc:match? (lambda () (match '(3 4) [(list a a) a]))))
     (test-case "Use parameter"
                     (parameterize ([match-equality-test eq?])
                       (check = 5 (match '((3) (3)) [(list a a) a] [_ 5]))))
     (test-case "Uses equal?"
                     (check equal? '(3) (match '((3) (3)) [(list a a) a] [_ 5])))))
    
  
  (define doc-tests
    (test-suite 
     "Tests from Help Desk Documentation"
     (test-case "match-let"
                     (check = 6 (match-let ([(list x y z) (list 1 2 3)]) (+ x y z))))
     #;
     (test-case "set! pattern"
                     (let ()
                       (define x (list 1 (list 2 3)))
                       (match x [(_ (_ (set! setit)))  (setit 4)])
                       (check-equal? x '(1 (2 4)))))
     (test-case "lambda calculus"
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
                           [(list 'lambda (and args (list (? symbol?) ...) (not (? repeats?))) body)
                            (make-Lam args (parse body))]
                           [(list f args ...)
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
                           [(struct Var (s)) s]
                           [(struct Const (n)) n]
                           [(struct Lam (args body)) `(lambda ,args ,(unparse body))]
                           [(struct App (f args)) `(,(unparse f) ,@(map unparse args))]))
                       
                       (check equal? '(lambda (x y) x) (unparse (parse '(lambda (x y) x))))))
     
     (test-case "counter : match-define"
                     (let ()
                       (match-define (list inc value reset)
                         (let ([val 0])
                           (list
                            (lambda () (set! val (add1 val)))
                            (lambda () val)
                            (lambda () (set! val 0)))))
                       (inc)
                       (inc)
                       (check =  2 (value))
                       (inc)
                       (check = 3 (value))
                       (reset)
                       (check =  0 (value))))

     ))

  (define match-tests
    (test-suite "Tests for match.rkt"
                     doc-tests
                     simple-tests
                     nonlinear-tests
                     match-expander-tests
                     hash-table-no-rep-tests
                     hash-table-rep-tests))
  )
