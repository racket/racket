#lang scheme/base

(require (for-syntax scheme/base))

(require (planet "test-compat2.ss" ("schematics" "schemeunit.plt" 2 10)))
(require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 10)))

(require mzlib/plt-match)

(require "match-tests.ss" "other-plt-tests.ss" "other-tests.ss" "examples.ss")

(require (planet "views.ss" ("cobbe" "views.plt" 1 1)))

(define reg-tests
  (make-test-suite "Tests for regressions"
                   (make-test-case "quote in qp"
                                   (assert eq? #t (match '(tile a b c)
                                                    [`(tile ,@'(a b c))
                                                     #t]
                                                    [else #f]))
                                   (assert eq? #t (match '(tile a b c)
                                                    [`(tile ,@`(a b c))
                                                     #t]
                                                    [else #f])))))
(define cons-tests
  (make-test-suite "Tests for cons pattern"
                   (make-test-case "simple"
                                   (assert = 3 (match (cons 1 2) [(cons a b) (+ a b)])))))

(define match-expander-tests
  (make-test-suite
   "Tests for define-match-expander"
   (make-test-case "Trivial expander"
                   (let ()
                     (define-match-expander bar (lambda (x) #'_) +)
                     (assert = 4 (match 3 [(app add1 x) x])) ; other stuff still works
                     (assert-true (match 3 [(bar) #t])) ; (bar) matches anything
                     (assert = 12 (bar 3 4 5))
                     (assert = 12 (apply bar '(3 4 5))))) ; bar works like +
   
   (make-test-case "Trivial expander w/ keywords"
                   (let ()
                     (define-match-expander bar #:plt-match (lambda (x) #'_) #:expression +)
                     (assert = 4 (match 3 [(app add1 x) x])) ; other stuff still works
                     (assert-true (match 3 [(bar) #t])) ; (bar) matches anything
                     (assert = 12 (bar 3 4 5))
                     (assert = 12 (apply bar '(3 4 5))))) ; bar works like +
   
   ;; gross hack to check for syntax errors
   (make-test-case "Only one xform gives syntax error"
                   (assert-exn exn:fail:syntax?
                               (lambda ()
                                 (expand #'(let ()
                                             (define-match-expander bar (lambda (x) #'_))
                                             (bar 3 4))))))
   
   ;; more complex example from Dale
   (make-test-case "Point structs"
                   (let ()
                     (define-struct point (x y))
                     (define-match-expander Point
                       (lambda (x)
                         (syntax-case x ()
                           ((Point a b) #'(struct point (a b)))))
                       make-point)
                     ;; check that it works as expression and as pattern
                     (assert = 5 (match (Point 2 3)
                                   [(Point x y) (+ x y)]))
                     ;; check that sub-patterns still work
                     (assert = 7 (match (make-point 2 3)
                                   [(Point (app add1 x) (app add1 y)) (+ x y)]))
                     ;; check that it works inside a list
                     (assert = 7 (match (list (make-point 2 3))
                                   [(list (Point (app add1 x) (app add1 y))) (+ x y)]))
                     ))
   
   ;; from richard's view documentation
   
   (make-test-case "Natural number views"
                   (let ()
                     (define natural-number?
                       (lambda (x)
                         (and (integer? x)
                              (>= x 0))))
                     (define natural-zero? (lambda (x) (and (integer? x) (zero? x))))
                     
                     (define-view peano-zero natural-zero? ())
                     (define-view peano-succ natural-number? (sub1))
                     
                     (define factorial
                       (match-lambda
                         [(peano-zero) 1]
                         [(and (peano-succ pred) n) (* n (factorial pred))]))
                     (assert = 120 (factorial 5))))
   
   ;; more complex example from Dale
   (make-test-case "Point structs with keywords"
                   (let ()
                     (define-struct point (x y))
                     (define-match-expander Point
                       #:plt-match
                       (lambda (x)
                         (syntax-case x ()
                           ((Point a b) #'(struct point (a b)))))
                       #:expression make-point) 
                     ;; check that it works as expression and as pattern
                     (assert = 5 (match (Point 2 3)
                                   [(Point x y) (+ x y)]))
                     ;; check that sub-patterns still work
                     (assert = 7 (match (make-point 2 3)
                                   [(Point (app add1 x) (app add1 y)) (+ x y)]))
                     ;; check that it works inside a list
                     (assert = 7 (match (list (make-point 2 3))
                                   [(list (Point (app add1 x) (app add1 y))) (+ x y)]))
                     ))
   ))

(define simple-tests 
  (make-test-suite
   "Some Simple Tests"
   (make-test-case "Trivial"
                   (assert = 3 (match 3 [x x])))
   (make-test-case "no order"
                   (assert equal? #t (match '(1 2 3 1) 
                                       [(list-no-order 3 2 1 1) #t]
                                       [_ #f])))
   (make-test-case "app pattern"
                   (assert = 4 (match 3 [(app add1 y) y])))
   (make-test-case "struct patterns"
                   (let ()
                     (define-struct point (x y))
                     (define (origin? pt)
                       (match pt
                         ((struct point (0 0)) #t)
                         (else #f)))
                     (assert-true (origin? (make-point 0 0)))
                     (assert-false (origin? (make-point 1 1)))))
   ))

(define nonlinear-tests
  (make-test-suite 
   "Non-linear patterns"
   (make-test-case "Very simple"
                   (assert = 3 (match '(3 3) [(list a a) a])))
   (make-test-case "Fails"
                   (assert-exn exn:misc:match? (lambda () (match '(3 4) [(list a a) a]))))
   (make-test-case "Use parameter"
                   (parameterize ([match-equality-test eq?])
                     (assert = 5 (match '((3) (3)) [(list a a) a] [_ 5]))))
   (make-test-case "Nonlinear patterns use equal?"
                   (assert equal? '(3) (match '((3) (3)) [(list a a) a] [_ 5])))))


(define doc-tests
  (make-test-suite 
   "Tests from Help Desk Documentation"
   (make-test-case "match-let"
                   (assert = 6 (match-let ([(list x y z) (list 1 2 3)]) (+ x y z))))     
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
                     
                     (assert equal? '(lambda (x y) x) (unparse (parse '(lambda (x y) x))))))
   
   (make-test-case "counter : match-define"
                   (let ()
                     (match-define (list inc value reset)
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

(define struct*-tests
  (make-test-suite 
   "Tests of struct*"
   (make-test-case "not an id for struct"
                   (assert-exn exn:fail:syntax?
                               (lambda ()
                                 (expand #'(let ()
                                             (define-struct tree (val left right))
                                             (match (make-tree 0 1 2)
                                               [(struct* 4 ())
                                                #f]))))))
   (make-test-case "not a struct-info for struct"
                   (assert-exn exn:fail:syntax?
                               (lambda ()
                                 (expand #'(let ()
                                             (define-syntax tree 1)
                                             (match 1
                                               [(struct* tree ())
                                                #f]))))))
   (make-test-case "bad form"
                   (assert-exn exn:fail:syntax?
                               (lambda ()
                                 (expand #'(let ()
                                             (define-struct tree (val left right))
                                             (match (make-tree 0 1 2)
                                               [(struct* tree ([val]))
                                                #f]))))))
   (make-test-case "bad form"
                   (assert-exn exn:fail:syntax?
                               (lambda ()
                                 (expand #'(let ()
                                             (define-struct tree (val left right))
                                             (match (make-tree 0 1 2)
                                               [(struct* tree (val))
                                                #f]))))))
   (make-test-case "field appears twice"
                   (assert-exn exn:fail:syntax?
                               (lambda ()
                                 (expand #'(let ()
                                             (define-struct tree (val left right))
                                             (match (make-tree 0 1 2)
                                               [(struct* tree ([val 0] [val 0]))
                                                #f]))))))
   (make-test-case "not a field"
                   (assert-exn exn:fail:syntax?
                               (lambda ()
                                 (expand #'(let ()
                                             (define-struct tree (val left right))
                                             (match (make-tree 0 1 2)
                                               [(struct* tree ([feet 0]))
                                                #f]))))))
   (make-test-case "super structs don't work"
                   (assert-exn exn:fail:syntax?
                               (lambda ()
                                 (expand #'(let ()
                                             (define-struct extra (foo))
                                             (define-struct (tree extra) (val left right))
                                             (match (make-tree #f 0 1 2)
                                               [(struct* tree ([extra #f] [val 0]))
                                                #f]))))))
   (make-test-case "super struct kinda work"
                   (let ()
                     (define-struct extra (foo))
                     (define-struct (tree extra) (val left right))
                     (match (make-tree #f 0 1 2)
                       [(struct* tree ([val a]))
                        (assert = 0 a)])))
   (make-test-case "from documentation"
                   (let ()
                     (define-struct tree (val left right))
                     (match-define 
                      (struct* 
                       tree 
                       ([val a]
                        [left
                         (struct*
                          tree 
                          ([right #f]
                           [val b]))]))
                      (make-tree 0 (make-tree 1 #f #f) #f))
                     (assert = 0 a)
                     (assert = 1 b)))))

(define plt-match-tests
  (make-test-suite "Tests for plt-match.ss"
                   doc-tests
                   cons-tests
                   simple-tests
                   nonlinear-tests
                   match-expander-tests
                   reg-tests
                   struct*-tests
                   ))

(define (run-tests)
  (test/text-ui (make-test-suite "Match Tests"
                                 plt-match-tests
                                 match-tests
                                 new-tests
                                 ;; from bruce
                                 other-tests 
                                 other-plt-tests
                                 )))
(unless (= 0 (run-tests))
  (error "Match Tests did not pass."))
