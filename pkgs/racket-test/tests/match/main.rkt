#lang scheme/base

(require (for-syntax scheme/base)
         "match-tests.rkt" "other-plt-tests.rkt" "other-tests.rkt"
         "examples.rkt"
         rackunit rackunit/text-ui)

(require mzlib/plt-match)

;(require (planet "views.ss" ("cobbe" "views.plt" 1 1)))

(define reg-tests
  (test-suite "Tests for regressions"
              (test-case "quote in qp"
                         (check eq? #t (match '(tile a b c)
                                         [`(tile ,@'(a b c))
                                          #t]
                                         [else #f]))
                         (check eq? #t (match '(tile a b c)
                                         [`(tile ,@`(a b c))
                                          #t]
                                         [else #f])))))
(define cons-tests
  (test-suite "Tests for cons pattern"
              (test-case "simple"
                         (check = 3 (match (cons 1 2) [(cons a b) (+ a b)])))))

(define match-expander-tests
  (test-suite
   "Tests for define-match-expander"
   (test-case "Trivial expander"
              (let ()
                (define-match-expander bar (lambda (x) #'_) +)
                (check = 4 (match 3 [(app add1 x) x])) ; other stuff still works
                (check-true (match 3 [(bar) #t])) ; (bar) matches anything
                (check = 12 (bar 3 4 5))
                (check = 12 (apply bar '(3 4 5))))) ; bar works like +
   
   (test-case "Trivial expander w/ keywords"
              (let ()
                (define-match-expander bar #:plt-match (lambda (x) #'_) #:expression +)
                (check = 4 (match 3 [(app add1 x) x])) ; other stuff still works
                (check-true (match 3 [(bar) #t])) ; (bar) matches anything
                (check = 12 (bar 3 4 5))
                (check = 12 (apply bar '(3 4 5))))) ; bar works like +
   
   ;; gross hack to check for syntax errors
   (test-case "Only one xform gives syntax error"
              (check-exn exn:fail:syntax?
                         (lambda ()
                           (expand #'(let ()
                                       (define-match-expander bar (lambda (x) #'_))
                                       (bar 3 4))))))
   
   ;; more complex example from Dale
   (test-case "Point structs"
              (let ()
                (define-struct point (x y))
                (define-match-expander Point
                  (lambda (x)
                    (syntax-case x ()
                      ((Point a b) #'(struct point (a b)))))
                  make-point)
                ;; check that it works as expression and as pattern
                (check = 5 (match (Point 2 3)
                             [(Point x y) (+ x y)]))
                ;; check that sub-patterns still work
                (check = 7 (match (make-point 2 3)
                             [(Point (app add1 x) (app add1 y)) (+ x y)]))
                ;; check that it works inside a list
                (check = 7 (match (list (make-point 2 3))
                             [(list (Point (app add1 x) (app add1 y))) (+ x y)]))
                ))
   
   ;; from richard's view documentation
   
   (test-case "Natural number views"
              (let ()
                ;; the view implementation from planet:                
                (define-match-expander view
                  (lambda (stx)
                    (syntax-case stx ()
                      [(_ pred? ([selector pattern] ...))
                       #'(? pred? (app selector pattern) ...)]))
                  (lambda (stx)
                    (syntax-case stx ()
                      [(_ pred? ([selector pattern] ...))
                       #'(? pred? (= selector pattern) ...)]))
                  (lambda (stx)
                    (raise-syntax-error #f "may only be used as match pattern" stx)))
                
                (define-syntax define-view
                  (lambda (stx)
                    (syntax-case stx ()
                      [(_ view-name pred? (selector ...))
                       (identifier? #'view-name)
                       (with-syntax ([(pattern-var ...)
                                      (generate-temporaries #'(selector ...))]
                                     [(pred-var) (generate-temporaries #'(pred?))]
                                     [(selector-var ...)
                                      (generate-temporaries #'(selector ...))])
                         #'(begin
                             (define pred-var pred?)
                             (define selector-var selector) ...
                             (define-match-expander view-name
                               (lambda (stx)
                                 (syntax-case stx ()
                                   [(_ pattern-var ...)
                                    #'(? pred-var (app selector-var pattern-var) ...)]))
                               (lambda (stx)
                                 (syntax-case stx ()
                                   [(_ pattern-var ...)
                                    #'(? pred-var (= selector-var pattern-var) ...)]))
                               (lambda (stx)
                                 (raise-syntax-error #f
                                                     "may only be used as match pattern"
                                                     stx)))))]
                      [(_ bad-name pred? (selector ...))
                       (raise-syntax-error #f "bad view name" stx #'bad-name)]
                      [_
                       (raise-syntax-error
                        #f
                        "bad view defn: expected (define-view view-name pred? (selector ...))"
                        stx)])))                
                
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
                (check = 120 (factorial 5))))
   
   ;; more complex example from Dale
   (test-case "Point structs with keywords"
              (let ()
                (define-struct point (x y))
                (define-match-expander Point
                  #:plt-match
                  (lambda (x)
                    (syntax-case x ()
                      ((Point a b) #'(struct point (a b)))))
                  #:expression make-point) 
                ;; check that it works as expression and as pattern
                (check = 5 (match (Point 2 3)
                             [(Point x y) (+ x y)]))
                ;; check that sub-patterns still work
                (check = 7 (match (make-point 2 3)
                             [(Point (app add1 x) (app add1 y)) (+ x y)]))
                ;; check that it works inside a list
                (check = 7 (match (list (make-point 2 3))
                             [(list (Point (app add1 x) (app add1 y))) (+ x y)]))
                ))
   ))

(define simple-tests 
  (test-suite
   "Some Simple Tests"
   (test-case "Trivial"
              (check = 3 (match 3 [x x])))
   (test-case "no order"
              (check equal? #t (match '(1 2 3 1) 
                                 [(list-no-order 3 2 1 1) #t]
                                 [_ #f])))
   (test-case "app pattern"
              (check = 4 (match 3 [(app add1 y) y])))
   (test-case "struct patterns"
              (let ()
                (define-struct point (x y))
                (define (origin? pt)
                  (match pt
                    ((struct point (0 0)) #t)
                    (else #f)))
                (check-true (origin? (make-point 0 0)))
                (check-false (origin? (make-point 1 1)))))
   ; These tests ensures that the unsafe struct optimization is correct
   (test-case "struct patterns (with opaque parent)"
              (let ()
                (define-struct opq (any))
                (parameterize ([current-inspector (make-sibling-inspector)])
                  (define-struct point (x y) #:super struct:opq)
                  (define (origin? pt)
                    (match pt
                      ((struct point (0 0)) #t)
                      (else #f)))
                  (check-true (origin? (make-point 'a 0 0)))
                  (check-false (origin? (make-point 'a 1 1))))))
   (test-case "struct patterns (with fake struct info)"
              (let ()
                (define (point? x)
                  (and (list? x) (= 2 (length x))))
                (define-syntax point
                  (list #f #f #'point? (list #'cadr #'car) (list #f #f) #t))
                (define (origin? pt)
                  (match pt
                    ((struct point (0 1)) #t)
                    (else #f)))
                (check-true (origin? (list 0 1)))
                (check-false (origin? (list 1 1)))
                (check-false (origin? (list 1 1 1)))
                (check-false (origin? (list 1)))
                (check-false (origin? 1))))
   ))

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
   (test-case "Nonlinear patterns use equal?"
              (check equal? '(3) (match '((3) (3)) [(list a a) a] [_ 5])))))


(define doc-tests
  (test-suite 
   "Tests from Help Desk Documentation"
   (test-case "match-let"
              (check = 6 (match-let ([(list x y z) (list 1 2 3)]) (+ x y z))))     
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

(define struct*-tests
  (test-suite 
   "Tests of struct*"
   (test-case "not an id for struct"
              (check-exn exn:fail:syntax?
                         (lambda ()
                           (expand #'(let ()
                                       (define-struct tree (val left right))
                                       (match (make-tree 0 1 2)
                                         [(struct* 4 ())
                                          #f]))))))
   (test-case "not a struct-info for struct"
              (check-exn exn:fail:syntax?
                         (lambda ()
                           (expand #'(let ()
                                       (define-syntax tree 1)
                                       (match 1
                                         [(struct* tree ())
                                          #f]))))))
   (test-case "bad form"
              (check-exn exn:fail:syntax?
                         (lambda ()
                           (expand #'(let ()
                                       (define-struct tree (val left right))
                                       (match (make-tree 0 1 2)
                                         [(struct* tree ([val]))
                                          #f]))))))
   (test-case "bad form"
              (check-exn exn:fail:syntax?
                         (lambda ()
                           (expand #'(let ()
                                       (define-struct tree (val left right))
                                       (match (make-tree 0 1 2)
                                         [(struct* tree (val))
                                          #f]))))))
   (test-case "field appears twice"
              (check-exn exn:fail:syntax?
                         (lambda ()
                           (expand #'(let ()
                                       (define-struct tree (val left right))
                                       (match (make-tree 0 1 2)
                                         [(struct* tree ([val 0] [val 0]))
                                          #f]))))))
   (test-case "not a field"
              (check-exn exn:fail:syntax?
                         (lambda ()
                           (expand #'(let ()
                                       (define-struct tree (val left right))
                                       (match (make-tree 0 1 2)
                                         [(struct* tree ([feet 0]))
                                          #f]))))))
   (test-case "super structs don't work"
              (check-exn exn:fail:syntax?
                         (lambda ()
                           (expand #'(let ()
                                       (define-struct extra (foo))
                                       (define-struct (tree extra) (val left right))
                                       (match (make-tree #f 0 1 2)
                                         [(struct* tree ([extra #f] [val 0]))
                                          #f]))))))
   (test-case "super struct kinda work"
              (let ()
                (define-struct extra (foo))
                (define-struct (tree extra) (val left right))
                (match (make-tree #f 0 1 2)
                  [(struct* tree ([val a]))
                   (check = 0 a)])))
   (test-case "from documentation"
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
                (check = 0 a)
                (check = 1 b)))))

(define plt-match-tests
  (test-suite "Tests for plt-match.rkt"
              doc-tests
              cons-tests
              simple-tests
              nonlinear-tests
              match-expander-tests
              reg-tests
              struct*-tests))

(define (run-all-tests)
  (run-tests (test-suite "Match Tests"
                            plt-match-tests
                            match-tests
                            new-tests
                            ;; from bruce
                            other-tests 
                            other-plt-tests)
             'verbose))

(unless (= 0 (run-all-tests))
  (error "Match Tests did not pass."))
