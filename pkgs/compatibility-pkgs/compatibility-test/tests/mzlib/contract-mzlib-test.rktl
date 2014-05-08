#|

This file started out as a copy of contract-test.rktl.
Its purpose is to try to ensure that the mzlib version
of the contract library does not change over time.

|#

(load-relative "loadtest.rktl")
(Section 'mzlib/contract)

(parameterize ([error-print-width 200])
(let ()
  
  (define contract-namespace 
    (let ([n ((dynamic-require 'mzscheme 'make-namespace))])
      (parameterize ([current-namespace n])
        (namespace-require 'mzlib/contract)
        (namespace-require 'mzlib/class)
        (namespace-require 'mzlib/etc)
        (namespace-require '(only mzscheme force delay)))
      n))
  
  (define (contract-eval x)
    (parameterize ([current-namespace contract-namespace])
      (eval x)))
  
  (define-syntax (ctest stx)
    (syntax-case stx ()
      [(_ a ...)
       (syntax (contract-eval `(,test a ...)))]))

  (define (contract-error-test exp exn-ok?)
    (test #t 
          'contract-error-test 
          (contract-eval `(with-handlers ((exn? (λ (x) (and (,exn-ok? x) #t)))) ,exp))))
  
  ;; test/spec-passed : symbol sexp -> void
  ;; tests a passing specification
  (define (test/spec-passed name expression)
    (printf "testing: ~s\n" name)
    (contract-eval
     `(,test 
        (void)
        (let ([for-each-eval (lambda (l) (for-each eval l))]) for-each-eval)
        (list ',expression '(void))))
    (let/ec k
      (contract-eval
       `(,test (void)
          (let ([for-each-eval (lambda (l) (for-each (λ (x) (eval x)) l))])
            for-each-eval)
          (list ',(rewrite expression k) '(void))))))
  
  (define (test/spec-passed/result name expression result)
    (printf "testing: ~s\n" name)
    (contract-eval `(,test ',result eval ',expression))
    (let/ec k
      (contract-eval
       `(,test
          ',result
          eval
          ',(rewrite expression k)))))
  
  ;; rewrites `contract' to use opt/c. If there is a module definition in there, we skip that test.
  (define (rewrite exp k)
    (let loop ([exp exp])
      (cond
        [(null? exp) null]
        [(list? exp)
         (case (car exp)
           [(contract) `(contract (opt/c ,(loop (cadr exp))) ,@(map loop (cddr exp)))]
           [(module) (k #f)]
           [else (map loop exp)])]
        [(pair? exp) (cons (loop (car exp))
                           (loop (cdr exp)))]
        [else exp])))
  
  (define (test/spec-failed name expression blame)
    (let ()
      (define (has-proper-blame? msg)
        (define reg
          (case blame
            [(pos) #rx"^self-contract violation"]
            [(neg) #rx"blaming neg"]
            [else (error 'test/spec-failed "unknown blame name ~s" blame)]))
        (regexp-match? reg msg))
      (printf "testing: ~s\n" name)
      (contract-eval
       `(,thunk-error-test 
          (lambda () ,expression)
          (datum->syntax-object #'here ',expression)
          (lambda (exn)
            (and (exn? exn)
                 (,has-proper-blame? (exn-message exn))))))
      (let/ec k
        (let ([rewritten (rewrite expression k)])
          (contract-eval
           `(,thunk-error-test 
             (lambda () ,rewritten)
             (datum->syntax-object #'here ',rewritten)
             (lambda (exn)
               (and (exn? exn)
                    (,has-proper-blame? (exn-message exn))))))))))
  
  (define (test/pos-blame name expression) (test/spec-failed name expression "pos"))
  (define (test/neg-blame name expression) (test/spec-failed name expression "neg"))
  
  (define (test/well-formed stx)
    (contract-eval
     `(,test (void) 
            (let ([expand/ret-void (lambda (x) (expand x) (void))]) expand/ret-void)
            ,stx)))
  
  (define (test/no-error sexp)
    (contract-eval
    `(,test (void)
       eval
       '(begin ,sexp (void)))))
  
  (define (test-flat-contract contract pass fail)
    (define (run-three-tests contract)
      (let ([name (if (pair? contract)
                      (car contract)
                      contract)])
        (contract-eval `(,test #t flat-contract? ,contract))
        (test/spec-failed (format "~a fail" name)
                          `(contract ,contract ',fail 'pos 'neg)
                          "pos")
        (test/spec-passed/result
         (format "~a pass" name)
         `(contract ,contract ',pass 'pos 'neg)
         pass)))
    (run-three-tests contract)
    (let/ec k (run-three-tests (rewrite contract k))))

  (define-syntax (test-name stx)
    (syntax-case stx ()
      [(_ name contract)
       #'(do-name-test 'name 'contract)]))
  
  (define (do-name-test name contract-exp)
    (printf "~s\n" (list 'do-name-test name contract-exp))
    (contract-eval `(,test ,name contract-name ,contract-exp))
    (contract-eval `(,test ,name contract-name (opt/c ,contract-exp))))
  
  (test/spec-passed
   'contract-flat1 
   '(contract not #f 'pos 'neg))
  
  (test/pos-blame
   'contract-flat2 
   '(contract not #t 'pos 'neg))
  
  (test/no-error '(-> integer? integer?))
  (test/no-error '(-> (flat-contract integer?) (flat-contract integer?)))
  (test/no-error '(-> integer? any))
  (test/no-error '(-> (flat-contract integer?) any))
  
  (test/no-error '(->* (integer?) (integer?)))
  (test/no-error '(->* (integer?) integer? (integer?)))
  (test/no-error '(->* (integer?) integer? any))
  (test/no-error '(->* ((flat-contract integer?)) ((flat-contract integer?))))
  (test/no-error '(->* ((flat-contract integer?)) (flat-contract integer?) ((flat-contract integer?))))
  (test/no-error '(->* ((flat-contract integer?)) (flat-contract integer?) any))
  
  (test/no-error '(->d integer? (lambda (x) integer?)))
  (test/no-error '(->d (flat-contract integer?) (lambda (x) (flat-contract integer?))))

  (test/no-error '(->d* (integer?) (lambda (x) integer?)))
  (test/no-error '(->d* ((flat-contract integer?)) (lambda (x) (flat-contract integer?))))
  (test/no-error '(->d* (integer?) integer? (lambda (x . y) integer?)))
  (test/no-error '(->d* ((flat-contract integer?)) (flat-contract integer?) (lambda (x . y) (flat-contract integer?))))
  
  (test/no-error '(opt-> (integer?) (integer?) integer?))
  (test/no-error '(opt-> ((flat-contract integer?)) ((flat-contract integer?)) (flat-contract integer?)))
  (test/no-error '(opt-> ((flat-contract integer?)) ((flat-contract integer?)) any))
  (test/no-error '(opt->* (integer?) (integer?) (integer?)))
  (test/no-error '(opt->* ((flat-contract integer?)) ((flat-contract integer?)) ((flat-contract integer?))))
  (test/no-error '(opt->* (integer?) (integer?) any))
  (test/no-error '(opt->* ((flat-contract integer?)) ((flat-contract integer?)) any))
  
  (test/no-error '(unconstrained-domain-> number?))
  (test/no-error '(unconstrained-domain-> (flat-contract number?)))
  
  (test/no-error '(listof any/c))
  (test/no-error '(listof (lambda (x) #t)))
  
  (test/spec-passed/result 'any/c '(contract any/c 1 'pos 'neg) 1)
  (test/pos-blame 'none/c '(contract none/c 1 'pos 'neg))
  
  (test/spec-passed
   'contract-arrow-star0a
   '(contract (->* (integer?) (integer?))
              (lambda (x) x)
              'pos
              'neg))
  
  (test/neg-blame
   'contract-arrow-star0b
   '((contract (->* (integer?) (integer?))
               (lambda (x) x)
               'pos
               'neg)
     #f))
  
  (test/pos-blame
   'contract-arrow-star0c
   '((contract (->* (integer?) (integer?))
               (lambda (x) #f)
               'pos
               'neg)
     1))
  
  (test/spec-passed
   'contract-arrow-star1
   '(let-values ([(a b) ((contract (->* (integer?) (integer? integer?))
                                   (lambda (x) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))
  
  (test/neg-blame
   'contract-arrow-star2
   '((contract (->* (integer?) (integer? integer?))
               (lambda (x) (values x x))
               'pos
               'neg)
     #f))
  
  (test/pos-blame
   'contract-arrow-star3
   '((contract (->* (integer?) (integer? integer?))
               (lambda (x) (values 1 #t))
               'pos
               'neg)
     1))
  
  (test/pos-blame
   'contract-arrow-star4
   '((contract (->* (integer?) (integer? integer?))
               (lambda (x) (values #t 1))
               'pos
               'neg)
     1))
  
  
  (test/spec-passed
   'contract-arrow-star5
   '(let-values ([(a b) ((contract (->* (integer?) 
                                        (listof integer?)
                                        (integer? integer?))
                                   (lambda (x . y) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))
  
  (test/neg-blame
   'contract-arrow-star6
   '((contract (->* (integer?) (listof integer?) (integer? integer?))
               (lambda (x . y) (values x x))
               'pos
               'neg)
     #f))
  
  (test/pos-blame
   'contract-arrow-star7
   '((contract (->* (integer?) (listof integer?) (integer? integer?))
               (lambda (x . y) (values 1 #t))
               'pos
               'neg)
     1))
  
  (test/pos-blame
   'contract-arrow-star8
   '((contract (->* (integer?) (listof integer?) (integer? integer?))
               (lambda (x) (values #t 1))
               'pos
               'neg)
     1))
  
  (test/spec-passed
   'contract-arrow-star9
   '((contract (->* (integer?) (listof integer?) (integer?))
               (lambda (x . y) 1)
               'pos
               'neg)
     1 2))
  
  (test/neg-blame
   'contract-arrow-star10
   '((contract (->* (integer?) (listof integer?) (integer?))
               (lambda (x . y) 1)
               'pos
               'neg)
     1 2 'bad))
  
  (test/spec-passed
   'contract-arrow-star11
   '(let-values ([(a b) ((contract (->* (integer?) 
                                        (listof integer?)
					any)
                                   (lambda (x . y) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))
  
  (test/pos-blame
   'contract-arrow-star11b
   '(let-values ([(a b) ((contract (->* (integer?) 
                                        (listof integer?)
					any)
                                   (lambda (x) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))
  
  (test/neg-blame
   'contract-arrow-star12
   '((contract (->* (integer?) (listof integer?) any)
               (lambda (x . y) (values x x))
               'pos
               'neg)
     #f))
  
  (test/spec-passed
   'contract-arrow-star13
   '((contract (->* (integer?) (listof integer?) any)
               (lambda (x . y) 1)
               'pos
               'neg)
     1 2))
  
  (test/neg-blame
   'contract-arrow-star14
   '((contract (->* (integer?) (listof integer?) any)
               (lambda (x . y) 1)
               'pos
               'neg)
     1 2 'bad))
  
  (test/spec-passed
   'contract-arrow-star15
   '(let-values ([(a b) ((contract (->* (integer?) any)
                                   (lambda (x) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))
  
  (test/spec-passed
   'contract-arrow-star16
   '((contract (->* (integer?) any)
               (lambda (x) x)
               'pos
               'neg)
     2))
  
  (test/neg-blame
   'contract-arrow-star17
   '((contract (->* (integer?) any)
               (lambda (x) (values x x))
               'pos
               'neg)
     #f))

  (test/pos-blame
   'contract-arrow-star-arity-check1
   '(contract (->* (integer?) (listof integer?) (integer? integer?))
              (lambda (x) (values 1 #t))
              'pos
              'neg))
  
  (test/pos-blame
   'contract-arrow-star-arity-check2
   '(contract (->* (integer?) (listof integer?) (integer? integer?))
              (lambda (x y) (values 1 #t))
              'pos
              'neg))
  
  (test/pos-blame
   'contract-arrow-star-arity-check3
   '(contract (->* (integer?) (listof integer?) (integer? integer?))
              (case-lambda [(x y) #f] [(x y . z) #t])
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-star-arity-check4
   '(contract (->* (integer?) (listof integer?) (integer? integer?))
              (case-lambda [(x y) #f] [(x y . z) #t] [(x) #f])
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-values1
   '(let-values ([(a b) ((contract (-> integer? (values integer? integer?))
                                   (lambda (x) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))
  
  (test/neg-blame
   'contract-arrow-values2
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) (values x x))
               'pos
               'neg)
     #f))
  
  (test/pos-blame
   'contract-arrow-values3
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) (values 1 #t))
               'pos
               'neg)
     1))
  
  (test/pos-blame
   'contract-arrow-values4
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) (values #t 1))
               'pos
               'neg)
     1))


  (test/pos-blame
   'contract-d1
   '(contract (integer? . ->d . (lambda (x) (lambda (y) (= x y))))
              1
              'pos
              'neg))
  
  (test/spec-passed
   'contract-d2
   '(contract (integer? . ->d . (lambda (x) (lambda (y) (= x y))))
              (lambda (x) x)
              'pos
              'neg))
  
  (test/pos-blame
   'contract-d2
   '((contract (integer? . ->d . (lambda (x) (lambda (y) (= x y))))
               (lambda (x) (+ x 1))
               'pos
               'neg)
     2))

  (test/neg-blame
   'contract-d3
   '((contract (integer? . ->d . (lambda (x)  (let ([z (+ x 1)]) (lambda (y) (= z y)))))
               (lambda (x) (+ x 1))
               'pos
               'neg)
     "bad input"))
  
  (test/neg-blame
   'contract-d4
   '((contract (integer? . ->d . (lambda (x)  (lambda (y) (= (+ x 1) y))))
               (lambda (x) (+ x 1))
               'pos
               'neg)
     "bad input"))
  
  (test/spec-passed
   'contract-arrow1
   '(contract (integer? . -> . integer?) (lambda (x) x) 'pos 'neg))
  
  ;; make sure we skip the optimizations
  (test/spec-passed
   'contract-arrow1b
   '(contract (integer? integer? integer? integer? integer? integer? integer? integer? integer? integer? . -> . integer?) 
              (lambda (x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) x1) 'pos 'neg))
  
  (test/pos-blame
   'contract-arrow2
   '(contract (integer? . -> . integer?) (lambda (x y) x) 'pos 'neg))
  
  (test/neg-blame
   'contract-arrow3
   '((contract (integer? . -> . integer?) (lambda (x) #f) 'pos 'neg) #t))
  
  (test/pos-blame
   'contract-arrow4
   '((contract (integer? . -> . integer?) (lambda (x) #f) 'pos 'neg) 1))


  (test/spec-passed
   'contract-arrow-any1
   '(contract (integer? . -> . any) (lambda (x) x) 'pos 'neg))
  
  (test/pos-blame
   'contract-arrow-any2
   '(contract (integer? . -> . any) (lambda (x y) x) 'pos 'neg))
  
  (test/neg-blame
   'contract-arrow-any3
   '((contract (integer? . -> . any) (lambda (x) #f) 'pos 'neg) #t))

  (test/spec-passed
   'contract-arrow-star-d1
   '((contract (->d* (integer?) (lambda (arg) (lambda (res) (= arg res))))
               (lambda (x) x)
               'pos
               'neg)
     1))
  
  (test/spec-passed
   'contract-arrow-star-d2
   '(let-values ([(a b)
                  ((contract (->d* (integer?) (lambda (arg) 
                                                (values (lambda (res) (= arg res)) 
                                                        (lambda (res) (= arg res)))))
                             (lambda (x) (values x x))
                             'pos
                             'neg)
                   1)])
      1))
  
  (test/pos-blame
   'contract-arrow-star-d3
   '((contract (->d* (integer?) (lambda (arg) 
                                  (values (lambda (res) (= arg res)) 
                                          (lambda (res) (= arg res)))))
               (lambda (x) (values 1 2))
               'pos
               'neg)
     2))
  
  (test/pos-blame
   'contract-arrow-star-d4
   '((contract (->d* (integer?) (lambda (arg) 
                                  (values (lambda (res) (= arg res)) 
                                          (lambda (res) (= arg res)))))
               (lambda (x) (values 2 1))
               'pos
               'neg)
     2))
  
  (test/spec-passed
   'contract-arrow-star-d5
   '((contract (->d* ()
                     (listof integer?)
                     (lambda args (lambda (res) (= (car args) res))))
               (lambda x (car x))
               'pos
               'neg)
     1))
  
  (test/spec-passed
   'contract-arrow-star-d6
   '((contract (->d* () 
                     (listof integer?)
                     (lambda args
                       (values (lambda (res) (= (car args) res)) 
                               (lambda (res) (= (car args) res)))))
               (lambda x (values (car x) (car x)))
               'pos
               'neg)
     1))
  
  (test/pos-blame
   'contract-arrow-star-d7
   '((contract (->d* () 
                     (listof integer?)
                     (lambda args
                       (values (lambda (res) (= (car args) res)) 
                               (lambda (res) (= (car args) res)))))
               (lambda x (values 1 2))
               'pos
               'neg)
     2))
  
  (test/pos-blame
   'contract-arrow-star-d8
   '((contract (->d* ()
                     (listof integer?)
                     (lambda args
                       (values (lambda (res) (= (car args) res)) 
                               (lambda (res) (= (car args) res)))))
               (lambda x (values 2 1))
               'pos
               'neg)
     2))
  
  (test/pos-blame
   'contract-arrow-star-d8
   '(contract (->d* ()
                    (listof integer?)
                    (lambda arg
                      (values (lambda (res) (= (car arg) res)) 
                              (lambda (res) (= (car arg) res)))))
              (lambda (x) (values 2 1))
              'pos
              'neg))
  
  (test/spec-passed
   'and/c1
   '((contract (and/c (-> (<=/c 100) (<=/c 100))
                      (-> (>=/c -100) (>=/c -100)))
               (λ (x) x)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   'and/c2
   '((contract (and/c (-> (<=/c 100) (<=/c 100))
                      (-> (>=/c -100) (>=/c -100)))
               (λ (x) x)
               'pos
               'neg)
     200))
  
  (test/pos-blame
   'and/c3
   '((contract (and/c (-> (<=/c 100) (<=/c 100))
                      (-> (>=/c -100) (>=/c -100)))
               (λ (x) 200)
               'pos
               'neg)
     1))
  
  (test/spec-passed
   '->r1
   '((contract (->r () number?) (lambda () 1) 'pos 'neg)))
  
  (test/spec-passed
   '->r2
   '((contract (->r ([x number?]) number?) (lambda (x) (+ x 1)) 'pos 'neg) 1))

  (test/pos-blame
   '->r3
   '((contract (->r () number?) 1 'pos 'neg)))
  
  (test/pos-blame
   '->r4
   '((contract (->r () number?) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->r5
   '((contract (->r ([x number?]) any) (lambda (x) (+ x 1)) 'pos 'neg) #f))
  
  (test/pos-blame
   '->r6
   '((contract (->r ([x number?]) (<=/c x)) (lambda (x) (+ x 1)) 'pos 'neg) 1))
  
  (test/spec-passed
   '->r7
   '((contract (->r ([x number?] [y (<=/c x)]) (<=/c x)) (lambda (x y) (- x 1)) 'pos 'neg) 1 0))
  
  (test/neg-blame
   '->r8
   '((contract (->r ([x number?] [y (<=/c x)]) (<=/c x)) (lambda (x y) (+ x 1)) 'pos 'neg) 1 2))
  
  (test/spec-passed
   '->r9
   '((contract (->r ([y (<=/c x)] [x number?]) (<=/c x)) (lambda (y x) (- x 1)) 'pos 'neg) 1 2))
  
  (test/neg-blame
   '->r10
   '((contract (->r ([y (<=/c x)] [x number?]) (<=/c x)) (lambda (y x) (+ x 1)) 'pos 'neg) 1 0))
  
  (test/spec-passed
   '->r11
   '((contract (->r () rest any/c number?) (lambda x 1) 'pos 'neg)))
  
  (test/spec-passed
   '->r12
   '((contract (->r ([x number?]) rest any/c number?) (lambda (x . y) (+ x 1)) 'pos 'neg) 1))

  (test/pos-blame
   '->r13
   '((contract (->r () rest any/c number?) 1 'pos 'neg)))
  
  (test/pos-blame
   '->r14
   '((contract (->r () rest any/c number?) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->r15
   '((contract (->r ([x number?]) rest any/c any) (lambda (x . y) (+ x 1)) 'pos 'neg) #f))
  
  (test/pos-blame
   '->r16
   '((contract (->r ([x number?]) rest any/c (<=/c x)) (lambda (x . y) (+ x 1)) 'pos 'neg) 1))
  
  (test/spec-passed
   '->r17
   '((contract (->r ([x number?] [y (<=/c x)]) rest any/c (<=/c x)) (lambda (x y . z) (- x 1)) 'pos 'neg) 1 0))
  
  (test/neg-blame
   '->r18
   '((contract (->r ([x number?] [y (<=/c x)]) rest any/c (<=/c x)) (lambda (x y . z) (+ x 1)) 'pos 'neg) 1 2))
  
  (test/spec-passed
   '->r19
   '((contract (->r ([y (<=/c x)] [x number?]) rest any/c (<=/c x)) (lambda (y x . z) (- x 1)) 'pos 'neg) 1 2))
  
  (test/neg-blame
   '->r20
   '((contract (->r ([y (<=/c x)] [x number?]) rest any/c (<=/c x)) (lambda (y x . z) (+ x 1)) 'pos 'neg) 1 0))

  (test/spec-passed
   '->r21
   '((contract (->r () rst (listof number?) any/c) (lambda w 1) 'pos 'neg) 1))
  
  (test/neg-blame
   '->r22
   '((contract (->r () rst (listof number?) any/c) (lambda w 1) 'pos 'neg) #f))
  
  (test/spec-passed
   '->r-any1
   '((contract (->r () any) (lambda () 1) 'pos 'neg)))
  
  (test/spec-passed
   '->r-any2
   '((contract (->r ([x number?]) any) (lambda (x) (+ x 1)) 'pos 'neg) 1))

  (test/pos-blame
   '->r-any3
   '((contract (->r () any) 1 'pos 'neg)))
  
  (test/pos-blame
   '->r-any4
   '((contract (->r () any) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->r-any5
   '((contract (->r ([x number?]) any) (lambda (x) (+ x 1)) 'pos 'neg) #f))
  
  (test/spec-passed
   '->r-any6
   '((contract (->r ([x number?] [y (<=/c x)]) any) (lambda (x y) (- x 1)) 'pos 'neg) 1 0))
  
  (test/neg-blame
   '->r-any7
   '((contract (->r ([x number?] [y (<=/c x)]) any) (lambda (x y) (+ x 1)) 'pos 'neg) 1 2))
  
  (test/spec-passed
   '->r-any8
   '((contract (->r ([y (<=/c x)] [x number?]) any) (lambda (y x) (- x 1)) 'pos 'neg) 1 2))
  
  (test/neg-blame
   '->r-any9
   '((contract (->r ([y (<=/c x)] [x number?]) any) (lambda (y x) (+ x 1)) 'pos 'neg) 1 0))
  
  (test/spec-passed
   '->r-any10
   '((contract (->r () rest any/c any) (lambda x 1) 'pos 'neg)))
  
  (test/spec-passed
   '->r-any11
   '((contract (->r ([x number?]) rest any/c any) (lambda (x . y) (+ x 1)) 'pos 'neg) 1))

  (test/pos-blame
   '->r-any12
   '((contract (->r () rest any/c any) 1 'pos 'neg)))
  
  (test/pos-blame
   '->r-any13
   '((contract (->r () rest any/c any) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->r-any14
   '((contract (->r ([x number?]) rest any/c any) (lambda (x . y) (+ x 1)) 'pos 'neg) #f))
  
  (test/spec-passed
   '->r-any15
   '((contract (->r ([x number?] [y (<=/c x)]) rest any/c any) (lambda (x y . z) (- x 1)) 'pos 'neg) 1 0))
  
  (test/neg-blame
   '->r-any16
   '((contract (->r ([x number?] [y (<=/c x)]) rest any/c any) (lambda (x y . z) (+ x 1)) 'pos 'neg) 1 2))
  
  (test/spec-passed
   '->r-any17
   '((contract (->r ([y (<=/c x)] [x number?]) rest any/c any) (lambda (y x . z) (- x 1)) 'pos 'neg) 1 2))
  
  (test/neg-blame
   '->r-any18
   '((contract (->r ([y (<=/c x)] [x number?]) rest any/c any) (lambda (y x . z) (+ x 1)) 'pos 'neg) 1 0))

  (test/spec-passed
   '->r-any19
   '((contract (->r () rst (listof number?) any) (lambda w 1) 'pos 'neg) 1))
  
  (test/neg-blame
   '->r-any20
   '((contract (->r () rst (listof number?) any) (lambda w 1) 'pos 'neg) #f))
  
  (test/spec-passed
   '->r-values1
   '((contract (->r () (values [x boolean?] [y number?])) (lambda () (values #t 1)) 'pos 'neg)))
  
  (test/spec-passed
   '->r-values2
   '((contract (->r ([x number?]) (values [x boolean?] [y number?])) (lambda (x) (values #t (+ x 1))) 'pos 'neg) 1))

  (test/pos-blame
   '->r-values3
   '((contract (->r () (values [x boolean?] [y number?])) 1 'pos 'neg)))
  
  (test/pos-blame
   '->r-values4
   '((contract (->r () (values [x boolean?] [y number?])) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->r-values5
   '((contract (->r ([x number?]) (values [y boolean?] [z (<=/c x)])) (lambda (x) (+ x 1)) 'pos 'neg) #f))
  
  (test/pos-blame
   '->r-values6
   '((contract (->r ([x number?]) (values [y boolean?] [z (<=/c x)])) (lambda (x) (values #t (+ x 1))) 'pos 'neg) 1))
  
  (test/spec-passed
   '->r-values7
   '((contract (->r ([x number?] [y (<=/c x)]) (values [z boolean?] [w (<=/c x)])) 
               (lambda (x y) (values #t (- x 1)))
               'pos
               'neg) 
     1
     0))
  
  (test/neg-blame
   '->r-values8
   '((contract (->r ([x number?] [y (<=/c x)]) (values [z boolean?] [w (<=/c x)])) 
               (lambda (x y) (values #f (+ x 1)))
               'pos
               'neg)
     1
     2))
  
  (test/spec-passed
   '->r-values9
   '((contract (->r ([y (<=/c x)] [x number?]) (values [z boolean?] [w (<=/c x)]))
               (lambda (y x) (values #f (- x 1)))
               'pos 
               'neg)
     1
     2))
  
  (test/neg-blame
   '->r-values10
   '((contract (->r ([y (<=/c x)] [x number?]) (values [z boolean?] [w (<=/c x)]))
               (lambda (y x) (values #f (+ x 1))) 'pos 'neg)
     1 0))
  
  (test/spec-passed
   '->r-values11
   '((contract (->r () rest any/c (values [z boolean?] [w number?])) (lambda x (values #f 1)) 'pos 'neg)))
  
  (test/spec-passed
   '->r-values12
   '((contract (->r ([x number?]) rest any/c (values [z boolean?] [w number?]))
               (lambda (x . y) (values #f (+ x 1)))
               'pos 
               'neg)
     1))

  (test/pos-blame
   '->r-values13
   '((contract (->r () rest any/c (values [z boolean?] [w number?])) 1 'pos 'neg)))
  
  (test/pos-blame
   '->r-values14
   '((contract (->r () rest any/c (values [z boolean?] [w number?])) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->r-values15
   '((contract (->r ([x number?]) rest any/c  (values [z boolean?] [w (<=/c x)]))
               (lambda (x . y) (+ x 1)) 'pos 'neg)
     #f))
  
  (test/pos-blame
   '->r-values16
   '((contract (->r ([x number?]) rest any/c (values [z boolean?] [w (<=/c x)]))
               (lambda (x . y) (values #f (+ x 1))) 'pos 'neg) 
     1))
  
  (test/spec-passed
   '->r-values17
   '((contract (->r ([x number?] [y (<=/c x)]) rest any/c (values [z boolean?] [w (<=/c x)]))
               (lambda (x y . z) (values #f (- x 1))) 'pos 'neg) 
     1 0))
  
  (test/neg-blame
   '->r-values18
   '((contract (->r ([x number?] [y (<=/c x)]) rest any/c (values [z boolean?] [w (<=/c x)]))
               (lambda (x y . z) (values #f (+ x 1))) 'pos 'neg) 
     1 2))
  
  (test/spec-passed
   '->r-values19
   '((contract (->r ([y (<=/c x)] [x number?]) rest any/c  (values [z boolean?] [w (<=/c x)]))
               (lambda (y x . z) (values #f (- x 1))) 'pos 'neg)
     1 2))
  
  (test/neg-blame
   '->r-values20
   '((contract (->r ([y (<=/c x)] [x number?]) rest any/c  (values [z boolean?] [w (<=/c x)]))
               (lambda (y x . z) (values #f (+ x 1))) 'pos 'neg) 
     1 0))

  (test/spec-passed
   '->r-values21
   '((contract (->r () rst (listof number?) (values [z boolean?] [w any/c])) (lambda w (values #f 1)) 'pos 'neg) 1))
  
  (test/neg-blame
   '->r-values22
   '((contract (->r () rst (listof number?) (values [z boolean?] [w any/c])) (lambda w (values #f 1)) 'pos 'neg) #f))

  (test/spec-passed
   '->r-values23
   '((contract (->r () (values [x number?] [y (>=/c x)])) (lambda () (values 1 2)) 'pos 'neg)))
  
  (test/pos-blame
   '->r-values24
   '((contract (->r () (values [x number?] [y (>=/c x)])) (lambda () (values 2 1)) 'pos 'neg)))

  (test/spec-passed
   '->r-values25
   '((contract (->r ([x number?]) (values [z number?] [y (>=/c x)])) (lambda (x) (values 1 2)) 'pos 'neg) 1))
  
  (test/pos-blame
   '->r-values26
   '((contract (->r ([x number?]) (values [z number?] [y (>=/c x)])) (lambda (x) (values 2 1)) 'pos 'neg) 4))


    
  (test/spec-passed
   '->r1
   '((contract (->r () number?) (lambda () 1) 'pos 'neg)))
  
  (test/spec-passed
   '->r2
   '((contract (->r ([x number?]) number?) (lambda (x) (+ x 1)) 'pos 'neg) 1))

  (test/pos-blame
   '->r3
   '((contract (->r () number?) 1 'pos 'neg)))
  
  (test/pos-blame
   '->r4
   '((contract (->r () number?) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->r5
   '((contract (->r ([x number?]) any) (lambda (x) (+ x 1)) 'pos 'neg) #f))
  
  (test/pos-blame
   '->r6
   '((contract (->r ([x number?]) (<=/c x)) (lambda (x) (+ x 1)) 'pos 'neg) 1))
  
  (test/spec-passed
   '->r7
   '((contract (->r ([x number?] [y (<=/c x)]) (<=/c x)) (lambda (x y) (- x 1)) 'pos 'neg) 1 0))
  
  (test/neg-blame
   '->r8
   '((contract (->r ([x number?] [y (<=/c x)]) (<=/c x)) (lambda (x y) (+ x 1)) 'pos 'neg) 1 2))
  
  (test/spec-passed
   '->r9
   '((contract (->r ([y (<=/c x)] [x number?]) (<=/c x)) (lambda (y x) (- x 1)) 'pos 'neg) 1 2))
  
  (test/neg-blame
   '->r10
   '((contract (->r ([y (<=/c x)] [x number?]) (<=/c x)) (lambda (y x) (+ x 1)) 'pos 'neg) 1 0))
  
  (test/spec-passed
   '->r11
   '((contract (->r () rest any/c number?) (lambda x 1) 'pos 'neg)))
  
  (test/spec-passed
   '->r12
   '((contract (->r ([x number?]) rest any/c number?) (lambda (x . y) (+ x 1)) 'pos 'neg) 1))

  (test/pos-blame
   '->r13
   '((contract (->r () rest any/c number?) 1 'pos 'neg)))
  
  (test/pos-blame
   '->r14
   '((contract (->r () rest any/c number?) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->r15
   '((contract (->r ([x number?]) rest any/c any) (lambda (x . y) (+ x 1)) 'pos 'neg) #f))
  
  (test/pos-blame
   '->r16
   '((contract (->r ([x number?]) rest any/c (<=/c x)) (lambda (x . y) (+ x 1)) 'pos 'neg) 1))
  
  (test/spec-passed
   '->r17
   '((contract (->r ([x number?] [y (<=/c x)]) rest any/c (<=/c x)) (lambda (x y . z) (- x 1)) 'pos 'neg) 1 0))
  
  (test/neg-blame
   '->r18
   '((contract (->r ([x number?] [y (<=/c x)]) rest any/c (<=/c x)) (lambda (x y . z) (+ x 1)) 'pos 'neg) 1 2))
  
  (test/spec-passed
   '->r19
   '((contract (->r ([y (<=/c x)] [x number?]) rest any/c (<=/c x)) (lambda (y x . z) (- x 1)) 'pos 'neg) 1 2))
  
  (test/neg-blame
   '->r20
   '((contract (->r ([y (<=/c x)] [x number?]) rest any/c (<=/c x)) (lambda (y x . z) (+ x 1)) 'pos 'neg) 1 0))

  (test/spec-passed
   '->r21
   '((contract (->r () rst (listof number?) any/c) (lambda w 1) 'pos 'neg) 1))
  
  (test/neg-blame
   '->r22
   '((contract (->r () rst (listof number?) any/c) (lambda w 1) 'pos 'neg) #f))

  
  (test/spec-passed/result
   '->r23
   '((contract (->r ((i number?) (j (and/c number? (>=/c i)))) number?)
               (λ (i j) 1)
               'pos
               'neg)
     1
     2)
   1)

  (test/spec-passed/result
   '->r24
   '((contract (->r ((i number?) (j (and/c number? (>=/c i)))) any)
               (λ (i j) 1)
               'pos
               'neg)
     1
     2)
   1)

  (test/spec-passed/result
   '->r25
   '(call-with-values
    (λ ()
      ((contract (->r ((i number?) (j (and/c number? (>=/c i)))) (values [x number?] [y number?]))
                 (λ (i j) (values 1 2))
                 'pos
                 'neg)
       1
       2))
    list)
   '(1 2))

  (test/spec-passed/result
   '->r26
   '((contract (->r ((i number?) (j (and/c number? (>=/c i)))) rest-args any/c number?)
               (λ (i j . z) 1)
               'pos
               'neg)
     1
     2)
   1)

  (test/spec-passed/result
   '->r27
   '((contract (->r ((i number?) (j (and/c number? (>=/c i)))) rest-args any/c any)
               (λ (i j . z) 1)
               'pos
               'neg)
     1
     2)
   1)

(test/spec-passed/result
 '->r28
 '(call-with-values
   (λ ()
     ((contract (->r ((i number?) (j (and/c number? (>=/c i)))) rest-args any/c (values [x number?] [y number?]))
                (λ (i j . z) (values 1 2))
                'pos
                'neg)
      1
      2))
   list)
 '(1 2))

  
  (test/pos-blame
   '->pp1
   '((contract (->pp ([x number?]) (= x 1) number? result (= x 2))
               (λ (x) x)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   '->pp2
   '((contract (->pp ([x number?]) (= x 1) number? result (= x 2))
               (λ (x) x)
               'pos
               'neg)
     2))
  
  (test/pos-blame
   '->pp3
   '((contract (->pp ([x number?]) (= x 1) number? result (= result 2))
               (λ (x) x)
               'pos
               'neg)
     1))
  
  (test/spec-passed
   '->pp3.5
   '((contract (->pp ([x number?]) (= x 1) number? result (= result 2))
               (λ (x) 2)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   '->pp4
   '((contract (->pp ([x number?]) (= x 1) any)
               (λ (x) x)
               'pos
               'neg)
     2))
  
  (test/neg-blame
   '->pp5
   '((contract (->pp ([x number?]) (= x 1) (values [x number?] [y number?]) (= x y 3))
               (λ (x) (values 4 5))
               'pos
               'neg)
     2))
  
  (test/pos-blame
   '->pp6
   '((contract (->pp ([x number?]) (= x 1) (values [x number?] [y number?]) (= x y 3))
               (λ (x) (values 4 5))
               'pos
               'neg)
     1))

  (test/pos-blame
   '->pp-r1
   '((contract (->pp-rest ([x number?]) rst any/c (= x 1) number? result (= x 2))
               (λ (x . rst) x)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   '->pp-r2
   '((contract (->pp-rest ([x number?]) rst any/c (= x 1)  number? result (= x 2))
               (λ (x . rst) x)
               'pos
               'neg)
     2))
  
  (test/pos-blame
   '->pp-r3
   '((contract (->pp-rest ([x number?]) rst any/c (= x 1) number? result (= result 2))
               (λ (x . rst) x)
               'pos
               'neg)
     1))
  
  (test/spec-passed
   '->pp-r3.5
   '((contract (->pp-rest ([x number?]) rst any/c (= x 1) number? result (= result 2))
               (λ (x . rst) 2)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   '->pp-r4
   '((contract (->pp-rest ([x number?]) rst any/c (= x 1) any)
               (λ (x . rst) x)
               'pos
               'neg)
     2))
  
  (test/neg-blame
   '->pp-r5
   '((contract (->pp-rest ([x number?]) rst any/c (= x 1) (values [x number?] [y number?]) (= x y 3))
               (λ (x . rst) (values 4 5))
               'pos
               'neg)
     2))
  
  (test/pos-blame
   '->pp-r6
   '((contract (->pp-rest ([x number?]) rst any/c (= x 1) (values [x number?] [y number?]) (= x y 3))
               (λ (x . rst) (values 4 5))
               'pos
               'neg)
     1))
  
  (test/pos-blame
   'contract-case->0a
   '(contract (case->)
              (lambda (x) x)
              'pos
              'neg))
  
  (test/pos-blame
   'contract-case->0b
   '(contract (case->)
              (lambda () 1)
              'pos
              'neg))
  
  (test/pos-blame
   'contract-case->0c
   '(contract (case->)
              1
              'pos
              'neg))
  
  (test/spec-passed
   'contract-case->0d
   '(contract (case->)
              (case-lambda)
              'pos
              'neg))
  
  (test/pos-blame
   'contract-case->1
   '(contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
              (lambda (x) x)
              'pos
              'neg))
  
  (test/pos-blame
   'contract-case->2
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda 
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     1 2))
  
  (test/pos-blame
   'contract-case->3
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda 
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     1))
  
  (test/neg-blame
   'contract-case->4
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda 
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     'a 2))
  
  (test/neg-blame
   'contract-case->5
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda 
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     2 'a))
  
  (test/neg-blame
   'contract-case->6
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda 
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     #t))
  
  (test/pos-blame
   'contract-case->7
   '((contract (case-> (integer? integer? . -> . integer?) (->* (integer?) any/c (boolean?)))
               (lambda x #\a)
               'pos
               'neg)
     1 2))
  
  (test/pos-blame
   'contract-case->8
   '((contract (case-> (integer? integer? . -> . integer?) (->* (integer?) any/c (boolean?)))
               (lambda x #t)
               'pos
               'neg)
     1 2))
 
  (test/spec-passed
   'contract-case->8
   '((contract (case-> (integer? integer? . -> . integer?) (->* (integer?) any/c (boolean?)))
               (lambda x 1)
               'pos
               'neg)
     1 2))
  
  (test/spec-passed
   'contract-case->9
   '((contract (case-> (->r ([x number?]) (<=/c x)))
               (lambda (x) (- x 1))
               'pos
               'neg)
     1))
  
  (test/spec-passed
   'contract-case->9b
   '((contract (case-> (->r ([x number?]) (<=/c x)) (-> integer? integer? integer?))
               (case-lambda 
                 [(x) (- x 1)]
                 [(x y) x])
               'pos
               'neg)
     1))
  
  (test/pos-blame 
   'contract-case->10
   '((contract (case-> (->r ([x number?]) (<=/c x)))
               (lambda (x) (+ x 1))
               'pos
               'neg)
     1))
  
  (test/pos-blame 
   'contract-case->10b
   '((contract (case-> (->r ([x number?]) (<=/c x)) (-> number? number? number?))
               (case-lambda
                 [(x) (+ x 1)]
                 [(x y) x])
               'pos
               'neg)
     1))
  
  (test/spec-passed/result
   'contract-case->11
   '(let ([f 
           (contract (case-> (-> char?) (-> integer? boolean?) (-> symbol? input-port? string?))
                     (case-lambda
                       [() #\a]
                       [(x) (= x 0)]
                       [(sym port) 
                        (string-append
                         (symbol->string sym)
                         (read port))])
                     'pos
                     'neg)])
      (list (f)
            (f 1)
            (f 'x (open-input-string (format "~s" "string")))))
   (list #\a #f "xstring"))
 
  (test/neg-blame
   'contract-d-protect-shared-state
   '(let ([x 1])
      ((contract ((->d (lambda () (let ([pre-x x]) (lambda (res) (= x pre-x)))))
                  . -> .
                  (lambda (x) #t))
                 (lambda (thnk) (thnk))
                 'pos
                 'neg)
       (lambda () (set! x 2)))))
  
  #;
  (test/neg-blame
   'combo1
   '(let ([cf (contract (case->
                         ((class? . ->d . (lambda (%) (lambda (x) #f))) . -> . void?)
                         ((class? . ->d . (lambda (%) (lambda (x) #f))) boolean? . -> . void?))
                        (letrec ([c% (class object% (super-instantiate ()))]
                                 [f
                                  (case-lambda
                                    [(class-maker) (f class-maker #t)]
                                    [(class-maker b) 
                                     (class-maker c%)
                                     (void)])])
                          f)
                        'pos
                        'neg)])
      (cf (lambda (x%) 'going-to-be-bad))))   

  (test/spec-passed
   'unconstrained-domain->1
   '(contract (unconstrained-domain-> number?) (λ (x) x) 'pos 'neg))
  (test/pos-blame
   'unconstrained-domain->2
   '(contract (unconstrained-domain-> number?) 1 'pos 'neg))
  (test/spec-passed
   'unconstrained-domain->3
   '((contract (unconstrained-domain-> number?) (λ (x) x) 'pos 'neg) 1))
  (test/pos-blame
   'unconstrained-domain->4
   '((contract (unconstrained-domain-> number?) (λ (x) x) 'pos 'neg) #f))
  
  (test/spec-passed/result
   'unconstrained-domain->4
   '((contract (->r ([size natural-number/c]
                     [proc (and/c (unconstrained-domain-> number?)
                                  (λ (p) (procedure-arity-includes? p size)))])
                    number?)
               (λ (i f) (apply f (build-list i add1)))
               'pos
               'neg)
     10 +)
   55)
  
  (test/pos-blame
   'or/c1
   '(contract (or/c false/c) #t 'pos 'neg))

  (test/spec-passed
   'or/c2
   '(contract (or/c false/c) #f 'pos 'neg))

  (test/spec-passed
   'or/c3
   '((contract (or/c (-> integer? integer?)) (lambda (x) x) 'pos 'neg) 1))
  
  (test/neg-blame
   'or/c4
   '((contract (or/c (-> integer? integer?)) (lambda (x) x) 'pos 'neg) #f))
  
  (test/pos-blame
   'or/c5
   '((contract (or/c (-> integer? integer?)) (lambda (x) #f) 'pos 'neg) 1))
  
  (test/spec-passed
   'or/c6
   '(contract (or/c false/c (-> integer? integer?)) #f 'pos 'neg))
  
  (test/spec-passed
   'or/c7
   '((contract (or/c false/c (-> integer? integer?)) (lambda (x) x) 'pos 'neg) 1))
  
  (test/spec-passed/result
   'or/c8
   '((contract ((or/c false/c (-> string?))  . -> . any)
               (λ (y) y)
               'pos
               'neg)
     #f)
   #f)

  (test/spec-passed/result
   'or/c9
   '((contract (or/c (-> string?) (-> integer? integer?))
               (λ () "x")
               'pos
               'neg))
   "x")
  
  (test/spec-passed/result
   'or/c10
   '((contract (or/c (-> string?) (-> integer? integer?))
               (λ (x) x)
               'pos
               'neg) 
     1)
   1)
  
  (test/pos-blame
   'or/c11
   '(contract (or/c (-> string?) (-> integer? integer?))
              1
              'pos
              'neg))
  
  (test/pos-blame
   'or/c12
   '((contract (or/c (-> string?) (-> integer? integer?))
               1
               'pos
               'neg)
     'x))
  
  (test/pos-blame
   'or/c13
   '(contract (or/c not) #t 'pos 'neg))
  
  (test/spec-passed
   'or/c14
   '(contract (or/c not) #f 'pos 'neg))
  
  (test/spec-passed/result
   'or/c-not-error-early 
   '(begin (or/c (-> integer? integer?) (-> boolean? boolean?))
           1)
   1)
  
  (contract-error-test
   #'(contract (or/c (-> integer? integer?) (-> boolean? boolean?))
               (λ (x) x)
               'pos
               'neg)
   exn:fail?)
  
  (test/spec-passed/result
   'or/c-ordering
   '(let ([x '()])
      (contract (or/c (lambda (y) (set! x (cons 2 x)) #f) (lambda (y) (set! x (cons 1 x)) #t))
                'anything
                'pos
                'neg)
      x)
   '(1 2))
  
  (test/spec-passed/result
   'or/c-ordering2
   '(let ([x '()])
      (contract (or/c (lambda (y) (set! x (cons 2 x)) #t) (lambda (y) (set! x (cons 1 x)) #t))
                'anything
                'pos
                'neg)
      x)
   '(2))
  
  (test/spec-passed/result
   'and/c-ordering
   '(let ([x '()])
      (contract (and/c (lambda (y) (set! x (cons 2 x)) #t) (lambda (y) (set! x (cons 1 x)) #t))
                'anything
                'pos
                'neg)
      x)
   '(1 2))
  
  (test/spec-passed/result
   'ho-and/c-ordering
   '(let ([x '()])
      ((contract (and/c (-> (lambda (y) (set! x (cons 1 x)) #t)
                            (lambda (y) (set! x (cons 2 x)) #t))
                        (-> (lambda (y) (set! x (cons 3 x)) #t)
                            (lambda (y) (set! x (cons 4 x)) #t)))
                 (λ (x) x)
                 'pos
                 'neg)
       1)
      (reverse x))
   '(3 1 2 4))

  (test/neg-blame
   'parameter/c1
   '((contract (parameter/c integer?)
               (make-parameter 1)
               'pos 'neg)
     #f))
  
  (test/pos-blame
   'parameter/c1
   '((contract (parameter/c integer?)
               (make-parameter 'not-an-int)
               'pos 'neg)))
  
  (test/spec-passed
   'define/contract1
   '(let ()
      (define/contract i integer? 1)
      i))
  
  (test/spec-failed
   'define/contract2
   '(let ()
      (define/contract i integer? #t)
      i)
   "i")
  
  (test/spec-failed
   'define/contract3
   '(let ()
      (define/contract i (-> integer? integer?) (lambda (x) #t))
      (i 1))
   "i")
  
  (test/spec-failed
   'define/contract4
   '(let ()
      (define/contract i (-> integer? integer?) (lambda (x) 1))
      (i #f))
   "<<unknown>>")
  
  (test/spec-failed
   'define/contract5
   '(let ()
      (define/contract i (-> integer? integer?) (lambda (x) (i #t)))
      (i 1))
   "<<unknown>>")
  
  (test/spec-passed
   'define/contract6
   '(let ()
      (define/contract contracted-func
                       (string?  string? . -> . string?)
                       (lambda (label t)
                         t))
      (contracted-func
       "I'm a string constant with side effects"
       "ans")))

  (test/spec-passed
   'define/contract7
   '(let ()
      (eval '(module contract-test-suite-define1 mzscheme
               (require mzlib/contract)
               (define/contract x string? "a")
               x))
      (eval '(require 'contract-test-suite-define1))))
  

  
;                                                                                                     
;                                                                                                     
;                                                                                                     
;           ;       ;                                                                                 
;           ;                                                                                         
;           ;                         ;                                   ;                       ;   
;    ;;;    ; ;;    ;    ;;;    ;;;  ;;;;           ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;; 
;   ;   ;   ;;  ;   ;   ;   ;  ;   ;  ;            ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;   
;  ;     ;  ;    ;  ;  ;    ; ;       ;           ;      ;     ;  ;   ;   ;    ;       ;  ;       ;   
;  ;     ;  ;    ;  ;  ;;;;;; ;       ;    ;;;;;; ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;   
;  ;     ;  ;    ;  ;  ;      ;       ;           ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;   
;   ;   ;   ;;  ;   ;   ;      ;   ;  ;            ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   
;    ;;;    ; ;;    ;    ;;;;   ;;;    ;;           ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;; 
;                   ;                                                                                 
;                   ;                                                                                 
;                 ;;                                                                                  

  
  (test/spec-passed
   'object-contract0
   '(contract (object-contract)
              (new object%)
              'pos
              'neg))

  (test/pos-blame
   'object-contract/field1
   '(contract (object-contract (field x integer?))
              (new object%)
              'pos
              'neg))
  
  (test/pos-blame
   'object-contract/field2
   '(get-field
     x
     (contract (object-contract (field x integer?))
               (new (class object% (field [x #t]) (super-new)))
               'pos
               'neg)))
  
  (test/spec-passed/result
   'object-contract/field3
   '(get-field
     x
     (contract (object-contract (field x integer?))
               (new (class object% (field [x 12]) (super-new)))
               'pos
               'neg))
   12)
  
  (test/pos-blame
   'object-contract/field4
   '(get-field
     y
     (contract (object-contract (field x boolean?) (field y boolean?))
               (new (class object% (field [x #t] [y 'x]) (super-new)))
               'pos
               'neg)))
  
  (test/pos-blame
   'object-contract/field5
   '(get-field
     x
     (contract (object-contract (field x symbol?) (field y symbol?))
               (new (class object% (field [x #t] [y 'x]) (super-new)))
               'pos
               'neg)))
  
  (test/spec-passed/result
   'object-contract/field6
   '(let ([o (contract (object-contract [m (integer? . -> . integer?)])
                       (new (class object% (field [x 1]) (define/public (m y) x) (super-new)))
                       'pos
                       'neg)])
      (list (send o m 2)
            (send/apply o m '(2))
            (let ([x '(2)]) (send o m . x))
            (with-method ([mm (o m)])
              (mm 2))
            (send* o (m 3) (m 4))))
   (list 1 1 1 1 1))
  
  (test/spec-passed/result
   'object-contract/field7
   '(let ([o (contract (object-contract)
                       (new (class object% (field [x 1]) (define/public (m y) x) (super-new)))
                       'pos
                       'neg)])
      (list (send o m 2)
            (send/apply o m '(2))
            (let ([x '(2)]) (send o m . x))
            (with-method ([mm (o m)])
              (mm 2))
            (send* o (m 3) (m 4))))
   (list 1 1 1 1 1))
  
  (test/spec-passed/result
   'object-contract/field8
   '(let ([o (contract (object-contract [m (integer? . -> . integer?)])
                       (new (class object% (define x 6) (define/public (m y) x) (super-new)))
                       'pos
                       'neg)])
      (list (send o m 2)
            (send/apply o m '(2))
            (let ([x '(2)]) (send o m . x))
            (with-method ([mm (o m)])
              (mm 2))
            (send* o (m 3) (m 4))))
   (list 6 6 6 6 6))
  
  (test/spec-passed/result
   'object-contract/field9
   '(let ([o (contract (object-contract)
                       (new (class object% (define x 6) (define/public (m y) x) (super-new)))
                       'pos
                       'neg)])
      (list (send o m 2)
            (send/apply o m '(2))
            (let ([x '(2)]) (send o m . x))
            (with-method ([mm (o m)])
              (mm 2))
            (send* o (m 3) (m 4))))
   (list 6 6 6 6 6))
  
  (test/spec-passed/result
   'object-contract/field10
   '(send (contract (object-contract)
                    (new (class object% (define x 1) (define/public (m y) x) (super-new)))
                    'pos
                    'neg)
          m
          2)
   1)
  
  (test/spec-passed/result
   'object-contract->1
   '(send
     (contract (object-contract (m (integer? . -> . integer?)))
               (new (class object% (define/public (m x) x) (super-new)))
               'pos
               'neg)
     m
     1)
   1)
  
  (test/pos-blame
   'object-contract->2
   '(contract (object-contract (m (integer? . -> . integer?)))
              (make-object object%)
              'pos
              'neg))
  
  (test/neg-blame
   'object-contract->3
   '(send
     (contract (object-contract (m (integer? . -> . integer?)))
               (make-object (class object% (define/public (m x) x) (super-instantiate ())))
               'pos
               'neg)
     m
     'x))
  
  (test/pos-blame
   'object-contract->4
   '(send
     (contract (object-contract (m (integer? . -> . integer?)))
               (make-object (class object% (define/public (m x) 'x) (super-instantiate ())))
               'pos
               'neg)
     m
     1))
  
  (test/pos-blame
   'object-contract->5
   '(contract (object-contract (m (integer? integer? . -> . integer?)))
              (make-object (class object% (define/public (m x) 'x) (super-instantiate ())))
              'pos
              'neg))

  (test/spec-passed/result
   'object-contract->6
   '(send
     (contract (object-contract (m (integer? . -> . any)))
               (new (class object% (define/public (m x) x) (super-new)))
               'pos
               'neg)
     m
     1)
   1)
  
  (test/neg-blame
   'object-contract->7
   '(send
     (contract (object-contract (m (integer? . -> . any)))
               (make-object (class object% (define/public (m x) x) (super-instantiate ())))
               'pos
               'neg)
     m
     'x))
  
  (test/spec-passed
   'object-contract->8
   '(begin
      (send
       (contract (object-contract (m (integer? . -> . any)))
                 (make-object (class object% (define/public (m x) (values 1 2)) (super-instantiate ())))
                 'pos
                 'neg)
       m
       1)
      (void)))
  
  (test/spec-passed
   'object-contract->9
   '(begin
      (send
       (contract (object-contract (m (integer? . -> . any)))
                 (make-object (class object% (define/public (m x) (values)) (super-instantiate ())))
                 'pos
                 'neg)
       m
       1)
      (void)))
  
  (test/spec-passed
   'object-contract->10
   '(begin
      (send (contract (object-contract (m (integer? . -> . (values integer? boolean?))))
                      (make-object (class object% (define/public (m x) (values 1 #t)) (super-instantiate ())))
                      'pos
                      'neg)
            m 1)
      (void)))
  
  (test/neg-blame
   'object-contract->11
   '(send 
     (contract (object-contract (m (integer? . -> . (values integer? boolean?))))
               (make-object (class object% (define/public (m x) (values #t #t)) (super-instantiate ())))
               'pos
               'neg)
     m
     #f))
  
  (test/pos-blame
   'object-contract->12
   '(send 
     (contract (object-contract (m (integer? . -> . (values integer? boolean?))))
               (make-object (class object% (define/public (m x) (values #t #t)) (super-instantiate ())))
               'pos
               'neg)
     m
     1))
  
  (test/pos-blame
   'object-contract->13
   '(send (contract (object-contract (m (integer? . -> . (values integer? boolean?))))
                    (make-object (class object% (define/public (m x) (values #f #t)) (super-instantiate ())))
                    'pos
                    'neg)
          m 1))
  
  (test/pos-blame
   'object-contract->14
   '(send (contract (object-contract (m (integer? . -> . (values integer? boolean?))))
                    (make-object (class object% (define/public (m x) (values 5 6)) (super-instantiate ())))
                    'pos
                    'neg)
          m 1))
  
  (test/pos-blame
   'object-contract-case->1
   '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                          (integer? integer? . -> . integer?))))
              (new object%)
              'pos
              'neg))
  
  (test/pos-blame
   'object-contract-case->2
   '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                          (integer? integer? . -> . integer?))))
              (new (class object% (define/public (m x) x) (super-new)))
              'pos
              'neg))
  
  (test/pos-blame
   'object-contract-case->3
   '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                          (integer? integer? . -> . integer?))))
              (new (class object% (define/public (m x y) x) (super-new)))
              'pos
              'neg))
  
  (test/spec-passed
   'object-contract-case->4
   '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                          (integer? integer? . -> . integer?))))
              (new (class object% 
                     (define/public m
                       (case-lambda
                         [(b) (not b)]
                         [(x y) (+ x y)]))
                     (super-new)))
              'pos
              'neg))
  
  (test/spec-passed/result
   'object-contract-case->5
   '(send (contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                                (integer? integer? . -> . integer?))))
                    (new (class object% 
                           (define/public m
                             (case-lambda
                               [(b) (not b)]
                               [(x y) (+ x y)]))
                           (super-new)))
                    'pos
                    'neg)
          m 
          #t)
   #f)
  
  (test/spec-passed/result
   'object-contract-case->6
   '(send (contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                                (integer? integer? . -> . integer?))))
                    (new (class object% 
                           (define/public m
                             (case-lambda
                               [(b) (not b)]
                               [(x y) (+ x y)]))
                           (super-new)))
                    'pos
                    'neg)
          m 
          3
          4)
   7)
  
  (test/pos-blame
   'object-contract-opt->*1
   '(contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
              (new (class object%
                     (define/public m
                       (opt-lambda (x [y 'a])
                         x))
                     (super-new)))
              'pos
              'neg))
  
  (test/pos-blame
   'object-contract-opt->*2
   '(contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
              (new (class object%
                     (define/public m
                       (opt-lambda (x y [z #t])
                         x))
                     (super-new)))
              'pos
              'neg))
  
  (test/spec-passed
   'object-contract-opt->*3
   '(contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
              (new (class object%
                     (define/public m
                       (opt-lambda (x [y 'a] [z #t])
                         x))
                     (super-new)))
              'pos
              'neg))
  
  (test/spec-passed/result
   'object-contract-opt->*4
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          1)
   1)
  
  (test/spec-passed/result
   'object-contract-opt->*5
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          2
          'z)
   2)
  
  (test/spec-passed/result
   'object-contract-opt->*7
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          'z
          #f)
   3)
  
  (test/neg-blame
   'object-contract-opt->*8
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          #f))
  
  (test/neg-blame
   'object-contract-opt->*9
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          2
          4))
  
  (test/neg-blame
   'object-contract-opt->*10
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          'z
          'y))
  
  (test/pos-blame
   'object-contract-opt->*11
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               'x))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          'z
          #f))
  
  (test/spec-passed/result
   'object-contract-opt->*12
   '(let-values ([(x y)
                  (send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number? symbol?))))
                                  (new (class object%
                                         (define/public m
                                           (opt-lambda (x [y 'a] [z #t])
                                             (values 1 'x)))
                                         (super-new)))
                                  'pos
                                  'neg)
                        m
                        3
                        'z
                        #f)])
      (cons x y))
   (cons 1 'x))
  
  (test/pos-blame
   'object-contract-opt->*13
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number? symbol?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               (values 'x 'x)))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          'z
          #f))
  
  (test/pos-blame
   'object-contract-opt->*14
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number? symbol?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               (values 1 1)))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          'z
          #f))

  (test/pos-blame
   'object-contract->*1
   '(contract (object-contract (m (->* (integer?) (boolean?))))
              (new (class object% (define/public (m x y) x) (super-new)))
              'pos
              'neg))
  
  (test/neg-blame
   'object-contract->*2
   '(send (contract (object-contract (m (->* (integer?) (boolean?))))
                    (new (class object% (define/public (m x) x) (super-new)))
                    'pos
                    'neg)
          m #f))
  
  (test/pos-blame
   'object-contract->*3
   '(send (contract (object-contract (m (->* (integer?) (boolean?))))
                    (new (class object% (define/public (m x) x) (super-new)))
                    'pos
                    'neg)
          m 1))
  
  (test/spec-passed
   'object-contract->*4
   '(send (contract (object-contract (m (->* (integer?) (boolean?))))
                    (new (class object% (define/public (m x) #f) (super-new)))
                    'pos
                    'neg)
          m 1))
  
  (test/pos-blame
   'object-contract->*5
   '(contract (object-contract (m (->* (integer?) any/c (boolean?))))
              (new (class object% (define/public (m x y . z) x) (super-new)))
              'pos
              'neg))
  
  (test/neg-blame
   'object-contract->*6
   '(send (contract (object-contract (m (->* (integer?) any/c (boolean?))))
                    (new (class object% (define/public (m x . z) x) (super-new)))
                    'pos
                    'neg)
          m #t))
  
  (test/pos-blame
   'object-contract->*7
   '(send (contract (object-contract (m (->* (integer?) any/c (boolean?))))
                    (new (class object% (define/public (m x . z) 1) (super-new)))
                    'pos
                    'neg)
          m 1))
  
  (test/spec-passed
   'object-contract->*8
   '(send (contract (object-contract (m (->* (integer?) any/c (boolean?))))
                    (new (class object% (define/public (m x . z) #f) (super-new)))
                    'pos
                    'neg)
          m 1))
  
  (test/spec-passed
   'object-contract->*9
   '(send (contract (object-contract (m (->* () (listof number?) (boolean?))))
                    (new (class object% (define/public (m . z) #f) (super-new)))
                    'pos
                    'neg)
          m 1 2 3))
  
  (test/neg-blame
   'object-contract->*10
   '(send (contract (object-contract (m (->* () (listof number?) (boolean?))))
                    (new (class object% (define/public (m . z) #f) (super-new)))
                    'pos
                    'neg)
          m 
          #t))
  
  (test/spec-passed
   'object-contract->d1
   '(contract (object-contract (m (->d integer? (lambda (x) (lambda (y) (and (integer? y) (= y (+ x 1))))))))
              (new (class object% (define/public (m x) 1) (super-new)))
              'pos 
              'neg))
  
  (test/neg-blame
   'object-contract->d2
   '(send (contract (object-contract (m (->d integer? (lambda (x) (lambda (y) (and (integer? y) (= y (+ x 1))))))))
                    (new (class object% (define/public (m x) 1) (super-new)))
                    'pos 
                    'neg)
          m #f))
  
  (test/pos-blame
   'object-contract->d3
   '(send (contract (object-contract (m (->d integer? (lambda (x) (lambda (y) (and (integer? y) (= y (+ x 1))))))))
                    (new (class object% (define/public (m x) 1) (super-new)))
                    'pos 
                    'neg)
          m 
          1))
  
  (test/spec-passed
   'object-contract->d4
   '(send (contract (object-contract (m (->d integer? (lambda (x) (lambda (y) (and (integer? y) (= y (+ x 1))))))))
                    (new (class object% (define/public (m x) 1) (super-new)))
                    'pos 
                    'neg)
          m 
          0))
  
  (test/spec-passed
   'object-contract->d*1
   '(contract (object-contract (m (->d* (integer? integer?) 
                                        (lambda (x z) (lambda (y) (and (integer? y) (= y (+ x 1))))))))
              (new (class object% (define/public (m x y) 1) (super-new)))
              'pos 
              'neg))
  
  (test/neg-blame
   'object-contract->d*2
   '(send (contract (object-contract (m (->d* (integer? boolean?)
                                              (lambda (x z) (lambda (y) (and (integer? y) (= y (+ x 1))))))))
                    (new (class object% (define/public (m x y) 1) (super-new)))
                    'pos 
                    'neg)
          m #f #f))
  
  (test/neg-blame
   'object-contract->d*3
   '(send (contract (object-contract (m (->d* (integer? boolean?)
                                              (lambda (x z) (lambda (y) (and (integer? y) (= y (+ x 1))))))))
                    (new (class object% (define/public (m x y) 1) (super-new)))
                    'pos 
                    'neg)
          m 1 1))
  
  (test/pos-blame
   'object-contract->d*4
   '(send (contract (object-contract (m (->d* (integer? boolean?)
                                              (lambda (x z) (lambda (y) (and (integer? y) (= y (+ x 1))))))))
                    (new (class object% (define/public (m x y) 1) (super-new)))
                    'pos 
                    'neg)
          m 
          1
          #t))
  
  (test/spec-passed
   'object-contract->d*5
   '(send (contract (object-contract (m (->d* (integer? boolean?)
                                              (lambda (x z) (lambda (y) (and (integer? y) (= y (+ x 1))))))))
                    (new (class object% (define/public (m x y) 1) (super-new)))
                    'pos 
                    'neg)
          m 
          0
          #t))
  
  (test/spec-passed
   'object-contract->d*6
   '(contract (object-contract (m (->d* (integer? integer?) 
                                        any/c
                                        (lambda (x z . rst) (lambda (y) 
                                                              (= y (length rst)))))))
              (new (class object% (define/public (m x y . z) 2) (super-new)))
              'pos 
              'neg))
  
  (test/neg-blame
   'object-contract->d*7
   '(send (contract (object-contract (m (->d* (integer? boolean?) 
                                              any/c
                                              (lambda (x z . rst) (lambda (y) 
                                                                    (= y (length rst)))))))
                    (new (class object% (define/public (m x y . z) 2) (super-new)))
                    'pos 
                    'neg)
          m 1 1))
  
  (test/neg-blame
   'object-contract->d*8
   '(send (contract (object-contract (m (->d* (integer? boolean?) 
                                              any/c
                                              (lambda (x z . rst) (lambda (y) 
                                                                    (= y (length rst)))))))
                    (new (class object% (define/public (m x y . z) 2) (super-new)))
                    'pos 
                    'neg)
          m #t #t))
  
  (test/neg-blame
   'object-contract->d*9
   '(send (contract (object-contract (m (->d* (integer? boolean?) 
                                              (listof symbol?)
                                              (lambda (x z . rst) (lambda (y) 
                                                                    (= y (length rst)))))))
                    (new (class object% (define/public (m x y . z) 2) (super-new)))
                    'pos 
                    'neg)
          m #t #t))
  
  (test/neg-blame
   'object-contract->d*10
   '(send (contract (object-contract (m (->d* (integer? boolean?) 
                                              (listof symbol?)
                                              (lambda (x z . rst) (lambda (y) 
                                                                    (= y (length rst)))))))
                    (new (class object% (define/public (m x y . z) 2) (super-new)))
                    'pos 
                    'neg)
          m 1 #t #t))
  
  (test/pos-blame
   'object-contract->d*11
   '(send (contract (object-contract (m (->d* (integer? boolean?) 
                                              (listof symbol?)
                                              (lambda (x z . rst) (lambda (y) 
                                                                    (= y (length rst)))))))
                    (new (class object% (define/public (m x y . z) 2) (super-new)))
                    'pos 
                    'neg)
          m 1 #t 'x))
  
  (test/spec-passed
   'object-contract->d*12
   '(send (contract (object-contract (m (->d* (integer? boolean?) 
                                              (listof symbol?)
                                              (lambda (x z . rst) (lambda (y) 
                                                                    (= y (length rst)))))))
                    (new (class object% (define/public (m x y . z) 2) (super-new)))
                    'pos 
                    'neg)
          m 1 #t 'x 'y))

  (test/spec-passed
   'object-contract-->r1
   '(send (contract (object-contract (m (case-> (->r ([x number?]) (<=/c x)))))
                    (new (class object% (define/public m (lambda (x) (- x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))
  
  (test/spec-passed
   'object-contract-->r1b
   '(send (contract (object-contract (m (case-> (->r ([x number?]) (<=/c x))
                                                (-> integer? integer? integer?))))
                    (new (class object% (define/public m (case-lambda [(x) (- x 1)] [(x y) x])) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/pos-blame 
   'object-contract-->r2
   '(send (contract (object-contract (m (case-> (->r ([x number?]) (<=/c x)))))
                    (new (class object% (define/public m (lambda (x) (+ x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))
  
  (test/pos-blame 
   'object-contract-->r2b
   '(send (contract (object-contract (m (case-> (->r ([x number?]) (<=/c x)) (-> integer? integer? integer?))))
                    (new (class object% (define/public m (case-lambda [(x) (+ x 1)] [(x y) y])) (super-new)))
                    'pos
                    'neg)
          m
          1))
  
  (test/spec-passed
   'object-contract-->r3
   '(send (contract (object-contract (m (->r () rst (listof number?) any/c)))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos
                    'neg)
          m
          1))
  
  (test/neg-blame
   'object-contract-->r4
   '(send (contract (object-contract (m (->r () rst (listof number?) any/c)))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos 
                    'neg)
          m
          #f))
    
  (test/spec-passed
   'object-contract-->r5
   '(send (contract (object-contract (m (->r () any)))
                    (new (class object% (define/public m (lambda () 1)) (super-new)))
                    'pos
                    'neg)
          m))
  
  (test/spec-passed
   'object-contract-->r6
   '(send (contract (object-contract (m (->r () (values [x number?] [y (>=/c x)]))))
                    (new (class object% (define/public m (lambda () (values 1 2))) (super-new)))
                    'pos
                    'neg)
          m))
  
  (test/pos-blame
   'object-contract-->r7
   '(send (contract (object-contract (m (->r () (values [x number?] [y (>=/c x)]))))
                    (new (class object% (define/public m (lambda () (values 2 1))) (super-new)))
                    'pos
                    'neg)
          m))
  
  (test/neg-blame
   'object-contract-->r/this-1
   '(send (contract (object-contract (m (->r ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x) 1)) (super-new)))
                    'pos
                    'neg)
          m
          2))
  
  (test/spec-passed
   'object-contract-->r/this-2
   '(send (contract (object-contract (m (->r ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x) 1)) (super-new)))
                    'pos
                    'neg)
          m
          1))
  
  (test/neg-blame
   'object-contract-->r/this-3
   '(send (contract (object-contract (m (->r ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                                             rest-var any/c
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x . rest) 1)) (super-new)))
                    'pos
                    'neg)
          m
          2))
  
  (test/spec-passed
   'object-contract-->r/this-4
   '(send (contract (object-contract (m (->r ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                                             rest-var any/c
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x . rest) 1)) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->pp1
   '(send (contract (object-contract (m (case-> (->pp ([x number?]) #t (<=/c x) unused #t))))
                    (new (class object% (define/public m (lambda (x) (- x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))
  
  (test/spec-passed
   'object-contract-->pp1b
   '(send (contract (object-contract (m (case-> (->pp ([x number?]) #t (<=/c x) unused #t) 
                                                (-> integer? integer? integer?))))
                    (new (class object%
                           (define/public m (case-lambda [(x) (- x 1)]
                                                         [(x y) y]))
                           (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/pos-blame 
   'object-contract-->pp2
   '(send (contract (object-contract (m (case-> (->pp ([x number?]) #t (<=/c x) unused #t))))
                    (new (class object% (define/public m (lambda (x) (+ x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))
  
  (test/pos-blame 
   'object-contract-->pp2b
   '(send (contract (object-contract (m (case-> (->pp ([x number?]) #t (<=/c x) unused #t)
                                                (-> integer? integer? integer?))))
                    (new (class object% 
                           (define/public m (case-lambda
                                              [(x) (+ x 1)]
                                              [(x y) x]))
                           (super-new)))
                    'pos
                    'neg)
          m
          1))
  
  (test/spec-passed
   'object-contract-->pp3
   '(send (contract (object-contract (m (->pp-rest () rst (listof number?) #t any/c unused #t)))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos
                    'neg)
          m
          1))
  
  (test/neg-blame
   'object-contract-->pp4
   '(send (contract (object-contract (m (->pp-rest () rst (listof number?) #t any/c unused #t)))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos 
                    'neg)
          m
          #f))
    
  (test/spec-passed
   'object-contract-->pp5
   '(send (contract (object-contract (m (->pp () #t any)))
                    (new (class object% (define/public m (lambda () 1)) (super-new)))
                    'pos
                    'neg)
          m))
  
  (test/spec-passed
   'object-contract-->pp6
   '(send (contract (object-contract (m (->pp () #t (values [x number?] [y (>=/c x)]) #t)))
                    (new (class object% (define/public m (lambda () (values 1 2))) (super-new)))
                    'pos
                    'neg)
          m))
  
  (test/pos-blame
   'object-contract-->pp7
   '(send (contract (object-contract (m (->pp () #t (values [x number?] [y (>=/c x)]) #t)))
                    (new (class object% (define/public m (lambda () (values 2 1))) (super-new)))
                    'pos
                    'neg)
          m))
  
  (test/neg-blame
   'object-contract-->pp/this-1
   '(send (contract (object-contract (m (->pp ()
                                              (= 1 (get-field f this))
                                              any/c
                                              result-x
                                              (= 2 (get-field f this)))))
                    (new (class object% (field [f 2]) (define/public m (lambda () (set! f 3))) (super-new)))
                    'pos
                    'neg)
          m))
  
  (test/pos-blame
   'object-contract-->pp/this-2
   '(send (contract (object-contract (m (->pp ()
                                              (= 1 (get-field f this))
                                              any/c
                                              result-x
                                              (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda () (set! f 3))) (super-new)))
                    'pos
                    'neg)
          m))
  
  (test/spec-passed
   'object-contract-->pp/this-3
   '(send (contract (object-contract (m (->pp ()
                                              (= 1 (get-field f this))
                                              any/c
                                              result-x
                                              (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda () (set! f 2))) (super-new)))
                    'pos
                    'neg)
          m))
  
  (test/neg-blame
   'object-contract-->pp/this-4
   '(send (contract (object-contract (m (->pp-rest ()
                                                   rest-id
                                                   any/c
                                                   (= 1 (get-field f this))
                                                   any/c
                                                   result-x
                                                   (= 2 (get-field f this)))))
                    (new (class object% (field [f 2]) (define/public m (lambda args (set! f 3))) (super-new)))
                    'pos
                    'neg)
          m))
  
  (test/pos-blame
   'object-contract-->pp/this-5
   '(send (contract (object-contract (m (->pp-rest ()
                                                   rest-id
                                                   any/c
                                                   (= 1 (get-field f this))
                                                   any/c
                                                   result-x
                                                   (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda args (set! f 3))) (super-new)))
                    'pos
                    'neg)
          m))
  
  
  
  (test/spec-passed
   'object-contract-->pp/this-6
   '(send (contract (object-contract (m (->pp-rest ()
                                                   rest-id
                                                   any/c
                                                   (= 1 (get-field f this))
                                                   any/c
                                                   result-x
                                                   (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda args (set! f 2))) (super-new)))
                    'pos
                    'neg)
          m))
  
  (test/spec-passed/result
   'object-contract-drop-method1
   '(send (contract (object-contract (m (-> integer? integer?)))
                    (new (class object% (define/public (m x) x) (define/public (n x) x) (super-new)))
                    'pos
                    'neg)
          n 1)
   1)
  
  (test/spec-passed/result
   'object-contract-drop-method2
   '(let ([o (contract (object-contract (m (-> integer? integer?)))
                       (new (class object% (define/public (m x) x) (define/public (n x) x) (super-new)))
                       'pos
                       'neg)])
      (with-method ([m (o m)]
                    [n (o n)])
        (list (m 1) (n 2))))
   '(1 2))
  
  (test/spec-passed/result
   'object-contract-drop-field1
   '(get-field g (contract (object-contract (field f integer?))
                           (new (class object% (field [f 1] [g 2]) (super-new)))
                           'pos
                           'neg))
   2)
  
  (test/spec-passed/result
   'object-contract-drop-field2
   '(field-bound? g (contract (object-contract (field f integer?))
                              (new (class object% (field [f 1] [g 2]) (super-new)))
                              'pos
                              'neg))
   #t)
  
  (test/spec-passed/result
   'object-contract-drop-field3
   '(field-names
     (contract (object-contract)
               (new (class object% (field [g 2]) (super-new)))
               'pos
               'neg))
   '(g))

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; test error message has right format
  ;;

  (test/spec-passed/result
   'wrong-method-arity-error-message
   '(with-handlers ([exn:fail? exn-message])
      (send (contract (object-contract [m (integer? . -> . integer?)])
                      (new (class object% (define/public (m x) x) (super-new)))
                      'pos
                      'neg)
            m
            1
            2))
   (string-append
    "m method: arity mismatch;\n"
    " the expected number of arguments does not match the given number\n"
    "  expected: 1\n"
    "  given: 2\n"
    "  arguments...:\n"
    "   1\n"
    "   2"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; tests object utilities to be sure wrappers work right
  ;;
  
  (let* ([o1 (contract-eval '(new object%))]
         [o2 (contract-eval `(contract (object-contract) ,o1 'pos 'neg))])
    (test #t (contract-eval 'object=?) o1 o1)
    (test #f (contract-eval 'object=?) o1 (contract-eval '(new object%)))
    (test #t (contract-eval 'object=?) o1 o2)
    (test #t (contract-eval 'object=?) o2 o1)
    (test #f (contract-eval 'object=?) (contract-eval '(new object%)) o2))
  
  (ctest #t
         method-in-interface? 
         'm 
         (object-interface 
          (contract
           (object-contract (m (integer? . -> . integer?)))
           (new (class object% (define/public (m x) x) (super-new)))
           'pos
           'neg)))
  
  (let* ([i<%> (contract-eval '(interface ()))]
         [c% (contract-eval `(class* object% (,i<%>) (super-new)))]
         [o (contract-eval `(new ,c%))])
    (test #t (contract-eval 'is-a?) o i<%>)
    (test #t (contract-eval 'is-a?) o c%)
    (test #t (contract-eval 'is-a?) (contract-eval `(contract (object-contract) ,o 'pos 'neg)) i<%>)
    (test #t (contract-eval 'is-a?) (contract-eval `(contract (object-contract) ,o 'pos 'neg)) c%))
  
  (let ([c% (parameterize ([current-inspector (make-inspector)])
              (contract-eval '(class object% (super-new))))])
    (test (list c% #f) 
          'object-info
          (contract-eval
           `(call-with-values 
             (lambda () (object-info (contract (object-contract) (new ,c%) 'pos 'neg)))
             list))))

  ;; object->vector tests
  (let* ([obj
          (parameterize ([current-inspector (make-inspector)])
            (contract-eval '(new (class object% (field [x 1] [y 2]) (super-new)))))]
         [vec (contract-eval `(object->vector ,obj))])
    (test vec
          (contract-eval 'object->vector)
          (contract-eval
           `(contract (object-contract (field x integer?) (field y integer?))
                      ,obj
                      'pos
                      'neg))))
  
;                                                                     
;                                                                     
;                                                                     
;   ;                                               ;       ;         
;                                                   ;       ;         
;                                       ;           ;       ;         
;   ;   ; ;;  ;;    ; ;;  ;;    ;   ;  ;;;;  ;;;    ; ;;    ;    ;;;  
;   ;   ;;  ;;  ;   ;;  ;;  ;   ;   ;   ;   ;   ;   ;;  ;   ;   ;   ; 
;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;       ;   ;    ;  ;  ;    ; 
;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;    ;;;;   ;    ;  ;  ;;;;;; 
;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;    ;  ;  ;      
;   ;   ;   ;   ;   ;   ;   ;   ;  ;;   ;   ;   ;   ;;  ;   ;   ;     
;   ;   ;   ;   ;   ;   ;   ;    ;; ;    ;;  ;;;;;  ; ;;    ;    ;;;; 
;                                                                     
;                                                                     
;                                                                     


  (test/pos-blame
   'immutable1
   '(let ([ct (contract (listof (boolean? . -> . boolean?)) 
                        #f 
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/neg-blame
   'immutable2
   '(let ([ct (contract (listof (boolean? . -> . boolean?)) 
                        (list (lambda (x) x)) 
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/neg-blame
   'immutable3
   '(let ([ct (contract (listof (number? . -> . boolean?)) 
                        (list (lambda (x) 1)) 
                        'pos
                        'neg)])
      ((car ct) #f)))
  
  (test/pos-blame
   'immutable4
   '(let ([ct (contract (listof (number? . -> . boolean?)) 
                        (list (lambda (x) 1)) 
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/spec-passed
   'immutable5
   '(let ([ct (contract (listof (number? . -> . boolean?)) 
                        (list (lambda (x) #t)) 
                        'pos
                        'neg)])
      ((car ct) 1)))
  

  (test/pos-blame
   'immutable6
   '(contract (cons/c (boolean? . -> . boolean?) (boolean? . -> . boolean?)) 
              #f 
              'pos
              'neg))
  
  (test/neg-blame
   'immutable8
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons (lambda (x) 1) (lambda (x) 1))
                        'pos
                        'neg)])
      ((car ct) #f)))
  
  (test/neg-blame
   'immutable9
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons (lambda (x) 1) (lambda (x) 1))
                        'pos
                        'neg)])
      ((cdr ct) #f)))
  
  (test/pos-blame
   'immutable10
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons (lambda (x) 1) (lambda (x) 1)) 
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/pos-blame
   'immutable11
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons (lambda (x) 1) (lambda (x) 1)) 
                        'pos
                        'neg)])
      ((cdr ct) 1)))
  
  (test/spec-passed
   'immutable12
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons (lambda (x) #t) (lambda (x) #t)) 
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/spec-passed
   'immutable13
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons (lambda (x) #t) (lambda (x) #t)) 
                        'pos
                        'neg)])
      ((cdr ct) 1)))
  
  (test/spec-passed/result
   'immutable14
   '(contract (cons/c number? boolean?) 
              (cons 1 #t) 
              'pos
              'neg)
   (cons 1 #t))
  
  (test/pos-blame
   'immutable15
   '(contract (list/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              #f
              'pos
              'neg))
  
  (test/pos-blame
   'immutable17
   '(contract (list/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              (list (lambda (x) #t)) 
              'pos
              'neg))
  
  (test/pos-blame
   'immutable18
   '(contract (list/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              (list (lambda (x) #t) (lambda (x) #t) (lambda (x) #t)) 
              'pos
              'neg))
  
  (test/spec-passed
   'immutable19
   '(let ([ctc (contract (list/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                         (list (lambda (x) #t) (lambda (x) #t)) 
                         'pos
                         'neg)])
      (for-each (lambda (x) (x 1)) ctc)))
  
  (test/pos-blame
   'vector-immutable1
   '(contract (vector-immutableof (boolean? . -> . boolean?)) 
              #f 
              'pos
              'neg))
  
  (test/pos-blame
   'vector-immutable2
   '(contract (vector-immutableof (boolean? . -> . boolean?)) 
              (vector (lambda (x) x)) 
              'pos
              'neg))
  
  (test/neg-blame
   'vector-immutable3
   '(let ([ct (contract (vector-immutableof (number? . -> . boolean?)) 
                        (vector->immutable-vector (vector (lambda (x) 1)))
                        'pos
                        'neg)])
      ((vector-ref ct 0) #f)))
  
  (test/pos-blame
   'vector-immutable4
   '(let ([ct (contract (vector-immutableof (number? . -> . boolean?)) 
                        (vector->immutable-vector (vector (lambda (x) 1)))
                        'pos
                        'neg)])
      ((vector-ref ct 0) 1)))
  
  (test/spec-passed
   'vector-immutable5
   '(let ([ct (contract (vector-immutableof (number? . -> . boolean?)) 
                        (vector->immutable-vector (vector (lambda (x) #t)))
                        'pos
                        'neg)])
      ((vector-ref ct 0) 1)))
  
  (test/pos-blame
   'vector-immutable6
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              #f
              'pos
              'neg))
  
  (test/pos-blame
   'vector-immutable7
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              (vector (lambda (x) #t) (lambda (x) #t)) 
              'pos
              'neg))
  
  (test/pos-blame
   'vector-immutable8
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              (vector->immutable-vector (vector (lambda (x) #t)))
              'pos
              'neg))
  
  (test/pos-blame
   'vector-immutable9
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              (vector->immutable-vector (vector (lambda (x) #t) (lambda (x) #t) (lambda (x) #t)))
              'pos
              'neg))
  
  (test/spec-passed
   'vector-immutable10
   '(let ([ctc (contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                         (vector->immutable-vector (vector (lambda (x) #t) (lambda (x) #t))) 
                         'pos
                         'neg)])
      ((vector-ref ctc 0) 1)
      ((vector-ref ctc 1) 1)))

  (test/spec-passed/result
   'vector-immutable11
   '(contract (vector-immutable/c number? boolean?) 
              (vector->immutable-vector (vector 1 #t))
              'pos
              'neg)
   (vector->immutable-vector (vector 1 #t)))
  
  (test/spec-passed/result
   'vector-immutable12
   '(immutable? (contract (vector-immutable/c number? boolean?) 
                          (vector->immutable-vector (vector 1 #t))
                          'pos
                          'neg))
   #t)

  (test/pos-blame
   'box-immutable1
   '(contract (box-immutable/c (number? . -> . boolean?)) 
              #f
              'pos
              'neg))
  
  (test/pos-blame
   'box-immutable2
   '(contract (box-immutable/c (number? . -> . boolean?)) 
              (box (lambda (x) #t)) 
              'pos
              'neg))
  
  (test/neg-blame
   'box-immutable3
   '(let ([ctc (contract (box-immutable/c (number? . -> . boolean?)) 
                         (box-immutable (lambda (x) #t))
                         'pos
                         'neg)])
      ((unbox ctc) #f)))
  
  (test/pos-blame
   'box-immutable4
   '(let ([ctc (contract (box-immutable/c (number? . -> . boolean?)) 
                         (box-immutable (lambda (x) 1))
                         'pos
                         'neg)])
      ((unbox ctc) 1)))
  
  (test/spec-passed
   'box-immutable5
   '(let ([ctc (contract (box-immutable/c (number? . -> . boolean?)) 
                         (box-immutable (lambda (x) #t))
                         'pos
                         'neg)])
      ((unbox ctc) 1)))

  (test/spec-passed/result
   'box-immutable6
   '(contract (box-immutable/c boolean?) 
              (box-immutable #t)
              'pos
              'neg)
   (box-immutable #t))
  
  (test/spec-passed/result
   'box-immutable7
   '(immutable? (contract (box-immutable/c boolean?) 
                          (box-immutable #t)
                          'pos
                          'neg))
   #t)
  
  (test/pos-blame
   'promise/c1
   '(force (contract (promise/c boolean?)
                     (delay 1)
                     'pos
                     'neg)))
  
  (test/spec-passed
   'promise/c2
   '(force (contract (promise/c boolean?)
                     (delay #t)
                     'pos
                     'neg)))
  
  (test/spec-passed/result
   'promise/c3
   '(let ([x 0])
      (contract (promise/c any/c)
                (delay (set! x (+ x 1)))
                'pos
                'neg)
      x)
   0)
  
  (test/spec-passed/result
   'promise/c4
   '(let ([x 0])
      (force (contract (promise/c any/c)
                       (delay (set! x (+ x 1)))
                       'pos
                       'neg))
      x)
   1)
  
  (test/spec-passed/result
   'promise/c5
   '(let ([x 0])
      (let ([p (contract (promise/c any/c)
                         (delay (set! x (+ x 1)))
                         'pos
                         'neg)])
        (force p)
        (force p))
      x)
   1)
  
  (test/pos-blame 
   'syntax/c1
   '(contract (syntax/c boolean?)
              #'x
              'pos
              'neg))
  
  (test/spec-passed
   'syntax/c2
   '(contract (syntax/c symbol?)
              #'x
              'pos
              'neg))
  
  (test/spec-passed
   'struct/c1
   '(let ()
      (define-struct s (a))
      (contract (struct/c s integer?)
                (make-s 1)
                'pos
                'neg)))
  
  (test/pos-blame
   'struct/c2
   '(let ()
      (define-struct s (a))
      (contract (struct/c s integer?)
                (make-s #f)
                'pos
                'neg)))
  
  (test/pos-blame
   'struct/c3
   '(let ()
      (define-struct s (a))
      (contract (struct/c s integer?)
                1
                'pos
                'neg)))
  
  (test/spec-passed
   'struct/c4
   '(let ()
      (define-struct s (a b))
      (contract (struct/c s integer? (struct/c s integer? boolean?))
                (make-s 1 (make-s 2 #t))
                'pos
                'neg)))
  
  (test/pos-blame
   'struct/c5
   '(let ()
      (define-struct s (a b))
      (contract (struct/c s integer? (struct/c s integer? boolean?))
                (make-s 1 (make-s 2 3))
                'pos
                'neg)))

  (test/spec-passed
   'recursive-contract1
   '(letrec ([ctc (-> integer? (recursive-contract ctc))])
      (letrec ([f (λ (x) f)])
        ((((contract ctc f 'pos 'neg) 1) 2) 3))))
  
  (test/neg-blame
   'recursive-contract2
   '(letrec ([ctc (-> integer? (recursive-contract ctc))])
      (letrec ([f (λ (x) f)])
        ((contract ctc f 'pos 'neg) #f))))
  
  (test/neg-blame
   'recursive-contract3
   '(letrec ([ctc (-> integer? (recursive-contract ctc))])
      (letrec ([f (λ (x) f)])
        ((((contract ctc f 'pos 'neg) 1) 2) #f))))
  
  (test/pos-blame
   'recursive-contract4
   '(letrec ([ctc (-> integer? (recursive-contract ctc))])
      (letrec ([c 0]
               [f (λ (x) 
                    (set! c (+ c 1))
                    (if (= c 2)
                        'nope
                        f))])
        ((((contract ctc f 'pos 'neg) 1) 2) 3))))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; define-contract-struct tests
  ;;
  
  (contract-eval '(define-contract-struct couple (hd tl)))
  
  (test/spec-passed
   'd-c-s-match1
   '(begin
      (eval '(module d-c-s-match1 mzscheme
               (require mzlib/contract
                        mzlib/match)
               
               (define-contract-struct foo (bar baz))
               
               (match (make-foo #t #f)
                 [($ foo bar baz) #t]
                 [_ #f])))
      (eval '(require 'd-c-s-match1))))
  
  (test/spec-passed/result
   'd-c-s-match2
   '(begin
      (eval '(module d-c-s-match2 mzscheme
               (require mzlib/contract
                        mzlib/match)
               
               (define-contract-struct foo (bar baz))
               
               (provide d-c-s-match2-f1)
               (define d-c-s-match2-f1
                 (match (make-foo 'first 'second)
                   [($ foo bar baz) (list bar baz)]
                   [_ #f]))))
      (eval '(require 'd-c-s-match2))
      (eval 'd-c-s-match2-f1))
   '(first second))
   

  
  (test/pos-blame 'd-c-s1
                  '(begin
                     (eval '(module d-c-s1 mzscheme
                              (require mzlib/contract)
                              (define-contract-struct couple (hd tl))
                              (contract (couple/c any/c any/c) 1 'pos 'neg)))
                     (eval '(require 'd-c-s1))))
  
  (test/spec-passed 'd-c-s2
                    '(contract (couple/c any/c any/c) (make-couple 1 2) 'pos 'neg))
  (test/spec-passed 'd-c-s3
                    '(contract (couple/c number? number?)
                               (make-couple 1 2)
                               'pos 'neg))
  (test/spec-passed 'd-c-s4
                    '(couple-hd 
                      (contract (couple/c number? number?)
                                (make-couple 1 2)
                                'pos 'neg)))
  (test/spec-passed 'd-c-s5
                    '(couple-tl
                      (contract (couple/c number? number?)
                                (make-couple 1 2)
                                'pos 'neg)))
  

  (test/pos-blame 
   'd-c-s6 
   '(couple-tl
     (contract (couple/c number?
                         number?)
               (make-couple #f 2)
               'pos 'neg)))
  (test/pos-blame
   'd-c-s7
   '(couple-hd
     (contract (couple/c number? number?)
               (make-couple #f 2)
               'pos 'neg)))
  
  (test/pos-blame 
   'd-c-s8
   '(contract (couple/dc [hd any/c] [tl any/c])
              1
              'pos 'neg))
  
  (test/pos-blame 
   'd-c-s9
   '(contract (couple/dc [hd () any/c] [tl () any/c])
              1
              'pos 'neg))

  
  (test/spec-passed 'd-c-s10
                    '(contract (couple/dc [hd any/c] [tl any/c])
                               (make-couple 1 2)
                               'pos 'neg))
  (test/spec-passed 'd-c-s11
                    '(contract (couple/dc [hd () any/c] [tl () any/c]) 
                               (make-couple 1 2)
                               'pos 'neg))
  
  (test/spec-passed 'd-c-s12
                    '(contract (couple/dc [hd number?]
                                          [tl number?])
                               (make-couple 1 2)
                               'pos 'neg))
  (test/spec-passed 'd-c-s13
                    '(couple-hd 
                      (contract (couple/dc [hd number?]
                                           [tl number?])
                                (make-couple 1 2)
                                'pos 'neg)))
  (test/spec-passed 'd-c-s14
                    '(couple-tl
                      (contract (couple/dc [hd number?]
                                           [tl number?])
                                (make-couple 1 2)
                                'pos 'neg)))
  
  
  (test/pos-blame 
   'd-c-s15
   '(couple-hd
     (contract (couple/dc [hd number?]
                          [tl number?])
               (make-couple #f 2)
               'pos 'neg)))
  
  (test/pos-blame
   'd-c-s16
   '(couple-tl
     (contract (couple/dc [hd number?]
                          [tl number?])
               (make-couple #f 2)
               'pos 'neg)))

  (test/spec-passed
   'd-c-s17
   '(couple-hd
     (contract (couple/dc [hd number?]
                          [tl (hd) (>=/c hd)])
               (make-couple 1 2)
               'pos 'neg)))

  (test/pos-blame
   'd-c-s18
   '(couple-hd
     (contract (couple/dc [hd number?]
                          [tl (hd) (>=/c hd)])
               (make-couple 2 1)
               'pos 'neg)))
  
  (test/spec-passed
   'd-c-s19
   '(couple-tl
     (couple-tl
      (contract (couple/dc [hd number?]
                           [tl (hd)
                               (let ([hd1 hd])
                                 (couple/dc [hd (>=/c hd1)]
                                            [tl (hd) (>=/c hd)]))])
                (make-couple 1 (make-couple 2 3))
                'pos 'neg))))
  
  (test/pos-blame
   'd-c-s20
   '(couple-tl
     (couple-tl
      (contract (couple/dc [hd number?]
                           [tl (hd)
                               (let ([hd1 hd])
                                 (couple/dc [hd (>=/c hd1)]
                                            [tl (hd) (>=/c hd1)]))])
                (make-couple 1 (make-couple 2 0))
                'pos 'neg))))
  
  (test/spec-passed
   'd-c-s21
   '(couple-hd
     (contract (couple/dc [hd number?]
                          [tl number?])
               (contract (couple/dc [hd number?]
                                    [tl number?])
                         (make-couple 1 2)
                         'pos 'neg)
               'pos 'neg)))
  
  (test/spec-passed
   'd-c-s22
   '(couple-hd
     (contract (couple/dc [hd (>=/c 0)]
                          [tl (>=/c 0)])
               (contract (couple/dc [hd number?]
                                    [tl number?])
                         (make-couple 1 2)
                         'pos 'neg)
               'pos 'neg)))
  
  (test/pos-blame
   'd-c-s23
   '(couple-hd
     (contract (couple/dc [hd (>=/c 0)]
                          [tl (>=/c 0)])
               (contract (couple/dc [hd number?]
                                    [tl number?])
                         (make-couple -1 2)
                         'pos 'neg)
               'pos 'neg)))
  
   (test/pos-blame
    'd-c-s24
    '(couple-hd
      (contract (couple/dc [hd number?]
                           [tl number?])
                (contract (couple/dc [hd (>=/c 0)]
                                     [tl (>=/c 0)])
                          (make-couple -1 2)
                          'pos 'neg)
                'pos 'neg)))
  
   (test/pos-blame
    'd-c-s25
    '(couple-hd
      (contract (couple/dc [hd number?]
                           [tl number?])
                (contract (couple/dc [hd number?]
                                     [tl number?])
                          (contract (couple/dc [hd (>=/c 0)]
                                               [tl (>=/c 0)])
                                    (make-couple -1 2)
                                    'pos 'neg)
                          'pos 'neg)
                'pos 'neg)))
  
   (test/pos-blame
    'd-c-s26
    '(couple-hd
      (contract (couple/dc [hd (>=/c 10)]
                           [tl (>=/c 10)])
                (contract (couple/dc [hd positive?]
                                     [tl positive?])
                          (contract (couple/dc [hd number?]
                                               [tl number?])
                                    (make-couple 1 2)
                                    'pos 'neg)
                          'pos 'neg)
                'pos 'neg)))
  

  ;; test caching
  (test/spec-passed 
   'd-c-s27
   '(let ([ctc (couple/c any/c any/c)])
      (couple-hd (contract ctc (contract ctc (make-couple 1 2) 'pos 'neg) 'pos 'neg))))

  ;; make sure lazy really is lazy
  (test/spec-passed
   'd-c-s28
   '(contract (couple/c number? number?)
              (make-couple #f #f)
              'pos 'neg))
  
  (test/spec-passed
   'd-c-s29
   '(couple-hd
     (contract (couple/c (couple/c number? number?)
                         (couple/c number? number?))
               (make-couple (make-couple #f #f)
                            (make-couple #f #f))
               'pos 'neg)))
  
  (test/spec-passed
   'd-c-s30
   '(couple-tl
     (contract (couple/c (couple/c number? number?)
                         (couple/c number? number?))
               (make-couple (make-couple #f #f)
                            (make-couple #f #f))
               'pos 'neg)))
  
  ;; make sure second accesses work
  (test/spec-passed/result
   'd-c-s31
   '(let ([v (contract (couple/c number? number?)
                       (make-couple 1 2)
                       'pos 'neg)])
      (list (couple-hd v) (couple-hd v)))
   (list 1 1))
  
  (test/pos-blame
   'd-c-s32
   '(let ([v (contract (couple/c number? boolean?)
                       (make-couple 1 2)
                       'pos 'neg)])
      (with-handlers ([void void]) (couple-hd v))
      (couple-hd v)))
  
  (test/pos-blame
   'd-c-s33
   '(let ([v (contract (couple/c number? number?)
                       (make-couple 1 2)
                       'pos 'neg)])
      (couple-hd v)
      (couple-hd v)
      (couple-hd
       (contract (couple/c boolean? boolean?)
                 v
                 'pos 'neg))))

  (contract-eval '(define-contract-struct single (a)))
  ;; a related test to the above:
  (test/spec-passed/result
   'd-c-s34
   '(let ([v (contract (single/c number?) (make-single 1) 'pos 'neg)])
      (single-a v)
      (let ([v3 (contract (single/c number?) v 'pos 'neg)])
        (single-a v3)))
   1)
  
  ;; make sure the caching doesn't break the semantics
  (test/pos-blame
   'd-c-s35
   '(let ([v (contract (couple/c any/c
                                 (couple/c any/c
                                           (couple/c any/c
                                                     number?)))
                       (make-couple 1
                                    (make-couple 2
                                                 (make-couple 3
                                                              #f)))
                       'pos 'neg)])
      (let* ([x (couple-tl v)]
             [y (couple-tl x)])
        (couple-hd (couple-tl x)))))
  
  (test/spec-passed/result
   'd-c-s36
   '(let ([x (make-couple 1 2)]
          [y (make-couple 1 2)]
          [c1 (couple/dc [hd any/c]
                         [tl (hd) any/c])]
          [c2 (couple/c any/c any/c)])
      (couple-hd (contract c1 x 'pos 'neg))
      (couple-hd (contract c2 x 'pos 'neg))
      (couple-hd (contract c2 y 'pos 'neg))
      (couple-hd (contract c1 y 'pos 'neg)))
   1)
  
  ;; make sure that define-contract-struct contracts can go at the top level
  (test/spec-passed
   'd-c-s37
   '(contract-stronger?
     (couple/dc [hd any/c]
                [tl (hd) any/c])
     (couple/dc [hd any/c]
                [tl (hd) any/c])))
  
  ;; test functions inside structs
  
  (test/spec-passed/result
   'd-c-s38
   '(let ([x (make-couple (lambda (x) x) (lambda (x) x))]
          [c (couple/dc [hd (-> integer? integer?)]
                        [tl (hd) any/c])])
      ((couple-hd (contract c x 'pos 'neg)) 1))
   1)
  
  (test/neg-blame
   'd-c-s39
   '(let ([x (make-couple (lambda (x) x) (lambda (x) x))]
          [c (couple/dc [hd (-> integer? integer?)]
                        [tl (hd) any/c])])
      ((couple-hd (contract c x 'pos 'neg)) #f)))
  
  (test/pos-blame
   'd-c-s40
   '(let ([x (make-couple (lambda (x) #f) (lambda (x) #f))]
          [c (couple/dc [hd (-> integer? integer?)]
                        [tl (hd) any/c])])
      ((couple-hd (contract c x 'pos 'neg)) 1)))
  
  (test/spec-passed/result
   'd-c-s41
   '(let ([x (make-couple 5 (lambda (x) x))]
          [c (couple/dc [hd number?]
                        [tl (hd) (-> (>=/c hd) (>=/c hd))])])
      ((couple-tl (contract c x 'pos 'neg)) 6))
   6)
  
  (test/pos-blame
   'd-c-s42
   '(let ([x (make-couple 5 (lambda (x) -10))]
          [c (couple/dc [hd number?]
                        [tl (hd) (-> (>=/c hd) (>=/c hd))])])
      ((couple-tl (contract c x 'pos 'neg)) 6)))
  
  (test/neg-blame
   'd-c-s42
   '(let ([x (make-couple 5 (lambda (x) -10))]
          [c (couple/dc [hd number?]
                        [tl (hd) (-> (>=/c hd) (>=/c hd))])])
      ((couple-tl (contract c x 'pos 'neg)) -11)))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  testing define-opt/c
  ;;
  
  (contract-eval '(define-contract-struct node (val obj rank left right) (make-inspector)))
  (contract-eval '(define (compute-rank n)
                    (if n 
                        (node-rank n)
                        0)))
  
  (contract-eval '(define-opt/c (leftist-heap-greater-than/rank/opt n r)
                    (or/c not
                          (node/dc [val (>=/c n)]
                                   [obj any/c]
                                   [rank (<=/c r)]
                                   [left (val) (leftist-heap-greater-than/rank/opt val +inf.0)]
                                   [right (val left) (leftist-heap-greater-than/rank/opt val (compute-rank left))]))))
  
  (contract-eval '(define leftist-heap/c (leftist-heap-greater-than/rank/opt -inf.0 +inf.0)))
  
  (test/pos-blame 'd-o/c1 '(contract leftist-heap/c 2 'pos 'neg))
  
  
  (test/spec-passed 'd-o/c2 '(contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg))
  (test/spec-passed 'd-o/c3 '(contract leftist-heap/c #f 'pos 'neg))
  (test/spec-passed 'd-o/c4 '(contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg))
  (test/spec-passed/result 'd-o/c5
                           '(node? (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg))
                           #t)
  
  (test/spec-passed/result 'd-o/c6 '(node-val (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)) 1)
  (test/spec-passed/result 'd-o/c7 '(node-obj (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)) 2)
  (test/spec-passed/result 'd-o/c8 '(node-rank (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)) 3)
  (test/spec-passed/result 'd-o/c9 '(node-left (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)) #f)
  (test/spec-passed/result 'd-o/c10 '(node-right (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)) #f)
  
  (test/spec-passed/result 'd-o/c11
                           '(node-val (contract leftist-heap/c 
                                                (contract leftist-heap/c 
                                                          (make-node 1 2 3 #f #f)
                                                          'pos 'neg)
                                                'pos 'neg))
                           1)
  (test/spec-passed/result 'd-o/c12
                           '(node-obj (contract leftist-heap/c 
                                                (contract leftist-heap/c 
                                                          (make-node 1 2 3 #f #f)
                                                          'pos 'neg)
                                                'pos 'neg))
                           2)
  (test/spec-passed/result 'd-o/c13
                           '(node-rank (contract leftist-heap/c 
                                                 (contract leftist-heap/c 
                                                           (make-node 1 2 3 #f #f)
                                                           'pos 'neg)
                                                 'pos 'neg))
                           3)
  (test/spec-passed/result 'd-o/c14
                           '(node-left (contract leftist-heap/c 
                                                 (contract leftist-heap/c 
                                                           (make-node 1 2 3 #f #f)
                                                           'pos 'neg)
                                                 'pos 'neg)) 
                           #f)
  (test/spec-passed/result 'd-o/c15
                           '(node-right (contract leftist-heap/c 
                                                  (contract leftist-heap/c 
                                                            (make-node 1 2 3 #f #f)
                                                            'pos 'neg)
                                                  'pos 'neg))
                           #f)
  
  (test/spec-passed/result 'd-o/c16
                           '(let ([h (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)])
                              (node-val h)
                              (node-val h))
                           1)
  (test/spec-passed/result 'd-o/c17
                           '(let ([h (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)])
                              (node-obj h)
                              (node-obj h))
                           2)
  
  (test/spec-passed/result 'd-o/c18
                           '(let ([h (contract leftist-heap/c (make-node 1 2 3 #f #f)'pos 'neg)])
                              (node-rank h)
                              (node-rank h))
                           3)
  (test/spec-passed/result 'd-o/c19
                           '(let ([h (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)])
                              (node-left h)
                              (node-left h))
                           #f)
  (test/spec-passed/result 'd-o/c20
                           '(let ([h (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)])
                              (node-right h)
                              (node-right h))
                           #f)
  
  (test/spec-passed/result 'd-o/c21
                           '(node-val
                             (node-right
                              (contract leftist-heap/c 
                                        (make-node 1 2 3 
                                                   (make-node 7 8 9 #f #f)
                                                   (make-node 4 5 6 #f #f))
                                        'pos 'neg)))
                           4)
  (test/spec-passed/result 'd-o/c22
                           '(node-val
                             (node-left
                              (contract leftist-heap/c 
                                        (make-node 1 2 3 
                                                   (make-node 7 8 9 #f #f)
                                                   (make-node 4 5 6 #f #f))
                                        'pos 'neg)))
                           7)
  
  (test/pos-blame 'd-o/c23
                  '(node-val
                    (node-right
                     (contract leftist-heap/c 
                               (make-node 5 2 3 
                                          (make-node 7 8 9 #f #f)
                                          (make-node 4 5 6 #f #f))
                               'pos 'neg))))
  
  (test/pos-blame 'd-o/c24
                  '(node-val
                    (node-left
                     (contract leftist-heap/c 
                               (make-node 9 2 3 
                                          (make-node 7 8 9 #f #f)
                                          (make-node 11 5 6 #f #f))
                               'pos 'neg))))
  
  (test/neg-blame 'd-o/c25
                  '((contract (-> leftist-heap/c any)
                              (λ (kh)
                                (node-val
                                 (node-left
                                  kh)))
                              'pos 'neg)
                    (make-node 9 2 3 
                               (make-node 7 8 9 #f #f)
                               (make-node 11 5 6 #f #f))))
  
  
  
  (test/spec-passed/result
   'd-o/c26 
   '(let ([ai (λ (x) (contract leftist-heap/c x 'pos 'neg))])
      (define (remove-min t) (merge (node-left t) (node-right t)))
      
      (define (merge t1 t2)
        (cond
          [(not t1) t2]
          [(not t2) t1]
          [#t
           (let ([t1-val (node-val t1)]
                 [t2-val (node-val t2)])
             (cond
               [(<= t1-val t2-val)
                (pick t1-val 
                      (node-obj t1)
                      (node-left t1)
                      (merge (node-right t1)
                             t2))]
               [#t
                (pick t2-val 
                      (node-obj t2)
                      (node-left t2)
                      (merge t1
                             (node-right t2)))]))]))
      
      (define (pick x obj a b)
        (let ([ra (compute-rank a)]
              [rb (compute-rank b)])
          (cond
            [(>= ra rb)
             (make-node x obj (+ rb 1) a b)]
            [#t
             (make-node x obj (+ ra 1) b a)])))
      (node-val
       (remove-min (ai (make-node 137 'x 1 
                                  (ai (make-node 178 'y 1 
                                                 (make-node 178 'z 1 #f #f)
                                                 #f))
                                  #f)))))
   178)
  
  ;;
  ;;  end of define-opt/c
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; NOT YET RELEASED
  #;
  (test/pos-blame
   'd-c-s/attr-1
   '(let ()
      (define-contract-struct pr (x y))
      (pr-x
       (contract (pr/dc [x integer?]
                        [y integer?]
                        where
                        [x-val x]
                        [y-val y]
                        and
                        (= x-val y-val))
                 (make-pr 4 5)
                 'pos
                 'neg))))
  
  ;; NOT YET RELEASED
  #;
  (test/spec-passed
   'd-c-s/attr-2
   '(let ()
      (define-contract-struct pr (x y))
      (contract (pr/dc [x integer?]
                       [y integer?]
                       where
                       [x-val x]
                       [y-val y]
                       and
                       (= x-val y-val))
                (make-pr 4 5)
                'pos
                'neg)))
  
  ;; NOT YET RELEASED
  #;
  (let ()
    (define-contract-struct node (n l r) (make-inspector))
    
    (define (get-val n attr)
      (if (null? n)
          1
          (let ([h (synthesized-value n attr)])
            (if (unknown? h)
                h
                (+ h 1)))))
    
    (define (full-bbt lo hi)
      (or/c null?
            (node/dc [n (between/c lo hi)]
                     [l (n) (full-bbt lo n)]
                     [r (n) (full-bbt n hi)]
                     
                     where
                     [lheight (get-val l lheight)]
                     [rheight (get-val r rheight)]
                     
                     and
                     (<= 0 (- lheight rheight) 1))))
    
    (define t (contract (full-bbt -inf.0 +inf.0)
                        (make-node 0
                                   (make-node -1 null null) 
                                   (make-node 2
                                              (make-node 1 null null) 
                                              (make-node 3 null null)))
                        'pos
                        'neg))
    (test/spec-passed
     'd-c-s/attr-3
     `(,node-l (,node-l ,t)))
     
    (test/pos-blame 
     'd-c-s/attr-4
     `(,node-r (,node-r (,node-r ,t)))))

  ;; NOT YET RELEASED
  #|

need a test that will revisit a node a second time (when it already has a wrapper)
with a new parent. make sure the new parent is recorded in the parents field
so that propagation occurs.

|#
  
  
  ;; test the predicate
  (ctest #t couple? (contract (couple/c any/c any/c) (make-couple 1 2) 'pos 'neg))
  (ctest #t couple? (make-couple 1 2))
  (ctest #t couple? (contract (couple/dc [hd any/c] [tl (hd) any/c]) (make-couple 1 2) 'pos 'neg))
  (ctest #f couple? 1)
  (ctest #f couple? #f)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                        ;;
  ;;   Flat Contract Tests                                  ;;
  ;;                                                        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (ctest #t flat-contract? (or/c))
  (ctest #t flat-contract? (or/c integer? (lambda (x) (> x 0))))
  (ctest #t flat-contract? (or/c (flat-contract integer?) (flat-contract boolean?)))
  (ctest #t flat-contract? (or/c integer? boolean?))
  (test-flat-contract '(or/c (flat-contract integer?) char?) #\a #t)
  (test-flat-contract '(or/c (flat-contract integer?) char?) 1 #t)
  
  (ctest #t flat-contract? (and/c))
  (ctest #t flat-contract? (and/c number? integer?))
  (ctest #t flat-contract? (and/c (flat-contract number?)
				 (flat-contract integer?)))
  (ctest #t flat-contract? (let ()
                            (define-struct s (a b))
                            (struct/c s any/c any/c)))
  
  (test-flat-contract '(and/c number? integer?) 1 3/2)

  (test-flat-contract '(not/c integer?) #t 1)
  (test-flat-contract '(=/c 2) 2 3)
  (test-flat-contract '(>=/c 5) 5 0)
  (test-flat-contract '(<=/c 5) 5 10)
  (test-flat-contract '(</c 5) 0 5)
  (test-flat-contract '(>/c 5) 10 5)
  (test-flat-contract '(integer-in 0 10) 0 11)
  (test-flat-contract '(integer-in 0 10) 10 3/2)
  (test-flat-contract '(integer-in 0 10) 1 1.0)
  (test-flat-contract '(real-in 1 10) 3/2 20)
  (test-flat-contract '(string/len 3) "ab" "abc")
  (test-flat-contract 'natural-number/c 5 -1)
  (test-flat-contract 'false/c #f #t)
  (test/spec-passed 'any/c '(contract any/c 1 'pos 'neg))
  (test-flat-contract 'printable/c (vector (cons 1 (box #f))) (lambda (x) x))
  (test-flat-contract '(symbols 'a 'b 'c) 'a 'd)
  (test-flat-contract '(one-of/c (expt 2 65)) (expt 2 65) 12)
  (test-flat-contract '(one-of/c '#:x '#:z) '#:x '#:y)
  
  (let ([c% (contract-eval '(class object% (super-new)))])
    (test-flat-contract `(subclass?/c ,c%) c% (contract-eval `object%))
    (test-flat-contract `(subclass?/c ,c%) (contract-eval `(class ,c%)) (contract-eval `(class object%))))
  
  (let ([i<%> (contract-eval '(interface ()))])
    (test-flat-contract `(implementation?/c ,i<%>) 
                        (contract-eval `(class* object% (,i<%>) (super-new)))
                        (contract-eval 'object%))
    (test-flat-contract `(implementation?/c ,i<%>) 
                        (contract-eval `(class* object% (,i<%>) (super-new)))
                        #f))
  
  (let ([i<%> (contract-eval '(interface ()))]
        [c% (contract-eval '(class object% (super-new)))])
    (test-flat-contract `(is-a?/c ,i<%>) 
                        (contract-eval `(new (class* object% (,i<%>) (super-new))))
                        (contract-eval '(new object%)))
    (test-flat-contract `(is-a?/c ,c%)
                        (contract-eval `(new ,c%))
                        (contract-eval '(new object%))))
  
  (test-flat-contract '(listof boolean?) (list #t #f) (list #f 3 #t))
  (test-flat-contract '(listof any/c) (list #t #f) 3)
  
  (test-flat-contract '(vectorof boolean?) (vector #t #f) (vector #f 3 #t))
  (test-flat-contract '(vectorof any/c) (vector #t #f) 3)
  
  (test-flat-contract '(vector/c boolean? (flat-contract integer?)) (vector #t 1) (vector 1 #f))
  (test-flat-contract '(vector/c boolean? (flat-contract integer?)) (vector #t 1) #f)

  (test-flat-contract '(cons/c boolean? (flat-contract integer?)) (cons #t 1) (cons 1 #f))
  (test-flat-contract '(cons/c boolean? (flat-contract integer?)) (cons #t 1) #f)
  (test-flat-contract '(list/c boolean? (flat-contract integer?)) (list #t 1) (list 1 #f))
  (test-flat-contract '(list/c boolean? (flat-contract integer?)) (list #t 1) #f)

  (contract-eval '(define (a-predicate-that-wont-be-optimized x) (boolean? x)))
  (test-flat-contract '(cons/c a-predicate-that-wont-be-optimized (flat-contract integer?)) (cons #t 1) (cons 1 #f))
  (test-flat-contract '(cons/c a-predicate-that-wont-be-optimized (flat-contract integer?)) (cons #t 1) #f)
  (test-flat-contract '(list/c a-predicate-that-wont-be-optimized (flat-contract integer?)) (list #t 1) (list 1 #f))
  (test-flat-contract '(list/c a-predicate-that-wont-be-optimized (flat-contract integer?)) (list #t 1) #f)

  (test-flat-contract '(box/c boolean?) (box #f) (box 1))
  (test-flat-contract '(box/c (flat-contract boolean?)) (box #t) #f)
  
  (test-flat-contract '(flat-rec-contract sexp (cons/c sexp sexp) number?) '(1 2 . 3) '(1 . #f))
  (test-flat-contract '(flat-murec-contract ([even1 (or/c null? (cons/c number? even2))] 
                                             [even2 (cons/c number? even1)])
                                            even1)
                      '(1 2 3 4)
                      '(1 2 3))
  (test #t 'malformed-binder
        (with-handlers ((exn? exn:fail:syntax?)) 
          (contract-eval '(flat-murec-contract ([(x) y]) x))
          'no-err))
  (test #t 'missing-body 
        (with-handlers ((exn? exn:fail:syntax?)) 
          (contract-eval '(flat-murec-contract ([x y])))
          'no-err))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                        ;;
  ;;   case-> arity checking tests                          ;;
  ;;                                                        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (test/well-formed '(case-> (-> integer? integer?)))
  (test/well-formed '(case-> (-> integer? integer?) (-> integer? integer? integer?)))
  (test/well-formed '(case-> (-> integer? integer?) (-> integer? integer? any)))
  (test/well-formed '(case-> (-> integer? any) (-> integer? integer? any)))
  
  (test/well-formed '(case-> (->d (lambda x any/c)) (-> integer? integer?)))

  (test/well-formed '(case-> (->* (any/c any/c) (integer?)) (-> integer? integer?)))
  (test/well-formed '(case-> (->* (any/c any/c) any/c (integer?)) (-> integer? integer?)))
  (test/well-formed '(case-> (->* (any/c any/c) any/c any) (-> integer? integer?)))
  
  (test/well-formed '(case-> (->d* (any/c any/c) (lambda x any/c)) (-> integer? integer?)))
  (test/well-formed '(case-> (->d* (any/c any/c) any/c (lambda x any/c)) (-> integer? integer?)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                        ;;
  ;;   Inferred Name Tests                                  ;;
  ;;                                                        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (contract-eval 
   '(module contract-test-suite-inferred-name1 mzscheme
      (require mzlib/contract)
      (define contract-inferred-name-test-contract (-> integer? any))
      (define (contract-inferred-name-test x) #t)
      (provide/contract (contract-inferred-name-test contract-inferred-name-test-contract))
      
      (define (contract-inferred-name-test2 x) x)
      (provide/contract (contract-inferred-name-test2 (-> number? number?)))
      
      (define (contract-inferred-name-test2b x) (values x x))
      (provide/contract (contract-inferred-name-test2b (-> number? (values number? number?))))
      
      (define (contract-inferred-name-test3 x . y) x)
      (provide/contract (contract-inferred-name-test3 (->* (number?) (listof number?) (number?))))

      (define contract-inferred-name-test4
        (case-lambda [(x) x]
                     [(x y) x]))
      (provide/contract (contract-inferred-name-test4 (case-> (->* (number?) (number?))
                                                              (-> integer? integer? integer?))))

      (define contract-inferred-name-test5 (case-lambda [(x) x] [(x y) x]))
      (provide/contract (contract-inferred-name-test5 (case-> (-> number? number?) 
                                                              (-> number? number? number?))))

      (define contract-inferred-name-test6 (case-lambda [(x) x]
                                                        [(x y) y]))
      (provide/contract (contract-inferred-name-test6 (opt-> (number?) (number?) number?)))
      
      (define contract-inferred-name-test7 (case-lambda [(x) (values x x)]
                                                        [(x y) (values y y)]))
      (provide/contract (contract-inferred-name-test7 (opt->* (number?) (number?) (number? number?))))))
  (contract-eval '(require 'contract-test-suite-inferred-name1))
  ;; (eval '(test 'contract-inferred-name-test object-name contract-inferred-name-test)) ;; this one can't be made to pass, sadly.
  (test 'contract-inferred-name-test2 object-name (contract-eval 'contract-inferred-name-test2))
  (test 'contract-inferred-name-test2b object-name (contract-eval 'contract-inferred-name-test2b))
  (test 'contract-inferred-name-test3 object-name (contract-eval 'contract-inferred-name-test3))
  (test 'contract-inferred-name-test4 object-name (contract-eval 'contract-inferred-name-test4))
  (test 'contract-inferred-name-test5 object-name (contract-eval 'contract-inferred-name-test5))
  (test 'contract-inferred-name-test6 object-name (contract-eval 'contract-inferred-name-test6))
  (test 'contract-inferred-name-test7 object-name (contract-eval 'contract-inferred-name-test7))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                        ;;
  ;;   Contract Name Tests                                  ;;
  ;;                                                        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (test-name 'integer? (flat-contract integer?))
  (test-name 'boolean? (flat-contract boolean?))
  (test-name 'char? (flat-contract char?))
  (test-name 'any/c any/c)
  (test-name '(-> integer? integer?) (-> integer? integer?))
  (test-name '(-> integer? any) (-> integer? any))
  (test-name '(-> integer? (values boolean? char?)) (-> integer? (values boolean? char?)))
  (test-name '(-> integer? boolean? (values char? any/c)) (->* (integer? boolean?) (char? any/c)))
  (test-name '(-> integer? boolean? any) (->* (integer? boolean?) any))
  (test-name '(->* (integer?) boolean? (char? any/c)) (->* (integer?) boolean? (char? any/c)))
  (test-name '(->* (integer? char?) boolean? any) (->* (integer? char?) boolean? any))
  (test-name '(->d integer? boolean? ...) (->d integer? boolean? (lambda (x y) char?)))
  (test-name '(->d* (integer? boolean?) ...) (->d* (integer? boolean?) (lambda (x y) char?)))
  (test-name '(->d* (integer? boolean?) any/c ...) (->d* (integer? boolean?) any/c (lambda (x y . z) char?)))
  (test-name '(->r ((x ...)) ...) (->r ((x number?)) number?))
  (test-name '(->r ((x ...) (y ...) (z ...)) ...) (->r ((x number?) (y boolean?) (z pair?)) number?))
  (test-name '(->r ((x ...) (y ...) (z ...)) rest-x ... ...) 
             (->r ((x number?) (y boolean?) (z pair?)) rest-x any/c number?))
  (test-name '(->pp ((x ...)) ...) (->pp ((x number?)) #t number? blech #t))
  
  (test-name '(->r ((x ...)) ...) (case-> (->r ((x number?)) number?)))
  (test-name '(case-> (->r ((x ...)) ...) (-> integer? integer? integer?))
             (case-> (->r ((x number?)) number?) (-> integer? integer? integer?)))
  (test-name '(->r ((x ...) (y ...) (z ...)) ...)
             (case-> (->r ((x number?) (y boolean?) (z pair?)) number?)))
  (test-name '(case-> (->r ((x ...) (y ...) (z ...)) ...)
                      (-> integer? integer? integer?))
             (case-> (->r ((x number?) (y boolean?) (z pair?)) number?)
                     (-> integer? integer? integer?)))
  (test-name '(case->) (case->))
  
  (test-name '(case-> (-> integer? integer?) (-> integer? integer? integer?))
             (case-> (-> integer? integer?) (-> integer? integer? integer?)))

  (test-name '(unconstrained-domain-> number?) (unconstrained-domain-> number?))
  
  (test-name '(or/c) (or/c))
  (test-name '(or/c integer? gt0?) (or/c integer? (let ([gt0? (lambda (x) (> x 0))]) gt0?)))
  (test-name '(or/c integer? boolean?)
             (or/c (flat-contract integer?)
                   (flat-contract boolean?)))
  (test-name '(or/c integer? boolean?)
             (or/c integer? boolean?))
  (test-name '(or/c (-> (>=/c 5) (>=/c 5)) boolean?)
             (or/c (-> (>=/c 5) (>=/c 5)) boolean?))
  (test-name '(or/c boolean? (-> (>=/c 5) (>=/c 5)))
             (or/c boolean? (-> (>=/c 5) (>=/c 5))))
  (test-name '(or/c (-> (>=/c 5) (>=/c 5))
                    (-> (<=/c 5) (<=/c 5) (<=/c 5)))
             (or/c (-> (>=/c 5) (>=/c 5))
                   (-> (<=/c 5) (<=/c 5) (<=/c 5))))
  (test-name '(or/c boolean?
                    (-> (>=/c 5) (>=/c 5))
                    (-> (<=/c 5) (<=/c 5) (<=/c 5)))
             (or/c boolean?
                   (-> (>=/c 5) (>=/c 5))
                   (-> (<=/c 5) (<=/c 5) (<=/c 5))))
  
  (test-name 'any/c (and/c))
  (test-name '(and/c any/c) (and/c any/c))
  (test-name '(and/c any/c any/c) (and/c any/c any/c))
  (test-name '(and/c number? integer?) (and/c number? integer?))
  (test-name '(and/c number? integer?) (and/c (flat-contract number?)
                                              (flat-contract integer?)))
  (test-name '(and/c number? (-> integer? integer?)) (and/c number? (-> integer? integer?)))
  (test-name '(and/c (-> boolean? boolean?) (-> integer? integer?)) (and/c (-> boolean? boolean?) (-> integer? integer?)))

  (test-name '(not/c integer?) (not/c integer?))
  (test-name '(=/c 5) (=/c 5))
  (test-name '(>=/c 5) (>=/c 5))
  (test-name '(<=/c 5) (<=/c 5))
  (test-name '(</c 5) (</c 5))
  (test-name '(>/c 5) (>/c 5))
  (test-name '(between/c 5 6) (between/c 5 6))
  (test-name '(integer-in 0 10) (integer-in 0 10))
  (test-name '(between/c 1 10) (real-in 1 10))
  (test-name '(string-len/c 3) (string/len 3))
  (test-name 'natural-number/c natural-number/c)
  (test-name #f false/c)
  (test-name 'printable/c printable/c)
  (test-name '(or/c 'a 'b 'c) (symbols 'a 'b 'c))
  (test-name '(or/c 1 2 3) (one-of/c 1 2 3))
  (test-name '(or/c '() 'x 1 #f #\a void?)
             (one-of/c '() 'x 1 #f #\a (void)))
  
  (test-name '(subclass?/c c%) 
             (let ([c% (class object% (super-new))]) (subclass?/c c%)))
  
  (test-name '(implementation?/c i<%>) 
             (let ([i<%> (interface ())])
               (implementation?/c i<%>)))
  
  (test-name '(is-a?/c i<%>)
             (let ([i<%> (interface ())])
               (is-a?/c i<%>)))
  (test-name '(is-a?/c c%) 
             (let ([i<%> (interface ())]
                   [c% (class object% (super-new))])
               (is-a?/c c%)))
  
  (test-name '(listof boolean?) (listof boolean?))  
  (test-name '(listof any/c) (listof any/c))
  (test-name '(listof boolean?) (listof boolean?))
  (test-name '(listof any/c) (listof any/c))
  (test-name '(listof boolean?) (listof boolean?))
  (test-name '(listof (-> boolean? boolean?)) (listof (-> boolean? boolean?)))
  
  (test-name '(vectorof boolean?) (vectorof boolean?))
  (test-name '(vectorof any/c) (vectorof any/c))
  
  (test-name '(vector/c boolean? integer?) (vector/c boolean? integer?))
  (test-name '(vector/c boolean? integer?) (vector/c boolean? (flat-contract integer?)))

  (test-name '(cons/c boolean? integer?) (cons/c boolean? (flat-contract integer?)))
  (test-name '(cons/c boolean? integer?) (cons/c boolean? (flat-contract integer?)))
  (test-name '(list/c boolean? integer?) (list/c boolean? (flat-contract integer?)))
  (test-name '(list/c boolean? integer?) (list/c boolean? (flat-contract integer?)))

  (test-name '(cons/c boolean? integer?) (cons/c boolean? (flat-contract integer?)))
  (test-name '(cons/c boolean? integer?) (cons/c boolean? (flat-contract integer?)))
  (test-name '(cons/c boolean? integer?) (cons/c boolean? (flat-contract integer?)))
  (test-name '(cons/c (-> boolean? boolean?) integer?) (cons/c (-> boolean? boolean?) integer?))
  
  (test-name '(list/c boolean? integer?)
             (list/c boolean? (flat-contract integer?)))
  (test-name '(list/c boolean? integer?) 
             (list/c boolean? (flat-contract integer?)))
  (test-name '(list/c boolean? integer?)
             (list/c boolean? (flat-contract integer?)))
  (test-name '(list/c (-> boolean? boolean?) integer?)
             (list/c (-> boolean? boolean?) integer?))
  
  (test-name '(parameter/c integer?) (parameter/c integer?))
  
  (test-name '(box/c boolean?) (box/c boolean?))
  (test-name '(box/c boolean?) (box/c (flat-contract boolean?)))
  (test-name 'the-name (flat-rec-contract the-name))

  (test-name '(object-contract) (object-contract))
  (test-name '(object-contract (field x integer?)) (object-contract (field x integer?)))
  (test-name '(object-contract (m (-> integer? integer?)))
             (object-contract (m (-> integer? integer?))))
  (test-name '(object-contract (m (-> integer? any)))
             (object-contract (m (-> integer? any))))
  (test-name '(object-contract (m (-> integer? (values integer? integer?)))) 
             (object-contract (m (-> integer? (values integer? integer?)))))
  (test-name '(object-contract (m (case-> (-> integer? integer? integer?)
                                          (-> integer? (values integer? integer?)))))
             (object-contract (m (case-> 
                                  (-> integer? integer? integer?)
                                  (-> integer? (values integer? integer?))))))
  (test-name
   '(object-contract (m (case-> (-> integer? symbol?)
                                (-> integer? boolean? symbol?)
                                (-> integer? boolean? number? symbol?))))
   (object-contract (m (opt->* (integer?) (boolean? number?) (symbol?)))))
  (test-name
    '(object-contract (m (case-> (-> integer? symbol?)
                                 (-> integer? boolean? symbol?)
                                 (-> integer? boolean? number? symbol?))))
   (object-contract (m (opt-> (integer?) (boolean? number?) symbol?))))
  (test-name
    '(object-contract (m (case-> (-> integer? any)
                                 (-> integer? boolean? any)
                                 (-> integer? boolean? number? any))))
   (object-contract (m (opt->* (integer?) (boolean? number?) any))))
  (test-name
    '(object-contract (m (case-> (-> integer? (values symbol? boolean?))
                                 (-> integer? boolean? (values symbol? boolean?)))))
   (object-contract (m (opt->* (integer?) (boolean?) (symbol? boolean?)))))
  
  (test-name '(object-contract (m (->r ((x ...)) ...))) (object-contract (m (->r ((x number?)) number?))))
  (test-name '(object-contract (m (->r ((x ...) (y ...) (z ...)) ...))) 
             (object-contract (m (->r ((x number?) (y boolean?) (z pair?)) number?))))
  (test-name '(object-contract (m (->r ((x ...) (y ...) (z ...)) rest-x ... ...))) 
             (object-contract (m (->r ((x number?) (y boolean?) (z pair?)) rest-x any/c number?))))
  (test-name '(promise/c any/c) (promise/c any/c))
  (test-name '(syntax/c any/c) (syntax/c any/c))
  (test-name '(struct/c st integer?) 
             (let ()
               (define-struct st (a))
               (struct/c st integer?)))
  
  (test-name '(recursive-contract (box/c boolean?)) (recursive-contract (box/c boolean?)))
  (test-name '(recursive-contract x) (let ([x (box/c boolean?)]) (recursive-contract x)))
  
  (test-name '(couple/c any/c any/c) 
             (couple/c any/c any/c))
  (test-name '(couple/c any/c any/c) 
             (couple/dc [hd any/c] [tl any/c]))
  (test-name '(couple/dc [hd any/c] [tl ...])
             (couple/dc [hd any/c] [tl (hd) any/c]))

  ;; NOT YET RELEASED
  #;
  (test-name '(pr/dc [x integer?]
                     [y integer?]
                     where
                     [x-val ...]
                     [y-val ...]
                     and
                     ...)
             (let ()
               (define-contract-struct pr (x y))
               (pr/dc [x integer?]
                      [y integer?]
                      where
                      [x-val x]
                      [y-val y]
                      and
                      (= x-val y-val))))
             
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  stronger tests
  ;;
  
  (ctest #t contract-stronger? any/c any/c)
  (ctest #t contract-stronger? (between/c 1 3) (between/c 0 4))
  (ctest #f contract-stronger? (between/c 0 4) (between/c 1 3))
  (ctest #t contract-stronger? (>=/c 3) (>=/c 2))
  (ctest #f contract-stronger? (>=/c 2) (>=/c 3))
  (ctest #f contract-stronger? (<=/c 3) (<=/c 2))
  (ctest #t contract-stronger? (<=/c 2) (<=/c 3))
  (ctest #f contract-stronger? (recursive-contract (<=/c 2)) (recursive-contract (<=/c 3)))
  (ctest #f contract-stronger? (recursive-contract (<=/c 3)) (recursive-contract (<=/c 2)))
  (let ([f (contract-eval '(λ (x) (recursive-contract (<=/c x))))])
    (test #t (contract-eval 'contract-stronger?) (contract-eval `(,f 1)) (contract-eval `(,f 1))))
  (ctest #t contract-stronger? (-> integer? integer?) (-> integer? integer?))
  (ctest #f contract-stronger? (-> boolean? boolean?) (-> integer? integer?))
  (ctest #t contract-stronger? (-> (>=/c 3) (>=/c 3)) (-> (>=/c 4) (>=/c 3)))
  (ctest #f contract-stronger? (-> (>=/c 4) (>=/c 3)) (-> (>=/c 3) (>=/c 3)))
  (ctest #t contract-stronger? (-> (>=/c 3) (>=/c 3)) (-> (>=/c 3) (>=/c 2)))
  (ctest #f contract-stronger? (-> (>=/c 3) (>=/c 2)) (-> (>=/c 3) (>=/c 3)))
  (ctest #f contract-stronger? (-> (>=/c 2)) (-> (>=/c 3) (>=/c 3)))
  (ctest #t contract-stronger? (or/c null? any/c) (or/c null? any/c))
  (ctest #f contract-stronger? (or/c null? any/c) (or/c boolean? any/c))
  (ctest #t contract-stronger? (or/c null? boolean?) (or/c null? boolean?))
  (ctest #t contract-stronger? (or/c null? boolean?) (or/c boolean? null?))
  (ctest #t contract-stronger? (or/c null? (-> integer? integer?)) (or/c null? (-> integer? integer?)))
  (ctest #f contract-stronger? (or/c null? (-> boolean? boolean?)) (or/c null? (-> integer? integer?)))
  
  (ctest #t contract-stronger? number? number?)
  (ctest #f contract-stronger? boolean? number?)
    
  (ctest #t contract-stronger? (parameter/c (between/c 0 5)) (parameter/c (between/c 0 5)))
  (ctest #f contract-stronger? (parameter/c (between/c 0 5)) (parameter/c (between/c 1 4)))
  (ctest #f contract-stronger? (parameter/c (between/c 1 4)) (parameter/c (between/c 0 5)))
  
  (ctest #t contract-stronger? (symbols 'x 'y) (symbols 'x 'y 'z))
  (ctest #f contract-stronger? (symbols 'x 'y 'z) (symbols 'x 'y))
  (ctest #t contract-stronger? (symbols 'x 'y) (symbols 'z 'x 'y))
  (ctest #f contract-stronger? (symbols 'z 'x 'y) (symbols 'x 'y))
  (ctest #t contract-stronger? (one-of/c (expt 2 100)) (one-of/c (expt 2 100) 12))
  
  (ctest #t contract-stronger?
         (or/c (-> (>=/c 3) (>=/c 3)) (-> string?))
         (or/c (-> (>=/c 4) (>=/c 3)) (-> string?)))
  (ctest #f contract-stronger?
         (or/c (-> string?) (-> integer? integer?))
         (or/c (-> string?) (-> any/c integer?)))
  (ctest #f contract-stronger?
         (or/c (-> string?) (-> any/c integer?))
         (or/c (-> string?) (-> integer? integer?)))
  (ctest #t contract-stronger?
         (or/c (-> string?) (-> integer? integer?) integer? boolean?)
         (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (ctest #f contract-stronger?
         (or/c (-> string?) (-> integer? integer?) integer? char?)
         (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (ctest #t contract-stronger?
         (or/c (-> string?) (-> integer? integer?) integer?)
         (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (ctest #f contract-stronger?
         (or/c (-> string?) (-> integer? integer?) integer?)
         (or/c (-> integer? integer?) integer?))

  (contract-eval
   `(let ()
      (define (non-zero? x) (not (zero? x)))
      (define list-of-numbers
        (or/c null?
              (couple/c number?
                        (recursive-contract list-of-numbers))))
      (define (short-list/less-than n)
        (or/c null?
              (couple/c (<=/c n)
                        (or/c null?
                              (couple/c (<=/c n)
                                        any/c)))))
      (define (short-sorted-list/less-than n)
        (or/c null?
              (couple/dc 
               [hd (<=/c n)]
               [tl (hd) (or/c null?
                              (couple/c (<=/c hd)
                                        any/c))])))
      
      (define (sorted-list/less-than n)
        (or/c null?
              (couple/dc 
               [hd (<=/c n)]
               [tl (hd) (sorted-list/less-than hd)])))
      
      ;; for some reason, the `n' makes it harder to optimize. without it, this test isn't as good a test
      (define (closure-comparison-test n)
        (couple/dc 
         [hd any/c]
         [tl (hd) any/c]))
      
      (,test #t contract-stronger? (couple/c any/c any/c) (couple/c any/c any/c))
      (,test #f contract-stronger? (couple/c (>=/c 2) (>=/c 3)) (couple/c (>=/c 4) (>=/c 5)))
      (,test #t contract-stronger? (couple/c (>=/c 4) (>=/c 5)) (couple/c (>=/c 2) (>=/c 3)))
      (,test #f contract-stronger? (couple/c (>=/c 1) (>=/c 5)) (couple/c (>=/c 5) (>=/c 1)))
      (let ([ctc (couple/dc [hd any/c] [tl (hd) any/c])])
        (,test #t contract-stronger? ctc ctc))
      (let ([ctc (couple/dc [hd any/c] [tl (hd) (<=/c hd)])])
        (,test #t contract-stronger? ctc ctc))
      (,test #t contract-stronger? list-of-numbers list-of-numbers)
      (,test #t contract-stronger? (short-list/less-than 4) (short-list/less-than 5))
      (,test #f contract-stronger? (short-list/less-than 5) (short-list/less-than 4))
      (,test #t contract-stronger? (short-sorted-list/less-than 4) (short-sorted-list/less-than 5))
      (,test #f contract-stronger? (short-sorted-list/less-than 5) (short-sorted-list/less-than 4))
      (,test #t contract-stronger? (sorted-list/less-than 4) (sorted-list/less-than 5))
      (,test #f contract-stronger? (sorted-list/less-than 5) (sorted-list/less-than 4))
      (,test #t contract-stronger? (closure-comparison-test 4) (closure-comparison-test 5))))

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  first-order tests
  ;;
  
  (ctest #t contract-first-order-passes? (flat-contract integer?) 1)
  (ctest #f contract-first-order-passes? (flat-contract integer?) 'x)
  (ctest #t contract-first-order-passes? (flat-contract boolean?) #t)
  (ctest #f contract-first-order-passes? (flat-contract boolean?) 'x)
  (ctest #t contract-first-order-passes? any/c 1)
  (ctest #t contract-first-order-passes? any/c #t)
  (ctest #t contract-first-order-passes? (-> integer? integer?) (λ (x) #t))
  (ctest #f contract-first-order-passes? (-> integer? integer?) (λ (x y) #t))
  (ctest #f contract-first-order-passes? (-> integer? integer?) 'x)
  (ctest #t contract-first-order-passes? (-> integer? boolean? integer?) (λ (x y) #t))
  (ctest #f contract-first-order-passes? (-> integer? boolean? integer?) (λ (x) #t))
  (ctest #f contract-first-order-passes? (-> integer? boolean? integer?) (λ (x y z) #t))
  
  (ctest #t contract-first-order-passes? (->* (integer?) boolean? (char? any/c)) (λ (x . y) #f))
  (ctest #f contract-first-order-passes? (->* (integer?) boolean? (char? any/c)) (λ (x y . z) #f))
  (ctest #f contract-first-order-passes? (->* (integer?) boolean? (char? any/c)) (λ (x) #f))
  (ctest #t contract-first-order-passes? (->* (integer?) boolean? (char? any/c)) (λ x #f))
  
  (ctest #t contract-first-order-passes? (->d integer? boolean? (lambda (x y) char?)) (λ (x y) x))
  (ctest #f contract-first-order-passes? (->d integer? boolean? (lambda (x y) char?)) (λ (x) x))
  (ctest #f contract-first-order-passes? (->d integer? boolean? (lambda (x y) char?)) (λ (x y z) x))

  (ctest #t contract-first-order-passes? (listof integer?) (list 1))
  (ctest #f contract-first-order-passes? (listof integer?) #f)

  (ctest #t contract-first-order-passes? (vector-immutableof integer?) (vector->immutable-vector (vector 1)))
  (ctest #f contract-first-order-passes? (vector-immutableof integer?) 'x)
  (ctest #f contract-first-order-passes? (vector-immutableof integer?) '())
  
  (ctest #t contract-first-order-passes? (promise/c integer?) (delay 1))
  (ctest #f contract-first-order-passes? (promise/c integer?) 1)
  
  (ctest #t contract-first-order-passes? (->d* (integer? boolean?) (lambda (x y) char?)) (λ (x y) #t))
  (ctest #f contract-first-order-passes? (->d* (integer? boolean?) (lambda (x y) char?)) (λ (x) #t))
  (ctest #f contract-first-order-passes? (->d* (integer? boolean?) (lambda (x y) char?)) (λ (x y z) #t))

  (ctest #t contract-first-order-passes? 
        (->d* (integer? boolean?) any/c (lambda (x y . z) char?))
        (λ (x y . z) z))
  (ctest #t contract-first-order-passes? 
        (->d* (integer? boolean?) any/c (lambda (x y . z) char?))
        (λ (y . z) z))
  (ctest #t contract-first-order-passes? 
        (->d* (integer? boolean?) any/c (lambda (x y . z) char?))
        (λ z z))
  (ctest #f contract-first-order-passes? 
        (->d* (integer? boolean?) any/c (lambda (x y . z) char?))
        (λ (x y z . w) 1))
  (ctest #f contract-first-order-passes? 
        (->d* (integer? boolean?) any/c (lambda (x y . z) char?))
        (λ (x y) 1))
  
  (ctest #t contract-first-order-passes? (->r ((x number?)) number?) (λ (x) 1))
  (ctest #f contract-first-order-passes? (->r ((x number?)) number?) (λ (x y) 1))
  (ctest #f contract-first-order-passes? (->r ((x number?)) number?) (λ () 1))
  (ctest #t contract-first-order-passes? (->r ((x number?)) number?) (λ args 1))
  
  (ctest #t contract-first-order-passes? (->pp ((x number?)) #t number? blech #t) (λ (x) 1))
  (ctest #f contract-first-order-passes? (->pp ((x number?)) #t number? blech #t) (λ () 1))
  (ctest #t contract-first-order-passes? (->pp ((x number?)) #t number? blech #t) (λ (x . y) 1))
  
  (ctest #f contract-first-order-passes? 
        (case-> (-> integer? integer?)
                (-> integer? integer? integer?))
        (λ () 1))
  (ctest #f contract-first-order-passes? 
        (case-> (-> integer? integer?)
                (-> integer? integer? integer?))
        (λ (x) 1))
  (ctest #f contract-first-order-passes? 
        (case-> (-> integer? integer?)
                (-> integer? integer? integer?))
        (λ (x y) 1))
  (ctest #f contract-first-order-passes? 
        (case->)
        1)
  
  (ctest #t contract-first-order-passes? 
        (case->)
        (case-lambda))
  
  (ctest #t contract-first-order-passes? 
        (case-> (-> integer? integer?)
                (-> integer? integer? integer?))
        (case-lambda [(x) x] [(x y) x]))
  (ctest #t contract-first-order-passes? 
        (case-> (-> integer? integer?)
                (-> integer? integer? integer?))
        (case-lambda [() 1] [(x) x] [(x y) x]))
  (ctest #t contract-first-order-passes? 
        (case-> (-> integer? integer?)
                (-> integer? integer? integer?))
        (case-lambda [() 1] [(x) x] [(x y) x] [(x y z) x]))
  
  (ctest #t contract-first-order-passes? (and/c (-> positive? positive?) (-> integer? integer?)) (λ (x) x))
  (ctest #t contract-first-order-passes? (and/c (-> positive? positive?) (-> integer? integer?)) values)
  (ctest #f contract-first-order-passes? (and/c (-> integer?) (-> integer? integer?)) (λ (x) x))
  
  (ctest #t contract-first-order-passes? 
        (cons/c boolean? (-> integer? integer?))
        (list* #t (λ (x) x)))
  (ctest #f contract-first-order-passes? 
        (cons/c boolean? (-> integer? integer?))
        (list* 1 2))
  
  (ctest #f contract-first-order-passes? (flat-rec-contract the-name) 1)
  
  (ctest #f contract-first-order-passes? 
         (object-contract (m (-> integer? integer?)))
         (new object%))
  (ctest #f contract-first-order-passes? 
         (object-contract (m (-> integer? integer?)))
         1)
  
  (ctest #t contract-first-order-passes?
         (couple/c any/c any/c) 
         (make-couple 1 2))
  
  (ctest #f contract-first-order-passes?
         (couple/c any/c any/c) 
         2)
  
  (ctest #t contract-first-order-passes?
         (couple/dc [hd any/c] [tl any/c]) 
         (make-couple 1 2))
  
  (ctest #f contract-first-order-passes?
         (couple/dc [hd any/c] [tl any/c]) 
         1)
  
  (ctest #t contract-first-order-passes?
         (couple/dc [hd any/c] [tl (hd) any/c]) 
         (make-couple 1 2))
  
  (ctest #f contract-first-order-passes?
         (couple/dc [hd any/c] [tl (hd) any/c]) 
         1)

  (ctest #t contract-first-order-passes? (or/c (-> (>=/c 5) (>=/c 5)) boolean?) #t)
  (ctest #t contract-first-order-passes? (or/c (-> (>=/c 5) (>=/c 5)) boolean?) (λ (x) x))
  (ctest #f contract-first-order-passes? (or/c (-> (>=/c 5) (>=/c 5)) boolean?) 'x)

  (ctest #t contract-first-order-passes? 
        (or/c (-> integer? integer? integer?)
              (-> integer? integer?))
        (λ (x) x))
  (ctest #t contract-first-order-passes? 
        (or/c (-> integer? integer? integer?)
              (-> integer? integer?))
        (λ (x y) x))
  (ctest #f contract-first-order-passes? 
        (or/c (-> integer? integer? integer?)
              (-> integer? integer?))
        (λ () x))
  (ctest #f contract-first-order-passes? 
        (or/c (-> integer? integer? integer?)
              (-> integer? integer?))
        1)
  
  (test-name '(or/c) (or/c))
  (test-name '(or/c integer? gt0?) (or/c integer? (let ([gt0? (lambda (x) (> x 0))]) gt0?)))
  (test-name '(or/c integer? boolean?)
             (or/c (flat-contract integer?)
                   (flat-contract boolean?)))
  (test-name '(or/c (-> (>=/c 5) (>=/c 5)) boolean?)
             (or/c (-> (>=/c 5) (>=/c 5)) boolean?))
  (test-name '(or/c boolean? (-> (>=/c 5) (>=/c 5)))
             (or/c boolean? (-> (>=/c 5) (>=/c 5))))

  
  (ctest 1
        length
        (let ([f (contract (-> integer? any)
                           (lambda (x) 
                             (with-continuation-mark 'x 'x
                               (continuation-mark-set->list (current-continuation-marks) 'x)))
                           'pos
                           'neg)])
          (with-continuation-mark 'x 'x
            (f 1))))
  
  (ctest 2
        length
        (let ([f (contract (-> integer? list?)
                           (lambda (x) 
                             (with-continuation-mark 'x 'x
                               (continuation-mark-set->list (current-continuation-marks) 'x)))
                           'pos
                           'neg)])
          (with-continuation-mark 'x 'x
            (f 1))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; provide/contract tests
  ;; (at the end, because they are slow w/out .zo files)
  ;;
  
  (test/spec-passed
   'provide/contract1
   '(let ()
      (eval '(module contract-test-suite1 mzscheme
                (require mzlib/contract)
                (define x 1)
                (provide/contract (x integer?))))
      (eval '(require 'contract-test-suite1))
      (eval 'x)))
  
  (test/spec-passed
   'provide/contract2
   '(let ()
      (eval '(module contract-test-suite2 mzscheme
                (require mzlib/contract)
                (provide/contract)))
      (eval '(require 'contract-test-suite2))))
  
  (test/spec-failed
   'provide/contract3
   '(let ()
      (eval '(module contract-test-suite3 mzscheme
               (require mzlib/contract)
               (define x #f)
               (provide/contract (x integer?))))
      (eval '(require 'contract-test-suite3))
      (eval 'x))
   "'contract-test-suite3")
  
  (test/spec-passed
   'provide/contract4
   '(begin
      (eval '(module contract-test-suite4 mzscheme
               (require mzlib/contract)
               (define-struct s (a))
               (provide/contract (struct s ((a any/c))))))
      (eval '(require 'contract-test-suite4))
      (eval '(list (make-s 1)
                   (s-a (make-s 1))
                   (s? (make-s 1))
                   (set-s-a! (make-s 1) 2)))))
  
  (test/spec-passed
   'provide/contract4-b
   '(begin
      (eval '(module contract-test-suite4-b mzscheme
               (require mzlib/contract)
               (define-struct s (a))
               (provide/contract (struct s ((a any/c))))))
      (eval '(require 'contract-test-suite4-b))
      (eval '(list (make-s 1)
                   (s-a (make-s 1))
                   (s? (make-s 1))))))
  
  (test/spec-passed/result
   'provide/contract4-c
   '(begin
      (eval '(module contract-test-suite4-c mzscheme
               (require mzlib/contract)
               (define-struct s (a b))
               (provide/contract (struct s ((a any/c) (b any/c))))))
      (eval '(require 'contract-test-suite4-c))
      (eval '(let ([an-s (make-s 1 2)])
               (list (s-a an-s)
                     (s-b an-s)
                     (begin (set-s-a! an-s 3)
                            (s-a an-s))
                     (begin (set-s-b! an-s 4)
                            (s-b an-s))))))
   
   (list 1 2 3 4))
  
  (test/spec-passed
   'provide/contract5
   '(begin
      (eval '(module contract-test-suite5 mzscheme
               (require mzlib/contract)
               (define-struct s (a))
               (define-struct t (a))
               (provide/contract (struct s ((a any/c)))
                                 (struct t ((a any/c))))))
      (eval '(require 'contract-test-suite5))
      (eval '(list (make-s 1)
                   (s-a (make-s 1))
                   (s? (make-s 1))
                   (make-t 1)
                   (t-a (make-t 1))
                   (t? (make-t 1))))))
  
  (test/spec-passed
   'provide/contract6
   '(begin
      (eval '(module contract-test-suite6 mzscheme
               (require mzlib/contract)
               (define-struct s (a))
               (provide/contract (struct s ((a any/c))))))
      (eval '(require 'contract-test-suite6))
      (eval '(define-struct (t s) ()))))
  
  (test/spec-passed
   'provide/contract6b
   '(begin
      (eval '(module contract-test-suite6b mzscheme
               (require mzlib/contract)
               (define-struct s_ (a))
               (provide/contract (struct s_ ((a any/c))))))
      (eval '(require 'contract-test-suite6b))
      (eval '(module contract-test-suite6b2 mzscheme
               (require 'contract-test-suite6b)
               (require mzlib/contract)
               (define-struct (t_ s_) (b))
               (provide s_-a)
               (provide/contract (struct (t_ s_) ((a any/c) (b any/c))))))
      (eval '(require 'contract-test-suite6b2))
      (eval '(define-struct (u_ t_) ()))
      (eval '(s_-a (make-u_ 1 2)))))
  
  (test/spec-passed
   'provide/contract7
   '(begin
      (eval '(module contract-test-suite7 mzscheme
               (require mzlib/contract)
               (define-struct s (a b))
               (define-struct (t s) (c d))
               (provide/contract 
                (struct s ((a any/c) (b any/c)))
                (struct (t s) ((a any/c) (b any/c) (c any/c) (d any/c))))))
      (eval '(require 'contract-test-suite7))
      (eval '(let ([x (make-t 1 2 3 4)])
               (s-a x)
               (s-b x)
               (t-c x)
               (t-d x)
               (void)))))
  
  (test/spec-passed
   'provide/contract8
   '(begin
      (eval '(module contract-test-suite8 mzscheme
               (require mzlib/contract)
               (define-struct i-s (contents))
               (define (w-f-s? x) #t)
               (provide/contract 
                (struct i-s ((contents (flat-named-contract "integer-set-list" w-f-s?)))))))
      (eval '(require 'contract-test-suite8))
      (eval '(i-s-contents (make-i-s 1)))))
   
  (test/spec-passed
   'provide/contract9
   '(begin
      (eval '(module contract-test-suite9 mzscheme
               (require mzlib/contract)
               (define the-internal-name 1)
               (provide/contract (rename the-internal-name the-external-name integer?))
               (+ the-internal-name 1)))
      (eval '(require 'contract-test-suite9))
      (eval '(+ the-external-name 1))))
  
  (test/spec-passed
   'provide/contract10
   '(begin
      (eval '(module pc10-m mzscheme
               (require mzlib/contract)
               (define-struct s (a b) (make-inspector))
               (provide/contract (struct s ((a number?) (b number?))))))
      (eval '(module pc10-n mzscheme
               (require mzlib/struct
                        'pc10-m)
               (print-struct #t)
               (copy-struct s 
                            (make-s 1 2)
                            [s-a 3])))
      (eval '(require 'pc10-n))))
  
  (test/spec-passed
   'provide/contract11
   '(begin
      (eval '(module pc11-m mzscheme
               (require mzlib/contract)
               (define x 1)
               (provide/contract [rename x y integer?]
                                 [rename x z integer?])))
      (eval '(module pc11-n mzscheme
               (require 'pc11-m)
               (+ y z)))
      (eval '(require 'pc11-n))))
  
  ;; this test is broken, not sure why
  #|
  (test/spec-failed
   'provide/contract11b
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module pc11b-m mzscheme
               (require mzlib/contract)
               (define-struct s (a b) (make-inspector))
               (provide/contract (struct s ((a number?) (b number?))))))
      (eval '(module pc11b-n mzscheme
               (require mzlib/struct
                        m)
               (print-struct #t)
               (copy-struct s 
                            (make-s 1 2)
                            [s-a #f])))
      (eval '(require 'pc11b-n)))
   "'n")
|#
  
  (test/spec-passed
   'provide/contract12
   '(begin
      (eval '(module pc12-m mzscheme
               (require mzlib/contract)
               (define-struct (exn2 exn) ())
               (provide/contract (struct (exn2 exn) ((message any/c) (continuation-marks any/c))))))
      (eval '(require 'pc12-m))))
  
  (test/spec-passed/result
   'provide/contract13
   '(begin
      (eval '(module pc13-common-msg-structs mzscheme
               (require mzlib/contract)
               (define-struct register (name type) (make-inspector))
               (provide/contract (struct register ([name any/c] [type any/c])))))
      
      (eval '(require 'pc13-common-msg-structs))
      (eval '(require (lib "plt-match.rkt")))
      (eval '(match (make-register 1 2)
               [(struct register (name type))
                (list name type)])))
   (list 1 2))
  
  (test/spec-passed
   'provide/contract14
   '(begin
      (eval '(module pc14-test1 mzscheme
               (require mzlib/contract)
               
               (define-struct type (flags))
               (define-struct (type:ptr type) (type))
               
               (provide/contract
                (struct type
                        ([flags (listof string?)]))
                
                (struct (type:ptr type)
                        ([flags (listof string?)] [type type?])))))

      (eval '(module pc14-test2 mzscheme
               (require mzlib/plt-match)
               (require 'pc14-test1)
               (match (make-type:ptr '() (make-type '()))
                 [(struct type:ptr (flags type)) #f])))
      (eval '(require 'pc14-test2))))
  
  ;; make sure unbound identifier exception is raised.
  (contract-error-test
   #'(begin
       (eval '(module pos mzscheme
                (require mzlib/contract)
                (provide/contract [i any/c]))))
   exn:fail:syntax?)
  
  ;; provide/contract should signal errors without requiring a reference to the variable
  ;; this test is bogus, because provide/contract'd variables can be set!'d.
  (test/spec-failed
   'provide/contract15
   '(begin
      (eval '(module pos mzscheme
               (require mzlib/contract)
               (define i #f)
               (provide/contract [i integer?])))
      (eval '(require 'pos)))
   "'pos")
  
  ;; this is really a positive violation, but name the module `neg' just for an addl test
  (test/spec-failed
   'provide/contract16
   '(begin
      (eval '(module neg mzscheme
               (require mzlib/contract)
               (define i #f)
               (provide/contract [i integer?])))
      (eval '(require 'neg)))
   "'neg")
  
  ;; this test doesn't pass yet ... waiting for support from define-struct
  
  #;
  (test/neg-blame
   'provide/contract17
   '(begin
      (eval '(module pos mzscheme
               (require mzlib/contract)
               (define-struct s (a))
               (provide/contract [struct s ((a integer?))])))
      (eval '(module neg mzscheme
               (require 'pos)
               (define-struct (t s) ())
               (make-t #f)))
      (eval '(require 'neg))))
  
  (test/spec-passed
   'provide/contract18
   '(begin
      (eval '(module pc18-pos mzscheme
               (require mzlib/contract)
               (define-struct s ())
               (provide/contract [struct s ()])))
      (eval '(require 'pc18-pos))
      (eval '(make-s))))

  (test/spec-passed/result
   'provide/contract19
   '(begin
      (eval '(module pc19-a mzscheme
               (require mzlib/contract)
               (define-struct a (x))
               (provide/contract [struct a ([x number?])])))

      (eval '(module pc19-b mzscheme
               (require 'pc19-a
                        mzlib/contract)
               (define-struct (b a) (y))
               (provide/contract [struct (b a) ([x number?] [y number?])])))

      (eval '(module pc19-c mzscheme
               (require 'pc19-b
                        mzlib/contract)
               
               (define-struct (c b) (z))
               (provide/contract [struct (c b) ([x number?] [y number?] [z number?])])))

      (eval' (module pc19-d mzscheme
               (require 'pc19-a 'pc19-c)
               (define pc19-ans (a-x (make-c 1 2 3)))
               (provide pc19-ans)))
      
      (eval '(require 'pc19-d))
      (eval 'pc19-ans))
   1)

  ;; test that unit & contract don't collide over the name `struct'
  (test/spec-passed
   'provide/contract20
   '(eval '(module tmp mzscheme
             (require mzlib/contract
                      mzlib/unit)
             
             (define-struct s (a b))
             
             (provide/contract
              [struct s ([a number?]
                         [b symbol?])]))))
  
  (test/spec-passed
   'provide/contract21
   '(begin
      (eval '(module provide/contract21a mzscheme
               (require mzlib/contract)
               (provide/contract [f integer?])
               (define f 1)))
      (eval '(module provide/contract21b mzscheme
               (require (for-syntax 'provide/contract21a)
                        (for-syntax mzscheme))
               (define-syntax (unit-body stx)
                 f f
                 #'1)))))
  
  (test/spec-passed
   'provide/contract22
   '(begin
      (eval '(module provide/contract22a mzscheme
               (require mzlib/contract)
               (provide/contract [make-bound-identifier-mapping integer?])
               (define make-bound-identifier-mapping 1)))
      (eval '(module provide/contract22b mzscheme
               (require (for-syntax 'provide/contract22a)
                        (for-syntax mzscheme))
               
               (define-syntax (unit-body stx)
                 make-bound-identifier-mapping)
               
               (define-syntax (f stx)
                 make-bound-identifier-mapping)))))
  
  (test/spec-passed
   'provide/contract23
   '(begin
      (eval '(module provide/contract23a mzscheme
               (require mzlib/contract)
               (provide/contract [f integer?])
               (define f 3)))
      
      (eval '(module provide/contract23b mzscheme
               (require 'provide/contract23a)
               (#%expression f)
               f))
      
      (eval '(require 'provide/contract23b))))
  
  (test/spec-passed
   'provide/contract24
   '(begin
      (eval '(module provide/contract24 mzscheme
               (require (prefix c: mzlib/contract))
               (c:case-> (c:-> integer? integer?)
                         (c:-> integer? integer? integer?))))))
  
  ;; tests that contracts pick up the #%app from the context
  ;; instead of always using the mzscheme #%app.
  (test/spec-passed
   'provide/contract25
   '(begin
      (eval '(module provide/contract25a mzscheme
               (require mzlib/contract)
               (provide/contract [seventeen integer?])
               (define seventeen 17)))
      (eval '(module provide/contract25b mzscheme
               (require 'provide/contract25a)
               (let-syntax ([#%app (syntax-rules ()
                                     [(#%app e ...) (list e ...)])])
                 (seventeen 18))))
      (eval '(require 'provide/contract25b))))
  
  (test/spec-passed/result
   'provide/contract26
   '(begin
      (eval '(module provide/contract26 mzscheme
               (require mzlib/contract)
               (define-struct pc26-s (a))
               (provide/contract (struct pc26-s ((a integer?))))))
      (eval '(require 'provide/contract26))
      (eval '(pc26-s-a (make-pc26-s 1))))
   1)

  (contract-error-test
   #'(begin
       (eval '(module pce1-bug mzscheme
                (require mzlib/contract)
                (define the-defined-variable1 'five)
                (provide/contract [the-defined-variable1 number?])))
       (eval '(require 'pce1-bug)))
   (λ (x)
     (and (exn? x)
          (regexp-match #rx"the-defined-variable1:" (exn-message x)))))
  
  (contract-error-test
   #'(begin
       (eval '(module pce2-bug mzscheme
                (require mzlib/contract)
                (define the-defined-variable2 values)
                (provide/contract [the-defined-variable2 (-> number? any)])))
       (eval '(require 'pce2-bug))
       (eval '(the-defined-variable2 #f)))
   (λ (x)
     (and (exn? x)
          (regexp-match #rx"the-defined-variable2:" (exn-message x)))))
  
  (contract-error-test
   #'(begin
       (eval '(module pce3-bug mzscheme
                (require mzlib/contract)
                (define the-defined-variable3 (λ (x) #f))
                (provide/contract [the-defined-variable3 (-> any/c number?)])))
       (eval '(require 'pce3-bug))
       (eval '(the-defined-variable3 #f)))
   (λ (x)
     (and (exn? x)
          (regexp-match #rx"the-defined-variable3:" (exn-message x)))))
  
  (contract-error-test
   #'(begin
       (eval '(module pce4-bug mzscheme
                (require mzlib/contract)
                (define the-defined-variable4 (λ (x) #f))
                (provide/contract [the-defined-variable4 (-> any/c number?)])))
       (eval '(require 'pce4-bug))
       (eval '((if #t the-defined-variable4 the-defined-variable4) #f)))
   (λ (x)
     (and (exn? x)
          (regexp-match #rx"the-defined-variable4:" (exn-message x)))))

  (contract-error-test
   #'(begin
       (eval '(module pce5-bug mzscheme
                (require mzlib/contract)
                
                (define-struct bad (a b))
                
                (provide/contract
                 [struct bad ((string? a) (string? b))])))
       (eval '(require 'pce5-bug)))
   (λ (x)
     (and (exn? x)
          (regexp-match #rx"expected field name to be b, but found string?" (exn-message x)))))
  
  (contract-error-test
   #'(begin
       (eval '(module pce6-bug mzscheme
                (require mzlib/contract)
                
                (define-struct bad-parent (a))
                (define-struct (bad bad-parent) (b))
                
                (provide/contract
                 [struct bad ((a string?) (string? b))])))
       (eval '(require 'pce6-bug)))
   (λ (x)
     (and (exn? x)
          (regexp-match #rx"expected field name to be b, but found string?" (exn-message x)))))
  
  (contract-eval
   `(,test
     'pos
     (compose blame-positive exn:fail:contract:blame-object)
     (with-handlers ((void values)) (contract not #t 'pos 'neg))))
  
  (report-errs)
  
))
