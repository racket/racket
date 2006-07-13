(load-relative "loadtest.ss")
(require (lib "contract.ss")
	 (lib "class.ss")
         (lib "etc.ss"))
  
(Section 'contract)

(parameterize ([error-print-width 200])
(let ()
  ;; test/spec-passed : symbol sexp -> void
  ;; tests a passing specification
  (define (test/spec-passed name expression)
    (printf "testing: ~s\n" name)
    (test (void)
          (let ([for-each-eval (lambda (l) (for-each eval l))]) for-each-eval)
          (list expression '(void))))
  
  (define (test/spec-passed/result name expression result)
    (printf "testing: ~s\n" name)
    (test result
          eval
          expression))
  
  (define (test/spec-failed name expression blame)
    (cond
      [(equal? blame "pos")
       (test/pos-blame name expression)]
      [(equal? blame "neg")
       (test/neg-blame name expression)]
      [else
       (let ()
         (define (has-proper-blame? msg)
           (equal?
            blame
            (cond
              [(regexp-match #rx"^([^ ]*) broke" msg) => cadr]
              [else (format "no blame in error message: \"~a\"" msg)])))
         (printf "testing: ~s\n" name)
         (thunk-error-test 
          (lambda () (eval expression))
          (datum->syntax-object #'here expression)
          (lambda (exn)
            (and (exn? exn)
                 (has-proper-blame? (exn-message exn))))))]))
  
  (define (test/pos-blame name expression)
    (define (has-pos-blame? exn)
      (and (exn? exn)
           (and (regexp-match #rx"pos broke" (exn-message exn)))))
    (printf "testing: ~s\n" name)
    (thunk-error-test 
     (lambda () (eval expression))
     (datum->syntax-object #'here expression)
     has-pos-blame?))
  
  (define (test/neg-blame name expression)
    (define (has-neg-blame? exn)
      (and (exn? exn)
           (and (regexp-match #rx"neg broke" (exn-message exn)))))
    (printf "testing: ~s\n" name)
    (thunk-error-test 
     (lambda () (eval expression))
     (datum->syntax-object #'here expression)
     has-neg-blame?))
  
  (define (test/well-formed stx)
    (test (void) 
          (let ([expand/ret-void (lambda (x) (expand x) (void))]) expand/ret-void)
          stx))
  
  (define (test/no-error sexp)
    (test (void)
          eval
          `(begin ,sexp (void))))
  
  (define (test-flat-contract contract pass fail)
    (let ([name (if (pair? contract)
                    (car contract)
                    contract)])
      (test #t flat-contract? (eval contract))
      (test/spec-failed (format "~a fail" name)
                        `(contract ,contract ',fail 'pos 'neg)
                        "pos")
      (test/spec-passed/result
       (format "~a pass" name)
       `(contract ,contract ',pass 'pos 'neg)
       pass)))

  (define (test-name name contract)
    (test name contract-name contract))
  
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
   '((contract (->r ([x number?]) (<=/c x)) (lambda (x) (+ x 1)) 'pos 'neg) #f))
  
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
   '((contract (->r ([x number?]) rest any/c (<=/c x)) (lambda (x . y) (+ x 1)) 'pos 'neg) #f))
  
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
   '((contract (->r ([x number?]) (<=/c x)) (lambda (x) (+ x 1)) 'pos 'neg) #f))
  
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
   '((contract (->r ([x number?]) rest any/c (<=/c x)) (lambda (x . y) (+ x 1)) 'pos 'neg) #f))
  
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
  
  (test 1 'or/c-not-error-early 
        (begin (or/c (-> integer? integer?) (-> boolean? boolean?))
               1))
  (error-test #'(contract (or/c (-> integer? integer?) (-> boolean? boolean?))
                          (λ (x) x)
                          'pos
                          'neg)
              exn:fail?)
  
  (test
   '(1 2)
   'or/c-ordering
   (let ([x '()])
     (contract (or/c (lambda (y) (set! x (cons 2 x)) #f) (lambda (y) (set! x (cons 1 x)) #t))
               'anything
               'pos
               'neg)
     x))
  
  (test
   '(2)
   'or/c-ordering2
   (let ([x '()])
     (contract (or/c (lambda (y) (set! x (cons 2 x)) #t) (lambda (y) (set! x (cons 1 x)) #t))
               'anything
               'pos
               'neg)
     x))
  
  (test
   '(1 2)
   'and/c-ordering
   (let ([x '()])
     (contract (and/c (lambda (y) (set! x (cons 2 x)) #t) (lambda (y) (set! x (cons 1 x)) #t))
               'anything
               'pos
               'neg)
     x))
  
  (test
   (reverse '(1 3 4 2))
   'ho-and/c-ordering
   (let ([x '()])
     ((contract (and/c (-> (lambda (y) (set! x (cons 1 x)) #t)
                           (lambda (y) (set! x (cons 2 x)) #t))
                       (-> (lambda (y) (set! x (cons 3 x)) #t)
                           (lambda (y) (set! x (cons 4 x)) #t)))
                (λ (x) x)
                'pos
                'neg)
      1)
     x))
  
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
   "")
  
  (test/spec-failed
   'define/contract5
   '(let ()
      (define/contract i (-> integer? integer?) (lambda (x) (i #t)))
      (i 1))
   "")
  
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
               (require (lib "contract.ss"))
               (define/contract x string? "a")
               x))
      (eval '(require contract-test-suite-define1))))
  

  
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
   '(contract (object-contract (field x integer?))
              (new (class object% (field [x #t]) (super-new)))
              'pos
              'neg))
  
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
   '(contract (object-contract (field x boolean?) (field y boolean?))
              (new (class object% (field [x #t] [y 'x]) (super-new)))
              'pos
              'neg))
  
  (test/pos-blame
   'object-contract/field5
   '(contract (object-contract (field x symbol?) (field y symbol?))
              (new (class object% (field [x #t] [y 'x]) (super-new)))
              'pos
              'neg))
  
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
  
  (test "procedure m method: expects 1 argument, given 2: 1 2"
        'wrong-method-arity-error-message
        (with-handlers ([exn:fail? exn-message])
          (send (contract (object-contract [m (integer? . -> . integer?)])
                          (new (class object% (define/public (m x) x) (super-new)))
                          'pos
                          'neg)
                m
                1
                2)))
        
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; tests object utilities to be sure wrappers work right
  ;;
  
  (let* ([o1 (new object%)]
         [o2 (contract (object-contract) o1 'pos 'neg)])
    (test #t object=? o1 o1)
    (test #f object=? o1 (new object%))
    (test #t object=? o1 o2)
    (test #t object=? o2 o1)
    (test #f object=? (new object%) o2))
  
  (test #t method-in-interface? 'm 
        (object-interface 
         (contract
          (object-contract (m (integer? . -> . integer?)))
          (new (class object% (define/public (m x) x) (super-new)))
          'pos
          'neg)))
  
  (let* ([i<%> (interface ())]
         [c% (class* object% (i<%>) (super-new))]
         [o (new c%)])
    (test #t is-a? o i<%>)
    (test #t is-a? o c%)
    (test #t is-a? (contract (object-contract) o 'pos 'neg) i<%>)
    (test #t is-a? (contract (object-contract) o 'pos 'neg) c%))
  
  (let ([c% (parameterize ([current-inspector (make-inspector)])
              (class object% (super-new)))])
    (test (list c% #f) 
          'object-info
          (call-with-values 
           (lambda () (object-info (contract (object-contract) (new c%) 'pos 'neg)))
           list)))

  ;; object->vector tests
  (let* ([obj
          (parameterize ([current-inspector (make-inspector)])
            (new (class object% (field [x 1] [y 2]) (super-new))))]
         [vec (object->vector obj)])
    (test vec
          object->vector
          (contract (object-contract (field x integer?) (field y integer?))
                    obj
                    'pos
                    'neg)))
  
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
   '(let ([ct (contract (list-immutableof (boolean? . -> . boolean?)) 
                        #f 
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/pos-blame
   'immutable2
   '(let ([ct (contract (list-immutableof (boolean? . -> . boolean?)) 
                        (list (lambda (x) x)) 
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/neg-blame
   'immutable3
   '(let ([ct (contract (list-immutableof (number? . -> . boolean?)) 
                        (list-immutable (lambda (x) 1)) 
                        'pos
                        'neg)])
      ((car ct) #f)))
  
  (test/pos-blame
   'immutable4
   '(let ([ct (contract (list-immutableof (number? . -> . boolean?)) 
                        (list-immutable (lambda (x) 1)) 
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/spec-passed
   'immutable5
   '(let ([ct (contract (list-immutableof (number? . -> . boolean?)) 
                        (list-immutable (lambda (x) #t)) 
                        'pos
                        'neg)])
      ((car ct) 1)))
  

  (test/pos-blame
   'immutable6
   '(contract (cons-immutable/c (boolean? . -> . boolean?) (boolean? . -> . boolean?)) 
              #f 
              'pos
              'neg))
  
  (test/pos-blame
   'immutable7
   '(contract (cons-immutable/c (boolean? . -> . boolean?) (boolean? . -> . boolean?)) 
              (cons (lambda (x) x) (lambda (x) x))
              'pos
              'neg))
  
  (test/neg-blame
   'immutable8
   '(let ([ct (contract (cons-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons-immutable (lambda (x) 1) (lambda (x) 1))
                        'pos
                        'neg)])
      ((car ct) #f)))
  
  (test/neg-blame
   'immutable9
   '(let ([ct (contract (cons-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons-immutable (lambda (x) 1) (lambda (x) 1))
                        'pos
                        'neg)])
      ((cdr ct) #f)))
  
  (test/pos-blame
   'immutable10
   '(let ([ct (contract (cons-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons-immutable (lambda (x) 1) (lambda (x) 1)) 
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/pos-blame
   'immutable11
   '(let ([ct (contract (cons-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons-immutable (lambda (x) 1) (lambda (x) 1)) 
                        'pos
                        'neg)])
      ((cdr ct) 1)))
  
  (test/spec-passed
   'immutable12
   '(let ([ct (contract (cons-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons-immutable (lambda (x) #t) (lambda (x) #t)) 
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/spec-passed
   'immutable13
   '(let ([ct (contract (cons-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons-immutable (lambda (x) #t) (lambda (x) #t)) 
                        'pos
                        'neg)])
      ((cdr ct) 1)))
  
  (test/spec-passed/result
   'immutable14
   '(contract (cons-immutable/c number? boolean?) 
              (cons-immutable 1 #t) 
              'pos
              'neg)
   (cons-immutable 1 #t))
  
  (test/pos-blame
   'immutable15
   '(contract (list-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              #f
              'pos
              'neg))
  
  (test/pos-blame
   'immutable16
   '(contract (list-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              (list (lambda (x) #t) (lambda (x) #t)) 
              'pos
              'neg))
  
  (test/pos-blame
   'immutable17
   '(contract (list-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              (list-immutable (lambda (x) #t)) 
              'pos
              'neg))
  
  (test/pos-blame
   'immutable18
   '(contract (list-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              (list-immutable (lambda (x) #t) (lambda (x) #t) (lambda (x) #t)) 
              'pos
              'neg))
  
  (test/spec-passed
   'immutable19
   '(let ([ctc (contract (list-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                         (list-immutable (lambda (x) #t) (lambda (x) #t)) 
                         'pos
                         'neg)])
      (for-each (lambda (x) (x 1)) ctc)))
  
  (test/spec-passed/result
   'immutable20
   '(let ([ctc (contract (list-immutable/c number?) 
                         (list-immutable 1) 
                         'pos
                         'neg)])
      (immutable? ctc))
   #t)

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
   (letrec ([ctc (-> integer? (recursive-contract ctc))])
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
  
  
  (test/pos-blame 'd-c-s1
                  '(let ()
                     (define-contract-struct couple (hd tl))
                     (contract (couple/c any/c any/c) 1 'pos 'neg)))
  
  
  (test/spec-passed 'd-c-s2
                    '(let ()
                       (define-contract-struct couple (hd tl))
                       (contract (couple/c any/c any/c) (make-couple 1 2) 'pos 'neg)))
  (test/spec-passed 'd-c-s3
                    '(let ()
                       (define-contract-struct couple (hd tl))
                       (contract (couple/c number? number?)
                                 (make-couple 1 2)
                                 'pos 'neg)))
  (test/spec-passed 'd-c-s4
                    '(let ()
                       (define-contract-struct couple (hd tl))
                       (couple-hd 
                        (contract (couple/c number? number?)
                                  (make-couple 1 2)
                                  'pos 'neg))))
  (test/spec-passed 'd-c-s5
                    '(let ()
                       (define-contract-struct couple (hd tl))
                       (couple-tl
                        (contract (couple/c number? number?)
                                  (make-couple 1 2)
                                  'pos 'neg))))
  
  (test/pos-blame 
   'd-c-s6 
   '(let ()
      (define-contract-struct couple (hd tl))
      (couple-tl
       (contract (couple/c number?
                           number?)
                 (make-couple #f 2)
                 'pos 'neg))))
  (test/pos-blame
   'd-c-s7
   '(let ()
      (define-contract-struct couple (hd tl))
      (couple-hd
       (contract (couple/c number? number?)
                 (make-couple #f 2)
                 'pos 'neg))))
  
  (test/pos-blame 
   'd-c-s8
   '(let ()
      (define-contract-struct couple (hd tl))
      (contract (couple/dc [hd any/c] [tl any/c])
                1
                'pos 'neg)))
  
  (test/pos-blame 
   'd-c-s9
   '(let ()
      (define-contract-struct couple (hd tl))
      (contract (couple/dc [hd () any/c] [tl () any/c])
                1
                'pos 'neg)))

  
  (test/spec-passed 'd-c-s10
                    '(let ()
                       (define-contract-struct couple (hd tl))
                       (contract (couple/dc [hd any/c] [tl any/c]) (make-couple 1 2)
                                 'pos 'neg)))
  (test/spec-passed 'd-c-s11
                    '(let ()
                       (define-contract-struct couple (hd tl))
                       (contract (couple/dc [hd () any/c] [tl () any/c]) 
                                 (make-couple 1 2)
                                 'pos 'neg)))
  
  (test/spec-passed 'd-c-s12
                    '(let ()
                       (define-contract-struct couple (hd tl))
                       (contract (couple/dc [hd number?]
                                            [tl number?])
                                 (make-couple 1 2)
                                 'pos 'neg)))
  (test/spec-passed 'd-c-s13
                    '(let ()
                       (define-contract-struct couple (hd tl))
                       (couple-hd 
                        (contract (couple/dc [hd number?]
                                             [tl number?])
                                  (make-couple 1 2)
                                  'pos 'neg))))
  (test/spec-passed 'd-c-s14
                    '(let ()
                       (define-contract-struct couple (hd tl))
                       (couple-tl
                        (contract (couple/dc [hd number?]
                                             [tl number?])
                                  (make-couple 1 2)
                                  'pos 'neg))))
  
  
  (test/pos-blame 
   'd-c-s15
   '(let ()
      (define-contract-struct couple (hd tl))
      (couple-hd
       (contract (couple/dc [hd number?]
                            [tl number?])
                 (make-couple #f 2)
                 'pos 'neg))))
  
  (test/pos-blame
   'd-c-s16
   '(let ()
      (define-contract-struct couple (hd tl))
      (couple-tl
       (contract (couple/dc [hd number?]
                            [tl number?])
                 (make-couple #f 2)
                 'pos 'neg))))

  (test/spec-passed
   'd-c-s17
   '(let ()
      (define-contract-struct couple (hd tl))
      (couple-hd
       (contract (couple/dc [hd number?]
                            [tl (hd) (>=/c hd)])
                 (make-couple 1 2)
                 'pos 'neg))))

  (test/pos-blame
   'd-c-s18
   '(let ()
      (define-contract-struct couple (hd tl))
      (couple-hd
       (contract (couple/dc [hd number?]
                            [tl (hd) (>=/c hd)])
                 (make-couple 2 1)
                 'pos 'neg))))
  
  (test/spec-passed
   'd-c-s19
   '(let ()
      (define-contract-struct couple (hd tl))
      (couple-tl
       (couple-tl
        (contract (couple/dc [hd number?]
                             [tl (hd)
                                 (let ([hd1 hd])
                                   (couple/dc [hd (>=/c hd1)]
                                              [tl (hd) (>=/c hd)]))])
                  (make-couple 1 (make-couple 2 3))
                  'pos 'neg)))))
  
  (test/pos-blame
   'd-c-s20
   '(let ()
      (define-contract-struct couple (hd tl))
      (couple-tl
       (couple-tl
        (contract (couple/dc [hd number?]
                             [tl (hd)
                                 (let ([hd1 hd])
                                   (couple/dc [hd (>=/c hd1)]
                                              [tl (hd) (>=/c hd1)]))])
                  (make-couple 1 (make-couple 2 0))
                  'pos 'neg)))))
  
  (test/spec-passed
   'd-c-s21
   '(let ()
      (define-contract-struct couple (hd tl))
      
      (couple-hd
       (contract (couple/dc [hd number?]
                            [tl number?])
                 (contract (couple/dc [hd number?]
                                      [tl number?])
                           (make-couple 1 2)
                           'pos 'neg)
                 'pos 'neg))))
  
  (test/spec-passed
   'd-c-s22
   '(let ()
      (define-contract-struct couple (hd tl))
      (couple-hd
       (contract (couple/dc [hd (>=/c 0)]
                            [tl (>=/c 0)])
                 (contract (couple/dc [hd number?]
                                      [tl number?])
                           (make-couple 1 2)
                           'pos 'neg)
                 'pos 'neg))))
  
  (test/pos-blame
   'd-c-s23
   '(let ()
      (define-contract-struct couple (hd tl))
      (couple-hd
       (contract (couple/dc [hd (>=/c 0)]
                            [tl (>=/c 0)])
                 (contract (couple/dc [hd number?]
                                      [tl number?])
                           (make-couple -1 2)
                           'pos 'neg)
                 'pos 'neg))))
  
   (test/pos-blame
    'd-c-s24
    '(let ()
       (define-contract-struct couple (hd tl))
       (couple-hd
        (contract (couple/dc [hd number?]
                             [tl number?])
                  (contract (couple/dc [hd (>=/c 0)]
                                       [tl (>=/c 0)])
                            (make-couple -1 2)
                            'pos 'neg)
                  'pos 'neg))))
  
   (test/pos-blame
    'd-c-s25
    '(let ()
       (define-contract-struct couple (hd tl))
       (couple-hd
        (contract (couple/dc [hd number?]
                             [tl number?])
                  (contract (couple/dc [hd number?]
                                       [tl number?])
                            (contract (couple/dc [hd (>=/c 0)]
                                                 [tl (>=/c 0)])
                                      (make-couple -1 2)
                                      'pos 'neg)
                            'pos 'neg)
                  'pos 'neg))))
  
   (test/pos-blame
    'd-c-s26
    '(let ()
       (define-contract-struct couple (hd tl))
       (couple-hd
        (contract (couple/dc [hd (>=/c 10)]
                             [tl (>=/c 10)])
                  (contract (couple/dc [hd positive?]
                                       [tl positive?])
                            (contract (couple/dc [hd number?]
                                                 [tl number?])
                                      (make-couple 1 2)
                                      'pos 'neg)
                            'pos 'neg)
                  'pos 'neg))))
  

  ;; test caching
  (test/spec-passed 
   'd-c-s27
   '(let ()
      (define-contract-struct couple (hd tl))
      (let ([ctc (couple/c any/c any/c)])
        (couple-hd (contract ctc (contract ctc (make-couple 1 2) 'pos 'neg) 'pos 'neg)))))

  ;; make sure lazy really is lazy
  (test/spec-passed
   'd-c-s28
   '(let ()
      (define-contract-struct couple (hd tl))
      (contract (couple/c number? number?)
                (make-couple #f #f)
                'pos 'neg)))
  
  (test/spec-passed
   'd-c-s29
   '(let ()
      (define-contract-struct couple (hd tl))
      
      (couple-hd
       (contract (couple/c (couple/c number? number?)
                           (couple/c number? number?))
                 (make-couple (make-couple #f #f)
                              (make-couple #f #f))
                 'pos 'neg))))
  
  (test/spec-passed
   'd-c-s30
   '(let ()
      (define-contract-struct couple (hd tl))
      
      (couple-tl
       (contract (couple/c (couple/c number? number?)
                           (couple/c number? number?))
                 (make-couple (make-couple #f #f)
                              (make-couple #f #f))
                 'pos 'neg))))
  
  ;; make sure second accesses work
  (test/spec-passed/result
   'd-c-s31
   '(let ()
      (define-contract-struct couple (hd tl))
      (let ([v (contract (couple/c number? number?)
                         (make-couple 1 2)
                         'pos 'neg)])
        (list (couple-hd v) (couple-hd v))))
   (list 1 1))
  
  (test/pos-blame
   'd-c-s32
   '(let ()
      (define-contract-struct couple (hd tl))
      (let ([v (contract (couple/c number? boolean?)
                         (make-couple 1 2)
                         'pos 'neg)])
        (with-handlers ([void void]) (couple-hd v))
        (couple-hd v))))
  
  (test/pos-blame
   'd-c-s33
   '(let ()
      (define-contract-struct couple (hd tl))
      (let ([v (contract (couple/c number? number?)
                         (make-couple 1 2)
                         'pos 'neg)])
        (couple-hd v)
        (couple-hd v)
        (couple-hd
         (contract (couple/c boolean? boolean?)
                   v
                   'pos 'neg)))))
  
  ;; a related test to the above:
  (test/spec-passed/result
   'd-c-s34
   '(let ()
      (define-contract-struct s (a) (make-inspector))
      (let ([v (contract (s/c number?) (make-s 1) 'pos 'neg)])
        (s-a v)
        (let ([v3 (contract (s/c number?) v 'pos 'neg)])
          (s-a v3))))
   1)
  
  ;; make sure the caching doesn't break the semantics
  (test/pos-blame
   'd-c-s35
   '(let ()
      (define-contract-struct couple (hd tl))
      (let ([v (contract (couple/c any/c
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
          (couple-hd (couple-tl x))))))
  
  (test/spec-passed/result
   'd-c-s36
   '(let ()
      (define-contract-struct couple (hd tl))
      (let ([x (make-couple 1 2)]
            [y (make-couple 1 2)]
            [c1 (couple/dc [hd any/c]
                           [tl (hd) any/c])]
            [c2 (couple/c any/c any/c)])
        (couple-hd (contract c1 x 'pos 'neg))
        (couple-hd (contract c2 x 'pos 'neg))
        (couple-hd (contract c2 y 'pos 'neg))
        (couple-hd (contract c1 y 'pos 'neg))))
   1)
  
  ;; make sure that define-contract-struct contracts can go at the top level
  (test/spec-passed
   'd-c-s37
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(require (lib "contract.ss")))
      (eval '(define-contract-struct couple (hd tl)))
      (eval '(contract-stronger?
              (couple/dc [hd any/c]
                         [tl (hd) any/c])
              (couple/dc [hd any/c]
                         [tl (hd) any/c])))))
  
  ;; test functions inside structs
  
  (test/spec-passed/result
   'd-c-s38
   '(let ()
      (define-contract-struct couple (hd tl))
      (let ([x (make-couple (lambda (x) x) (lambda (x) x))]
            [c (couple/dc [hd (-> integer? integer?)]
                          [tl (hd) any/c])])
        ((couple-hd (contract c x 'pos 'neg)) 1)))
   1)
  
  (test/neg-blame
   'd-c-s39
   '(let ()
      (define-contract-struct couple (hd tl))
      (let ([x (make-couple (lambda (x) x) (lambda (x) x))]
            [c (couple/dc [hd (-> integer? integer?)]
                          [tl (hd) any/c])])
        ((couple-hd (contract c x 'pos 'neg)) #f))))
  
  (test/pos-blame
   'd-c-s40
   '(let ()
      (define-contract-struct couple (hd tl))
      (let ([x (make-couple (lambda (x) #f) (lambda (x) #f))]
            [c (couple/dc [hd (-> integer? integer?)]
                          [tl (hd) any/c])])
        ((couple-hd (contract c x 'pos 'neg)) 1))))
  
  (test/spec-passed/result
   'd-c-s41
   '(let ()
      (define-contract-struct couple (hd tl))
      (let ([x (make-couple 5 (lambda (x) x))]
            [c (couple/dc [hd number?]
                          [tl (hd) (-> (>=/c hd) (>=/c hd))])])
        ((couple-tl (contract c x 'pos 'neg)) 6)))
   6)
  
  (test/pos-blame
   'd-c-s42
   '(let ()
      (define-contract-struct couple (hd tl))
      (let ([x (make-couple 5 (lambda (x) -10))]
            [c (couple/dc [hd number?]
                          [tl (hd) (-> (>=/c hd) (>=/c hd))])])
        ((couple-tl (contract c x 'pos 'neg)) 6))))
  
  (test/neg-blame
   'd-c-s42
   '(let ()
      (define-contract-struct couple (hd tl))
      (let ([x (make-couple 5 (lambda (x) -10))]
            [c (couple/dc [hd number?]
                          [tl (hd) (-> (>=/c hd) (>=/c hd))])])
        ((couple-tl (contract c x 'pos 'neg)) -11))))
  
  
  
  

  
  ;; test the predicate
  (let ()
    (define-contract-struct couple (hd tl))
    (test #t couple? (contract (couple/c any/c any/c) (make-couple 1 2) 'pos 'neg))
    (test #t couple? (make-couple 1 2))
    (test #t couple? (contract (couple/dc [hd any/c] [tl (hd) any/c]) (make-couple 1 2) 'pos 'neg))
    (test #f couple? 1)
    (test #f couple? #f))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                        ;;
  ;;   Flat Contract Tests                                  ;;
  ;;                                                        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (test #t flat-contract? (or/c))
  (test #t flat-contract? (or/c integer? (lambda (x) (> x 0))))
  (test #t flat-contract? (or/c (flat-contract integer?) (flat-contract boolean?)))
  (test #t flat-contract? (or/c integer? boolean?))
  (test-flat-contract '(or/c (flat-contract integer?) char?) #\a #t)
  (test-flat-contract '(or/c (flat-contract integer?) char?) 1 #t)
  
  (test #t flat-contract? (and/c))
  (test #t flat-contract? (and/c number? integer?))
  (test #t flat-contract? (and/c (flat-contract number?)
				 (flat-contract integer?)))
  (test #t flat-contract? (let ()
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
  (test-flat-contract '(exact-integer-in 0 10) 0 11)
  (test-flat-contract '(exact-integer-in 0 10) 10 3/2)
  (test-flat-contract '(exact-integer-in 0 10) 1 1.0)
  (test-flat-contract '(real-in 1 10) 3/2 20)
  (test-flat-contract '(string/len 3) "ab" "abc")
  (test-flat-contract 'natural-number/c 5 -1)
  (test-flat-contract 'false/c #f #t)
  (test/spec-passed 'any/c '(contract any/c 1 'pos 'neg))
  (test-flat-contract 'printable/c (vector (cons 1 (box #f))) (lambda (x) x))
  (test-flat-contract '(symbols 'a 'b 'c) 'a 'd)
  (test-flat-contract '(one-of/c (expt 2 65)) (expt 2 65) 12)
  (test-flat-contract '(one-of/c #:x #:z) #:x #:y)
  
  (let ([c% (class object% (super-new))])
    (test-flat-contract (subclass?/c c%) c% object%)
    (test-flat-contract (subclass?/c c%) (class c%) (class object%)))
  
  (let ([i<%> (interface ())])
    (test-flat-contract `(implementation?/c ,i<%>) (class* object% (i<%>) (super-new)) object%)
    (test-flat-contract `(implementation?/c ,i<%>) (class* object% (i<%>) (super-new)) #f))
  
  (let ([i<%> (interface ())]
        [c% (class object% (super-new))])
    (test-flat-contract `(is-a?/c ,i<%>) (new (class* object% (i<%>) (super-new))) (new object%))
    (test-flat-contract `(is-a?/c ,c%) (new c%) (new object%)))
  
  (test-flat-contract '(listof boolean?) (list #t #f) (list #f 3 #t))
  (test-flat-contract '(listof any/c) (list #t #f) 3)
  ;(test-flat-contract '(list-immutableof boolean?) (list-immutable #t #f) (list-immutable #f 3 #t))
  ;(test-flat-contract '(list-immutableof any/c) (list-immutable #t #f) 3)
  ;(test-flat-contract '(list-immutableof boolean?) (list-immutable) (list))
  ;(test-flat-contract '(list-immutableof (-> boolean? boolean?)) (list-immutable (lambda (x) x)) (list (lambda (x) x)))
  
  (test-flat-contract '(vectorof boolean?) (vector #t #f) (vector #f 3 #t))
  (test-flat-contract '(vectorof any/c) (vector #t #f) 3)
  
  (test-flat-contract '(vector/c boolean? (flat-contract integer?)) (vector #t 1) (vector 1 #f))
  (test-flat-contract '(vector/c boolean? (flat-contract integer?)) (vector #t 1) #f)

  (test-flat-contract '(cons/c boolean? (flat-contract integer?)) (cons #t 1) (cons 1 #f))
  (test-flat-contract '(cons/c boolean? (flat-contract integer?)) (cons #t 1) #f)
  (test-flat-contract '(list/c boolean? (flat-contract integer?)) (list #t 1) (list 1 #f))
  (test-flat-contract '(list/c boolean? (flat-contract integer?)) (list #t 1) #f)

  ;(test-flat-contract '(cons-immutable/c boolean? (flat-contract integer?)) (cons-immutable #t 1) (cons-immutable 1 #f))
  ;(test-flat-contract '(cons-immutable/c boolean? (flat-contract integer?)) (cons-immutable #t 1) #f)
  ;(test-flat-contract '(cons-immutable/c boolean? (flat-contract integer?)) (cons-immutable #t 1) (cons #t 1))
  ;(test-flat-contract '(cons-immutable/c (-> boolean? boolean?) integer?) (cons-immutable (lambda (x) x) 1) #f)
  
  ;(test-flat-contract '(list-immutable/c boolean? (flat-contract integer?)) (list-immutable #t 1) (list-immutable 1 #f))
  ;(test-flat-contract '(list-immutable/c boolean? (flat-contract integer?)) (list-immutable #t 1) #f)
  ;(test-flat-contract '(list-immutable/c boolean? (flat-contract integer?)) (list-immutable #t 1) (list #t 1))
  ;(test-flat-contract '(list-immutable/c (-> boolean? boolean?) integer?) (list-immutable (lambda (x) x) 1) #f)
  
  (test-flat-contract '(box/c boolean?) (box #f) (box 1))
  (test-flat-contract '(box/c (flat-contract boolean?)) (box #t) #f)
  
  (test-flat-contract '(flat-rec-contract sexp (cons/c sexp sexp) number?) '(1 2 . 3) '(1 . #f))
  (test-flat-contract '(flat-murec-contract ([even1 (or/c null? (cons/c number? even2))] 
                                             [even2 (cons/c number? even1)])
                                            even1)
                      '(1 2 3 4)
                      '(1 2 3))
  (syntax-test #'(flat-murec-contract ([(x) y]) x)) ;; malformed binder
  (syntax-test #'(flat-murec-contract ([x y]))) ;; missing body

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                        ;;
  ;;   case-> arity checking tests                          ;;
  ;;                                                        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (test/well-formed #'(case-> (-> integer? integer?)))
  (test/well-formed #'(case-> (-> integer? integer?) (-> integer? integer? integer?)))
  (test/well-formed #'(case-> (-> integer? integer?) (-> integer? integer? any)))
  (test/well-formed #'(case-> (-> integer? any) (-> integer? integer? any)))
  
  (test/well-formed #'(case-> (->d (lambda x any/c)) (-> integer? integer?)))

  (test/well-formed #'(case-> (->* (any/c any/c) (integer?)) (-> integer? integer?)))
  (test/well-formed #'(case-> (->* (any/c any/c) any/c (integer?)) (-> integer? integer?)))
  (test/well-formed #'(case-> (->* (any/c any/c) any/c any) (-> integer? integer?)))
  
  (test/well-formed #'(case-> (->d* (any/c any/c) (lambda x any/c)) (-> integer? integer?)))
  (test/well-formed #'(case-> (->d* (any/c any/c) any/c (lambda x any/c)) (-> integer? integer?)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                        ;;
  ;;   Inferred Name Tests                                  ;;
  ;;                                                        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (eval 
   '(module contract-test-suite-inferred-name1 mzscheme
      (require (lib "contract.ss"))
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
  (eval '(require contract-test-suite-inferred-name1))
  ;; (eval '(test 'contract-inferred-name-test object-name contract-inferred-name-test)) ;; this one can't be made to pass, sadly.
  (eval '(test 'contract-inferred-name-test2 object-name contract-inferred-name-test2))
  (eval '(test 'contract-inferred-name-test2b object-name contract-inferred-name-test2b))
  (eval '(test 'contract-inferred-name-test3 object-name contract-inferred-name-test3))
  (eval '(test 'contract-inferred-name-test4 object-name contract-inferred-name-test4))
  (eval '(test 'contract-inferred-name-test5 object-name contract-inferred-name-test5))
  (eval '(test 'contract-inferred-name-test6 object-name contract-inferred-name-test6))
  (eval '(test 'contract-inferred-name-test7 object-name contract-inferred-name-test7))
  
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
  
  (test-name '(or/c) (or/c))
  (test-name '(or/c integer? gt0?) (or/c integer? (let ([gt0? (lambda (x) (> x 0))]) gt0?)))
  (test-name '(or/c integer? boolean?)
             (or/c (flat-contract integer?)
                   (flat-contract boolean?)))
  (test-name '(or/c (-> (>=/c 5) (>=/c 5)) boolean?)
             (or/c (-> (>=/c 5) (>=/c 5)) boolean?))
  (test-name '(or/c (-> (>=/c 5) (>=/c 5)) boolean?)
             (or/c boolean? (-> (>=/c 5) (>=/c 5))))
  
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
  (test-name '(exact-integer-in 0 10) (exact-integer-in 0 10))
  (test-name '(real-in 1 10) (real-in 1 10))
  (test-name '(string/len 3) (string/len 3))
  (test-name 'natural-number/c natural-number/c)
  (test-name 'false/c false/c)
  (test-name 'printable/c printable/c)
  (test-name '(symbols 'a 'b 'c) (symbols 'a 'b 'c))
  (test-name '(one-of/c 1 2 3) (one-of/c 1 2 3))
  
  (let ([c% (class object% (super-new))])
    (test-name '(subclass?/c class:c%) (subclass?/c c%)))
  
  (let ([i<%> (interface ())])
    (test-name '(implementation?/c interface:i<%>) (implementation?/c i<%>)))
  
  (let ([i<%> (interface ())]
        [c% (class object% (super-new))])
    (test-name '(is-a?/c interface:i<%>) (is-a?/c i<%>))
    (test-name '(is-a?/c class:c%) (is-a?/c c%)))
  
  (test-name '(listof boolean?) (listof boolean?))  
  (test-name '(listof any/c) (listof any/c))
  (test-name '(list-immutableof boolean?) (list-immutableof boolean?))
  (test-name '(list-immutableof any/c) (list-immutableof any/c))
  (test-name '(list-immutableof boolean?) (list-immutableof boolean?))
  (test-name '(list-immutableof (-> boolean? boolean?)) (list-immutableof (-> boolean? boolean?)))
  
  (test-name '(vectorof boolean?) (vectorof boolean?))
  (test-name '(vectorof any/c) (vectorof any/c))
  
  (test-name '(vector/c boolean? integer?) (vector/c boolean? integer?))
  (test-name '(vector/c boolean? integer?) (vector/c boolean? (flat-contract integer?)))

  (test-name '(cons/c boolean? integer?) (cons/c boolean? (flat-contract integer?)))
  (test-name '(cons/c boolean? integer?) (cons/c boolean? (flat-contract integer?)))
  (test-name '(cons/c boolean? (cons/c integer? null?)) (list/c boolean? (flat-contract integer?)))
  (test-name '(cons/c boolean? (cons/c integer? null?)) (list/c boolean? (flat-contract integer?)))

  (test-name '(cons-immutable/c boolean? integer?) (cons-immutable/c boolean? (flat-contract integer?)))
  (test-name '(cons-immutable/c boolean? integer?) (cons-immutable/c boolean? (flat-contract integer?)))
  (test-name '(cons-immutable/c boolean? integer?) (cons-immutable/c boolean? (flat-contract integer?)))
  (test-name '(cons-immutable/c (-> boolean? boolean?) integer?) (cons-immutable/c (-> boolean? boolean?) integer?))
  
  (test-name '(cons-immutable/c boolean? (cons-immutable/c integer? null?))
             (list-immutable/c boolean? (flat-contract integer?)))
  (test-name '(cons-immutable/c boolean? (cons-immutable/c integer? null?)) 
             (list-immutable/c boolean? (flat-contract integer?)))
  (test-name '(cons-immutable/c boolean? (cons-immutable/c integer? null?))
             (list-immutable/c boolean? (flat-contract integer?)))
  (test-name '(cons-immutable/c (-> boolean? boolean?) (cons-immutable/c integer? null?))
             (list-immutable/c (-> boolean? boolean?) integer?))
  
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
             (let ()
               (define-contract-struct couple (hd tl))
               (couple/c any/c any/c)))
  (test-name '(couple/c any/c any/c) 
             (let ()
               (define-contract-struct couple (hd tl))
               (couple/dc [hd any/c] [tl any/c])))
  (test-name '(couple/dc [hd any/c] [tl ...])
             (let ()
               (define-contract-struct couple (hd tl))
               (couple/dc [hd any/c] [tl (hd) any/c])))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  stronger tests
  ;;
  
  (test #t contract-stronger? any/c any/c)
  (test #t contract-stronger? (between/c 1 3) (between/c 0 4))
  (test #f contract-stronger? (between/c 0 4) (between/c 1 3))
  (test #t contract-stronger? (>=/c 3) (>=/c 2))
  (test #f contract-stronger? (>=/c 2) (>=/c 3))
  (test #f contract-stronger? (<=/c 3) (<=/c 2))
  (test #t contract-stronger? (<=/c 2) (<=/c 3))
  (test #f contract-stronger? (recursive-contract (<=/c 2)) (recursive-contract (<=/c 3)))
  (test #f contract-stronger? (recursive-contract (<=/c 3)) (recursive-contract (<=/c 2)))
  (let ([f (λ (x) (recursive-contract (<=/c x)))])
    (test #t contract-stronger? (f 1) (f 1)))
  (test #t contract-stronger? (-> integer? integer?) (-> integer? integer?))
  (test #f contract-stronger? (-> boolean? boolean?) (-> integer? integer?))
  (test #t contract-stronger? (-> (>=/c 3) (>=/c 3)) (-> (>=/c 4) (>=/c 3)))
  (test #f contract-stronger? (-> (>=/c 4) (>=/c 3)) (-> (>=/c 3) (>=/c 3)))
  (test #t contract-stronger? (-> (>=/c 3) (>=/c 3)) (-> (>=/c 3) (>=/c 2)))
  (test #f contract-stronger? (-> (>=/c 3) (>=/c 2)) (-> (>=/c 3) (>=/c 3)))
  (test #f contract-stronger? (-> (>=/c 2)) (-> (>=/c 3) (>=/c 3)))
  (test #t contract-stronger? (or/c null? any/c) (or/c null? any/c))
  (test #f contract-stronger? (or/c null? any/c) (or/c boolean? any/c))
  (test #t contract-stronger? (or/c null? boolean?) (or/c null? boolean?))
  (test #f contract-stronger? (or/c null? boolean?) (or/c boolean? null?))
  (test #t contract-stronger? (or/c null? (-> integer? integer?)) (or/c null? (-> integer? integer?)))
  (test #f contract-stronger? (or/c null? (-> boolean? boolean?)) (or/c null? (-> integer? integer?)))
  
  (test #t contract-stronger? number? number?)
  (test #f contract-stronger? boolean? number?)
  
  (test #t contract-stronger? (symbols 'x 'y) (symbols 'x 'y 'z))
  (test #f contract-stronger? (symbols 'x 'y 'z) (symbols 'x 'y))
  (test #t contract-stronger? (symbols 'x 'y) (symbols 'z 'x 'y))
  (test #f contract-stronger? (symbols 'z 'x 'y) (symbols 'x 'y))
  (test #t contract-stronger? (one-of/c (expt 2 100)) (one-of/c (expt 2 100) 12))
  
  (test #t contract-stronger?
        (or/c (-> (>=/c 3) (>=/c 3)) (-> string?))
        (or/c (-> (>=/c 4) (>=/c 3)) (-> string?)))
  (test #f contract-stronger?
        (or/c (-> string?) (-> integer? integer?))
        (or/c (-> string?) (-> any/c integer?)))
  (test #f contract-stronger?
        (or/c (-> string?) (-> any/c integer?))
        (or/c (-> string?) (-> integer? integer?)))
  (test #t contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer? boolean?)
        (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (test #f contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer? char?)
        (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (test #f contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer?)
        (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (test #f contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer?)
        (or/c (-> integer? integer?) integer?))
  
  (let ()
    (define-contract-struct couple (hd tl))
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
    
    (test #t contract-stronger? (couple/c any/c any/c) (couple/c any/c any/c))
    (test #f contract-stronger? (couple/c (>=/c 2) (>=/c 3)) (couple/c (>=/c 4) (>=/c 5)))
    (test #t contract-stronger? (couple/c (>=/c 4) (>=/c 5)) (couple/c (>=/c 2) (>=/c 3)))
    (test #f contract-stronger? (couple/c (>=/c 1) (>=/c 5)) (couple/c (>=/c 5) (>=/c 1)))
    (let ([ctc (couple/dc [hd any/c] [tl (hd) any/c])])
      (test #t contract-stronger? ctc ctc))
    (let ([ctc (couple/dc [hd any/c] [tl (hd) (<=/c hd)])])
      (test #t contract-stronger? ctc ctc))
    (test #t contract-stronger? list-of-numbers list-of-numbers)
    (test #t contract-stronger? (short-list/less-than 4) (short-list/less-than 5))
    (test #f contract-stronger? (short-list/less-than 5) (short-list/less-than 4))
    (test #t contract-stronger? (short-sorted-list/less-than 4) (short-sorted-list/less-than 5))
    (test #f contract-stronger? (short-sorted-list/less-than 5) (short-sorted-list/less-than 4))
    (test #t contract-stronger? (sorted-list/less-than 4) (sorted-list/less-than 5))
    (test #f contract-stronger? (sorted-list/less-than 5) (sorted-list/less-than 4)))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  first-order tests
  ;;
  
  (test #t contract-first-order-passes? (flat-contract integer?) 1)
  (test #f contract-first-order-passes? (flat-contract integer?) 'x)
  (test #t contract-first-order-passes? (flat-contract boolean?) #t)
  (test #f contract-first-order-passes? (flat-contract boolean?) 'x)
  (test #t contract-first-order-passes? any/c 1)
  (test #t contract-first-order-passes? any/c #t)
  (test #t contract-first-order-passes? (-> integer? integer?) (λ (x) #t))
  (test #f contract-first-order-passes? (-> integer? integer?) (λ (x y) #t))
  (test #f contract-first-order-passes? (-> integer? integer?) 'x)
  (test #t contract-first-order-passes? (-> integer? boolean? integer?) (λ (x y) #t))
  (test #f contract-first-order-passes? (-> integer? boolean? integer?) (λ (x) #t))
  (test #f contract-first-order-passes? (-> integer? boolean? integer?) (λ (x y z) #t))
  
  (test #t contract-first-order-passes? (->* (integer?) boolean? (char? any/c)) (λ (x . y) #f))
  (test #f contract-first-order-passes? (->* (integer?) boolean? (char? any/c)) (λ (x y . z) #f))
  (test #f contract-first-order-passes? (->* (integer?) boolean? (char? any/c)) (λ (x) #f))
  (test #t contract-first-order-passes? (->* (integer?) boolean? (char? any/c)) (λ x #f))
  
  (test #t contract-first-order-passes? (->d integer? boolean? (lambda (x y) char?)) (λ (x y) x))
  (test #f contract-first-order-passes? (->d integer? boolean? (lambda (x y) char?)) (λ (x) x))
  (test #f contract-first-order-passes? (->d integer? boolean? (lambda (x y) char?)) (λ (x y z) x))

  (test #t contract-first-order-passes? (list-immutableof integer?) (list-immutable 1))
  (test #f contract-first-order-passes? (list-immutableof integer?) (list 1))
  (test #f contract-first-order-passes? (list-immutableof integer?) #f)

  (test #t contract-first-order-passes? (vector-immutableof integer?) (vector->immutable-vector (vector 1)))
  (test #f contract-first-order-passes? (vector-immutableof integer?) 'x)
  (test #f contract-first-order-passes? (vector-immutableof integer?) '())
  
  (test #t contract-first-order-passes? (promise/c integer?) (delay 1))
  (test #f contract-first-order-passes? (promise/c integer?) 1)
  
  (test #t contract-first-order-passes? (->d* (integer? boolean?) (lambda (x y) char?)) (λ (x y) #t))
  (test #f contract-first-order-passes? (->d* (integer? boolean?) (lambda (x y) char?)) (λ (x) #t))
  (test #f contract-first-order-passes? (->d* (integer? boolean?) (lambda (x y) char?)) (λ (x y z) #t))

  (test #t contract-first-order-passes? 
        (->d* (integer? boolean?) any/c (lambda (x y . z) char?))
        (λ (x y . z) z))
  (test #t contract-first-order-passes? 
        (->d* (integer? boolean?) any/c (lambda (x y . z) char?))
        (λ (y . z) z))
  (test #t contract-first-order-passes? 
        (->d* (integer? boolean?) any/c (lambda (x y . z) char?))
        (λ z z))
  (test #f contract-first-order-passes? 
        (->d* (integer? boolean?) any/c (lambda (x y . z) char?))
        (λ (x y z . w) 1))
  (test #f contract-first-order-passes? 
        (->d* (integer? boolean?) any/c (lambda (x y . z) char?))
        (λ (x y) 1))
  
  (test #t contract-first-order-passes? (->r ((x number?)) number?) (λ (x) 1))
  (test #f contract-first-order-passes? (->r ((x number?)) number?) (λ (x y) 1))
  (test #f contract-first-order-passes? (->r ((x number?)) number?) (λ () 1))
  (test #t contract-first-order-passes? (->r ((x number?)) number?) (λ args 1))
  
  (test #t contract-first-order-passes? (->pp ((x number?)) #t number? blech #t) (λ (x) 1))
  (test #f contract-first-order-passes? (->pp ((x number?)) #t number? blech #t) (λ () 1))
  (test #t contract-first-order-passes? (->pp ((x number?)) #t number? blech #t) (λ (x . y) 1))
  
  (test #f contract-first-order-passes? 
        (case-> (-> integer? integer?)
                (-> integer? integer? integer?))
        (λ () 1))
  (test #f contract-first-order-passes? 
        (case-> (-> integer? integer?)
                (-> integer? integer? integer?))
        (λ (x) 1))
  (test #f contract-first-order-passes? 
        (case-> (-> integer? integer?)
                (-> integer? integer? integer?))
        (λ (x y) 1))
  (test #f contract-first-order-passes? 
        (case->)
        1)
  
  (test #t contract-first-order-passes? 
        (case->)
        (case-lambda))
  
  (test #t contract-first-order-passes? 
        (case-> (-> integer? integer?)
                (-> integer? integer? integer?))
        (case-lambda [(x) x] [(x y) x]))
  (test #t contract-first-order-passes? 
        (case-> (-> integer? integer?)
                (-> integer? integer? integer?))
        (case-lambda [() 1] [(x) x] [(x y) x]))
  (test #t contract-first-order-passes? 
        (case-> (-> integer? integer?)
                (-> integer? integer? integer?))
        (case-lambda [() 1] [(x) x] [(x y) x] [(x y z) x]))
  
  (test #t contract-first-order-passes? (and/c (-> positive? positive?) (-> integer? integer?)) (λ (x) x))
  (test #t contract-first-order-passes? (and/c (-> positive? positive?) (-> integer? integer?)) values)
  (test #f contract-first-order-passes? (and/c (-> integer?) (-> integer? integer?)) (λ (x) x))
  
  (test #t contract-first-order-passes? 
        (cons-immutable/c boolean? (-> integer? integer?))
        (list*-immutable #t (λ (x) x)))
  (test #t contract-first-order-passes? 
        (cons-immutable/c boolean? (-> integer? integer?))
        (list*-immutable 1 2))
  
  (test #f contract-first-order-passes? (flat-rec-contract the-name) 1)

  (test #t contract-first-order-passes? 
        (object-contract (m (-> integer? integer?)))
        (new object%))
  (test #t contract-first-order-passes? 
        (object-contract (m (-> integer? integer?)))
        1)

  (let ()
    (define-contract-struct couple (hd tl))
    (test #t contract-first-order-passes?
          (couple/c any/c any/c) 
          (make-couple 1 2))
    
    (test #f contract-first-order-passes?
          (couple/c any/c any/c) 
          2)
    
    (test #t contract-first-order-passes?
          (couple/dc [hd any/c] [tl any/c]) 
          (make-couple 1 2))
    
    (test #f contract-first-order-passes?
          (couple/dc [hd any/c] [tl any/c]) 
          1)
    
    (test #t contract-first-order-passes?
          (couple/dc [hd any/c] [tl (hd) any/c]) 
          (make-couple 1 2))
    
    (test #f contract-first-order-passes?
          (couple/dc [hd any/c] [tl (hd) any/c]) 
          1))

  (test #t contract-first-order-passes? (or/c (-> (>=/c 5) (>=/c 5)) boolean?) #t)
  (test #t contract-first-order-passes? (or/c (-> (>=/c 5) (>=/c 5)) boolean?) (λ (x) x))
  (test #f contract-first-order-passes? (or/c (-> (>=/c 5) (>=/c 5)) boolean?) 'x)

  (test #t contract-first-order-passes? 
        (or/c (-> integer? integer? integer?)
              (-> integer? integer?))
        (λ (x) x))
  (test #t contract-first-order-passes? 
        (or/c (-> integer? integer? integer?)
              (-> integer? integer?))
        (λ (x y) x))
  (test #f contract-first-order-passes? 
        (or/c (-> integer? integer? integer?)
              (-> integer? integer?))
        (λ () x))
  (test #f contract-first-order-passes? 
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
  (test-name '(or/c (-> (>=/c 5) (>=/c 5)) boolean?)
             (or/c boolean? (-> (>=/c 5) (>=/c 5))))
  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; provide/contract tests
  ;; (at the end, becuase they are slow w/out .zo files)
  ;;
  
    (test/spec-passed
   'provide/contract1
   '(let ()
      (eval '(module contract-test-suite1 mzscheme
               (require (lib "contract.ss"))
               (define x 1)
               (provide/contract (x integer?))))
      (eval '(require contract-test-suite1))
      (eval 'x)))
  
  (test/spec-passed
   'provide/contract2
   '(let ()
      (eval '(module contract-test-suite2 mzscheme
               (require (lib "contract.ss"))
               (provide/contract)))
      (eval '(require contract-test-suite2))))
  
  (test/spec-failed
   'provide/contract3
   '(let ()
      (eval '(module contract-test-suite3 mzscheme
               (require (lib "contract.ss"))
               (define x #f)
               (provide/contract (x integer?))))
      (eval '(require contract-test-suite3))
      (eval 'x))
   "contract-test-suite3")
  
  (test/spec-passed
   'provide/contract4
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module contract-test-suite4 mzscheme
               (require (lib "contract.ss"))
               (define-struct s (a))
               (provide/contract (struct s ((a any/c))))))
      (eval '(require contract-test-suite4))
      (eval '(list (make-s 1)
                   (s-a (make-s 1))
                   (s? (make-s 1))
                   (set-s-a! (make-s 1) 2)))))
  
  (test/spec-passed/result
   'provide/contract4-b
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module contract-test-suite4-b mzscheme
               (require (lib "contract.ss"))
               (define-struct s (a b))
               (provide/contract (struct s ((a any/c) (b any/c))))))
      (eval '(require contract-test-suite4-b))
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
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module contract-test-suite5 mzscheme
               (require (lib "contract.ss"))
               (define-struct s (a))
               (define-struct t (a))
               (provide/contract (struct s ((a any/c)))
                                 (struct t ((a any/c))))))
      (eval '(require contract-test-suite5))
      (eval '(list (make-s 1)
                   (s-a (make-s 1))
                   (s? (make-s 1))
                   (set-s-a! (make-s 1) 2)
                   (make-t 1)
                   (t-a (make-t 1))
                   (t? (make-t 1))
                   (set-t-a! (make-t 1) 2)))))
  
  (test/spec-passed
   'provide/contract6
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module contract-test-suite6 mzscheme
               (require (lib "contract.ss"))
               (define-struct s (a))
               (provide/contract (struct s ((a any/c))))))
      (eval '(require contract-test-suite6))
      (eval '(define-struct (t s) ()))))
  
  (test/spec-passed
   'provide/contract6
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module contract-test-suite6 mzscheme
               (require (lib "contract.ss"))
               (define-struct s (a))
               (provide/contract (struct s ((a any/c))))))
      (eval '(require contract-test-suite6))
      (eval '(define-struct (t s) ()))))
  
  (test/spec-passed
   'provide/contract6b
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module contract-test-suite6b mzscheme
               (require (lib "contract.ss"))
               (define-struct s_ (a))
               (provide/contract (struct s_ ((a any/c))))))
      (eval '(require contract-test-suite6b))
      (eval '(module contract-test-suite6b2 mzscheme
               (require contract-test-suite6b)
               (require (lib "contract.ss"))
               (define-struct (t_ s_) (b))
               (provide s_-a)
               (provide/contract (struct (t_ s_) ((a any/c) (b any/c))))))
      (eval '(require contract-test-suite6b2))
      (eval '(define-struct (u_ t_) ()))
      (eval '(s_-a (make-u_ 1 2)))))
  
  (test/spec-passed
   'provide/contract7
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module contract-test-suite7 mzscheme
               (require (lib "contract.ss"))
               (define-struct s (a b))
               (define-struct (t s) (c d))
               (provide/contract 
                (struct s ((a any/c) (b any/c)))
                (struct (t s) ((a any/c) (b any/c) (c any/c) (d any/c))))))
      (eval '(require contract-test-suite7))
      (eval '(let ([x (make-t 1 2 3 4)])
               (s-a x)
               (s-b x)
               (t-c x)
               (t-d x)
               (void)))))
  
  (test/spec-passed
   'provide/contract8
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module contract-test-suite8 mzscheme
               (require (lib "contract.ss"))
               (define-struct i-s (contents))
               (define (w-f-s? x) #t)
               (provide/contract 
                (struct i-s ((contents (flat-named-contract "integer-set-list" w-f-s?)))))))
      (eval '(require contract-test-suite8))
      (eval '(i-s-contents (make-i-s 1)))))
   
  (test/spec-passed
   'provide/contract9
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module contract-test-suite9 mzscheme
               (require (lib "contract.ss"))
               (define the-internal-name 1)
               (provide/contract (rename the-internal-name the-external-name integer?))
               (+ the-internal-name 1)))
      (eval '(require contract-test-suite9))
      (eval '(+ the-external-name 1))))
  
  (test/spec-passed
   'provide/contract10
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module m mzscheme
               (require (lib "contract.ss"))
               (define-struct s (a b) (make-inspector))
               (provide/contract (struct s ((a number?) (b number?))))))
      (eval '(module n mzscheme
               (require (lib "struct.ss")
                        m)
               (print-struct #t)
               (copy-struct s 
                            (make-s 1 2)
                            [s-a 3])))
      (eval '(require n))))
  
  ;; this test is broken, not sure why
  #|
  (test/spec-failed
   'provide/contract11
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module m mzscheme
               (require (lib "contract.ss"))
               (define-struct s (a b) (make-inspector))
               (provide/contract (struct s ((a number?) (b number?))))))
      (eval '(module n mzscheme
               (require (lib "struct.ss")
                        m)
               (print-struct #t)
               (copy-struct s 
                            (make-s 1 2)
                            [s-a #f])))
      (eval '(require n)))
   'n)
|#
  
  (test/spec-passed
   'provide/contract12
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module m mzscheme
               (require (lib "contract.ss"))
               (define-struct (exn2 exn) ())
               (provide/contract (struct (exn2 exn) ((message any/c) (continuation-marks any/c))))))
      (eval '(require m))))
  
  (test/spec-passed/result
   'provide/contract13
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module common-msg-structs mzscheme
               (require (lib "contract.ss" "mzlib"))
               (define-struct register (name type) (make-inspector))
               (provide/contract (struct register ([name any/c] [type any/c])))))
      
      (eval '(require common-msg-structs))
      (eval '(require (lib "plt-match.ss")))
      (eval '(match (make-register 1 2)
               [(struct register (name type))
                (list name type)])))
   (list 1 2))
  
  (test/spec-passed
   'provide/contract14
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module test1 mzscheme
               (require (lib "contract.ss"))
               
               (define-struct type (flags))
               (define-struct (type:ptr type) (type))
               
               (provide/contract
                (struct type
                        ([flags (listof string?)]))
                
                (struct (type:ptr type)
                        ([flags (listof string?)] [type type?])))))

      (eval '(module test2 mzscheme
               (require (lib "plt-match.ss"))
               (require test1)
               (match (make-type:ptr '() (make-type '()))
                 [(struct type:ptr (flags type)) #f])))
      (eval '(require test2))))
  
  ;; make sure unbound identifier exception is raised.
  (error-test
   #'(parameterize ([current-namespace (make-namespace)])
       (eval '(module pos mzscheme
                (require (lib "contract.ss"))
                (provide/contract [i any/c]))))
   exn:fail:syntax?)
  
  ;; provide/contract should signal errors without requiring a reference to the variable
  ;; this test is bogus, because provide/contract'd variables can be set!'d.
  (test/pos-blame
   'provide/contract15
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module pos mzscheme
               (require (lib "contract.ss"))
               (define i #f)
               (provide/contract [i integer?])))
      (eval '(require pos))))
  
  ;; this is really a positive violation, but name the module `neg' just for an addl test
  (test/neg-blame
   'provide/contract16
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module neg mzscheme
               (require (lib "contract.ss"))
               (define i #f)
               (provide/contract [i integer?])))
      (eval '(require neg))))
  
  ;; this test doesn't pass yet ... waiting for support from define-struct
  
  #;
  (test/neg-blame
   'provide/contract17
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module pos mzscheme
               (require (lib "contract.ss"))
               (define-struct s (a))
               (provide/contract [struct s ((a integer?))])))
      (eval '(module neg mzscheme
               (require pos)
               (define-struct (t s) ())
               (make-t #f)))
      (eval '(require neg))))
  

  (error-test
   #'(parameterize ([current-namespace (make-namespace)])
       (eval '(module bug mzscheme
                (require (lib "contract.ss"))
                (define the-defined-variable 'five)
                (provide/contract [the-defined-variable number?])))
       (eval '(require bug)))
   (λ (x)
     (and (exn? x)
          (regexp-match #rx"on the-defined-variable" (exn-message x)))))
  

  
))
(report-errs)
