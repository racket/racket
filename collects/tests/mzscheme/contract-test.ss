(load-relative "loadtest.ss")
(require (lib "contract.ss")
	 (lib "class.ss")
         (lib "etc.ss"))
  
(SECTION 'contract)

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
           (and (regexp-match #rx"^pos broke" (exn-message exn)))))
    (printf "testing: ~s\n" name)
    (thunk-error-test 
     (lambda () (eval expression))
     (datum->syntax-object #'here expression)
     has-pos-blame?))
  
  (define (test/neg-blame name expression)
    (define (has-neg-blame? exn)
      (and (exn? exn)
           (and (regexp-match #rx"^neg broke" (exn-message exn)))))
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
  
  (test/pos-blame 
   'contract-case->10
   '((contract (case-> (->r ([x number?]) (<=/c x)))
               (lambda (x) (+ x 1))
               'pos
               'neg)
     1))
 
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
   'union1
   '(contract (union false/c) #t 'pos 'neg))

  (test/spec-passed
   'union2
   '(contract (union false/c) #f 'pos 'neg))

  (test/spec-passed
   'union3
   '((contract (union (-> integer? integer?)) (lambda (x) x) 'pos 'neg) 1))
  
  (test/neg-blame
   'union4
   '((contract (union (-> integer? integer?)) (lambda (x) x) 'pos 'neg) #f))
  
  (test/pos-blame
   'union5
   '((contract (union (-> integer? integer?)) (lambda (x) #f) 'pos 'neg) 1))
  
  (test/spec-passed
   'union6
   '(contract (union false/c (-> integer? integer?)) #f 'pos 'neg))
  
  (test/spec-passed
   'union7
   '((contract (union false/c (-> integer? integer?)) (lambda (x) x) 'pos 'neg) 1))

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
  
  (test/spec-passed
   'provide/contract1
   '(let ()
      (eval '(module contract-test-suite1 mzscheme
               (require (lib "contract.ss"))
               (provide/contract (x integer?))
               (define x 1)))
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
               (provide/contract (x integer?))
               (define x #f)))
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
               (provide/contract (rename the-internal-name the-external-name integer?))
               (define the-internal-name 1)
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

  (test/pos-blame 
   'object-contract-->r2
   '(send (contract (object-contract (m (case-> (->r ([x number?]) (<=/c x)))))
                    (new (class object% (define/public m (lambda (x) (+ x 1))) (super-new)))
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

  (test/spec-passed
   'object-contract-->pp1
   '(send (contract (object-contract (m (case-> (->pp ([x number?]) #t (<=/c x) unused #t))))
                    (new (class object% (define/public m (lambda (x) (- x 1))) (super-new)))
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

  
  (test/spec-passed 
   'anaphoric1
   '(contract (let-values ([(in out) (anaphoric-contracts)]) in)
              1
              'pos
              'neg))
  
  (test/pos-blame
   'anaphoric2
   '(contract (let-values ([(in out) (anaphoric-contracts)]) out)
              1
              'pos
              'neg))
  
  (test/spec-passed
   'anaphoric3
   '((contract (let-values ([(in out) (anaphoric-contracts)]) (-> in out))
               (lambda (x) x)
               'pos
               'neg)
     1))
  
  (test/pos-blame
   'anaphoric4
   '((contract (let-values ([(in out) (anaphoric-contracts)]) (-> in out))
               (lambda (x) (* 2 x))
               'pos
               'neg)
     1))
  
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
  ;;                                                        ;;
  ;;   Flat Contract Tests                                  ;;
  ;;                                                        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (test #t flat-contract? (union))
  (test #t flat-contract? (union integer? (lambda (x) (> x 0))))
  (test #t flat-contract? (union (flat-contract integer?)
				 (flat-contract boolean?)))
  (test-flat-contract '(union (flat-contract integer?) char?) #\a #t)
  (test-flat-contract '(union (flat-contract integer?) char?) 1 #t)
  
  (test #t flat-contract? (and/c))
  (test #t flat-contract? (and/c number? integer?))
  (test #t flat-contract? (and/c (flat-contract number?)
				 (flat-contract integer?)))
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
  (test-flat-contract '(flat-murec-contract ([even1 (union null? (cons/c number? even2))] 
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
      
      (define (contract-inferred-name-test3 x) x)
      (provide/contract (contract-inferred-name-test3 (->* (number?) (number?))))

      (define (contract-inferred-name-test4 x) x)
      (provide/contract (contract-inferred-name-test4 (case-> (->* (number?) (number?)))))

      (define contract-inferred-name-test5 (case-lambda [(x) x] [(x y) x]))
      (provide/contract (contract-inferred-name-test5 (case-> (-> number? number?) 
                                                              (-> number? number? number?))))

      (define contract-inferred-name-test6 (case-lambda [(x) x]
                                                        [(x y) y]))
      (provide/contract (contract-inferred-name-test6 (opt-> (number?) (number?) number?)))))
  (eval '(require contract-test-suite-inferred-name1))
  (eval '(test 'contract-inferred-name-test object-name contract-inferred-name-test))
  (eval '(test 'contract-inferred-name-test2 object-name contract-inferred-name-test2))
  (eval '(test 'contract-inferred-name-test3 object-name contract-inferred-name-test3))
  (eval '(test 'contract-inferred-name-test4 object-name contract-inferred-name-test4))
  (eval '(test 'contract-inferred-name-test5 object-name contract-inferred-name-test5))
  (eval '(test 'contract-inferred-name-test6 object-name contract-inferred-name-test6))
  
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
  (test-name '(->* (integer? boolean?) (char? any/c)) (->* (integer? boolean?) (char? any/c)))
  (test-name '(->* (integer? boolean?) any) (->* (integer? boolean?) any))
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
  
  (test-name '(case-> (->r ((x ...)) ...)) (case-> (->r ((x number?)) number?)))
  (test-name '(case-> (->r ((x ...) (y ...) (z ...)) ...))
             (case-> (->r ((x number?) (y boolean?) (z pair?)) number?)))
  
  (test-name '(case-> (-> integer? integer?) (-> integer? integer? integer?))
             (case-> (-> integer? integer?) (-> integer? integer? integer?)))
  
  (test-name '(union) (union))
  (test-name '(union integer? gt0?) (union integer? (let ([gt0? (lambda (x) (> x 0))]) gt0?)))
  (test-name '(union integer? boolean?)
             (union (flat-contract integer?)
                    (flat-contract boolean?)))
  (test-name '(union (-> (>=/c 5) (>=/c 5)) boolean?)
             (union (-> (>=/c 5) (>=/c 5)) boolean?))
  
  (test-name 'any/c (and/c))
  (test-name '(and/c any/c) (and/c any/c))
  (test-name '(and/c any/c any/c) (and/c any/c any/c))
  (test-name '(and/c number? integer?) (and/c number? integer?))
  (test-name '(and/c number? integer?) (and/c (flat-contract number?)
                                              (flat-contract integer?)))
  (test-name '(and/c number? (-> integer? integer?)) (and/c number? (-> integer? integer?)))

  (test-name '(not/c integer?) (not/c integer?))
  (test-name '(=/c 5) (=/c 5))
  (test-name '(>=/c 5) (>=/c 5))
  (test-name '(<=/c 5) (<=/c 5))
  (test-name '(</c 5) (</c 5))
  (test-name '(>/c 5) (>/c 5))
  (test-name '(integer-in 0 10) (integer-in 0 10))
  (test-name '(exact-integer-in 0 10) (exact-integer-in 0 10))
  (test-name '(real-in 1 10) (real-in 1 10))
  (test-name '(string/len 3) (string/len 3))
  (test-name 'natural-number/c natural-number/c)
  (test-name 'false/c false/c)
  (test-name 'printable/c printable/c)
  (test-name '(symbols 'a 'b 'c)(symbols 'a 'b 'c))
  
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
  ))
(report-errs)
