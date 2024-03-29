#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract/parametric
                                               'racket/contract/combinator)])
  (define exn:fail:contract:blame? (contract-eval 'exn:fail:contract:blame?))

  (test/no-error '(->i ([x integer?]) ([y integer?]) any))
  (test/no-error '(->i ([x integer?]) ([y integer?]) (values [a number?] [b boolean?])))
  (test/no-error '(->i ([x integer?] #:z [z integer?]) ([y integer?] #:w [w integer?]) 
                       (range boolean?)))
  (test/no-error '(->i ([x integer?] #:z [z integer?]) ([y integer?] #:w [w integer?]) 
                       #:rest [rest any/c] (range boolean?)))
  (test/no-error '(->i ([x integer?] #:z [z integer?]) #:rest [rest any/c] (range boolean?)))
  
  
  (test/spec-passed
   '->i-stx-1
   '(->i ([x (y) number?]
          [y number?])
         any))
  
  (test/spec-passed
   '->i-stx-2
   '(->i ()
         (values [x (y) number?]
                 [y number?])))
  
  (test/spec-passed
   '->i-stx-3
   '(->i ()
         #:rest [x number?]
         [y (x) number?]))
  
  (contract-syntax-error-test
   '->i-stx4
   '(->i (#:kwd1 [x number?]
                 #:kwd2 [x number?])
         (values [y number?]
                 [z number?])))
  
  (contract-syntax-error-test
   '->i-stx5
   #'(->i (#:kwd1 [w number?]
                  #:kwd1 [x number?])
          (values [y number?]
                  [z number?])))
  
  (contract-syntax-error-test
   '->i-stx6
   #'(->i (#:kwd1 [w number?]
                  #:kwd2 [x number?])
          (values [y number?]
                  [w number?])))
  
  (contract-syntax-error-test
   '->i-stx7
   #'(->i (#:kwd1 [w number?]
                  #:kwd2 [x number?])
          (values [y number?]
                  [y number?])))
  
  (contract-syntax-error-test
   '->i-stx8
   #'(->i (#:kwd1 [w number?]
                  #:kwd2 [x number?])
          (values [y number?]
                  [w number?])))
  
  (contract-syntax-error-test
   '->i-stx10
   #'(->i (#:kwd1 [x number?]
                  #:kwd2 [y number?])
          [x number?]))
  
  (contract-syntax-error-test
   '->i-stx11
   #'(->i (#:kwd1 [x number?]
                  #:kwd2 [y number?])
          #:rest [x any/c]
          any))
  
  (contract-syntax-error-test
   '->i-stx12
   #'(let ([c integer?])
       (->i ((arg any/c)) () (values (_ (arg) c) (x (arg) c) (_ (arg) c)))))
  
  (contract-syntax-error-test
   '->i-stx13
   #'(->i ([x (y) number?])
          any))
  
  (contract-syntax-error-test
   '->i-stx14
   #'(->i ([x number?]) #:pre (y) #t any))
  
  (contract-syntax-error-test
   '->i-stx15
   #'(->i ([x number?]) #:pre (x) #t [res any/c] #:post (y) #t))
  
  (contract-syntax-error-test
   '->i-stx16
   #'(->i ([x (y) number?])
          [y number?]))
  
  (contract-syntax-error-test
   '->i-stx17
   #'(->i ()
          #:rest [x (y) number?]
          [y number?]))
  
  (contract-syntax-error-test
   '->i-stx18
   #'(->i ([x number?]) #:pre (res) #t [res any/c] #:post (x) #t))
  
  (contract-syntax-error-test
   '->i-stx19
   #'(->i ([x (x) number?])
          any))
  
  (contract-syntax-error-test
   '->i-stx20
   #'(->i ([x (y) number?]
           [y (x) number?])
          any))
  
  (contract-syntax-error-test
   '->i-stx21
   #'(->i ([in number?])
          (values [x (y) number?]
                  [y (z) number?]
                  [z (x) number?])))
  
  (contract-syntax-error-test
   '->i-stx22
   #'(->i ()
          #:rest [x (x) number?]
          any))
  
  (contract-syntax-error-test
   '->i-stx23
   #'(->i ([f (f x) any/c]) (#:x [x any/c]) [res any/c]))
  
  (contract-syntax-error-test
   '->i-stx24
   #'(let ([p (make-parameter #f)])
       (->i ([x integer?]) #:param (x) p any)))

  (test/spec-passed
   '->i1
   '((contract (->i () () [x number?]) (lambda () 1) 'pos 'neg)))
  
  (test/spec-passed
   '->i2
   '((contract (->i ([x number?]) () (values [r number?])) (lambda (x) (+ x 1)) 'pos 'neg) 1))
  
  (test/pos-blame
   '->i3
   '((contract (->i () () [r number?]) 1 'pos 'neg)))
  
  (test/pos-blame
   '->i4
   '((contract (->i () () [r number?]) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->i5
   '((contract (->i ([x number?]) () any) (lambda (x) (+ x 1)) 'pos 'neg) #f))
  
  (test/pos-blame
   '->i6
   '((contract (->i ([x number?]) () [r (x) (<=/c x)]) (lambda (x) (+ x 1)) 'pos 'neg) 1))
  
  (test/spec-passed
   '->i7
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () [r (x) (<=/c x)]) 
               (lambda (x y) (- x 1)) 'pos 'neg) 1 0))
  
  (test/neg-blame
   '->i8
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () [r (x) (<=/c x)]) 
               (lambda (x y) (+ x 1)) 'pos 'neg) 1 2))

  (test/neg-blame
   '->i8b
   '((contract (->i ([x number?] [y (x) number?]) any)
               (lambda (x y) y) 'pos 'neg) "1" 2))
  
  (test/spec-passed
   '->i9
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () [r (x) (<=/c x)]) 
               (lambda (y x) (- x 1)) 'pos 'neg) 1 2))
  
  (test/neg-blame
   '->i10
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () [r (x) (<=/c x)]) 
               (lambda (y x) (+ x 1)) 'pos 'neg) 1 0))
  
  (test/spec-passed
   '->i11
   '((contract (->i () () #:rest [rest any/c] [r number?]) (lambda x 1) 'pos 'neg)))
  
  (test/spec-passed
   '->i12
   '((contract (->i ([x number?]) () #:rest [rest any/c] [r number?]) 
               (lambda (x . y) (+ x 1)) 'pos 'neg) 1))
  
  (test/pos-blame
   '->i13
   '((contract (->i () () #:rest [rest any/c] [r number?]) 1 'pos 'neg)))
  
  (test/pos-blame
   '->i14
   '((contract (->i () () #:rest [rest any/c] [r number?]) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->i15
   '((contract (->i ([x number?]) () #:rest [rest any/c] any) (lambda (x . y) (+ x 1)) 'pos 'neg) #f))
  
  (test/pos-blame
   '->i16
   '((contract (->i ([x number?]) () #:rest [rest any/c] [r (x) (<=/c x)])
               (lambda (x . y) (+ x 1)) 'pos 'neg) 1))
  
  (test/spec-passed
   '->i17
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () #:rest [rest any/c] [r (x) (<=/c x)])
               (lambda (x y . z) (- x 1)) 'pos 'neg) 1 0))
  
  (test/neg-blame
   '->i18
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () #:rest [rest any/c] [r (x) (<=/c x)])
               (lambda (x y . z) (+ x 1)) 'pos 'neg) 1 2))
  
  (test/spec-passed
   '->i19
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () #:rest [rest any/c] [r (x) (<=/c x)])
               (lambda (y x . z) (- x 1)) 'pos 'neg) 1 2))
  
  (test/neg-blame
   '->i20
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () #:rest [rest any/c] [r (x) (<=/c x)])
               (lambda (y x . z) (+ x 1)) 'pos 'neg) 1 0))
  
  (test/spec-passed
   '->i21
   '((contract (->i () () #:rest [rst (listof number?)] [r any/c]) (lambda w 1) 'pos 'neg) 1))
  
  (test/neg-blame
   '->i22
   '((contract (->i () () #:rest [rst (listof number?)] [r any/c]) (lambda w 1) 'pos 'neg) #f))
  
  (test/spec-passed/result
   '->i23
   '((contract (->i ([x any/c] #:y [y any/c]) ([z any/c]) any)
               (let ()
                 (define (m x #:y y [z 1]) x)
                 m)
               'pos
               'neg)
     1 #:y 2)
   1)
  
  (test/spec-passed/result
   '->i24
   '((contract (->i ([x any/c]) ([y any/c]) any)
               (let ()
                 (define (m x [y 1]) x)
                 m)
               'pos
               'neg)
     1)
   1)
  
  
  (test/spec-passed/result
   '->i28
   '((contract (->i ([x real?])
                    #:rest [rest (x) (listof (>=/c x))]
                    any)
               (λ (x . rest)
                 (cons x rest))
               'pos
               'neg)
     1
     2
     3)
   '(1 2 3))
  
  (test/neg-blame
   '->i29
   '((contract (->i ([x real?])
                    #:rest [rest (x) (listof (>=/c x))]
                    any)
               (λ (x . rest)
                 (cons x rest))
               'pos
               'neg)
     1
     -2
     -3))

  (test/pos-blame
   '->i30
   '((contract (->i () (values))
               (λ () 1)
               'pos 'neg)))

  (test/spec-passed
   '->i31
   '((contract (->i () (values))
               values
               'pos 'neg)))
  
  (test/spec-passed
   '->i-any1
   '((contract (->i () () any) (lambda () 1) 'pos 'neg)))
  
  (test/spec-passed
   '->i-any2
   '((contract (->i ([x number?]) () any) (lambda (x) (+ x 1)) 'pos 'neg) 1))
  
  (test/pos-blame
   '->i-any3
   '((contract (->i () () any) 1 'pos 'neg)))
  
  (test/pos-blame
   '->i-any4
   '((contract (->i () () any) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->i-any5
   '((contract (->i ([x number?]) () any) (lambda (x) (+ x 1)) 'pos 'neg) #f))
  
  (test/spec-passed
   '->i-any6
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () any) (lambda (x y) (- x 1)) 'pos 'neg) 1 0))
  
  (test/neg-blame
   '->i-any7
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () any) (lambda (x y) (+ x 1)) 'pos 'neg) 1 2))
  
  (test/spec-passed
   '->i-any8
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () any) (lambda (y x) (- x 1)) 'pos 'neg) 1 2))
  
  (test/neg-blame
   '->i-any9
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () any) (lambda (y x) (+ x 1)) 'pos 'neg) 1 0))
  
  (test/spec-passed
   '->i-any10
   '((contract (->i () () #:rest [rest any/c] any) (lambda x 1) 'pos 'neg)))
  
  (test/spec-passed
   '->i-any11
   '((contract (->i ([x number?]) () #:rest [rest any/c] any) (lambda (x . y) (+ x 1)) 'pos 'neg) 1))
  
  (test/pos-blame
   '->i-any12
   '((contract (->i () () #:rest [rest any/c] any) 1 'pos 'neg)))
  
  (test/pos-blame
   '->i-any13
   '((contract (->i () () #:rest [rest any/c] any) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->i-any14
   '((contract (->i ([x number?]) () #:rest [rest any/c] any) (lambda (x . y) (+ x 1)) 'pos 'neg) #f))
  
  (test/spec-passed
   '->i-any15
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () #:rest [rest any/c] any) 
               (lambda (x y . z) (- x 1)) 'pos 'neg) 1 0))
  
  (test/neg-blame
   '->i-any16
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () #:rest [rest any/c] any) 
               (lambda (x y . z) (+ x 1)) 'pos 'neg) 1 2))
  
  (test/spec-passed
   '->i-any17
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () #:rest [rest any/c] any) 
               (lambda (y x . z) (- x 1)) 'pos 'neg) 1 2))
  
  (test/neg-blame
   '->i-any18
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () #:rest [rest any/c] any) 
               (lambda (y x . z) (+ x 1)) 'pos 'neg) 1 0))
  
  (test/spec-passed
   '->i-any19
   '((contract (->i () () #:rest [rst (listof number?)] any) (lambda w 1) 'pos 'neg) 1))
  
  (test/neg-blame
   '->i-any20
   '((contract (->i () () #:rest [rst (listof number?)] any) (lambda w 1) 'pos 'neg) #f))
  
  (test/spec-passed
   '->i-values1
   '((contract (->i () () (values [x boolean?] [y number?])) (lambda () (values #t 1)) 'pos 'neg)))
  
  (test/spec-passed
   '->i-values2
   '((contract (->i ([x number?]) () (values [z boolean?] [y number?]))
               (lambda (x) (values #t (+ x 1))) 'pos 'neg) 1))
  
  (test/pos-blame
   '->i-values3
   '((contract (->i () () (values [x boolean?] [y number?])) 1 'pos 'neg)))
  
  (test/pos-blame
   '->i-values4
   '((contract (->i () () (values [x boolean?] [y number?])) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->i-values5
   '((contract (->i ([x number?]) () (values [y boolean?] [z (x) (<=/c x)]))
               (lambda (x) (+ x 1)) 'pos 'neg) #f))
  
  (test/pos-blame
   '->i-values6
   '((contract (->i ([x number?]) () (values [y boolean?] [z (x) (<=/c x)])) 
               (lambda (x) (values #t (+ x 1))) 'pos 'neg) 1))
  
  (test/spec-passed
   '->i-values7
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (x y) (values #t (- x 1)))
               'pos
               'neg)
     1
     0))
  
  (test/neg-blame
   '->i-values8
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (x y) (values #f (+ x 1)))
               'pos
               'neg)
     1
     2))
  
  (test/spec-passed
   '->i-values9
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (y x) (values #f (- x 1)))
               'pos
               'neg)
     1
     2))
  
  (test/neg-blame
   '->i-values10
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (y x) (values #f (+ x 1))) 'pos 'neg)
     1 0))
  
  (test/spec-passed
   '->i-values11
   '((contract (->i () () #:rest [rest any/c] (values [z boolean?] [w number?])) 
               (lambda x (values #f 1)) 'pos 'neg)))
  
  (test/spec-passed
   '->i-values12
   '((contract (->i ([x number?]) () #:rest [rest any/c] (values [z boolean?] [w number?]))
               (lambda (x . y) (values #f (+ x 1)))
               'pos
               'neg)
     1))
  
  (test/pos-blame
   '->i-values13
   '((contract (->i () () #:rest [rest any/c] (values [z boolean?] [w number?])) 1 'pos 'neg)))
  
  (test/pos-blame
   '->i-values14
   '((contract (->i () () #:rest [rest any/c] (values [z boolean?] [w number?]))
               (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->i-values15
   '((contract (->i ([x number?]) () #:rest [rest any/c]  (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (x . y) (+ x 1)) 'pos 'neg)
     #f))
  
  (test/pos-blame
   '->i-values16
   '((contract (->i ([x number?]) () #:rest [rest any/c] (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (x . y) (values #f (+ x 1))) 'pos 'neg)
     1))
  
  (test/spec-passed
   '->i-values17
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () #:rest [rest any/c]
                    (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (x y . z) (values #f (- x 1))) 'pos 'neg)
     1 0))
  
  (test/neg-blame
   '->i-values18
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () #:rest [rest any/c]
                    (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (x y . z) (values #f (+ x 1))) 'pos 'neg)
     1 2))
  
  (test/spec-passed
   '->i-values19
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () #:rest [rest any/c] 
                    (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (y x . z) (values #f (- x 1))) 'pos 'neg)
     1 2))
  
  (test/neg-blame
   '->i-values20
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () #:rest [rest any/c] 
                    (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (y x . z) (values #f (+ x 1))) 'pos 'neg)
     1 0))
  
  (test/spec-passed
   '->i-values21
   '((contract (->i () () #:rest [rst (listof number?)] (values [z boolean?] [w any/c]))
               (lambda w (values #f 1)) 'pos 'neg) 1))
  
  (test/neg-blame
   '->i-values22
   '((contract (->i () () #:rest [rst (listof number?)] (values [z boolean?] [w any/c]))
               (lambda w (values #f 1)) 'pos 'neg) #f))
  
  (test/spec-passed
   '->i-values23
   '((contract (->i () () (values [x number?] [y (x) (>=/c x)])) (lambda () (values 1 2)) 'pos 'neg)))
  
  (test/pos-blame
   '->i-values24
   '((contract (->i () () (values [x number?] [y (x) (>=/c x)])) (lambda () (values 2 1)) 'pos 'neg)))
  
  (test/spec-passed
   '->i-values25
   '((contract (->i ([x number?]) () (values [z number?] [y (x) (>=/c x)])) 
               (lambda (x) (values 1 2)) 'pos 'neg) 1))
  
  (test/pos-blame
   '->i-values26
   '((contract (->i ([x number?]) () (values [z number?] [y (x) (>=/c x)])) 
               (lambda (x) (values 2 1)) 'pos 'neg) 4))
  
  (test/spec-passed/result
   '->i23
   '((contract (->i ((i number?) (j (i) (and/c number? (>=/c i)))) () [r number?])
               (λ (i j) 1)
               'pos
               'neg)
     1
     2)
   1)
  
  (test/spec-passed/result
   '->i24
   '((contract (->i ([i number?] [j (i) (and/c number? (>=/c i))]) () any)
               (λ (i j) 1)
               'pos
               'neg)
     1
     2)
   1)
  
  (test/spec-passed/result
   '->i25
   '(call-with-values
     (λ ()
       ((contract (->i ((i number?) (j (i) (and/c number? (>=/c i)))) () 
                       (values [x number?] [y number?]))
                  (λ (i j) (values 1 2))
                  'pos
                  'neg)
        1
        2))
     list)
   '(1 2))
  
  (test/spec-passed/result
   '->i26
   '((contract (->i ((i number?) (j (i) (and/c number? (>=/c i)))) () 
                    #:rest [rest-args any/c] [r number?])
               (λ (i j . z) 1)
               'pos
               'neg)
     1
     2)
   1)
  
  (test/spec-passed/result
   '->i27
   '((contract (->i ((i number?) (j (i) (and/c number? (>=/c i)))) () #:rest [rest-args any/c] any)
               (λ (i j . z) 1)
               'pos
               'neg)
     1
     2)
   1)
  
  (test/spec-passed/result
   '->i28
   '(call-with-values
     (λ ()
       ((contract (->i ((i number?) (j (i) (and/c number? (>=/c i)))) ()
                       #:rest [rest-args any/c] 
                       (values [x number?] [y number?]))
                  (λ (i j . z) (values 1 2))
                  'pos
                  'neg)
        1
        2))
     list)
   '(1 2))
  
  (test/neg-blame
   '->i30
   '((contract (->i ([x number?]) () #:rest [rst number?] any)
               (λ (x . rst) (values 4 5))
               'pos
               'neg)
     #f))
  
  (test/spec-passed/result
   '->i34
   '((contract (->i ([x number?]
                     [y (x z) (between/c x z)]
                     [z number?])
                    any)
               (λ (x y z) (+ x y z))
               'pos 'neg)
     1 2 3)
   6)
  
  (test/neg-blame
   '->i35
   '((contract (->i ([x number?]) #:pre () (= 1 2) any)
               (λ (x) 1)
               'pos 'neg) 2))
  
  (test/neg-blame
   '->i35-b
   '((contract (->i ([x number?]) #:pre () #t #:pre () (= 1 2) any)
               (λ (x) 1)
               'pos 'neg) 2))
  
  (test/neg-blame
   '->i35-c
   '((contract (->i ([x number?]) #:pre (x) (even? x) #:pre (x) (positive? x) any)
               (λ (x) 1)
               'pos 'neg) 3))
  
  (test/neg-blame
   '->i35-d
   '((contract (->i ([x number?]) #:pre (x) (even? x) #:pre (x) (positive? x) any)
               (λ (x) 1)
               'pos 'neg) -2))
  
  (test/neg-blame
   '->i35-e
   '((contract (->i ([x any/c]) #:pre (x) (pair? x) #:pre (x) (car x) any)
               (λ (x) 1)
               'pos 'neg)
     (cons #f 1)))
  
  (test/neg-blame
   '->i35-f
   '((contract (->i ([x any/c]) #:pre/name (x) "pair" (pair? x) #:pre/name (x) "car" (car x) any)
               (λ (x) 1)
               'pos 'neg)
     (cons #f 1)))
  
  (test/neg-blame
   '->i35-g
   '((contract (->i ([x any/c]) #:pre/desc (x) (pair? x) #:pre/desc (x) (car x) any)
               (λ (x) 1)
               'pos 'neg)
     (cons #f 1)))
  
  (test/neg-blame
   '->i35-h
   '((contract (->i ([x any/c]) #:pre/desc (x) '("x") any)
               (λ (x) 1)
               'pos 'neg)
     (cons #f 1)))
  
  (test/spec-passed/result
   '->i36
   '((contract (->i ([f (-> number? number?)]) [res number?])
               (λ (f) (f 1))
               'pos 'neg)
     (λ (n) (+ n 1)))
   2)
  
  (test/pos-blame
   '->i37
   '((contract (->i ([f (-> number? number?)]) [res number?])
               (λ (f) #f)
               'pos 'neg)
     (λ (n) (+ n 1))))
  
  (test/spec-passed/result
   '->i38
   '((contract (->i ([x integer?]) () #:rest [rst (listof number?)] [r any/c]) (lambda w w) 'pos 'neg)
     1 2)
   '(1 2))
  
  (test/spec-passed/result
   '->i39
   '((contract (->i (#:x [x integer?]) () #:rest [rst (listof number?)] [r any/c])
               (lambda (#:x x . w) (cons x w)) 'pos 'neg) #:x 1 2)
   '(1 2))
  
  (test/spec-passed/result
   '->i40
   '((contract (->i () ([x integer?]) #:rest [rst (listof number?)] [r any/c])
               (lambda w w) 'pos 'neg) 1 2)
   '(1 2))
  
  (test/spec-passed/result
   '->i41
   '((contract (->i () (#:x [x integer?]) #:rest [rst (listof number?)] [r any/c])
               (lambda (#:x [x 1] . w) (cons x w)) 'pos 'neg) #:x 2 3)
   '(2 3))
  
  (test/spec-passed/result
   '->i42
   '((contract (->i () (#:x [x integer?]) #:rest [rst (listof number?)] [r any/c])
               (lambda (#:x [x 1] . w) (cons x w)) 'pos 'neg)  2 3)
   '(1 2 3))
  
  (test/spec-passed/result
   '->i43
   '(let ([b (box '())])
      ((contract (->i ([i (box/c (listof integer?))])
                      (values [_ (i)
                                 (begin
                                   (set-box! i (cons 1 (unbox i)))
                                   (λ (x)
                                     (set-box! i (cons 4 (unbox i)))
                                     #t))]
                              [_ (i)
                                 (begin
                                   (set-box! i (cons 2 (unbox i)))
                                   (λ (x)
                                     (set-box! i (cons 5 (unbox i)))
                                     #t))]))
                 (λ (i)
                   (set-box! i (cons 3 (unbox i)))
                   (values 2 2))
                 (quote pos)
                 (quote neg))
       b)
      (unbox b))
   '(5 4 3 2 1)
   do-not-double-wrap)
  
  (test/spec-passed/result
   '->i43-double-wrap
   '(let ([b (box '())])
      ((contract (->i ([i (box/c (listof integer?))])
                      (values [_ (i)
                                 (begin
                                   (set-box! i (cons 1 (unbox i)))
                                   (λ (x)
                                     (set-box! i (cons 4 (unbox i)))
                                     #t))]
                              [_ (i)
                                 (begin
                                   (set-box! i (cons 2 (unbox i)))
                                   (λ (x)
                                     (set-box! i (cons 5 (unbox i)))
                                     #t))]))
                 (contract
                  (->i ([i (box/c (listof integer?))])
                      (values [_ (i)
                                 (begin
                                   (set-box! i (cons 1 (unbox i)))
                                   (λ (x)
                                     (set-box! i (cons 4 (unbox i)))
                                     #t))]
                              [_ (i)
                                 (begin
                                   (set-box! i (cons 2 (unbox i)))
                                   (λ (x)
                                     (set-box! i (cons 5 (unbox i)))
                                     #t))]))
                  (λ (i)
                   (set-box! i (cons 3 (unbox i)))
                   (values 2 2))
                  'pos 'neg)
                 (quote pos)
                 (quote neg))
       b)
      (unbox b))
   '(5 4 5 4 3 2 1 2 1)
   do-not-double-wrap)
  
  (test/pos-blame
   '->i45
   '((contract (->i ([x () any/c])
                    [y any/c]
                    #:post (x) x)
               (lambda (x) x)
               'pos
               'neg)
     #f))
  
  (test/spec-passed/result
   '->i46
   '((contract (->i ([x any/c])
                    [y () any/c]
                    #:post (y) y)
               (lambda (x) x)
               'pos
               'neg)
     #t)
   '#t)
  
  (test/pos-blame
   '->i47
   '((contract (->i ([x any/c])
                    [y () any/c]
                    #:post (y) y)
               (lambda (x) x)
               'pos
               'neg)
     #f))
  
  (test/pos-blame
   '->i47-b
   '((contract (->i ([x any/c])
                    [y () any/c]
                    #:post (y) (even? y)
                    #:post (y) (positive? y))
               (lambda (x) x)
               'pos
               'neg)
     -2))
  
  (test/pos-blame
   '->i47-c
   '((contract (->i ([x any/c])
                    [y () any/c]
                    #:post (y) (even? y)
                    #:post (y) (positive? y))
               (lambda (x) x)
               'pos
               'neg)
     3))
  
  (test/pos-blame
   '->i47-d
   '((contract (->i ([x any/c])
                    [y () any/c]
                    #:post (y) (pair? y)
                    #:post (y) (car y))
               (lambda (x) x)
               'pos
               'neg)
     (cons #f 1)))
  
  (test/pos-blame
   '->i47-e
   '((contract (->i ([x any/c])
                    [y () any/c]
                    #:post/name (y) "pair" (pair? y)
                    #:post/name (y) "car" (car y))
               (lambda (x) x)
               'pos
               'neg)
     (cons #f 1)))
  
  (test/pos-blame
   '->i47-f
   '((contract (->i ([x any/c])
                    [y () any/c]
                    #:post/desc (y) (pair? y)
                    #:post/desc (y) (car y))
               (lambda (x) x)
               'pos
               'neg)
     (cons #f 1)))
  
  (test/pos-blame
   '->i47-g
   '((contract (->i ([x any/c])
                    [y () any/c]
                    #:post/desc (y) (pair? y)
                    #:post/desc (y) "x")
               (lambda (x) x)
               'pos
               'neg)
     (cons #f 1)))
  
  (test/spec-passed/result
   '->i48
   '(let ([x '()])
      ((contract (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
                      [res () (begin
                                (set! x (cons 'res-eval x))
                                (λ (res)
                                  (set! x (cons 'res-check x))))])
                 (λ (arg)
                   (set! x (cons 'body x)))
                 'pos
                 'neg)
       1)
      x)
   '(res-check res-eval body arg-eval)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i48-double-wrap
   '(let ([x '()])
      ((contract (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
                      [res () (begin
                                (set! x (cons 'res-eval x))
                                (λ (res)
                                  (set! x (cons 'res-check x))))])
                 (contract
                  (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
                      [res () (begin
                                (set! x (cons 'res-eval x))
                                (λ (res)
                                  (set! x (cons 'res-check x))))])
                  (λ (arg)
                   (set! x (cons 'body x)))
                  'pos 'neg)
                 'pos
                 'neg)
       1)
      x)
   '(res-check res-eval res-check res-eval body arg-eval arg-eval)
   do-not-double-wrap)
  
  (test/spec-passed/result
   '->i49
   '(let ([x '()])
      ((contract (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
                      [_ () (begin
                              (set! x (cons 'res-eval x))
                              (λ (res)
                                (set! x (cons 'res-check x))))])
                 (λ (arg)
                   (set! x (cons 'body x)))
                 'pos
                 'neg)
       1)
      x)
   '(res-check body res-eval arg-eval)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i49-double-wrap
   '(let ([x '()])
      ((contract (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
                      [_ () (begin
                              (set! x (cons 'res-eval x))
                              (λ (res)
                                (set! x (cons 'res-check x))))])
                 (contract
                  (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
                      [_ () (begin
                              (set! x (cons 'res-eval x))
                              (λ (res)
                                (set! x (cons 'res-check x))))])
                  (λ (arg)
                   (set! x (cons 'body x)))
                  'pos 'neg)
                 'pos
                 'neg)
       1)
      x)
   '(res-check res-check body res-eval res-eval arg-eval arg-eval)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i50
   '(let ([x '()])
      ((contract (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
                      [res (begin
                             (set! x (cons 'res-eval x))
                             (λ (res)
                               (set! x (cons 'res-check x))))])
                 (λ (arg)
                   (set! x (cons 'body x)))
                 'pos
                 'neg)
       1)
      x)
   '(res-check body res-eval arg-eval)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i50-double-wrap
   '(let ([x '()])
      ((contract (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
                      [res (begin
                             (set! x (cons 'res-eval x))
                             (λ (res)
                               (set! x (cons 'res-check x))))])
                 (contract
                  (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
                      [res (begin
                             (set! x (cons 'res-eval x))
                             (λ (res)
                               (set! x (cons 'res-check x))))])
                  (λ (arg)
                    (set! x (cons 'body x)))
                  'pos 'neg)
                 'pos
                 'neg)
       1)
      x)
   '(res-check res-check body res-eval arg-eval res-eval arg-eval)
   do-not-double-wrap)
  
  (test/spec-passed/result
   '->i51
   '(let ([x '()])
      ((contract (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
                      [_ (begin
                           (set! x (cons 'res-eval x))
                           (λ (res)
                             (set! x (cons 'res-check x))))])
                 (λ (arg)
                   (set! x (cons 'body x)))
                 'pos
                 'neg)
       1)
      x)
   '(res-check body res-eval arg-eval)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i51-double-wrap
   '(let ([x '()])
      ((contract (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
                      [_ (begin
                           (set! x (cons 'res-eval x))
                           (λ (res)
                             (set! x (cons 'res-check x))))])
                 (contract
                  (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
                      [_ (begin
                           (set! x (cons 'res-eval x))
                           (λ (res)
                             (set! x (cons 'res-check x))))])
                  (λ (arg)
                   (set! x (cons 'body x)))
                  'pos 'neg)
                 'pos
                 'neg)
       1)
      x)
   '(res-check res-check body res-eval arg-eval res-eval arg-eval)
   do-not-double-wrap)
  
  (test/spec-passed/result
   '->i52
   '((contract (->i ()
                    ([x integer?])
                    any)
               (λ ([x 'qq]) x)
               'pos
               'neg))
   'qq)
  
  (test/spec-passed/result
   '->i53
   '((contract (->i ([x (z) (if (equal? z 1) any/c none/c)] [y any/c] [z any/c]) any)
               (λ (x y z) x)
               'pos 'neg)
     3 2 1)
   3)

  (test/spec-passed/result
   '->i54
   '((contract (->i (#:one [one any/c] #:two [two any/c] #:three [three any/c]) any)
               (λ (#:one one #:two two #:three three) (list one two three))
               'pos 'neg)
     #:one 1 #:two 2 #:three 3)
   '(1 2 3))

  (test/spec-passed/result
   '->i55
   '(let ([b '()])
      ((contract (->i ([y () (begin (set! b (cons 1 b)) any/c)]
                       [z (y) (begin (set! b (cons 2 b)) any/c)])
                      any)
                 (λ args (set! b (cons 3 b)) 0)
                 'pos 'neg)
       1 2)
      b)
   '(3 2 1)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i55-double-wrap
   '(let ([b '()])
      ((contract (->i ([y () (begin (set! b (cons 1 b)) any/c)]
                       [z (y) (begin (set! b (cons 2 b)) any/c)])
                      any)
                 (contract
                  (->i ([y () (begin (set! b (cons 1 b)) any/c)]
                       [z (y) (begin (set! b (cons 2 b)) any/c)])
                      any)
                  (λ args (set! b (cons 3 b)) 0)
                  'pos 'neg)
                 'pos 'neg)
       1 2)
      b)
   '(3 2 1 2 1)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i56
   '(let ([b '()])
      ((contract (->i ([y () (begin (set! b (cons 1 b)) any/c)]
                       [z (y) (begin (set! b (cons 2 b)) any/c)])
                      (values
                       [a () (begin (set! b (cons 4 b)) any/c)]
                       [b (a) (begin (set! b (cons 5 b)) any/c)]))
                 (λ args (set! b (cons 3 b)) (values 0 0))
                 'pos 'neg)
       1 2)
      b)
   '(5 4 3 2 1)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i56-double-wrap
   '(let ([b '()])
      ((contract (->i ([y () (begin (set! b (cons 1 b)) any/c)]
                       [z (y) (begin (set! b (cons 2 b)) any/c)])
                      (values
                       [a () (begin (set! b (cons 4 b)) any/c)]
                       [b (a) (begin (set! b (cons 5 b)) any/c)]))
                 (contract
                  (->i ([y () (begin (set! b (cons 1 b)) any/c)]
                       [z (y) (begin (set! b (cons 2 b)) any/c)])
                      (values
                       [a () (begin (set! b (cons 4 b)) any/c)]
                       [b (a) (begin (set! b (cons 5 b)) any/c)]))
                  (λ args (set! b (cons 3 b)) (values 0 0))
                  'pos 'neg)
                 'pos 'neg)
       1 2)
      b)
   '(5 4 5 4 3 2 1 2 1)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i57
   '(let ([b '()])
      ((contract (->i ([y () (begin (set! b (cons 1 b))
                                    (λ (y) (set! b (cons 2 b)) #t))]
                       [z (y) (begin (set! b (cons 3 b))
                                     (λ (y) (set! b (cons 4 b)) #t))])
                      (values
                       [a () (begin (set! b (cons 6 b))
                                    (λ (a) (set! b (cons 7 b)) #t))]
                       [b (a) (begin (set! b (cons 8 b))
                                     (λ (a) (set! b (cons 9 b)) #t))]))
                 (λ args (set! b (cons 5 b)) (values 0 0))
                 'pos 'neg)
       1 2)
      b)
   '(9 8 7 6 5 4 3 2 1)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i57-double-wrap
   '(let ([b '()])
      ((contract (->i ([y () (begin (set! b (cons 1 b))
                                    (λ (y) (set! b (cons 2 b)) #t))]
                       [z (y) (begin (set! b (cons 3 b))
                                     (λ (y) (set! b (cons 4 b)) #t))])
                      (values
                       [a () (begin (set! b (cons 6 b))
                                    (λ (a) (set! b (cons 7 b)) #t))]
                       [b (a) (begin (set! b (cons 8 b))
                                     (λ (a) (set! b (cons 9 b)) #t))]))
                 (contract
                  (->i ([y () (begin (set! b (cons 1 b))
                                    (λ (y) (set! b (cons 2 b)) #t))]
                       [z (y) (begin (set! b (cons 3 b))
                                     (λ (y) (set! b (cons 4 b)) #t))])
                      (values
                       [a () (begin (set! b (cons 6 b))
                                    (λ (a) (set! b (cons 7 b)) #t))]
                       [b (a) (begin (set! b (cons 8 b))
                                     (λ (a) (set! b (cons 9 b)) #t))]))
                  (λ args (set! b (cons 5 b)) (values 0 0))
                  'pos 'neg)
                 'pos 'neg)
       1 2)
      b)
   '(9 8 7 6 9 8 7 6 5 4 3 2 1 4 3 2 1)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i58
   '((contract (->i ()
                    ([x integer?]
                     [y (x) (>=/c x)])
                    [result any/c])
               (λ ([x 0] [y 1]) 2)
               'pos 'neg))
   2)

  (test/spec-passed/result
   '->i59
   '(let ([b '()])
      ((contract (->i ()
                      ([x (begin (set! b (cons 1 b)) integer?)]
                       [y (x) (begin (set! b (cons 'nope b)) (>=/c x))])
                      [result (begin (set! b (cons 2 b)) any/c)])
                 (λ ([x #f] [y #f]) (set! b (cons 3 b)) 0)
                 'pos 'neg))
      b)
   '(3 2 1)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i59-double-wrap
   '(let ([b '()])
      ((contract (->i ()
                      ([x (begin (set! b (cons 1 b)) integer?)]
                       [y (x) (begin (set! b (cons 'nope b)) (>=/c x))])
                      [result (begin (set! b (cons 2 b)) any/c)])
                 (contract
                  (->i ()
                      ([x (begin (set! b (cons 1 b)) integer?)]
                       [y (x) (begin (set! b (cons 'nope b)) (>=/c x))])
                      [result (begin (set! b (cons 2 b)) any/c)])
                  (λ ([x #f] [y #f]) (set! b (cons 3 b)) 0)
                  'pos 'neg)
                 'pos 'neg))
      b)
   '(3 2 1 2 1)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i60
   '(let ([order '()])
      ((contract (->i ([x (λ (xyzpdq) (set! order (cons 0 order)) (integer? xyzpdq))])
                      #:pre (x) (begin (set! order (cons 1 order)) #t)
                      #:pre () (begin (set! order (cons 2 order)) #t)
                      any)
                 (λ (x) x)
                 'pos 'neg)
       1)
      (reverse order))
   ;; we see `0` twice because we check the indy contracts
   '(2 0 0 1)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i61
   '(let ([order '()])
      ((contract (->i ([x (λ (xyzpdq) (set! order (cons 0 order)) (integer? xyzpdq))])
                      #:pre (x) (begin (set! order (cons 1 order)) #t)
                      #:pre () (begin (set! order (cons 2 order)) #t)
                      [res (λ (x) (set! order (cons 3 order)) #t)]
                      #:post () (begin (set! order (cons 4 order)) #t)
                      #:post (x) (begin (set! order (cons 5 order)) #t)
                      #:post (res) (begin (set! order (cons 6 order)) #t))
                 (λ (x) x)
                 'pos 'neg)
       1)
      (reverse order))
   ;; we see `0` and the `3` twice because we check the indy contracts
   '(2 0 0 1 4 5 3 3 6)
   do-not-double-wrap)
  
  (test/spec-passed/result
   '->i62
   '(let* ([p (make-parameter 0)]
           [f (contract (->i ([x integer?])
                             #:param (x) p x
                             any)
                        (λ (_) (p)) 'pos 'neg)])
      (list (f 1) (f 2)))
   '(1 2))

  (test/spec-passed/result
   '->i63
   '(let* ([p (make-parameter 0)]
           [f (contract (->i (#:x [x integer?])
                             (#:y [y integer?])
                             #:rest [rst (listof integer?)]
                             #:param (x y rst) p
                             (+ x (if (unsupplied-arg? y) 0 y) (apply + rst))
                             any)
                        (λ (#:x x #:y [y -100]. rst) (p)) 'pos 'neg)])
      (list (f #:x 1 10 13) (f 10 8 #:x 2 #:y 1)))
   '(24 21))

  (test/spec-passed/result
   '->i64
   '(let* ([p1 (make-parameter 0)]
           [p2 (make-parameter 0)]
           [f (contract (->i ([x integer?]
                              [y integer?])
                             #:param (x) p1 (add1 x)
                             #:param (y) p2 (add1 y)
                             any)
                        (λ (_x _y) (list (p1) (p2))) 'pos 'neg)])
      (list (f 1 2) (f 3 4)))
   '((2 3) (4 5)))

  (test/pos-blame
   '->i-arity1
   '(contract (->i ([x number?]) () any) (λ () 1) 'pos 'neg))
  
  (test/pos-blame
   '->i-arity2
   '(contract (->i ([x number?]) () any) (λ (x #:y y) 1) 'pos 'neg))
  
  (test/spec-passed
   '->i-arity3
   '(contract (->i ([x number?] #:y [y integer?]) () any) (λ (x #:y y) 1) 'pos 'neg))
  
  (test/pos-blame
   '->i-arity4
   '(contract (->i () ([x integer?]) any) (λ (x) 1) 'pos 'neg))
  
  (test/pos-blame
   '->i-arity5
   '(contract (->i () ([x integer?]) any) (λ () 1) 'pos 'neg))
  
  (test/spec-passed
   '->i-arity6
   '(contract (->i () ([x integer?]) any) (λ ([x 1]) 1) 'pos 'neg))
  
  (test/pos-blame
   '->i-arity7
   '(contract (->i () (#:x [x integer?]) any) (λ ([x 1]) 1) 'pos 'neg))
  
  (test/pos-blame
   '->i-arity8
   '(contract (->i () (#:x [x integer?]) any) (λ () 1) 'pos 'neg))
  
  (test/pos-blame
   '->i-arity8
   '(contract (->i () (#:x [x integer?]) any) (λ (#:x x) 1) 'pos 'neg))
  
  (test/spec-passed
   '->i-arity10
   '(contract (->i () (#:x [x integer?]) any) (λ (#:x [x 1]) 1) 'pos 'neg))
  
  (test/pos-blame
   '->i-pp1
   '((contract (->i ([x number?]) () #:pre (x) (= x 1) [result number?] #:post (x) (= x 2))
               (λ (x) x)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   '->i-pp2
   '((contract (->i ([x number?]) () #:pre (x) (= x 1) [result number?] #:post (x) (= x 2))
               (λ (x) x)
               'pos
               'neg)
     2))
  
  (test/pos-blame
   '->i-pp3
   '((contract (->i ([x number?]) () #:pre (x) (= x 1) [result number?] #:post (result) (= result 2))
               (λ (x) x)
               'pos
               'neg)
     1))
  
  (test/spec-passed
   '->i-pp3.5
   '((contract (->i ([x number?]) () #:pre (x) (= x 1) [result number?] #:post (result) (= result 2))
               (λ (x) 2)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   '->i-pp4
   '((contract (->i ([x number?]) () #:pre (x) (= x 1) any)
               (λ (x) x)
               'pos
               'neg)
     2))
  
  (test/neg-blame
   '->i-pp5
   '((contract (->i ([x number?]) () #:pre (x) (= x 1) (values [z number?] [y number?])
                    #:post (x y z) (= x y z 3))
               (λ (x) (values 4 5))
               'pos
               'neg)
     2))
  
  (test/pos-blame
   '->i-pp6
   '((contract (->i ([x number?]) () #:pre (x) (= x 1) (values [z number?] [y number?]) 
                    #:post (z y) (= z y 3))
               (λ (x) (values 4 5))
               'pos
               'neg)
     1))
  
  (test/pos-blame
   '->i-pp-r1
   '((contract (->i ([x number?]) () #:rest [rst any/c] #:pre (x) (= x 1) [result number?] 
                    #:post (x) (= x 2))
               (λ (x . rst) x)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   '->i-pp-r2
   '((contract (->i ([x number?]) () #:rest [rst any/c] 
                    #:pre (x) (= x 1)  [result number?] 
                    #:post (x) (= x 2))
               (λ (x . rst) x)
               'pos
               'neg)
     2))
  
  (test/pos-blame
   '->i-pp-r3
   '((contract (->i ([x number?]) () #:rest [rst any/c] 
                    #:pre (x) (= x 1) [result number?] 
                    #:post (result) (= result 2))
               (λ (x . rst) x)
               'pos
               'neg)
     1))
  
  (test/spec-passed
   '->i-pp-r3.5
   '((contract (->i ([x number?]) () #:rest [rst any/c] 
                    #:pre (x) (= x 1) [result number?] 
                    #:post (result) (= result 2))
               (λ (x . rst) 2)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   '->i-pp-r4
   '((contract (->i ([x number?]) () #:rest [rst any/c] #:pre (x) (= x 1) any)
               (λ (x . rst) x)
               'pos
               'neg)
     2))
  
  (test/neg-blame
   '->i-pp-r5
   '((contract (->i ([x number?]) () #:rest [rst any/c] 
                    #:pre (x) (= x 1) (values [z number?] [y number?]) 
                    #:post (x y z) (= x y z 3))
               (λ (x . rst) (values 4 5))
               'pos
               'neg)
     2))
  
  (test/pos-blame
   '->i-pp-r6
   '((contract (->i ([x number?]) () #:rest [rst any/c] 
                    #:pre (x) (= x 1) (values [z number?] [y number?]) 
                    #:post (x y z) (= z x y 3))
               (λ (x . rst) (values 4 5))
               'pos
               'neg)
     1))
  
  
  ;; test to make sure the values are in the error messages
  (contract-error-test
   '->i-contract-error-test1
   '((contract (->i ([x number?]) #:pre (x) #f any)
               (λ (x) x)
               'pos
               'neg)
     123456789)
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"x: 123456789" (exn-message x)))))
  (contract-error-test
   '->i-contract-error-test2
   '((contract (->i ([|x y| number?]) #:pre (|x y|) #f any)
               (λ (x) x)
               'pos
               'neg)
     123456789)
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match (regexp-quote "|x y|: 123456789") (exn-message x)))))
  
  ;; test to make sure the collects directories are appropriately prefixed
  (contract-error-test
   '->i-contract-error-test3
   '(contract symbol? "not a symbol" 'pos 'neg 'not-a-symbol 
              (datum->syntax
               #f
               'here
               (vector (collection-file-path "base.rkt" "racket")
                       1 1 1 1)))
   (lambda (x)
     (and (exn:fail:contract:blame? x)
          (let ([msg (exn-message x)])
            (define ans (regexp-match? #px"<collects>" msg))
            (unless ans
              (printf "msg: ~s\n" msg))
            ans))))
  
  ;; make sure that ->i checks its arguments
  (contract-error-test
   '->i-contract-error-test4
   '(->i ([x (λ (x y z) #f)]) any)
   exn:fail?)
  
  (contract-error-test
   '->i-contract-error-test5
   '(->i () (values [x (λ (x y z) #f)][y 5]))
   exn:fail?)
  
  (test/neg-blame
   '->i-protect-shared-state
   '(let ([x 1])
      ((contract (let ([save #f])
                   (-> (->i () () #:pre () (set! save x) [range any/c] #:post () (= save x))
                       any))
                 (λ (t) (t))
                 'pos
                 'neg)
       (lambda () (set! x 2)))))
  
  
  (test/spec-passed
   '->i-optopt1
   '((contract (->i ([x number?]) any)
               (λ (x) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   '->i-optopt2
   '((contract (->i ([x number?]) #:rest [rst any/c] any)
               (λ (x . y) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   '->i-optopt3
   '((contract (->i ([x number?]) #:pre () #t any)
               (λ (x) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   '->i-optopt4
   '((contract (->i ([x number?]) #:rest [rst any/c] #:pre () #t any)
               (λ (x . y) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   '->i-optopt5
   '((contract (->i ([x number?]) #:rest [rst any/c] #:pre () #t [res any/c] #:post () #t)
               (λ (x . y) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   '->i-optopt6
   '((contract (->i ([x number?]) #:rest [rst any/c] [res any/c] #:post () #t)
               (λ (x . y) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   '->i-optopt7
   '((contract (->i ([x number?]) #:pre () #t [res any/c] #:post () #t)
               (λ (x . y) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   '->i-optopt8
   '((contract (->i ([x number?]) [res any/c] #:post () #t)
               (λ (x . y) x)
               'pos 'neg)
     1))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  make sure the variables are all bound properly
  ;;
  
  (test/spec-passed
   '->i-binding1
   '((contract (->i ([x number?]) ()
                    #:rest [rest any/c] [range any/c] 
                    #:post (rest) (equal? rest '(2 3 4)))
               (λ (x . y) y)
               'pos
               'neg)
     1 2 3 4))
  
  (test/spec-passed
   '->i-binding2
   '((contract (->i ([x number?]) () #:rest [rest any/c] [range any/c] #:post (x) (equal? x 1))
               (λ (x . y) y)
               'pos
               'neg)
     1 2 3 4))
  
  (test/spec-passed
   '->i-binding3
   '(let ([p 'p]
          [q 'q]
          [r 'r])
      ((contract (->i ([x number?] [y number?] #:z [z number?] #:w [w number?])
                      ([a number?] [b number?] #:c [c number?] #:d [d number?])
                      #:rest [rest any/c]
                      #:pre (x y z w a b c d rest)
                      (equal? (list x y z w a b c d rest p q r)
                              (list 1 2 3 4 5 6 7 8 '(z) 'p 'q 'r))
                      (values [p number?] [q number?] [r number?]))
                 (λ (x y #:z z #:w w [a 101] [b 102] #:c [c 103] #:d [d 104] . rest)
                   (values 11 12 13))
                 'pos
                 'neg)
       1 2 #:z 3 #:w 4 5 6 #:c 7 #:d 8 'z)))
  
  (test/spec-passed
   '->i-binding4
   '((contract (->i ([x number?] [y number?] #:z [z number?] #:w [w number?])
                    ([a number?] [b number?] #:c [c number?] #:d [d number?])
                    #:rest [rest any/c]
                    (values [p number?] [q number?] [r number?])
                    #:post (x y z w a b c d rest p q r)
                    (equal? (list x y z w a b c d rest p q r)
                            (list 1 2 3 4 5 6 7 8 '(z) 11 12 13)))
               (λ (x y #:z z #:w w [a 101] [b 102] #:c [c 103] #:d [d 104] . rest)
                 (values 11 12 13))
               'pos
               'neg)
     1 2 #:z 3 #:w 4 5 6 #:c 7 #:d 8 'z))
  
  (test/spec-passed
   '->i-binding5
   '(let ([p 'p]
          [q 'q]
          [r 'r])
      ((contract (->i ([x number?] [y number?] #:z [z number?] #:w [w number?])
                      ([a number?] [b number?] #:c [c number?] #:d [d number?])
                      #:rest [rest any/c]
                      #:pre (x y z w a b c d rest)
                      (equal? (list x y z w a b c d rest p q r)
                              (list 1 2 3 4
                                    the-unsupplied-arg the-unsupplied-arg
                                    the-unsupplied-arg the-unsupplied-arg
                                    '() 'p 'q 'r))
                      (values [p number?] [q number?] [r number?]))
                 (λ (x y #:z z #:w w [a 101] [b 102] #:c [c 103] #:d [d 104] . rest)
                   (values 11 12 13))
                 'pos
                 'neg)
       1 2 #:z 3 #:w 4)))
  
  (test/spec-passed
   '->i-binding6
   '((contract (->i ([x number?] [y number?] #:z [z number?] #:w [w number?])
                    ([a number?] [b number?] #:c [c number?] #:d [d number?])
                    #:rest [rest any/c]
                    (values [p number?] [q number?] [r number?])
                    #:post (x y z w a b c d rest p q r)
                    (equal? (list x y z w a b c d rest p q r)
                            (list 1 2 3 4
                                  the-unsupplied-arg the-unsupplied-arg
                                  the-unsupplied-arg the-unsupplied-arg
                                  '() 11 12 13)))
               (λ (x y #:z z #:w w [a 101] [b 102] #:c [c 103] #:d [d 104] . rest)
                 (values 11 12 13))
               'pos
               'neg)
     1 2 #:z 3 #:w 4))
  
  ;; test that the rest parameter is right when there aren't 
  ;; enough arguments to even make it to the rest parameter
  (test/spec-passed
   '->i-binding7
   '((contract (->i ()
                    ([a number?])
                    #:rest [rest any/c]
                    [_ any/c]
                    #:post (a rest) (equal? (list a rest) (list the-unsupplied-arg '())))
               (λ ([a 1] . rest) 1)
               'pos
               'neg)))
  
  (test/pos-blame
   '->i-underscore1
   '((contract (->i ([b (box/c integer?)])
                    ()
                    [_ (b)
                       (let ([old (unbox b)])
                         (and/c
                          void?
                          (λ (new)
                            (= old (unbox b)))))])
               (λ (b)
                 (set-box! b (+ (unbox b) 1)))
               'pos
               'neg)
     (box 1)))
  
  (test/spec-passed/result
   '->i-underscore2
   '(let ([x '()])
      ((contract (->i () () [_ (begin (set! x (cons 'ctc x)) any/c)])
                 (λ () (set! x (cons 'body x)))
                 'pos
                 'neg))
      x)
   '(body ctc)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i-underscore2-double-wrap
   '(let ([x '()])
      ((contract (->i () () [_ (begin (set! x (cons 'ctc x)) any/c)])
                 (contract
                  (->i () () [_ (begin (set! x (cons 'ctc x)) any/c)])
                  (λ () (set! x (cons 'body x)))
                  'pos 'neg)
                 'pos
                 'neg))
      x)
   '(body ctc ctc)
   do-not-double-wrap)
  
  (test/spec-passed/result
   '->i-underscore3
   '(let ([x '()])
      ((contract (->i () () [res (begin (set! x (cons 'ctc x)) any/c)])
                 (λ () (set! x (cons 'body x)))
                 'pos
                 'neg))
      x)
   '(body ctc)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i-underscore3-double-wrap
   '(let ([x '()])
      ((contract (->i () () [res (begin (set! x (cons 'ctc x)) any/c)])
                 (contract
                  (->i () () [res (begin (set! x (cons 'ctc x)) any/c)])
                  (λ () (set! x (cons 'body x)))
                  'pos 'neg)
                 'pos
                 'neg))
      x)
   '(body ctc ctc)
   do-not-double-wrap)
  
  (test/spec-passed/result
   '->i-underscore4
   '((contract (->i ([str any/c]) () #:rest [rest (listof any/c)] [_ any/c])
               (λ (x . y) (cons x y))
               'pos 'neg)
     1 2 3)
   '(1 2 3))
  
  (test/spec-passed/result
   '->i-underscore5
   '((contract (->i ([str any/c]) () #:rest [rest (listof any/c)] [_ any/c])
               (λ (x . y) (cons x y))
               'pos 'neg)
     1 2 3 4 5)
   '(1 2 3 4 5))
  
  (test/spec-passed/result
   '->i-underscore6
   '(let ([x '()])
      ((contract (->i ([a integer?]) () [_ (a) (begin (set! x (cons 'ctc x)) any/c)])
                 (λ (a) (set! x (cons 'body x)))
                 'pos
                 'neg)
       11)
      x)
   '(body ctc)
   do-not-double-wrap)

  (test/spec-passed/result
   '->i-underscore6-double-wrap
   '(let ([x '()])
      ((contract (->i ([a integer?]) () [_ (a) (begin (set! x (cons 'ctc x)) any/c)])
                 (contract
                  (->i ([a integer?]) () [_ (a) (begin (set! x (cons 'ctc x)) any/c)])
                  (λ (a) (set! x (cons 'body x)))
                  'pos 'neg)
                 'pos
                 'neg)
       11)
      x)
   '(body ctc ctc)
   do-not-double-wrap)
  
  (test/pos-blame
   '->i-bad-number-of-result-values1
   '((contract (->i ((x any/c)) (result (x) any/c))
               (λ (x) (values 1 2))
               'pos
               'neg)
     1))
  
  (test/pos-blame
   '->i-bad-number-of-result-values2
   '((contract (->i ((giraffe any/c)) (elephant any/c))
               (λ (x) (values 1 2))
               'pos
               'neg)
     1))

  (test/neg-blame
   '->i-neg-party-is-being-passed-properly
   '((contract (-> (->i () any) any)
               (λ (x) 1)
               'pos
               'neg)
     0))
  
  ;; this used to cause a runtime error in the code that parses ->i
  (test/no-error '(->i ([x () any/c] [y (x) any/c]) any))

  (test/spec-passed/result
   'really-chaperones.1
   '(let ([f (λ () 1)])
      (chaperone-of?
       (contract (->i #:chaperone () any) f 'pos 'neg)
       f))
   #t)

  (test/spec-passed/result
   'really-chaperones.2
   '(let ([f (λ () 1)])
      (chaperone-of?
       (contract (->i () [_ (new-∀/c)]) f 'pos 'neg)
       f))
   #f)

  (test/spec-passed/result
   'really-chaperones.3
   '(with-handlers ([exn:fail?
                     (λ (x)
                       (regexp-match? #rx"^->i:.*chaperone" (exn-message x)))])
      ((contract (->i #:chaperone ([x integer?] [y (x) (new-∀/c)]) any)
                 (λ (x y) x)
                 'pos 'neg) 1 2)
      "didn't raise an error")
   #t)

  (test/spec-passed/result
   'shortcut-error-message-1
   '(with-handlers ([exn:fail?
                     (λ (x) (define m
                              (regexp-match #rx"expected: ([^\n]*)\n"
                                            (exn-message x)))
                       (if m
                           (list-ref m 1)
                           (format "ack regexp didn't match: ~s"
                                   (exn-message x))))])
      ((contract (->i ([y () (and/c number? (>/c 1))]) any)
                  (λ (y) 1)
                  'pos 'neg)
        1))
   "a number strictly greater than 1")

  (contract-error-test
   'shortcut-error-message-2
   '(let ()
      (define ctc
        (make-flat-contract
         #:first-order (λ (x) #f)
         #:late-neg-projection
         (λ (b)
           (λ (x neg-party)
             (raise-blame-error
              b x #:missing-party neg-party
              "an informative error message")))))
      ((contract (->i ([x any/c])
                      [_ (x) ctc])
                 (λ (x) 42)
                 'pos 'neg)
       10))
    (λ (x)
      (and (exn:fail:contract:blame? x)
           (regexp-match? #rx"an informative error message"
                          (exn-message x)))))

  (test/spec-passed/result
   'two-underscores
   '((contract (->i ([abc any/c] [_ (abc) any/c]) [_ () any/c])
               (λ (abc whatevs) whatevs)
               'pos 'neg)
     441 144)
   144)

  (test/spec-passed/result
   'many-underscores
   '(call-with-values
     (λ () ((contract (->i ([abc any/c] [_ (abc) any/c])
                           (values [_ () any/c]
                                   [_ () any/c]
                                   [_ () any/c]
                                   [_ () any/c]))
                      (λ (abc whatevs)
                        (values 1 2 3 4))
                      'pos 'neg)
            55 66))
     list)
   '(1 2 3 4))
  )
