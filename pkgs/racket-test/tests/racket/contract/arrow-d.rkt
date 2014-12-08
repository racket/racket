#lang racket/base

(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace)])
  
  (test/no-error '(->d ([x integer?]) ([y integer?]) any))
  (test/no-error '(->d ([x integer?]) ([y integer?]) (values [a number?] [b boolean?])))
  (test/no-error '(->d ([x integer?] #:z [z integer?]) ([y integer?] #:w [w integer?])
                       (range boolean?)))
  (test/no-error '(->d ([x integer?] #:z [z integer?]) ([y integer?] #:w [w integer?])
                       #:rest rest any/c (range boolean?)))
  (test/no-error '(->d ([x integer?] #:z [z integer?]) #:rest rest any/c (range boolean?)))
  
  
  (test/spec-passed
   '->d1
   '((contract (->d () () [x number?]) (lambda () 1) 'pos 'neg)))
  
  (test/spec-passed
   '->d2
   '((contract (->d ([x number?]) () (values [r number?])) (lambda (x) (+ x 1)) 'pos 'neg) 1))
  
  (test/pos-blame
   '->d3
   '((contract (->d () () [r number?]) 1 'pos 'neg)))
  
  (test/pos-blame
   '->d4
   '((contract (->d () () [r number?]) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->d5
   '((contract (->d ([x number?]) () any) (lambda (x) (+ x 1)) 'pos 'neg) #f))
  
  (test/pos-blame
   '->d6
   '((contract (->d ([x number?]) () [r (<=/c x)]) (lambda (x) (+ x 1)) 'pos 'neg) 1))
  
  (test/spec-passed
   '->d7
   '((contract (->d ([x number?] [y (<=/c x)]) () [r (<=/c x)]) 
               (lambda (x y) (- x 1)) 'pos 'neg) 1 0))
  
  (test/neg-blame
   '->d8
   '((contract (->d ([x number?] [y (<=/c x)]) () [r (<=/c x)]) 
               (lambda (x y) (+ x 1)) 'pos 'neg) 1 2))
  
  (test/spec-passed
   '->d9
   '((contract (->d ([y (<=/c x)] [x number?]) () [r (<=/c x)]) 
               (lambda (y x) (- x 1)) 'pos 'neg) 1 2))
  
  (test/neg-blame
   '->d10
   '((contract (->d ([y (<=/c x)] [x number?]) () [r (<=/c x)]) 
               (lambda (y x) (+ x 1)) 'pos 'neg) 1 0))
  
  (test/spec-passed
   '->d11
   '((contract (->d () () #:rest rest any/c [r number?]) (lambda x 1) 'pos 'neg)))
  
  (test/spec-passed
   '->d12
   '((contract (->d ([x number?]) () #:rest rest any/c [r number?]) 
               (lambda (x . y) (+ x 1)) 'pos 'neg) 1))
  
  (test/pos-blame
   '->d13
   '((contract (->d () () #:rest rest any/c [r number?]) 1 'pos 'neg)))
  
  (test/pos-blame
   '->d14
   '((contract (->d () () #:rest rest any/c [r number?]) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->d15
   '((contract (->d ([x number?]) () #:rest rest any/c any) (lambda (x . y) (+ x 1)) 'pos 'neg) #f))
  
  (test/pos-blame
   '->d16
   '((contract (->d ([x number?]) () #:rest rest any/c [r (<=/c x)]) 
               (lambda (x . y) (+ x 1)) 'pos 'neg) 1))
  
  (test/spec-passed
   '->d17
   '((contract (->d ([x number?] [y (<=/c x)]) () #:rest rest any/c [r (<=/c x)]) 
               (lambda (x y . z) (- x 1)) 'pos 'neg) 1 0))
  
  (test/neg-blame
   '->d18
   '((contract (->d ([x number?] [y (<=/c x)]) () #:rest rest any/c [r (<=/c x)]) 
               (lambda (x y . z) (+ x 1)) 'pos 'neg) 1 2))
  
  (test/spec-passed
   '->d19
   '((contract (->d ([y (<=/c x)] [x number?]) () #:rest rest any/c [r (<=/c x)]) 
               (lambda (y x . z) (- x 1)) 'pos 'neg) 1 2))
  
  (test/neg-blame
   '->d20
   '((contract (->d ([y (<=/c x)] [x number?]) () #:rest rest any/c [r (<=/c x)]) 
               (lambda (y x . z) (+ x 1)) 'pos 'neg) 1 0))
  
  (test/spec-passed
   '->d21
   '((contract (->d () () #:rest rst (listof number?) [r any/c]) (lambda w 1) 'pos 'neg) 1))
  
  (test/neg-blame
   '->d22
   '((contract (->d () () #:rest rst (listof number?) [r any/c]) (lambda w 1) 'pos 'neg) #f))
  
  (test/spec-passed
   '->d-any1
   '((contract (->d () () any) (lambda () 1) 'pos 'neg)))
  
  (test/spec-passed
   '->d-any2
   '((contract (->d ([x number?]) () any) (lambda (x) (+ x 1)) 'pos 'neg) 1))
  
  (test/pos-blame
   '->d-any3
   '((contract (->d () () any) 1 'pos 'neg)))
  
  (test/pos-blame
   '->d-any4
   '((contract (->d () () any) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->d-any5
   '((contract (->d ([x number?]) () any) (lambda (x) (+ x 1)) 'pos 'neg) #f))
  
  (test/spec-passed
   '->d-any6
   '((contract (->d ([x number?] [y (<=/c x)]) () any) (lambda (x y) (- x 1)) 'pos 'neg) 1 0))
  
  (test/neg-blame
   '->d-any7
   '((contract (->d ([x number?] [y (<=/c x)]) () any) (lambda (x y) (+ x 1)) 'pos 'neg) 1 2))
  
  (test/spec-passed
   '->d-any8
   '((contract (->d ([y (<=/c x)] [x number?]) () any) (lambda (y x) (- x 1)) 'pos 'neg) 1 2))
  
  (test/neg-blame
   '->d-any9
   '((contract (->d ([y (<=/c x)] [x number?]) () any) (lambda (y x) (+ x 1)) 'pos 'neg) 1 0))
  
  (test/spec-passed
   '->d-any10
   '((contract (->d () () #:rest rest any/c any) (lambda x 1) 'pos 'neg)))
  
  (test/spec-passed
   '->d-any11
   '((contract (->d ([x number?]) () #:rest rest any/c any) (lambda (x . y) (+ x 1)) 'pos 'neg) 1))
  
  (test/pos-blame
   '->d-any12
   '((contract (->d () () #:rest rest any/c any) 1 'pos 'neg)))
  
  (test/pos-blame
   '->d-any13
   '((contract (->d () () #:rest rest any/c any) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->d-any14
   '((contract (->d ([x number?]) () #:rest rest any/c any) (lambda (x . y) (+ x 1)) 'pos 'neg) #f))
  
  (test/spec-passed
   '->d-any15
   '((contract (->d ([x number?] [y (<=/c x)]) () #:rest rest any/c any) 
               (lambda (x y . z) (- x 1)) 'pos 'neg) 1 0))
  
  (test/neg-blame
   '->d-any16
   '((contract (->d ([x number?] [y (<=/c x)]) () #:rest rest any/c any)
               (lambda (x y . z) (+ x 1)) 'pos 'neg) 1 2))
  
  (test/spec-passed
   '->d-any17
   '((contract (->d ([y (<=/c x)] [x number?]) () #:rest rest any/c any)
               (lambda (y x . z) (- x 1)) 'pos 'neg) 1 2))
  
  (test/neg-blame
   '->d-any18
   '((contract (->d ([y (<=/c x)] [x number?]) () #:rest rest any/c any)
               (lambda (y x . z) (+ x 1)) 'pos 'neg) 1 0))
  
  (test/spec-passed
   '->d-any19
   '((contract (->d () () #:rest rst (listof number?) any) (lambda w 1) 'pos 'neg) 1))
  
  (test/neg-blame
   '->d-any20
   '((contract (->d () () #:rest rst (listof number?) any) (lambda w 1) 'pos 'neg) #f))
  
  (test/spec-passed
   '->d-values1
   '((contract (->d () () (values [x boolean?] [y number?])) (lambda () (values #t 1)) 'pos 'neg)))
  
  (test/spec-passed
   '->d-values2
   '((contract (->d ([x number?]) () (values [z boolean?] [y number?])) 
               (lambda (x) (values #t (+ x 1))) 'pos 'neg) 1))
  
  (test/pos-blame
   '->d-values3
   '((contract (->d () () (values [x boolean?] [y number?])) 1 'pos 'neg)))
  
  (test/pos-blame
   '->d-values4
   '((contract (->d () () (values [x boolean?] [y number?])) (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->d-values5
   '((contract (->d ([x number?]) () (values [y boolean?] [z (<=/c x)]))
               (lambda (x) (+ x 1)) 'pos 'neg) #f))
  
  (test/pos-blame
   '->d-values6
   '((contract (->d ([x number?]) () (values [y boolean?] [z (<=/c x)])) 
               (lambda (x) (values #t (+ x 1))) 'pos 'neg) 1))
  
  (test/spec-passed
   '->d-values7
   '((contract (->d ([x number?] [y (<=/c x)]) () (values [z boolean?] [w (<=/c x)]))
               (lambda (x y) (values #t (- x 1)))
               'pos
               'neg)
     1
     0))
  
  (test/neg-blame
   '->d-values8
   '((contract (->d ([x number?] [y (<=/c x)]) () (values [z boolean?] [w (<=/c x)]))
               (lambda (x y) (values #f (+ x 1)))
               'pos
               'neg)
     1
     2))
  
  (test/spec-passed
   '->d-values9
   '((contract (->d ([y (<=/c x)] [x number?]) () (values [z boolean?] [w (<=/c x)]))
               (lambda (y x) (values #f (- x 1)))
               'pos
               'neg)
     1
     2))
  
  (test/neg-blame
   '->d-values10
   '((contract (->d ([y (<=/c x)] [x number?]) () (values [z boolean?] [w (<=/c x)]))
               (lambda (y x) (values #f (+ x 1))) 'pos 'neg)
     1 0))
  
  (test/spec-passed
   '->d-values11
   '((contract (->d () () #:rest rest any/c (values [z boolean?] [w number?]))
               (lambda x (values #f 1)) 'pos 'neg)))
  
  (test/spec-passed
   '->d-values12
   '((contract (->d ([x number?]) () #:rest rest any/c (values [z boolean?] [w number?]))
               (lambda (x . y) (values #f (+ x 1)))
               'pos
               'neg)
     1))
  
  (test/pos-blame
   '->d-values13
   '((contract (->d () () #:rest rest any/c (values [z boolean?] [w number?])) 1 'pos 'neg)))
  
  (test/pos-blame
   '->d-values14
   '((contract (->d () () #:rest rest any/c (values [z boolean?] [w number?]))
               (lambda (x) x) 'pos 'neg)))
  
  (test/neg-blame
   '->d-values15
   '((contract (->d ([x number?]) () #:rest rest any/c  (values [z boolean?] [w (<=/c x)]))
               (lambda (x . y) (+ x 1)) 'pos 'neg)
     #f))
  
  (test/pos-blame
   '->d-values16
   '((contract (->d ([x number?]) () #:rest rest any/c (values [z boolean?] [w (<=/c x)]))
               (lambda (x . y) (values #f (+ x 1))) 'pos 'neg)
     1))
  
  (test/spec-passed
   '->d-values17
   '((contract (->d ([x number?] [y (<=/c x)]) () #:rest rest any/c 
                    (values [z boolean?] [w (<=/c x)]))
               (lambda (x y . z) (values #f (- x 1))) 'pos 'neg)
     1 0))
  
  (test/neg-blame
   '->d-values18
   '((contract (->d ([x number?] [y (<=/c x)]) () #:rest rest any/c 
                    (values [z boolean?] [w (<=/c x)]))
               (lambda (x y . z) (values #f (+ x 1))) 'pos 'neg)
     1 2))
  
  (test/spec-passed
   '->d-values19
   '((contract (->d ([y (<=/c x)] [x number?]) () #:rest rest any/c  
                    (values [z boolean?] [w (<=/c x)]))
               (lambda (y x . z) (values #f (- x 1))) 'pos 'neg)
     1 2))
  
  (test/neg-blame
   '->d-values20
   '((contract (->d ([y (<=/c x)] [x number?]) () #:rest rest any/c  
                    (values [z boolean?] [w (<=/c x)]))
               (lambda (y x . z) (values #f (+ x 1))) 'pos 'neg)
     1 0))
  
  (test/spec-passed
   '->d-values21
   '((contract (->d () () #:rest rst (listof number?) (values [z boolean?] [w any/c]))
               (lambda w (values #f 1)) 'pos 'neg) 1))
  
  (test/neg-blame
   '->d-values22
   '((contract (->d () () #:rest rst (listof number?) (values [z boolean?] [w any/c])) 
               (lambda w (values #f 1)) 'pos 'neg) #f))
  
  (test/spec-passed
   '->d-values23
   '((contract (->d () () (values [x number?] [y (>=/c x)])) (lambda () (values 1 2)) 'pos 'neg)))
  
  (test/pos-blame
   '->d-values24
   '((contract (->d () () (values [x number?] [y (>=/c x)])) (lambda () (values 2 1)) 'pos 'neg)))
  
  (test/spec-passed
   '->d-values25
   '((contract (->d ([x number?]) () (values [z number?] [y (>=/c x)])) 
               (lambda (x) (values 1 2)) 'pos 'neg) 1))
  
  (test/pos-blame
   '->d-values26
   '((contract (->d ([x number?]) () (values [z number?] [y (>=/c x)])) 
               (lambda (x) (values 2 1)) 'pos 'neg) 4))
  
  (test/spec-passed/result
   '->d23
   '((contract (->d ((i number?) (j (and/c number? (>=/c i)))) () [r number?])
               (λ (i j) 1)
               'pos
               'neg)
     1
     2)
   1)
  
  (test/spec-passed/result
   '->d24
   '((contract (->d ((i number?) (j (and/c number? (>=/c i)))) () any)
               (λ (i j) 1)
               'pos
               'neg)
     1
     2)
   1)
  
  (test/spec-passed/result
   '->d25
   '(call-with-values
     (λ ()
       ((contract (->d ((i number?) (j (and/c number? (>=/c i)))) () (values [x number?] [y number?]))
                  (λ (i j) (values 1 2))
                  'pos
                  'neg)
        1
        2))
     list)
   '(1 2))
  
  (test/spec-passed/result
   '->d26
   '((contract (->d ((i number?) (j (and/c number? (>=/c i)))) () #:rest rest-args any/c [r number?])
               (λ (i j . z) 1)
               'pos
               'neg)
     1
     2)
   1)
  
  (test/spec-passed/result
   '->d27
   '((contract (->d ((i number?) (j (and/c number? (>=/c i)))) () #:rest rest-args any/c any)
               (λ (i j . z) 1)
               'pos
               'neg)
     1
     2)
   1)
  
  (test/spec-passed/result
   '->d28
   '(call-with-values
     (λ ()
       ((contract (->d ((i number?) (j (and/c number? (>=/c i)))) () 
                       #:rest rest-args any/c
                       (values [x number?] [y number?]))
                  (λ (i j . z) (values 1 2))
                  'pos
                  'neg)
        1
        2))
     list)
   '(1 2))
  
  (test/neg-blame
   '->d30
   '((contract (->d ([x number?]) () #:rest rst number? any)
               (λ (x . rst) (values 4 5))
               'pos
               'neg)
     #f))
  
  (test/pos-blame
   '->d-arity1
   '(contract (->d ([x number?]) () any) (λ () 1) 'pos 'neg))
  
  (test/pos-blame
   '->d-arity2
   '(contract (->d ([x number?]) () any) (λ (x #:y y) 1) 'pos 'neg))
  
  (test/spec-passed
   '->d-arity3
   '(contract (->d ([x number?] #:y [y integer?]) () any) (λ (x #:y y) 1) 'pos 'neg))
  
  (test/pos-blame
   '->d-arity4
   '(contract (->d () ([x integer?]) any) (λ (x) 1) 'pos 'neg))
  
  (test/pos-blame
   '->d-arity5
   '(contract (->d () ([x integer?]) any) (λ () 1) 'pos 'neg))
  
  (test/spec-passed
   '->d-arity6
   '(contract (->d () ([x integer?]) any) (λ ([x 1]) 1) 'pos 'neg))
  
  (test/pos-blame
   '->d-arity7
   '(contract (->d () (#:x [x integer?]) any) (λ ([x 1]) 1) 'pos 'neg))
  
  (test/pos-blame
   '->d-arity8
   '(contract (->d () (#:x [x integer?]) any) (λ () 1) 'pos 'neg))
  
  (test/pos-blame
   '->d-arity8
   '(contract (->d () (#:x [x integer?]) any) (λ (#:x x) 1) 'pos 'neg))
  
  (test/spec-passed
   '->d-arity10
   '(contract (->d () (#:x [x integer?]) any) (λ (#:x [x 1]) 1) 'pos 'neg))
  
  (test/spec-passed
   '->d-pp0
   '((contract (->d ([x number?]) () #:pre (= x 1) [result number?] #:post (= x 1))
               (λ (x) x)
               'pos
               'neg)
     1))
  
  (test/pos-blame
   '->d-pp1
   '((contract (->d ([x number?]) () #:pre-cond (= x 1) [result number?] #:post-cond (= x 2))
               (λ (x) x)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   '->d-pp2
   '((contract (->d ([x number?]) () #:pre-cond (= x 1) [result number?] #:post-cond (= x 2))
               (λ (x) x)
               'pos
               'neg)
     2))
  
  (test/pos-blame
   '->d-pp3
   '((contract (->d ([x number?]) () #:pre-cond (= x 1) [result number?] #:post-cond (= result 2))
               (λ (x) x)
               'pos
               'neg)
     1))
  
  (test/spec-passed
   '->d-pp3.5
   '((contract (->d ([x number?]) () #:pre-cond (= x 1) [result number?] #:post-cond (= result 2))
               (λ (x) 2)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   '->d-pp4
   '((contract (->d ([x number?]) () #:pre-cond (= x 1) any)
               (λ (x) x)
               'pos
               'neg)
     2))
  
  (test/neg-blame
   '->d-pp5
   '((contract (->d ([x number?]) () #:pre-cond (= x 1) (values [z number?] [y number?])
                    #:post-cond (= x y z 3))
               (λ (x) (values 4 5))
               'pos
               'neg)
     2))
  
  (test/pos-blame
   '->d-pp6
   '((contract (->d ([x number?]) () #:pre-cond (= x 1) (values [z number?] [y number?]) 
                    #:post-cond (= z y 3))
               (λ (x) (values 4 5))
               'pos
               'neg)
     1))
  
  (test/pos-blame
   '->d-pp-r1
   '((contract (->d ([x number?]) () #:rest rst any/c #:pre-cond (= x 1) [result number?] 
                    #:post-cond (= x 2))
               (λ (x . rst) x)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   '->d-pp-r2
   '((contract (->d ([x number?]) () #:rest rst any/c #:pre-cond (= x 1)  [result number?]
                    #:post-cond (= x 2))
               (λ (x . rst) x)
               'pos
               'neg)
     2))
  
  (test/pos-blame
   '->d-pp-r3
   '((contract (->d ([x number?]) () #:rest rst any/c #:pre-cond (= x 1) [result number?] 
                    #:post-cond (= result 2))
               (λ (x . rst) x)
               'pos
               'neg)
     1))
  
  (test/spec-passed
   '->d-pp-r3.5
   '((contract (->d ([x number?]) () #:rest rst any/c #:pre-cond (= x 1) [result number?] 
                    #:post-cond (= result 2))
               (λ (x . rst) 2)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   '->d-pp-r4
   '((contract (->d ([x number?]) () #:rest rst any/c #:pre-cond (= x 1) any)
               (λ (x . rst) x)
               'pos
               'neg)
     2))
  
  (test/neg-blame
   '->d-pp-r5
   '((contract (->d ([x number?]) () #:rest rst any/c 
                    #:pre-cond (= x 1) (values [z number?] [y number?]) 
                    #:post-cond (= x y z 3))
               (λ (x . rst) (values 4 5))
               'pos
               'neg)
     2))
  
  (test/pos-blame
   '->d-pp-r6
   '((contract (->d ([x number?]) () #:rest rst any/c 
                    #:pre-cond (= x 1) (values [z number?] [y number?]) 
                    #:post-cond (= z x y 3))
               (λ (x . rst) (values 4 5))
               'pos
               'neg)
     1))
  
  (test/neg-blame
   '->d-protect-shared-state
   '(let ([x 1])
      ((contract (let ([save #f])
                   (-> (->d () () #:pre-cond (set! save x) [range any/c] #:post-cond (= save x))
                       any))
                 (λ (t) (t))
                 'pos
                 'neg)
       (lambda () (set! x 2)))))
  
  
  (test/spec-passed
   '->d-optopt1
   '((contract (->d ([x number?]) any)
               (λ (x) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   '->d-optopt2
   '((contract (->d ([x number?]) #:rest rst any/c any)
               (λ (x . y) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   '->d-optopt3
   '((contract (->d ([x number?]) #:pre-cond #t any)
               (λ (x) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   '->d-optopt4
   '((contract (->d ([x number?]) #:rest rst any/c #:pre-cond #t any)
               (λ (x . y) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   '->d-optopt5
   '((contract (->d ([x number?]) #:rest rst any/c #:pre-cond #t [res any/c] #:post-cond #t)
               (λ (x . y) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   '->d-optopt6
   '((contract (->d ([x number?]) #:rest rst any/c [res any/c] #:post-cond #t)
               (λ (x . y) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   '->d-optopt7
   '((contract (->d ([x number?]) #:pre-cond #t [res any/c] #:post-cond #t)
               (λ (x . y) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   '->d-optopt8
   '((contract (->d ([x number?]) [res any/c] #:post-cond #t)
               (λ (x . y) x)
               'pos 'neg)
     1))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  make sure the variables are all bound properly
  ;;
  
  (test/spec-passed
   '->d-binding1
   '((contract (->d ([x number?]) () #:rest rest any/c [range any/c] 
                    #:post-cond (equal? rest '(2 3 4)))
               (λ (x . y) y)
               'pos
               'neg)
     1 2 3 4))
  
  (test/spec-passed
   '->d-binding2
   '((contract (->d ([x number?]) () #:rest rest any/c [range any/c] #:post-cond (equal? x 1))
               (λ (x . y) y)
               'pos
               'neg)
     1 2 3 4))
  
  (test/spec-passed
   '->d-binding3
   '(let ([p 'p]
          [q 'q]
          [r 'r])
      ((contract (->d ([x number?] [y number?] #:z [z number?] #:w [w number?])
                      ([a number?] [b number?] #:c [c number?] #:d [d number?])
                      #:rest rest any/c
                      #:pre-cond (equal? (list x y z w a b c d rest p q r)
                                         (list 1 2 3 4 5 6 7 8 '(z) 'p 'q 'r))
                      (values [p number?] [q number?] [r number?]))
                 (λ (x y #:z z #:w w [a 101] [b 102] #:c [c 103] #:d [d 104] . rest)
                   (values 11 12 13))
                 'pos
                 'neg)
       1 2 #:z 3 #:w 4 5 6 #:c 7 #:d 8 'z)))
  
  (test/spec-passed
   '->d-binding4
   '((contract (->d ([x number?] [y number?] #:z [z number?] #:w [w number?])
                    ([a number?] [b number?] #:c [c number?] #:d [d number?])
                    #:rest rest any/c
                    (values [p number?] [q number?] [r number?])
                    #:post-cond (equal? (list x y z w a b c d rest p q r)
                                        (list 1 2 3 4 5 6 7 8 '(z) 11 12 13)))
               (λ (x y #:z z #:w w [a 101] [b 102] #:c [c 103] #:d [d 104] . rest)
                 (values 11 12 13))
               'pos
               'neg)
     1 2 #:z 3 #:w 4 5 6 #:c 7 #:d 8 'z))
  
  (test/spec-passed
   '->d-binding5
   '(let ([p 'p]
          [q 'q]
          [r 'r])
      ((contract (->d ([x number?] [y number?] #:z [z number?] #:w [w number?])
                      ([a number?] [b number?] #:c [c number?] #:d [d number?])
                      #:rest rest any/c
                      #:pre-cond (equal? (list x y z w a b c d rest p q r)
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
   '->d-binding6
   '((contract (->d ([x number?] [y number?] #:z [z number?] #:w [w number?])
                    ([a number?] [b number?] #:c [c number?] #:d [d number?])
                    #:rest rest any/c
                    (values [p number?] [q number?] [r number?])
                    #:post-cond (equal? (list x y z w a b c d rest p q r)
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
   '->d-binding7
   '((contract (->d ()
                    ([a number?])
                    #:rest rest any/c
                    [_ any/c]
                    #:post-cond (equal? (list a rest) (list the-unsupplied-arg '())))
               (λ ([a 1] . rest) 1)
               'pos
               'neg)))
  
  (test/pos-blame
   '->d-underscore1
   '((contract (->d ([b (box/c integer?)])
                    ()
                    [_ (let ([old (unbox b)])
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
   '->d-underscore2
   '(let ([x '()])
      ((contract (->d () () [_ (begin (set! x (cons 'ctc x)) any/c)])
                 (λ () (set! x (cons 'body x)))
                 'pos
                 'neg))
      x)
   '(body ctc))
  
  (test/spec-passed/result
   '->d-underscore3
   '(let ([x '()])
      ((contract (->d () () [res (begin (set! x (cons 'ctc x)) any/c)])
                 (λ () (set! x (cons 'body x)))
                 'pos
                 'neg))
      x)
   '(ctc body))
  
  (test/spec-passed/result
   '->d-underscore4
   '((contract (->d ([str any/c]) () #:rest rest (listof any/c) [_ any/c])
               (λ (x . y) (cons x y))
               'pos 'neg)
     1 2 3)
   '(1 2 3))
  
  (test/spec-passed/result
   '->d-underscore5
   '((contract (->d ([str any/c]) () #:rest rest (listof any/c) [_ any/c])
               (λ (x . y) (cons x y))
               'pos 'neg)
     1 2 3 4 5)
   '(1 2 3 4 5)))
