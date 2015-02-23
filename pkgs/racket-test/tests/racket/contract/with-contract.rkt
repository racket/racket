#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract)])
  
  (test/spec-passed
   'with-contract-def-1
   '(let ()
      (with-contract odd-even
                     ([oddp (-> number? boolean?)]
                      [evenp (-> number? boolean?)])
                     (define (oddp n)
                       (if (zero? n) #f (evenp (sub1 n))))
                     (define (evenp n)
                       (if (zero? n) #t (oddp (sub1 n)))))
      (oddp 5)))
  
  (test/spec-failed
   'with-contract-def-2
   '(let ()
      (with-contract odd-even
                     ([oddp (-> number? boolean?)]
                      [evenp (-> number? boolean?)])
                     (define (oddp n)
                       (if (zero? n) #f (evenp (sub1 n))))
                     (define (evenp n)
                       (if (zero? n) #t (oddp (sub1 n)))))
      (oddp #t))
   "top-level")
  
  (test/spec-failed
   'with-contract-def-3
   '(let ()
      (with-contract odd-even
                     ([oddp (-> number? boolean?)]
                      [evenp (-> number? boolean?)])
                     (define (oddp n)
                       (if (zero? n) n (evenp (sub1 n))))
                     (define (evenp n)
                       (if (zero? n) #t (oddp (sub1 n)))))
      (oddp 4))
   "(region odd-even)")
  
  ;; Functions within the same with-contract region can call
  ;; each other however they want, so here we have even?
  ;; call odd? with a boolean, even though its contract in
  ;; the odd-even contract says it only takes numbers.
  (test/spec-passed
   'with-contract-def-4
   '(let ()
      (with-contract odd-even
                     ([oddp (-> number? boolean?)]
                      [evenp (-> number? boolean?)])
                     (define (oddp n)
                       (cond
                         [(not (number? n)) #f]
                         [(zero? n) #f]
                         [else (evenp (sub1 n))]))
                     (define (evenp n)
                       (if (zero? n) #t (oddp (zero? n)))))
      (oddp 5)))
  
  (test/spec-passed
   'with-contract-def-5
   '(let ()
      (with-contract region1
                     ([x (-> number? number?)])
                     (with-contract region2
                                    ([y (-> number? boolean?)])
                                    (define (y n) #t))
                     (define (x n) (if (y n) 0 3)))
      (x 4)))
  
  (test/spec-failed
   'with-contract-def-6
   '(let ()
      (with-contract region1
                     ([x (-> number? number?)])
                     (with-contract region2
                                    ([y (-> number? boolean?)])
                                    (define (y n) #t))
                     (define (x n) (y n)))
      (x 4))
   "(region region1)")
  
  (test/spec-failed
   'with-contract-def-7
   '(let ()
      (with-contract region1
                     ([x (-> number? number?)])
                     (with-contract region2
                                    ([y (-> number? boolean?)])
                                    (define (y n) #t))
                     (define (x n) (if (y #t) 4 0)))
      (x 4))
   "(region region1)")
  
  (test/spec-failed
   'with-contract-def-8
   '(let ()
      (with-contract region1
                     ([x (-> number? number?)])
                     (with-contract region2
                                    ([y (-> number? boolean?)])
                                    (define (y n) 3))
                     (define (x n) (if (y n) 4 0)))
      (x 4))
   "(region region2)")
  
  ;; make sure uncontracted exports make it out
  (test/spec-passed
   'with-contract-def-9
   '(let ()
      (with-contract region1 ()
                     (define f 3))
      f))
  
  (test/spec-failed
   'with-contract-def-10
   '(let ()
      (with-contract r
                     ([x number?])
                     (define x 3)
                     (define-values ()
                       (begin (set! x #f) (values))))
      x)
   "(region r)")
  
  (test/spec-failed
   'with-contract-def-11
   '(let ()
      (with-contract r
                     ([x number?])
                     (define x 3))
      (set! x #f)
      x)
   "top-level")
  
  (test/spec-passed
   'with-contract-exp-1
   '(with-contract r
                   #:result number?
                   3))
  
  (test/spec-failed
   'with-contract-exp-2
   '(with-contract r
                   #:result number?
                   "foo")
   "(region r)")
  
  (test/spec-passed
   'with-contract-exp-3
   '((with-contract r
                    #:result (-> number? number?)
                    (λ (x) 5))
     3))
  
  (test/spec-failed
   'with-contract-exp-4
   '((with-contract r
                    #:result (-> number? number?)
                    (λ (x) (zero? x)))
     3)
   "(region r)")
  
  (test/spec-failed
   'with-contract-exp-5
   '((with-contract r
                    #:result (-> number? number?)
                    (λ (x) 5))
     #t)
   "top-level")
  
  (test/spec-passed
   'with-contract-exp-values-1
   '(let-values ([() (with-contract r #:results () (values))])
      1))
  
  (test/spec-passed
   'with-contract-exp-values-1
   '(let-values ([(x y) (with-contract r
                                       #:results (number? string?)
                                       (values 3 "foo"))])
      1))
  
  (test/spec-failed
   'with-contract-exp-values-2
   '(let-values ([(x y) (with-contract r
                                       #:results (number? string?)
                                       (values "bar" "foo"))])
      1)
   "(region r)")
  
  (test/spec-passed
   'with-contract-exp-values-3
   '(let-values ([(x y) (with-contract r
                                       #:results (number? string?)
                                       (define (f) (values 3 "foo"))
                                       (f))])
      1))
  
  (test/spec-passed/result
   'with-contract-#%app
   '(begin
      (eval '(module with-contract-#%app-app racket
               (define-syntax (-app x) #''apped)
               (provide (rename-out (-app #%app)))))
      (eval '(module with-contract-#%app-client racket
               (require 'with-contract-#%app-app)
               (provide with-contract-#%app-h with-contract-#%app-i)
               (with-contract x ([f any/c]) (define (f x) 'f))
               (define (g x) 'g)
               (define with-contract-#%app-h (f 2))
               (define with-contract-#%app-i (g 2))))
      (eval '(require 'with-contract-#%app-client))
      (eval '(list with-contract-#%app-h with-contract-#%app-i)))
   (list 'apped 'apped))
  
  (test/spec-passed/result
   'with-contract-one-contract-app-per-mutation-1
   '(let* ([counter 0]
           [ctc (λ (x) (set! counter (add1 counter)) (number? x))])
      (with-contract foo ([x ctc])
                     (define x 3))
      (+ x 4)
      (+ x 6)
      counter)
   1)
  
  (test/spec-passed/result
   'with-contract-one-contract-app-per-mutation-2
   '(let* ([counter 0]
           [ctc (λ (x) (set! counter (add1 counter)) (number? x))])
      (with-contract foo ([x ctc])
                     (define x 3))
      (+ x 4)
      (+ x 6)
      (set! x 6)
      (+ x 6)
      (+ x 2)
      counter)
   ;; 3 because of a double wrapping that occurs for outside mutations
   ;; (that is, internal gets wrapped, then external gets wrapped again
   ;; to fix blame.  Could be optimized away if we had blame collapsing,
   ;; e.g., (contract ctc (contract ctc x n1 p1) n2 p2) =>
   ;; (contract ctc x n2 p1), so if this breaks after adding such adjust it.
   3)
  
  ;; Check to make sure that any non-delayed set!s within the region don't
  ;; get overridden when accessing the external version.
  (test/spec-passed/result
   'with-contract-on-contract-app-per-mutation-3
   '(let ()
      (with-contract foo ([x number?])
                     (define x 3)
                     (set! x 4))
      x)
   4))
