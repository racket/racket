#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract)])
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
   "(definition i)")
  
  (test/spec-failed
   'define/contract3
   '(let ()
      (define/contract i (-> integer? integer?) (lambda (x) #t))
      (i 1))
   "(definition i)")
  
  (test/spec-failed
   'define/contract4
   '(let ()
      (define/contract i (-> integer? integer?) (lambda (x) 1))
      (i #f))
   "top-level")
  
  (test/spec-failed
   'define/contract5
   '(let ()
      (define/contract (i x) (-> integer? integer?) 1)
      (i #f))
   "top-level")
  
  (test/spec-passed
   'define/contract6
   '(let ()
      (define/contract (i x) (-> integer? integer?)
        (cond
          [(not (integer? x)) 1]
          [else (i #f)]))
      (i 1)))
  
  (test/spec-passed
   'define/contract7
   '(let ()
      (define/contract (contracted-func label t)
        (string?  string? . -> . string?)
        t)
      (contracted-func
       "I'm a string constant with side effects"
       "ans")))
  
  (test/spec-passed
   'define/contract8
   '(let ()
      (eval '(module contract-test-suite-define1 scheme/base
               (require scheme/contract)
               (define/contract x string? "a")
               (void x)))
      (eval '(require 'contract-test-suite-define1))))
  
  (test/spec-failed
   'define/contract9
   '(let ()
      (define/contract (a n)
        (-> number? number?)
        (define/contract (b m)
          (-> number? number?)
          (+ m 1))
        (b (zero? n)))
      (a 5))
   "(function a)")
  
  (test/spec-failed
   'define/contract10
   '(let ()
      (define/contract (a n)
        (-> number? number?)
        (define/contract (b m)
          (-> number? number?)
          #t)
        (b (add1 n)))
      (a 5))
   "(function b)")
  
  (test/spec-passed
   'define/contract11
   '(let ()
      (define/contract (f n)
        (-> number? number?)
        (+ n 1))
      (define/contract (g b m)
        (-> boolean? number? number?)
        (if b (f m) (f #t)))
      (g #t 3)))
  
  ;; For some of the following tests, it may not be clear
  ;; why the blame is what it is.  The contract(s) entered
  ;; into via with-contract are between the contracting
  ;; region and its context.  If the context allows the
  ;; value to flow into other regions without contracts
  ;; that protect it from misuses in those regions, it's
  ;; the context's fault.
  (test/spec-failed
   'define/contract12
   '(let ()
      (define/contract (f n)
        (-> number? number?)
        (+ n 1))
      (define/contract (g b m)
        (-> boolean? number? number?)
        (if b (f m) (f #t)))
      (g #f 3))
   "top-level")
  
  (test/spec-failed
   'define/contract13
   '(begin
      (eval '(module foo-dc13 scheme/base
               (require scheme/contract)
               (define/contract (foo-dc13 n)
                 (-> number? number?)
                 (+ n 1))
               (foo-dc13 #t)))
      (eval '(require 'foo-dc13)))
   "foo-dc13")
  
  (test/spec-failed
   'define/contract14
   '(begin
      (eval '(module foo-dc14 scheme/base
               (require scheme/contract)
               (provide foo-dc14)
               (define/contract (foo-dc14 n)
                 (-> number? number?)
                 (+ n 1))))
      (eval '(module bar-dc14 scheme/base
               (require 'foo-dc14)
               (foo-dc14 #t)))
      (eval '(require 'bar-dc14)))
   "foo-dc14")
  
  (test/spec-failed
   'define/contract15
   '(begin
      (eval '(module foo-dc15 scheme/base
               (require scheme/contract)
               (provide foo-dc15)
               (define/contract (foo-dc15 n)
                 (-> number? number?)
                 (+ n 1))))
      (eval '(require 'foo-dc15))
      (eval '(foo-dc15 #t)))
   "foo-dc15")
  
  ;; Let's see how units + define/contract interact
  
  (test/spec-failed
   'define/contract16
   '(begin
      (eval '(module foo-dc16 scheme/base
               (require scheme/contract)
               (require scheme/unit)
               (let ()
                 (define/contract (foo n)
                   (-> number? number?)
                   (define-signature U^
                     ((contracted [x (-> number? number?)])))
                   (define-unit U@
                     (import)
                     (export U^)
                     (define (x n) #t))
                   (define-values/invoke-unit U@
                     (import)
                     (export U^))
                   (x n))
                 (foo 3))))
      (eval '(require 'foo-dc16)))
   "(unit U@)")
  
  (test/spec-failed
   'define/contract16a
   '(begin
      (eval '(module foo-dc16a scheme/base
               (require scheme/contract)
               (require scheme/unit)
               (let ()
                 (define/contract (foo n)
                   (-> number? number?)
                   (define-signature U^
                     (x))
                   (define-unit/contract U@
                     (import)
                     (export (U^ [x (-> number? number?)]))
                     (define (x n) #t))
                   (define-values/invoke-unit U@
                     (import)
                     (export U^))
                   (x n))
                 (foo 3))))
      (eval '(require 'foo-dc16a)))
   "(unit U@)")
  
  (test/spec-failed
   'define/contract17
   '(begin
      (eval '(module foo-dc17 scheme/base
               (require scheme/contract)
               (require scheme/unit)
               (let ()
                 (define/contract (foo n)
                   (-> number? number?)
                   (define-signature U^
                     ((contracted [x (-> number? number?)])))
                   (define-unit U@
                     (import)
                     (export U^)
                     (define (x n) 3))
                   (define-values/invoke-unit U@
                     (import)
                     (export U^))
                   (x (zero? n)))
                 (foo 3))))
      (eval '(require 'foo-dc17)))
   "(function foo)")
  
  (test/spec-failed
   'define/contract18
   '(begin
      (eval '(module foo-dc18 scheme/base
               (require scheme/contract)
               (require scheme/unit)
               (let ()
                 (define-signature U^
                   ((contracted [x (-> number? number?)])))
                 (define-unit U@
                   (import)
                   (export U^)
                   ;; Can't define/contract x directly because
                   ;; x ends up bound to a transformer and thus
                   ;; is syntax.
                   (define/contract (y n)
                     (-> number? boolean?) #t)
                   (define x y))
                 (define-values/invoke-unit U@
                   (import)
                   (export U^))
                 (x 3))))
      (eval '(require 'foo-dc18)))
   "(unit U@)")
  
  (test/spec-failed
   'define/contract19
   '(let ()
      (define y 3)
      (define/contract (f n)
        (-> number? number?)
        #:freevar y (-> number? boolean?)
        3)
      1)
   "top-level")
  
  (test/spec-passed
   'define/contract20
   '(let ()
      (define y (lambda (n) 4))
      (define/contract (f n)
        (-> number? number?)
        #:freevar y (-> number? boolean?)
        3)
      1))
  
  (test/spec-passed
   'define/contract21
   '(let ()
      (define y (lambda (n) 4))
      (define/contract (f n)
        (-> number? number?)
        #:freevar y (-> number? boolean?)
        (if (y n) 3 1))
      1))
  
  (test/spec-failed
   'define/contract22
   '(let ()
      (define y 4)
      (define/contract (f n)
        (-> number? number?)
        #:freevar y (-> number? boolean?)
        3)
      1)
   "top-level")
  
  (test/spec-failed
   'define/contract23
   '(let ()
      (define y (lambda (n) #t))
      (define/contract (f n)
        (-> number? number?)
        #:freevar y (-> number? number?)
        (y n))
      (f 5))
   "top-level")
  
  (test/spec-failed
   'define/contract24
   '(let ()
      (define y (lambda (n) 4))
      (define/contract (f n)
        (-> number? number?)
        #:freevar y (-> number? boolean?)
        (if (y #t) 3 1))
      (f 5))
   "(function f)")
  
  (test/spec-failed
   'define/contract25
   '(let ()
      (define y #t)
      (define z 3)
      (define/contract f
        number?
        #:freevars ([y number?] [z number?])
        (+ y z))
      1)
   "top-level")
  
  (test/spec-passed
   'define/contract
   '(let ()
      (define/contract (f x #:y [y 3])
        (->* (integer?) (#:y integer?) integer?)
        x)
      (f 3 #:y 7)))
  
  (test/spec-failed
   'define/contract
   '(let ()
      (define/contract (f x #:y [y 3])
        (->* (integer?) (#:y integer?) integer?)
        x)
      (f 3 #:y #f))
   "top-level"))