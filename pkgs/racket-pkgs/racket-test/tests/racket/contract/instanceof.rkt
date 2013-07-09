#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace
                 'racket/class)])
  
  (test/spec-passed
   'instanceof/c-first-order-1
   '(let* ([c% object%]
           [c%/c (class/c)])
      (contract (instanceof/c c%/c) (new c%) 'pos 'neg)))
  
  (test/pos-blame
   'instanceof/c-first-order-2
   '(let* ([c% object%]
           [c%/c (class/c (field [f number?]))])
      (contract (instanceof/c c%/c) (new c%) 'pos 'neg)))
  
  (test/pos-blame
   'instanceof/c-first-order-3
   '(let* ([c% object%]
           [c%/c (class/c [m (->m number? number?)])])
      (contract (instanceof/c c%/c) (new c%) 'pos 'neg)))
  
  (test/spec-passed
   'instanceof/c-first-order-4
   '(let* ([c% (class object% (super-new) (field [f 3]))]
           [c%/c (class/c (field [f number?]))])
      (contract (instanceof/c c%/c) (new c%) 'pos 'neg)))
  
  (test/spec-passed
   'instanceof/c-first-order-5
   '(let* ([c% (class object% (super-new) (define/public (m x) x))]
           [c%/c (class/c [m (->m number? number?)])])
      (contract (instanceof/c c%/c) (new c%) 'pos 'neg)))
  
  (test/spec-passed
   'instanceof/c-first-order-6
   '(let* ([c% (class object% (super-new) (define/public (m x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])])
      (contract (instanceof/c (or/c c%/c d%/c)) (new c%) 'pos 'neg)))
  
  (test/spec-passed
   'instanceof/c-first-order-7
   '(let* ([d% (class object% (super-new) (define/public (n x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])])
      (contract (instanceof/c (or/c c%/c d%/c)) (new d%) 'pos 'neg)))
  
  (test/pos-blame
   'instanceof/c-first-order-8
   '(let* ([e% (class object% (super-new) (define/public (p x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])])
      (contract (instanceof/c (or/c c%/c d%/c)) (new e%) 'pos 'neg)))
  
  (test/spec-passed/result
   'instanceof/c-higher-order-1
   '(let* ([c% (class object% (super-new) (field [f 3]))]
           [c%/c (class/c (field [f number?]))]
           [o (contract (instanceof/c c%/c) (new c%) 'pos 'neg)])
      (get-field f o))
   3)
  
  (test/neg-blame
   'instanceof/c-higher-order-2
   '(let* ([c% (class object% (super-new) (field [f 3]))]
           [c%/c (class/c (field [f number?]))]
           [o (contract (instanceof/c c%/c) (new c%) 'pos 'neg)])
      (set-field! f o #t)))
  
  (test/pos-blame
   'instanceof/c-higher-order-3
   '(let* ([c% (class object% (super-new) (define/public (m x) (zero? x)))]
           [c%/c (class/c [m (->m number? number?)])]
           [o (contract (instanceof/c c%/c) (new c%) 'pos 'neg)])
      (send o m 3)))
  
  (test/spec-passed
   'instanceof/c-higher-order-4
   '(let* ([c% (class object% (super-new) (define/public (m x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])]
           [o (contract (instanceof/c (or/c c%/c d%/c)) (new c%) 'pos 'neg)])
      (send o m 3)))
  
  (test/pos-blame
   'instanceof/c-higher-order-4
   '(let* ([c% (class object% (super-new) (define/public (m x) #t))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])]
           [o (contract (instanceof/c (or/c c%/c d%/c)) (new c%) 'pos 'neg)])
      (send o m 3)))
  
  (test/neg-blame
   'instanceof/c-higher-order-4
   '(let* ([c% (class object% (super-new) (define/public (m x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])]
           [o (contract (instanceof/c (or/c c%/c d%/c)) (new c%) 'pos 'neg)])
      (send o m #t))))