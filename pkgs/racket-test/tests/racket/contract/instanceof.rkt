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

  (test/spec-passed
   'instanceof/c-first-order-9
   '(let* ([c% (class object% (super-new) (define/public (m x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])])
      (contract (instanceof/c (first-or/c c%/c d%/c)) (new c%) 'pos 'neg)))
  
  (test/spec-passed
   'instanceof/c-first-order-10
   '(let* ([d% (class object% (super-new) (define/public (n x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])])
      (contract (instanceof/c (first-or/c c%/c d%/c)) (new d%) 'pos 'neg)))
  
  (test/pos-blame
   'instanceof/c-first-order-11
   '(let* ([e% (class object% (super-new) (define/public (p x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])])
      (contract (instanceof/c (first-or/c c%/c d%/c)) (new e%) 'pos 'neg)))
  
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
   'instanceof/c-higher-order-5
   '(let* ([c% (class object% (super-new) (define/public (m x) #t))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])]
           [o (contract (instanceof/c (or/c c%/c d%/c)) (new c%) 'pos 'neg)])
      (send o m 3)))
  
  (test/neg-blame
   'instanceof/c-higher-order-6
   '(let* ([c% (class object% (super-new) (define/public (m x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])]
           [o (contract (instanceof/c (or/c c%/c d%/c)) (new c%) 'pos 'neg)])
      (send o m #t)))

  (test/spec-passed
   'instanceof/c-higher-order-7
   '(let* ([c% (class object% (super-new) (define/public (m x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])]
           [o (contract (instanceof/c (first-or/c c%/c d%/c)) (new c%) 'pos 'neg)])
      (send o m 3)))
  
  (test/pos-blame
   'instanceof/c-higher-order-8
   '(let* ([c% (class object% (super-new) (define/public (m x) #t))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])]
           [o (contract (instanceof/c (first-or/c c%/c d%/c)) (new c%) 'pos 'neg)])
      (send o m 3)))
  
  (test/neg-blame
   'instanceof/c-higher-order-9
   '(let* ([c% (class object% (super-new) (define/public (m x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])]
           [o (contract (instanceof/c (first-or/c c%/c d%/c)) (new c%) 'pos 'neg)])
      (send o m #t)))

  ;; todo:
  ;; - add two contracts that are different; ensure both are checked
  ;; - add three contracts that are different; ensure all three are checked
  ;; - add two contracts that are the same but different blame and do some eq? testing
  ;; - add two contracts that are the same but different blame and make sure the right one is checked

  ;; test that wrapping the same unwrapped object twice gets us eq? objects
  (test/spec-passed/result
   'instanceof/c-0to1-wrap.1
   '(let ()
      (eval
       '(module instanceof/c-0to1-wrap.1-server racket/base
          (require racket/class racket/contract)
          (define c% (class object% (super-new) (define/public (m x) x)))
          (define c%/c (class/c [m (->m number? number?)]))
          (define i/c (instanceof/c c%/c))
          (define o1 (new c%))
          (define (get1) o1)
          (provide
           (contract-out
            [get1 (-> i/c)]))))
      (eval
       '(module instanceof/c-0to1-wrap.1-client racket/base
          (require 'instanceof/c-0to1-wrap.1-server)
          (provide same?)
          (define o1 (get1))
          (define o2 (get1))
          (define same? (eq? o1 o2))))
      (dynamic-require ''instanceof/c-0to1-wrap.1-client 'same?))
   #t)

  (test/spec-failed
   ''instanceof/c-0to1-wrap.2
   '(let ()
      (eval
       '(module instanceof/c-0to1-wrap.2-server racket/base
          (require racket/class racket/contract)
          (define c% (class object% (super-new) (define/public (m x) x)))
          (define c%/c (class/c [m (->m number? number?)]))
          (define i/c (instanceof/c c%/c))
          (define o1 (new c%))
          (define (get1) o1)
          (provide
           (contract-out
            [get1 (-> i/c)]))))
      (eval
       '(module instanceof/c-0to1-wrap.2-client racket/base
          (require 'instanceof/c-0to1-wrap.2-server racket/class)
          (define o1 (get1))
          (define o2 (get1))
          (void (send o1 m "non number"))))
      (dynamic-require ''instanceof/c-0to1-wrap.2-client #f))
   "instanceof/c-0to1-wrap.2-client")

  (test/spec-failed
   ''instanceof/c-0to1-wrap.3
   '(let ()
      (eval
       '(module instanceof/c-0to1-wrap.3-server racket/base
          (require racket/class racket/contract)
          (define c% (class object% (super-new) (define/public (m x) x)))
          (define c%/c (class/c [m (->m number? number?)]))
          (define i/c (instanceof/c c%/c))
          (define o1 (new c%))
          (define (get1) o1)
          (provide
           (contract-out
            [get1 (-> i/c)]))))
      (eval
       '(module instanceof/c-0to1-wrap.3-client racket/base
          (require 'instanceof/c-0to1-wrap.3-server racket/class)
          (define o1 (get1))
          (define o2 (get1))
          (void (send o2 m "non number"))))
      (dynamic-require ''instanceof/c-0to1-wrap.3-client #f))
   "instanceof/c-0to1-wrap.3-client")

  (test/spec-failed
   ''instanceof/c-0to1-wrap.4
   '(let ()
      (eval
       '(module instanceof/c-0to1-wrap.4-server racket/base
          (require racket/class racket/contract)
          (define c% (class object% (super-new) (define/public (m x) "wrong")))
          (define c%/c (class/c [m (->m number? number?)]))
          (define i/c (instanceof/c c%/c))
          (define o1 (new c%))
          (define (get1) o1)
          (provide
           (contract-out
            [get1 (-> i/c)]))))
      (eval
       '(module instanceof/c-0to1-wrap.4-client racket/base
          (require 'instanceof/c-0to1-wrap.4-server racket/class)
          (define o1 (get1))
          (define o2 (get1))
          (void (send o1 m 11))))
      (dynamic-require ''instanceof/c-0to1-wrap.4-client #f))
   "instanceof/c-0to1-wrap.4-server")

  (test/spec-failed
   ''instanceof/c-0to1-wrap.5
   '(let ()
      (eval
       '(module instanceof/c-0to1-wrap.5-server racket/base
          (require racket/class racket/contract)
          (define c% (class object% (super-new) (define/public (m x) "wrong")))
          (define c%/c (class/c [m (->m number? number?)]))
          (define i/c (instanceof/c c%/c))
          (define o1 (new c%))
          (define (get1) o1)
          (provide
           (contract-out
            [get1 (-> i/c)]))))
      (eval
       '(module instanceof/c-0to1-wrap.5-client racket/base
          (require 'instanceof/c-0to1-wrap.5-server racket/class)
          (define o1 (get1))
          (define o2 (get1))
          (void (send o2 m 11))))
      (dynamic-require ''instanceof/c-0to1-wrap.5-client #f))
   "instanceof/c-0to1-wrap.5-server")


  ;; test that wrapping the same wrapped object twice gets us eq? objects
  (test/spec-passed/result
   'instanceof/c-1to1-wrap.1
   '(let ()
      (eval
       '(module instanceof/c-1to1-wrap.1-server racket/base
          (require racket/class racket/contract)
          (define c% (class object% (super-new) (define/public (m x) x)))
          (define c%/c (class/c [m (->m number? number?)]))
          (define i/c (instanceof/c c%/c))
          (define o1 (new c%))
          (define (get1) o1)
          (provide
           i/c
           (contract-out
            [get1 (-> i/c)]))))
      (eval
       '(module instanceof/c-1to1-wrap.1-between racket/base
          (require 'instanceof/c-1to1-wrap.1-server racket/contract)
          (define o2 (get1))
          (define (get2) o2)
          (provide
           (contract-out
            [get2 (-> i/c)]))))
      (eval
       '(module instanceof/c-1to1-wrap.1-client racket/base
          (require 'instanceof/c-1to1-wrap.1-between)
          (provide same?)
          (define o3 (get2))
          (define o4 (get2))
          (define same? (eq? o3 o4))))
      (dynamic-require ''instanceof/c-1to1-wrap.1-client 'same?))
   #t)

  (test/spec-failed
   'instanceof/c-1to1-wrap.2
   '(let ()
      (eval
       '(module instanceof/c-1to1-wrap.2-server racket/base
          (require racket/class racket/contract)
          (define c% (class object% (super-new) (define/public (m x) x)))
          (define c%/c (class/c [m (->m number? number?)]))
          (define i/c (instanceof/c c%/c))
          (define o1 (new c%))
          (define (get1) o1)
          (provide
           i/c
           (contract-out
            [get1 (-> i/c)]))))
      (eval
       '(module instanceof/c-1to1-wrap.2-between racket/base
          (require 'instanceof/c-1to1-wrap.2-server racket/contract)
          (define o2 (get1))
          (define (get2) o2)
          (provide
           (contract-out
            [get2 (-> i/c)]))))
      (eval
       '(module instanceof/c-1to1-wrap.2-client racket/base
          (require 'instanceof/c-1to1-wrap.2-between racket/class)
          (define o3 (get2))
          (define o4 (get2))
          (send o3 m "wrong")))
      (dynamic-require ''instanceof/c-1to1-wrap.2-client #f))
   "instanceof/c-1to1-wrap.2-client")

  (test/spec-failed
   'instanceof/c-1to1-wrap.3
   '(let ()
      (eval
       '(module instanceof/c-1to1-wrap.3-server racket/base
          (require racket/class racket/contract)
          (define c% (class object% (super-new) (define/public (m x) x)))
          (define c%/c (class/c [m (->m number? number?)]))
          (define i/c (instanceof/c c%/c))
          (define o1 (new c%))
          (define (get1) o1)
          (provide
           i/c
           (contract-out
            [get1 (-> i/c)]))))
      (eval
       '(module instanceof/c-1to1-wrap.3-between racket/base
          (require 'instanceof/c-1to1-wrap.3-server racket/contract)
          (define o2 (get1))
          (define (get2) o2)
          (provide
           (contract-out
            [get2 (-> i/c)]))))
      (eval
       '(module instanceof/c-1to1-wrap.3-client racket/base
          (require 'instanceof/c-1to1-wrap.3-between racket/class)
          (define o3 (get2))
          (define o4 (get2))
          (send o4 m "wrong")))
      (dynamic-require ''instanceof/c-1to1-wrap.3-client #f))
   "instanceof/c-1to1-wrap.3-client")

  (test/spec-failed
   'instanceof/c-1to1-wrap.4
   '(let ()
      (eval
       '(module instanceof/c-1to1-wrap.4-server racket/base
          (require racket/class racket/contract)
          (define c% (class object% (super-new) (define/public (m x) "wrong")))
          (define c%/c (class/c [m (->m number? number?)]))
          (define i/c (instanceof/c c%/c))
          (define o1 (new c%))
          (define (get1) o1)
          (provide
           i/c
           (contract-out
            [get1 (-> i/c)]))))
      (eval
       '(module instanceof/c-1to1-wrap.4-between racket/base
          (require 'instanceof/c-1to1-wrap.4-server racket/contract)
          (define o2 (get1))
          (define (get2) o2)
          (provide
           (contract-out
            [get2 (-> i/c)]))))
      (eval
       '(module instanceof/c-1to1-wrap.4-client racket/base
          (require 'instanceof/c-1to1-wrap.4-between racket/class)
          (define o3 (get2))
          (define o4 (get2))
          (send o4 m 11)))
      (dynamic-require ''instanceof/c-1to1-wrap.4-client #f))
   "instanceof/c-1to1-wrap.4-server")
  )
