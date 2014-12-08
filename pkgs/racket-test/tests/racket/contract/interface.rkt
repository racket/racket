#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 
                 'racket/class
                 'racket/contract)])
  
  (define exn:fail:contract:blame? (contract-eval 'exn:fail:contract:blame?))
  (test/spec-passed
   'interface-1
   '(interface () [x number?]))
  
  (test/spec-passed
   'interface-2
   '(interface () [x number?] [y number?]))
  
  (test/spec-passed
   'interface-3
   '(interface () [get-x (-> integer?)]))
  
  (contract-syntax-error-test
   'interface-4
   '(interface () [x number?] [x symbol?]))
  
  (contract-error-test
   'interface-5
   '(interface () [x (λ (x y) x)])
   exn:fail?)
  
  (contract-error-test
   'interface-6
   '(interface ((interface () x)) x)
   exn:fail?)
  
  (test/spec-passed
   'interface-7
   '(interface ((interface () x)) [x integer?]))
  
  (test/spec-passed
   'interface-8
   '(interface ((interface () [x number?])) [x integer?]))
  
  (contract-error-test
   'interface-9
   '(interface ((interface () [x number?])) x)
   exn:fail?)
  
  (test/spec-passed
   'interface-first-order-1
   '(let* ([i<%> (interface () [m (->m number? number?)])]
           [c% (class* object% (i<%>) (super-new) (define/public (m x) x))])
      (new c%)))
  
  (test/spec-failed
   'interface-first-order-2
   '(let* ([i<%> (interface () [m (->m number? number?)])]
           [c% (class* object% (i<%>) (super-new) (define/public (m) x))])
      (new c%))
   "(class c%)")
  
  (test/spec-passed
   'interface-higher-order-1
   '(let* ([i<%> (interface () [m (->m number? number?)])]
           [c% (class* object% (i<%>) (super-new) (define/public (m x) x))])
      (send (new c%) m 3)))
  
  (test/spec-failed
   'interface-higher-order-2
   '(let* ([i<%> (interface () [m (->m number? number?)])]
           [c% (class* object% (i<%>) (super-new) (define/public (m x) x))])
      (send (new c%) m "wrong"))
   "top-level")
  
  (test/spec-failed
   'interface-higher-order-3
   '(let* ([i<%> (interface () [m (->m number? number?)])]
           [c% (class* object% (i<%>) (super-new) (define/public (m x) "bad"))])
      (send (new c%) m 3))
   "(class c%)")
  
  (test/spec-failed
   'interface-higher-order-4
   '(let* ([i1<%> (interface () [m (->m number? number?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) x))])
      (send (new c%) m 3.14))
   "(interface i2<%>)")
  
  (test/spec-failed
   'interface-higher-order-5
   '(let* ([i1<%> (interface () [m (->m number? number?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) 3.14))])
      (send (new c%) m 3))
   "(class c%)")
  
  (test/spec-failed
   'interface-higher-order-6
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m number? number?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) 3.14))])
      (send (new c%) m 3))
   "(interface i2<%>)")
  
  (test/spec-passed
   'interface-higher-order-7
   '(let* ([i1<%> (interface () [m (->m integer? number?)])]
           [i2<%> (interface (i1<%>) [m (->m number? integer?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) x))])
      (send (new c%) m 3)))
  
  (test/spec-failed
   'interface-higher-order-8
   '(let* ([i1<%> (interface () [m (->m integer? number?)])]
           [i2<%> (interface (i1<%>) [m (->m number? integer?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) x))])
      (send (new c%) m 3.14))
   "top-level")
  
  (test/spec-failed
   'interface-higher-order-9
   '(let* ([i1<%> (interface () [m (->m integer? number?)])]
           [i2<%> (interface (i1<%>) [m (->m number? integer?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) 3.14))])
      (send (new c%) m 3))
   "(class c%)")
  
  (test/spec-passed
   'interface-higher-order-10
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) x))]
           [c2% (class c% (super-new))])
      (send (new c2%) m 3)))
  
  (test/spec-passed
   'interface-higher-order-11
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (contract (class/c)
                         (class* object% (i2<%>) (super-new) (define/public (m x) x))
                         'pos
                         'neg)])
      (send (new c%) m 3)))
  
  (test/neg-blame
   'interface-higher-order-12
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (contract (class/c [m (->m integer? integer?)])
                         (class* object% (i2<%>) (super-new) (define/public (m x) x))
                         'pos
                         'neg)])
      (send (new c%) m 5.14)))
  
  (test/spec-failed
   'interface-higher-order-13
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (contract (class/c m)
                         (class* object% (i2<%>) (super-new) (define/public (m x) x))
                         'pos
                         'neg)])
      (send (new c%) m 5.14))
   "pos")
  
  (test/spec-failed
   'interface-higher-order-14
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (contract (class/c)
                         (class* object% (i2<%>) (super-new) (define/public (m x) x))
                         'pos
                         'neg)])
      (send (new c%) m 5.14))
   "top-level")
  
  (test/spec-passed
   'interface-internal-name-1
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (class* object% (i2<%>)
                 (super-new)
                 (public [n m])
                 (define n (λ (x) x)))])
      (send (new c%) m 3)))
  
  (test/spec-passed
   'interface-internal-name-2
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (contract
                (class/c [m (->m integer? integer?)])
                (class* object% (i2<%>)
                  (super-new)
                  (public [n m])
                  (define n (λ (x) x)))
                'pos
                'neg)])
      (send (new c%) m 3)))
  
  (test/spec-passed
   'interface-mixin-1
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [mixin (λ (cls)
                    (class* cls (i2<%>)
                      (super-new)
                      (define/public (m x) x)))])
      (send (new (mixin object%)) m 3)))
  
  (test/spec-passed
   'interface-bad-concretization-1
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) x))]
           [c2% (class c% (super-new))])
      (send (new c2%) m 3)
      (with-contract region
                     #:result integer?
                     (send (new c2%) m 3))))
  
  (contract-error-test
   'interface-method-name-1
   #'(begin
       (eval '(module imn-bug scheme/base
                (require scheme/class)
                (define i<%> (interface () [m (->m integer? integer?)]))
                (define c% (class* object% (i<%>) (super-new) (define/public (m x) x)))
                (send (new c%) m "foo")))
       (eval '(require 'imn-bug)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"m: contract violation" (exn-message x))))))