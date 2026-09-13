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


  (test/spec-passed
   'instanceof/c-first-order-local-method-2
   '(let ()
      (define-local-member-name m)
      (contract (instanceof/c (class/c [m (-> any/c number? number?)]))
                (new (class object% (super-new) (define/public (m x) (add1 x))))
                'pos
                'neg)))

  (test/neg-blame
   'instanceof-first-order-local-method-3
   '(let ()
      (define-local-member-name m)
      (send (contract (instanceof/c (class/c [m (-> any/c number? number?)]))
                      (new (class object% (define/public (m x) x) (super-new)))
                      'pos
                      'neg)
            m "one")))

  (test/pos-blame
   'instanceof-first-order-local-method-4
   '(let ()
      (define-local-member-name m)
      (send (contract (instanceof/c (class/c [m (-> any/c number? number?)]))
                      (new (class object% (define/public (m x) "wrong") (super-new)))
                      'pos
                      'neg)
            m 1)))

  (test/spec-passed
   'instanceof-flat
   '(let ()
      (define i<%> (interface ()))
      (define c% (class* object% (i<%>) (super-new)))
      (contract (instanceof/c (implementation?/c i<%>))
                (new c%)
                'pos 'neg)))

  (test/pos-blame
   'instanceof-flat
   '(let ()
      (define i<%> (interface ()))
      (contract (instanceof/c (implementation?/c i<%>))
                (new object%)
                'pos 'neg)))

  (test/pos-blame
   'instanceof-flat
   '(let ()
      (define i<%> (interface ()))
      (define c% (class* object% () (super-new)))
      (contract (instanceof/c (implementation?/c i<%>))
                (new c%)
                'pos 'neg)))

  (test/pos-blame
   'instanceof-init-field
   '(get-field f
               (contract (instanceof/c (class/c (init-field [f integer?])))
                         (new (class object% (init-field [f "wrong"]) (super-new)))
                         'pos 'neg)))
  )
