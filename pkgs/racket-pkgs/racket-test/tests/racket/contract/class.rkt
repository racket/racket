#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 
                 'racket/class
                 'racket/contract)])
  (test/pos-blame
   'class/c-first-order-class-1
   '(contract (class/c)
              3
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-class-2
   '(contract (class/c)
              object%
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-opaque-class-1
   '(contract (class/c #:opaque)
              object%
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-opaque-class-2
   '(contract (class/c #:opaque)
              (class object% (super-new) (define/public (m x) (add1 x)))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-opaque-class-3
   '(contract (class/c #:opaque m)
              (class object% (super-new) (define/public (m) 3))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-method-1
   '(contract (class/c [m (-> any/c number? number?)])
              object%
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-method-2
   '(contract (class/c [m (-> any/c number? number?)])
              (class object% (super-new) (define/public (m x) (add1 x)))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-method-3
   '(contract (class/c [m (-> any/c number? number?)])
              (class object% (super-new) (define/public (m) 3))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-method-4
   '(contract (class/c m)
              object%
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-method-5
   '(contract (class/c m)
              (class object% (super-new) (define/public (m) 3))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-method-6
   '(contract (class/c [m (-> any/c number? number?)])
              (class object% (super-new) (define/public (m) 3))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-local-method-1
   '(let ()
      (define-local-member-name m)
      (contract (class/c [m (->m number? number?)])
                (class object% (super-new) (define/public (m x) 3))
                'pos
                'neg)))
  
  (test/pos-blame
   'class/c-first-order-local-method-2
   '(let ()
      (define-local-member-name m)
      (contract (class/c [m (->m number? number? number?)])
                (class object% (super-new) (define/public (m x) 3))
                'pos
                'neg)))
  
  (test/pos-blame
   'class/c-first-order-local-method-3
   '(let ()
      (define-local-member-name m)
      (contract (class/c [m (->m number? number? number?)])
                (class object% (super-new))
                'pos
                'neg)))
  
  (test/spec-passed
   'class/c-first-order-opaque-method-1
   '(contract (class/c #:opaque [m (-> any/c number? number?)])
              (class object% (super-new) (define/public (m x) 3))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-opaque-method-2
   '(contract (class/c #:opaque [m (-> any/c number? number?)])
              (class object% (super-new) (define/public (m x) 3) (define/public (n) 4))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-opaque-method-3
   '(let ()
      (define-local-member-name n)
      (contract (class/c #:opaque [m (-> any/c number? number?)])
                (class object% (super-new) (define/public (m x) 3) (define/public (n) 4))
                'pos
                'neg)))
  
  (test/pos-blame
   'class/c-first-order-opaque-method-4
   '(contract
     (class/c #:opaque [m (-> any/c number? number?)])
     (let ()
       (define-local-member-name n)
       (class object% (super-new) (define/public (m x) 3) (define/public (n) 4)))
     'pos
     'neg))
  
  (test/pos-blame
   'class/c-first-order-opaque-method-5
   '(contract
     (class/c #:opaque [m (-> any/c number? number?)] [n (-> any/c number?)])
     (let ()
       (define-local-member-name n)
       (class object% (super-new) (define/public (m x) 3) (define/public (n) 4)))
     'pos
     'neg))
  
  (test/spec-passed
   'class/c-first-order-opaque-method-6
   '(let ()
      (define-local-member-name n)
      (contract (class/c #:opaque [m (-> any/c number? number?)] [n (-> any/c number?)])
                (class object% (super-new) (define/public (m x) 3) (define/public (n) 4))
                'pos
                'neg)))
  
  (test/pos-blame
   'class/c-first-order-field-1
   '(contract (class/c (field [n number?]))
              object%
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-field-2
   '(contract (class/c (field [n number?]))
              (class object% (super-new) (field [n 3]))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-field-3
   '(contract (class/c (field n))
              object%
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-field-4
   '(contract (class/c (field n))
              (class object% (super-new) (field [n 3]))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-local-field-1
   '(let ()
      (define-local-member-name n)
      (contract (class/c (field n))
                (class object% (super-new) (field [n 3]))
                'pos
                'neg)))
  
  (test/spec-passed
   'class/c-first-order-local-field-2
   '(let ()
      (define-local-member-name n)
      (contract (class/c (field [n integer?]))
                (class object% (super-new) (field [n 3]))
                'pos
                'neg)))
  
  (test/pos-blame
   'class/c-first-order-local-field-3
   '(let ()
      (define-local-member-name n)
      (contract (class/c (field [n integer?]))
                (class object% (super-new))
                'pos
                'neg)))
  
  (test/spec-passed
   'class/c-first-order-opaque-field-1
   '(contract (class/c #:opaque (field n))
              (class object% (super-new) (field [n 3]))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-opaque-field-2
   '(contract (class/c #:opaque (field n))
              (class object% (super-new) (field [m 5] [n 3]))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-opaque-field-2
   '(contract (class/c #:opaque (field [m number?]))
              (let ()
                (define-local-member-name n)
                (class object% (super-new) (field [m 5] [n 3])))
              'pos
              'neg))
  
  ;; No true first-order tests here, other than just to make
  ;; sure they're accepted.  For init-field, we can at least
  ;; make sure the given field is public (which happens
  ;; naturally by splitting an init-field into init and field).
  (test/spec-passed
   'class/c-first-order-init-1
   '(contract (class/c (init [a number?]))
              (class object% (super-new) (init a))
              'pos
              'neg))
  
  (test/spec-passed/result
   'class/c-first-order-init-2
   '(send (new (contract (class/c (init [a number?]))
                         (class object% 
                           (super-new) 
                           (init [a 11]) 
                           (define the-a a) 
                           (define/public (m) the-a))
                         'pos
                         'neg))
          m)
   11)
  
  (test/spec-passed/result
   'class/c-first-order-init-3
   '(send (new (contract (class/c (init [a number?] [b number?] [c number?]))
                         (class object% 
                           (super-new) 
                           (init [a 11] [b 12] [c 13])
                           (define args (list a b c))
                           (define/public (m) args))
                         'pos
                         'neg))
          m)
   '(11 12 13))
  
  (test/spec-passed/result
   'class/c-first-order-init-4
   '(send (new (contract (class/c (init [a number?] [b number?] [c number?]))
                         (class object% 
                           (super-new) 
                           (init [a 11] [b 12] [c 13])
                           (define args (list a b c))
                           (define/public (m) args))
                         'pos
                         'neg) [a 22])
          m)
   '(22 12 13))
  
  (test/spec-passed/result
   'class/c-first-order-init-5
   '(send (new (contract (class/c (init [a number?] [b number?] [c number?]))
                         (class object% 
                           (super-new) 
                           (init [a 11] [b 12] [c 13])
                           (define args (list a b c))
                           (define/public (m) args))
                         'pos
                         'neg) [b 22])
          m)
   '(11 22 13))
  
  (test/spec-passed/result
   'class/c-first-order-init-6
   '(send (new (contract (class/c (init [a number?] [b number?] [c number?]))
                         (class object% 
                           (super-new) 
                           (init [a 11] [b 12] [c 13])
                           (define args (list a b c))
                           (define/public (m) args))
                         'pos
                         'neg) [c 22])
          m)
   '(11 12 22))
  
  (test/spec-passed/result
   'class/c-first-order-init-7
   '(send (new (contract (class/c (init [a number?] [b number?] [c number?]))
                         (class object% 
                           (super-new) 
                           (init [a 11] [b 12] [c 13])
                           (define args (list a b c))
                           (define/public (m) args))
                         'pos
                         'neg) [c 22] [b 33])
          m)
   '(11 33 22))
  
  (test/spec-passed/result
   'class/c-first-order-init-8
   '(send (new (contract (class/c (init [a number?] [b number?] [c number?]))
                         (class object% 
                           (super-new) 
                           (init [a 11] [b 12] [c 13])
                           (define args (list a b c))
                           (define/public (m) args))
                         'pos
                         'neg) [c 22] [b 33] [a 44])
          m)
   '(44 33 22))
  
  (test/spec-passed/result
   'class/c-first-order-init-9
   '(send (new (contract (class/c (init [a number?] [b number?] [c number?]))
                         (class object% 
                           (super-new) 
                           (init [a 11] [b 12] [c 13])
                           (define args (list a b c))
                           (define/public (m) args))
                         'pos
                         'neg) [c 22] [a 44])
          m)
   '(44 12 22))
  
  (test/spec-passed/result
   'class/c-first-order-init-10
   '(send (new (contract (class/c (init [a number?] [b number?] [c number?]))
                         (class object% 
                           (super-new) 
                           (init [a 11] [b 12] [c 13])
                           (define args (list a b c))
                           (define/public (m) args))
                         'pos
                         'neg) [b 33] [a 44])
          m)
   '(44 33 13))
  
  
  (test/spec-passed
   'class/c-first-order-init-field-1
   '(contract (class/c (init-field [a number?]))
              (class object% (super-new) (init-field a))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-init-field-2
   '(contract (class/c (init-field [a number?]))
              object%
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-inherit-field-1
   '(contract (class/c (inherit-field [n number?]))
              object%
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-inherit-field-2
   '(contract (class/c (inherit-field [n number?]))
              (class object% (super-new) (field [n 3]))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-inherit-field-3
   '(contract (class/c (inherit-field f))
              object%
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-inherit-field-4
   '(contract (class/c (inherit-field f))
              (class object% (super-new) (field [f 10]))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-super-1
   '(contract (class/c (super [m (-> any/c number? number?)]))
              object%
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-super-2
   '(contract (class/c (super [m (-> any/c number? number?)]))
              (class object% (super-new) (define/pubment (m x) (add1 x)))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-super-3
   '(contract (class/c (super [m (-> any/c number? number?)]))
              (class object% (super-new) (define/public-final (m x) (add1 x)))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-super-4
   '(contract (class/c (super [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/overment (m x) (add1 x))))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-super-5
   '(contract (class/c (super [m (-> any/c number? number?)]))
              (class object% (super-new) (define/public (m x) (add1 x)))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-super-6
   '(contract (class/c (super [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-super-7
   '(contract (class/c (super m))
              object%
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-super-8
   '(contract (class/c (super m))
              (class object% (super-new) (define/public (m) 3))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-super-9
   '(contract (class/c (super [m (-> any/c number? number?)]))
              (class object% (super-new) (define/public (m) 3))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-opaque-super-1
   '(contract (class/c #:opaque (super m))
              (class (class object% (super-new) (define/public (m) 3)) (super-new))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-opaque-super-2
   '(contract (class/c #:opaque (super m) m)
              (class (class object% (super-new) (define/public (m) 3)) (super-new))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-opaque-super-3
   '(contract (class/c #:opaque)
              (class (let ()
                       (define-local-member-name m)
                       (class object% (super-new) (define/public (m) 3)))
                (super-new))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-opaque-super-4
   '(contract (class/c #:opaque (super m) m)
              (class (class object% (super-new) (define/public (m) 3)) (super-new))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-inner-1
   '(contract (class/c (inner [m (-> any/c number? number?)]))
              object%
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-inner-2
   '(contract (class/c (inner [m (-> any/c number? number?)]))
              (class object% (super-new) (define/pubment (m x) (inner x m x)))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-inner-3
   '(contract (class/c (inner [m (-> any/c number? number?)]))
              (class object% (super-new) (define/public (m x) (add1 x)))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-inner-4
   '(contract (class/c (inner [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-inner-5
   '(contract (class/c (inner [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/overment (m x) (+ (super m x) (inner x m x)))))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-inner-6
   '(contract (class/c (inner [m (-> any/c number? number?)]))
              (let* ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))]
                     [d% (class c% (super-new) (define/augride (m x) (add1 x)))])
                (class d% (super-new) (define/override-final (m x) (add1 x))))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-inner-7
   '(contract (class/c (inner m))
              object%
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-inner-8
   '(let* ([c% (contract (class/c (inner m))
                         (class object% (super-new) (define/pubment (m) (inner 3 m)))
                         'pos
                         'neg)])
      (class c% (super-new) (define/augment (m) 5))))
  
  (test/neg-blame
   'class/c-first-order-inner-9
   '(let* ([c% (contract (class/c (inner [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/pubment (m x) (inner x m x)))
                         'pos
                         'neg)])
      (class c% (super-new) (define/augment (m) 5))))
  
  (test/pos-blame
   'class/c-first-order-opaque-inner-1
   '(contract (class/c #:opaque (inner m))
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-opaque-inner-2
   '(contract (class/c #:opaque (inner m) m)
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-local-inner-1
   '(let ()
      (define-local-member-name m)
      (contract (class/c (inner m) m)
                (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                  (class c% (super-new) (define/augride (m x) (add1 x))))
                'pos
                'neg)))
  
  (test/pos-blame
   'class/c-first-order-local-inner-2
   '(let ()
      (define-local-member-name m)
      (contract (class/c (inner m))
                object%
                'pos
                'neg)))
  
  (test/pos-blame
   'class/c-first-order-override-1
   '(contract (class/c (override [m (-> any/c number? number?)]))
              object%
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-override-2
   '(contract (class/c (override [m (-> any/c number? number?)]))
              (class object% (super-new) (define/public (m x) (add1 x)))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-override-3
   '(contract (class/c (override [m (-> any/c number? number?)]))
              (class object% (super-new) (define/pubment (m x) (add1 x)))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-override-4
   '(contract (class/c (override [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/overment (m x) (+ (super m x) (inner x m x)))))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-override-5
   '(contract (class/c (override [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/override (m x) (add1 (super m x)))))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-override-6
   '(contract (class/c (override [m (-> any/c number? number?)]))
              (let* ([c% (class object% (super-new) (define/public (m x) (add1 x)))]
                     [d% (class c% (super-new) (define/overment (m x) 
                                                 (+ (super m x) (inner x m x))))])
                (class d% (super-new) (define/augride (m x) x)))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-override-7
   '(contract (class/c (override m))
              object%
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-override-8
   '(let ([c% (contract (class/c (override m))
                        (class object% (super-new) (define/public (m) 3))
                        'pos
                        'neg)])
      (class c% (super-new) (define/override (m) 5))))
  
  (test/neg-blame
   'class/c-first-order-override-9
   '(let ([c% (contract (class/c (override [m (-> any/c number? number?)]))
                        (class object% (super-new) (define/public (m x) 3))
                        'pos
                        'neg)])
      (class c% (super-new) (define/override (m) 5))))
  
  (test/pos-blame
   'class/c-first-order-opaque-override-1
   '(contract (class/c #:opaque (override m))
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/override (m x) (add1 (super m x)))))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-opaque-override-2
   '(contract (class/c #:opaque (override m) m)
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/override (m x) (add1 (super m x)))))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-local-override-1
   '(let ()
      (define-local-member-name m)
      (contract (class/c (override m))
                object%
                'pos
                'neg)))
  
  (test/spec-passed
   'class/c-first-order-local-override-2
   '(let ()
      (define-local-member-name m)
      (define c% (contract (class/c (override m))
                           (class object% (super-new) (define/public (m) 3))
                           'pos
                           'neg))
      (class c% (super-new) (define/override (m) 5))))
  
  (test/pos-blame
   'class/c-first-order-augment-1
   '(contract (class/c (augment [m (-> any/c number? number?)]))
              object%
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-augment-2
   '(contract (class/c (augment [m (-> any/c number? number?)]))
              (class object% (super-new) (define/pubment (m x) (add1 x)))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-augment-3
   '(contract (class/c (augment [m (-> any/c number? number?)]))
              (class object% (super-new) (define/public (m x) (add1 x)))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-augment-4
   '(contract (class/c (augment [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-augment-5
   '(contract (class/c (augment [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/override (m x) (add1 (super m x)))))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-augment-6
   '(contract (class/c (augment [m (-> any/c number? number?)]))
              (let* ([c% (class object% (super-new) (define/public (m x) (add1 x)))]
                     [d% (class c% (super-new) (define/overment (m x) 
                                                 (+ (super m x) (inner x m x))))])
                (class d% (super-new) (define/augment (m x) x)))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-augment-7
   '(contract (class/c (augment m))
              object%
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-augment-8
   '(let ([c% (contract (class/c (augment m))
                        (class object% (super-new) (define/pubment (m) 3))
                        'pos
                        'neg)])
      (class c% (super-new) (inherit m))))
  
  (test/pos-blame
   'class/c-first-order-augment-9
   '(let ([c% (contract (class/c (augment [m (-> any/c number? number?)]))
                        (class object% (super-new) (define/pubment (m) 3))
                        'pos
                        'neg)])
      (class c% (super-new) (inherit m))))
  
  (test/pos-blame
   'class/c-first-order-opaque-augment-1
   '(contract (class/c #:opaque (augment m))
              (class object% (super-new) (define/pubment (m x) (add1 x)))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-opaque-augment-2
   '(contract (class/c #:opaque (augment m) m)
              (class object% (super-new) (define/pubment (m x) (add1 x)))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-local-augment-1
   '(let ()
      (define-local-member-name m)
      (contract (class/c (augment m))
                object%
                'pos
                'neg)))
  
  (test/spec-passed
   'class/c-first-order-local-augment-2
   '(let ()
      (define-local-member-name m)
      (define c% (contract (class/c (augment m))
                           (class object% (super-new) (define/pubment (m) 3))
                           'pos
                           'neg))
      (class c% (super-new) (inherit m))))
  
  (test/pos-blame
   'class/c-first-order-augride-1
   '(contract (class/c (augride [m (-> any/c number? number?)]))
              object%
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-augride-2
   '(contract (class/c (augride [m (-> any/c number? number?)]))
              (class object% (super-new) (define/pubment (m x) (add1 x)))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-augride-4
   '(contract (class/c (augride [m (-> any/c number? number?)]))
              (class object% (super-new) (define/public (m x) (add1 x)))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-augride-5
   '(contract (class/c (augride [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-augride-5
   '(contract (class/c (augride [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/override (m x) (add1 (super m x)))))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-augride-6
   '(contract (class/c (augride [m (-> any/c number? number?)]))
              (let* ([c% (class object% (super-new) (define/public (m x) (add1 x)))]
                     [d% (class c% (super-new) (define/overment (m x) 
                                                 (+ (super m x) (inner x m x))))])
                (class d% (super-new) (define/augride (m x) x)))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-augride-7
   '(contract (class/c (augride m))
              object%
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-augride-8
   '(let ([c% (contract (class/c (augride m))
                        (class (class object% (super-new) (define/pubment (m) 3))
                          (super-new) (define/augride (m) 4))
                        'pos
                        'neg)])
      (class c% (super-new) (inherit m))))
  
  (test/pos-blame
   'class/c-first-order-augride-9
   '(let ([c% (contract (class/c (augride [m (-> any/c number? number?)]))
                        (class (class object% (super-new) (define/pubment (m) 3))
                          (super-new) (define/augride (m) 4))
                        'pos
                        'neg)])
      (class c% (super-new) (inherit m))))
  
  (test/pos-blame
   'class/c-first-order-opaque-augride-1
   '(contract (class/c #:opaque (augride m))
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-opaque-augride-2
   '(contract (class/c #:opaque (augride m) m)
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-local-augride-1
   '(let ()
      (define-local-member-name m)
      (contract (class/c (augride m))
                object%
                'pos
                'neg)))
  
  (test/spec-passed
   'class/c-first-order-local-augride-2
   '(let ()
      (define-local-member-name m)
      (define c% (contract (class/c (augride m))
                           (class (class object% (super-new) (define/pubment (m) 3))
                             (super-new) (define/augride (m) 4))
                           'pos
                           'neg))
      (class c% (super-new) (inherit m))))
  
  (test/pos-blame
   'class/c-first-order-inherit-1
   '(let* ([c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                         object%
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))])
      (send (new d%) f)))
  
  (test/spec-passed
   'class/c-first-order-inherit-2
   '(let* ([c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) x))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))])
      (send (new d%) f)))
  
  (test/pos-blame
   'class/c-first-order-inherit-3
   '(let* ([c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m) 3))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))])
      (send (new d%) f)))
  
  (test/pos-blame
   'class/c-first-order-opaque-inherit-1
   '(let* ([c% (contract (class/c #:opaque (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) x))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))])
      (send (new d%) f)))
  
  (test/spec-passed
   'class/c-first-order-opaque-inherit-2
   '(let* ([c% (contract (class/c #:opaque m (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) x))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))])
      (send (new d%) f)))
  
  (test/pos-blame
   'class/c-first-order-local-inherit-1
   '(let ()
      (define-local-member-name m)
      (define c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                           object%
                           'pos
                           'neg))
      (define d% (class c% (super-new) (inherit m) (define/public (f) (m 5))))
      (send (new d%) f)))
  
  (test/spec-passed
   'class/c-first-order-local-inherit-2
   '(let ()
      (define-local-member-name m)
      (define c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                           (class object% (super-new) (define/public (m x) x))
                           'pos
                           'neg))
      (define d% (class c% (super-new) (inherit m) (define/public (f) (m 5))))
      (send (new d%) f)))
  
  (test/spec-passed
   'class/c-first-order-absent-1
   '(contract (class/c (absent m)) object% 'pos 'neg))
  
  (test/pos-blame
   'class/c-first-order-absent-2
   '(contract (class/c (absent m))
              (class object% (super-new) (define/public (m) 3))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-absent-3
   '(contract (class/c (absent (field f))) object% 'pos 'neg))
  
  (test/pos-blame
   'class/c-first-order-absent-4
   '(contract (class/c (absent (field f)))
              (class object% (super-new) (field [f 3]))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-first-order-local-absent-1
   '(let ()
      (define-local-member-name f)
      (contract (class/c (absent (field f))) object% 'pos 'neg)))
  
  (test/pos-blame
   'class/c-first-order-local-absent-2
   '(let ()
      (define-local-member-name f)
      (contract (class/c (absent (field f)))
                (class object% (super-new) (field [f 3]))
                'pos
                'neg)))
  
  (test/spec-passed
   'class/c-first-order-opaque-absent-1
   '(contract (class/c #:opaque (absent (field f))) object% 'pos 'neg))
  
  (test/pos-blame
   'class/c-first-order-opaque-absent-2
   '(contract (class/c #:opaque (absent (field f)))
              (class object% (super-new) (field [g 0]))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-first-order-opaque-absent-3
   '(contract (class/c #:opaque (absent (field f g)))
              (class object% (super-new) (field [g 0]))
              'pos
              'neg))
  
  (test/spec-passed
   'class/c-higher-order-init-1
   '(let ([c% (contract (class/c (init [a number?]))
                        (class object% (super-new) (init a))
                        'pos
                        'neg)])
      (new c% [a 3])))
  
  (test/neg-blame
   'class/c-higher-order-init-2
   '(let ([c% (contract (class/c (init [a number?]))
                        (class object% (super-new) (init a))
                        'pos
                        'neg)])
      (new c% [a #t])))
  
  (test/spec-passed
   'class/c-higher-order-init-3
   '(let* ([c% (class object% (super-new) (init a))]
           [d% (contract (class/c (init [a number?] [a string?]))
                         (class c% (super-new) (init a))
                         'pos
                         'neg)])
      (new d% [a 3] [a "foo"])))
  
  (test/neg-blame
   'class/c-higher-order-init-4
   '(let* ([c% (class object% (super-new) (init a))]
           [d% (contract (class/c (init [a number?] [a string?]))
                         (class c% (super-new) (init a))
                         'pos
                         'neg)])
      (new d% [a 3] [a 4])))
  
  (test/neg-blame
   'class/c-higher-order-init-5
   '(let* ([c% (class object% (super-new) (init a))]
           [d% (contract (class/c (init [a number?] [a string?]))
                         (class c% (super-new) (init a))
                         'pos
                         'neg)])
      (new d% [a "bar"] [a "foo"])))
  
  (test/spec-passed
   'class/c-higher-order-init-6
   '(let* ([c% (class object% (super-new) (init a))]
           [d% (class c% (super-new) (init a))]
           [d%/c (contract (class/c (init [a integer?] [a string?])) d% 'pos 'neg1)]
           [d%/c/c (contract (class/c (init [a number?])) d%/c 'pos1 'neg)])
      (new d%/c/c [a 3] [a "foo"])))
  
  (test/neg-blame
   'class/c-higher-order-init-7
   '(let* ([c% (class object% (super-new) (init a))]
           [d% (class c% (super-new) (init a))]
           [d%/c (contract (class/c (init [a integer?] [a string?])) d% 'pos1 'neg)]
           [d%/c/c (contract (class/c (init [a number?])) d%/c 'pos 'neg1)])
      (new d%/c/c [a 3.5] [a "foo"])))
  
  (test/neg-blame
   'class/c-higher-order-init-8
   '(let* ([c% (class object% (super-new) (init a))]
           [d% (class c% (super-new) (init a))]
           [d%/c (contract (class/c (init [a integer?] [a string?])) d% 'pos 'neg)]
           [d%/c/c (contract (class/c (init [a number?])) d%/c 'pos 'neg)])
      (new d%/c/c [a #t] [a "foo"])))
  
  (test/spec-passed
   'class/c-higher-order-init-9
   '(let ([c% (contract (class/c (init [a number?]))
                        (class object% (super-new) (init a))
                        'pos
                        'neg)])
      (make-object c% 3)))
  
  (test/neg-blame
   'class/c-higher-order-init-10
   '(let ([c% (contract (class/c (init [a number?]))
                        (class object% (super-new) (init a))
                        'pos
                        'neg)])
      (make-object c% #f)))
  
  (test/spec-passed
   'class/c-higher-order-init-field-1
   '(let ([c% (contract (class/c (init-field [f (-> number? number?)]))
                        (class object% (super-new) (init-field f) (f 3))
                        'pos
                        'neg)])
      (new c% [f (lambda (x) x)])))
  
  (test/pos-blame
   'class/c-higher-order-init-field-2
   '(let ([c% (contract (class/c (init-field [f (-> number? number?)]))
                        (class object% (super-new) (init-field f) (f #t))
                        'pos
                        'neg)])
      (new c% [f (lambda (x) x)])))
  
  (test/neg-blame
   'class/c-higher-order-init-field-3
   '(let ([c% (contract (class/c (init-field [f (-> number? number?)]))
                        (class object% (super-new) (init-field f) (f 3))
                        'pos
                        'neg)])
      (new c% [f (lambda (x) (zero? x))])))
  
  ;; Make sure that the original provider of the value is blamed if an
  ;; init arg is given an invalid value, and then that is retrieved by
  ;; an external client.
  (test/neg-blame
   'class/c-higher-order-init-field-4
   '(let* ([c% (contract (class/c (init-field [f (-> number? number?)]))
                         (class object% (super-new) (init-field f))
                         'pos
                         'neg)]
           [o (new c% [f (lambda (x) (zero? x))])])
      ((get-field f o) 3)))
  
  (test/spec-passed
   'class/c-higher-order-method-1
   '(let ([c% (contract (class/c [m (-> any/c number? number?)])
                        (class object% (super-new) (define/public (m x) (add1 x)))
                        'pos
                        'neg)])
      (send (new c%) m 3)))
  
  (test/neg-blame
   'class/c-higher-order-method-2
   '(let ([c% (contract (class/c [m (-> any/c number? number?)])
                        (class object% (super-new) (define/public (m x) (add1 x)))
                        'pos
                        'neg)])
      (send (new c%) m #f)))
  
  (test/pos-blame
   'class/c-higher-order-method-3
   '(let ([c% (contract (class/c [m (-> any/c number? number?)])
                        (class object% (super-new) (define/public (m x) (zero? x)))
                        'pos
                        'neg)])
      (send (new c%) m 3)))
  
  ;; test that unspecified inits and fields aren't internally conflating #f with the contract #f
  (test/spec-passed
   'false/no-contract-conflation1
   '(new (contract (class/c (init x))
                   (class object% (init x) (super-new))
                   'pos 
                   'neg)
         [x 1]))
  
  (test/neg-blame
   'false/no-contract-conflation2
   '(new (contract (class/c (init [x #f]))
                   (class object% (init x) (super-new))
                   'pos 
                   'neg)
         [x 1]))
  
  (test/spec-passed
   'false/no-contract-conflation3
   '(new (contract (class/c (field x))
                   (class object% (field [x 1]) (super-new))
                   'pos 
                   'neg)))
  
  (test/pos-blame
   'false/no-contract-conflation4
   '(get-field x
               (new (contract (class/c (field [x #f]))
                              (class object% (field [x 1]) (super-new))
                              'pos 
                              'neg))))
  
  (test/spec-passed
   'false/no-contract-conflation5
   '(new (contract (class/c (init-field x))
                   (class object% (init-field x) (super-new))
                   'pos 
                   'neg)
         [x 1]))
  
  (test/neg-blame
   'false/no-contract-conflation6
   '(new (contract (class/c (init-field [x #f]))
                   (class object% (init-field x) (super-new))
                   'pos 
                   'neg)
         [x 1]))
  
  ;; Test that public method contracts are not checked for implication.
  ;; Public method contracts do not check behavioral subtyping.
  ;; Once interfaces have contracts, those will.
  (test/spec-passed
   'class/c-higher-order-method-4
   '(let* ([c% (contract (class/c [m (-> any/c number? number?)])
                         (class object% (super-new) (define/public (m x) (zero? x)))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/override (m) (super m 5)))])
      (send (new d%) m)))
  
  (test/spec-passed
   'class/c-higher-order-local-method-1
   '(let ()
      (define-local-member-name m)
      (define c% (contract (class/c [m (-> any/c number? number?)])
                           (class object% (super-new) (define/public (m x) (add1 x)))
                           'pos
                           'neg))
      (send (new c%) m 3)))
  
  (test/neg-blame
   'class/c-higher-order-local-method-2
   '(let ()
      (define-local-member-name m)
      (define c% (contract (class/c [m (-> any/c number? number?)])
                           (class object% (super-new) (define/public (m x) (add1 x)))
                           'pos
                           'neg))
      (send (new c%) m #f)))
  
  (test/spec-passed
   'class/c-higher-order-super-1
   '(let* ([c% (contract (class/c [m (-> any/c integer? integer?)]
                                  (super [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) (add1 x)))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/override (m x) (+ x (super m 3.5))))])
      (send (new d%) m 4.5)))
  
  (test/neg-blame
   'class/c-higher-order-super-2
   '(let* ([c% (contract (class/c [m (-> any/c integer? integer?)]
                                  (super [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) (add1 x)))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/override (m) (super m #f)))])
      (send (new d%) m)))
  
  (test/pos-blame
   'class/c-higher-order-super-3
   '(let* ([c% (contract (class/c [m (-> any/c integer? integer?)]
                                  (super [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) (zero? x)))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/override (m) (super m 3.5)))])
      (send (new d%) m)))
  
  (test/spec-passed
   'class/c-higher-order-inner-1
   '(let* ([c% (contract (class/c (inner [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (+ x (inner x m x))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/augride (m x) (add1 x)))])
      (send (new d%) m 3)))
  
  (test/neg-blame
   'class/c-higher-order-inner-2
   '(let* ([c% (contract (class/c (inner [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (+ x (inner x m x))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/augride (m x) (zero? x)))])
      (send (new d%) m 3)))
  
  (test/pos-blame
   'class/c-higher-order-inner-3
   '(let* ([c% (contract (class/c (inner [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) 
                                                      (+ x (inner x m (zero? x)))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/augride (m x) (add1 x)))])
      (send (new d%) m 3)))
  
  (test/neg-blame
   'class/c-higher-order-inner-4
   '(let* ([c% (contract (class/c (inner [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (+ x (inner x m x))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/augride (m x) (add1 x)))]
           [e% (class d% (super-new) (define/override (m x) (zero? (super m x))))])
      (send (new e%) m 3)))
  
  (test/spec-passed
   'class/c-higher-order-inner-5
   '(let* ([c% (contract (class/c (inner [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (+ x (inner x m x))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/augment (m x) (if (inner x m x) (add1 x) x)))]
           [e% (class d% (super-new) (define/augride (m x) (zero? x)))])
      (send (new e%) m 3)))
  
  (let ([expected-obj
         (λ (exn) (regexp-match? #rx"promised: an object" (exn-message exn)))]
        [expected-class
         (λ (exn) (regexp-match? #rx"promised: a class" (exn-message exn)))])
    (contract-error-test
     'not-an-object-1
     '(contract (object/c) 3 'pos 'neg)
     expected-obj)
    
    (contract-error-test
     'not-an-object-2
     '(contract (instanceof/c (class/c)) 3 'pos 'neg)
     expected-obj)
    
    (contract-error-test
     'not-a-class-1
     '(contract (class/c) 3 'pos 'neg)
     expected-class))
  
  ;; Make sure the order of the wrapping is correct in the next two.
  (test/neg-blame
   'class/c-higher-order-inner-6
   '(let* ([c% (contract (class/c (inner [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (+ x (inner x m x))))
                         'pos
                         'neg1)]
           [d% (contract (class/c (inner [m (-> any/c number? number?)]))
                         c%
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (define/augride (m x) (zero? x)))])
      (send (new e%) m 3)))
  
  (test/pos-blame
   'class/c-higher-order-inner-7
   '(let* ([c% (contract (class/c (inner [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (+ x (inner x m #f))))
                         'pos
                         'neg1)]
           [d% (contract (class/c (inner [m (-> any/c number? number?)]))
                         c%
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (define/augride (m x) (add1 x)))])
      (send (new e%) m 3)))
  
  ;; Test that overriding an augmenting method can still be effected by an inner contract.
  (test/neg-blame
   'class/c-higher-order-inner-8
   '(let* ([c% (contract (class/c (inner [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/pubment (m x) (+ x (inner x m 3))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/augride (m x) (add1 x)))]
           [e% (class d% (super-new) (define/override (m x) (zero? (super m x))))])
      (send (new e%) m 3)))
  
  ;; The inner contract can be added before the next augmenting method, as seen here.
  (test/neg-blame
   'class/c-higher-order-inner-9
   '(let* ([c% (class object% (super-new) (define/pubment (m x) (+ x (inner x m 3))))]
           [d% (contract (class/c (inner [m (-> any/c number? number?)]))
                         (class c% (super-new) (define/augride (m x) (add1 x)))
                         'pos
                         'neg)]
           [e% (class d% (super-new) (define/override (m x) (zero? (super m x))))])
      (send (new e%) m 3)))
  
  ;; Show both inner and super contracts.
  (test/spec-passed
   'class/c-higher-order-inner-10
   '(let* ([c% (class object% (super-new) (define/pubment (m x) (+ x (inner x m 3))))]
           [d% (contract (class/c (inner [m (-> any/c number? number?)])
                                  (super [m (-> any/c number? number?)]))
                         (class c% (super-new) (define/augride (m x) (add1 x)))
                         'pos
                         'neg)]
           [e% (class d% (super-new) (define/override (m x) (+ x (super m x))))])
      (send (new e%) m 3)))
  
  (test/pos-blame
   'class/c-higher-order-inner-11
   '(let* ([c% (class object% (super-new) (define/pubment (m x) (+ x (inner x m #f))))]
           [d% (contract (class/c (inner [m (-> any/c number? number?)])
                                  (super [m (-> any/c number? number?)]))
                         (class c% (super-new) (define/augride (m x) (add1 x)))
                         'pos
                         'neg)]
           [e% (class d% (super-new) (define/override (m x) (+ x (super m x))))])
      (send (new e%) m 3)))
  
  (test/neg-blame
   'class/c-higher-order-inner-10
   '(let* ([c% (class object% (super-new) (define/pubment (m x) (+ x (inner x m 3))))]
           [d% (contract (class/c (inner [m (-> any/c number? number?)])
                                  (super [m (-> any/c number? number?)]))
                         (class c% (super-new) (define/augride (m x) (add1 x)))
                         'pos
                         'neg)]
           [e% (class d% (super-new) (define/override (m x) (+ x (super m #f))))])
      (send (new e%) m 3)))
  
  (test/spec-passed/result
   'class/c-higher-order-field-1
   '(let* ([c% (contract (class/c (field [f number?]))
                         (class object% (super-new) (field [f 10]))
                         'pos
                         'neg)])
      (get-field f (new c%)))
   10)
  
  (test/spec-passed/result
   'class/c-higher-order-field-2
   '(let* ([c% (contract (class/c (field [f number?]))
                         (class object% (super-new) (field [f 10]))
                         'pos
                         'neg)]
           [o (new c%)])
      (set-field! f o 5)
      (get-field f o))
   5)
  
  (test/pos-blame
   'class/c-higher-order-field-3
   '(let* ([c% (contract (class/c (field [f number?]))
                         (class object% (super-new) (field [f #f]))
                         'pos
                         'neg)]
           [o (new c%)])
      (get-field f o)))
  
  (test/neg-blame
   'class/c-higher-order-field-4
   '(let* ([c% (contract (class/c (field [f number?]))
                         (class object% (super-new) (field [f 10]))
                         'pos
                         'neg)]
           [o (new c%)])
      (set-field! f o #f)))
  
  (test/spec-passed
   'class/c-higher-order-field-5
   '(let ([c% (contract (class/c (field f))
                        (class object% (super-new) (field [f 10]))
                        'pos
                        'neg)])
      (get-field f (new c%))))
  
  (test/spec-passed/result
   'class/c-higher-order-inherit-field-1
   '(let* ([c% (contract (class/c (inherit-field [f number?]))
                         (class object% (super-new) (field [f 10]))
                         'pos
                         'neg)]
           [d% (class c% (super-new)
                 (inherit-field f)
                 (define/public (m) f))])
      (send (new d%) m))
   10)
  
  (test/spec-passed/result
   'class/c-higher-order-inherit-field-2
   '(let* ([c% (contract (class/c (inherit-field [f number?]))
                         (class object% (super-new) (field [f 10]))
                         'pos
                         'neg)]
           [d% (class c% (super-new)
                 (inherit-field f)
                 (define/public (m) (set! f 12)))]
           [o (new d%)])
      (send o m)
      (get-field f o))
   12)
  
  (test/pos-blame
   'class/c-higher-order-inherit-field-3
   '(let* ([c% (contract (class/c (inherit-field [f number?]))
                         (class object% (super-new) (field [f #f]))
                         'pos
                         'neg)]
           [d% (class c% (super-new)
                 (inherit-field f)
                 (define/public (m) f))])
      (send (new d%) m)))
  
  (test/neg-blame
   'class/c-higher-order-inherit-field-4
   '(let* ([c% (contract (class/c (inherit-field [f number?]))
                         (class object% (super-new) (field [f 10]))
                         'pos
                         'neg)]
           [d% (class c% (super-new)
                 (inherit-field f)
                 (define/public (m) (set! f #f)))])
      (send (new d%) m)))
  
  (test/spec-passed
   'class/c-higher-order-inherit-field-5
   '(let* ([c% (contract (class/c (inherit-field f))
                         (class object% (super-new) (field [f 10]))
                         'pos
                         'neg)]
           [d% (class c% (super-new)
                 (inherit-field f)
                 (define/public (m) f))])
      (send (new d%) m)))
  
  (test/spec-passed
   'class/c-higher-order-override-1
   '(let* ([c% (contract (class/c (override [m (-> any/c integer? integer?)]))
                         (class object% (super-new)
                           (define/public (m x) x)
                           (define/public (f x) (m x)))
                         'pos
                         'neg)]
           [d% (class c% (super-new)
                 (define/public (g x) (m x))
                 (define/override (m x) (add1 (super m x))))])
      (send (new d%) g 3.5)))
  
  (test/neg-blame
   'class/c-higher-order-override-2
   '(let* ([c% (contract (class/c (override [m (-> any/c number? number?)]))
                         (class object% (super-new)
                           (define/public (m x) x)
                           (define/public (f x) (add1 (m x))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/override (m x) (zero? (super m x))))])
      (send (new d%) f 3)))
  
  (test/neg-blame
   'class/c-higher-order-override-3
   '(let* ([c% (contract (class/c (override [m (-> any/c number? number?)]))
                         (class object% (super-new)
                           (define/public (m x) (zero? x))
                           (define/public (f x) (add1 (m x))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/override (m x) (super m x)))])
      (send (new d%) f 3)))
  
  (test/pos-blame
   'class/c-higher-order-override-4
   '(let* ([c% (contract (class/c (override [m (-> any/c number? number?)]))
                         (class object% (super-new)
                           (define/public (m x) x)
                           (define/public (f x) (add1 (m #f))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/override (m x) (+ x (super m x))))])
      (send (new d%) f 3)))
  
  (test/pos-blame
   'class/c-higher-order-override-5
   '(let* ([c% (contract (class/c (override [m (-> any/c number? number?)]))
                         (class object% (super-new)
                           (define/public (m x) x)
                           (define/public (f x) (add1 (m #f))))
                         'pos
                         'neg1)]
           [d% (contract (class/c (override [m (-> any/c string? string?)]))
                         c%
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (define/override (m x) (+ x (super m x))))])
      (send (new e%) f 3)))
  
  (test/spec-passed
   'class/c-higher-order-override-6
   '(let* ([c% (contract (class/c (override [m (-> any/c number? number?)]))
                         (class object% (super-new)
                           (define/public (m x) x)
                           (define/public (f x) (add1 (m 3.5))))
                         'pos
                         'neg1)]
           [d% (contract (class/c (override [m (-> any/c integer? integer?)]))
                         (class c% (super-new) (inherit m) (define/public (g x) (add1 (m 3))))
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (define/override (m x) (+ x (super m x))))])
      (send (new e%) g 3)))
  
  (test/pos-blame
   'class/c-higher-order-override-7
   '(let* ([c% (contract (class/c (override [m (-> any/c number? number?)]))
                         (class object% (super-new)
                           (define/public (m x) x)
                           (define/public (f x) (add1 (m #f))))
                         'pos
                         'neg1)]
           [d% (contract (class/c (override [m (-> any/c integer? integer?)]))
                         (class c% (super-new) (define/public (g x) (add1 (m 3))))
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (define/override (m x) (+ x (super m x))))])
      (send (new e%) f 3)))
  
  (test/spec-passed
   'class/c-higher-order-augment-1
   '(let* ([c% (contract (class/c (augment [m (-> any/c integer? integer?)]))
                         (class object% (super-new)
                           (define/pubment (m x) x)
                           (define/public (f x) (m (zero? x))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (g x) (m x)))])
      (send (new d%) f 3)))
  
  (test/neg-blame
   'class/c-higher-order-augment-2
   '(let* ([c% (contract (class/c (augment [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) x))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (g x) (m x)))])
      (send (new d%) g 3.5)))
  
  (test/pos-blame
   'class/c-higher-order-augment-3
   '(let* ([c% (contract (class/c (augment [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) #f))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (g x) (m x)))])
      (send (new d%) g 3)))
  
  (test/pos-blame
   'class/c-higher-order-augment-4
   '(let* ([c% (contract (class/c (augment [m (-> any/c number? integer?)]))
                         (class object% (super-new) (define/pubment (m x) #f))
                         'pos
                         'neg1)]
           [d% (contract (class/c (augment [m (-> any/c integer? number?)]))
                         c%
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (inherit m) (define/public (g x) (m x)))])
      (send (new e%) g 3)))
  
  (test/neg-blame
   'class/c-higher-order-augment-5
   '(let* ([c% (contract (class/c (augment [m (-> any/c number? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (floor x)))
                         'pos
                         'neg1)]
           [d% (contract (class/c (augment [m (-> any/c integer? number?)]))
                         c%
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (inherit m) (define/public (g x) (m x)))])
      (send (new e%) g 3.5)))
  
  (test/spec-passed
   'class/c-higher-order-augment-6
   '(let* ([c% (contract (class/c (augment [m (-> any/c number? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (floor x)))
                         'pos
                         'neg1)]
           [d% (contract (class/c (augment [m (-> any/c integer? number?)]))
                         (class c% (super-new) (inherit m) (define/public (f x) (m x)))
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (inherit m) (define/public (g x) (m x)))])
      (send (new e%) f 3.5)))
  
  (test/spec-passed
   'class/c-higher-order-inherit-1
   '(let* ([c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) x))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))])
      (send (new d%) f)))
  
  (test/neg-blame
   'class/c-higher-order-inherit-2
   '(let* ([c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) x))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m #f)))])
      (send (new d%) f)))
  
  (test/pos-blame
   'class/c-higher-order-inherit-3
   '(let* ([c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) (zero? x)))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))])
      (send (new d%) f)))
  
  ;; Should not be checked if overridden (i.e. target of dyn disp changes).
  (test/spec-passed
   'class/c-higher-order-inherit-4
   '(let* ([c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) (zero? x)))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))]
           [e% (class d% (super-new) (define/override (m x) x))])
      (send (new e%) f)))
  
  (test/spec-passed
   '->m-first-order-1
   '(contract (class/c [m (->m number? number?)])
              (class object% (super-new) (define/public (m x) x))
              'pos
              'neg))
  
  (test/pos-blame
   '->m-first-order-2
   '(contract (class/c [m (->m any/c number? number?)])
              (class object% (super-new) (define/public (m x) x))
              'pos
              'neg))
  
  (test/spec-passed
   '->m-kwd-first-order-1
   '(contract (class/c [m (->m #:x number? number?)])
              (class object% (super-new) (define/public (m #:x x) x))
              'pos
              'neg))
  
  (test/pos-blame
   '->m-kwd-first-order-2
   '(contract (class/c [m (->m #:y number? number?)])
              (class object% (super-new) (define/public (m #:x x) x))
              'pos
              'neg))
  
  (test/spec-passed
   '->*m-first-order-1
   '(contract (class/c [m (->*m (number?) (string?) number?)])
              (class object% (super-new) (define/public (m x [f "foo"]) x))
              'pos
              'neg))
  
  (test/pos-blame
   '->*m-first-order-2
   '(contract (class/c [m (->*m (any/c number?) (string?) number?)])
              (class object% (super-new) (define/public (m x [f "foo"]) x))
              'pos
              'neg))
  
  (test/spec-passed
   '->dm-first-order-1
   '(contract (class/c [pop (->dm () () #:pre-cond (not (send this empty?)) [_ number?])]
                       [push (->dm ([arg number?]) () [_ void?] #:post-cond (not (send this empty?)))]
                       [empty? (->m boolean?)])
              (class object% (super-new)
                (define stack null)
                (define/public (empty?) (null? stack))
                (define/public (push v) (set! stack (cons v stack)))
                (define/public (pop) (let ([res (car stack)]) (set! stack (cdr stack)) res)))
              'pos
              'neg))
  
  (test/pos-blame
   '->dm-first-order-1
   '(contract (class/c [pop (->dm () () #:pre-cond (not (send this empty?)) [_ number?])]
                       [push (->dm ([arg number?]) () [_ void?] #:post-cond (not (send this empty?)))]
                       [empty? (->m boolean?)])
              (class object% (super-new)
                (define stack null)
                (define/public (empty?) (null? stack))
                (define/public (push v) (set! stack (cons v stack)))
                (define/public (pop v) (let ([res (car stack)]) (set! stack (cdr stack)) res)))
              'pos
              'neg))
  
  (test/spec-passed
   '->dm-higher-order-1
   '(let* ([stack% (contract (class/c 
                              [pop (->dm () () #:pre-cond (not (send this empty?)) [_ number?])]
                              [push (->dm ([arg number?]) () [_ void?] 
                                          #:post-cond (not (send this empty?)))]
                              [empty? (->m boolean?)])
                             (class object% (super-new)
                               (define stack null)
                               (define/public (empty?) (null? stack))
                               (define/public (push v) (set! stack (cons v stack)))
                               (define/public (pop)
                                 (let ([res (car stack)]) (set! stack (cdr stack)) res)))
                             'pos
                             'neg)]
           [o (new stack%)])
      (send o push 4)
      (send o empty?)
      (send o pop)))
  
  (test/pos-blame
   '->dm-higher-order-2
   '(let* ([stack% (contract (class/c
                              [pop (->dm () () #:pre-cond (not (send this empty?)) [_ number?])]
                              [push (->dm ([arg number?]) () [_ void?] 
                                          #:post-cond (not (send this empty?)))]
                              [empty? (->m boolean?)])
                             (class object% (super-new)
                               (define stack null)
                               (define/public (empty?) (null? stack))
                               (define/public (push v) (void))
                               (define/public (pop)
                                 (define res (car stack))
                                 (set! stack (cdr stack))
                                 res))
                             'pos
                             'neg)]
           [o (new stack%)])
      (send o push 4)
      (send o empty?)
      (send o pop)))
  
  (test/neg-blame
   '->dm-higher-order-3
   '(let* ([stack% (contract 
                    (class/c
                     [pop (->dm () () #:pre-cond (not (send this empty?)) [_ number?])]
                     [push (->dm ([arg number?]) () [_ void?] #:post-cond (not (send this empty?)))]
                     [empty? (->m boolean?)])
                    (class object% (super-new)
                      (define stack null)
                      (define/public (empty?) (null? stack))
                      (define/public (push v) (set! stack (cons v stack)))
                      (define/public (pop) (let ([res (car stack)]) (set! stack (cdr stack)) res)))
                    'pos
                    'neg)]
           [o (new stack%)])
      (send o pop)))
  
  (test/spec-passed
   'case->m-first-order-1
   '(contract (class/c [m (case->m (-> number? number?) (-> number? number? number?))])
              (class object% (super-new) (define/public (m x [y 3]) (+ x y)))
              'pos
              'neg))
  
  (test/pos-blame
   'case->m-first-order-2
   '(contract (class/c [m (case->m (-> number? number?) (-> number? number? number?))])
              (class object% (super-new) (define/public (m x) (+ x y)))
              'pos
              'neg))
  
  (test/spec-passed
   'case->m-higher-order-1
   '(let ([cls% (contract (class/c [m (case->m (-> number? number?) (-> number? number? number?))])
                          (class object% (super-new) (define/public (m x [y 3]) (+ x y)))
                          'pos
                          'neg)])
      (send (new cls%) m 3)
      (send (new cls%) m 3 4)))
  
  (test/neg-blame
   'case->m-higher-order-2
   '(let ([cls% (contract (class/c [m (case->m (-> number? number?) (-> number? number? number?))])
                          (class object% (super-new) (define/public (m x [y 3]) (+ x y)))
                          'pos
                          'neg)])
      (send (new cls%) m #t)))
  
  (test/neg-blame
   'case->m-higher-order-3
   '(let ([cls% (contract (class/c [m (case->m (-> number? number?) (-> number? number? number?))])
                          (class object% (super-new) (define/public (m x [y 3]) (+ x y)))
                          'pos
                          'neg)])
      (send (new cls%) m 3 #t)))
  
  
  (contract-eval '(define a-class% (class object% (define/public (m x) x) (super-new))))
  (ctest #t 
         is-a?
         (new (contract (class/c [m (-> any/c integer? integer?)])
                        a-class% 'pos 'neg))
         a-class%)
  
  (test/spec-passed/result
   'missing-method1
   '(send (new (contract (class/c)
                         (class object% 
                           (define/public (m) 1)
                           (super-new))
                         'pos 'neg))
          m)
   1)
  
  (test/spec-passed/result
   'missing-method2
   '(send (new (contract (class/c)
                         (class object% 
                           (define/public (m #:x x) x)
                           (super-new))
                         'pos 'neg))
          m #:x 123)
   123)
  
  (test/spec-passed
   'double-wrapping1
   '(new (contract (class/c)
                   (contract (class/c) object% 'pos1 'neg1)
                   'pos2 'neg2)))
  
  (test/spec-passed/result
   'double-wrapping2
   '(send (new (contract (class/c [m (-> any/c (and/c integer? even?)
                                         (and/c integer? even?))])
                         (contract (class/c [m (-> any/c (and/c positive? even?)
                                                   (and/c positive? even?))])
                                   (class object% 
                                     (define/public (m x) x)
                                     (super-new))
                                   'pos 'neg)
                         'somethingelse1 'somethingelse2))
          m 2)
   2)
  
  (test/neg-blame
   'double-wrapping3
   '(send (new (contract (class/c [m (-> any/c (and/c integer? even?)
                                         (and/c integer? even?))])
                         (contract (class/c [m (-> any/c (and/c positive? even?)
                                                   (and/c positive? even?))])
                                   (class object% 
                                     (define/public (m x) x)
                                     (super-new))
                                   'pos 'neg)
                         'somethingelse1 'somethingelse2))
          m -2))
  
  (test/neg-blame
   'double-wrapping4
   '(send (new (contract (class/c [m (-> any/c (and/c integer? even?)
                                         (and/c integer? even?))])
                         (contract (class/c [m (-> any/c (and/c positive? even?)
                                                   (and/c positive? even?))])
                                   (class object% 
                                     (define/public (m x) x)
                                     (super-new))
                                   'somethingelse1 'somethingelse2)
                         'pos 'neg))
          m 1))
  
  (test/spec-passed/result
   'double-wrapping5
   '(send (new (contract (class/c [m (-> any/c (and/c integer? even?)
                                         (and/c integer? even?))])
                         (contract (class/c [m (-> any/c (and/c positive? even?)
                                                   (and/c positive? even?))])
                                   (class object% 
                                     (define/public (m x) x)
                                     (define/public (n x) x)
                                     (super-new))
                                   'somethingelse1 'somethingelse2)
                         'pos 'neg))
          n 1)
   1)
  
  (test/neg-blame
   'double-wrapping6
   '(send (new (contract (class/c [m (-> any/c (and/c integer? even?)
                                         (and/c integer? even?))]
                                  [n (-> any/c boolean?  boolean?)])
                         (contract (class/c [m (-> any/c (and/c positive? even?)
                                                   (and/c positive? even?))])
                                   (class object% 
                                     (define/public (m x) x)
                                     (define/public (n x) x)
                                     (super-new))
                                   'somethingelse1 'somethingelse2)
                         'pos 'neg))
          n 1))
  
  (test/neg-blame
   'double-wrapping7
   '(new (contract (class/c (init [x (and/c integer? even?)]))
                   (contract (class/c (init [x (and/c integer? positive?)]))
                             (class object% (init x) (super-new))
                             'somethingelse1 'somethingelse2)
                   'pos 'neg)
         [x 1]))
  
  (test/neg-blame
   'double-wrapping8
   '(new (contract (class/c (init [x (and/c integer? even?)]))
                   (contract (class/c (init [x (and/c integer? positive?)]))
                             (class object% (init x) (super-new))
                             'pos 'neg)
                   'somethingelse1 'somethingelse2)
         [x -2]))
  
  (test/spec-passed
   'double-wrapping9
   '(new (contract (class/c (init [x (and/c integer? even?)]))
                   (contract (class/c (init [x (and/c integer? positive?)]))
                             (class object% (init x) (super-new))
                             'somethingelse1 'somethingelse2)
                   'pos 'neg)
         [x 2]))
  
  (test/spec-passed/result
   'class-field-accessor1
   '(let ([c% (class object% (super-new) (field [f 1]))])
      ((class-field-accessor c% f)
       (new (contract (class/c) c% 'pos 'neg))))
   1)
  
  (test/spec-passed
   'class-field-accessor2
   '(let* ([c% (class object% (field [f 1]) (super-new))]
           [c+c% (contract (class/c (field [f integer?])) c% 'pos 'neg)])
      ((class-field-accessor c+c% f) (new c%))))
  
  (test/pos-blame
   'class-field-accessor3
   '(let* ([c% (class object% (field [f #f]) (super-new))]
           [c+c% (contract (class/c (field [f integer?])) c% 'pos 'neg)])
      ((class-field-accessor c+c% f) (new c%))))
  
  (test/spec-passed
   'class-field-accessor4
   '(let* ([c% (class object% (field [f #f]) (super-new))]
           [c+c% (contract (class/c (field [f integer?])) c% 'pos 'neg)])
      ((class-field-accessor c% f) (new c+c%))))
  
  (test/spec-passed/result
   'class-field-mutator1
   '(let* ([c% (class object% (super-new) (field [f 1]))]
           [o (new (contract (class/c) c% 'pos 'neg))])
      ((class-field-mutator c% f) o 2)
      ((class-field-accessor c% f) o))
   2)
  
  (test/spec-passed/result
   'class-field-mutator2
   '(let* ([c% (class object% (super-new) (field [f 1]))]
           [o (new (contract (class/c (field [f boolean?])) c% 'pos 'neg))])
      ((class-field-mutator c% f) o #f)
      ((class-field-accessor c% f) o))
   #f)
  
  (test/spec-passed
   'class-field-mutator3 
   '(let* ([c% (class object% (super-new) (field [f 1]))]
           [o (new (contract (class/c (field [f boolean?])) c% 'pos 'neg))])
      ((class-field-mutator c% f) o 11)))
  
  (test/neg-blame
   'class-field-mutator4
   '(let* ([c% (class object% (super-new) (field [f 1]))]
           [c%+c (contract (class/c (field [f boolean?])) c% 'pos 'neg)]
           [o (new c%+c)])
      ((class-field-mutator c%+c f) o 11)))
  
  (test/neg-blame
   'class-field-mutator5
   '(let* ([c% (class object% (super-new) (field [f 1]))]
           [c%+c (contract (class/c (field [f boolean?])) c% 'pos 'neg)]
           [o (new c%)])
      ((class-field-mutator c%+c f) o 11)))
  
  (test/spec-passed/result
   'order-of-evaluation
   '(let ([x '()])
      (class/c [m (begin (set! x (cons 1 x)) any/c)]
               (field [a (begin (set! x (cons 2 x)) any/c)]
                      [d (begin (set! x (cons 3 x)) any/c)]
                      [b (begin (set! x (cons 4 x)) any/c)])
               [n (begin (set! x (cons 5 x)) any/c)])
      x)
   '(5 4 3 2 1))
  
  (test/spec-passed/result
   'object-method-arity-includes-1
   '(object-method-arity-includes?
     (new (contract (class/c [m (-> any/c integer? integer?)])
                    (class object% (super-new) (define/public (m x) (add1 x)))
                    'pos
                    'neg))
     'm
     1)
   #t)
  
  (test/spec-passed/result
   'object-method-arity-includes-2
   '(object-method-arity-includes?
     (new (contract (class/c [m (-> any/c integer? integer?)])
                    (contract (class/c [m (-> any/c integer? integer?)])
                              (class object% (super-new) (define/public (m x) (add1 x)))
                              'pos1
                              'neg1)
                    'pos2
                    'neg2))
     'm
     1)
   #t)
  
  (test/spec-passed/result
   'class/c-init-field-no-code-duplication
   '(let ([x 0])
      (class/c (init-field [x (begin (set! x (+ x 1)) any/c)]))
      x)
   1)
  
  (test/pos-blame
   'class/c-field-in-class-with-interface-ctc
   '(get-field f (new (contract (class/c (field [f integer?]))
                                (class* object% ((interface () [m (-> any/c integer?)]))
                                  (field [f #f])
                                  (define/public (m) 1)
                                  (super-new))
                                'pos
                                'neg))))
  
  (test/pos-blame
   'class/c-field-in-class-with-interface-ctc2
   '(new (contract (class/c (field f))
                   (class* object% ((interface () [m (-> any/c integer?)]))
                     (define/public (m) 1)
                     (super-new))
                   'pos
                   'neg)))
  
  (test/pos-blame
   'class/c-absent-in-class-with-interface-ctc
   '(contract (class/c (absent m))
              (class* object% ((interface () [m (-> any/c integer?)]))
                (define/public (m) 1)
                (super-new))
              'pos
              'neg))
  
  (test/pos-blame
   'class/c-absent-in-class-with-interface-ctc
   '(contract (class/c (absent (field f)))
              (class* object% ((interface () [m (-> any/c integer?)]))
                (define/public (m) 1)
                (field [f 1])
                (super-new))
              'pos
              'neg))
  
  (test/spec-passed
   'generic1
   '(let* ([c% (class object%
                 (super-new)
                 (define/public (m x) x))])
      (send-generic (new (contract (class/c
                                    (m (->m integer? integer?)))
                                   c%
                                   'pos 'neg))
                    (generic c% m)
                    5)))
  
  (test/neg-blame
   'generic2
   '(let* ([c% (class object%
                 (super-new)
                 (define/public (m x) x))])
      (send-generic (new (contract (class/c
                                    (m (->m integer? integer?)))
                                   c%
                                   'pos 'neg))
                    (generic c% m)
                    #f)))
  
  (test/spec-passed
   'generic3
   '(let* ([i<%> (interface () m)]
           [c% (class* object% (i<%>)
                (super-new)
                (define/public (m x) x))])
      (send-generic (new (contract (class/c
                                    (m (->m integer? integer?)))
                                   c%
                                   'pos 'neg))
                    (generic i<%> m)
                    5)))
  
  (test/neg-blame
   'generic4
   '(let* ([i<%> (interface () m)]
           [c% (class* object% (i<%>)
                (super-new)
                (define/public (m x) x))])
      (send-generic (new (contract (class/c
                                    (m (->m integer? integer?)))
                                   c%
                                   'pos 'neg))
                    (generic i<%> m)
                    #f)))
  
  (test/spec-passed
   'generic5
   '(let* ([c% (class object%
                 (super-new)
                 (define/public (m x) x))]
           [c%+c (contract (class/c (m (->m integer? integer?)))
                           c%
                           'pos 'neg)]
           [o (new c%)]
           [g (generic c%+c m)])
      (send-generic o g #f)))

  (test/neg-blame
   'generic6
   '(let* ([c% (class object%
                 (super-new)
                 (define/public (m x) x))]
           [c%+c (contract (class/c (m (->m integer? integer?)))
                           c%
                           'pos 'neg)]
           [o (new c%+c)]
           [g (generic c% m)])
      (send-generic o g #f)))
  
  (test/spec-passed
   'generic7
   '(let* ([i<%> (interface () m)]
           [c% (class* object% (i<%>)
                (super-new)
                (define/public (m x) x))])
      (send-generic (new (contract (class/c m)
                                   c%
                                   'pos 'neg))
                    (generic c% m)
                    5)))
  
  (test/spec-passed
   'generic8
   '(let* ([i<%> (interface () n)]
           [c% (class* object% (i<%>)
                (super-new)
                 (define/public (m x) x)
                 (define/public (n x) x))])
      (send-generic (new (contract (class/c m)
                                   c%
                                   'pos 'neg))
                    (generic c% n)
                    5)))
  
  (test/spec-passed
   'dynamic-send1
   '(dynamic-send (new (contract (class/c [m (->m integer? integer?)])
                                 (class object% (define/public (m x) x) (super-new))
                                 'pos
                                 'neg))
                  'm 1))
  (test/spec-passed
   'dynamic-send2
   '(dynamic-send (new (contract (class/c m)
                                 (class object% (define/public (m x) x) (super-new))
                                 'pos
                                 'neg))
                  'm 1))
  (test/spec-passed
   'dynamic-send3
   '(dynamic-send (new (contract (class/c m)
                                 (class object% 
                                   (define/public (m x) x) 
                                   (define/public (n x) x)
                                   (super-new))
                                 'pos
                                 'neg))
                  'n 1))
  (test/neg-blame
   'dynamic-send4
   '(dynamic-send (new (contract (class/c [m (->m integer? integer?)])
                                 (class object% (define/public (m x) x) (super-new))
                                 'pos
                                 'neg))
                  'm #f))
  
  (test/spec-passed
   'with-method1
   '(let ([o (new (contract (class/c [m (->m integer? integer?)])
                            (class object% (define/public (m x) x) (super-new))
                            'pos
                            'neg))])
      (with-method ([m (o m)])
        (m 1))))
  
  (test/neg-blame
   'with-method1
   '(let ([o (new (contract (class/c [m (->m integer? integer?)])
                            (class object% (define/public (m x) x) (super-new))
                            'pos
                            'neg))])
      (with-method ([m (o m)])
        (m #f))))
  
  (test/spec-passed
   'mixin1
   '((mixin () () (super-new)) 
     (contract (class/c m)
               (class object% (define/public (m x) x) (super-new))
               'pos
               'neg)))
  
  (test/spec-passed
   'mixin2
   '(send (new ((mixin () () (super-new)) 
                (contract (class/c [m (->m integer? integer?)])
                          (class object% (define/public (m x) x) (super-new))
                          'pos
                          'neg)))
          m 1))
  
  (test/neg-blame
   'subclass-and-external-contracts1
   '(let* ([c%
            (contract (class/c [m (->m integer? integer?)])
                      (class object%
                        (define/public (m x) x)
                        (super-new))
                      'pos 'neg)]
           [sub-c% (class c% (super-new))])
      (send (new sub-c%) m #f)))
  
  (test/spec-passed
   'subclass-and-external-contracts2
   '(let* ([c%
            (contract (class/c [m (->m integer? integer?)])
                      (class object%
                        (define/public (m x) x)
                        (super-new))
                      'pos 'neg)]
           [sub-c% (class c% 
                     (define/override (m x) (super m x))
                     (super-new))])
      (send (new sub-c%) m #f)))
  
  (test/spec-passed
   'subclass-and-external-contracts3
   '(let* ([c% (contract (class/c (field [f integer?]))
                         (class object% (field [f #f]) (super-new))
                         'pos 'neg)]
           [sub-c% (class c% (super-new))])
      (new sub-c%)))
  
  (test/pos-blame
   'subclass-and-external-contracts4
   '(let* ([c% (contract (class/c (field [f integer?]))
                         (class object% (field [f #f]) (super-new))
                         'pos 'neg)]
           [sub-c% (class c% (super-new))])
      (get-field f (new sub-c%))))
  
  (test/spec-passed
   'subclass-and-external-contracts5
   '(let* ([c% (contract (class/c (init [f integer?]))
                         (class object% (init f) (super-new))
                         'pos 'neg)]
           [sub-c% (class c% (super-new))])
      (new sub-c% [f 1])))
  
  (test/neg-blame
   'subclass-and-external-contracts6
   '(let* ([c% (contract (class/c (init [f integer?]))
                         (class object% (init f) (super-new))
                         'pos 'neg)]
           [sub-c% (class c% (super-new))])
      (new sub-c% [f #f])))
  
  (test/spec-passed
   'subclass-and-external-contracts7
   '(let* ([c% (contract (class/c (init [i integer?]))
                         (class object% (init i) (super-new))
                         'pos 'neg)]
           [sub-c% (class c% (init i) (super-new [i 2]))])
      (new sub-c% [i #f])))
  
  (test/neg-blame
   'subclass-and-external-contracts8
   '(let* ([c%
            (contract (class/c (init [i integer?]))
                      (class object% (init i) (super-new))
                      'pos 'neg)]
            [d% (class c% (super-new [i #f]))])
      (new d%)))
  
  (test/spec-passed
   'subclass-and-external-contracts9
   '(let* ([c%
            (contract (class/c (init [i integer?] [j integer?]))
                      (class object% (init i) (init j) (super-new))
                      'pos 'neg)]
            [d% (class c% (init i) (super-new [i 1]))])
      (new d% [i #f] [j 1])))
  
  (test/neg-blame
   'subclass-and-external-contracts10
   '(let* ([c%
            (contract (class/c (init [i integer?] [j integer?]))
                      (class object% (init i) (init j) (super-new))
                      'pos 'neg)]
           [d% (class c% (init i) (super-new [i 1]))])
      (new d% [i #f] [j #f])))
  
  (test/spec-passed/result
   'object=?
   '(let ([o (new (contract (class/c (m (->m integer? integer?)))
                            (class object%
                              (define/public (m x) x)
                              (define/public (get-this) this)
                              (super-new))
                            'pos 'neg))])
      (object=? (send o get-this) o))
   #t)

  ;; this test case won't pass until the internal-ctc
  ;; call is delayed in the new class/c projections
  ;; (but otherwise it passes)
  #;
  (test/spec-passed/result
   'chaperone-of
   '(let* ([c% (class object% (define/public (m x) x))]
           [c+c% (contract (class/c (m (->m integer? integer?))) c% 'pos 'neg)])
      (chaperone-of? c+c% c%))
   #t)
  
  (let ([expected-given?
         (λ (exn) (and (regexp-match? #rx"callback: contract violation" (exn-message exn))
                       (regexp-match? #rx"expected: boolean[?]" (exn-message exn))
                       (regexp-match? #rx"given: 1" (exn-message exn))))]
        [promised-produced?
         (λ (exn) (and (regexp-match? #rx"callback: broke its contract" (exn-message exn))
                       (regexp-match? #rx"promised: boolean[?]" (exn-message exn))
                       (regexp-match? #rx"produced: 1" (exn-message exn))))])
    (contract-error-test
     'blame-important1
     '(send (new (contract (class/c [callback (->m boolean? void)])
                           (class object%
                             (super-new)
                             (define/public (callback n) (void)))
                           'pos
                           'neg))
            callback 1)
     expected-given?)
    (contract-error-test
     'blame-important2
     '((contract (-> (class/c (callback (->m boolean? any)))
                     any)
                 (λ (c%) (send (new c%) callback 1))
                 'pos 'neg)
       (class object%
         (super-new)
         (define/public (callback x) 3)))
     expected-given?)
    (contract-error-test
     'blame-important3
     '((contract (-> (class/c (callback (->m (-> boolean? void?) any)))
                     any)
                 (λ (c%) (send (new c%) callback void))
                 'pos 'neg)
       (class object%
         (super-new)
         (define/public (callback f) (f 1))))
     promised-produced?)))
