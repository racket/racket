#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/class)])
(test/pos-blame
 'object/c-first-order-object-1
 '(contract (object/c)
            3
            'pos
            'neg))

(test/spec-passed
 'object/c-first-order-object-2
 '(contract (object/c)
            (new object%)
            'pos
            'neg))

(test/pos-blame
 'object/c-first-order-opaque-object-1
 '(contract (object/c #:opaque)
            3
            'pos
            'neg))

(test/spec-passed
 'object/c-first-order-opaque-object-2
 '(contract (object/c)
            (new object%)
            'pos
            'neg))

(test/pos-blame
 'object/c-first-order-method-1
 '(contract (object/c [m (-> any/c number? number?)])
            (new object%)
            'pos
            'neg))

(test/spec-passed
 'object/c-first-order-method-2
 '(contract (object/c [m (-> any/c number? number?)])
            (new (class object% (super-new) (define/public (m x) (add1 x))))
            'pos
            'neg))

(test/spec-passed
 'object/c-first-order-opaque-method-1
 '(contract (object/c #:opaque [m (-> any/c number? number?)])
            (new (class object% (super-new) (define/public (m x) (add1 x))))
            'pos
            'neg))

(test/pos-blame
 'object/c-first-order-local-method-1
 '(let ()
    (define-local-member-name m)
    (contract (object/c [m (-> any/c number? number?)])
              (new object%)
              'pos
              'neg)))

(test/spec-passed
 'object/c-first-order-local-method-2
 '(let ()
    (define-local-member-name m)
    (contract (object/c [m (-> any/c number? number?)])
              (new (class object% (super-new) (define/public (m x) (add1 x))))
              'pos
              'neg)))

(test/pos-blame
 'object/c-first-order-field-1
 '(contract (object/c (field [n number?]))
            (new object%)
            'pos
            'neg))

(test/spec-passed
 'object/c-first-order-field-2
 '(contract (object/c (field [n number?]))
            (new (class object% (super-new) (field [n 3])))
            'pos
            'neg))

(test/pos-blame
 'object/c-first-order-opaque-field-1
 '(contract (object/c #:opaque (field [n number?]))
            (new object%)
            'pos
            'neg))

(test/spec-passed
 'object/c-first-order-field-2
 '(contract (object/c #:opaque (field [n number?]))
            (new (class object% (super-new) (field [n 3])))
            'pos
            'neg))

(test/pos-blame
 'object/c-first-order-local-field-1
 '(let ()
    (define-local-member-name n)
    (contract (object/c (field [n number?]))
              (new object%)
              'pos
              'neg)))

(test/spec-passed
 'object/c-first-order-local-field-2
 '(let ()
    (define-local-member-name n)
    (contract (object/c (field [n number?]))
              (new (class object% (super-new) (field [n 3])))
              'pos
              'neg)))

(test/spec-passed/result
 'object/c-higher-order-field-1
 '(get-field
   n
   (contract (object/c (field [n number?]))
             (new (class object% (super-new) (field [n 3])))
             'pos
             'neg))
 3)

(test/pos-blame
 'object/c-higher-order-field-2
 '(get-field
   n
   (contract (object/c (field [n number?]))
             (new (class object% (super-new) (field [n #t])))
             'pos
             'neg)))

(test/spec-passed/result
 'object/c-higher-order-field-3
 '(let ([o (contract (object/c (field [n number?]))
                     (new (class object% (super-new) (field [n 3])))
                     'pos
                     'neg)])
    (set-field! n o 5)
    (get-field n o))
 5)

(test/neg-blame
 'object/c-higher-order-field-4
 '(let ([o (contract (object/c (field [n number?]))
                     (new (class object% (super-new) (field [n 3])))
                     'pos
                     'neg)])
    (set-field! n o #t)))

(test/spec-passed/result
 'object/c-higher-order-field-5
 '(let* ([pre-o (new (class object% (super-new) (field [n 3])))]
         [o (contract (object/c (field [n number?]))
                      pre-o
                      'pos
                      'neg)])
    (set-field! n pre-o 5)
    (get-field n o))
 5)

(test/spec-passed/result
 'object/c-higher-order-field-6
 '(let* ([pre-o (new (class object% (super-new) (field [n 3])))]
         [o (contract (object/c (field [n number?]))
                      pre-o
                      'pos
                      'neg)])
    (set-field! n o 5)
    (get-field n pre-o))
 5)

(test/neg-blame
 'object/c-higher-order-field-7
 '(let* ([pre-o (new (class object% (super-new) (field [n 3])))]
         [o (contract (object/c (field [n number?]))
                      pre-o
                      'pos
                      'neg)])
    (set-field! n o #t)
    (get-field n pre-o)))

(test/pos-blame
 'object/c-higher-order-field-8
 '(let* ([pre-o (new (class object% (super-new) (field [n 3])))]
         [o (contract (object/c (field [n number?]))
                      pre-o
                      'pos
                      'neg)])
    (set-field! n pre-o #t)
    (get-field n o)))

(let ([missing-method?
       (λ (exn) (regexp-match? #rx"no such method"
                               (exn-message exn)))]
      [missing-field?
       (λ (exn) (regexp-match? #rx"does not have the requested field"
                               (exn-message exn)))])
  (contract-error-test
   'opaque-hiding-1
   '(send (contract (object/c #:opaque [m (->m string?)])
                    (new (class object%
                           (super-new)
                           (define/public (m) "foo")
                           (define/public (n) "bar")))
                    'pos 'neg)
     n)
   missing-method?)
  (contract-error-test
   'opaque-hiding-2
   '(get-field f
     (contract (object/c #:opaque [m (->m string?)])
               (new (class object%
                      (super-new)
                      (define/public (m) "foo")
                      (field [f "bar"])))
               'pos 'neg))
   missing-field?)
  (test/spec-passed/result
   'opaque-hiding-3
   '(get-field f
     (contract (object/c #:opaque (field [f string?]))
               (new (class object%
                      (super-new)
                      (define/public (m) "foo")
                      (field [f "bar"])))
               'pos 'neg))
   "bar")))
