#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace (make-basic-contract-namespace)])
  
  (test/pos-blame
   'immutable1
   '(let ([ct (contract (listof (boolean? . -> . boolean?))
                        #f
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/neg-blame
   'immutable2
   '(let ([ct (contract (listof (boolean? . -> . boolean?))
                        (list (lambda (x) x))
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/neg-blame
   'immutable3
   '(let ([ct (contract (listof (number? . -> . boolean?))
                        (list (lambda (x) 1))
                        'pos
                        'neg)])
      ((car ct) #f)))
  
  (test/pos-blame
   'immutable4
   '(let ([ct (contract (listof (number? . -> . boolean?))
                        (list (lambda (x) 1))
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/spec-passed
   'immutable5
   '(let ([ct (contract (listof (number? . -> . boolean?))
                        (list (lambda (x) #t))
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  
  (test/pos-blame
   'immutable6
   '(contract (cons/c (boolean? . -> . boolean?) (boolean? . -> . boolean?))
              #f
              'pos
              'neg))
  
  (test/spec-passed
   'immutable7
   '(let ([ct (contract (non-empty-listof (boolean? . -> . boolean?))
                        (list (λ (x) #t))
                        'pos
                        'neg)])
      ((car ct) #f)))
  
  (test/neg-blame
   'immutable8
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?))
                        (cons (lambda (x) 1) (lambda (x) 1))
                        'pos
                        'neg)])
      ((car ct) #f)))
  
  (test/neg-blame
   'immutable9
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?))
                        (cons (lambda (x) 1) (lambda (x) 1))
                        'pos
                        'neg)])
      ((cdr ct) #f)))
  
  (test/pos-blame
   'immutable10
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?))
                        (cons (lambda (x) 1) (lambda (x) 1))
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/pos-blame
   'immutable11
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?))
                        (cons (lambda (x) 1) (lambda (x) 1))
                        'pos
                        'neg)])
      ((cdr ct) 1)))
  
  (test/spec-passed
   'immutable12
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?))
                        (cons (lambda (x) #t) (lambda (x) #t))
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/spec-passed
   'immutable13
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?))
                        (cons (lambda (x) #t) (lambda (x) #t))
                        'pos
                        'neg)])
      ((cdr ct) 1)))
  
  (test/spec-passed/result
   'immutable14
   '(contract (cons/c number? boolean?)
              (cons 1 #t)
              'pos
              'neg)
   (cons 1 #t))
  
  (test/pos-blame
   'immutable15
   '(contract (list/c (number? . -> . boolean?) (number? . -> . boolean?))
              #f
              'pos
              'neg))
  
  (test/pos-blame
   'immutable17
   '(contract (list/c (number? . -> . boolean?) (number? . -> . boolean?))
              (list (lambda (x) #t))
              'pos
              'neg))
  
  (test/pos-blame
   'immutable18
   '(contract (list/c (number? . -> . boolean?) (number? . -> . boolean?))
              (list (lambda (x) #t) (lambda (x) #t) (lambda (x) #t))
              'pos
              'neg))
  
  (test/spec-passed
   'immutable19
   '(let ([ctc (contract (list/c (number? . -> . boolean?) (number? . -> . boolean?))
                         (list (lambda (x) #t) (lambda (x) #t))
                         'pos
                         'neg)])
      (for-each (lambda (x) (x 1)) ctc)))
  
  (test/pos-blame
   'vector-immutable1
   '(contract (vector-immutableof (boolean? . -> . boolean?))
              #f
              'pos
              'neg))
  
  (test/pos-blame
   'vector-immutable2
   '(contract (vector-immutableof (boolean? . -> . boolean?))
              (vector (lambda (x) x))
              'pos
              'neg))
  
  (test/neg-blame
   'vector-immutable3
   '(let ([ct (contract (vector-immutableof (number? . -> . boolean?))
                        (vector->immutable-vector (vector (lambda (x) 1)))
                        'pos
                        'neg)])
      ((vector-ref ct 0) #f)))
  
  (test/pos-blame
   'vector-immutable4
   '(let ([ct (contract (vector-immutableof (number? . -> . boolean?))
                        (vector->immutable-vector (vector (lambda (x) 1)))
                        'pos
                        'neg)])
      ((vector-ref ct 0) 1)))
  
  (test/spec-passed
   'vector-immutable5
   '(let ([ct (contract (vector-immutableof (number? . -> . boolean?))
                        (vector->immutable-vector (vector (lambda (x) #t)))
                        'pos
                        'neg)])
      ((vector-ref ct 0) 1)))
  
  (test/pos-blame
   'vector-immutable6
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?))
              #f
              'pos
              'neg))
  
  (test/pos-blame
   'vector-immutable7
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?))
              (vector (lambda (x) #t) (lambda (x) #t))
              'pos
              'neg))
  
  (test/pos-blame
   'vector-immutable8
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?))
              (vector->immutable-vector (vector (lambda (x) #t)))
              'pos
              'neg))
  
  (test/pos-blame
   'vector-immutable9
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?))
              (vector->immutable-vector (vector (lambda (x) #t) (lambda (x) #t) (lambda (x) #t)))
              'pos
              'neg))
  
  (test/spec-passed
   'vector-immutable10
   '(let ([ctc (contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?))
                         (vector->immutable-vector (vector (lambda (x) #t) (lambda (x) #t)))
                         'pos
                         'neg)])
      ((vector-ref ctc 0) 1)
      ((vector-ref ctc 1) 1)))
  
  (test/spec-passed/result
   'vector-immutable11
   '(contract (vector-immutable/c number? boolean?)
              (vector->immutable-vector (vector 1 #t))
              'pos
              'neg)
   (vector->immutable-vector (vector 1 #t)))
  
  (test/spec-passed/result
   'vector-immutable12
   '(immutable? (contract (vector-immutable/c number? boolean?)
                          (vector->immutable-vector (vector 1 #t))
                          'pos
                          'neg))
   #t)
  
  (test/pos-blame
   'box-immutable1
   '(contract (box-immutable/c (number? . -> . boolean?))
              #f
              'pos
              'neg))
  
  (test/pos-blame
   'box-immutable2
   '(contract (box-immutable/c (number? . -> . boolean?))
              (box (lambda (x) #t))
              'pos
              'neg))
  
  (test/neg-blame
   'box-immutable3
   '(let ([ctc (contract (box-immutable/c (number? . -> . boolean?))
                         (box-immutable (lambda (x) #t))
                         'pos
                         'neg)])
      ((unbox ctc) #f)))
  
  (test/pos-blame
   'box-immutable4
   '(let ([ctc (contract (box-immutable/c (number? . -> . boolean?))
                         (box-immutable (lambda (x) 1))
                         'pos
                         'neg)])
      ((unbox ctc) 1)))
  
  (test/spec-passed
   'box-immutable5
   '(let ([ctc (contract (box-immutable/c (number? . -> . boolean?))
                         (box-immutable (lambda (x) #t))
                         'pos
                         'neg)])
      ((unbox ctc) 1)))
  
  (test/spec-passed/result
   'box-immutable6
   '(contract (box-immutable/c boolean?)
              (box-immutable #t)
              'pos
              'neg)
   (box-immutable #t))
  
  (test/spec-passed/result
   'box-immutable7
   '(immutable? (contract (box-immutable/c boolean?)
                          (box-immutable #t)
                          'pos
                          'neg))
   #t))