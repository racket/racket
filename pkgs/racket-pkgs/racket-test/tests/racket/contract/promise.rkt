#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/promise
                                               'racket/contract)])
  
  (test/pos-blame
   'promise/c1
   '(force (contract (promise/c boolean?)
                     (delay 1)
                     'pos
                     'neg)))
  
  (test/spec-passed
   'promise/c2
   '(force (contract (promise/c boolean?)
                     (delay #t)
                     'pos
                     'neg)))
  
  (test/spec-passed/result
   'promise/c3
   '(let ([x 0])
      (contract (promise/c any/c)
                (delay (set! x (+ x 1)))
                'pos
                'neg)
      x)
   0)
  
  (test/spec-passed/result
   'promise/c4
   '(let ([x 0])
      (force (contract (promise/c any/c)
                       (delay (set! x (+ x 1)))
                       'pos
                       'neg))
      x)
   1)
  
  (test/spec-passed/result
   'promise/c5
   '(let ([x 0])
      (let ([p (contract (promise/c any/c)
                         (delay (set! x (+ x 1)))
                         'pos
                         'neg)])
        (force p)
        (force p))
      x)
   1)
  
  (test/spec-passed/result
   'promise/c5
   '(let ([a (delay 7)])
      (equal? a
              (contract (promise/c integer?)
                        a
                        'pos
                        'neg)))
   #t)
  
  (test/spec-passed/result
   'promise/c6
   '(let ([a (delay 7)])
      (equal? a
              (contract (promise/c (new-∃/c 'α))
                        a
                        'pos
                        'neg)))
   #t))
