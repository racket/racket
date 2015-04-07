#lang racket/base
(require "test-util.rkt")
(parameterize ([current-contract-namespace
                (make-basic-contract-namespace
                 'racket/sequence 'racket/dict
                 'racket/stream)])

  (test/spec-passed
   'sequence/c1
   '(for ([x (contract (sequence/c integer?) '(1 2 3 4) 'pos 'neg)])
      (void)))
  
  (test/pos-blame 
   'sequence/c2
   '(for ([x (contract (sequence/c integer?) '(1 2 #f 4) 'pos 'neg)])
      (void)))

  (test/pos-blame
   'sequence/c3
   '(for ([x (contract (sequence/c integer? symbol?) (list 1 2 3 4) 'pos 'neg)])
      (void)))

  (test/spec-passed
   'sequence/c4
   '(for ([(x y) (contract (sequence/c integer? symbol?)
                           (in-dict (list (cons 1 'one) (cons 2 'two)))
                          'pos 'neg)])
      (void)))
  
  (test/pos-blame
   'sequence/c5
   '(for ([(x y) (contract (sequence/c integer? symbol?)
                           (in-dict (list (cons 1 'one) (cons 2 "two")))
                           'pos 'neg)])
      (void)))
  
  (test/pos-blame
   'sequence/c6
   '(for ([(x y) (contract (sequence/c integer?)
                           (in-dict (list (cons 1 'one) (cons 2 'two)))
                           'pos 'neg)])
      (void)))
  
  (test/spec-passed/result
   'sequence/c7
   '(let ([s (sequence->stream (contract (sequence/c #:min-count 2 any/c) "x" 'pos 'neg))])
      (stream-first s))
   #\x)
  
  (test/pos-blame
   'sequence/c8
   '(let ([s (sequence->stream (contract (sequence/c #:min-count 2 any/c) "x" 'pos 'neg))])
      (stream-first (stream-rest s))))

  (test/spec-passed/result
   'sequence/c9
   '(contract (sequence/c integer?) (list 1 2 3 4) 'pos 'neg)
   '(1 2 3 4))
  
  (test/spec-passed/result
   'sequence-stream/c1
   '(stream? (contract (sequence/c integer?) (stream #f #f #f) 'pos 'neg))
   #t)

  (test/pos-blame
   'sequence-stream/c2
   '(stream-first (contract (sequence/c integer?) (stream #f #f #f) 'pos 'neg)))

  (test/spec-passed/result
   'sequence-stream/c3
   '(stream-first (contract (sequence/c integer?) '(1 2 3) 'pos 'neg))
   1)
  
  (test/spec-passed/result
   'sequence-stream/c4
   '(stream-first (contract (sequence/c integer?) (in-range 10) 'pos 'neg))
   0))
