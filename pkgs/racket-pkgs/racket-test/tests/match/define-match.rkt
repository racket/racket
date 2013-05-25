#lang racket/base

(require rackunit racket/match)

(define/match ((curried x) y)
  [((? number? x) y) (+ x y)]
  [((? symbol? x) y) x])

(check-equal? ((curried 3) 5) 8)
(check-equal? ((curried 'foo) 5) 'foo)

(define/match (fact n)
  [(0) 1]
  [(n) (* n (fact (sub1 n)))])

(check-equal? (fact 0) 1)
(check-equal? (fact 5) 120)

(define/match (foo #:bar [bar 5] [baz 7])
  [(5 7) #t]
  [(_ _) #f])

(check-true (foo))
(check-true (foo #:bar 5 7))
(check-false (foo #:bar 7 8))

(define/match (foo2 #:qux qux #:bar [bar 5] [baz 7])
  [(1 5 7) #t]
  [(_ _ _) #f])

(check-true (foo2 #:qux 1))
(check-false (foo2 #:qux 2))
(check-true (foo2 #:qux 1 #:bar 5 7))

(define/match (f [x 3] . rst)
  [(3 '(1 2)) #t]
  [(_ _) #f])

(check-true (f 3 1 2))
(check-false (f))
(check-false (f 2))
(check-false (f 2 4 5))

(require (prefix-in legacy: mzlib/match))
(legacy:define/match (fact-2 n)
  [(0) 1]
  [(n) (* n (fact (sub1 n)))])

(check-equal? (fact-2 0) 1)
(check-equal? (fact-2 5) 120)

(legacy:define/match (list-fun lst)
  [((1 2 3)) #t]
  [((_ _ _ )) #f])

(check-equal? (list-fun '(1 2 3)) #t)
(check-equal? (list-fun '(4 5 6)) #f)
