#lang racket/load

(module defs typed/racket
  (provide (all-defined-out))

  (: foo (case-> ([#:extra Integer] -> Integer)
                 (Integer [#:extra Integer] -> Integer)))
  (define (foo [x 0] #:extra [y 0]) (+ x y))

  ;; this is not contractable, yet (keywords not the same)
  (: bar (case-> (Integer [#:extra Integer] -> Integer)
                 (Integer [#:extra String]  -> Integer)))
  (define (bar x #:extra [y "a"]) (+ x (if (integer? y) y (string-length y))))

  (: baz (case-> (#:extra Integer -> Integer)
                 (Integer #:extra Integer -> Integer)))
  (define (baz [x 0] #:extra y) (+ x y))

  (: qux (case-> (#:extra Integer [#:super-extra Integer] -> Integer)
                 (Integer #:extra Integer [#:super-extra Integer] -> Integer)))
  (define (qux [x 0] #:extra y #:super-extra [z 0]) (+ x y z)))

(require 'defs)
(foo)
(foo 1)
(foo #:extra 1)
(foo 1 #:extra 1)

; (bar 3) ; not contractable

(baz #:extra 1)
(baz 1 #:extra 1)

(qux #:extra 1)
(qux 1 #:extra 1)
(qux #:extra 1 #:super-extra 2)
(qux 1 #:extra 1 #:super-extra 3)
