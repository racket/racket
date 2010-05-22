#lang scheme

(require "checks.ss"
         "../match.ss")

(provide match-suite)

(define match-suite
  (test-suite "match.ss"
    (test-suite "match?"
      (test
       (check-true (match? (list 1 2 3)
                     (list a b c)
                     (vector x y z))))
      (test
       (check-true (match? (vector 1 2 3)
                     (list a b c)
                     (vector x y z))))
      (test
       (check-false (match? (+ 1 2 3)
                      (list a b c)
                      (vector x y z)))))
    (test-suite "define-struct-pattern"
      (test
       (let ()
         (define-struct pair [a b] #:transparent)
         (define-struct-pattern both pair)
         (check-equal?
          (match (make-pair 1 2)
            [(both a b) (list a b)])
          (list 1 2)))))
    (test-suite "as"
      (test
       (match (list 1 2 3)
         [(as ([a 0]) (list b c d)) (list a b c d)])
       (list 0 1 2 3)))
    (test-suite "$"
      (test
       (let ()
         (define-struct pair [a b] #:transparent)
         (check-equal? ($ pair 1 2) (make-pair 1 2))
         (check-equal?
          (match ($ pair 1 2)
            [($ pair a b) (list a b)])
          (list 1 2)))))))
