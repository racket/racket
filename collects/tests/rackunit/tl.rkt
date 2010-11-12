#lang racket/base
(require rackunit)

;; test to make sure that the various check functions
;; return what they are promised to at the top-level

;; make drdr notice when a check prints something.
(current-output-port (current-error-port))

(check-equal? (check + 1 2) (void))

(check-equal? (check-eq? 1 1) (void))
(check-equal? (check-not-eq? #f #t) (void))
(check-equal? (check-eqv? (expt 2 100) (expt 2 100)) (void))
(check-equal? (check-not-eqv? (expt 2 100) 1) (void))
(check-equal? (check-equal? (list 1 2) (list 1 2)) (void))
(check-equal? (check-not-equal? (list 1 2) (list 2 1)) (void))

(check-equal? (check-pred not #f) (void))
(check-equal? (check-= 1.1 1.2 0.5) (void))
(check-equal? (check-true #t) (void))
(check-equal? (check-false #f) (void))
(check-equal? (check-not-false 3) (void))

(check-equal? (check-exn #rx"car" (λ () (car 1))) (void))
(check-equal? (check-not-exn (λ () 1)) (void))

(check-equal? (check-regexp-match #rx"a*b" "aaaaaaab") (void))
