
;; For every test here, make sure the opposite test is in advanced.ss

(htdp-syntax-test #'(define (xthnk) 10))
(htdp-syntax-test #'(define xthnk (lambda () 10)))
