
;; For every test here, make sure the opposite test is in intml-adv.ss

(htdp-syntax-test #'(1 2 3))
(htdp-syntax-test #'("hello" 1 2))


(htdp-syntax-test #'(define x17 (lambda (y) (lambda (z) z))))
(htdp-syntax-test #'(lambda (x) 10))

(htdp-syntax-test #'(lambda (f) (f f)))
