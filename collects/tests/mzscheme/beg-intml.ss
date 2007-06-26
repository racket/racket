
;; For every test here, make sure the opposite test is in advanced.ss

(htdp-syntax-test #'(define (xthnk) 10))
(htdp-syntax-test #'(define xthnk (lambda () 10)))

;; CCE: This tests the error handling for ...
;; It should be duplicated for .. through ......
;; but (for-each (lambda foo bar) baz) won't work here.
(htdp-error-test #'(set! ... true))
