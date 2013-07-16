
;; For every test here, make sure the opposite test is in advanced.rkt

(htdp-syntax-test #'(define (xthnk) 10) "define: expected at least one variable after the function name, but found none")
(htdp-syntax-test #'(define xthnk (lambda () 10)) "lambda: expected at least one variable after lambda, but found none")

(htdp-err/rt-test (cons 1 2) "cons: second argument must be a list, but received 1 and 2")
(htdp-err/rt-test (append (list 1) 2) "append: last argument must be a list, but received 2")
