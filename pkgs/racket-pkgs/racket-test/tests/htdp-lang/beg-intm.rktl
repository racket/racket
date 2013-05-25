
;; For every test here, make sure the opposite test is in intml-adv.rkt

(htdp-syntax-test #'(1 2 3) "function call: expected a function after the open parenthesis, but found a number")
(htdp-syntax-test #'("hello" 1 2) "function call: expected a function after the open parenthesis, but found a string")

(htdp-syntax-test #'(define x17 (lambda (y) (lambda (z) z))) "lambda: found a lambda that is not a function definition")
(htdp-syntax-test #'(lambda (x) 10) "lambda: found a lambda that is not a function definition")

(htdp-syntax-test #'(lambda (f) (f f)) "lambda: found a lambda that is not a function definition")

(htdp-syntax-test #'(recur empty-f () 10) "recur: this function is not defined")

(htdp-syntax-test #'((unquote-splicing (list 10))) "function call: expected a function after the open parenthesis, but found a part")
