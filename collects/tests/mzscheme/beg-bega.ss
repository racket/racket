
(htdp-syntax-test #'(local [(define x 5)] x))
(htdp-syntax-test #'(recur name ([x 18]) x))

(htdp-syntax-test #'(define (f78 a) (a))) ; no functions as arguments

;; See htdp-top uses in beg-adv.ss:
(htdp-error-test #'f)
(htdp-error-test #'(x 1))
(htdp-error-test #'(+ f 1))
(htdp-error-test #'((f 1)))
(htdp-error-test #'a1)
(htdp-error-test #'make-a1)
(htdp-error-test #'a1?)
(htdp-error-test #'a1-b)

(htdp-top (define (g w) (h w))) ;; h isn't defined, yet
(htdp-error-test #'1)
(htdp-top (define (h j) (add1 j)))
(htdp-test 3 'h (h 2))

(htdp-top (define (p j) (x j)))
(htdp-error-test #'1)
(htdp-top-pop 1)


