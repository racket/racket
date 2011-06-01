
(syntax-test #'(lambda (z z) 10))

(define f7 (lambda (y) (lambda (z) z)))
(test #t procedure? f7)
(test 778 (lambda (x) 778) 'ignored)

(test values (lambda (f) (f f)) values)

(define (f11 y) ((lambda (x) x) y))
(test 'id f11 'id)

(err/rt-test (1 2 3))

(htdp-syntax-test #'(recur empty-f () 10) "recur: expected a function name after recur, but nothing's there"
(htdp-syntax-test #'(local [(lambda (x) x)] 1) "local: expected a definition, but found a part")

(htdp-syntax-test #'((unquote-splicing (list 10))) "unquote-splicing: misuse of ,@ or unquote-splicing, not under a quasiquoting backquote")



