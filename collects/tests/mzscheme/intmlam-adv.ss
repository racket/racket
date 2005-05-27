
(syntax-test #'(lambda (z z) 10))

(define f7 (lambda (y) (lambda (z) z)))
(test #t procedure? f7)
(test 778 (lambda (x) 778) 'ignored)

(test values (lambda (f) (f f)) values)

(define (f11 y) ((lambda (x) x) y))
(test 'id f11 'id)

(err/rt-test (1 2 3))
(err/rt-test (+) exn:application:arity?)


