#lang typed/racket

;; Test struct forms without colons. Ideally should be
;; unit tests, but these have to be at the top-level.

(struct foo (x y))
(foo-x (foo 'a 'b))

(define-struct foo2 (x y))
(foo2-x (make-foo2 'a 'b))

(struct foo3 ([x : String] y))
(string-append (foo3-x (foo3 "a" 'b)) "bar")

(define-struct foo4 ([x : String] y))
(string-append (foo4-x (make-foo4 "a" 'b)) "bar")

(struct foo5 ([x : String] [y : Symbol]))
(symbol->string (foo5-y (foo5 "a" 'b)))

(define-struct foo6 ([x : String] [y : Symbol]))
(symbol->string (foo6-y (make-foo6 "a" 'b)))

