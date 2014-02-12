#lang typed/racket

;; Test struct forms without colons. Ideally should be
;; unit tests, but these have to be at the top-level.

(struct foo ([x : String] [y : Symbol]))
(string-append (foo-x (foo "a" 'b)) "b")
(symbol->string (foo-y (foo "a" 'b)))

(define-struct foo2 ([x : String] [y : Symbol]))
(string-append (foo2-x (make-foo2 "a" 'b)) "b")
(symbol->string (foo2-y (make-foo2 "a" 'b)))

