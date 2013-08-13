#lang typed/racket

(require racket/runtime-path)

(define-runtime-path foo "bar")
(define-runtime-path foo2 '(lib "racket"))
(define-runtime-paths (bar baz) (values "bar" "baz"))
(define-runtime-module-path-index x "quux")
(define-runtime-module-path z "with-type.rkt")

