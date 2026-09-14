#lang reader "../lang/reader.rkt" racket/base

(provide
 rendered
 template-value
 nested-rendered
) ; end provide

(define name "Alice")
(define rendered f"hello {name}")
(define template-value t"hello {name}")
(define nested-rendered f"outer {f"inner {name}"}")
