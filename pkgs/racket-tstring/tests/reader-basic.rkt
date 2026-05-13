#lang reader "../lang/reader.rkt"

(provide
 rendered
 template-value
 ordinary-string
) ; end provide

(define name "Alice")
(define rendered f"hello {name}")
(define template-value t"hello {name}")
(define ordinary-string "f\"not a template\"")
