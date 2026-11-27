#lang reader "../lang/reader.rkt"

(provide
 nested-rendered
 nested-template
 expression-nested-rendered
 string-brace-rendered
) ; end provide

(define x 1)
(define nested-rendered f"outer {f"inner {x}"}")
(define nested-template t"outer {t"inner {x}"}")
(define expression-nested-rendered f"{(string-append f"a{x}" "b")}")
(define string-brace-rendered f"{(string-append "}" "x")}")
