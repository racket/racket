#lang racket/base
(require racket/place
         racket/runtime-path)

(define-runtime-module-path-index mod-for-place "embed-me34-print.rkt")

(void
 (place-wait
  (dynamic-place
   (let ([n (resolved-module-path-name (module-path-index-resolve mod-for-place))])
     (if (symbol? n)
         `(quote ,n)
         n))
   'print)))
