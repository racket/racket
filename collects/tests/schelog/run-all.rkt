#lang racket
(require racket/runtime-path tests/eli-tester)
(define-runtime-path here ".")
(define this-file (build-path "run-all.rkt"))

(test
 (for ([p (in-list (directory-list here))]
       #:when (not (equal? this-file p)))
   (dynamic-require (build-path here p) #f)))