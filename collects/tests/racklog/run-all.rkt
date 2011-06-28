#lang racket
(require racket/runtime-path tests/eli-tester)
(define-runtime-path here ".")

(test
 (for ([p (in-list (directory-list here))])
   (define s (path->string p))
   (when (regexp-match #rx"rkt$" s)
     (unless (or (directory-exists? s)
                 (string=? "run-all.rkt" s))
       (dynamic-require (build-path here p) #f)))))
