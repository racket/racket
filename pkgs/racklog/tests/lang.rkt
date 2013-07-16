#lang racket
(require racket/runtime-path tests/eli-tester)
(define-runtime-path lang "lang")

(test
 (for ([p (in-list (directory-list lang))]
       #:when (regexp-match #rx"rkt$" (path->bytes p)))
   (define test-rkt (build-path lang p))
   (define test-txt (build-path lang (path-replace-suffix p #".txt")))
   (test #:failure-prefix (path->string p)
         (filter (lambda (l)
                   (not (string=? l "")))
                 (with-input-from-string
                     (with-output-to-string
                         (lambda () (dynamic-require test-rkt #f)))
                   port->lines))
         =>
         (filter (lambda (l)
                   (not (string=? l "")))
                 (file->lines test-txt)))))
