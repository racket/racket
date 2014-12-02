#lang racket/base
(require racket/match)

;; check using `racket/match', particularly with a pattern
;; that eneds run-time support that may go through a
;; compile-time `lazy-require':

(match "x"
  [(pregexp "x")
   (with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
     (lambda () (printf "This is 21.\n"))
     #:exists 'append)])
