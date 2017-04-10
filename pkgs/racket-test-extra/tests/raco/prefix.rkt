#lang racket/base
(require compiler/find-exe
         racket/system)

;; Make sure that `raco` recognizes unambiguous command prefixes.
;; This test assumes that "pk" will unambiguously refer to "pkg".

(define o (open-output-bytes))
(parameterize ([current-output-port o])
  (system* (find-exe)
           "-N" "raco"
           "-l-" "raco"
           "pk"))
(unless (regexp-match? #rx"raco pkg install" (get-output-bytes o))
  (error "expected output from `raco pkg`"))
