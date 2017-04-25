#lang racket/base
(require compiler/find-exe
         racket/system)

;; Sanity checks to run in an installer-building context to make sure
;; that things bascially work. Assuming that this file is run through
;; `raco test`, quite a lot is implicitly tested even if this module
;; is empty.

(let ([o (open-output-bytes)])
  (parameterize ([current-output-port o])
    (system* (find-exe) "-e" "'hello"))
  (unless (equal? #"'hello\n" (get-output-bytes o))
    (error "sanity check failed")))
