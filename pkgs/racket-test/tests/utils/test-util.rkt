#lang racket/base

(require racket/string)

(provide run-unreliable-tests?)

;; from racket-test-core/tests/racket/testing.rktl
(define (run-unreliable-tests? mode)
  (define s (getenv "PLT_RUN_UNRELIABLE_TESTS"))
  (and s
       (let ([l (map string->symbol (string-split s ","))])
         (memq mode l))))
