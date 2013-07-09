#lang racket/base
(require "test-util.rkt")
;; errortrace test
(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 
                 'racket/contract 
                 'errortrace)])
  (define sp (open-input-string (format "~s\n" '(-> (λ (a b c) #f) any))))
  (define stx (read-syntax 'whereitsat sp))
  (define exn
    (with-handlers ((exn:fail? values))
      (contract-eval stx)))
  (define sp2 (open-output-string))
  (parameterize ([current-error-port sp2])
    ((error-display-handler) (exn-message exn) exn))
  (test #t 
        'checking-arrow-src-locs
        (regexp-match? #rx"whereitsat" (get-output-string sp2))))
