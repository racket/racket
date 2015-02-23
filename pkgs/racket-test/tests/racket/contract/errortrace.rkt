#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract)])
  ;; have to do this here so that errortrace is loaded
  ;; in the right context (cannot put 'errortrace into 
  ;; the argument to make-basic-contract-namespace)
  (parameterize ([current-namespace (current-contract-namespace)])
    (dynamic-require 'errortrace #f))
  (define sp (open-input-string (format "~s\n" '(-> (λ (a b c) #f) any))))
  (define stx (read-syntax 'whereitsat sp))
  (define exn
    (with-handlers ((exn:fail? values))
      (contract-eval stx)))
  (define sp2 (open-output-string))
  (parameterize ([current-error-port sp2])
    ((error-display-handler) (exn-message exn) exn))
  (define matches?
    (regexp-match? #rx"whereitsat" (get-output-string sp2)))
  (unless matches?
    (display (get-output-string sp2)))
  (test #t 
        'checking-arrow-src-locs
        matches?))
