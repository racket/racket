#lang racket/base

(provide exn->string)

(define (exn->string exn)
  (if (exn? exn)
      (parameterize ([current-error-port (open-output-string)])
        ((error-display-handler) (exn-message exn) exn)
        (get-output-string (current-error-port)))
      (format "~s\n" exn)))
