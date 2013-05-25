#lang racket
(require "notify.rkt")

(define-syntax-rule (retry-until-success msg expr ...)
  (retry-until-success* msg (lambda () expr ...)))

(define (retry-until-success* msg thunk)
  (notify! msg)
  (thunk)
  #;(with-handlers ([exn:fail? (lambda (x)
                          ((error-display-handler) (format "Error trying to: ~a: ~a" msg (exn-message x)) x)
                          (notify! "Retrying...")
                          (retry-until-success* msg thunk))])
    (thunk)))

(provide retry-until-success)
