#lang racket/gui

(require rackunit)

(provide
  ;; syntax
  ;; (testing e ...) creates a top-level test in its own eventspace that
  ;; shuts down after the tests are run 
  testing
  (all-from-out rackunit))

;; -----------------------------------------------------------------------------

(define-syntax-rule
  (testing e ...)
  (begin ;; module+ test
    (parameterize ((current-eventspace (make-eventspace))) e ...)))
