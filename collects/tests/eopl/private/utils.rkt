#lang racket
(provide (all-from-out rackunit)
         check-error
         define-syntax-rule
         define-syntax
         syntax-rules)

;;------------------------------------------------------------------------
;; Testing utilities
(require rackunit)

(define-syntax-rule (check-error e msg)
  (check-exn (lambda (x) (and (exn:fail? x)
                              (string=? msg (exn-message x))))
             (lambda () e)))

