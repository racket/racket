#lang racket

(require rackunit)

;; the setup here is just to make it play nice with Rackunit
(check-not-exn
 (λ ()
  (parameterize ([current-error-port (open-output-nowhere)])
    (eval
     (quote
      (begin
        ;; This is the actual test case
        (module typed typed/racket
          (require typed/rackunit)

          ;; Any wrappings should be okay here
          (check-equal? (delay 0) (delay 0)))
        (require 'typed)))
     (make-base-namespace)))))
