#lang racket

;; ---------------------------------------------------------------------------------------------------
;; the stop-when clause crashes. make sure that it signals a catchable error.

(require 2htdp/universe 2htdp/image)

(with-handlers ((exn:fail? (λ (x) 
                             (unless (pair? (regexp-match #px"return a boolean" (exn-message x)))
                               (raise x)))))
  (big-bang 0
            (on-draw (λ _ (empty-scene 500 500)))
            (stop-when (λ _ 5)))
  (displayln '(*** SOMETHING WENT WRONG IN STOP-WHEN NOT BOOLEAN ***)))
