#lang racket

;; ---------------------------------------------------------------------------------------------------
;; the error message should refer to the 'on-tick handler, not the lambda in the clause 

(require 2htdp/universe)
(require 2htdp/image)

(define (main)
  (big-bang 0
            (on-tick (lambda (w) "3"))
            (to-draw (lambda (w) (circle 10 'solid 'red)))
            (check-with number?)))

(with-handlers ((exn:fail? (lambda (x) 
                             (define msg (exn-message x))
                             (define hdl (regexp-match "check-with: (.*) returned" msg))
                             (unless (and hdl (cons? (regexp-match "on-tick" (second hdl))))
                               (error 'test "expected: \"on-tick\", actual: ~e" (second hdl))))))
  (main))
