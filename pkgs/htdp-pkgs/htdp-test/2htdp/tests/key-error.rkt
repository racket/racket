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


(define (my-fun x) "hi")

(with-handlers ((exn:fail? 
                 (lambda (x) 
                   (define msg (exn-message x))
                   (define hdl (regexp-match "check-with's handler test" msg))
                   (unless hdl
                     (error 'test "expected: \"check-with's handler test, error says: ~e" msg)))))
  (big-bang 0 
            [to-draw (lambda (x) (circle 1 'solid 'red))]
            [on-tick (lambda (x) (my-fun x))]
            [check-with (lambda (x) (number? x))])
  (raise `(bad "must fail")))
