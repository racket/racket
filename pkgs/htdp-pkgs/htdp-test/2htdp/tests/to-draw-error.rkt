#lang racket

(require 2htdp/universe rackunit)

(check-equal? (with-handlers ((exn:fail:contract? exn-message))
                (big-bang #f (to-draw (λ (_) '...) "not-a-number" "either")))
              "to-draw: expects a natural number as width argument, given \"not-a-number\"")


(check-equal? (with-handlers ((exn:fail:contract? exn-message))
                (big-bang #f (to-draw (λ (_) '...) 100 "either")))
              "to-draw: expects a natural number as height argument, given \"either\"")
