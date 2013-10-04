#lang racket

;; ---------------------------------------------------------------------------------------------------
;; the stop-when clause crashes. make sure that it signals a catchable error.

(require 2htdp/universe 2htdp/image)

(with-handlers ((exn:fail? void))
  (big-bang 0
            (on-draw (λ _ (empty-scene 500 500)))
            (stop-when (λ _ (car '()))))
  (displayln '(*** something went wrong in stop-when crash ***)))
