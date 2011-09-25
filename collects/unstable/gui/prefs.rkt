#lang racket/base
;; owner: ryanc
(require framework/preferences)
(provide pref:get/set)

(define (pref:get/set sym)
  (case-lambda
    [() (preferences:get sym)]
    [(v) (preferences:set sym v)]))
