#lang scheme/base
;; owner: ryanc
(require (for-syntax scheme/base syntax/parse)
         framework/framework)
(provide pref:get/set)

(define (pref:get/set sym)
  (case-lambda
    [() (preferences:get sym)]
    [(v) (preferences:set sym v)]))
