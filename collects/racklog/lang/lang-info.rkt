#lang racket/base

(define (get-info data)
  (λ (key default)
    (case key
      [(configure-runtime)
       '(#(racklog/lang/configure-runtime configure #f))]
      [else
       default])))
(provide get-info)
