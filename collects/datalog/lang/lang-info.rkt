#lang racket/base

(define (get-info data)
  (λ (key default)
    (case key
      [(configure-runtime)
       '(#(datalog/lang/configure-runtime configure #f))]
      [else
       default])))
(provide get-info)
