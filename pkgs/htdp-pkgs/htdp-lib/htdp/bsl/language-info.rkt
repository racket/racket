#lang scheme/base
(provide get-info)

(define ((get-info options) key default)
  (case key
    [(configure-runtime) `(#(htdp/bsl/runtime configure ,options))]
    [else default]))

