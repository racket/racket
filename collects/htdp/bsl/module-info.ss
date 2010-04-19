#lang scheme/base
(provide module-info)

(define ((module-info options) key default)
  (case key
    [(configure-runtime) `(#(htdp/bsl/runtime configure ,options))]
    [else default]))

