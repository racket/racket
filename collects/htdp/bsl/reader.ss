#lang scheme/base
(provide wrap-reader
         make-info
         make-module-info)

(define (wrap-reader read-proc options)
  (lambda args
    (parameterize ([read-decimal-as-inexact #f]
                   [read-accept-dot #f]
                   [read-accept-quasiquote (memq 'read-accept-quasiquote options)])
      (apply read-proc args))))

(define ((make-info options) key default use-default)
  (case key
    [else (use-default key default)]))

(define (make-module-info options)
  `#(htdp/bsl/module-info module-info ,options))
