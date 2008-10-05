#lang scheme/base
(require scheme/unit
         scheme/runtime-path
         (prefix-in x: lang/htdp-intermediate-lambda)
         (prefix-in x: htdp/world))


(provide game@)
(define orig-namespace (current-namespace))
(define-runtime-path chat-noir "chat-noir.ss")

(define-unit game@
  (import)
  (export)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (namespace-attach-module orig-namespace
                             '(lib "htdp-intermediate-lambda.ss" "lang"))
    (namespace-attach-module orig-namespace
                             '(lib "world.ss" "htdp"))
    (dynamic-require chat-noir #f)))