#lang racket/base
(provide with-both-output-to-string)
(define (with-both-output-to-string thunk)
  (define sp (open-output-string))
  (parameterize ([current-output-port sp]
                 [current-error-port sp])
    (thunk))
  (get-output-string sp))