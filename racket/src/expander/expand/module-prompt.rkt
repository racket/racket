#lang racket/base
(require "bindings-arity-error.rkt")

(provide call-with-module-prompt
         call-with-module-prompt/value-list)

;; Installs a prompt to consistency with the one used for definitions
;; and expressions around a module's top forms at run/visit time (as
;; opposed to expansion time)
(define (call-with-module-prompt thunk)
  (call-with-continuation-prompt
   thunk
   (default-continuation-prompt-tag)
   module-prompt-handler))

(define (call-with-module-prompt/value-list who thunk ids handler)
  (call-with-module-prompt
   (lambda ()
     (call-with-values
      thunk
      (lambda vals
        (unless (= (length vals) (length ids))
          (raise-bindings-arity-error who ids vals))
        (handler vals)
        (apply values vals))))))

(define module-prompt-handler
  (lambda args
    (apply
     abort-current-continuation
     (default-continuation-prompt-tag)
     args)))
