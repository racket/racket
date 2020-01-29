#lang racket/base
(require '#%linklet)

(provide (protect-out vm-primitive
                      vm-eval))

(define (vm-primitive sym)
  (unless (symbol? sym)
    (raise-syntax-error 'vm-primitive "symbol?" sym))
  (primitive-lookup sym))

(define (vm-eval s)
  (case (system-type 'vm)
    [(chez-scheme)
     (define primitive-eval (vm-primitive 'eval))
     (define call-with-system-wind (vm-primitive 'call-with-system-wind))
     (call-with-system-wind
      (lambda ()
        (primitive-eval s)))]
    [else
     (instantiate-linklet (compile-linklet `(linklet () () ,s))
                          null
                          (make-instance 'eval))]))
