#lang scheme/base

(require (for-syntax scheme/base))

(provide enter!)

(define-syntax (enter! stx)
  (syntax-case stx ()
    [(enter! mod)
     (if (or (not (syntax-e #'mod))
             (module-path? (syntax->datum #'mod)))
         #'(do-enter! 'mod)
         (raise-syntax-error
          #f
          "not a valid module path, and not #f"
          stx
          #'mod))]
    [_ (raise-syntax-error
        #f
        "bad syntax; should be `(enter! <module-path-or-#f>)'"
        stx)]))

(define orig-namespace (current-namespace))

(define (do-enter! mod)
  (if mod
      (begin
        (dynamic-require mod #f)
        (let ([ns (module->namespace mod)])
          (current-namespace ns)
          (namespace-require 'scheme/enter)))
      (current-namespace orig-namespace)))
