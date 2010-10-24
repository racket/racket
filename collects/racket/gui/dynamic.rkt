#lang scheme/base

(provide gui-available?
         gui-dynamic-require)

(define (gui-available?)
  (and (zero? (variable-reference->phase (#%variable-reference)))
       (with-handlers ([exn:fail? (lambda (exn) #f)])
         ;; Fails if `mred/private/dynamic' is not declared
         ;;  (without loading it if not):
         (module->language-info 'mred/private/dynamic #f)
         ;; Double check that it seems to have started ok:
         (eq? (dynamic-require 'mred/private/dynamic 'kernel-initialized)
              'done))))

(define-namespace-anchor anchor)

(define (gui-dynamic-require sym)
  (parameterize ([current-namespace (namespace-anchor->empty-namespace anchor)])
    (if (gui-available?)
        (dynamic-require 'mred sym)
        (error "racket/gui/base is not available"))))
