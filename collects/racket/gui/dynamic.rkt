#lang racket/base
(require ffi/unsafe)

(provide gui-available?
         gui-dynamic-require)

(define scheme_register_process_global
  (get-ffi-obj 'scheme_register_process_global #f (_fun _string _pointer -> _pointer)))

(define (gui-available?)
  (and 
   ;; Never available in non-0 phases:
   (zero? (variable-reference->phase (#%variable-reference)))
   ;; Must be instantiated:
   (scheme_register_process_global "GRacket-support-initialized" #f)
   (with-handlers ([exn:fail? (lambda (exn) #f)])
     ;; Fails if `mred/private/dynamic' is not declared
     ;;  (without loading it if not):
     (module->language-info 'mred/private/dynamic #f)
     ;; Fails if `mred/private/dynamic' is not instantiated:
     (namespace-attach-module (current-namespace) 'mred/private/dynamic)
     ;; Double check that it seems to have started ok:
     (eq? (dynamic-require 'mred/private/dynamic 'kernel-initialized)
          'done))))

(define (gui-dynamic-require sym)
  (parameterize ([current-namespace (variable-reference->empty-namespace
                                     (#%variable-reference))])
    (if (gui-available?)
        (dynamic-require 'racket/gui/base sym)
        (error "racket/gui/base is not available"))))
