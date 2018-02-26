#lang racket/base
(require "module.rkt"
         "../host/linklet.rkt"
         "../common/module-path.rkt"
         "../syntax/module-binding.rkt"
         "../namespace/provided.rkt"
         "link.rkt"
         "variable.rkt")

(provide get-module-export-variables)

(define (get-module-export-variables lnk
                                     #:compiled-modules compiled-modules
                                     #:cache cache)
  (define name (link-name lnk))
  (define phase (link-phase lnk))
  (define root-name (if (pair? name) (car name) name)) ; strip away submodule path
  (define comp-mod
    (get-compiled-module name root-name
                         #:compiled-modules compiled-modules
                         #:cache cache))
    
  (define provs (instance-variable-value (compiled-module-declaration comp-mod) 'provides))
  
  (for/hash ([(sym binding/p) (in-hash (hash-ref provs 0 #hasheq()))])
    (define binding (provided-as-binding binding/p))
    (values sym (variable (link (module-path-index->module-name (module-binding-module binding) name)
                                (module-binding-phase binding))
                          (module-binding-sym binding)))))
