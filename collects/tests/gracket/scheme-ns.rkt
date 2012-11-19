#lang scheme/base
(require scheme/gui/base
         scheme/gui/dynamic)
;; Don't change this program to use `racket'; ths point
;; is to test `scheme/gui/base' exports

(let ([ns ((gui-dynamic-require 'make-gui-namespace))]
      [orig-ns (current-namespace)])
  (when (namespace-variable-value 'hash #t (lambda () #f) ns)
    (error "did not expect a binding for `hash'"))
  (namespace-attach-module orig-ns 'scheme/base ns))
