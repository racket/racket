#lang scheme/base
(require scheme/gui/base
         scheme/gui/dynamic)
;; Don't change this program to use `racket'; ths point
;; is to test `scheme/gui/base' exports

(let ([ns ((gui-dynamic-require 'make-gui-namespace))]
      [orig-ns (current-namespace)])
  (namespace-attach-module orig-ns 'scheme/base ns))
