#lang racket/base
(require racket/gui/base
         racket/gui/dynamic)
;; Don't change this program to use `racket'; ths point
;; is to test `racket/gui/base' exports

(let ([ns ((gui-dynamic-require 'make-gui-namespace))]
      [orig-ns (current-namespace)])
  (unless (namespace-variable-value 'hash #t (lambda () #f) ns)
    (error "expected a binding for `hash'"))
  (namespace-attach-module orig-ns 'racket/base ns))
