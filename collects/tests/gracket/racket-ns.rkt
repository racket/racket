#lang racket/base
(require racket/gui/base
         racket/gui/dynamic)
;; Don't change this program to use `racket'; ths point
;; is to test `racket/gui/base' exports

(let ([ns ((gui-dynamic-require 'make-gui-namespace))]
      [orig-ns (current-namespace)])
  (namespace-attach-module orig-ns 'racket/base ns))
