#lang racket/base
(require (except-in mred
                    make-gui-namespace
                    make-gui-empty-namespace))

(provide (all-from-out mred)
         make-gui-namespace
         make-gui-empty-namespace)

(define-namespace-anchor anchor)

(define (make-gui-empty-namespace)
  (let ([ns (make-base-empty-namespace)])
    (namespace-attach-module (namespace-anchor->empty-namespace anchor)
                             'racket/gui/base
                             ns)
    ns))

(define (make-gui-namespace)
  (let ([ns (make-gui-empty-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require 'racket/base)
      (namespace-require 'racket/gui/base)
      (namespace-require 'racket/class))
    ns))
