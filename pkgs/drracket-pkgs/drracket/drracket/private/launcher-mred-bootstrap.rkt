#lang racket/base

(module test racket/base)

(require racket/gui/base "launcher-bootstrap.rkt")

(current-namespace (make-gui-empty-namespace))
(namespace-require 'racket/gui/base)
(namespace-require 'racket/class)

(startup)
