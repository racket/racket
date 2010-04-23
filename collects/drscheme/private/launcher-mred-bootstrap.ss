#lang racket/base

(require racket/gui/base "launcher-bootstrap.ss")

(current-namespace (make-gui-empty-namespace))
(namespace-require 'racket/gui/base)
(namespace-require 'racket/class)

(startup)
