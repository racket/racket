#lang scheme/base

(require scheme/gui/base "launcher-bootstrap.ss")

(current-namespace (make-gui-empty-namespace))
(namespace-require 'scheme/gui/base)
(namespace-require 'scheme/class)

(startup)
