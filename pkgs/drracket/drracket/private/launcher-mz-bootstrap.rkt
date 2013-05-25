#lang racket/base

(require "launcher-bootstrap.rkt")

(current-namespace (make-base-empty-namespace))
(namespace-require 'racket/base)

(startup)
