#lang racket/base

(require "launcher-bootstrap.ss")

(current-namespace (make-base-empty-namespace))
(namespace-require 'racket/base)

(startup)
