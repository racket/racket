#lang scheme/base

(require "launcher-bootstrap.ss")

(current-namespace (make-base-empty-namespace))
(namespace-require 'scheme/base)

(startup)
