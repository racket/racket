#lang scheme

(require "checks.ss"
         "../planet.ss"
         planet/util)

(provide planet-suite)

(define planet-suite
  (test-suite "planet.ss"
    (test-suite "this-package-version-symbol")))
