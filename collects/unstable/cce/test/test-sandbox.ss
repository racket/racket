#lang scheme

(require "checks.ss"
         "../sandbox.ss")

(provide sandbox-suite)

(define sandbox-suite
  (test-suite "sandbox.ss"))
