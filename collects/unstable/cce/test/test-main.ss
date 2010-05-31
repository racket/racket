#lang scheme

(require "checks.ss"
         "test-debug.ss")

(run-tests
 (test-suite "scheme.plt"
   debug-suite))
