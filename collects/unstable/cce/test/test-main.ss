#lang scheme

(require "checks.ss"
         "test-debug.ss"
         "test-set.ss")

(run-tests
 (test-suite "scheme.plt"
   debug-suite
   set-suite))
