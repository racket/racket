#lang scheme

(require "checks.ss"
         "test-debug.ss"
         "test-scribble.ss"
         "test-set.ss")

(run-tests
 (test-suite "scheme.plt"
   debug-suite
   scribble-suite
   set-suite))
