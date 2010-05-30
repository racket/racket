#lang scheme

(require "checks.ss"
         "test-debug.ss"
         "test-planet.ss"
         "test-require-provide.ss"
         "test-scribble.ss"
         "test-set.ss")

(run-tests
 (test-suite "scheme.plt"
   debug-suite
   planet-suite
   require-provide-suite
   scribble-suite
   set-suite))
