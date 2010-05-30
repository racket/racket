#lang scheme

(require "checks.ss"
         "test-debug.ss"
         "test-define.ss"
         "test-dict.ss"
         "test-planet.ss"
         "test-require-provide.ss"
         "test-scribble.ss"
         "test-set.ss"
         "test-syntax.ss")

(run-tests
 (test-suite "scheme.plt"
   debug-suite
   define-suite
   dict-suite
   planet-suite
   require-provide-suite
   scribble-suite
   set-suite
   syntax-suite))
