#lang scheme

(require "checks.ss"
         "test-class.ss"
         "test-debug.ss"
         "test-define.ss"
         "test-dict.ss"
         "test-exn.ss"
         "test-planet.ss"
         "test-port.ss"
         "test-regexp.ss"
         "test-require-provide.ss"
         "test-sandbox.ss"
         "test-scribble.ss"
         "test-set.ss"
         "test-syntax.ss")

(run-tests
 (test-suite "scheme.plt"
   class-suite
   debug-suite
   define-suite
   dict-suite
   exn-suite
   planet-suite
   port-suite
   regexp-suite
   require-provide-suite
   sandbox-suite
   scribble-suite
   set-suite
   syntax-suite))
