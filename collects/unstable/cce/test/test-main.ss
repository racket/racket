#lang scheme

(require "checks.ss"
         "test-class.ss"
         "test-contract.ss"
         "test-debug.ss"
         "test-define.ss"
         "test-dict.ss"
         "test-exn.ss"
         "test-hash.ss"
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
   contract-suite
   debug-suite
   define-suite
   dict-suite
   exn-suite
   hash-suite
   planet-suite
   port-suite
   regexp-suite
   require-provide-suite
   sandbox-suite
   scribble-suite
   set-suite
   syntax-suite))
