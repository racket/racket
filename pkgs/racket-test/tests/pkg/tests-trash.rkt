#lang racket/base
(require "util.rkt"
         "shelly.rkt")

(this-test-is-run-by-the-main-test)

(pkg-tests
 (with-fake-root
     (shelly-begin

      (define (check-empty)
        (shelly-begin
         $ "raco pkg empty-trash -l" =stdout> #rx"  \\[(none|does not exist)\\]\n"))
      
      (define (add-remove)
        (shelly-begin
         $ "raco pkg install test-pkgs/pkg-test1.zip"
         $ "raco pkg remove pkg-test1"))
      
      (define (check-single)
        (shelly-begin
         $ "raco pkg empty-trash -l" =stdout> #rx"Content:\n  [0-9]+-0-pkg-test1\n$"))

      (define (check-double)
        (shelly-begin
         $ "raco pkg empty-trash -l" =stdout> #rx"Content:\n  [0-9]+-0-pkg-test1\n  [0-9]*-.-pkg-test1\n$"))
      
      $ "raco pkg install test-pkgs/pkg-test1.zip"
      $ "raco pkg remove --no-trash pkg-test1"
      (check-empty)
      
      
      (add-remove)
      (check-single)
      
      (add-remove)
      (check-double)

      $ "raco pkg empty-trash"
      (check-empty)
      
      $ "raco pkg config --set trash-max-packages 1"
      (add-remove)
      (check-single)
      (add-remove)
      (check-single)
      $ "raco pkg config --set trash-max-packages 10"
      (add-remove)
      (check-double)
      
      $ "raco pkg config --set trash-max-seconds 0"
      (add-remove)
      (check-single))))
