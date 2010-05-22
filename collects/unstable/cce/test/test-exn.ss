#lang scheme

(require "checks.ss"
         "../exn.ss")

(provide exn-suite)

(define exn-suite
  (test-suite "exn.ss"
    (test-suite "try"
      (test-ok (try (+ 1 2)))
      (test-bad (try (+ 'a 'b)))
      (test-ok (try (+ 'a 'b) (+ 3 4)))
      (test-ok (try (+ 1 2) (+ 'a 'b)))
      (test-bad (try (+ 'a 'b) (+ 'c 'd))))))
