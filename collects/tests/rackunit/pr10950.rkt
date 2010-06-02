#lang racket/base
(require rackunit
         rackunit/text-ui
         racket/port
         tests/eli-tester)

(test
 (with-output-to-string
     (lambda ()
       (parameterize ([current-error-port (current-output-port)])
         (define-check (check3)
           (fail-check))
         
         (run-tests (test-suite "tests" (let ((foo check3)) (foo)))))))
 =>
 "--------------------\ntests > #f\nUnnamed test \nFAILURE\nname:       check3\nlocation:   unknown:?:?\nparams:     \n--------------------\n0 success(es) 1 failure(s) 0 error(s) 1 test(s) run\n")