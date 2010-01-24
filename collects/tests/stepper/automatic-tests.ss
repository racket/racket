#lang scheme

(require "through-tests.ss" 
         "test-engine.ss")

(parameterize ([display-only-errors #t]
               [current-output-port (open-output-string)]
               [current-namespace (make-base-namespace)])
  (if (run-all-tests-except '(bad-and bad-cons check-error begin-let-bug prims qq-splice time set! local-set! lazy1 lazy2 lazy3))
      (exit 0)
      (exit 1)))
