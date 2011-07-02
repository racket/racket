#lang racket

(require rackunit rackunit/text-ui unstable/debug "helpers.rkt")

(run-tests
 (test-suite "debug.rkt"
   (test-suite "dprintf"
     (test
      (let ()
        (parameterize ([current-error-port (open-output-string)])
          (dprintf "Danger, ~a!" "Will Robinson")
          (check-equal?
           (get-output-string (current-error-port))
           "Danger, Will Robinson!\n")))))))
