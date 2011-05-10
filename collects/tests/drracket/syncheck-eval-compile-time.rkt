#lang racket/base

(require "drracket-test-util.rkt"
         racket/class
         framework)

(define (main)
  (fire-up-drscheme-and-run-tests
   (λ ()
     (let ([drs (wait-for-drscheme-frame)])
       (set-module-language!)
       (do-execute drs)
       (queue-callback/res (λ () (handler:edit-file (collection-file-path "map.rkt" "racket" "private"))))
       
       (preferences:set 'framework:coloring-active #f)
       (click-check-syntax-and-check-errors drs "syncheck-eval-compile-time.rkt")))))


;; copied from syncheck-test.rkt ....
(define (click-check-syntax-and-check-errors drs test)
  (click-check-syntax-button drs)
  (wait-for-computation drs)
  (when (queue-callback/res (λ () (send (send drs get-definitions-text) in-edit-sequence?)))
    (error 'syncheck-test.rkt "still in edit sequence for ~s" test))
  
  (let ([err (queue-callback/res (λ () (send drs syncheck:get-error-report-contents)))]) 
    (when err
      (fprintf (current-error-port)
               "FAILED ~s\n   error report window is visible:\n   ~a\n"
               test
               err))))
(define (click-check-syntax-button drs)
  (test:run-one (lambda () (send (send drs syncheck:get-button) command))))

(main)