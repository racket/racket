#lang racket
(require "drracket-test-util.rkt")

#|

This test checks to see if the code that htdp-lang.rkt injects
into the teaching languages relies only on modules that are
shared between drracket's and the teaching langauges's namespace.

In practice when this test fails it means that some code in
langs/htdp-lang.rkt has something like #'(f) for some function
f that is intended to be eval'd in the teaching languages and 
it should instead have #'(#%plain-app f). In that case, this
test fails because the #'(f) is reall #'(#%app f) which uses
the application form from racket/base and that #%app macro
is not there in the teaching languages (since the modules
implementing it are not being shared, you get a different instance
of that macro, tho; one that triggers an error).

Of course, other (similar) things can go wrong, too.

|#

(fire-up-drscheme-and-run-tests
 (λ ()
   (putenv "PLTDRHTDPNOCOMPILED" "yes")
   (define drs-frame (wait-for-drscheme-frame))
   (set-language-level! '("How to Design Programs" "Beginning Student"))
   (clear-definitions drs-frame)
   (type-in-definitions drs-frame "(check-expect 1 1)")
   (do-execute drs-frame)
   (let ([output (fetch-output drs-frame)])
     (cond
       [(equal? output "The test passed!")
        (try-interaction-tests)]
       [else
        (fprintf (current-error-port)
                 "teaching-lang-sharing-modules.rkt: got bad output from execute: ~s"
                 output)]))))

(define (try-interaction-test drs-frame)
  (type-in-interactions drs-frame "1\n")
  (wait-for-computation drs-frame)
  (let ([interactions-output (fetch-output drs-frame)])
    (unless (equal? interactions-output "The test passed!\n> 1\n3")
      (error 'teaching-language-sharing-modules.rkt
             "got bad output from interaction: ~s\n" 
             interactions-output))))
