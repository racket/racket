#lang racket
(require "private/drracket-test-util.rkt")

#|

This test checks to see if the code that htdp-lang.rkt injects
into the teaching languages relies only on modules that are
shared between drracket's and the teaching languages's namespace.

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

(define things-to-try
  (list '(check-expect 1 1)
        '(check-within 1 1.01 2)
        '(check-error (car))
        '(check-error (error 'x "y") "x: y")
        '(check-member-of 1 2 3 1 4)
        '(check-range 1 0 2)))

(define first-line-output (format "All ~a tests passed!" (length things-to-try)))

(define (go)
  (fire-up-drracket-and-run-tests
   (λ ()
     (putenv "PLTDRHTDPNOCOMPILED" "yes")
     (define drs-frame (wait-for-drracket-frame))
     (set-language-level! '("Beginning Student"))
     (clear-definitions drs-frame)
     (for ([exp (in-list things-to-try)])
       (insert-in-definitions drs-frame (format "~s\n" exp)))
     (do-execute drs-frame)
     (let ([output (fetch-output drs-frame)])
       (cond
         [(equal? output first-line-output)
          (try-interaction-test drs-frame)]
         [else
          (eprintf "teaching-lang-sharing-modules.rkt: got bad output from execute: ~s"
                   output)])))))

(define (try-interaction-test drs-frame)
  (type-in-interactions drs-frame "1\n")
  (wait-for-computation drs-frame)
  (let ([interactions-output (fetch-output drs-frame)])
    (unless (equal? interactions-output (format "~a\n> 1\n1" first-line-output))
      (error 'teaching-language-sharing-modules.rkt
             "got bad output from interaction: ~s\n" 
             interactions-output))))

(go)
