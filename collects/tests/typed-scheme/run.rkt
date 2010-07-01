#lang racket
(require racket/vector racket/gui/dynamic)

(require "main.ss")

(define exec (make-parameter go/text))
(define the-tests (make-parameter tests))
(define skip-all? #f)
(current-namespace (make-base-namespace))
(command-line
 #:once-each
 ["--unit" "run just the unit tests" (the-tests unit-tests)]
 ["--int" "run just the integration tests" (the-tests int-tests)]
 ["--nightly" "for the nightly builds" (when (eq? 'cgc (system-type 'gc))
                                         (set! skip-all? #t))]
 ["--just" path "run only this test" (the-tests (just-one path))]
 ["--gui" "run using the gui" 
          (if (gui-available?)
              (begin (exec go))
              (error "GUI not available"))]
 )

(if skip-all?
    (printf "Skipping Typed Racket tests.\n")
    (unless (= 0 ((exec) (the-tests)))
      (error "Typed Racket Tests did not pass.")))
