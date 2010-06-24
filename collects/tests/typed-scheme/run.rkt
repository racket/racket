#lang racket
(require racket/vector racket/gui/dynamic)

(require "main.ss")

(define exec (make-parameter go/text))
(define unit-only? (make-parameter #f))
(define int-only? (make-parameter #f))
(define skip-all? #f)
(current-namespace (make-base-namespace))
(command-line
 #:once-each
 ["--unit" "run just the unit tests" (unit-only? #t)]
 ["--int" "run just the integration tests" (int-only? #t)]
 ["--nightly" "for the nightly builds" (when (eq? 'cgc (system-type 'gc))
                                         (set! skip-all? #t))]
 ["--gui" "run using the gui" 
          (if (gui-available?)
              (begin (exec go))
              (error "GUI not available"))])

(if skip-all?
    (printf "Skipping Typed Racket tests.\n")
    (unless (= 0 ((exec) (unit-only?) (int-only?)))
      (error "Typed Racket Tests did not pass.")))
