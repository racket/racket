#lang racket
(require racket/vector racket/gui/dynamic)

(require "main.ss")

(define exec (make-parameter go/text))
(define the-tests (make-parameter tests))
(define skip-all? #f)
(define nightly? (make-parameter #f))
(define opt? (make-parameter #f))
(define bench? (make-parameter #f))
(current-namespace (make-base-namespace))
(command-line
 #:once-each
 ["--unit" "run just the unit tests" (the-tests unit-tests)]
 ["--int" "run just the integration tests" (the-tests int-tests)]
 ["--nightly" "for the nightly builds" (nightly? #t)]
 ["--just" path "run only this test" (the-tests (just-one path))]
 ["--opt" "run the optimizer tests" (opt? #t)]
 ["--benchmarks" "compile the typed benchmarks" (bench? #t)]
 ["--gui" "run using the gui" 
          (if (gui-available?)
              (begin (exec go))
              (error "GUI not available"))]
 )

(cond [(and (nightly?) (eq? 'cgc (system-type 'gc)))
       (printf "Skipping Typed Racket tests.\n")]
      [(unless (= 0 ((exec) (the-tests)))
         (eprintf "Typed Racket Tests did not pass."))
       (when (opt?)
         (parameterize ([current-command-line-arguments #()])
           (dynamic-require '(file "optimizer/run.rkt") #f))
         (printf "Typed Racket Optimizer tests passed\n"))
       (when (bench?)
         (unless (= 0 ((exec) (compile-benchmarks)))
           (error "Typed Racket Tests did not pass.\n")))])
