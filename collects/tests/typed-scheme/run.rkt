#lang racket
(require racket/vector racket/gui/dynamic)

(require "main.ss")

(define exec (make-parameter go/text))
(define the-tests (make-parameter #f))
(define nightly? (make-parameter #f))
(define unit? (make-parameter #f))
(define int? (make-parameter #f))
(define opt? (make-parameter #f))
(define bench? (make-parameter #f))
(current-namespace (make-base-namespace))
(command-line
 #:once-each
 ["--unit" "run the unit tests" (unit? #t)]
 ["--int" "run the integration tests" (int? #t)]
 ["--nightly" "for the nightly builds" (begin (nightly? #t) (unit? #t) (int? #t))]
 ["--just" path "run only this test" (the-tests (just-one path))]
 ["--opt" "run the optimizer tests" (opt? #t)]
 ["--benchmarks" "compile the typed benchmarks" (bench? #t)]
 ["--all" "run all tests" (begin (unit? #t) (int? #t) (opt? #t) (bench? #t))]
 ["--gui" "run using the gui" 
          (if (gui-available?)
              (begin (exec go))
              (error "GUI not available"))]
 )

(the-tests
 (cond [(and (unit?) (int?)) tests]
       [(unit?)              unit-tests]
       [(int?)               int-tests]
       [(the-tests)          (the-tests)]
       [else
        (error "You must specify which tests should be run. See --help for more info.\n")]))

(cond [(and (nightly?) (eq? 'cgc (system-type 'gc)))
       (printf "Skipping Typed Racket tests.\n")]
      [(when (the-tests)
         (unless (= 0 ((exec) (the-tests)))
           (eprintf "Typed Racket Tests did not pass.\n")))
       (when (opt?)
         (parameterize ([current-command-line-arguments #()])
           (dynamic-require '(file "optimizer/run.rkt") #f))
         (printf "Typed Racket Optimizer tests passed\n"))
       (when (bench?)
         (unless (= 0 ((exec) (compile-benchmarks)))
           (error "Typed Racket Tests did not pass.\n")))])
