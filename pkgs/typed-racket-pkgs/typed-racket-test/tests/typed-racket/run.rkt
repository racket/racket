#lang racket
(require racket/vector racket/gui/dynamic rackunit)

(require "main.rkt")

(define exec (make-parameter go/text))
(define nightly? (make-parameter #f))
(define unit? (make-parameter #f))
(define int? (make-parameter #f))
(define opt? (make-parameter #f))
(define missed-opt? (make-parameter #f))
(define bench? (make-parameter #f))
(define single (make-parameter #f))
(current-namespace (make-base-namespace))
(command-line
 #:once-each
 ["-v" "verbose" (verbose? #t)]
 ["--unit" "run the unit tests" (unit? #t)]
 ["--int" "run the integration tests" (int? #t)]
 ["--opt" "run the optimization tests" (opt? #t)]
 ["--missed-opt" "run the missed optimization tests" (missed-opt? #t)]
 ["--benchmarks" "compile the typed benchmarks" (bench? #t)]
 ["--just" path "run only this test" (single (just-one path))]
 ["--nightly" "for the nightly builds" (begin (nightly? #t) (unit? #t) (opt? #t) (missed-opt? #t) (places 1))]
 ["--all" "run all tests" (begin (unit? #t) (int? #t) (opt? #t) (missed-opt? #t) (bench? #t))]
 ["-j" num "number of places to use" 
       (let ([n (string->number num)])
         (places (and (integer? n) (> n 1) n)))]
 ["--gui" "run using the gui"
          (if (gui-available?)
              (exec go)
              (error "GUI not available"))])

(start-workers)

(if (and (nightly?) (eq? 'cgc (system-type 'gc)))
    (printf "Skipping Typed Racket tests.\n")
    (let ([to-run (cond [(single) (single)]
                        [else
                         (make-test-suite
                          "Typed Racket Tests"
                          (append (if (unit?)       (list unit-tests)                    '())
                                  (if (int?)        (list (int-tests))                     '())
                                  (if (opt?)        (list (optimization-tests))            '())
                                  (if (missed-opt?) (list (missed-optimization-tests))     '())
                                  (if (bench?)      (list (compile-benchmarks))          '())))])])
      (unless (= 0 ((exec) to-run))
        (eprintf "Typed Racket Tests did not pass.\n")
        (exit 1))))

;; Test mode:
(module test racket/base
  (require syntax/location)
  (parameterize ([current-command-line-arguments (vector "--nightly")])
    (dynamic-require (quote-module-path "..") 0))
  (module config info
    (define timeout 1800)))
