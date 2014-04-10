#lang racket

(require math/statistics)

#|

From the command line, for example:
racket p-calc.rkt stlc

Finds a p-value such that the average size of generated terms 
is within 0.5 of the maximum counterexample size for that model.
This target size will likely need to change.
(Also makes sure that the standard error in the mean is less than 0.5.)

Doesn't work well with rbtrees, which has an extremely large standard deviation.

|#

(module+
    main
  (command-line
   #:args (dir)
   (printf "p-value: ~s\n" 
           (exact->inexact 
            (model-p-value 
             (string->symbol 
              (string-append
               "redex/examples/benchmark/"
               dir)))))))

(define (model-p-value mod-path)
  (define target-size (max-cexp-size mod-path))
  (define base (test-mod-path->base mod-path))
  (define enum-gen (dynamic-require base 'generate-enum-term))
  (find-p-value enum-gen target-size))

(define (find-p-value enum-gen target-size)
  (fpv-search enum-gen target-size 0 1 100))

(define (fpv-search gen size l r num-samples)
  (define mid (/ (+ l r) 2))
  (printf "Target: ~s, current interval: (~s ~s)\n" size l r)
  (define-values (m e) (sample-sizes gen mid num-samples))
  (printf "At ~s, mean: ~s, stderr: ~s, with ~s samples\n" mid m e num-samples)
  (cond
    [(e . > . 0.5)
     (fpv-search gen size l r (* num-samples 2))]
    [((abs (- size m)) . < . 0.5)
     mid]
    [(m . < . size)
     (fpv-search gen size l mid num-samples)]
    [(m . > . size)
     (fpv-search gen size mid r num-samples)]))  

(define (sample-sizes enum-gen p-value num-samples)
  (define sizes
    (for/list ([_ (in-range num-samples)])
      (count-size
       (enum-gen p-value))))
  (values (mean sizes)
          (/ (stddev sizes) 
             (sqrt num-samples))))

(module+
    test
  (require rackunit)
  (define stlc-path 'redex/examples/benchmark/stlc)
  (define stlc-base 'redex/examples/benchmark/stlc/stlc-base)
  (define stlc-gen (dynamic-require stlc-base 'generate-enum-term))
  (define-values (m e) (sample-sizes stlc-gen 0.03125 100)))

(define max-bug-num 20)
  
(define (test-paths t-m-p)
  (for/list ([n (in-range 1 max-bug-num)])
    (string->symbol
     (string-append
      (test-mod-path->stem t-m-p)
      (number->string n)))))

(define (test-mod-path->base t-m-p)
  (string->symbol
   (string-append
    (test-mod-path->stem t-m-p)
    "base")))

(define (test-mod-path->stem t-m-p)
  (define tms (symbol->string t-m-p))
  (define name (second (regexp-match #rx"^.*/(.*)$" tms)))
  (string-append tms "/" name "-"))

(define (cexps t-m-p)
  (filter 
   values
   (map
    (λ (p)
      (with-handlers
          ([exn:fail:filesystem?
            (λ (e)
              (if (regexp-match? #rx"No such file"
                                 (exn-message e))
                  #f
                  (raise e)))])
        (dynamic-require p 'small-counter-example)))
    (test-paths t-m-p))))

(define (count-size l)
  (cond
    [(list? l) (apply + 1 (map count-size l))]
    [else 1]))

(define (cexp-sizes t-m-p)
  (map count-size (cexps t-m-p)))

(define (max-cexp-size t-m-p)
  (apply max (cexp-sizes t-m-p)))

(module+
    test
  (check-equal? (test-mod-path->stem stlc-path)
                "redex/examples/benchmark/stlc/stlc-")
  (define stlc-size (max-cexp-size stlc-path))
  (check-equal? stlc-size 15))