#lang racket

(require "randomized-tests.rkt"
         "reduce.rkt"
         "grammar.rkt"
         rackunit
         (except-in redex/reduction-semantics plug))

(define-syntax (test-transformation stx)
  (syntax-case stx ()
    [(_ program expected-output expected-result)
     #`(match-let ([(answer actual-output actual-result)
                    (model-eval (transform-intermediate (term program)))])
         (begin
           #,(syntax/loc #'expected-output
               (check-equal? actual-output expected-output))
           #,(syntax/loc #'expected-result
               (check-equal? actual-result 'expected-result))))]))

(test-transformation
 (<> ()
     ()
     (% 0
        (wcm ()
             ((λ (k)
                (begin (k 7) (print 1)))
              (cont 0 hole)))
        (λ (x) x)))
 "" 7)

(test-transformation
 (<> ()
        ()
        (cont 1 (begin hole (print 3))))
 "" procedure)

(test-transformation
 (<> ()
     ()
     (% 0
        (print
         (wcm ()
              ((λ (k) (begin (k 1) 2))
               (comp (print hole)))))
        (λ (x) x)))
 "12" #f)

(test-transformation
 (<> ()
     (1)
     (% 1
        (dw 
         x_1
         (print 1)
         (wcm ()
              ((λ (k) (k 3))
               (cont 1 (dw x_1 (print 1) hole (print 2)))))
         (print 2))
        (λ (x) x)))
 "12" 3)

(test-transformation
 (<> ()
     (1)
     (% 0
        ((% 0
            (dw
             x_1
             (print 1)
             (wcm ()
                  ((λ (k) k)
                   (cont 0 (dw x_1  (print 1) hole (print 2)))))
             (print 2))
            (λ (x) x))
         3)
        (λ (x) x)))
 "1212" 3)

(test-transformation
 (<> () []
        (% 0
           (wcm ([1 2] [3 4])
                ((λ (x) x)
                 (wcm ([1 5] [3 6])
                      (cons (current-marks 1 0)
                            (cons (current-marks 3 0)
                                  (list))))))
           (λ (x) x)))
 "" ((5 2) (6 4)))

(test-transformation
 (<>
  ()
  ()
  (dw
   ra
   (print 1)
   (print 2)
   (print 3)))
 "23" #f)

(test-transformation
 (<> ()
     ()
     (%
      1
      (dw x_1
          (print 1)
          (abort 1 (cont 1 (dw x_1 (print 1) hole (print 3))))
          (print 3))
      (λ (k) (% 1 (k 4) (λ (x) x)))))
 "313" 4)

(test-transformation
 (<>
  ()
  ()
  ((comp
    (dw
     ra
     (print 1)
     hole
     (dw q (print 2) (print 3) (print 4))))
   5))
 "134" 5)

(test-transformation
 (<>
  ()
  ()
  (cont 0
        (dw x
            #f
            (cons (cont 1 hole) hole)
            (print 2))))
 "" procedure)

(define (transformation-preserves-meaning? p)
  (let ([original-result (parameterize ([model-eval-steps 1000]) (model-eval p))]
        [transformed (transform-intermediate p)]
        [warn (λ () (eprintf "Long test:\n") (pretty-write p (current-error-port)))]
        [threshold (* 60 2)])
    (or (timeout? original-result)
        (let ([transformed-result 
               (timeout-warn threshold (model-eval transformed) (warn))])
          (if (answer? original-result)
              (equal? original-result transformed-result)
              (not (answer? transformed-result))))
        ; filters bad tests
        (bad-test? (timeout-warn threshold (impl-eval (impl-program transformed)) (warn))))))

(define-syntax-rule (test-transformation/randomized . kw-args)
  (let ([test-number 1])
    (redex-check grammar p (transformation-preserves-meaning? (term p))
               #:prepare fix-prog 
               #:source :-> . kw-args)))
