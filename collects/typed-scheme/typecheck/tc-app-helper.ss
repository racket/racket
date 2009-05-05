#lang scheme/base

(require "../utils/utils.ss"
         (utils tc-utils))

(provide (all-defined-out))

(define (stringify-domain dom rst drst [rng #f])
  (let ([doms-string (if (null? dom) "" (string-append (stringify dom) " "))]
        [rng-string (if rng (format " -> ~a" rng) "")])
    (cond [drst
           (format "~a~a ... ~a~a" doms-string (car drst) (cdr drst) rng-string)]
          [rst
           (format "~a~a *~a" doms-string rst rng-string)]
          [else (string-append (stringify dom) rng-string)])))

(define (domain-mismatches ty doms rests drests rngs arg-tys tail-ty tail-bound #:expected [expected #f])
  (define arguments-str
    (stringify-domain arg-tys (if (not tail-bound) tail-ty #f) (if tail-bound (cons tail-ty tail-bound) #f)))    
  (cond
    [(null? doms)
     (int-err "How could doms be null: ~a ~a" ty)]
    [(= 1 (length doms))
     (format "Domain: ~a~nArguments: ~a~n~a"
             (stringify-domain (car doms) (car rests) (car drests))
             arguments-str
             (if expected
                 (format "Result type: ~a~nExpected result: ~a~n"
                         (car rngs) expected)
                 ""))]
    [else
     (format "~a: ~a~nArguments: ~a~n~a"
             (if expected "Types" "Domains")
             (stringify (if expected 
                            (map stringify-domain doms rests drests rngs)
                            (map stringify-domain doms rests drests))
                        "~n\t")
             arguments-str
             (if expected
                 (format "Expected result: ~a~n" expected)
                 ""))]))
