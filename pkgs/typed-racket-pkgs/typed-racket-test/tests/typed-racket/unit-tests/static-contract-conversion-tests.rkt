#lang racket/base

(require "test-utils.rkt" "evaluator.rkt"
         rackunit
         (only-in racket/contract contract?)
         syntax/srcloc syntax/location
         (for-syntax
           syntax/parse
           racket/base
           (private type-contract)
           (static-contracts instantiate)
           (types abbrev numeric-tower union)))

(provide tests)
(gen-test-main)

(define-syntax t/sc
  (syntax-parser
    [(_ e:expr)
     (syntax/loc #'e
       (test-case
         (format "Conversion:~a" (quote-line-number e))
         (with-check-info (['type 'e]
                           ['location (build-source-location-list (quote-srcloc e))])
           (phase1-phase0-eval
             (define sc
                (type->static-contract e (lambda (#:reason _) #f)))
             (if sc
                 #`(with-check-info (['static '#,sc])
                     (phase1-phase0-eval
                       (define ctc (instantiate '#,sc
                                     (lambda (#:reason _) (error "static-contract could not be converted to a contract"))))
                       #,#'#`(with-check-info (['contract '#,ctc])
                          (define runtime-contract #,ctc)
                          (check-pred contract? runtime-contract))))
                 #'(fail-check "Type could not be converted to a static contract"))))))]))

(define-syntax t/fail
  (syntax-parser
    [(_ e:expr (~optional (~seq #:typed-side typed-side) #:defaults ([typed-side #'#t])))
     #`(test-case (format "~a" 'e)
         (define sc
           (phase1-phase0-eval
             (let/ec exit
               #`'#,(type->static-contract e (lambda (#:reason _) (exit #'#f)) #:typed-side typed-side))))
         (when sc
           (with-check-info (['static sc])
             (fail-check "Type was incorrectly converted to contract"))))]))


(define tests
  (test-suite "Conversion Tests"
    (t/sc (-Number . -> . -Number))
    (t/sc (cl->* (-> -Symbol)
                 (-Symbol . -> . -Symbol)))
    (t/sc (cl->* (-> -Symbol)
                 (-Symbol -Symbol . -> . -Symbol)))
    (t/sc (cl->* (-Symbol . -> . -Symbol)
                 (-Symbol -Symbol . -> . -Symbol)))
    (t/sc (-Promise -Number))
    (t/sc (-lst -Symbol))
    (t/sc -Boolean)
    (t/sc Univ)
    (t/sc (-set Univ))
    (t/sc (-poly (a) (-lst a)))
    (t/fail ((-poly (a) (-vec a)) . -> . -Symbol))
    (t/fail (-poly (a) (-lst a)) #:typed-side #f)
    (t/sc (-mu a (-lst a)))
    (t/sc (-mu a (-box a)))
    (t/sc (-mu sexp (Un -Null -Symbol (-pair sexp sexp) (-vec sexp) (-box sexp))))
    (t/sc (-mu a (-> a a)))
    (t/sc (-seq -Symbol))))
