#lang scheme/base

(require tests/typed-racket/unit-tests/test-utils
         (for-syntax scheme/base)
         (for-template scheme/base)
         (types abbrev numeric-tower union)
         rackunit
         "types.rkt" "instantiate.rkt")

(define-syntax-rule (t e)
  (test-not-exn (format "~a" e) (lambda () (type->contract e (lambda _ (error "type could not be converted to contract"))))))

(define-namespace-anchor anchor)
(define ns (namespace-anchor->empty-namespace anchor))

(define-syntax-rule (t/sc e)
  (test-case (format "~a" e)
    (define sc
       (type->static-contract e (lambda _ (error "type could not be converted to static-contract"))))
    (with-check-info (['static-contract sc])
      (define ctc (instantiate sc (lambda _ (error "static-contract could not be converted to a contract"))))
      (with-check-info (['contract (syntax->datum ctc)])
        (eval-syntax ctc ns)))))

(define-syntax-rule (t/fail e)
  (test-not-exn (format "~a" e) (lambda ()
                                  (let/ec exit
                                    (type->static-contract e (lambda _ (exit #t)))
                                    (error "type could be converted to contract")))))

(define-syntax-rule (t/fail-import e)
  (test-not-exn (format "~a" e) (lambda ()
                                  (let/ec exit
                                    (type->static-contract e (lambda _ (exit #t)) #:typed-side #f)
                                    (error "type could be converted to contract")))))


(define contract-tests
  (test-suite "Contract Tests"
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
              (t/fail-import (-poly (a) (-lst a)))
              (t/sc (-mu a (-lst a)))
              (t/sc (-mu a (-box a)))
              (t/sc (-mu sexp (Un (-val '()) -Symbol (-pair sexp sexp) (-vec sexp) (-box sexp))))
              (t/sc (-mu a (-> a a)))
              (t/sc (-seq -Symbol))
              ))


(require (submod "optimize.rkt" test))
(define all-tests
  (test-suite "All Tests"
    contract-tests
    optimizer-tests))

(require rackunit/text-ui)
(void (run-tests all-tests))

