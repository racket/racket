#lang racket/base

(require "test-utils.rkt"
         rackunit
         mzlib/pconvert
         (env init-envs)
         (types abbrev union))

(provide tests)
(gen-test-main)

(define (convert v)
  (parameterize ((current-print-convert-hook converter)
                 ;; ignore sharing in all cases
                 (current-build-share-hook (λ (v basic sub) 'atomic))
                 (show-sharing #f)
                 (booleans-as-true/false #f))
   (syntax->datum (datum->syntax #f (print-convert v)))))


(define tests
  (test-suite "Init Env"
    (test-suite "Convert"
      (check-equal?
        (convert (-> -String -Symbol))
        '(simple-> (list -String) -Symbol))
      (check-equal?
        (convert (make-pred-ty -String))
        '(make-pred-ty (list Univ) -Boolean -String (make-Path `() (list 0 0))))
      (check-equal?
        (convert (->acc (list (-lst -String)) -String (list -car)))
        '(->acc (list (-lst -String)) -String `(,-car)))
      (check-equal?
        (convert (-mu x (-lst* Univ (-box x))))
        '(make-Mu 'x (make-Pair Univ (make-Pair (make-Box (make-F 'x)) -Null))))
    )
  ))
