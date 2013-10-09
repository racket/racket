#lang scheme/base

(require "test-utils.rkt"
         (for-syntax scheme/base)
         (for-template scheme/base)
         (private type-contract)
         (rep type-rep)
         (types abbrev numeric-tower union)
         rackunit)
(provide contract-tests)


(define-syntax-rule (t e)
  (test-case (format "~a" 'e)
    (let ((v e))
      (with-check-info (('type v))
        (type->contract
          e
          (λ (#:reason [reason #f])
            (fail-check (or reason "Type could not be converted to contract"))))))))

(define-syntax-rule (t/fail e expected-reason)
  (test-case (format "~a" 'e)
   (let ((v e))
     (with-check-info (('expected expected-reason)
                       ('type v))
       (define reason
         (let/ec exit
           (type->contract v (λ (#:reason [reason #f]) (exit reason)))
           (fail-check "type could be converted to contract")))
       (unless (regexp-match? expected-reason reason)
         (with-check-info (('reason reason))
           (fail-check "Reason didn't match expected.")))))))


(define known-bugs
  (test-suite "Known Bugs"
    ;; This should fail as case-> doesn't support keyword arguments.
    (t
      (make-Function
        (list
          (make-arr* (list) -Boolean #:kws (list (make-Keyword '#:key Univ #t)))
          (make-arr* (list Univ Univ) -Boolean #:kws (list (make-Keyword '#:key2 Univ #t))))))

    ;; Polydotted functions should work
    (t/fail (-polydots (a) (->... (list) (a a) -Symbol))
            "not supported for this type")))



(define (contract-tests)
  (test-suite "Contract Tests"
              known-bugs
              (t (-Number . -> . -Number))
              (t (-Promise -Number))
              (t (-set Univ)) 
              (t (-> Univ -Boolean : -Symbol))
              (t (->key -Symbol #:key -Boolean #t Univ))
              (t (make-Function
                   (list (make-arr* (list Univ) -Boolean #:kws (list (make-Keyword '#:key Univ #t))
                                    #:filters -Symbol))))
              (t (-struct #'struct-name #f (list (make-fld -Symbol #'acc #f))))
              ;; Adapted from PR 13815
              (t (-poly (a) (-> a a)))
              (t (-poly (a) (-mu X (-> a X))))
              (t (-poly (a) (-poly (b) (-> a a))))
              (t (-poly (a) (-App (-poly (b) (-> a a)) (list -Number) #'#f)))
              (t/fail
                (make-Function
                  (list (make-arr* (list) -Boolean #:kws (list (make-Keyword '#:key Univ #f)))
                    (make-arr* (list Univ) -Boolean #:kws (list (make-Keyword '#:key2 Univ #f)))))
                "case function type with optional keyword arguments")
              (t/fail (-poly (a) -Flonum) "non-function polymorphic type")
              (t/fail (-> (-> Univ -Boolean : -Symbol) -Symbol)
                      "function type with filters or objects")
              (t/fail (-poly (a) (-set -Number)) "non-function polymorphic type")
              (t/fail (cl->*
                        (-> -Boolean -Boolean)
                        (-> -Symbol -Symbol))
                      "two cases of arity 1")
              (t/fail (-Syntax (-HT -Symbol -Symbol))
                      "first-order contract, but got a hashtable.")
              (t/fail (-Syntax (-struct #'struct-name #f (list (make-fld -Symbol #'acc #t))))
                      "first-order contract, .* struct with at least one mutable field")
              (t/fail (-struct #'struct-name #f (list (make-fld -Symbol #'acc #f)) (-> -Symbol))
                      "procedural structs are not supported")
              (t/fail (-Syntax (-> -Boolean -Boolean))
                      #rx"required a first-order .* generate a higher-order")
              (t/fail (-set (-Param -Boolean -Boolean))
                      #rx"required a chaperone or flat contract .* generate an impersonator")
              (t/fail (-poly (a b) (-> (Un a b) (Un a b)))
                      "multiple distinct type variables")
              ))

