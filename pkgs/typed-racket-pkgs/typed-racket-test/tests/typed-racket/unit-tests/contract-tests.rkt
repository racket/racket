#lang racket/base

(require "test-utils.rkt"
         (for-syntax racket/base
                     syntax/parse)
         (for-template racket/base)
         (private type-contract)
         (rep type-rep)
         (types abbrev numeric-tower union)
         (static-contracts combinators optimize)
         (submod typed-racket/private/type-contract numeric-contracts)
         (only-in racket/contract contract)
         rackunit)
(provide tests)
(gen-test-main)


(define-syntax-rule (t e)
  (test-case (format "~a" 'e)
    (let ((v e))
      (with-check-info (('type v))
        (type->contract
          e
          (λ (#:reason [reason #f])
            (fail-check (or reason "Type could not be converted to contract"))))))))

(define-syntax-rule (t-sc e-t e-sc)
  (test-case (format "~a" '(e-t -> e-sc))
    (let ([t e-t] [sc e-sc])
      (with-check-info (['type t] ['expected sc])
        (define actual
          (optimize
            (type->static-contract
              t
              (λ (#:reason [reason #f])
                (fail-check (or reason "Type could not be converted to contract"))))))
        (with-check-info (['actual actual])
          (unless (equal? actual sc)
            (fail-check "Static contract didn't match expected")))))))


(define-syntax-rule (t/fail e expected-reason)
  (test-case (format "~a" 'e)
   (let ((v e))
     (with-check-info (('expected expected-reason)
                       ('type v))
       (define reason
         (let/ec exit
           (let ([contract (type->contract v (λ (#:reason [reason #f])
                                                (exit (or reason "No reason given"))))])
             (with-check-info (('contract (syntax->datum contract)))
               (fail-check "type could be converted to contract")))))
       (unless (regexp-match? expected-reason reason)
         (with-check-info (('reason reason))
           (fail-check "Reason didn't match expected.")))))))

;; construct a namespace for use in typed-untyped interaction tests
(define (ctc-namespace)
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'racket/contract)
    (namespace-require 'unstable/contract)
    (namespace-require 'typed-racket/utils/any-wrap)
    (namespace-require '(submod typed-racket/private/type-contract predicates))
    (current-namespace)))

;; (t-int type (-> any any) any)
;; Use #:typed (default) to simulate typed export,
;; #:untyped for untyped export.
(define-syntax-rule (t-int arg ...)
  (t-int/check arg ... check-not-exn))

(define (check-re re)
  (λ (thunk)
    (check-exn
     (λ (e)
       (and (exn:fail? e)
            (regexp-match? re (exn-message e))))
     thunk)))

;; (t-int/fail type (-> any any) any #:msg regexp)
;; Like t-int, but checks failing cases. Takes a regexp for checking
;; the exception message.
(define-syntax-rule (t-int/fail arg ... #:msg re)
  (t-int/check arg ... (check-re re)))

;; tests typed-untyped interaction
(define-syntax (t-int/check stx)
  (syntax-parse stx
    [(_ type-expr fun-expr val-expr
        (~or (~and (~or (~seq) (~seq #:typed))
                   (~bind [typed-side #'#t]))
             (~and (~seq #:untyped)
                   (~bind [typed-side #'#f])))
        check)
     (define pos (if (syntax-e #'typed-side) 'typed 'untyped))
     (define neg (if (syntax-e #'typed-side) 'untyped 'typed))
     #`(test-case (format "~a for ~a in ~a" 'type-expr 'val-expr 'fun-expr)
         (let ([type-val type-expr] [fun-val fun-expr] [val val-expr])
           (with-check-info (['type type-val] ['test-value val])
             (define ctc-stx
               (type->contract type-val
                               #:typed-side typed-side
                               (λ (#:reason [reason #f])
                                 (fail-check (or reason "Type could not be converted to contract")))))
             (define ctced-val
               (eval #`(contract #,(syntax-shift-phase-level ctc-stx 1)
                                 #,val
                                 #,(quote (quote #,pos))
                                 #,(quote (quote #,neg)))
                     (ctc-namespace)))
             (check (λ () (fun-val ctced-val))))))]))

(define tests
  (test-suite "Contract Tests"
              (t (-Number . -> . -Number))
              (t (-Promise -Number))
              (t (-set Univ)) 
              (t (make-pred-ty -Symbol))
              (t (->key -Symbol #:key -Boolean #t Univ))
              (t (make-Function
                   (list (make-arr* (list Univ) -Boolean #:kws (list (make-Keyword '#:key Univ #t))
                                    #:filters (-FS (-filter -Symbol 0) (-not-filter -Symbol 0))))))
              (t (-struct #'struct-name #f (list (make-fld -Symbol #'acc #f))))
              ;; Adapted from PR 13815
              (t (-poly (a) (-> a a)))
              (t (-poly (a) (-mu X (-> a X))))
              (t (-poly (a) (-poly (b) (-> a a))))
              (t (-poly (a) (-App (-poly (b) (-> a a)) (list -Number) #'#f)))

              (t (-poly (a) -Flonum))
              (t (-poly (a) (-set -Number)))
              (t (-poly (a) (-lst a)))
              (t (-poly (a) (-vec a)))
              (t (-> (-poly (A B) (-> (Un A (-mu X (Un A (-lst X)))) (Un A (-mu X (Un A (-lst X))))))
                     (-> -Symbol (-mu X (Un -Symbol (-lst X))))))

              (t (-polydots (a) -Symbol))
              (t (-polydots (a) (->... (list) (a a) -Symbol)))

              (t (-mu x (-Syntax x)))
              (t (-> (-> Univ -Bottom : -bot-filter) -Bottom : -bot-filter))
              (t (-poly (A B) (-> A B (Un A B))))


              (t/fail ((-poly (a) (-vec a)) . -> . -Symbol)
                      "cannot generate contract for non-function polymorphic type")
              (t/fail (-> (-poly (a b) (-> (Un a b) (Un a b))) Univ)
                      "multiple parametric contracts are not supported")
              (t/fail
                (-> (-poly (A B) (-> (Un B (-mu X (Un A (-lst X)))) (Un B (-mu X (Un A (-lst X))))))
                    (-> -Symbol (-mu X (Un -Symbol (-lst X)))))
                "multiple parametric contracts are not supported")
              (t/fail (-> (-polydots (a) (->... (list) (a a) -Symbol)) Univ)
                      "cannot generate contract for variable arity polymorphic type")

              (t/fail
                (make-Function
                  (list (make-arr* (list) -Boolean #:kws (list (make-Keyword '#:key Univ #f)))
                    (make-arr* (list Univ) -Boolean #:kws (list (make-Keyword '#:key2 Univ #f)))))
                "case function type with optional keyword arguments")
              (t/fail (-> (make-pred-ty -Symbol)-Symbol)
                      "function type with filters or objects")
              (t/fail (cl->*
                        (-> -Boolean -Boolean)
                        (-> -Symbol -Symbol))
                      "two cases of arity 1")
              (t/fail (-struct #'struct-name #f (list (make-fld -Symbol #'acc #f)) (-> -Symbol))
                      "procedural structs are not supported")
              (t/fail (-Syntax (-> -Boolean -Boolean))
                      "required a flat contract but generated a chaperone contract")
              (t/fail (-Syntax (-seq -Boolean))
                      "required a flat contract but generated an impersonator contract")
              (t/fail (-set (-seq -Boolean))
                      "required a chaperone contract but generated an impersonator contract")

              (t/fail
                (make-Function
                  (list
                    (make-arr* (list) -Boolean #:kws (list (make-Keyword '#:key Univ #t)))
                    (make-arr* (list Univ Univ) -Boolean #:kws (list (make-Keyword '#:key2 Univ #t)))))
                "case function type with optional keyword arguments")
              (t/fail (-vec (-struct #'struct-name #f (list (make-fld (-seq -Symbol) #'acc #f)) #f #t))
                      "required a chaperone contract but generated an impersonator contract")

              (t-sc -Number number/sc)
              (t-sc -Integer integer/sc)
              (t-sc (-lst Univ) (listof/sc any-wrap/sc))
              (t-sc (Un (-lst Univ) -Number) (or/sc number/sc (listof/sc any-wrap/sc)))

              ;; classes
              (t-sc (-class) (class/sc null #f null null))
              (t-sc (-class #:init ([x -Number #f] [y -Number #f]))
                    (class/sc (list (member-spec 'init 'x number/sc)
                                    (member-spec 'init 'y number/sc))
                              #f null null))
              (t-sc (-class #:init ([x -Number #f] [y -Number #t]))
                    (class/sc (list (member-spec 'init 'x number/sc)
                                    (member-spec 'init 'y number/sc))
                              #f null null))
              (t-sc (-class #:init ([x -Number #f]) #:init-field ([y -Integer #f]))
                    (class/sc (list (member-spec 'init 'x number/sc)
                                    (member-spec 'init 'y integer/sc)
                                    (member-spec 'field 'y integer/sc)
                                    (member-spec 'inherit-field 'y integer/sc))
                              #f null null))

              (t-int (-poly (a) (-> a a))
                     (λ (f) (f 1))
                     (λ (x) 1)
                     #:typed)
              (t-int/fail (-poly (a) (-> a a))
                          (λ (f) (f 1))
                          (λ (x) 1)
                          #:untyped
                          #:msg #rx"produced: 1.*blaming: untyped")
              (t-int (cl->* (->* '() -String -String)
                            (->* (list -Symbol) -Symbol -Symbol))
                     (λ (f) (f "a" "b"))
                     (case-lambda [xs (car xs)]
                                  [(sym . xs) sym]))
              ))
