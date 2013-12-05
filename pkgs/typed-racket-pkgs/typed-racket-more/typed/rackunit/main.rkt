#lang typed/racket
(require typed/private/utils
         typed/private/rewriter
         "type-env-ext.rkt")

(define-type check-ish-ty
  (case-lambda
    (Any Any -> Any)
    (Any Any String -> Any)))
(define-type (Predicate A) (A -> Boolean))
(define-type (Thunk A) (-> A))

; 3.2
(require/typed/provide
 rackunit
 [check-eq? check-ish-ty]
 [check-not-eq? check-ish-ty]
 [check-eqv? check-ish-ty]
 [check-not-eqv? check-ish-ty]
 [check-equal? check-ish-ty]
 [check-not-equal? check-ish-ty]
 [check-pred
  (All (A)
       (case-lambda
         ((A -> Any) A -> Any)
         ((A -> Any) A String -> Any)))]
 [check-=
  (case-lambda
    (Real Real Real -> Any)
    (Real Real Real String -> Any))]
 [check-true
  (case-lambda
    (Any -> Any)
    (Any String -> Any))]
 [check-false
  (case-lambda
    (Any -> Any)
    (Any String -> Any))]
 [check-not-false
  (case-lambda
    (Any -> Any)
    (Any String -> Any))]
 [check-exn
  (case-lambda
    ((U (Predicate Any) Regexp) (Thunk Any) -> Any)
    ((U (Predicate Any) Regexp) (Thunk Any) String -> Any))]
 [check-not-exn
  (case-lambda
    ((Thunk Any) -> Any)
    ((Thunk Any) String -> Any))]
 [check-regexp-match
  (Regexp String -> Any)]


 [check (All (A B)
             (case-lambda
               ((A B -> Any) A B -> Any)
               ((A B -> Any) A B String -> Any)))]

 [fail
  (case-lambda
    (-> Void)
    (String -> Void))])

; 3.2.1
(require-typed-struct check-info
                      ([name : Symbol] [value : Any])
                      rackunit)
(define-type CheckInfo check-info)
(provide (struct-out check-info) CheckInfo)
(require/typed/provide
 rackunit
 [make-check-name (String -> CheckInfo)]
 [make-check-params ((Listof Any) -> CheckInfo)]
 [make-check-location ((List Any (Option Number) (Option Number) (Option Number) (Option Number)) -> CheckInfo)]
 [make-check-expression (Any -> CheckInfo)]
 [make-check-message (String -> CheckInfo)]
 [make-check-actual (Any -> CheckInfo)]
 [make-check-expected (Any -> CheckInfo)]
 [with-check-info* (All (A) ((Listof CheckInfo) (Thunk A) -> A))])
(require (only-in rackunit with-check-info))
(provide with-check-info)

; 3.2.2
(require (only-in rackunit define-simple-check define-binary-check define-check fail-check))
(provide define-simple-check define-binary-check define-check fail-check)

; 3.3
(require (prefix-in t: (except-in rackunit struct:check-info struct:exn:test struct:exn:test:check struct:test-result struct:test-failure
                                  struct:test-error struct:test-success)))
(define-rewriter t:test-begin test-begin
  [t:current-test-case-around current-test-case-around]
  [t:check-around check-around]
  [t:current-check-handler current-check-handler]
  [t:current-check-around current-check-around])
(define-rewriter t:test-case test-case
  [t:current-test-case-around current-test-case-around]
  [t:check-around check-around]
  [t:current-check-handler current-check-handler]
  [t:current-check-around current-check-around])
(provide test-begin test-case)

(require/opaque-type TestCase test-case? rackunit)
(provide TestCase test-case?)



(require (only-in rackunit test-suite))
(provide test-suite)
(require/opaque-type TestSuite test-suite? rackunit)
(provide TestSuite test-suite?)

(define-type Test (U TestCase TestSuite))
(provide Test)

(require/typed/provide
 rackunit
 [make-test-suite
  (case-lambda
    (String (Listof Test) -> TestSuite)
    ; XXX #:before #:after
    )])

(require (only-in rackunit define-test-suite define/provide-test-suite))
(provide define-test-suite define/provide-test-suite)

(require/typed/provide
 rackunit
 [current-test-name (Parameter (Option String))]
 [current-test-case-around (Parameter ((Thunk Any) -> Any))]
 [test-suite-test-case-around ((Thunk Any) -> Any)]
 [test-suite-check-around ((Thunk Any) -> Any)])

; 3.4
(require (only-in rackunit before after around delay-test))
(provide before after around delay-test)

; 3.5
; XXX require/expose seems WRONG for typed/scheme

; 3.7
(require-typed-struct (exn:test exn) () rackunit)
(require-typed-struct (exn:test:check exn:test) ([stack : (Listof CheckInfo)]) rackunit)
(require-typed-struct test-result ([test-case-name : (Option String)]) rackunit)
(require-typed-struct (test-failure test-result) ([result : Any]) rackunit)
(require-typed-struct (test-error test-result) ([result : Any]) rackunit)
(require-typed-struct (test-success test-result) ([result : Any]) rackunit)
(provide (struct-out exn:test) (struct-out exn:test:check)
         (struct-out test-result)
         (struct-out test-failure) (struct-out test-error) (struct-out test-success))

(define-type (Tree A)
  (Rec The-Tree
       (Listof (U A The-Tree))))

(require/typed/provide
 rackunit
 [run-test-case
  ((Option String) (Thunk Any) -> test-result)]
 [run-test
  (Test -> (Tree test-result))]
 ; XXX Requires keywords and weird stuff
 #;[fold-test-results
    XXX]
 ; XXX Requires knowing more about test cases and structs
 #;[foldts-test-suite
    XXX])


; 5.1
(require/typed/provide
 rackunit
 [current-check-handler
  (Parameter (Any -> Any))]
 [current-check-around
  (Parameter ((Thunk Any) -> Any))])


