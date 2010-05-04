#lang typed/scheme
(require typed/private/utils)

(define-type check-ish-ty
  (case-lambda
    (Any Any -> Void)
    (Any Any String -> Void)))
(define-type (Predicate A) (A -> Boolean))
(define-type (Thunk A) (-> A))

; 3.2
(require/typed/provide
 racunit
 [check (All (A B C)
             (case-lambda
               ((A B -> C) A B -> C)
               ((A B -> C) A B String -> C)))]
 [check-eq? check-ish-ty]
 [check-not-eq? check-ish-ty]
 [check-eqv? check-ish-ty]
 [check-not-eqv? check-ish-ty]
 [check-equal? check-ish-ty]
 [check-not-equal? check-ish-ty]
 [check-pred
  (All (A B)
       (case-lambda
         ((A -> B) A -> #t)
         ((A -> B) A String -> #t)))]
 [check-=
  (case-lambda
    (Number Number Number -> #t)
    (Number Number Number String -> #t))]
 [check-true
  (case-lambda
    (Boolean -> #t)
    (Boolean String -> #t))]
 [check-false
  (case-lambda
    (Boolean -> #t)
    (Boolean String -> #t))]
 [check-not-false
  (case-lambda
    (Any -> #t)
    (Any String -> #t))]
 [check-exn
  (case-lambda 
    ((Predicate Any) (Thunk Any) -> #t)
    ((Predicate Any) (Thunk Any) String -> #t))]
 [check-not-exn
  (case-lambda
    ((Thunk Any) -> #t)
    ((Thunk Any) String -> #t))]
 [fail
  (case-lambda
    (-> #t)
    (String -> #t))]
 [check-regexp-match
  (Regexp String -> #t)])

; 3.2.1
(require-typed-struct check-info
                      ([name : Symbol] [value : Any])
                      racunit)
(define-type CheckInfo check-info)
(provide (struct-out check-info) CheckInfo)
(require/typed/provide
 racunit
 [make-check-name (String -> CheckInfo)]
 [make-check-params ((Listof Any) -> CheckInfo)]
 [make-check-location ((List Any (U Number #f) (U Number #f) (U Number #f) (U Number #f)) -> CheckInfo)]
 [make-check-expression (Any -> CheckInfo)]
 [make-check-message (String -> CheckInfo)]
 [make-check-actual (Any -> CheckInfo)]
 [make-check-expected (Any -> CheckInfo)]
 [with-check-info* (All (A) ((Listof CheckInfo) (Thunk A) -> A))])
(require (only-in racunit with-check-info))
(provide with-check-info)

; 3.2.2
(require (only-in racunit define-simple-check define-binary-check define-check fail-check))
(provide define-simple-check define-binary-check define-check fail-check)

; 3.2.3
(require/typed/provide
 racunit
 [current-check-handler
  (Parameter (Any -> Any))]
 [current-check-around
  (Parameter ((Thunk Any) -> Any))])

; 3.3
(require (only-in racunit test-begin test-case))
(provide test-begin test-case)

(require/opaque-type TestCase test-case? racunit)
(provide TestCase test-case?)

(require (only-in racunit test-suite))
(provide test-suite)
(require/opaque-type TestSuite test-suite? racunit)
(provide TestSuite test-suite?)

(define-type Test (U TestCase TestSuite))
(provide Test)

(require/typed/provide
 racunit
 [make-test-suite
  (case-lambda
    (String (Listof Test) -> TestSuite)
    ; XXX #:before #:after
    )])

(require (only-in racunit define-test-suite define/provide-test-suite))
(provide define-test-suite define/provide-test-suite)

(require/typed/provide
 racunit
 [current-test-name (Parameter (Option String))]
 [current-test-case-around (Parameter ((Thunk Any) -> Any))]
 [test-suite-test-case-around ((Thunk Any) -> Any)]
 [test-suite-check-around ((Thunk Any) -> Any)])

; 3.4
(require (only-in racunit before after around delay-test))
(provide before after around delay-test)

; 3.5
; XXX require/expose seems WRONG for typed/scheme

; 3.7
(require-typed-struct (exn:test exn) () racunit)
(require-typed-struct (exn:test:check exn:test) ([stack : (Listof CheckInfo)]) racunit)
(require-typed-struct test-result ([test-case-name : (Option String)]) racunit)
(require-typed-struct (test-failure test-result) ([result : Any]) racunit)
(require-typed-struct (test-error test-result) ([result : Any]) racunit)
(require-typed-struct (test-success test-result) ([result : Any]) racunit)
(provide (struct-out exn:test) (struct-out exn:test:check)
         (struct-out test-result)
         (struct-out test-failure) (struct-out test-error) (struct-out test-success))

(define-type (Tree A)
  (Rec The-Tree
       (Listof (U A The-Tree))))

(require/typed/provide
 racunit
 [run-test-case 
  ((Option String) (Thunk Any) -> test-result)]
 [run-test
  (Test -> (Tree test-result))]
 ; XXX Requires keywords and weird stuff
 #;[fold-test-results
    XXX]
 ; XXX Requires knowing more about test cases and structs
 #;[foldts
    XXX])
