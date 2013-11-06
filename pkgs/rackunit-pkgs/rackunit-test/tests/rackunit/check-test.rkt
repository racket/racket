;;;
;;; Time-stamp: <2009-03-25 12:32:55 noel>
;;;
;;; Copyright (C) by Noel Welsh. 
;;;

;;; This library is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU Lesser
;;; General Public License as published by the Free Software
;;; Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.

;;; This library is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.

;;; You should have received a copy of the GNU Lesser
;;; General Public License along with this library; if not,
;;; write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA 02111-1307 USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:

#lang racket/base

(require racket/runtime-path
         srfi/1
         rackunit
         rackunit/private/check
         rackunit/private/result
         rackunit/private/test-suite)

(provide check-tests)

(define (make-failure-test name pred . args)
  (test-case
   name
   (check-exn exn:test:check?
              (lambda ()
                (apply pred args)))))

;; NOTE(jpolitz): Not generalizing make-failure-test above because (at
;; least) util-test expects it to be present, not exported, and a
;; 2-argument procedure to test require/expose
(define-syntax make-failure-test/stx
  (syntax-rules ()
    [(_ name pred arg ...)
     (test-case
      name
      (check-exn exn:test:check?
                 (lambda ()
                   (pred arg ...))))]))

(define-check (good)
  #t)

(define-check (bad)
  (fail-check))

(define check-tests
  (test-suite
   "Check tests"
   ;; Successes
   (test-case "Simple check-equal? test"
              (check-equal? 1 1))
   (test-case "Simple check-eq? test"
              (check-eq? 'a 'a))
   (test-case "Simple check-eqv? test"
              (check-eqv? 'a 'a))
   (test-case "Simple check test"
              (check string=? "hello" "hello"))
   (test-case "Simple check-true test"
              (check-true (eq? 'a 'a)))
   (test-case "Simple check-pred test"
              (check-pred null? (list)))
   (test-case "Simple check-exn test"
              (check-exn exn:test:check?
                         (lambda ()
                           (check = 1 2))))
   (test-case "Simple check-not-exn test"
              (check-not-exn
               (lambda ()
                 (check = 1 1))))
   (test-case "Simple check-not-eq?"
              (check-not-eq? (cons 'a 'a) (cons 'a 'a)))
   (test-case "Simple check-not-equal?"
              (check-not-equal? 1 2))
   (test-case "Defined check succeeds"
              (good))
   (test-case "Simple check-not-false test"
              (check-not-false 3))
   (test-case "Simple check-= test"
              (check-= 1.0 1.0 0.0001))
   
   (test-case "Use of check as expression"
              (for-each check-false '(#f #f #f)))
   (test-case "Use of local check as expression"
              (let ()
                (define-simple-check (check-symbol? x)
                  (symbol? x))
                (for-each check-symbol? '(a b c))))

   (test-case "Trivial check-match test"
              (check-match "dirigible" _))

   (test-case "Simple check-match test"
              (check-match (list 1 2 3) (list _ _ 3)))

   (test-case "check-match with a nested struct"
              (let ()
                (struct data (f1 f2 f3))
                (check-match (data 1 2 (data 1 2 3))
                             (data _ 2 (data _ _ _)))))

   (test-case "Simple check-match test with a binding pred"
              (check-match 3 x (odd? x)))

   (test-case "check-match with a nested struct and a binding pred"
              (let ()
                (struct data (f1 f2 f3))
                (check-match (data 1 2 (data 1 2 3))
                             (data _ _ (data x y z))
                             (equal? (+ x y z) 6))))
   
   ;; Failures
   (make-failure-test "check-equal? failure"
                      check-equal? 1 2)
   (make-failure-test "check-eq? failure"
                      check-eq? 'a 'b)
   (make-failure-test "check-eqv? failure"
                      check-eqv? 'a 'b)
   (make-failure-test "check failure"
                      check string=? "hello" "bye")
   (make-failure-test "check-true failure"
                      check-true (eq? 'a 'b))
   (make-failure-test "check-pred failure"
                      check-pred null? (list 1 2 3))
   (make-failure-test "check-exn failure"
                      check-exn exn:test:check? (lambda () (check = 1 1)))
   (make-failure-test "check-exn wrong exception"
                      check-exn exn:fail:contract:arity? (lambda () (+ 1 2)))
   (make-failure-test "check-not-exn"
                      check-not-exn (lambda () (/ 1 0)))
   (make-failure-test "fail with message failure"
                      fail "With message")
   (make-failure-test "fail without message failure"
                      fail)
   (make-failure-test "Defined check fails"
                      bad)
   (make-failure-test "check-not-false failure"
                      check-not-false #f)
   (make-failure-test "check-= failure"
                      check-= 1.0 2.0 0.0)

   (make-failure-test/stx "check-match failure pred"
                          check-match 5 x (even? x))

   (make-failure-test/stx "check-match failure match"
                           check-match (list 4 5) (list _))
   
   (test-case "check-= allows differences within epsilon"
              (check-= 1.0 1.09 1.1))
   
   (make-failure-test "check-= failure > epsilon"
                      check-= 1 12/10 1/10)
   
   (test-case "check-as-expression failure"
              (check-exn exn:test:check?
                         (lambda ()
                           (for-each check-false '(#f not-false)))))
   
   (test-case
    "Check allows optional message"
    (begin
      (check = 1 1 "message")))
   
   ;; Some necessary semantics
   (test-case
    "Check macro parameters evaluated once (simple-check)"
    (let ((counter 0))
      (check-true (begin (set! counter (add1 counter))
                         #t))
      (check = counter 1)))
   (test-case
    "Check macro parameters evaluated once (binary-check)"
    (let ((counter 0))
      (check-equal? (begin (set! counter (add1 counter))
                           1)
                    (begin (set! counter (add1 counter))
                           1))
      (check = counter 2)))
   (test-case
    "Check function parameters evaluated once (simple-check)"
    (let ((counter 0))
      (check-true (begin (set! counter (add1 counter))
                         #t))
      (check = counter 1)))
   (test-case
    "Check function parameters evaluated once (binary-check)"
    (let ((counter 0))
      (check-equal? (begin (set! counter (add1 counter))
                           1)
                    (begin (set! counter (add1 counter))
                           1))
      (check = counter 2)))
   
   ;; Exceptions have the correct types
   (test-case
    "Macro w/ no message, message is a string"
    (let ((exn (with-handlers ([exn? (lambda (exn)
                                       exn)])
                 (check-true #f))))
      (check-pred string? (exn-message exn))))
   (test-case
    "Function w/ no message, message is a string"
    (let ((exn (with-handlers ([exn? (lambda (exn)
                                       exn)])
                 (check-true #f))))
      (check-pred string? (exn-message exn))))
   
   ;; The check construction language
   (test-case
    "with-check-info* captures information"
    (let ((name (make-check-info 'name "name"))
          (info (make-check-info 'info "info")))
      (with-handlers
          [(exn:test:check?
            (lambda (exn)
              (let ((stack (exn:test:check-stack exn)))
                (check = (length stack) 2)
                (let ((actual-name (first stack))
                      (actual-info (second stack)))
                  (check-equal? name actual-name)
                  (check-equal? info actual-info)))))]
        (with-check-info*
         (list name info)
         (lambda ()
           (fail-check))))))
   (test-case
    "with-check-info captures information"
    (with-handlers
        [(exn:test:check?
          (lambda (exn)
            (let ((stack (exn:test:check-stack exn)))
              (check = (length stack) 2)
              (let ((name (first stack))
                    (info (second stack)))
                (check-eq? (check-info-name name) 'name)
                (check string=? (check-info-value name) "name")
                (check-eq? (check-info-name info) 'info)
                (check string=? (check-info-value info) "info")))))]
      (with-check-info
       (('name "name") ('info "info"))
       (fail-check))))
   (test-case
    "check information stack unwinds"
    (with-handlers
        [(exn:test:check?
          (lambda (exn)
            (let ((stack (exn:test:check-stack exn)))
              (check = (length stack) 2)
              (let ((name (first stack))
                    (info (second stack)))
                (check-eq? (check-info-name name) 'name)
                (check string=? (check-info-value name) "name")
                (check-eq? (check-info-name info) 'info)
                (check string=? (check-info-value info) "info")))))]
      (with-check-info
       (('name "name") ('info "info"))
       (with-check-info
        (('name "name") ('info "info"))
        #t)
       (fail-check))))
   
   ;; If check-exn isn't working correctly many tests above will
   ;; silently fail.  Here we test check-exn is working.
   (test-case
    "check-exn traps exception"
    (with-handlers
        ((exn?
          (lambda (exn) (fail "Received exception"))))
      (check-exn exn:fail:contract:arity?
                 (lambda () (= 1)))))
   (test-case
    "check-exn fails if no exception raised"
    (with-handlers
        ((exn:test:check?
          (lambda (exn) #t))
         (exn:fail:contract:arity?
          (lambda (exn) (fail "check-exn didn't fail"))))
      (check-exn exn? (lambda () (= 1 1)))
      (= 1)))
   
   (test-case
    "check-not-exn captures exception information if one raised"
    (let* ([case (delay-test
                  (test-case "check-not-exn"
                             (check-not-exn
                              (lambda () (error "Oh dear!")))))]
           [result (test-failure-result (car (run-test case)))]
           [names (map check-info-name
                       (exn:test:check-stack result))])
      (check-true
       (fold (lambda (name found?)
               (if (eq? name 'exception)
                   #t
                   found?))
             #f names))
      (check-true
       (fold (lambda (name found?)
               (if (eq? name 'exception-message)
                   #t
                   found?))
             #f names))))
   
   ;; Verify that check-exn and check-not-exn raise errors (not check
   ;; failures) if not given thunks.
   (test-case
    "check-exn raises contract exception if not given a procedure"
    (check-exn exn:fail:contract?
               (lambda ()
                 (check-exn exn:fail? 'not-a-procedure))))

   (test-case
    "check-exn raises contract exception if given a procedure with incorrect arity"
    (check-exn exn:fail:contract?
               (lambda ()
                 (check-exn exn:fail? (lambda (x) x)))))

   (test-case
    "check-not-exn raises contract exception if not given a procedure"
    (check-exn exn:fail:contract?
               (lambda ()
                 (check-not-exn 'not-a-procedure))))

   (test-case
    "check-not-exn raises contract exception if given a procedure with incorrect arity"
    (check-exn exn:fail:contract?
               (lambda ()
                 (check-not-exn (lambda (x) x)))))

   ;; Regression test
   ;; Uses of check (and derived forms) used to be un-compilable!
   ;; We check that (write (compile --code-using-check--)) works.
   ;; That involves some namespace hacking.
   (test-case
    "Checks are compilable"
    (let ((destns (make-base-namespace))
          (cns (current-namespace)))
      (parameterize ((current-namespace destns))
        (namespace-require '(for-syntax racket/base))
        (namespace-require 'rackunit/private/check)
        ;; First check that the right check macro got
        ;; used: ie that it didn't just compile the thing
        ;; as an application.
        (let ((ecode
               (syntax->datum (expand '(check = 1 2)))))
          (check-false (and (pair? ecode)
                            (eq? (car ecode) '#%app)
                            (pair? (cdr ecode))
                            (equal? (cadr ecode)
                                    '(#%top . check)))))
        ;; Then check to make sure that the compiled code
        ;; is writable
        (let ((stx-string "(check = 1 2)"))
          (write (compile (read-syntax
                           "check-test"
                           (open-input-string stx-string)))
                 (open-output-string))))))
   
   ;; Check evaluation contexts
   (test-case
    "current-check-around is used by checks"
    (let ([x #f])
      (parameterize ([current-check-around (lambda (t) (set! x 'foo))])
                 (check-eq? 'a 'b))
      (check-eq? x
                 'foo)))

   (test-case
    "current-check-handler is used by checks"
    (check-eq? (let/ec escape
                 (parameterize ([current-check-handler (lambda (e) (escape 'foo))])
                   (check-eq? 'a 'b)))
               'foo))
   ))
