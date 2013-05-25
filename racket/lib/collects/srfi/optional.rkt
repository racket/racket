;;;
;;; <optional.rkt> ---- Let-optionals macros
;;; Time-stamp: <2011-02-02 11:44:21 dvanhorn>
;;;
;;; Usually, I would add a copyright notice, and the announce that
;;; this code is under the LGPL licence.  This code is been copied
;;; from Scheme48, and Scsh, and thus I will append here their
;;; Copyright notice:
;;;
;;; Copyright (c) 1993-1999 Richard Kelsey and Jonathan Rees
;;; Copyright (c) 1994-1999 by Olin Shivers and Brian D. Carlstrom.
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the authors may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;
;;; Author: Francisco Solsona <solsona@acm.org>
;;
;;
;; Commentary:
;;
;; this is a port of commonly referred (in SRFI mostly) macros, and
;; utilities, to the PLT Scheme system.  I'm not sure if I should use
;; the word "port", because I hardly touched Olin Shivers' superiour
;; macros. :-)
;;

#lang scheme/base

(require (for-syntax scheme/base))

(provide :optional let-optionals* check-arg)

;; (function (check-arg predicate value caller))
;;
;;
;; Checks parameter values.
(define (check-arg pred val caller)
  (if (not (pred val))
    (let ([expected-string
           (cond [(eq? pred number? ) "expected number, "]
                 [(eq? pred integer?) "expected integer, "]
                 [(eq? pred pair?) "expected pair, "]
                 [(eq? pred procedure?) "expected procedure, "]
                 [(eq? pred string?) "expected string, "]
                 [(eq? pred vector?) "expected vector, "]
                 [else ""])])
      (error caller (string-append expected-string "given ~s") val))
    val))



;; (:optional rest-arg default-exp [test-pred])
;;
;; This form is for evaluating optional arguments and their defaults
;; in simple procedures that take a *single* optional argument. It is
;; a macro so that the default will not be computed unless it is needed.
;;
;; REST-ARG is a rest list from a lambda -- e.g., R in
;;     (lambda (a b . r) ...)
;; - If REST-ARG has 0 elements, evaluate DEFAULT-EXP and return that.
;; - If REST-ARG has 1 element, return that element.
;; - If REST-ARG has >1 element, error.
;;
;; If there is an TEST-PRED form, it is a predicate that is used to test
;; a non-default value. If the predicate returns false, an error is raised.
(define-syntax (:optional stx)
  (syntax-case stx ()
    [(_ rest default-exp)
     (syntax
       (let ([maybe-arg rest])
         (if (pair? maybe-arg)
           (if (null? (cdr maybe-arg)) (car maybe-arg)
               (error "too many optional arguments:" maybe-arg))
           default-exp)))]
    [(_ rest default-exp arg-test)
     (syntax
       (let ([maybe-arg rest])
         (if (pair? maybe-arg)
           (if (null? (cdr maybe-arg))
             (let ([val (car maybe-arg)])
               (if (arg-test val) val
                   (error "optional argument failed test:" val)))
             (error "too many optional arguments:" maybe-arg))
           default-exp)))]))


;; NOTE: This is the `less-efficient version of LET-OPTIONALS*'.
;;       Once I understand the more efficient one, as for to
;;       adapt it to PLT Scheme, I will.  Sorry, Olin Shivers,
;;       wrote a far to complex thing for me to grasp. :-{

;;      (LET-OPTIONALS* arg-list (opt-clause1 ... opt-clauseN [rest])
;;       body ...)
;; where
;;     <opt-clause> ::= (var default [arg-check supplied?])
;;                  |   ((var1 ... varN) external-arg-parser)
;;
;; LET-OPTIONALS* has LET* scope -- each arg clause sees the bindings of
;; the previous clauses. LET-OPTIONALS has LET scope -- each arg clause
;; sees the outer scope (an ARG-CHECK expression sees the outer scope
;; *plus* the variable being bound by that clause, by necessity).
(define-syntax let-optionals*
  (syntax-rules ()
    [(let-optionals* arg (opt-clause ...) body ...)
     (let ([rest arg])
       (%let-optionals* rest (opt-clause ...) body ...))]))


(define-syntax %let-optionals*
  (syntax-rules ()
    [(%let-optionals* arg (((var ...) xparser) opt-clause ...) body ...)
     (call-with-values (lambda () (xparser arg))
         (lambda (rest var ...)
           (%let-optionals* rest (opt-clause ...) body ...)))]

    [(%let-optionals* arg ((var default) opt-clause ...) body ...)
     (call-with-values (lambda () (if (null? arg) (values default '())
                                      (values (car arg) (cdr arg))))
         (lambda (var rest)
           (%let-optionals* rest (opt-clause ...) body ...)))]

    [(%let-optionals* arg ((var default test) opt-clause ...) body ...)
     (call-with-values (lambda ()
                         (if (null? arg) (values default '())
                             (let ([var (car arg)])
                               (if test (values var (cdr arg))
                                   (error "arg failed LET-OPT test" var)))))
         (lambda (var rest)
           (%let-optionals* rest (opt-clause ...) body ...)))]

    [(%let-optionals* arg ((var default test supplied?) opt-clause ...) body ...)
     (call-with-values (lambda ()
                         (if (null? arg) (values default #f '())
                             (let ([var (car arg)])
                               (if test (values var #t (cdr arg))
                                   (error "arg failed LET-OPT test" var)))))
         (lambda (var supplied? rest)
           (%let-optionals* rest (opt-clause ...) body ...)))]

    [(%let-optionals* arg (rest) body ...)
     (let ([rest arg]) body ...)]

    [(%let-optionals* arg () body ...)
     (if (null? arg) (begin body ...)
         (error "Too many arguments in let-opt" arg))]))

;; optional.rkt ends here
