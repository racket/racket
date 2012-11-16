;;;
;;; Time-stamp: <2009-06-11 17:11:22 noel>
;;;
;;; Copyright (C) 2005 by Noel Welsh.
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

(require racket/match
         racket/pretty
         racket/contract
         "private/base.rkt"
         "private/counter.rkt"
         "private/format.rkt"
         "private/location.rkt"
         "private/result.rkt"
         "private/check-info.rkt"
         "private/monad.rkt"
         "private/hash-monad.rkt"
         "private/name-collector.rkt"
         "private/text-ui-util.rkt"
         "private/test.rkt")

(provide/contract 
 [run-tests (((or/c test-case? test-suite?)) 
             ((or/c 'quiet 'normal 'verbose))
             . ->* . exact-nonnegative-integer?)])

(provide display-context
         display-exn
         display-summary+return
         display-ticker
         display-result)


;; display-ticker : test-result -> void
;;
;; Prints a summary of the test result
(define (display-ticker result)
  (cond
    ((test-error? result)
     (display "!"))
    ((test-failure? result)
     (display "-"))
    (else
     (display "."))))

;; display-test-preamble : test-result -> (hash-monad-of void)
(define (display-test-preamble result)
  (lambda (hash)
    (if (test-success? result)
        hash
        (begin
          (display-delimiter)
          hash))))

;; display-test-postamble : test-result -> (hash-monad-of void)
(define (display-test-postamble result)
  (lambda (hash)
    (if (test-success? result)
        hash
        (begin
          (display-delimiter)
          hash))))


;; display-result : test-result -> void
(define (display-result result)
  (cond
    ((test-error? result)
     (display-test-name (test-result-test-case-name result))
     (display-error)
     (newline))
    ((test-failure? result)
     (display-test-name (test-result-test-case-name result))
     (display-failure)
     (newline))
    (else
     (void))))

;; display-context : test-result [(U #t #f)] -> void
(define (display-context result [verbose? #f])
  (cond
    [(test-failure? result)
     (let* ([exn (test-failure-result result)]
            [stack (exn:test:check-stack exn)])
       (textui-display-check-info-stack stack verbose?)
       (when #t
         ((error-display-handler) (exn-message exn) exn)))]
    [(test-error? result)
     (let ([exn (test-error-result result)])
       (when (exn? exn)
         (textui-display-check-info-stack (check-info-stack (exn-continuation-marks exn))))
       (display-exn exn))]
    [else (void)]))

(define (textui-display-check-info-stack stack [verbose? #f])
  (for-each
   (lambda (info)
     (cond
       [(check-name? info)
        (display-check-info info)]
       [(check-location? info)
        (display-check-info-name-value
         'location
         (trim-current-directory
          (location->string
           (check-info-value info)))
         display)]
       [(check-params? info)
        (display-check-info-name-value
         'params
         (check-info-value info)
         (lambda (v) (map pretty-print v)))]
       [(check-actual? info)
        (display-check-info-name-value
         'actual
         (check-info-value info)
         pretty-print)]
       [(check-expected? info)
        (display-check-info-name-value
         'expected
         (check-info-value info)
         pretty-print)]
       [(and (check-expression? info)
             (not verbose?))
        (void)]
       [else
        (display-check-info info)]))
   (if verbose?
       stack
       (strip-redundant-params stack))))

;; display-verbose-check-info : test-result -> void
(define (display-verbose-check-info result)
  (cond
    ((test-failure? result)
     (let* ((exn (test-failure-result result))
            (stack (exn:test:check-stack exn)))
       (for-each
        (lambda (info)
          (cond
            ((check-location? info)
             (display "location: ")
             (display (trim-current-directory
                       (location->string
                        (check-info-value info)))))
            (else
             (display (check-info-name info))
             (display ": ")
             (write (check-info-value info))))
          (newline))
        stack)))
    ((test-error? result)
     (display-exn (test-error-result result)))
    (else
     (void))))

(define (std-test/text-ui display-context test)
  (parameterize ([current-output-port (current-error-port)])
    (fold-test-results
     (lambda (result seed)
       ((sequence* (update-counter! result)
                   (display-test-preamble result)
                   (display-test-case-name result)
                   (lambda (hash)
                     (display-result result)
                     (display-context result)
                     hash)
                   (display-test-postamble result))
        seed))
     ((sequence
        (put-initial-counter)
        (put-initial-name))
      (make-empty-hash))
     test
     #:fdown (lambda (name seed) ((push-suite-name! name) seed))
     #:fup (lambda (name kid-seed) ((pop-suite-name!) kid-seed)))))

(define (display-summary+return monad)
  (monad-value
   ((compose
     (sequence*
      (display-counter*)
      (counter->vector))
     (match-lambda
       ((vector s f e)
        (return-hash (+ f e)))))
    monad)))

(define (display-counter*)
  (compose (counter->vector)
           (match-lambda
             [(vector s f e)
              (if (and (zero? f) (zero? e))
                  (display-counter)
                  (lambda args
                    (parameterize ([current-output-port (current-error-port)])
                      (apply (display-counter) args))))])))

;; run-tests : test [(U 'quiet 'normal 'verbose)] -> integer
(define (run-tests test [mode 'normal])
  (parameterize ((current-custodian (make-custodian)))
    (monad-value
     ((compose
       (sequence*
        (case mode
          [(normal verbose)
           (display-counter*)]
          [(quiet)
           (lambda (a) a)])
        (counter->vector))
       (match-lambda
        ((vector s f e)
         (return-hash (+ f e)))))
      (case mode
        ((quiet)
         (fold-test-results
          (lambda (result seed)
            ((update-counter! result) seed))
          ((put-initial-counter)
           (make-empty-hash))
          test))
        ((normal) (std-test/text-ui display-context test))
        ((verbose) (std-test/text-ui
                    (lambda (x) (display-context x #t))
                    test)))))))
