#lang racket/base

(require "test-utils.rkt" 
         (rep type-rep)
         (types utils kw-types abbrev numeric-tower)
         racket/match racket/set
         rackunit)
(provide tests)
(gen-test-main)

(define-syntax-rule (t arg expected)
  (begin
    (test-equal? (format "~a" '(arg expected))
                 (kw-convert arg #f)
                 expected)))

(define (extract-arrs t)
  (match t
    [(Function: arrs) (apply set arrs)]
    [t t]))

(define-syntax-rule (t-opt ((req-arg ...) (opt-arg ...)) expected)
  (let ()
    (test-equal? (format "~a" '(opt-convert (->opt req-arg ... (opt-arg ...) result) expected))
                 (extract-arrs
                   (opt-convert (->opt req-arg ... (opt-arg ...) result)
                                (length (list 'req-arg ...))
                                (length (list 'opt-arg ...))))
                 (extract-arrs expected))))


(define flag -Boolean)
(define true (-val #t))
(define false (-val #f))
(define result (-val 'result))
(define one (-val 'one))
(define two (-val 'two))
(define three (-val 'three))
(define four (-val 'four))

(define tests
  (test-suite "Tests for keyword expansion"

    [t (-> result) (-> result)]
    [t (-> one result)
       (-> one result)]
    [t (-> one two three four result)
       (-> one two three four result)]
    [t (->opt (one) result)
       (-> (-opt one) flag result)]
    [t (->opt (one two) result)
       (-> (-opt one) (-opt two) flag flag result)]
    [t (->opt one (two three) result)
       (-> one (-opt two) (-opt three) flag flag result)]

    [t-opt (() ()) (-> result)]
    [t-opt ((one) ())
           (-> one result)]
    [t-opt ((one two three four) ())
       (-> one two three four result)]
    [t-opt (() (one))
           (cl->*
             (-> one true result)
             (-> false false result))]
    [t-opt (() (one two))
           (cl->*
             (-> one two true true result)
             (-> one false true false result)
             (-> false false false false result))]
    [t-opt ((one) (two three))
           (cl->*
             (-> one two three true true result)
             (-> one two false true false  result)
             (-> one false false false false result))]
    ))
