#lang racket/base
(require "../utils/utils.rkt"
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
         (only-in racket/contract current-blame-format)
         "abbrev.rkt" "numeric-tower.rkt"
         unstable/lazy-require
         (for-template racket/base))

(lazy-require ["union.rkt" (Un)])

(provide (all-defined-out)
         (all-from-out "abbrev.rkt" "numeric-tower.rkt")
         ;; these should all eventually go away
         make-Name make-ValuesDots make-Function
         (rep-out filter-rep object-rep))

(define (one-of/c . args)
  (apply Un (map -val args)))

(define (-opt t) (Un (-val #f) t))

(define In-Syntax
  (-mu e
       (Un (-val null) -Boolean -Symbol -String -Keyword -Char -Number
           (make-Vector (-Syntax e))
           (make-Box (-Syntax e))
           (-lst (-Syntax e))
           (-pair (-Syntax e) (-Syntax e)))))

(define Any-Syntax (-Syntax In-Syntax))

(define (-Sexpof t)
  (-mu sexp
       (Un (-val '())
           -Number -Boolean -Symbol -String -Keyword -Char
           (-pair sexp sexp)
           (make-Vector sexp)
           (make-Box sexp)
           t)))

(define -Sexp (-Sexpof (*Un)))

(define Syntax-Sexp (-Sexpof Any-Syntax))

(define Ident (-Syntax -Symbol))


(define -Module-Path (*Un -Symbol -String
                          (-lst* (-val 'quote) -Symbol)
                          (-lst* (-val 'lib) -String)
                          (-lst* (-val 'file) -String)
                          (-pair (-val 'planet)
                           (*Un (-lst* -Symbol)
                                (-lst* -String)
                                (-lst* -String (-lst* -String -String #:tail (make-Listof (*Un -Nat (-lst* (*Un -Nat (one-of/c '= '+ '-)) -Nat)))))))))

(define -Log-Level (one-of/c 'fatal 'error 'warning 'info 'debug))

