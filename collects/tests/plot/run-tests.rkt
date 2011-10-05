#!/bin/sh
#| -*- racket -*-
exec gracket "$0" "$@"
|#
#lang racket

(require rackunit plot plot/utils)

(check-equal? (linear-seq 0 1 2 #:start? #t #:end? #t) '(0 1))
(check-equal? (linear-seq 0 1 2 #:start? #t #:end? #f) '(0 2/3))
(check-equal? (linear-seq 0 1 2 #:start? #f #:end? #t) '(1/3 1))
(check-equal? (linear-seq 0 1 2 #:start? #f #:end? #f) '(1/4 3/4))

(check-exn exn:fail:contract?
           (λ () (vector-field (λ (v [z 0]) v) -4 4 -4 4))
           "Exception should be 'two of the clauses in the or/c might both match' or similar")
