#lang racket/base
(require "private/sc.rkt"
         "private/litconv.rkt"
         "private/lib.rkt"
         "private/residual.rkt")
(provide (except-out (all-from-out "private/sc.rkt")
                     define-integrable-syntax-class
                     syntax-parser/template)
         (all-from-out "private/litconv.rkt")
         (all-from-out "private/lib.rkt")
         syntax-parse-state-ref
         syntax-parse-state-set!
         syntax-parse-state-update!
         syntax-parse-state-cons!
         syntax-parse-track-literals)

(define not-given (gensym))

(define (state-ref who key default)
  (define state (current-state))
  (if (eq? default not-given)
      (if (hash-has-key? state key)
          (hash-ref state key)
          (error who "no value found for key\n  key: ~e" key))
      (hash-ref state key default)))

(define (syntax-parse-state-ref key [default not-given])
  (state-ref 'syntax-parse-state-ref key default))

(define (check-update who)
  (unless (current-state-writable?)
    (error who "cannot update syntax-parse state outside of ~~do/#:do block")))

(define (syntax-parse-state-set! key value)
  (check-update 'syntax-parse-state-set!)
  (current-state (hash-set (current-state) key value)))

(define (syntax-parse-state-update! key update [default not-given])
  (check-update 'syntax-parse-state-update!)
  (define old (state-ref 'syntax-parse-state-update! key default))
  (current-state (hash-set (current-state) key (update old))))

(define (syntax-parse-state-cons! key value [default null])
  (check-update 'syntax-parse-state-cons!)
  (define old (hash-ref (current-state) key default))
  (current-state (hash-set (current-state) key (cons value old))))

(define (syntax-parse-track-literals stx #:introduce? [introduce? #t])
  (track-literals 'syntax-parse-track-literals stx #:introduce? introduce?))
