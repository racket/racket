#lang racket/base

(require (only-in "runtime.rkt"
                  match-equality-test
                  exn:misc:match?)
         (only-in "match-expander.rkt"
                  define-match-expander)
         "define-forms.rkt"
         (for-syntax "parse-legacy.rkt"
                     (only-in "patterns.rkt" match-...-nesting)))

(provide (for-syntax match-...-nesting)
         match-equality-test
         define-match-expander
         exn:misc:match?)

(define-forms parse/legacy
  match match* match/values
  match-lambda match-lambda* match-lambda** match-case-lambda
  match-λ match-λ* match-λ** match-case-λ
  match-let match-let*
  match-let-values match-let*-values
  match-define match-define-values
  match-letrec match-letrec-values
  match/derived match*/derived
  define/match)
