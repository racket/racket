#lang racket/base

(require (only-in "runtime.rkt"
                  match-equality-test
                  exn:misc:match?)
         (only-in "match-expander.rkt"
                  define-match-expander)
         "define-forms.rkt"
         (for-syntax "parse-legacy.rkt"
                     "gen-match.rkt"
                     (only-in "patterns.rkt" match-...-nesting)))

(provide (for-syntax match-...-nesting)
         match-equality-test
         define-match-expander
         exn:misc:match?)

(define-forms parse/legacy/cert
  match match* match-lambda match-lambda* match-lambda** match-let match-let*
  match-define match-letrec match/derived match*/derived)
