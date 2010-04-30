#lang racket/base

(require (only-in "runtime.ss"
                  match-equality-test
                  exn:misc:match?)
         (only-in "match-expander.ss"
                  define-match-expander)
         "define-forms.ss"
         (for-syntax "parse-legacy.ss"
                     "gen-match.ss"
                     (only-in "patterns.ss" match-...-nesting)))

(provide (for-syntax match-...-nesting)
         match-equality-test
         define-match-expander
         exn:misc:match?)

(define-forms parse/legacy/cert
  match match* match-lambda match-lambda* match-lambda** match-let match-let*
  match-define match-letrec match/derived match*/derived)
