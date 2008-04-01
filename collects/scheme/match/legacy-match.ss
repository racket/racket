#lang scheme/base

(require (only-in "patterns.ss"
                  match-equality-test
                  match-...-nesting
                  exn:misc:match?)
         (only-in "match-expander.ss"
                  define-match-expander)
         "define-forms.ss"
         (for-syntax "parse-legacy.ss"
                     "gen-match.ss"
                     (only-in "patterns.ss" match-...-nesting)))

(provide (for-syntax match-...-nesting)
         match-equality-test
         match-...-nesting
         define-match-expander
         exn:misc:match?)

(define-forms parse/legacy/cert
  match match* match-lambda match-lambda* match-let match-let*
  match-define match-letrec)
