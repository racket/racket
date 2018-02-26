#lang racket/base
(require "../common/make-match.rkt"
         "syntax.rkt"
         "scope.rkt"
         "error.rkt")

(provide define-match)

;; See "../common/make-match.rkt" for information on using
;; `define-match`

(define-define-match define-match
  syntax? syntax-e raise-syntax-error)
