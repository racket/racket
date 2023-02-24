#lang racket/base

(require syntax/parse/pre
         "exptime/import-export.rkt"
         (for-template "keywords.rkt"
                       racket/base))

(provide import-clause/c export-clause/c body-clause/c)

(define-syntax-class (unit/c-clause import?)
  #:auto-nested-attributes
  #:transparent
  (pattern ({~var s (tagged-ie-spec import?)} [x:identifier c:expr] ...))
  (pattern {~var s (tagged-ie-spec import?)} ;; allow a non-wrapped sig, which is the same as (sig)
           #:with (x ...) null
           #:with (c ...) null))
(define-syntax-class import-clause/c #:literals (import)
  #:auto-nested-attributes
  #:transparent
  (pattern (import {~var i (unit/c-clause #t)} ...)))
(define-syntax-class export-clause/c #:literals (export)
  #:auto-nested-attributes
  #:transparent
  (pattern (export {~var e (unit/c-clause #f)} ...)))

(define-syntax-class body-clause/c
  #:attributes [{ctc 1} name]
  #:literals [values]
  #:transparent
  #:commit
  (pattern (values ~! ctc:expr ...)
    #:attr name #'(quote (values ctc ...)))
  (pattern single:expr
    #:attr {ctc 1} (list #'single)
    #:attr name #'(quote single)))
