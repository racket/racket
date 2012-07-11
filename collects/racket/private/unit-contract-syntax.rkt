#lang racket/base

(require syntax/parse
         "unit-compiletime.rkt"
         "unit-keywords.rkt"
         (for-template "unit-keywords.rkt"))

(provide import-clause/contract export-clause/contract dep-clause
         import-clause/c export-clause/c)

(define-syntax-class sig-id
  #:attributes ()
  (pattern x
           #:declare x (static (λ (x)
                                 (signature? (set!-trans-extract x)))
                               'signature)))

(define-syntax-class sig-spec #:literals (prefix rename only except)
  #:attributes ((name 0))
  #:transparent
  (pattern name:sig-id)
  (pattern (prefix i:identifier s:sig-spec)
           #:with name #'s.name)
  (pattern (rename s:sig-spec [int:identifier ext:identifier] ...)
           #:with name #'s.name)
  (pattern (only s:sig-spec i:identifier ...)
           #:with name #'s.name)
  (pattern (except s:sig-spec i:identifier ...)
           #:with name #'s.name))

(define-syntax-class tagged-sig-spec #:literals (tag)
  #:auto-nested-attributes
  #:transparent
  (pattern s:sig-spec
           #:with i #f)
  (pattern (tag i:identifier s:sig-spec)))

(define-syntax-class tagged-sig-id #:literals (tag)
  #:attributes ()
  #:transparent
  (pattern s:sig-id)
  (pattern (tag i:identifier s:sig-id)))

(define-syntax-class unit/c-clause
  #:auto-nested-attributes
  #:transparent
  (pattern (s:tagged-sig-id [x:identifier c:expr] ...))
  (pattern s:tagged-sig-id ;; allow a non-wrapped sig, which is the same as (sig)
           #:with (x ...) null
           #:with (c ...) null))
(define-syntax-class import-clause/c #:literals (import)
  #:auto-nested-attributes
  #:transparent
  (pattern (import i:unit/c-clause ...)))
(define-syntax-class export-clause/c #:literals (export)
  #:auto-nested-attributes
  #:transparent
  (pattern (export e:unit/c-clause ...)))

(define-syntax-class unit/contract-clause
  #:auto-nested-attributes
  #:transparent
  (pattern (s:tagged-sig-spec [x:identifier c:expr] ...))
  (pattern s:tagged-sig-spec ;; allow a non-wrapped sig, which is the same as (sig)
           #:with (x ...) null
           #:with (c ...) null))
(define-syntax-class import-clause/contract #:literals (import)
  #:auto-nested-attributes
  #:transparent
  (pattern (import i:unit/contract-clause ...)))
(define-syntax-class export-clause/contract #:literals (export)
  #:auto-nested-attributes
  #:transparent
  (pattern (export e:unit/contract-clause ...)))
(define-syntax-class dep-clause #:literals (init-depend)
  #:auto-nested-attributes
  #:transparent
  (pattern (init-depend s:tagged-sig-id ...)))
