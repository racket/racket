#lang scheme/base

(require stxclass
         "unit-compiletime.ss"
         (for-template "unit-keywords.ss"))

(provide import-clause export-clause)

(define-syntax-class sig-id
  #:attributes ()
  (pattern x
           #:declare x (static-of 'signature 
                                  (λ (x)
                                    (signature? (set!-trans-extract x))))))

(define-syntax-class sig-spec #:literals (prefix rename only except)
  #:attributes ()
  #:transparent
  (pattern s:sig-id)
  (pattern (prefix i:identifier s:sig-spec))
  (pattern (rename s:sig-spec [int:identifier ext:identifier] ...))
  (pattern (only s:sig-spec i:identifier ...))
  (pattern (except s:sig-spec i:identifier ...)))

(define-syntax-class tagged-sig-spec #:literals (tag)
  #:attributes ()
  #:transparent
  (pattern s:sig-spec)
  (pattern (tag i:identifier s:sig-spec)))

(define-syntax-class unit/c-clause
  #:transparent
  (pattern (s:tagged-sig-spec [x:identifier c:expr] ...))
  (pattern s:tagged-sig-spec ;; allow a non-wrapped sig, which is the same as (sig)
           #:with (x ...) null
           #:with (c ...) null))
(define-syntax-class import-clause #:literals (import)
  #:transparent
  (pattern (import i:unit/c-clause ...)))
(define-syntax-class export-clause #:literals (export)
  #:transparent
  (pattern (export e:unit/c-clause ...)))