#lang racket/base

(provide (all-defined-out))

;; A fully expanded form can be parsed into an AST. In principle,
;; parsing could be a pass separate from the expander. As an important
;; shortcut, however, we fuse the expander and parser; the
;; `to-parsed?` field in an `expand-context` indicates whether the
;; expander should produce a syntax object or a `parsed` structure.

(struct parsed (s) #:authentic #:transparent)

(struct parsed-id parsed (binding inspector) #:authentic)
(struct parsed-primitive-id parsed-id () #:authentic)
(struct parsed-top-id parsed-id () #:authentic)

(struct parsed-lambda parsed (keys body) #:authentic)
(struct parsed-case-lambda parsed (clauses) #:authentic)
(struct parsed-app parsed (rator rands) #:authentic)
(struct parsed-if parsed (tst thn els) #:authentic)
(struct parsed-set! parsed (id rhs) #:authentic)
(struct parsed-with-continuation-mark parsed (key val body) #:authentic)
(struct parsed-#%variable-reference parsed (id) #:authentic)
(struct parsed-begin parsed (body) #:authentic)
(struct parsed-begin0 parsed (body) #:authentic)
(struct parsed-quote parsed (datum) #:authentic)
(struct parsed-quote-syntax parsed (datum) #:authentic)

(struct parsed-let_-values parsed (idss clauses body) #:authentic)
(struct parsed-let-values parsed-let_-values () #:authentic)
(struct parsed-letrec-values parsed-let_-values () #:authentic)

(struct parsed-define-values parsed (ids syms rhs) #:authentic)
(struct parsed-define-syntaxes parsed (ids syms rhs) #:authentic)
(struct parsed-begin-for-syntax parsed (body) #:authentic)

(struct parsed-#%declare parsed () #:authentic)
(struct parsed-require parsed () #:authentic)

(struct parsed-#%module-begin parsed (body) #:authentic)
(struct parsed-module parsed (star?
                              name-id
                              self
                              requires
                              provides
                              root-ctx-simple?
                              encoded-root-ctx
                              body
                              compiled-module       ; #f or already-compiled module
                              compiled-submodules)  ; already-compiled submodules
  #:authentic)
