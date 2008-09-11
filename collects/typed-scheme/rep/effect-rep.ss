#lang scheme/base

(require mzlib/plt-match)
(require mzlib/etc)
(require "rep-utils.ss" "free-variance.ss")

(de True-Effect () [#:frees #f] [#:fold-rhs #:base])

(de False-Effect () [#:frees #f] [#:fold-rhs #:base])

;; v is an identifier
(de Var-True-Effect (v) [#:intern (hash-id v)] [#:frees #f] [#:fold-rhs #:base])

;; v is an identifier
(de Var-False-Effect (v) [#:intern (hash-id v)] [#:frees #f] [#:fold-rhs #:base])

;; t is a Type
;; v is an identifier
(de Restrict-Effect (t v) [#:intern (list t (hash-id v))] [#:frees (free-vars* t) (free-idxs* t)]
    [#:fold-rhs (*Restrict-Effect (type-rec-id t) v)])

;; t is a Type
;; v is an identifier
(de Remove-Effect (t v) 
    [#:intern (list t (hash-id v))]
    [#:frees (free-vars* t) (free-idxs* t)]
    [#:fold-rhs (*Remove-Effect (type-rec-id t) v)])

;; t is a Type
(de Latent-Restrict-Effect (t))

;; t is a Type
(de Latent-Remove-Effect (t))

(de Latent-Var-True-Effect () [#:frees #f] [#:fold-rhs #:base])

(de Latent-Var-False-Effect () [#:frees #f] [#:fold-rhs #:base])

;; could also have latent true/false effects, but seems pointless
