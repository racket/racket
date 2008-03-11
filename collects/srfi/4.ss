#lang scheme/base

(require scheme/require)

;; Note: this also gets additional functions for srfi-66 (u8vector-copy,
;; u8vector=?, u8vector-compare, u8vector-copy!) -- but that should be fine,
;; just like a future extension of the srfi-4 stuff to provide these for all
;; vector types.
(require (matching-identifiers-in #px"\\b_?[suf](8|16|32|64)vector\\b"
                                  scheme/foreign))
(provide (all-from-out scheme/foreign))
