;;----------------------------------------------------------------------
;; Internals needed by the `compatibility-lib` package; primarily used
;; to implement `mzscheme`
;;
;; Note that this module only semi-private, in the sense that `compatibility-lib`
;; lives in another repository. This means any backwards-compatible API
;; changes need coordination. Try to avoid adding new exports to this file;
;; instead, consider whether a public API is appropriate.

(module for-compatibility-lib '#%kernel
  (#%require "more-scheme.rkt"
             "cond.rkt"
             "define.rkt"
             "stx.rkt")
  (#%provide define define-syntax define-for-syntax
             old-cond old-case
             fluid-let
             stx-pair? stx-car stx-cdr))
