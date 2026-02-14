#lang racket/base

;;----------------------------------------------------------------------
;; Internals needed by the `compatability` package; used to implement
;; `mzlib/a-signature`
;;
;; Note that this module only semi-private, in the sense that `compatability-lib`
;; lives in another repository. This means any backwards-compatable API
;; changes need coordination. Try to avoid adding new exports to this file;
;; instead, consider whether a public API is appropriate.

(require "unit/exptime/util.rkt")
(provide split-requires)
