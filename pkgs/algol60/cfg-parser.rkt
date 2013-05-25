#lang racket/base

;; Legacy module.  cfg-parser used to live in the algol60 collection.
;; This module re-exports its contents for backwards compatibility.

(require parser-tools/cfg-parser)
(provide (all-from-out parser-tools/cfg-parser))
