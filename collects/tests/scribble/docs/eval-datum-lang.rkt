#lang racket/base

;; This language does not provide a begin form.  Makes sure that @interaction still works
;; on strings and bytes.
(provide (except-out (all-from-out racket/base) begin))