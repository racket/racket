#lang scheme/base

(provide (all-defined-out))

;; use this to communicate the frame being
;; syntax checked w/out having to add new
;; parameters to all of the functions
(define currently-processing-drscheme-frame (make-parameter #f))