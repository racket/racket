#lang racket/base

;; kwd : keyword?
;; ctc : contract?
;; mandatory? : boolean?
(define-struct kwd-info (kwd ctc mandatory?) #:transparent)

(provide (struct-out kwd-info))