#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         "once.rkt")

(provide (protect-out define-mz))

(define-ffi-definer define-mz #f)
