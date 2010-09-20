#lang racket/base
(require ffi/unsafe
	 ffi/unsafe/define
         "../common/utils.rkt")

(provide define-user32
	 define-kernel32
	 define-comctl32
         define-mz)

(define user32-lib (ffi-lib "user32.dll"))
(define kernel32-lib (ffi-lib "kernel32.dll"))
(define comctl32-lib (ffi-lib "comctl32.dll"))

(define-ffi-definer define-user32 user32-lib)
(define-ffi-definer define-kernel32 kernel32-lib)
(define-ffi-definer define-comctl32 comctl32-lib)
