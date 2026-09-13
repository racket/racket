#lang racket/base
(require ffi2
         rackunit)

(define bstr (make-bytes 32 65))
(define bstr-p (cpointer->ptr_t bstr))
(check-equal? bstr #"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")

(ffi2-set! bstr-p int_t 3 0)
(check-equal? bstr #"AAAAAAAAAAAA\0\0\0\0AAAAAAAAAAAAAAAA")

(ffi2-set! (ffi2-cast bstr #:from bytes_ptr_t #:offset 1 #:to ptr_t) int16_t #x101)

(check-equal? bstr #"A\1\1AAAAAAAAA\0\0\0\0AAAAAAAAAAAAAAAA")

(ffi2-set! (ffi2-cast bstr #:from bytes_ptr_t #:offset 3) int16_t #x202)
(ffi2-set! (ffi2-cast bstr #:from bytes_ptr_t #:offset int16_t 6) int16_t #x303)
(check-equal? bstr #"A\1\1\2\2AAAAAAA\3\3\0\0AAAAAAAAAAAAAAAA")
