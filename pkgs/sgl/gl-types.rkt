#lang racket/base
(require ffi/unsafe
         ffi/cvector)

(provide (all-defined-out))

(define _float*
  (make-ctype _float 
              (lambda (n)
                (if (exact? n)
                    (exact->inexact n)
                    n))
              #f))

(define (make-gl-vector-type t)
  (make-ctype _cvector
              (lambda (sval)
                (unless (cvector? sval)
                  (raise-type-error 'Scheme->C "cvector" sval))
                (unless (eq? (cvector-type sval) t)
                  (error 'Scheme->C "wrong kind of cvector"))
                sval)
              #f))

;; Beware of problems with these type definitions.
;; They seem to be right for all currently supported
;; platforms, but in principle they can differ.

(define _gl-byte _int8)
(define _gl-ubyte _uint8)
(define _gl-short _int16)
(define _gl-ushort _uint16)
(define _gl-int _int)
(define _gl-uint _uint)
(define _gl-boolean (make-ctype _int8
                                (lambda (x)
                                  (if x 1 0))
                                (lambda (x) (not (= x 0)))))
(define _gl-sizei _int)
(define _gl-enum _int)
(define _gl-bitfield _uint)
(define _gl-float _float*)
(define _gl-double _double*)
(define _gl-clampf _float*)
(define _gl-clampd _double*)

(define _gl-bytev (make-gl-vector-type _gl-byte))
(define _gl-ubytev (make-gl-vector-type _gl-ubyte))
(define _gl-shortv (make-gl-vector-type _gl-short))
(define _gl-ushortv (make-gl-vector-type _gl-ushort))
(define _gl-intv (make-gl-vector-type _gl-int))
(define _gl-uintv (make-gl-vector-type _gl-uint))
(define _gl-booleanv (make-gl-vector-type _gl-boolean))
(define _gl-floatv (make-gl-vector-type _gl-float))
(define _gl-doublev (make-gl-vector-type _gl-double))
(define _gl-voidv _cvector)

