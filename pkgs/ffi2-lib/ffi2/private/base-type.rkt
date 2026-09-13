#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (only-in '#%foreign
                  ffi2-ptr?
                  ffi2-ptr/gcable?
                  ffi2-uintptr->ptr
                  ffi2-ptr->uintptr)
         racket/fixnum
         "string-convert.rkt"
         "base-pred.rkt"
         "type.rkt")

(provide ptr_t?
         ptr_t/gcable?
         void_t*?
         void_t*/gcable?
         uintptr_t->ptr_t
         ptr_t->uintptr_t)

(define-syntax (define-ffi2-base-type stx)
  (syntax-parse stx
    [(_ name arg ...)
     #'(begin
         (define-syntax name (make-ffi2-type 'name arg ...))
         (provide name))]))

(define-ffi2-base-type void_t 'void #'void? #:racket->c #f)
(define-ffi2-base-type int8_t 'integer-8 #'int8? #:category 'scalar)
(define-ffi2-base-type uint8_t 'unsigned-8 #'uint8? #:category 'scalar)
(define-ffi2-base-type byte_t 'unsigned-8 #'byte? #:category 'scalar)
(define-ffi2-base-type int16_t 'integer-16 #'int16? #:category 'scalar)
(define-ffi2-base-type uint16_t 'unsigned-16 #'uint16? #:category 'scalar)
(define-ffi2-base-type int32_t 'integer-32 #'int32? #:category 'scalar)
(define-ffi2-base-type uint32_t 'unsigned-32 #'uint32? #:category 'scalar)
(define-ffi2-base-type int64_t 'integer-64 #'int64? #:category 'scalar)
(define-ffi2-base-type uint64_t 'unsigned-64 #'uint64? #:category 'scalar)
(define-ffi2-base-type short_t 'integer-16 #'int16? #:category 'scalar)
(define-ffi2-base-type ushort_t 'unsigned-16 #'uint16? #:category 'scalar)
(define-ffi2-base-type int_t 'int #'int32? #:category 'scalar)
(define-ffi2-base-type uint_t 'unsigned #'uint32? #:category 'scalar)
(define-ffi2-base-type long_t 'long #'long? #:category 'scalar)
(define-ffi2-base-type ulong_t 'unsigned-long #'ulong? #:category 'scalar)
(define-ffi2-base-type intptr_t 'iptr #'intptr? #:category 'scalar)
(define-ffi2-base-type uintptr_t 'uptr #'uintptr? #:category 'scalar)
(define-ffi2-base-type size_t 'size_t #'size_t? #:category 'scalar)
(define-ffi2-base-type ssize_t 'ssize_t #'ssize_t? #:category 'scalar)
(define-ffi2-base-type float_t 'float #'flonum? #:category 'scalar)
(define-ffi2-base-type double_t 'double #'flonum? #:category 'scalar)
(define-ffi2-base-type intwchar_t 'integer-wchar #'wchar-int? #:category 'scalar)
(define-ffi2-base-type wchar_t 'integer-wchar #'wchar?
  #:racket->c #'char->integer
  #:c->racket #'maybe-integer->char)
(define-ffi2-base-type bool_t 'stdbool #'any? #:category 'scalar)
(define-ffi2-base-type boolint_t 'boolean #'any? #:category 'scalar)
(define-ffi2-base-type ptr_t 'pointer #'ffi2-ptr? #:release #'black-box #:category 'ptr)
(define-ffi2-base-type ptr_t/gcable 'pointer/gc #'ffi2-ptr? #:release #'black-box #:category 'ptr)
(define-ffi2-base-type void_t* 'pointer #'ffi2-ptr? #:release #'black-box #:category 'ptr)
(define-ffi2-base-type void_t*/gcable 'pointer/gc #'ffi2-ptr? #:release #'black-box #:category 'ptr)
(define-ffi2-base-type racket_t 'scheme-object #'any? #:release #'black-box #:category 'racket)
(define-ffi2-base-type string_t 'pointer #'string-or-false? #:release #'black-box
  #:racket->c #'maybe-string->pointer
  #:c->racket #'maybe-pointer->string)
(define-ffi2-base-type string_utf16_t 'pointer #'string-or-false? #:release #'black-box
  #:racket->c #'maybe-string->pointer/utf-16
  #:c->racket #'maybe-pointer->string/utf-16)
(define-ffi2-base-type bytes_t 'pointer #'bytes-or-false? #:release #'black-box
  #:racket->c #'maybe-bytes->pointer/add-terminator
  #:c->racket #'maybe-pointer->bytes)
(define-ffi2-base-type bytes_ptr_t'pointer #'bytes-or-false? #:release #'black-box
  #:racket->c #'maybe-bytes->pointer
  #:c->racket #'maybe-pointer->bytes)
(define-ffi2-base-type path_t 'pointer #'path-or-false? #:release #'black-box
  #:racket->c #'maybe-path->pointer
  #:c->racket #'maybe-pointer->path)

(define (ptr_t? v) (ffi2-ptr? v))
(define (ptr_t/gcable? v) (ffi2-ptr/gcable? v))

(define (void_t*? v) (ffi2-ptr? v))
(define (void_t*/gcable? v) (ffi2-ptr/gcable? v))

(define (uintptr_t->ptr_t v) (ffi2-uintptr->ptr v))
(define (ptr_t->uintptr_t v) (ffi2-ptr->uintptr v))
