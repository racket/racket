#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/fixnum
         racket/symbol)

(define-syntax (define-predicate stx)
  (syntax-parse stx
    [(_ (? arg) body)
     #'(begin
         (provide ?)
         (define-syntax-rule (? arg-expr)
           (let ([arg arg-expr]) body)))]))

(define-predicate (int8? v) (and (fixnum? v) (fx<= -128 v 127)))
(define-predicate (uint8? v) (byte? v))
(define-predicate (int16? v) (and (fixnum? v) (fx<= #x-8000 v #x7FFF)))
(define-predicate (uint16? v) (and (fixnum? v) (fx<= 0 v #xFFFF)))
(define-predicate (int32? v) (and (exact-integer? v) (<= #x-80000000 v #x7FFFFFFF)))
(define-predicate (uint32? v) (and (exact-integer? v) (<= 0 v #xFFFFFFFF)))
(define-predicate (int64? v) (and (exact-integer? v) (<= #x-8000000000000000 v #x7FFFFFFFFFFFFFFF)))
(define-predicate (uint64? v) (and (exact-integer? v) (<= 0 v #xFFFFFFFFFFFFFFFF)))
(define-predicate (any? v) #t)
(define-predicate (string-or-false? s) (or (not s) (string? s)))
(define-predicate (bytes-or-false? s) (or (not s) (bytes? s)))
(define-predicate (path-or-false? s) (or (not s) (path-for-some-system? s)))
(define-predicate (long? v) (if (fx= 4 (#%foreign-inline (ffi2-sizeof long) #:copy))
                                (int32? v)
                                (int64? v)))
(define-predicate (ulong? v) (if (fx= 4 (#%foreign-inline (ffi2-sizeof long) #:copy))
                                 (uint32? v)
                                 (uint64? v)))
(define-predicate (intptr? v) (if (fx= 4 (#%foreign-inline (ffi2-sizeof iptr) #:copy))
                                  (int32? v)
                                  (int64? v)))
(define-predicate (uintptr? v) (if (fx= 4 (#%foreign-inline (ffi2-sizeof uptr) #:copy))
                                   (uint32? v)
                                   (uint64? v)))
(define-predicate (size_t? v) (if (fx= 4 (#%foreign-inline (ffi2-sizeof size_t) #:copy))
                                  (uint32? v)
                                  (uint64? v)))
(define-predicate (ssize_t? v) (if (fx= 4 (#%foreign-inline (ffi2-sizeof size_t) #:copy))
                                   (int32? v)
                                   (int64? v)))
(define-predicate (wchar-int? v) (cond
                                   [(fx= 1 (#%foreign-inline (ffi2-sizeof integer-wchar) #:copy))
                                    (uint8? v)]
                                   [(fx= 2 (#%foreign-inline (ffi2-sizeof integer-wchar) #:copy))
                                    (uint16? v)]
                                   [(fx= 4 (#%foreign-inline (ffi2-sizeof integer-wchar) #:copy))
                                    (uint32? v)]
                                   [else
                                    (uint64? v)]))
(define-predicate (wchar? v) (and (char? v) (wchar-int? (char->integer v))))
