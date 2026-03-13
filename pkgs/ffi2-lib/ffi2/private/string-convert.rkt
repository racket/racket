#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (only-in '#%foreign
                  memcpy
                  cpointer->ffi2-ptr
                  ffi2-ptr->cpointer
                  ffi2-ptr->uintptr
                  ffi2-uintptr->ptr)
         racket/fixnum)

(provide null-pointer
         null-pointer?
         maybe-bytes->pointer
         maybe-bytes->pointer/add-terminator
         maybe-pointer->bytes
         maybe-string->pointer
         maybe-pointer->string
         maybe-path->pointer
         maybe-pointer->path
         maybe-integer->char)

(define null-pointer (ffi2-uintptr->ptr 0))
(define (null-pointer? ptr) (eqv? 0 (ffi2-ptr->uintptr ptr)))

(define (bytes-add-terminator bstr)
  (define len (bytes-length bstr))
  (define new-bstr (make-bytes (add1 len)))
  (memcpy new-bstr bstr len)
  new-bstr)

(define (maybe-bytes->pointer bstr)
  (if bstr
      (cpointer->ffi2-ptr 'bytes bstr)
      null-pointer))

(define (maybe-bytes->pointer/add-terminator bstr)
  (if bstr
      (cpointer->ffi2-ptr 'bytes (bytes-add-terminator bstr))
      null-pointer))

(define (maybe-pointer->bytes ptr)
  (cond
    [(null-pointer? ptr)
     #f]
    [else
     (define len (let loop ([i 0])
                   (cond
                     [(fx= 0 ((#%foreign-inline (begin-unsafe (ffi2-ptr-ref-maker unsigned-8)) #:copy) ptr i))
                      i]
                     [else (loop (fx+ i 1))])))
     (define bstr (make-bytes len))
     (memcpy bstr (ffi2-ptr->cpointer ptr) len)
     bstr]))

(define (maybe-string->pointer str)
  (if str
      (maybe-bytes->pointer/add-terminator (string->bytes/utf-8 str))
      null-pointer))

(define (maybe-pointer->string ptr)
  (let ([bstr (maybe-pointer->bytes ptr)])
    (and bstr
         (bytes->string/utf-8 bstr))))
  
(define (maybe-path->pointer path)
  (if path
      (maybe-bytes->pointer/add-terminator (path->bytes path))
      null-pointer))

(define (maybe-pointer->path ptr)
  (let ([bstr (maybe-pointer->bytes ptr)])
    (and bstr
         (bytes->path bstr))))

(define (maybe-integer->char c)
  (if (or (c . > . #x10FFFF)
          (<= #xD800 c #xDFFF))
      #\uFFFD
      (integer->char c)))
