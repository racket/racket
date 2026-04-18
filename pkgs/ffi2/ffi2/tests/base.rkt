#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         ffi2
         rackunit)

(define-syntax (check-base stx)
  (syntax-parse stx
    [(_ [type_t #:good (good-val ...)
                #:bad (bad-val ...)                
                (~optional kind
                           #:defaults ([kind #'#:set!/ref]))]
        ...)
     #'(begin
         (let ([p (if (eq? 'kind '#:copy)
                      (ffi2-malloc #:gcable-traced type_t)
                      (ffi2-malloc type_t))])
           (define (is-a? v)
             (ffi2-is-a? v type_t))
           (define (set v)
             (ffi2-set! p type_t v)
             (when (eq? 'kind '#:copy) (copy-content-to-immobile p)))
           (define (ref) (ffi2-ref p type_t))
           (check-true (is-a? good-val))
           ...
           (begin
             (set good-val)
             (check-equal? (ref) good-val))
           ...
           (check-false (is-a? bad-val))
           ...
           (check-exn exn:fail:contract? (lambda () (set bad-val)))
           ...
           (void (black-box p)))
         ...)]))

(define (copy-content-to-immobile p)
  ;; simulate a foreign function that keeps a copy of a string
  (define sp (ffi2-ref p ptr_t/gcable))
  (define bstr (ffi2-cast sp #:to bytes_t))
  (when bstr
    (define len (bytes-length bstr))
    (define new-sp (ffi2-malloc #:gcable-immobile (add1 (bytes-length bstr))))
    (ffi2-memcpy new-sp (ffi2-cast bstr #:from bytes_ptr_t #:to ptr_t) len)
    (ffi2-set! new-sp byte_t len 0)
    (ffi2-set! p ptr_t new-sp)))

(check-base
 [int8_t #:good (-128 0 127) #:bad (-129 255 1.0 "oops")]
 [uint8_t #:good (0 127 255) #:bad (-1 256 1.0 "oops")]
 [int16_t #:good (#x-8000 0 #x7FFF) #:bad (#x-8001 #x8000 1.0 "oops")]
 [uint16_t #:good (0 #x7FFF #xFFFF) #:bad (-1 #x10000 1.0 "oops")]
 [int32_t #:good (#x-80000000 0 #x7FFFFFFF) #:bad (#x-80000001 #x80000000 1.0 "oops")]
 [uint32_t #:good (0 #x7FFFFFFF #xFFFFFFFF) #:bad (-1 #x100000000 1.0 "oops")]
 [int64_t #:good (#x-8000000000000000 0 #x7FFFFFFFFFFFFFFF) #:bad (#x-8000000000000001 #x8000000000000000 1.0 "oops")]
 [uint64_t #:good (0 #x7FFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF) #:bad (-1 #x10000000000000000 1.0 "oops")]
 [byte_t #:good (0 127 255) #:bad (-1 256 1.0 "oops")]
 [short_t #:good (#x-8000 0 #x7FFF) #:bad (#x-8001 #x8000 1.0 "oops")]
 [ushort_t #:good (0 #x7FFF #xFFFF) #:bad (-1 #x10000 1.0 "oops")]
 [int_t #:good (#x-80000000 0 #x7FFFFFFF) #:bad (#x-80000001 #x80000000 1.0 "oops")]
 [uint_t #:good (0 #x7FFFFFFF #xFFFFFFFF) #:bad (-1 #x100000000 1.0 "oops")]
 [long_t #:good (-256 0 256) #:bad (#x-8000000000000001 #x8000000000000000 1.0 "oops")]
 [ulong_t #:good (0 256) #:bad (-1 #x10000000000000000 1.0 "oops")]
 [intptr_t #:good (-256 0 256) #:bad (#x-8000000000000001 #x8000000000000000 1.0 "oops")]
 [uintptr_t #:good (0 256) #:bad (-1 #x10000000000000000 1.0 "oops")]
 [size_t #:good (0 256) #:bad (-1 #x10000000000000000 1.0 "oops")]
 [ssize_t #:good (-256 0 256) #:bad (#x-8000000000000001 #x8000000000000000 1.0 "oops")]
 [float_t #:good (-1.0 1.0 +inf.0) #:bad (1 1/2 "oops")]
 [double_t #:good (-1.0 1.0 +inf.0) #:bad (1 1/2 "oops")]
 [intwchar_t #:good (0 255) #:bad (-1 1.0 "oops")]
 [wchar_t #:good (#\a #\!) #:bad (65 1.0 "oops")]
 [bool_t #:good (#t #f) #:bad ()]
 [boolint_t #:good (#t #f) #:bad ()]
 [string_t #:good ("apple" #f) #:bad (1 'apple #"apple") #:copy]
 [bytes_t #:good (#"apple" #f) #:bad (1 'apple "apple") #:copy]
 [bytes_ptr_t #:good (#f) #:bad (1 'apple "apple") #:copy]
 [path_t #:good (#f) #:bad (1 'apple "apple") #:copy])

(check-equal? (ffi2-sizeof int_t) 4)
(check-equal? (ffi2-sizeof float_t) 4)
(check-equal? (ffi2-sizeof double_t) 8)
(check-equal? (ffi2-sizeof intptr_t) (ffi2-sizeof ptr_t))
(check-equal? (ffi2-sizeof uintptr_t) (ffi2-sizeof ptr_t))

(let ()
  (define p (ffi2-malloc 16))
  (ffi2-set! p bool_t 'ok)
  (check-equal? (ffi2-ref p bool_t) #t)
  (ffi2-set! p boolint_t 'ok)
  (check-equal? (ffi2-ref p boolint_t) #t))

(let ()
  (define p (ffi2-malloc #:gcable-traced 16))
  (define path (bytes->path #"apple"))
  (ffi2-set! p path_t path)
  (copy-content-to-immobile p)
  (check-equal? (ffi2-ref p path_t) path)
  (void (black-box p)))

(let ()
  (define p (ffi2-malloc #:gcable-traced 16))
  (define bstr #"apple\0pie")
  (ffi2-set! p bytes_ptr_t bstr)
  (check-equal? (ffi2-cast (ffi2-ref p ptr_t/gcable) #:to bytes_t) #"apple")
  (define ptr (ffi2-cast (ffi2-ref p ptr_t/gcable) #:offset 6))
  (check-equal? (integer->char (ffi2-ref ptr byte_t 0)) #\p)
  (check-equal? (integer->char (ffi2-ref ptr byte_t 1)) #\i)
  (check-equal? (integer->char (ffi2-ref ptr byte_t 2)) #\e))

(check-equal? (ffi2-cast (uintptr_t->ptr_t 0) #:to bytes_t) #false)
(check-equal? (ffi2-cast (uintptr_t->ptr_t 0) #:to bytes_ptr_t) #false)
(check-equal? (ffi2-cast (uintptr_t->ptr_t 0) #:to string_t) #false)
(check-equal? (ffi2-cast (uintptr_t->ptr_t 0) #:to path_t) #false)

(check-equal? (ptr_t->uintptr_t (ffi2-cast #false #:from bytes_t)) 0)
(check-equal? (ptr_t->uintptr_t (ffi2-cast #false #:from bytes_ptr_t)) 0)
(check-equal? (ptr_t->uintptr_t (ffi2-cast #false #:from string_t)) 0)
(check-equal? (ptr_t->uintptr_t (ffi2-cast #false #:from path_t)) 0)

(let ()
  (define p (ffi2-cast "apple" #:from string_utf16_t))
  (check-equal? (ffi2-ref p short_t) (char->integer #\a))
  (check-equal? (ffi2-ref p short_t 4) (char->integer #\e))
  (check-equal? (ffi2-ref p short_t 5) 0)
  (check-equal? "apple" (ffi2-cast p #:to string_utf16_t)))
