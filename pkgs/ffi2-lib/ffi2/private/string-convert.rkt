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
         maybe-integer->char
         maybe-string->pointer/utf-16
         maybe-pointer->string/utf-16)

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

(define (maybe-string->pointer/utf-16 s)
  (cond
    [s
     (define surrogate-count
       (for/fold ([n 0]) ([c (in-string s)])
         (if ((char->integer c) . fx>= . #x10000)
             (fx+ n 1)
             n)))<
     (define bstr (make-bytes (fx* 2 (fx+ (string-length s) surrogate-count))))
     (for/fold ([pos 0]) ([c (in-string s)])
       (define v (char->integer c))
       (cond
         [(v . fx>= . #x10000)
          (define av (fx- v #x10000))
          (define hi (fxior #xD800 (fxand (fxrshift av 10) #x3FF)))
          (define lo (fxior #xDC00 (fxand av #x3FF)))
          (bytes-set-two! bstr pos (fxrshift hi 8) (fxand hi #xFF))
          (bytes-set-two! bstr (+ pos 2) (fxrshift lo 8) (fxand lo #xFF))
          (fx+ pos 4)]
         [else
          (bytes-set-two! bstr pos (fxrshift v 8) (fxand v #xFF))
          (fx+ pos 2)]))
     (cpointer->ffi2-ptr 'bytes bstr)]
    [else null-pointer]))

(define (maybe-pointer->string/utf-16 ptr)
  (cond
    [(null-pointer? ptr)
     #f]
    [else
     (define bstr (pointer->bytes/utf-16 ptr))
     (define len (bytes-length bstr))
     (define surrogate-count
       (if (fx= len 0)
           0
           (for/fold ([n 0]) ([b (in-bytes bstr (if big-endian? 0 1) len 2)])
             (if (fx= (fxand b #xDC) #xD8)
                 (fx+ n 1)
                 n))))
     (define str (make-string (fx- (fxrshift len 1) surrogate-count)))
     (let loop ([i 0] [pos 0])
       (unless (fx= i len)
         (define a (bytes-ref bstr i))
         (define b (bytes-ref bstr (fx+ i 1)))
         (define v (if big-endian?
                       (fxior (fxlshift a 8) b)
                       (fxior (fxlshift b 8) a)))
         (cond
           [(fx= (fxand v #xDC00) #xD800)
            ;; surrogate pair
            (define a (bytes-ref bstr (fx+ i 2)))
            (define b (bytes-ref bstr (fx+ i 3)))
            (define v2 (if big-endian?
                           (fxior (fxlshift a 8) b)
                           (fxior (fxlshift b 8) a)))
            (define all-v (fx+ #x10000
                               (fxior (fxlshift (fxand v #x3FF) 10)
                                      (fxand v2 #x3FF))))
            (string-set! str pos (integer->char all-v))
            (loop (fx+ i 4) (fx+ pos 1))]
           [else
            (string-set! str pos (integer->char v))
            (loop (fx+ i 2) (fx+ pos 1))])))
     str]))

(define big-endian? (system-big-endian?))
(define (bytes-set-two! out-bstr j hi lo)
  (cond
    [big-endian?
     (bytes-set! out-bstr j hi)
     (bytes-set! out-bstr (+ j 1) lo)]
    [else
     (bytes-set! out-bstr j lo)
     (bytes-set! out-bstr (+ j 1) hi)]))

(define (pointer->bytes/utf-16 ptr)
  (define len (let loop ([i 0])
                (cond
                  [(and (fx= 0 ((#%foreign-inline (begin-unsafe (ffi2-ptr-ref-maker unsigned-8)) #:copy) ptr i))
                        (fx= 0 ((#%foreign-inline (begin-unsafe (ffi2-ptr-ref-maker unsigned-8)) #:copy) ptr (fx+ i 1))))
                   i]
                  [else (loop (fx+ i 2))])))
  (define bstr (make-bytes len))
  (memcpy bstr (ffi2-ptr->cpointer ptr) len)
  bstr)
