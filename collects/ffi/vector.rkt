#lang racket/base

(require "unsafe.rkt"
         racket/unsafe/ops
         (for-syntax racket/base))

(define-syntax define*
  (syntax-rules ()
    [(_ (name . args) body ...)
     (begin (provide name) (define (name . args) body ...))]
    [(_ name expr)
     (begin (provide name) (define name expr))]))

(define-syntax (srfi-4-define/provide stx)
  (syntax-case stx ()
    [(_ TAG type)
     (identifier? #'TAG)
     (let ([name (format "~avector" (syntax->datum #'TAG))])
       (define (id prefix suffix)
         (let* ([name (if prefix (string-append prefix name) name)]
                [name (if suffix (string-append name suffix) name)])
           (datum->syntax #'TAG (string->symbol name) #'TAG)))
       (with-syntax ([TAG?         (id "" "?")]
                     [TAG          (id "" "")]
                     [s:TAG        (id "s:" "")]
                     [make-TAG     (id "make-" "")]
                     [TAG-ptr      (id "" "-ptr")]
                     [TAG-length   (id "" "-length")]
                     [allocate-TAG (id "allocate-" "")]
                     [TAG*         (id "" "*")]
                     [list->TAG    (id "list->" "")]
                     [TAG->list    (id "" "->list")]
                     [TAG-ref      (id "" "-ref")]
                     [TAG-set!     (id "" "-set!")]
                     [TAG->cpointer (id "" "->cpointer")]
                     [_TAG         (id "_" "")]
                     [_TAG*        (id "_" "*")]
                     [TAGname      name]
                     [f64?         (if (eq? (syntax-e #'TAG) 'f64) #'#t #'#f)]
                     [s16?         (if (eq? (syntax-e #'TAG) 's16) #'#t #'#f)]
                     [u16?         (if (eq? (syntax-e #'TAG) 'u16) #'#t #'#f)])
         #'(begin
             (define-struct TAG (ptr length))
             (provide TAG? TAG-length (rename-out [TAG s:TAG]))
             (provide (rename-out [allocate-TAG make-TAG]))
             (define (allocate-TAG n . init)
               (let* ([p (if (eq? n 0) #f (malloc n type))]
                      [v (make-TAG p n)])
                 (when (and p (pair? init))
                   (let ([init (car init)])
                     (let loop ([i (sub1 n)])
                       (unless (< i 0)
                         (ptr-set! p type i init)
                         (loop (sub1 i))))))
                 v))
             (provide (rename-out [TAG* TAG]))
             (define (TAG* . vals)
               (list->TAG vals))
             (define* (TAG-ref v i)
               (if (TAG? v)
                   (if (and (exact-nonnegative-integer? i) (< i (TAG-length v)))
                       ;; use JIT-inlined operation if available:
                       (cond
                        [f64? (unsafe-f64vector-ref v i)]
                        [s16? (unsafe-s16vector-ref v i)]
                        [u16? (unsafe-u16vector-ref v i)]
                        [else (ptr-ref (TAG-ptr v) type i)])
                       (error 'TAG-ref "bad index ~e for ~a bounds of 0..~e"
                              i 'TAG (sub1 (TAG-length v))))
                   (raise-type-error 'TAG-ref TAGname v)))
             (define* (TAG-set! v i x)
               (if (TAG? v)
                   (if (and (exact-nonnegative-integer? i) (< i (TAG-length v)))
                       ;; use JIT-inlined operation if available:
                       (cond
                        [(and f64? (inexact-real? x))
                         (unsafe-f64vector-set! v i x)]
                        [(and s16? (fixnum? x) (unsafe-fx<= -32768 x) (unsafe-fx<= x 32767))
                         (unsafe-s16vector-set! v i x)]
                        [(and u16? (fixnum? x) (unsafe-fx<= 0 x) (unsafe-fx<= x 65535))
                         (unsafe-u16vector-set! v i x)]
                        [else
                         (ptr-set! (TAG-ptr v) type i x)])
                       (error 'TAG-set! "bad index ~e for ~a bounds of 0..~e"
                              i 'TAG (sub1 (TAG-length v))))
                   (raise-type-error 'TAG-set! TAGname v)))
             (define* (TAG->list v)
               (if (TAG? v)
                   (cblock->list (TAG-ptr v) type (TAG-length v))
                   (raise-type-error 'TAG->list TAGname v)))
             (define* (list->TAG l)
               (make-TAG (list->cblock l type) (length l)))
             (define* (TAG->cpointer v)
               (if (TAG? v)
                   (TAG-ptr v)
                   (raise-type-error 'TAG->cpointer TAGname v)))
             ;; same as the _cvector implementation
             (provide _TAG)
             (define _TAG*
               (make-ctype _pointer TAG-ptr
                           (lambda (x)
                             (error
                              '_TAG
                              "cannot automatically convert a C pointer to a ~a"
                              TAGname))))
             (define-fun-syntax _TAG
               (syntax-id-rules (i o io)
                 [(_ i   ) _TAG*]
                 [(_ o  n) (type: _pointer
                                  pre:  (malloc n type)
                                  post: (x => (make-TAG x n)))]
                 [(_ io  ) (type: _cvector*
                                  bind: tmp
                                  pre:  (x => (TAG-ptr x))
                                  post: (x => tmp))]
                 [(_ . xs)   (_TAG* . xs)]
                 [_          _TAG*])))))]
    [(_ TAG type)
     (identifier? #'TAG)]))

;; check that the types that were used above have the proper sizes
(unless (= 4 (ctype-sizeof _float))
  (error 'foreign "internal error: float has a bad size (~s)"
         (ctype-sizeof _float)))
(unless (= 8 (ctype-sizeof _double*))
  (error 'foreign "internal error: double has a bad size (~s)"
         (ctype-sizeof _double*)))

(srfi-4-define/provide s8  _int8)
(srfi-4-define/provide s16 _int16)
(srfi-4-define/provide u16 _uint16)
(srfi-4-define/provide s32 _int32)
(srfi-4-define/provide u32 _uint32)
(srfi-4-define/provide s64 _int64)
(srfi-4-define/provide u64 _uint64)
(srfi-4-define/provide f32 _float)
(srfi-4-define/provide f64 _double*)

;; simply rename bytes* to implement the u8vector type
(provide (rename-out [bytes?       u8vector?      ]
                     [bytes-length u8vector-length]
                     [make-bytes   make-u8vector  ]
                     [bytes        u8vector       ]
                     [bytes-ref    u8vector-ref   ]
                     [bytes-set!   u8vector-set!  ]
                     [bytes->list  u8vector->list ]
                     [list->bytes  list->u8vector ]
                     [_bytes       _u8vector      ]))

(define (u8vector->cpointer v) 
  (unless (bytes? v)
    (raise-type-error 'u8vector->cpointer "byte string" v))
  v)
(provide u8vector->cpointer)

;; additional `u8vector' bindings for srfi-66
(provide (rename-out [bytes-copy u8vector-copy] [bytes=? u8vector=?]))
(define* (u8vector-compare v1 v2)
  (cond [(bytes<? v1 v2) -1]
        [(bytes>? v1 v2)  1]
        [else             0]))
(define* (u8vector-copy! src src-start dest dest-start n)
  (bytes-copy! dest dest-start src src-start (+ src-start n)))
