#lang scheme/base

(require (for-syntax scheme/base))

(require (file "foreign.ss"))



(unsafe!)

(define-syntax (define/provide-srfi-4 stx)
  (define (make-TAG-id prefix name suffix)
    (datum->syntax stx
                   (string->symbol
                    (string-append prefix name suffix))
                   stx))
  (syntax-case stx ()
    [(_ TAG type)
     (identifier? #'TAG)
     (let ([name (string-append
                  (symbol->string (syntax->datum #'TAG))
                  "vector")])
       (with-syntax ([TAG?         (make-TAG-id "" name "?")]
                     [TAG          (make-TAG-id "" name "")]
                     [s:TAG        (make-TAG-id "s:" name "")]
                     [make-TAG     (make-TAG-id "make-" name "")]
                     [TAG-ptr      (make-TAG-id "" name "-ptr")]
                     [TAG-length   (make-TAG-id "" name "-length")]
                     [allocate-TAG (make-TAG-id "allocate-" name "")]
                     [TAG*         (make-TAG-id "" name "*")]
                     [list->TAG    (make-TAG-id "list->" name "")]
                     [TAG->list    (make-TAG-id "" name "->list")]
                     [TAG-ref      (make-TAG-id "" name "-ref")]
                     [TAG-set!     (make-TAG-id "" name "-set!")]
                     [_TAG         (make-TAG-id "_" name "")]
                     [_TAG*        (make-TAG-id "_" name "*")]
                     [TAGname      name])
         #'(begin
             (define-struct TAG (ptr length))
             (provide (rename-out [TAG s:TAG]))
             (provide TAG? TAG-length)
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
             (define (TAG-ref v i)
               (if (TAG? v)
                   (if (and (integer? i) (< -1 i (TAG-length v)))
                       (ptr-ref (TAG-ptr v) type i)
                       (error 'TAG-ref "bad index ~e for ~a bounds of 0..~e"
                              i 'TAG (sub1 (TAG-length v))))
                   (raise-type-error 'TAG-ref TAGname v)))
             (provide TAG-ref)
             (define (TAG-set! v i x)
               (if (TAG? v)
                   (if (and (integer? i) (< -1 i (TAG-length v)))
                       (ptr-set! (TAG-ptr v) type i x)
                       (error 'TAG-set! "bad index ~e for ~a bounds of 0..~e"
                              i 'TAG (sub1 (TAG-length v))))
                   (raise-type-error 'TAG-set! TAGname v)))
             (provide TAG-set!)
             (define (TAG->list v)
               (if (TAG? v)
                   (cblock->list (TAG-ptr v) type (TAG-length v))
                   (raise-type-error 'TAG->list TAGname v)))
             (provide TAG->list)
             (define (list->TAG l)
               (make-TAG (list->cblock l type) (length l)))
             (provide list->TAG)
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
                 [_          _TAG*]))
             )))]
    ))


;; check that the types that were used above have the proper sizes
(unless (= 4 (ctype-sizeof _float))
  (error 'foreign "internal error: float has a bad size (~s)"
         (ctype-sizeof _float)))
(unless (= 8 (ctype-sizeof _double*))
  (error 'foreign "internal error: double has a bad size (~s)"
         (ctype-sizeof _double*)))


(define/provide-srfi-4 s8  _int8)
(define/provide-srfi-4 s16 _int16)
(define/provide-srfi-4 u16 _uint16)
(define/provide-srfi-4 s32 _int32)
(define/provide-srfi-4 u32 _uint32)
(define/provide-srfi-4 s64 _int64)
(define/provide-srfi-4 u64 _uint64)
(define/provide-srfi-4 f32 _float)
(define/provide-srfi-4 f64 _double*)

;; We simply rename bytes to implement the u8vector type
(provide (rename-out [bytes?       u8vector?      ]
                     [bytes-length u8vector-length]
                     [make-bytes   make-u8vector  ]
                     [bytes        u8vector       ]
                     [bytes-ref    u8vector-ref   ]
                     [bytes-set!   u8vector-set!  ]
                     [bytes->list  u8vector->list ]
                     [list->bytes  list->u8vector ]
                     [_bytes       _uint8   ]))