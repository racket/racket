#lang racket/base
(require "../unsafe.rkt")

(define-struct cvector (ptr type length))

(provide cvector? cvector-length cvector-type cvector-ptr
         ;; make-cvector* is a dangerous operation
         (protect-out (rename-out [make-cvector make-cvector*])))

(define-syntax define*
  (syntax-rules ()
    [(_ (name . args) body ...)
     (begin (provide name) (define (name . args) body ...))]
    [(_ name expr)
     (begin (provide name) (define name expr))]))

(define _cvector* ; used only as input types
  (make-ctype _pointer cvector-ptr
    (lambda (x)
      (error '_cvector
             "cannot automatically convert a C pointer to a cvector"))))

;; (_cvector <mode> [<type> <len>]) | _cevector
;; Same as _list etc above, except that it uses C vectors.
(provide _cvector)
(define-fun-syntax _cvector
  (syntax-id-rules (i o io)
    [(_ i     ) _cvector*]
    [(_ o  t n) (type: _pointer ; needs to be a pointer, not a cvector*
                 pre:  (malloc n t)
                 post: (x => (make-cvector x t n)))]
    [(_ io    ) (type: _cvector*
                 bind: tmp
                 pre:  (x => (cvector-ptr x))
                 post: (x => tmp))]
    [(_ . xs)   (_cvector* . xs)]
    [_          _cvector*]))

(provide (rename-out [allocate-cvector make-cvector]))
(define (allocate-cvector type len)
  (make-cvector (if (zero? len) #f ; 0 => NULL
                    (malloc len type))
                type len))

(provide (rename-out [cvector-args cvector]))
(define (cvector-args type . args)
  (list->cvector args type))

(define* (cvector-ref v i)
  (if (and (exact-nonnegative-integer? i) (< i (cvector-length v)))
    (ptr-ref (cvector-ptr v) (cvector-type v) i)
    (error 'cvector-ref "bad index ~e for cvector bounds of 0..~e"
           i (sub1 (cvector-length v)))))

(define* (cvector-set! v i x)
  (if (and (exact-nonnegative-integer? i) (< i (cvector-length v)))
    (ptr-set! (cvector-ptr v) (cvector-type v) i x)
    (error 'cvector-ref "bad index ~e for cvector bounds of 0..~e"
           i (sub1 (cvector-length v)))))

(define* (cvector->list v)
  (cblock->list (cvector-ptr v) (cvector-type v) (cvector-length v)))

(define* (list->cvector l type)
  (make-cvector (list->cblock l type) type (length l)))

