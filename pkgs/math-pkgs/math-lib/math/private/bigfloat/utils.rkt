#lang racket/base

(require (for-syntax racket/base racket/syntax syntax/strip-context)
         typed/racket/base
         (only-in ffi/unsafe
                  ctype-sizeof
                  _long
                  _ulong))

(provide (all-defined-out))

(define (unsigned-max type) (- (expt 2 (* 8 (ctype-sizeof type))) 1))
(define (signed-min type) (- (expt 2 (- (* 8 (ctype-sizeof type)) 1))))
(define (signed-max type) (- (expt 2 (- (* 8 (ctype-sizeof type)) 1)) 1))

(define _long-min (signed-min _long))
(define _long-max (signed-max _long))
(define _ulong-max (unsigned-max _ulong))

(define (_ulong? n) (and (exact-integer? n) (<= 0 n _ulong-max)))
(define (_long? n) (and (exact-integer? n) (<= _long-min n _long-max)))

(define-syntax (req/prov-uniform-collection stx)
  (syntax-case stx ()
    [(_ module collection type)
     (with-syntax ([require-it-name  (datum->syntax stx (gensym 'require-it))])
       (syntax/loc stx
         (begin
           (define-syntax (require-it-name stx1)
             (syntax-case stx1 ()
               [(require-it-name)
                (with-syntax ([(obj (... ...))  (replace-context #'require-it-name collection)])
                  #'(begin (require/typed module [obj  type] (... ...))
                           (provide obj (... ...))))]))
           (require-it-name))))]))
