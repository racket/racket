#lang racket/base
(require "match.rkt"
         "wrap.rkt")

(provide inline-ptr-ref
         inline-ptr-set)

(define (inline-ptr-ref args)
  (match args
    [`(,ptr-e ,type-e (quote abs) ,offset-e)
     (type->direct type-e ptr-e offset-e #t make-ref #f)]
    [`(,ptr-e ,type-e ,offset-e)
     (type->direct type-e ptr-e offset-e #f make-ref #f)]
    [`(,ptr-e ,type-e)
     (type->direct type-e ptr-e 0 #f make-ref #f)]
    [`,_ #f]))

(define (make-ref ref set ptr-e offset-e val-e abs?)
  `(,ref ,ptr-e ,offset-e ,abs?))

(define (inline-ptr-set args)
  (match args
    [`(,ptr-e ,type-e (quote abs) ,offset-e ,val-e)
     (type->direct type-e ptr-e offset-e #t make-set val-e)]
    [`(,ptr-e ,type-e ,offset-e, val-e)
     (type->direct type-e ptr-e offset-e #f make-set val-e)]
    [`(,ptr-e ,type-e ,val-e)
     (type->direct type-e ptr-e 0 #f make-set val-e)]
    [`,_ #f]))

(define (make-set ref set ptr-e offset-e val-e abs?)
  `(,set ,ptr-e ,offset-e ,val-e ,abs?))

(define (type->direct type-e ptr-e offset-e abs? make val-e)
  (define (do-make ref set)
    (make ref set ptr-e offset-e val-e abs?))
  (case (unwrap type-e)
    [(_int8) (do-make 'ptr-ref/int8 'ptr-set!/int8)]
    [(_uint8) (do-make 'ptr-ref/uint8 'ptr-set!/uint8)]
    [(_int16) (do-make 'ptr-ref/int16 'ptr-set!/int16)]
    [(_uint16) (do-make 'ptr-ref/uint16 'ptr-set!/uint16)]
    [(_int32) (do-make 'ptr-ref/int32 'ptr-set!/int32)]
    [(_uint32) (do-make 'ptr-ref/uint32 'ptr-set!/uint32)]
    [(_int64) (do-make 'ptr-ref/int64 'ptr-set!/int64)]
    [(_uint64) (do-make 'ptr-ref/uint64 'ptr-set!/uint64)]
    [(_double) (do-make 'ptr-ref/double 'ptr-set!/double)]
    [(_float) (do-make 'ptr-ref/float 'ptr-set!/float)]
    [else #f]))
