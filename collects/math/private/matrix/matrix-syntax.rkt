#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         (only-in typed/racket/base :)
         "../array/array-struct.rkt")

(provide matrix row-matrix col-matrix)

(define-syntax (matrix stx)
  (syntax-parse stx #:literals (:)
    [(_ [[x0 xs0 ...] [x xs ...] ...])
     (syntax/loc stx (array #[#[x0 xs0 ...] #[x xs ...] ...]))]
    [(_ [[x0 xs0 ...] [x xs ...] ...] : T)
     (syntax/loc stx (array #[#[x0 xs0 ...] #[x xs ...] ...] : T))]
    [(_ [xs ... (~and [] r) ys ...] (~optional (~seq : T)))
     (raise-syntax-error 'matrix "given empty row" stx #'r)]
    [(_ (~and [] c) (~optional (~seq : T)))
     (raise-syntax-error 'matrix "given empty matrix" stx #'c)]
    [(_ x (~optional (~seq : T)))
     (raise-syntax-error 'matrix "expected two-dimensional data" stx)]))

(define-syntax (row-matrix stx)
  (syntax-parse stx #:literals (:)
    [(_ [x xs ...])      (syntax/loc stx (array #[#[x xs ...]]))]
    [(_ [x xs ...] : T)  (syntax/loc stx (array #[#[x xs ...]] : T))]
    [(_ (~and [] r) (~optional (~seq : T)))
     (raise-syntax-error 'row-matrix "given empty row" stx #'r)]))

(define-syntax (col-matrix stx)
  (syntax-parse stx #:literals (:)
    [(_ [x xs ...])      (syntax/loc stx (array #[#[x] #[xs] ...]))]
    [(_ [x xs ...] : T)  (syntax/loc stx (array #[#[x] #[xs] ...] : T))]
    [(_ (~and [] c) (~optional (~seq : T)))
     (raise-syntax-error 'col-matrix "given empty column" stx #'c)]))
