#lang racket/base
;; This file is for the abbreviations need to implement union.rkt

(require "../utils/utils.rkt")

(require (rep type-rep)
         racket/match racket/list
         (for-template racket/base))

(provide (all-defined-out)
         (rename-out [make-Listof -lst]))

;Top and error types
(define Univ (make-Univ))
(define -Bottom (make-Union null))
(define Err (make-Error))

;A Type that corresponds to the any contract for the
;return type of functions
(define ManyUniv (make-AnyValues))

;; Char type and List type (needed because of how sequences are checked in subtype)
(define -Char (make-Base 'Char #'char? char? #'-Char #f))
(define (make-Listof elem) (-mu list-rec (simple-Un (make-Value null) (make-Pair elem list-rec))))


;; Simple union type, does not check for overlaps


;; Union constructor
;; Normalizes representation by sorting types.
;; Type * -> Type
;; The input types can be union types, but should not have a complicated
;; overlap relationship.
(define simple-Un
  (let ()
    ;; List[Type] -> Type
    ;; Argument types should not overlap or be union types
    (define (make-union* types)
      (match types
        [(list t) t]
        [_ (make-Union types)]))

    ;; Type -> List[Type]
    (define (flat t)
      (match t
        [(Union: es) es]
        [_ (list t)]))

    (case-lambda
      [() -Bottom]
      [(t) t]
      [args
       (make-union* (remove-dups (sort (append-map flat args) type<?)))])))

;; Recursive types
(define-syntax -v
  (syntax-rules ()
    [(_ x) (make-F 'x)]))

(define-syntax -mu
  (syntax-rules ()
    [(_ var ty)
     (let ([var (-v var)])
       (make-Mu 'var ty))]))
