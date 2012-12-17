#lang racket/base

(require typed/untyped-utils
         (except-in "typed-array-convert.rkt"
                    list*->array
                    vector*->array)
         (prefix-in typed: (only-in "typed-array-convert.rkt"
                                    list*->array
                                    vector*->array))
         (prefix-in untyped: (only-in "untyped-array-convert.rkt"
                                      list*->array
                                      vector*->array)))

(provide list*->array
         vector*->array
         array->list*
         array->vector*
         array->list
         array->vector)

(define-typed/untyped-identifier list*->array
  typed:list*->array
  untyped:list*->array)

(define-typed/untyped-identifier vector*->array
  typed:vector*->array
  untyped:vector*->array)
