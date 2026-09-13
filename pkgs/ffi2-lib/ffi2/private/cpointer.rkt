#lang racket/base
(require '#%foreign)

(provide ptr_t->cpointer
         cpointer->ptr_t)

(define (ptr_t->cpointer ptr)
  (unless (ffi2-ptr? ptr)
    (raise-argument-error 'ptr_t->cpointer "void_t*?" ptr))
  (ffi2-ptr->cpointer ptr))

(define (cpointer->ptr_t ptr)
  (cpointer->ffi2-ptr 'cpointer->ptr_t ptr))

