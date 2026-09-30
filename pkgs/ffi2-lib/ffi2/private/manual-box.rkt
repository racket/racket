#lang racket/base
(require '#%foreign
         (only-in "core.rkt"
                  ffi2-malloc
                  ffi2-sizeof
                  ffi2-set!
                  ffi2-ref
                  ptr_t
                  ptr_t->ptr_t/gcable
                  ptr_t/gcable->ptr_t))

(provide ffi2-malloc-manual-box
         ffi2-free-manual-box
         ffi2-manual-box-ref
         ffi2-manual-box-set!)

(define (ffi2-malloc-manual-box v)
  (define p (cpointer->ffi2-ptr #f (malloc-immobile-cell v)))
  ;; forget GCable:
  (ptr_t/gcable->ptr_t p))

(define (ffi2-free-manual-box p)
  (unless (ffi2-ptr? p)
    (raise-argument-error 'ffi2-free-manual-box "ptr_t?" p))
  (free-immobile-cell (ffi2-ptr->cpointer p)))

(define (ffi2-manual-box-ref p)
  (unless (ffi2-ptr? p)
    (raise-argument-error 'ffi2-manual-box-ref "ptr_t?" p))
  (ptr-ref (ffi2-ptr->cpointer p) _scheme))

(define (ffi2-manual-box-set! p v)
  (unless (ffi2-ptr? p)
    (raise-argument-error 'ffi2-manual-box-ref "ptr_t?" p))
  ;; remember GCable, so that assignment uses a write barrier
  (define p/gc (ptr_t->ptr_t/gcable p))
  (ptr-set! (ffi2-ptr->cpointer p/gc) _scheme v))
