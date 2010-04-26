#lang racket/base
  (require racket/gui/base
           racket/class)
  (provide (except-out (all-from-out racket/gui/base) frame%)
           (rename-out [registering-frame% frame%])
           lookup-frame-name)
  
  (define (lookup-frame-name frame)
    (semaphore-wait label-sema)
    (begin0
      (hash-ref label-ht frame (λ () #f))
      (semaphore-post label-sema)))
  
  (define label-sema (make-semaphore 1))
  (define label-ht (make-weak-hasheq))
  
  (define registering-frame%
    (class frame%
      (define/override (set-label x)
        (semaphore-wait label-sema)
        (hash-set! label-ht this x)
        (semaphore-post label-sema)
        (super set-label x))
      (inherit get-label)
      (super-instantiate ())
      (semaphore-wait label-sema)
      (hash-set! label-ht this (get-label))
      (semaphore-post label-sema)))
