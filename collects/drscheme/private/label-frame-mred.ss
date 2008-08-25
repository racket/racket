#lang mzscheme
  (require mred
           mzlib/class)
  (provide (all-from-except mred frame%)
           (rename registering-frame% frame%)
           lookup-frame-name)
  
  (define (lookup-frame-name frame)
    (semaphore-wait label-sema)
    (begin0
      (hash-table-get label-ht frame (λ () #f))
      (semaphore-post label-sema)))
  
  (define label-sema (make-semaphore 1))
  (define label-ht (make-hash-table 'weak))
  
  (define registering-frame%
    (class frame%
      (define/override (set-label x)
        (semaphore-wait label-sema)
        (hash-table-put! label-ht this x)
        (semaphore-post label-sema)
        (super set-label x))
      (inherit get-label)
      (super-instantiate ())
      (semaphore-wait label-sema)
      (hash-table-put! label-ht this (get-label))
      (semaphore-post label-sema)))
