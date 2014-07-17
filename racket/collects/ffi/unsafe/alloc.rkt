#lang racket/base
(require ffi/unsafe
         "atomic.rkt")

(provide allocator deallocator retainer 
         (rename-out [deallocator releaser]))

(define allocated (make-late-weak-hasheq))

(define (deallocate v)
  ;; Called as a finalizer, we we assume that the
  ;; enclosing thread will not be interrupted.
  (let ([ds (hash-ref allocated v #f)])
    (when ds
      (hash-remove! allocated v)
      (for ([d (in-list ds)])
        (d v)))))

(define ((allocator d) proc)
  (rename
    (lambda args
      (call-as-atomic
        (lambda ()
          (let ([v (apply proc args)])
            (when v
              (hash-set! allocated v (list d))
              (register-finalizer v deallocate))
            v))))
    proc))

(define ((deallocator [get-arg car]) proc)
  (rename
    (lambda args
      (call-as-atomic
        (lambda ()
         (apply proc args)
         (let ([v (get-arg args)])
           (let ([ds (hash-ref allocated v #f)])
             (when ds
               (if (null? (cdr ds))
                   (hash-remove! allocated v)
                   (hash-set! allocated v (cdr ds)))))))))
   proc))

(define ((retainer d [get-arg car]) proc)
  (rename
   (lambda args
     (call-as-atomic
       (lambda ()
         (begin0
           (apply proc args)
           (let ([v (get-arg args)])
             (let ([ds (hash-ref allocated v null)])
               (hash-set! allocated v (cons d ds))))))))
   proc))

(define (rename new orig)
  (and orig
       (let ([n (object-name orig)]
             [new (procedure-reduce-arity
                   new
                   (procedure-arity orig))])
         (if n
             (procedure-rename new n)
             new))))
