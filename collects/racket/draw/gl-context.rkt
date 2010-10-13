#lang racket/base
(require racket/class
         "syntax.rkt")

(provide gl-context%
         gl-context<%>
         
         do-call-as-current
         do-swap-buffers)

(define-local-member-name
  do-call-as-current
  do-swap-buffers)

(define (procedure-arity-0? v) 
  (and (procedure? v)
       (procedure-arity-includes? v 0)))

;; Implemented by subclasses:
(defclass gl-context% object%
  (define lock-thread #f)
  (define lock (make-semaphore 1))
  
  (define/private (with-gl-lock t)
    (if (eq? lock-thread (current-thread))
        (t)
        (call-with-semaphore
         lock
         (lambda ()
           (set! lock-thread (current-thread))
           (begin0 
            (t)
            (set! lock-thread #f))))))

  (def/public (call-as-current [procedure-arity-0? t])
    (with-gl-lock
     (lambda ()
       (do-call-as-current t))))
        
  (define/public (swap-buffers)
    (with-gl-lock
     (lambda ()
       (do-swap-buffers))))

  (define/public (ok?) #t)

  (define/public (do-call-as-current t) (t))
  (define/public (do-swap-buffers t) (void))

  (super-new))

(define gl-context<%> (class->interface gl-context%))
