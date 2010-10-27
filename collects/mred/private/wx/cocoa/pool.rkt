#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/atomic
         "utils.rkt"
         "const.rkt"
         "types.rkt")

(provide 
 (protect-out queue-autorelease-flush
              autorelease-flush))

(import-class NSAutoreleasePool)

;; This pool manages all objects that would otherwise not
;; have a pool:
(define pool (tell (tell NSAutoreleasePool alloc) init))

;; We need to periodically flush the main pool, otherwise
;; object autoreleased through the pool live until the
;; end of execution:
(define (autorelease-flush)
  (start-atomic)
  (tellv pool drain)
  (set! pool (tell (tell NSAutoreleasePool alloc) init))
  (end-atomic))

(define queued? #f)
(define autorelease-evt (make-semaphore))

(define (queue-autorelease-flush)
  (start-atomic)
  (unless queued?
    (semaphore-post autorelease-evt)
    (set! queued? #t))
  (end-atomic))

;; Create a thread to periodically flush:
(void
 (thread (lambda ()
           (let loop ()
             (sync autorelease-evt)
             (set! queued? #f)
             (autorelease-flush)
             (loop)))))
