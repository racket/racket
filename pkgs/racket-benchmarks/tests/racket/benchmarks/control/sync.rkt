#lang racket/base

(require racket/include)

(include "config.rktl")

'----------------------------------------

'semaphore-wait
(times
 (let ([s (make-semaphore M)])
   (for ([i (in-range M)])
     (semaphore-wait s))))

'semaphore-post+wait
(times
 (let ([s (make-semaphore)])
   (for ([i (in-range M)])
     (semaphore-post s)
     (semaphore-wait s))))

'semaphore-peek-evt
(times
 (let ([e (semaphore-peek-evt (make-semaphore 1))])
   (for ([i (in-range M)])
     (sync e))))

'sync-guard
(times
 (let ([g (guard-evt (lambda () always-evt))])
   (for ([i (in-range M)])
     (sync g))))

'sync-semaphore+never
(times
 (let ([s (make-semaphore M)])
   (for ([i (in-range M)])
     (sync s never-evt))))

'sync-semaphore+semaphore
(times
 (let ([s (make-semaphore M)]
       [s2 (make-semaphore M)])
   (for ([i (in-range M)])
     (sync s s2))))

'sync-semaphore+semaphore/timeout
(times
 (let ([s (make-semaphore)]
       [s2 (make-semaphore)])
   (for ([i (in-range M)])
     (sync/timeout 0 s s2))))

'sync-semaphore+semaphore/timeout-callback
(times
 (let ([s (make-semaphore)]
       [s2 (make-semaphore)])
   (for ([i (in-range M)])
     (sync/timeout list s s2))))

'sync-guard+guard
(times
 (let ([g (guard-evt (lambda () always-evt))])
   (for ([i (in-range M)])
     (sync g g))))

'sync-three-semaphores
(times
 (let ([s (make-semaphore M)]
       [s2 (make-semaphore M)]
       [s3 (make-semaphore M)])
   (for ([i (in-range M)])
     (sync s s2 s3))))
