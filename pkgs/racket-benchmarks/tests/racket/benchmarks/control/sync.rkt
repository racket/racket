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

'sync-two
(times
 (let ([s (make-semaphore M)])
   (for ([i (in-range M)])
     (sync s never-evt))))

'sync-three
(times
 (let ([s (make-semaphore M)])
   (for ([i (in-range M)])
     (sync s always-evt never-evt))))
