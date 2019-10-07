#lang racket/base

(require racket/include)

(include "config.rktl")

'----------------------------------------

'spin-ping-pong
(times
 (let ([s1 (box #t)]
       [s2 (box #t)])
   (define (box-wait b)
     (unless (box-cas! b #t #f)
       (sleep)
       (box-wait b)))
   (define (box-post b)
     (unless (box-cas! b #f #t)
       (sleep)
       (box-post b)))
   (define (make s1 s2)
     (thread (lambda ()
               (let loop ([i Q])
                 (box-wait s1)
                 (unless (zero? i)
                   (box-post s2)
                   (loop (sub1 i)))))))
   (define t1 (make s1 s2))
   (define t2 (make s2 s1))
   (thread-wait t1)
   (thread-wait t2)))

'channel-ping-pong
(times
 (let ([c1 (make-channel)]
       [c2 (make-channel)])
   (define (make first?)
     (thread (lambda ()
               (let loop ([i Q])
                 (if first?
                     (channel-put c2 (channel-get c1))
                     (begin
                       (channel-put c1 'ok)
                       (channel-get c2)))
                 (unless (zero? i)
                   (loop (sub1 i)))))))
   (define t1 (make #t))
   (define t2 (make #f))
   (thread-wait t1)
   (thread-wait t2)))

'sema-ping-pong
(times
 (let ([s1 (make-semaphore 1)]
       [s2 (make-semaphore 1)])
   (define (make s1 s2)
     (thread (lambda ()
               (let loop ([i Q])
                 (semaphore-wait s1)
                 (unless (zero? i)
                   (semaphore-post s2)
                   (loop (sub1 i)))))))
   (define t1 (make s1 s2))
   (define t2 (make s2 s1))
   (thread-wait t1)
   (thread-wait t2)))

'sema-ping-pong/prompts
(times
 (let ([s1 (make-semaphore 1)]
       [s2 (make-semaphore 1)])
   (define (make s1 s2)
     (thread (lambda ()
               (let loop ([j 100])
                 (if (zero? j)
                     (let loop ([i Q])
                       (semaphore-wait s1)
                       (unless (zero? i)
                         (semaphore-post s2)
                         (loop (sub1 i))))
                     (call-with-continuation-prompt
                      (lambda ()
                        (loop (sub1 j)))))))))
   (define t1 (make s1 s2))
   (define t2 (make s2 s1))
   (thread-wait t1)
   (thread-wait t2)))

'sema-ping-pong/marks
(times
 (let ([s1 (make-semaphore 1)]
       [s2 (make-semaphore 1)])
   (define (make s1 s2)
     (thread (lambda ()
               (let loop ([j 100])
                 (if (zero? j)
                     (let loop ([i Q])
                       (semaphore-wait s1)
                       (unless (zero? i)
                         (semaphore-post s2)
                         (loop (sub1 i))))
                     (with-continuation-mark
                      'key j
                      (loop (sub1 j))))))))
   (define t1 (make s1 s2))
   (define t2 (make s2 s1))
   (thread-wait t1)
   (thread-wait t2)))
