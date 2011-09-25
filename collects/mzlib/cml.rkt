#lang racket/base
(require racket/contract/base)

  (define (spawn thunk)
    (thread/suspend-to-kill thunk))

  (define (channel)
    (make-channel))

  (define (channel-recv-evt ch)
    ch)

  (define (channel-send-evt ch v)
    (wrap-evt
     (channel-put-evt ch v)
     void))

  (define (thread-done-evt th)
    (thread-dead-evt th))
  
  (define (current-time)
    (current-inexact-milliseconds))
  (define (time-evt t)
    (alarm-evt t))

  (provide/contract
   (spawn ((-> any) . -> . thread?))
   (channel (-> channel?))
   (channel-recv-evt (channel? . -> . evt?))
   (channel-send-evt (channel? any/c . -> . evt?))
   
   (thread-done-evt (thread? . -> . evt?))
   (current-time (-> number?))
   (time-evt (real? . -> . evt?)))

