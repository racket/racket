#lang racket/base

(provide make-async-bi-channel
         async-bi-channel-put
         async-bi-channel-get
         async-bi-channel?)


(define (make-async-channel)
  (define ch (make-channel))
  (values
    (thread
      (lambda ()
        (let loop ()
          (let ([v (thread-receive)])
            (channel-put ch v)
            (loop)))))
    ch))

(define-struct Async-Bi-Channel (in out)
                 #:property prop:evt (lambda (x) (Async-Bi-Channel-out x)))

(define async-bi-channel? Async-Bi-Channel?)

(define (make-async-bi-channel)
  (define-values (ch1s ch1r) (make-async-channel))
  (define-values (ch2s ch2r) (make-async-channel))

  (values
    (Async-Bi-Channel ch1s ch2r)
    (Async-Bi-Channel ch2s ch1r)))



(define (async-bi-channel-put ch msg)
  (void (thread-send (Async-Bi-Channel-in ch) msg #f)))

(define (async-bi-channel-get ch)
  (channel-get (Async-Bi-Channel-out ch)))
