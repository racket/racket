#lang typed/racket
(require typed/racket/async-channel)

;; same as threads-and-channels.rkt, but with async-channels

(: chan (Async-Channelof Symbol))
(define chan (make-async-channel))

(define (reader)
  (thread
   (lambda ()
     (let loop : True ((i : Integer 10))
       (if (= i 0)
           #t
           (begin (async-channel-get chan)
                  (loop (- i 1))))))))

(: writer (Symbol -> Thread))
(define (writer x)
  (thread
   (lambda ()
     (async-channel-put chan x)
     (async-channel-put chan x))))

(reader)
(writer 'a)
(writer 'b)
(writer 'c)
(writer 'd)
(writer 'e)


(define-type JumpingChannel (Rec JumpingChannel (Async-Channelof (Pair JumpingChannel Symbol))))
(: jump-chan JumpingChannel)
(define jump-chan (make-async-channel))

(define (jumping-reader)
  (thread
   (lambda ()
     (let loop : True ((i : Integer 3)
                        (c : JumpingChannel jump-chan))
       (if (= i 0)
           #t
           (loop (- i 1)
                 (car (async-channel-get c))))))))

(jumping-reader)
(let ((c2 : JumpingChannel (make-async-channel)))
  (async-channel-put jump-chan (cons c2 'a))
  (let ((c3 : JumpingChannel (make-async-channel)))
    (async-channel-put c2 (cons c3 'b))
    (let ((c4 : JumpingChannel (make-async-channel)))
      (async-channel-put c3 (cons c4 'c)))))




(: tc (Thread-Cellof Integer))
(define tc (make-thread-cell 0))

(thread-cell-set! tc 1)

(thread-wait (thread (lambda ()
                       (displayln (thread-cell-ref tc))
                       (thread-cell-set! tc 2)
                       (displayln (thread-cell-ref tc)))))

(thread-cell-ref tc)

(define blocked-thread
 (thread (lambda ()
           (async-channel-get ((inst make-async-channel 'unused))))))


(thread-suspend blocked-thread)
(kill-thread blocked-thread)
