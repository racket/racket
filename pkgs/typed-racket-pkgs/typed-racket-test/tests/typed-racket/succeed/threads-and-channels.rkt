#lang typed/racket

(: chan (Channelof Symbol))
(define chan (make-channel))

(define (reader)
  (thread
   (lambda ()
     (let: loop : True ((i : Integer 10))
       (if (= i 0)
           #t
           (begin (channel-get chan)
                  (loop (- i 1))))))))

(: writer (Symbol -> Thread))
(define (writer x)
  (thread
   (lambda ()
     (channel-put chan x)
     (channel-put chan x))))

(reader)
(writer 'a)
(writer 'b)
(writer 'c)
(writer 'd)
(writer 'e)


(define-type JumpingChannel (Rec JumpingChannel (Channelof (Pair JumpingChannel Symbol))))
(: jump-chan JumpingChannel)
(define jump-chan (make-channel))

(define (jumping-reader)
  (thread
   (lambda ()
     (let: loop : True ((i : Integer 3)
                        (c : JumpingChannel jump-chan))
       (if (= i 0)
           #t
           (loop (- i 1)
                 (car (channel-get c))))))))

(jumping-reader)
(let: ((c2 : JumpingChannel (make-channel)))
  (channel-put jump-chan (cons c2 'a))
  (let: ((c3 : JumpingChannel (make-channel)))
    (channel-put c2 (cons c3 'b))
    (let: ((c4 : JumpingChannel (make-channel)))
      (channel-put c3 (cons c4 'c)))))




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
           (channel-get ((inst make-channel 'unused))))))


(thread-suspend blocked-thread)
(kill-thread blocked-thread)
