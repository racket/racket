#lang racket
;; stress tests for place-channels

(define (splat txt fn)
  (call-with-output-file fn #:exists 'replace
      (lambda (out)
        (fprintf out "~a" txt))))

(define (byte-message-test)
  (splat
  #<<END
  (module pct1 scheme
    (provide place-main)

    (define (place-main ch)
      (define message-size (* 4024 1024))
      (define count 50)
      (define fourk-b-message (make-bytes message-size 66))
      (for ([i (in-range count)])
       (place-channel-recv ch)
       (place-channel-send ch fourk-b-message)))
  )
END
  "pct1.ss")

  (let ([pl (place "pct1.ss" 'place-main)])
    (define message-size (* 4024 1024))
    (define four-k-message (make-bytes message-size 65))
    (define count 50)
    (define-values (r t1 t2 t3)
      (time-apply (lambda ()
      (for ([i (in-range count)])
        (place-channel-send pl four-k-message)
        (place-channel-recv pl))) null))


    (define B/sE (/ (* 2 count message-size) (/ t2 1000)))
    (printf "~a ~a ~a ~a~n" r t1 t2 t3)
    (printf "B/s ~a~n" (exact->inexact B/sE))
    (printf "MB/s ~a~n" (exact->inexact (/ B/sE (* 1024 1024))))
    (place-wait pl)))

(define (cons-tree-test)
  (splat
  #<<END
  (module pct1 scheme
    (provide place-main)

    (define (place-main ch)
      (define count 500)
      (for ([i (in-range count)])
       (place-channel-send ch (place-channel-recv ch))))
  )
END
  "pct1.ss")

  (let ([pl (place "pct1.ss" 'place-main)])
    (define tree (let loop ([depth 8])
      (if (depth . <= . 0)
        1
        (cons (loop (sub1 depth)) (loop (sub1 depth))))))
    (define count 500)
    (define-values (r t1 t2 t3)
      (time-apply (lambda ()
      (for ([i (in-range count)])
        (place-channel-send pl tree)
        (place-channel-recv pl))) null))


    (printf "~a ~a ~a ~a~n" r t1 t2 t3)
    (place-wait pl)))

(byte-message-test)
(cons-tree-test)

