#lang racket/base
;; stress tests for place-channels
(require (prefix-in pp: "place-processes.rkt"))
(require racket/place)

(define (splat txt fn)
  (call-with-output-file fn #:exists 'replace
      (lambda (out)
        (fprintf out "~a" txt))))

(define (print-out msg B/sE)
  (displayln (list msg 
    (exact->inexact B/sE)
    (exact->inexact (/ B/sE (* 1024 1024))))))

(define (processes-byte-message-test)
  (let ([pl
    (pp:place/base (bo ch)
      (define message-size (* 4024 1024))
      (define count 10)
      (define fourk-b-message (make-bytes message-size 66))
      (for ([i (in-range count)])
       (place-channel-recv ch)
       (place-channel-send ch fourk-b-message)))])

    (define message-size (* 4024 1024))
    (define four-k-message (make-bytes message-size 65))
    (define count 10)
    (define-values (r t1 t2 t3)
      (time-apply (lambda ()
      (for ([i (in-range count)])
        (pp:place-channel-send pl four-k-message)
        (pp:place-channel-recv pl))) null))


    (print-out "processes" (/ (* 2 count message-size) (/ t2 1000)))
    (pp:place-wait pl)))


(define (byte-message-test)
  (splat
  #<<END
  (module pct1 racket/base
    (require racket/place)
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


    (print-out "places" (/ (* 2 count message-size) (/ t2 1000)))
    (place-wait pl)))

(define (cons-tree-test)
  (splat
  #<<END
  (module pct1 racket/base
    (require racket/place)
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


    (printf "~a ~a ~a ~a\n" r t1 t2 t3)
    (place-wait pl)))

(byte-message-test)
(processes-byte-message-test)
(cons-tree-test)

