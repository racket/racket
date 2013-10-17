#lang racket/base

(require compatibility/mlist)
(define SIZE 10000)

(define (sequence start stop)
  (if (> start stop)
      '()
      (mcons start (sequence (+ start 1) stop))))

(define (head-to-tail! headlist taillist)
  (when (null? taillist) (begin
                           (set! taillist (mlist (mcar headlist)))
                           (set! headlist (mcdr headlist))))
  (letrec ((htt-helper (lambda (dest)
                         (when (not (null? headlist))
                           (let ((headlink headlist))
                             (set-mcdr! dest headlink)
                             (set! headlist (mcdr headlist))
                             (htt-helper headlink))))))
    (htt-helper taillist)
    (values headlist taillist)))

(define (test-lists)
  (let* ([L1 (sequence 1 SIZE)]
         [L2 (mappend L1 '())]
         [L3 '()])
    (set!-values (L2 L3) (head-to-tail! L2 L3))
    (set!-values (L3 L2) (head-to-tail! (mreverse! L3) L2))
    (set! L1 (mreverse! L1))
    (cond ((not (= SIZE (mcar L1))) 0)
          ((not (equal? L1 L2))    0)
          (else           (mlength L1)))))

(define (main args)
  (let ((result 0))
    (let loop ((counter (if (= (vector-length args) 0)
                            1
                            (string->number (vector-ref args 0)))))
      (when (> counter 0)
        (set! result (test-lists))
        (loop (- counter 1))))
    (printf "~s\n" result)))

(main (current-command-line-arguments))
