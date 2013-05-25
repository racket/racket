#lang racket/base

(define HI 0)
(define LO 0)

(define (hi-excp? x) (eq? x 'Hi_Exception))
(define (lo-excp? x) (eq? x 'Lo_Exception))
(define (true? x) (if (boolean? x) x #t))

(define (some_fun n)
  (with-handlers
      ([true? (lambda (exn) #f)])
    (hi_fun n)))

(define (hi_fun n)
  (with-handlers
      ([hi-excp? (lambda (exn) (set! HI (+ HI 1))) ])
    (lo_fun n)))

(define (lo_fun n)
  (with-handlers
      ([lo-excp? (lambda (exn) (set! LO (+ LO 1))) ])
    (blowup n)))

(define (blowup n)
  (if (= 0 (modulo n 2))
      (raise 'Hi_Exception)
      (raise 'Lo_Exception)))

(define (main args)
  (let* ((n (if (= (vector-length args) 1) (string->number (vector-ref args 0)) 1)))
    (do ((i 0 (+ i 1)))
        ((= i n))
      (some_fun i)))
  (printf "Exceptions: HI=~a / LO=~a\n" HI LO))

(main (current-command-line-arguments))
