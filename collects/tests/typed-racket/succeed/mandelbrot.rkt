#lang typed/racket/base #:optimize
(require racket/future racket/flonum)
(define: MAX-ITERS : Positive-Fixnum 50)
(define MAX-DIST 2.0)
(define: N : Positive-Fixnum 512)
(: mandelbrot-point : Integer Integer -> Integer)
(define (mandelbrot-point x y)
  (define c
    (+ (- (/ (* 2.0 (->fl x)) N) 1.5)
       (* 0.0+1.0i (- (/ (* 2.0 (->fl y)) N) 1.0))))
  (let loop ((i 0) (z 0.0+0.0i))
    (cond
      [(> i MAX-ITERS) (char->integer #\*)]
      [(> (magnitude z) MAX-DIST)
       (char->integer #\space)]
      [else (loop (add1 i) (+ (* z z) c))])))

(: fs (Listof (Futureof Bytes)))
(define fs
  (for/list ([y (in-range N)])
    (let ([bstr (make-bytes N)])
      (future
       (lambda ()
         (for ([x (in-range N)])
           (bytes-set! bstr x (mandelbrot-point x y)))
         bstr)))))

(lambda ()
  (for: ([f : (Futureof Bytes) (in-list fs)])
    (write-bytes (touch f))
    (newline)))
