#;#;
#<<END
TR info: mandelbrot.rkt 10:15 (/ (* 2.0 y) n) -- possible exact real arith
TR info: mandelbrot.rkt 10:18 (* 2.0 y) -- possible exact real arith
TR info: mandelbrot.rkt 11:15 (/ (* 2.0 x) n) -- possible exact real arith
TR info: mandelbrot.rkt 11:18 (* 2.0 x) -- possible exact real arith
TR info: mandelbrot.rkt 21:28 (* 2 zr zi) -- possible exact real arith
TR missed opt: mandelbrot.rkt 10:12 (- (/ (* 2.0 y) n) 1.0) -- all args float-arg-expr, result not Float -- caused by: 10:15 (/ (* 2.0 y) n)
TR missed opt: mandelbrot.rkt 10:12 (- (/ (* 2.0 y) n) 1.0) -- exact ops inside float expr -- caused by: 10:15 (/ (* 2.0 y) n)
TR missed opt: mandelbrot.rkt 10:15 (/ (* 2.0 y) n) -- all args float-arg-expr, result not Float -- caused by: 10:18 (* 2.0 y), 10:28 n
TR missed opt: mandelbrot.rkt 10:18 (* 2.0 y) -- all args float-arg-expr, result not Float -- caused by: 10:25 y
TR missed opt: mandelbrot.rkt 11:12 (- (/ (* 2.0 x) n) 1.5) -- all args float-arg-expr, result not Float -- caused by: 11:15 (/ (* 2.0 x) n)
TR missed opt: mandelbrot.rkt 11:12 (- (/ (* 2.0 x) n) 1.5) -- exact ops inside float expr -- caused by: 11:15 (/ (* 2.0 x) n)
TR missed opt: mandelbrot.rkt 11:15 (/ (* 2.0 x) n) -- all args float-arg-expr, result not Float -- caused by: 11:18 (* 2.0 x), 11:28 n
TR missed opt: mandelbrot.rkt 11:18 (* 2.0 x) -- all args float-arg-expr, result not Float -- caused by: 11:25 x
TR missed opt: mandelbrot.rkt 18:14 (> (+ zrq ziq) 4) -- generic comparison -- caused by: 18:29 4
TR missed opt: mandelbrot.rkt 21:25 (- (* 2 zr zi) ci) -- all args float-arg-expr, result not Float -- caused by: 21:28 (* 2 zr zi)
TR missed opt: mandelbrot.rkt 21:28 (* 2 zr zi) -- all args float-arg-expr, result not Float -- caused by: 21:31 2
TR opt: mandelbrot.rkt 15:21 (* zr zr) -- binary float
TR opt: mandelbrot.rkt 16:21 (* zi zi) -- binary float
TR opt: mandelbrot.rkt 18:17 (+ zrq ziq) -- binary float
TR opt: mandelbrot.rkt 20:25 (+ (- zrq ziq) cr) -- binary float
TR opt: mandelbrot.rkt 20:28 (- zrq ziq) -- binary float
END
""

#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

;; from Matthew's talk at Mozilla
;; 2 things were blocking futures:
;; - (> (+ zrq ziq) 4) ; generic comparison
;; - (* 2 zr zi) ; generic multiplication
;; OC reports both

(: mandelbrot : Integer Integer Integer Integer -> Integer)
(define (mandelbrot iterations x y n)
  (let ([ci (- (/ (* 2.0 y) n) 1.0)]
        [cr (- (/ (* 2.0 x) n) 1.5)])
    (let loop ([i 0] [zr 0.0] [zi 0.0])
      (if (> i iterations)
          i
          (let ([zrq (* zr zr)]
                [ziq (* zi zi)])
            (cond
             [(> (+ zrq ziq) 4) i]
             [else (loop (add1 i)
                         (+ (- zrq ziq) cr)
                         (- (* 2 zr zi) ci))]))))))
