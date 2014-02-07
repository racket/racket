#;#;
#<<END
TR info: mandelbrot.rkt 46:15 (/ (* 2.0 y) n) -- possible exact real arith
TR info: mandelbrot.rkt 46:18 (* 2.0 y) -- possible exact real arith
TR info: mandelbrot.rkt 47:15 (/ (* 2.0 x) n) -- possible exact real arith
TR info: mandelbrot.rkt 47:18 (* 2.0 x) -- possible exact real arith
TR info: mandelbrot.rkt 57:28 (* 2 zr zi) -- possible exact real arith
TR info: mandelbrot.rkt 57:28 (* 2 zr zi) -- possible exact real arith
TR missed opt: mandelbrot.rkt 46:12 (- (/ (* 2.0 y) n) 1.0) -- all args float-arg-expr, result not Float -- caused by: 46:15 (/ (* 2.0 y) n)
TR missed opt: mandelbrot.rkt 46:12 (- (/ (* 2.0 y) n) 1.0) -- exact ops inside float expr -- caused by: 46:15 (/ (* 2.0 y) n)
TR missed opt: mandelbrot.rkt 46:15 (/ (* 2.0 y) n) -- all args float-arg-expr, result not Float -- caused by: 46:18 (* 2.0 y), 46:28 n
TR missed opt: mandelbrot.rkt 46:18 (* 2.0 y) -- all args float-arg-expr, result not Float -- caused by: 46:25 y
TR missed opt: mandelbrot.rkt 47:12 (- (/ (* 2.0 x) n) 1.5) -- all args float-arg-expr, result not Float -- caused by: 47:15 (/ (* 2.0 x) n)
TR missed opt: mandelbrot.rkt 47:12 (- (/ (* 2.0 x) n) 1.5) -- exact ops inside float expr -- caused by: 47:15 (/ (* 2.0 x) n)
TR missed opt: mandelbrot.rkt 47:15 (/ (* 2.0 x) n) -- all args float-arg-expr, result not Float -- caused by: 47:18 (* 2.0 x), 47:28 n
TR missed opt: mandelbrot.rkt 47:18 (* 2.0 x) -- all args float-arg-expr, result not Float -- caused by: 47:25 x
TR missed opt: mandelbrot.rkt 54:14 (> (+ zrq ziq) 4) -- generic comparison -- caused by: 54:29 4
TR missed opt: mandelbrot.rkt 54:14 (> (+ zrq ziq) 4) -- generic comparison -- caused by: 54:29 4
TR missed opt: mandelbrot.rkt 57:25 (- (* 2 zr zi) ci) -- all args float-arg-expr, result not Float -- caused by: 57:28 (* 2 zr zi)
TR missed opt: mandelbrot.rkt 57:25 (- (* 2 zr zi) ci) -- all args float-arg-expr, result not Float -- caused by: 57:28 (* 2 zr zi)
TR missed opt: mandelbrot.rkt 57:28 (* 2 zr zi) -- all args float-arg-expr, result not Float -- caused by: 57:31 2
TR missed opt: mandelbrot.rkt 57:28 (* 2 zr zi) -- all args float-arg-expr, result not Float -- caused by: 57:31 2
TR opt: mandelbrot.rkt 51:21 (* zr zr) -- binary float
TR opt: mandelbrot.rkt 51:21 (* zr zr) -- binary float
TR opt: mandelbrot.rkt 52:21 (* zi zi) -- binary float
TR opt: mandelbrot.rkt 52:21 (* zi zi) -- binary float
TR opt: mandelbrot.rkt 54:17 (+ zrq ziq) -- binary float
TR opt: mandelbrot.rkt 54:17 (+ zrq ziq) -- binary float
TR opt: mandelbrot.rkt 56:25 (+ (- zrq ziq) cr) -- binary float
TR opt: mandelbrot.rkt 56:25 (+ (- zrq ziq) cr) -- binary float
TR opt: mandelbrot.rkt 56:28 (- zrq ziq) -- binary float
TR opt: mandelbrot.rkt 56:28 (- zrq ziq) -- binary float
END
""

#lang typed/racket

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
