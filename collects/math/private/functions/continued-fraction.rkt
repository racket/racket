#lang typed/racket

(require racket/flonum
         "../../flonum.rkt")

(provide continued-fraction)

;; overflow = a power of 2 near sqrt(+max.0)
(define overflow (flexpt 2.0 (floor (/ (fllog (flsqrt +max.0)) (fllog 2.0)))))

(define-syntax-rule (continued-fraction a-init a-fun b-init b-fun eps-expr)
  (let: ([eps : Float  eps-expr])
    (let: loop : Float ([i : Float 0.0]
                        [a : Float a-init]
                        [b : Float b-init]
                        [last-n : Float 1.0]
                        [last-d : Float 0.0]
                        [n : Float 0.0]
                        [d : Float 1.0]
                        [x : Float 0.0])
      ;(printf "a = ~v  b = ~v~n" a b)
      (define next-n (+ (* a last-n) (* b n)))
      (define next-d (+ (* a last-d) (* b d)))
      (let-values ([(n d next-n next-d)
                    (cond [(or (next-n . > . overflow)
                               (next-d . > . overflow))
                           (values (/ n overflow) (/ d overflow)
                                   (/ next-n overflow) (/ next-d overflow))]
                          [(or (next-n . < . (/ 1.0 overflow))
                               (next-d . < . (/ 1.0 overflow)))
                           (values (* n overflow) (* d overflow)
                                   (* next-n overflow) (* next-d overflow))]
                          [else
                           (values n d next-n next-d)])])
        (define next-x (/ next-n next-d))
        ;(printf "n = ~v  d = ~v  x = ~v~n" next-n next-d next-x)
        (cond [(or ((abs (- x next-x)) . <= . (abs (* eps next-x)))
                   (not (rational? next-x)))
               ;(printf "i = ~v~n" i)
               ;i
               next-x]
              [else
               (let ([i  (+ i 1.0)])
                 (loop i (a-fun i a) (b-fun i b) n d next-n next-d next-x))])))))
