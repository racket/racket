#lang typed/racket/base

(require "flonum-constants.rkt"
         "flonum-functions.rkt")

(provide flbracketed-root)

(define debug-printf? #f)

(define-syntax-rule (debugf e ...)
  (when debug-printf? (printf e ...)))

(: flbracketed-root* (-> (-> Flonum Flonum) Flonum Flonum Flonum Flonum Flonum))
(define (flbracketed-root* f a fa b fb)
  (let loop ([bisected? #t] [a a] [fa fa] [b b] [fb fb] [c a] [fc fa] [d 0.0] [n 0])
    (debugf "~v: ~v ~v~n" n a b)
    (debugf "~v ~v~n~n" fa fb)
    (define min-abs-ab (min (abs a) (abs b)))
    (define ε (if (min-abs-ab . <= . +max-subnormal.0) +min.0 (* min-abs-ab epsilon.0)))
    (cond
      ;; If we got it right, return it
      [(= fb 0.0)  b]
      ;; If a and b are too close, return b
      [((abs (- a b)) . <= . ε)
       (debugf "(abs (- a b)) <= ~v; bailing~n" ε)
       b]
      [(n . < . 5000)
       (let*-values
           ([(bisect? s)
             (cond
               ;; Weird rules for forcing bisection to make progress
               [(or (and bisected?       ((abs (- b c)) . < . (* 128.0 ε))
                         (debugf "reason 1~n"))
                    (and (not bisected?) ((abs (- c d)) . < . (* 128.0 ε))
                         (debugf "reason 2:~n")))
                (values #t 0.0)]
               ;; Get an interpolated point
               [else
                (define fa-fb (- fa fb))
                (define fb-fc (- fb fc))
                (define fc-fa (- fc fa))
                (define da (* fa-fb (- fc-fa)))
                (define db (* fb-fc (- fa-fb)))
                (define dc (* fc-fa (- fb-fc)))
                (cond [(or (= da 0.0) (= db 0.0) (= dc 0.0))
                       ;; Secant method because quadratic method will fail
                       (values #f (- b (* fb (/ (- b a) (- fa-fb)))))]
                      [else
                       ;; Inverse quadratic interpolation method
                       (values #f (+ (/ (* a fb fc) da)
                                     (/ (* b fc fa) db)
                                     (/ (* c fa fb) dc)))])])]
            ;; Run tests to determine whether it's a bad point
            [(bisected? s)
             (cond
               [(or bisect?
                    (and (not (let ([s0  (/ (+ (* 3.0 a) b) 4.0)])
                                (if (<= s0 b) (<= s0 s b) (<= b s s0))))
                         (debugf "reason 3~n"))
                    (and bisected?       ((abs (- s b)) . >= . (* 0.5 (abs (- b c))))
                         (debugf "reason 4~n"))
                    (and (not bisected?) ((abs (- s b)) . >= . (* 0.5 (abs (- c d))))
                         (debugf "reason 5~n")))
                ;; Bisect this time
                (values #t (* 0.5 (+ a b)))]
               [else
                (values #f s)])]
            [(d)  c]
            [(c fc)  (values b fb)]
            [(fs)  (f s)]
            ;; Replace the endpoint with the same sign as s
            [(a fa b fb)  (if ((* fa fs) . < . 0.0)
                              (values a fa s fs)
                              (values s fs b fb))]
            ;; Make sure b is closer
            [(a fa b fb)  (if ((abs fa) . < . (abs fb))
                              (values b fb a fa)
                              (values a fa b fb))])
         (loop bisected? a fa b fb c fc d (+ n 1)))]
      [else  b])))

(: flbracketed-root (-> (-> Flonum Flonum) Flonum Flonum Flonum))
;; Find a root between a and b if f(a) and f(b) have opposite signs
(define (flbracketed-root f a b)
  (define fa (f a))
  (define fb (f b))
  (cond [(= fa 0.0)  a]
        [(= fb 0.0)  b]
        [(or (flnan? a) (flnan? fa) (flnan? b) (flnan? fb))  +nan.0]
        ;; Check signs
        [((* fa fb) . >= . 0.0)
         (debugf "(f ~v) = ~v and (f ~v) = ~v do not bracket a root~n" a fa b fb)
         +nan.0]
        [else
         (let*-values
             ([(a fa)  (cond [(= a -inf.0)  (values -max.0 (f -max.0))]
                             [(= a +inf.0)  (values +max.0 (f +max.0))]
                             [else  (values a fa)])]
              [(b fb)  (cond [(= b -inf.0)  (values -max.0 (f -max.0))]
                             [(= b +inf.0)  (values +max.0 (f +max.0))]
                             [else  (values b fb)])]
              ;; Make sure b is closer
              [(a fa b fb)  (if ((abs fa) . < . (abs fb))
                                (values b fb a fa)
                                (values a fa b fb))])
           (flbracketed-root* f a fa b fb))]))
