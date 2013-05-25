#lang typed/racket

(require "../../flonum.rkt")

(provide continued-fraction continued-fraction-parts)

;; overflow = a power of 2 near sqrt(+max.0)
(define overflow (flexpt 2.0 (flfloor (fl/ (fllog (flsqrt +max.0)) (fllog 2.0)))))

(define-syntax-rule (continued-fraction a-init a-fun b-init b-fun eps-expr)
  (let-values ([(n d)  (continued-fraction-parts a-init a-fun b-init b-fun eps-expr)])
    (fl/ n d)))

(define-syntax-rule (continued-fraction-parts a-init a-fun b-init b-fun eps-expr)
  (let: ([eps : Flonum  eps-expr])
    (let: loop : (Values Flonum Flonum) ([i : Flonum 0.0]
                                         [a : Flonum a-init]
                                         [b : Flonum b-init]
                                         [last-n : Flonum 1.0]
                                         [last-d : Flonum 0.0]
                                         [n : Flonum 0.0]
                                         [d : Flonum 1.0]
                                         [x : Flonum 0.0])
      ;(printf "a = ~v  b = ~v~n" a b)
      (define next-n (fl+ (fl* a last-n) (fl* b n)))
      (define next-d (fl+ (fl* a last-d) (fl* b d)))
      (let-values ([(n d next-n next-d)
                    (cond [(or (next-n . fl> . overflow)
                               (next-d . fl> . overflow))
                           (values (fl/ n overflow) (fl/ d overflow)
                                   (fl/ next-n overflow) (fl/ next-d overflow))]
                          [(or (next-n . < . (fl/ 1.0 overflow))
                               (next-d . < . (fl/ 1.0 overflow)))
                           (values (fl* n overflow) (fl* d overflow)
                                   (fl* next-n overflow) (fl* next-d overflow))]
                          [else
                           (values n d next-n next-d)])])
        (define next-x (fl/ next-n next-d))
        ;(printf "n = ~v  d = ~v  x = ~v~n" next-n next-d next-x)
        (cond [(or ((flabs (fl- x next-x)) . fl<= . (flabs (fl* eps next-x)))
                   (not (rational? next-x)))
               ;(printf "i = ~v~n" i)
               ;i
               (values next-n next-d)]
              [else
               (let ([i  (fl+ i 1.0)])
                 (loop i (a-fun i a) (b-fun i b) n d next-n next-d next-x))])))))
