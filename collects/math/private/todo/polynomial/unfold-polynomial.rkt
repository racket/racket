#lang typed/racket

(require racket/fixnum
         "chebyshev.rkt")

(define-syntax (do-unfold-polynomial stx)
  (syntax-case stx ()
    [(_ [(b-names ...) init-bs] unfold-proc multiply-add zero n es cs)
     (syntax/loc stx
       (let-values ([(b-names ...)  init-bs])
         (let loop ([i 0] [res zero] [b-names b-names] ...)
           (cond [(i . < . n)
                  (let-values ([(a b-names ...)  (unfold-proc (vector-ref es i) b-names ...)])
                    (loop (+ i 1) (multiply-add (vector-ref cs i) a res) b-names ...))]
                 [else
                  res]))))]))

(define-syntax (unfold-polynomial stx)
  (syntax-case stx ()
    [(_ unfolder x deg n es cs)
     (with-syntax ([([(b-names ...) init-bs] unfold-proc multiply-add zero)
                    (local-expand #'(unfolder deg n x)
                                  (syntax-local-context)
                                  #f)])
       (syntax/loc stx
         (do-unfold-polynomial [(b-names ...) init-bs] unfold-proc multiply-add zero n es cs)))]))

(: monomial-apply ((Vectorof Integer) (Vectorof Real) -> Real))
(define (monomial-apply es xs)
  (define n (min (vector-length es) (vector-length xs)))
  (let loop ([#{i : Nonnegative-Fixnum} 0] [#{z : Real} 1])
    (cond [(i . fx< . n)
           (define w (expt (vector-ref xs i) (vector-ref es i)))
           (with-asserts ([w real?])
             (loop (fx+ i 1) (* z w)))]
          [else  z])))

(define-syntax (power-unfolder stx)
  (syntax-case stx ()
    [(_ deg n x)
     #'([()  (values)]
        (λ (a) (expt x a))
        (λ: ([u : Real] [v : Real] [w : Real]) (+ (* u v) w))
        (ann 0 Real))]))

(define-syntax (lacunary-unfolder stx)
  (syntax-case stx ()
    [(_ deg n x)
     #'([()  (values)]
        (λ: ([a : (Vectorof Integer)]) (monomial-apply a x))
        (λ: ([u : Real] [v : Real] [w : Real]) (+ (* u v) w))
        (ann 0 Real))]))

(define-syntax (chebyshev-unfolder stx)
  (syntax-case stx ()
    [(_ deg n x)
     #'([(ts)  (let ()
                 (define ts (ann (make-vector (+ deg 1) #f) (Vectorof (Option Real))))
                 (vector-set! ts 0 1)
                 (vector-set! ts 1 x)
                 ts)]
        (let ()
          (: get-t (Integer (Vectorof (Option Real)) -> (Values Real (Vectorof (Option Real)))))
          (define (get-t a ts)
            (define t
              (or (vector-ref ts a)
                  (let*-values ([(t-1 ts)  (get-t (- a 1) ts)]
                                [(t-2 ts)  (get-t (- a 2) ts)]
                                [(t)  (- (* 2 (* x t-1)) t-2)])
                    (vector-set! ts a t)
                    t)))
            (values t ts))
          get-t)
        (λ: ([u : Real] [v : Real] [w : Real]) (+ (* u v) w))
        (ann 0 Real))]))

(define x 10)
(define y 5)

(define: xs : Any
  (unfold-polynomial power-unfolder x 5 4 #(0 1 2 3) #(5 2 3 1)))

(define: ys : Any
  (unfold-polynomial lacunary-unfolder
                     (ann (vector x y) (Vectorof Real))
                     #(5 5)
                     4
                     (ann #(#(1 2) #(2 1) #(4 3) #(5 5)) (Vectorof (Vectorof Integer)))
                     #(5 2 3 1)))

(define: zs : Any
  (unfold-polynomial chebyshev-unfolder
                     x
                     5
                     4
                     (ann #(0 1 3 5) (Vectorof Integer))
                     #(2.55 3.1 2.1 1.1)))

