#lang typed/racket/base

(provide divides? coprime? pairwise-coprime? bezout)

;;;
;;; DIVISIBILITY
;;;

(: divides? : Integer Integer -> Boolean)
; a divides b <=> exists unique k s.t. a*k=b
(define (divides? a b)
  (cond [(zero? a)  #f]
        [else  (= (remainder b a) 0)]))

; DEF (Coprime, relatively prime)
;  Two or more integers are called coprime, if their greatest common divisor is 1.
;     a, b and c coprime <=> gcd(a,b,c)=1

(: coprime? : Integer Integer * -> Boolean)
(define (coprime? a . bs)
  (= 1 (apply gcd (cons a bs))))

(: pairwise-coprime? : Integer Integer * -> Boolean)
(define (pairwise-coprime? a . bs)
  (or (null? bs)
      (and (andmap (λ: ([b : Integer]) (coprime? a b)) bs)
           (apply pairwise-coprime? bs))))

; THEOREM (Bezout's identity)
;  If a and b are integers (not both zero), then
;  there exists integers u and v such that
;    gcd(a,b) = au + bv
;  Note: u and v are not unique

; (bezout-binary a b) = (list u v)   <=>   gcd(a,b) = au + bv
(: bezout-binary : Integer Integer -> (List Integer Integer))
(define (bezout-binary a b)
  (: loop : Integer Integer Integer Integer Integer Integer -> (List Integer Integer))
  (define (loop a b ua va ub vb)  ; a>=b>0 , a = ua*a+ub*b,  b = ub*a+ub*b
    (let-values ([(q r) (quotient/remainder a b)])
      (if (= r 0)
          (list ub vb)
          (loop b r ub vb (- ua (* q ub)) (- va (* q vb))))))
  (: start : Integer Integer -> (List Integer Integer))
  (define (start a b)
    (if (> a b)
        (loop a b 1 0 0 1)
        (loop b a 0 1 1 0)))
  (cond [(and (positive? a) (positive? b))
         (start a b)]
        [(and (negative? a) (negative? b))
         (define uv (start (- a) (- b)))
         (list (- (car uv)) (- (cadr uv)))]
        [(and (negative? a) (positive? b))
         ; choose k s.t. a+kb>0
         (define k (+ (quotient (- a) b) 1))
         (define uv (start (+ a (* k b)) b))
         (define u (car uv)) (define v (cadr uv))
         (list u (+ (* u k) v))]
        [(and (positive? a) (negative? b))
         ; choose k s.t. ak+b>0
         (define k (+ (quotient (- b) a) 1))
         (define uv (start a (+ (* k a) b)))
         (define u (car uv)) (define v (cadr uv))
         (list (+ u (* k v)) v)]
        [else (error "Internal error in bezout-binary")]))

; (bezout a b c ...) -> (list u v w ...)    <=>  gcd(a,b,c,...) = au + bv + cw + ...
(: bezout : Integer Integer * -> (Listof Integer))
(define (bezout a . bs)
  (cond
    [(null? bs)        (list 1)]
    [(null? (cdr bs))  (bezout-binary a (car bs))]
    [else    
     (let ([uvs (apply bezout bs)]
           [st  (bezout-binary (apply gcd bs) a)])
       (let ([s (car st)]
             [t (cadr st)])
         (cons t (map (lambda: ([u : Integer]) (* s u))
                      uvs))))]))
