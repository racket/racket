#lang typed/racket/base

(require (for-syntax racket/base syntax/parse)
         racket/vector
         "../../base.rkt"
         "../../bigfloat.rkt"
         "../flonum/flonum-functions.rkt"
         "../unsafe.rkt")

(provide chebyshev-poly chebyshev-poly? chebyshev-poly-min chebyshev-poly-max chebyshev-poly-coefs
         chebyshev-poly-order chebyshev-poly-convert
         build-chebyshev-poly build-chebyshev-flpoly build-chebyshev-bfpoly
         chebyshev-poly-fun chebyshev-flpoly-fun chebyshev-bfpoly-fun
         inline-chebyshev-flpoly-fun)

(struct: (A) chebyshev-poly ([min : A] [max : A] [coefs : (Vectorof A)]) #:transparent)

(: chebyshev-poly-order (All (A) ((chebyshev-poly A) -> Index)))
(define (chebyshev-poly-order p)
  (define n (vector-length (chebyshev-poly-coefs p)))
  (if (zero? n) 0 (- n 1)))

(: chebyshev-poly-convert (All (A B) ((chebyshev-poly A) (A -> B) -> (chebyshev-poly B))))
(define (chebyshev-poly-convert p conv)
  (define mn (chebyshev-poly-min p))
  (define mx (chebyshev-poly-max p))
  (define cs (chebyshev-poly-coefs p))
  (chebyshev-poly (conv mn) (conv mx) (vector-map conv cs)))

#;
(: make-build-chebyshev-poly
   (All (A) ((A A -> A) (A A -> A) (A A -> A) (A A -> A) (A -> A) (Integer -> A) (-> A)
                        -> (A A Integer (A -> A) -> (chebyshev-poly A)))))
(define-syntax-rule (make-build-chebyshev-poly A num+ num- num* num/ numcos int->num pi-thnk)
  (λ: ([mn : A] [mx : A] [n : Integer] [f : (A -> A)])
    (define 0.num (int->num 0))
    (define 1.num (int->num 1))
    (define 2.num (int->num 2))
    (define 1/2.num (num/ 1.num 2.num))
    (define pi.num (pi-thnk))
    (define mx-mn (num- mx mn))
    (define mn+mx (num+ mn mx))
    (let ([n.num  (int->num n)])
      (define norm (num/ 2.num n.num))
      (define cs
        (for/vector: #:length n ([j  (in-range n)]) : A
          (let ([j  (int->num j)])
            (num* (for/fold: ([cj : A  0.num]) ([k  (in-range n)])
                    (define θ (num/ (num* pi.num (num+ (int->num k) 1/2.num)) n.num))
                    (define x (num* 1/2.num (num+ (num* mx-mn (numcos θ)) mn+mx)))
                    (num+ cj (num* (f x) (numcos (num* θ j)))))
                  norm))))
      (chebyshev-poly mn mx cs))))

#;
(: make-chebyshev-poly-fun
   (All (A) ((A A -> A) (A A -> A) (A A -> A) (A A -> A) (Integer -> A)
                        -> ((chebyshev-poly A) -> (A -> A)))))
(define-syntax-rule (make-chebyshev-poly-fun A num+ num- num* num/ int->num)
  (λ: ([p : (chebyshev-poly A)])
    (define cs (chebyshev-poly-coefs p))
    (define n (vector-length cs))
    (define 0.num (int->num 0))
    (define 2.num (int->num 2))
    (cond
      [(zero? n)  (λ: ([x : A]) 0.num)]
      [else
       (define mn (chebyshev-poly-min p))
       (define mx (chebyshev-poly-max p))
       (λ: ([x : A])
         (define i (- n 1))
         (define c (unsafe-vector-ref cs i))
         (define y (num/ (num- (num* x 2.num) (num+ mn mx))
                         (num- mx mn)))
         (define y2 (num* y 2.num))
         (let: loop : A ([i : Nonnegative-Fixnum  i]
                         [c : A  c]
                         [d : A  0.num]
                         [dd : A  0.num])
           (cond [(zero? i)  (num+ (num* y d) (num- (num/ c 2.num) dd))]
                 [else
                  (let ([d   (num+ (num* y2 d) (num- c dd))]
                        [dd  d])
                    (loop (- i 1) (unsafe-vector-ref cs (- i 1)) d dd))])))])))

(define build-chebyshev-poly
  (make-build-chebyshev-poly Real + - * / cos (λ (x) x) (λ () pi)))

(define build-chebyshev-flpoly
  (make-build-chebyshev-poly Float fl+ fl- fl* fl/ flcos ->fl (λ () pi)))

(define build-chebyshev-bfpoly
  (make-build-chebyshev-poly Bigfloat bf+ bf- bf* bf/ bfcos bf (λ () pi.bf)))

(define chebyshev-poly-fun
  (make-chebyshev-poly-fun Real + - * / (λ (x) x)))

(define chebyshev-flpoly-fun
  (make-chebyshev-poly-fun Float fl+ fl- fl* fl/ ->fl))

(define chebyshev-bfpoly-fun
  (make-chebyshev-poly-fun Bigfloat bf+ bf- bf* bf/ bf))

(define-syntax (chebyshev-iter stx)
  (syntax-case stx ()
    [(_ y y2 d dd ())  (syntax/loc stx d)]
    [(_ y y2 d dd (c0))
     (syntax/loc stx
       (fl+ (fl* y d) (fl- (fl/ c0 2.0) dd)))]
    [(_ y y2 d dd (c0 c ...))
     (syntax/loc stx
       (let ([d   (fl+ (fl* y2 d) (fl- c0 dd))]
             [dd  d])
         (chebyshev-iter y y2 d dd (c ...))))]))

(define-syntax (inline-chebyshev-flpoly-fun stx)
  (syntax-parse stx
    [(_ lower:expr upper:expr (c0:expr c:expr ...))
     (with-syntax ([(c0 c ...)  (reverse (syntax->list #'(c0 c ...)))])
       (syntax/loc stx
         (λ: ([z : Float])
           (define y (fl/ (fl- (fl+ z z) (fl+ lower upper))
                          (fl- upper lower)))
           (define y2 (fl+ y y))
           (let ([d   0.0]
                 [dd  0.0])
             (chebyshev-iter y y2 d dd (c ...))))))]))
