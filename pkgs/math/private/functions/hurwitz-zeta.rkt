#lang typed/racket/base

;; Computes the Hurwitz zeta function for s >= 1, q >= 0
;; Maximum observed error is 6 ulps, but is usually <= 2 ulps

(require racket/fixnum
         "../../flonum.rkt")

(provide hurwitz-zeta flhurwitz-zeta)

(: series-multiplier (Flonum Flonum -> Flonum))
;; To avoid adding subnormals and to correctly output zero when (hurwitz-zeta s q) is very small,
;; we compute every series multiplied by a^-s, then multiply the sum by a^s, where a = 
;; (series-multiplier s q)
;; a = 1.0 in the majority of the domain, but a < 1.0 near the upper-right curve where
;; hzeta(s, q) ~ 0.0
(define (series-multiplier s q)
  (cond [(q . < . 10.0)  1.0]
        [else
         ;; Initial estimate: solve (a*q)^-s = 1e-300 for `a'
         (define a (flmin 1.0 (fl/ (flexpt 1e-300 (fl/ -1.0 s)) q)))
         ;; Round to nearest power of 2 so multiplying by `a' will be exact
         (define pow2 (flmax -1022.0 (flfloor (fl/ (fllog a) (fllog 2.0)))))
         (flexpt 2.0 pow2)]))

(: flhurwitz-zeta-series (Flonum Flonum Flonum -> Flonum))
;; Series is accurate when computing no more than about 100 terms
;; Can be *extremely* slow, so we limit to 50 terms and make sure it doesn't happen
;; Measured error is <= 2 ulps when i > 50 doesn't happen
(define (flhurwitz-zeta-series s q a)
  (* (flexpt a s)
     (let: loop : Flonum ([i : Flonum  0.0] [y : Flonum  0.0])
       (define dy (flexpt (* a (fl+ q i)) (- s)))
       (define new-y (fl+ y dy))
       (cond [(or (i . > . 50.0)
                  ((flabs dy) . fl<= . (fl* (fl* 0.5 epsilon.0) new-y))
                  (not (rational? new-y)))
              new-y]
             [else
              (loop (fl+ i 1.0) new-y)]))))

;; Generated using math/number-theory and math/bigfloat, so that
;; (flvector-ref B2k/2k!s k) = (fl (/ (bernoulli (* 2 k)) (factorial (* 2 k))))
(define B2k/2k!s
  (flvector
   1.0
   +0.08333333333333333333333333333333333333346
   -0.001388888888888888888888888888888888888889
   +3.306878306878306878306878306878306878300e-5
   -8.267195767195767195767195767195767195779e-7
   +2.087675698786809897921009032120143231254e-8
   -5.284190138687493184847682202179556676918e-10
   +1.338253653068467883282698097512912327728e-11
   -3.389680296322582866830195391249442499571e-13
   +8.586062056277844564135905450425627133953e-15
   -2.174868698558061873041516423865917899850e-16
   +5.509002828360229515202652608902254877855e-18
   -1.395446468581252334070768626406354976390e-19
   +3.534707039629467471693229977803799214733e-21
   -8.953517427037546850402611318112741051624e-23
   +2.267952452337683060310950738868166063219e-24
   -5.744790668872202445263881987607018399621e-26
   +1.455172475614864901866264867271329335720e-27
   -3.685994940665310178181782479908660374450e-29
   +9.336734257095044672032555152785623295444e-31
   -2.365022415700629934559635196369838240069e-32
   +5.990671762482134304659912396819657826456e-34
   -1.517454884468290261710813135864718931538e-35
   +3.843758125454188232229445290990232105899e-37
   -9.736353072646691035267621279250454180943e-39))

(define n 6)
(define max-k (assert (- (flvector-length B2k/2k!s) 1) index?))

(: flhurwitz-zeta-asym (Flonum Flonum Flonum -> Flonum))
;; Derived from Euler-MacLaurin with n = 0 and max-k = 1
;; Measured error is <= 2 ulp for q > 1e5
(define (flhurwitz-zeta-asym s q a)
  (define fn (flexpt (* a q) (- s)))
  (define c0 (flvector-ref B2k/2k!s 1))
  (define y
    (cond [(q . fl> . 1e300)
           (fl+ (fl+ (fl* fn 0.5)
                     (fl/ (fl* fn q) (fl- s 1.0)))
                (fl* (fl* fn (fl/ s q)) c0))]
          [else
           (fl* fn (fl+ (fl+ 0.5 (fl/ q (fl- s 1.0)))
                        (fl* (fl/ s q) c0)))]))
  (fl* (flexpt a s) y))

(: flhurwitz-zeta-euler-maclaurin (Flonum Flonum Flonum -> Flonum))
(define (flhurwitz-zeta-euler-maclaurin s q a)
  (define fn (flexpt (* a (fl+ (fl n) q)) (- s)))
  (define n+q (fl+ (fl n) q))
  (define sqr-n+q (fl* n+q n+q))
  ;; Sum finite series part, plus misc terms
  (define y0
    (let: loop : Flonum ([y : Flonum  (fl* fn (fl+ (fl/ n+q (fl- s 1.0)) 0.5))]
                         [k : Nonnegative-Fixnum  0])
      (cond [(k . fx< . n)
             (define dy (flexpt (* a (fl+ (fl k) q)) (- s)))
             (define new-y (fl+ y dy))
             (cond [((flabs dy) . fl<= . (fl* (fl* 0.5 epsilon.0) (flabs new-y)))  new-y]
                   [else  (loop new-y (fx+ k 1))])]
            [else  y])))
  ;; Sum part of the asymptotic series
  (define y1
    (let: loop : Flonum ([y : Flonum  y0]
                         [t : Flonum  (fl/ (fl* s fn) n+q)]
                         [k : Nonnegative-Fixnum  0])
      (cond [(k . fx< . max-k)
             (define dy (fl* t (flvector-ref B2k/2k!s (fx+ k 1))))
             (define new-y (fl+ y dy))
             (cond [((flabs dy) . fl<= . (fl* (fl* 0.5 epsilon.0) (flabs new-y)))
                    new-y]
                   [else
                    (define 2k (fl* 2.0 (fl k)))
                    (define z (fl* (fl+ s (fl+ 2k 1.0)) (fl+ s (fl+ 2k 2.0))))
                    (loop new-y
                          (fl/ (fl* t z) sqr-n+q)
                          (fx+ k 1))])]
            [else  y])))
  (fl* (flexpt a s) y1))

(: flhurwitz-zeta (Flonum Flonum -> Flonum))
(define (flhurwitz-zeta s q)
  (cond [(s . fl<= . 1.0)  (if (fl= s 1.0) +inf.0 +nan.0)]
        [(q . fl<= . 0.0)  (if (fl= q 0.0) +inf.0 +nan.0)]
        [else
         (define a (series-multiplier s q))
         (cond
           [(fl= 0.0 (flexpt (* a q) (- s)))
            ;; The scaled leading series term is zero, so all further terms will be zero
            ;; Therefore, using either method is going to fail to produce anything but 0.0
            0.0]
           [(q . fl> . 1e5)
            ;; Experimentally found threshold
            (flhurwitz-zeta-asym s q a)]
           [(s . fl> . (fl+ (fl* 2.0 q) 15.0))
            ;; Determined experimentally that the series computes fewer total iterations here
            (flhurwitz-zeta-series s q a)]
           [else
            (flhurwitz-zeta-euler-maclaurin s q a)])]))

(: hurwitz-zeta (case-> (Flonum Flonum -> Flonum)
                        (Real Real -> Real)))
(define (hurwitz-zeta s q)
  (cond [(and (exact? s) (s . <= . 1))
         (raise-argument-error 'hurwitz-zeta "Real > 1" 0 s q)]
        [(and (exact? q) (q . <= . 0))
         (raise-argument-error 'hurwitz-zeta "Positive-Real" 1 s q)]
        [else  (flhurwitz-zeta (fl s) (fl q))]))
