#lang racket

(require math/bigfloat
         math/flonum
         math/base
         rackunit)

;; Exact tests

(check-= (bigfloat->integer (bf (expt 2 64)))
         (expt 2 64)  0)

(check-= (bigfloat->flonum (bf 123.456))
         123.456  0)

(check-= (bigfloat->rational (bf 123456/65536))
         123456/65536  0)

(check-= (bigfloat->rational (bf 123456/65536))
         123456/65536  0)

(check-equal? (bigfloat->string (bf "123456.25"))
              "123456.25")

(check-= (bigfloat->rational (bf+ (bf 23) (bf 1/16)))
         (+ 23 1/16)  0)

(check-= (bigfloat->rational (bf- (bf 23) (bf 1/16)))
         (- 23 1/16)  0)

(check-= (bigfloat->rational (bf* (bf 23) (bf 1/16)))
         (* 23 1/16)  0)

(check-= (bigfloat->rational (bf/ (bf 23) (bf 1/16)))
         (/ 23 1/16)  0)

(check bf= (bf 1) (bf 1.0))

;; exp(1) truncated to 600 digits
(parameterize ([bf-precision 2000])
  (define e-truncated600
    (string-append
     "2.718281828459045235360287471352662497757247093699959574966"
     "967627724076630353547594571382178525166427427466391932003059"
     "921817413596629043572900334295260595630738132328627943490763"
     "233829880753195251019011573834187930702154089149934884167509"
     "244761460668082264800168477411853742345442437107539077744992"
     "069551702761838606261331384583000752044933826560297606737113"
     "200709328709127443747047230696977209310141692836819025515108"
     "657463772111252389784425056953696770785449969967946864454905"
     "987931636889230098793127736178215424999229576351482208269895"
     "1936680331825288693984964651058209392398294887933203625094431"))
  
  ;; truncates to 600 digits (with other numbers of digits, correct rounding makes it look wrong)
  (check-equal? (substring (bigfloat->string (bfexp 1.bf)) 0 600)
                e-truncated600))

;; Inexact tests

(check-= (bigfloat->rational (bfsqr (bf 22/7)))
         (/ (sqr 22) (sqr 7))
         epsilon.0)

(check-= (bigfloat->flonum (bfsqrt (bf 2)))
         (sqrt 2)
         epsilon.0)

(check-= (bigfloat->flonum (bf1/sqrt (bf 2)))
         (/ 1 (sqrt 2))
         epsilon.0)

(check-= (bigfloat->flonum (bfcbrt (bf 2)))
         (flexpt 2.0 #i1/3)
         epsilon.0)

;; Constants

(check-equal? pi.bf (bf #e3.141592653589793238462643383279502884195))
(check-equal? gamma.bf (bf #e0.5772156649015328606065120900824024310432))
(check-equal? catalan.bf (bf #e0.9159655941772190150546035149323841107734))
(check-equal? log2.bf (bf #e0.6931471805599453094172321214581765680748))
(check-equal? phi.bf (bf #e1.61803398874989484820458683436563811772))

;; Special values

(check-eqv? (bigfloat->flonum +nan.bf) +nan.0)
(check-eqv? (bigfloat->flonum +inf.bf) +inf.0)
(check-eqv? (bigfloat->flonum -inf.bf) -inf.0)
(check-eqv? (bigfloat->flonum -0.bf) -0.0)
(check-eqv? (bigfloat->flonum 0.bf) 0.0)
(check-equal? (bf- +inf.bf) -inf.bf)
(check-equal? (bf- -inf.bf) +inf.bf)
(check-equal? (bf- +nan.bf) +nan.bf)
(check-equal? (bf- 0.bf) -0.bf)
(check-equal? (bf- -0.bf) 0.bf)

(for ([bits  '(2 4 8 16 32 64 128 3 5 7 11 13 129)])
  (parameterize ([bf-precision bits])
    ;; Epsilon
    (check-not-equal? (bf+ 1.bf epsilon.bf) 1.bf)
    (check-equal? (bf+ 1.bf (bf* (bf 0.5) epsilon.bf)) 1.bf)
    ;; +max.bf/-max.bf
    (check-equal? (bf- +max.bf) -max.bf)
    (check-equal? (bf+ +max.bf (bf* epsilon.bf +max.bf)) +inf.bf)
    (check-equal? (bf- -max.bf (bf* epsilon.bf +max.bf)) -inf.bf)
    (check-equal? (- (bigfloat->ordinal +inf.bf) 1) (bigfloat->ordinal +max.bf))
    (check-equal? (+ (bigfloat->ordinal -inf.bf) 1) (bigfloat->ordinal -max.bf))
    (check-equal? (bfnext +max.bf) +inf.bf)
    (check-equal? (bfprev +inf.bf) +max.bf)
    (check-equal? (bfprev -max.bf) -inf.bf)
    (check-equal? (bfnext -inf.bf) -max.bf)
    ;; +min.bf/-min.bf
    (check-equal? (bf- +min.bf) -min.bf)
    (check-equal? (bf* (bf 0.5) +min.bf)  0.bf)
    (check-equal? (bf* (bf 0.5) -min.bf) -0.bf)
    (check-equal? (bigfloat->ordinal +min.bf)  1)
    (check-equal? (bigfloat->ordinal -min.bf) -1)
    (check-equal? (bfprev +min.bf) 0.bf)
    (check-equal? (bfnext 0.bf) +min.bf)
    (check-equal? (bfnext -min.bf) -0.bf)
    (check-equal? (bfprev 0.bf) -min.bf)
    ))

;; Integer conversion

(check-equal? (bigfloat->integer (integer->bigfloat 0)) 0)

(for ([mode  (in-list '(nearest up down zero))])
  (define eps (if (eq? mode 'nearest) (bf* (bf 0.5) epsilon.bf) epsilon.bf))
  (parameterize ([bf-rounding-mode mode])
    (for* ([s  (in-list '(-1 1))]
           [i  (in-range 1000)])
      (define n (* s (expt 10 i)))
      (define n0 (bigfloat->integer (integer->bigfloat n)))
      (define err (bf (relative-error n0 n)))
      (unless (err . bf<= . eps)
        (printf "bad integer conversion: mode = ~v  n = ~v~n" mode n))
      (check-true (err . bf<= . eps)))))

;; Rational conversion

(for ([mode  (in-list '(nearest up down zero))])
  (parameterize ([bf-rounding-mode mode])
    (for* ([i  (in-range -37 37)]
           [j  (in-range 1 37)])
      (define q0 (bigfloat->rational (rational->bigfloat (/ i j))))
      (define q1 (bigfloat->rational (bf/ (bf i) (bf j))))
      (unless (equal? q0 q1)
        (printf "bad rational conversion: mode = ~v  q = ~v/~v~n" mode i j))
      (check-equal? q0 q1))))

;; Flonum conversion

(for ([mode  (in-list '(nearest up down zero))])
  (parameterize ([bf-rounding-mode mode])
    (for* ([i  (in-range -37 37)]
           [j  (in-range 1 37)])
      (define x (fl (/ i j)))
      (define x0 (bigfloat->flonum (flonum->bigfloat x)))
      (unless (equal? x x0)
        (printf "bad flonum conversion: mode = ~v  x = ~v~n" mode x))
      (check-equal? x x0))))

;; String conversion

(for ([mode  (in-list '(nearest up down zero))])
  (parameterize ([bf-rounding-mode mode])
    (for* ([i  (in-range -37 37)]
           [j  (in-range 1 37)])
      (define q (rational->bigfloat (/ i j)))
      (define q0 (string->bigfloat (bigfloat->string q)))
      (unless (equal? q q0)
        (printf "bad string conversion: mode = ~v  q = ~v/~v~n" mode i j))
      (check-equal? q q0))))

(parameterize ([bf-precision 53])
  (for ([f+xs+ys
         (list
          (list
           bfsqr
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list +inf.bf +inf.bf 1.bf 0.bf 0.bf 0.bf 0.bf 1.bf +inf.bf +inf.bf +nan.bf))
          (list
           bfsqrt
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf
                 +min.bf 1.bf +max.bf
                 +inf.bf +nan.bf)
           (list +nan.bf +nan.bf +nan.bf +nan.bf -0.bf 0.bf
                 (bf "4.8811524304081624e-161614249") 1.bf (bf "1.4486472022088012e161614248")
                 +inf.bf +nan.bf))
          (list
           bfcbrt
           (list -inf.bf
                 -max.bf -1.bf -min.bf
                 -0.bf 0.bf
                 +min.bf 1.bf +max.bf
                 +inf.bf +nan.bf)
           (list -inf.bf
                 (bf "-1.2802902004094324e107742832") -1.bf (bf "-6.1993798416193228e-107742833")
                 -0.bf 0.bf
                 (bf "6.1993798416193228e-107742833") 1.bf (bf "1.2802902004094324e107742832")
                 +inf.bf +nan.bf))
          (list
           bf-
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list +inf.bf +max.bf 1.bf +min.bf 0.bf -0.bf -min.bf -1.bf -max.bf -inf.bf +nan.bf))
          (list
           bf/
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list -0.bf (bf "-4.7651298097759032e-323228497") -1.bf -inf.bf -inf.bf
                 +inf.bf +inf.bf 1.bf (bf "4.7651298097759032e-323228497") 0.bf +nan.bf))
          (list
           bfabs
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list +inf.bf +max.bf 1.bf +min.bf 0.bf 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf))
          (list
           bflog
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf
                 +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list +nan.bf +nan.bf +nan.bf +nan.bf -inf.bf -inf.bf
                 (bf #e-744261117.95489299) 0.bf (bf #e744261117.26174581) +inf.bf +nan.bf))
          (list
           bflog2
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf
                 +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list +nan.bf +nan.bf +nan.bf +nan.bf -inf.bf -inf.bf
                 (bf -1073741824) 0.bf (bf 1073741823) +inf.bf +nan.bf))
          (list
           bflog10
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf
                 +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list +nan.bf +nan.bf +nan.bf +nan.bf -inf.bf -inf.bf
                 (bf #e-323228496.62295526) 0.bf (bf #e323228496.32192528) +inf.bf +nan.bf))
          (list
           bflog1p
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf +min.bf
                 1.bf +max.bf +inf.bf +nan.bf)
           (list +nan.bf +nan.bf -inf.bf -min.bf -0.bf 0.bf +min.bf
                 (bf #e0.69314718055994529) (bf #e744261117.26174581) +inf.bf +nan.bf))
          (list
           bfexp
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list 0.bf 0.bf (bf #e0.36787944117144233) 1.bf 1.bf
                 1.bf 1.bf (bf #e2.7182818284590451) +inf.bf +inf.bf +nan.bf))
          (list
           bfexp2
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list 0.bf 0.bf (bf #e0.5) 1.bf 1.bf 1.bf 1.bf 2.bf +inf.bf +inf.bf +nan.bf))
          (list
           bfexp10
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list 0.bf 0.bf (bf #e0.1) 1.bf 1.bf 1.bf 1.bf 10.bf +inf.bf +inf.bf +nan.bf))
          (list
           bfexpm1
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list -1.bf -1.bf (bf #e-0.63212055882855767) -min.bf -0.bf
                 0.bf +min.bf (bf #e1.7182818284590453) +inf.bf +inf.bf +nan.bf))
          (list
           bfsin
           (list -inf.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +inf.bf +nan.bf)
           (list +nan.bf (bf #e-0.8414709848078965) -min.bf -0.bf
                 0.bf +min.bf (bf #e0.8414709848078965) +nan.bf +nan.bf))
          (list
           bfcos
           (list -inf.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +inf.bf +nan.bf)
           (list +nan.bf (bf #e0.54030230586813977) 1.bf 1.bf
                 1.bf 1.bf (bf #e0.54030230586813977) +nan.bf +nan.bf))
          (list
           bftan
           (list -inf.bf -1.bf -min.bf -0.bf 0.bf +min.bf 1.bf +inf.bf +nan.bf)
           (list +nan.bf (bf #e-1.5574077246549023) -min.bf -0.bf
                 0.bf +min.bf (bf #e1.5574077246549023) +nan.bf +nan.bf))
          (list
           bfcsc
           (list -inf.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +inf.bf +nan.bf)
           (list +nan.bf (bf #e-1.1883951057781212) -inf.bf -inf.bf
                 +inf.bf +inf.bf (bf #e1.1883951057781212) +nan.bf +nan.bf))
          (list
           bfsec
           (list -inf.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +inf.bf +nan.bf)
           (list +nan.bf (bf #e1.8508157176809257) 1.bf 1.bf
                 1.bf 1.bf (bf #e1.8508157176809257) +nan.bf +nan.bf))
          (list
           bfcot
           (list -inf.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +inf.bf +nan.bf)
           (list +nan.bf (bf #e-0.64209261593433076) -inf.bf -inf.bf
                 +inf.bf +inf.bf (bf #e0.64209261593433076) +nan.bf +nan.bf))
          (list
           bfasin
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list +nan.bf +nan.bf (bf #e-1.5707963267948966) -min.bf -0.bf
                 0.bf +min.bf (bf #e1.5707963267948966) +nan.bf +nan.bf +nan.bf))
          (list
           bfacos
           (list -inf.bf -max.bf
                 -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf
                 +max.bf +inf.bf +nan.bf)
           (list +nan.bf +nan.bf
                 (bf #e3.1415926535897931) (bf #e1.5707963267948966) (bf #e1.5707963267948966)
                 (bf #e1.5707963267948966) (bf #e1.5707963267948966) 0.bf
                 +nan.bf +nan.bf +nan.bf))
          (list
           bfatan
           (list -inf.bf -max.bf
                 -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf
                 +max.bf +inf.bf +nan.bf)
           (list (bf #e-1.5707963267948966) (bf #e-1.5707963267948966)
                 (bf #e-0.78539816339744828) -min.bf -0.bf
                 0.bf +min.bf (bf #e0.78539816339744828)
                 (bf #e1.5707963267948966) (bf #e1.5707963267948966) +nan.bf))
          (list
           bfsinh
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list -inf.bf -inf.bf (bf #e-1.1752011936438014) -min.bf -0.bf
                 0.bf +min.bf (bf #e1.1752011936438014) +inf.bf +inf.bf +nan.bf))
          (list
           bfcosh
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list +inf.bf +inf.bf (bf #e1.5430806348152437) 1.bf 1.bf
                 1.bf 1.bf (bf #e1.5430806348152437) +inf.bf +inf.bf +nan.bf))
          (list
           bftanh
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list -1.bf -1.bf (bf #e-0.76159415595576485) -min.bf -0.bf
                 0.bf +min.bf (bf #e0.76159415595576485) 1.bf 1.bf +nan.bf))
          (list
           bfcsch
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list -0.bf -0.bf (bf #e-0.85091812823932156) -inf.bf -inf.bf
                 +inf.bf +inf.bf (bf #e0.85091812823932156) 0.bf 0.bf +nan.bf))
          (list
           bfsech
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list 0.bf 0.bf (bf #e0.64805427366388535) 1.bf 1.bf
                 1.bf 1.bf (bf #e0.64805427366388535) 0.bf 0.bf +nan.bf))
          (list
           bfcoth
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list -1.bf -1.bf (bf #e-1.3130352854993312) -inf.bf -inf.bf
                 +inf.bf +inf.bf (bf #e1.3130352854993312) 1.bf 1.bf +nan.bf))
          (list
           bfasinh
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list -inf.bf (bf #e-744261117.95489299) (bf #e-0.88137358701954305) -min.bf -0.bf
                 0.bf +min.bf (bf #e0.88137358701954305) (bf #e744261117.95489299) +inf.bf +nan.bf))
          (list
           bfacosh
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf +min.bf 1.bf
                 +max.bf +inf.bf +nan.bf)
           (list +nan.bf +nan.bf +nan.bf +nan.bf +nan.bf +nan.bf +nan.bf 0.bf
                 (bf #e744261117.95489299) +inf.bf +nan.bf))
          (list
           bfatanh
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list +nan.bf +nan.bf -inf.bf -min.bf -0.bf 0.bf +min.bf +inf.bf +nan.bf +nan.bf +nan.bf))
          (list
           bfeint
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf +min.bf
                 1.bf +max.bf +inf.bf +nan.bf)
           (list +nan.bf +nan.bf +nan.bf +nan.bf -inf.bf -inf.bf (bf #e-744261117.37767732)
                 (bf #e1.8951178163559368) +inf.bf +inf.bf +nan.bf))
          (list
           bfli2
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list -inf.bf (bf -276962305333851100) (bf #e-0.8224670334241132) -min.bf -0.bf
                 0.bf +min.bf (bf #e1.6449340668482264) (bf -276962305333851100) -inf.bf +nan.bf))
          (list
           bfgamma
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf +min.bf 1.bf +max.bf +inf.bf
                 +nan.bf)
           (list +nan.bf +nan.bf +nan.bf -inf.bf -inf.bf +inf.bf +inf.bf 1.bf +inf.bf +inf.bf
                 +nan.bf))
          #;
          (list
           bfpsi0
           (list -inf.bf -1.bf -min.bf -0.bf 0.bf +min.bf
                 1.bf +inf.bf +nan.bf)
           (list +nan.bf +nan.bf +inf.bf +inf.bf -inf.bf -inf.bf
                 (bf #e-0.57721566490153287) +inf.bf +nan.bf))
          (list
           bfzeta
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list +nan.bf 0.bf (bf #e-0.083333333333333329) (bf #e-0.5) (bf #e-0.5)
                 (bf #e-0.5) (bf #e-0.5) +inf.bf 1.bf 1.bf +nan.bf))
          (list
           bferf
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +max.bf +inf.bf
                 +nan.bf)
           (list -1.bf -1.bf (bf #e-0.84270079294971489) (bf "-2.6884366029284653e-323228497") -0.bf
                 0.bf (bf "2.6884366029284653e-323228497") (bf #e0.84270079294971489) 1.bf 1.bf
                 +nan.bf))
          (list
           bferfc
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 
                 0.bf +min.bf 1.bf +max.bf +inf.bf +nan.bf)
           (list 2.bf 2.bf (bf #e1.8427007929497148) 1.bf 1.bf
                 1.bf 1.bf (bf #e0.15729920705028513) 0.bf 0.bf +nan.bf))
          (list
           bfbesj0
           (list -inf.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +inf.bf +nan.bf)
           (list 0.bf (bf #e0.76519768655796661) 1.bf 1.bf
                 1.bf 1.bf (bf #e0.76519768655796661) 0.bf +nan.bf))
          (list
           bfbesj1
           (list -inf.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +inf.bf +nan.bf)
           (list 0.bf (bf #e-0.4400505857449335) -0.bf -0.bf
                 0.bf 0.bf (bf #e0.4400505857449335) 0.bf +nan.bf))
          (list
           (λ (x) (bfbesj 0 x))
           (list -inf.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +inf.bf +nan.bf)
           (list 0.bf (bf #e0.76519768655796661) 1.bf 1.bf
                 1.bf 1.bf (bf #e0.76519768655796661) 0.bf +nan.bf))
          (list
           (λ (x) (bfbesj 1 x))
           (list -inf.bf -1.bf -min.bf -0.bf
                 0.bf +min.bf 1.bf +inf.bf +nan.bf)
           (list 0.bf (bf #e-0.4400505857449335) -0.bf -0.bf
                 0.bf 0.bf (bf #e0.4400505857449335) 0.bf +nan.bf))
          (list
           bfbesy0
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf
                 +min.bf 1.bf +inf.bf +nan.bf)
           (list +nan.bf +nan.bf +nan.bf +nan.bf -inf.bf -inf.bf
                 (bf #e-473811343.56828988) (bf #e0.088256964215676956) 0.bf +nan.bf))
          (list
           bfbesy1
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf +min.bf
                 1.bf +inf.bf +nan.bf)
           (list +nan.bf +nan.bf +nan.bf +nan.bf -inf.bf -inf.bf -inf.bf
                 (bf #e-0.78121282130028868) 0.bf +nan.bf))
          (list
           (λ (x) (bfbesy 0 x))
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf
                 +min.bf 1.bf +inf.bf +nan.bf)
           (list +nan.bf +nan.bf +nan.bf +nan.bf -inf.bf -inf.bf
                 (bf #e-473811343.56828988) (bf #e0.088256964215676956) 0.bf +nan.bf))
          (list
           (λ (x) (bfbesy 1 x))
           (list -inf.bf -max.bf -1.bf -min.bf -0.bf 0.bf +min.bf
                 1.bf +inf.bf +nan.bf)
           (list +nan.bf +nan.bf +nan.bf +nan.bf -inf.bf -inf.bf -inf.bf
                 (bf #e-0.78121282130028868) 0.bf +nan.bf)))])
    (match-define (list f xs ys) f+xs+ys)
    (for ([x  (in-list xs)]
          [y  (in-list ys)])
      (define y0 (f x))
      (unless (equal? y y0)
        (printf "f = ~a  x = ~v  y = ~v~n" f x y))
      (check-equal? y0 y))))
