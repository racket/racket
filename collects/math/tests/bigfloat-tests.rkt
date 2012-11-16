#lang typed/racket

(require math/bigfloat
         math/flonum
         typed/rackunit)

;; ---------------------------------------------------------------------------------------------------
;; Exact

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
  
  ;; truncates to 600 digits (many other numbers of digits, correct rounding makes it look wrong)
  (check-equal? (substring (bigfloat->string (bfexp (bf 1))) 0 600)
                e-truncated600))

;; ---------------------------------------------------------------------------------------------------
;; Inexact

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
