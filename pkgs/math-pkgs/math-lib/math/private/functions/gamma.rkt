#lang typed/racket/base

#|
Compute Gamma(x) for +flgamma-min.0 <= x <= +flgamma-max.0 and non-integer x <= -flgamma-min.0

Identities:
 * Gamma(x) = x * Gamma(x-1)         [definition]
 * Gamma(x) = (x-1)!, integer x > 0  [definition]
 * Gamma(x) = Gamma(x/2) * Gamma(x/2 + 1/2) * 2^(x-1) / sqrt(pi)  [doubling formula]

Floating-point design choices:
 * Gamma(x) = +inf.0, x = +0.0
 * Gamma(x) = -inf.0, x = -0.0
 * Gamma(x) = +nan.0, integer x < 0.0 or x = -inf.0

Approximations:
 * A Lanczos polynomial approximation cribbed from the Boost library
 * Laurent expansion at 0
 * Taylor expansion at 1
 * Gamma(x) ~ 0.0, non-integer x < 184
|#

(require racket/fixnum
         (only-in racket/math exact-truncate)
         "../../flonum.rkt"
         "../../base.rkt"
         "../number-theory/factorial.rkt"
         "lanczos.rkt")

(provide gamma flgamma)

(define +flgamma-max.0 171.6243769563027)
(define +flgamma-min.0 5.56268464626801e-309)
;; Gamma(x) ~ 1/x around very small numbers
(define -flgamma-min.0 (- +flgamma-min.0))

(define flonum-fact-table-size 171.0)
(define flonum-fact-table
  (build-flvector (fl->fx flonum-fact-table-size)
                  (compose fl factorial)))

(: laurent-sum-0.00 (Float -> Float))
;; Laurent expansion for -0.001 <= x <= 0.01
(define (laurent-sum-0.00 x)
  (fl+ (fl+ (fl/ 1.0 x)
            (- gamma.0))
       (fl* x ((make-flpolyfun
                (+0.989055995327972555395395651500634707939184
                 -0.907479076080886289016560167356275114928611
                 +0.981728086834400187336380294021850850360574
                 -0.981995068903145202104701413791374675517427
                 +0.993149114621276193153867253328658498037491
                 -0.996001760442431533970078419664566686735299))
               x))))

(: taylor-sum-0.50 (Float -> Float))
;; Taylor expansion for 0.4 <= x <= 0.6
(define (taylor-sum-0.50 x)
  ((make-flpolyfun
    (+1.7724538509055160272981674833411451827976e0
     -3.4802309069132620269385951981443497500324e0
     +7.7900887212031263903372656425114121857627e0
     -1.5794767051535797204049615197802243695634e1
     +3.1878824821160837494175894816112256439409e1
     -6.3912695746921383395241144275057530819766e1
     +1.2794261210147711936949066602575334975374e2
     -2.5596122807406396899948543419353676781796e2
     +5.1197413136103945040675826840454682812852e2
     -1.0239827076083058300636481981776144171184e3
     +2.0479884602473964585839918538211131834179e3
     -4.0959923009175214003253088852154617518402e3
     +8.1919948651220466254256995848376321606002e3
     -1.6383996575849087970263593129045274051309e4
     +3.2767997716877703521062014120251608738987e4
     -6.5535998477775549290980669866329155027290e4
     +1.3107199898512658288491710421482620707257e5
     -2.6214399932339484788985894301333677175822e5
     +5.2428799954892074657103698368248495340879e5
     -1.0485759996992768355432014911781756798133e6
     +2.0971519997995164251845178211583153825802e6
     -4.1943039998663436972876236332679314277175e6
     +8.3886079999108955636973605473367056799677e6
     -1.6777215999940596948659351797275861694373e7
     +3.3554431999960397928248461161824067545415e7
     -6.7108863999973598603821896375158511265925e7))
   (fl- x 0.5)))

(: taylor-sum-0.75 (Float -> Float))
;; Taylor expansion for 0.6 <= x <= 0.9
(define (taylor-sum-0.75 x)
  ((make-flpolyfun
    (+1.2254167024651776451290983033628905268512e0
     -1.3306320586438753973158461714125306931800e0
     +2.2798715368921053860231453979712247015479e0
     -3.0356325054764770273358473132729245991211e0
     +4.1639356827972145971295506590931882302017e0
     -5.5828354599031279709094914618820777254957e0
     +7.4725560147638094113271595546749768089882e0
     -9.9773913436472698249761366263115873834879e0
     +1.3311875532717127406341612389831215273413e1
     -1.7754030284164089293469539018644025571507e1
     +2.3674855647597215599782047621734873185439e1
     -3.1568082210708622762952116316633632133940e1
     +4.2091697406490991625454319348036323661887e1
     -5.6122789967429137505629713694791499592973e1
     +7.4830687842248860777520911986043344973951e1
     -9.9774422649289317880735163267681771600803e1
     +1.3303266195453161667765264326398808665282e2
     -1.7737693885659633699219673616639872019028e2
     +2.3650261728872409183156067714435259160505e2
     -3.1533684142235722118226966734052049763571e2
     +4.2044913239448573163503097252418185989689e2
     -5.6059884919167464502416194940242251803045e2
     +7.4746513568364145256740278829357382166450e2
     -9.9662018287044384491125019062792037807204e2
     +1.3288269116133169880999542840071640880826e3
     -1.7717692161240780212684141496992227705508e3))
   (fl- x 0.75)))

(: taylor-sum-1.00 (Float -> Float))
;; Taylor expansion for 0.8 <= x <= 1.2
(define (taylor-sum-1.00 x)
  ((make-flpolyfun
    (+1.0
     -0.57721566490153286060651209008240243104216
     +0.98905599532797255539539565150063470793918
     -0.90747907608088628901656016735627511492861
     +0.98172808683440018733638029402185085036057
     -0.98199506890314520210470141379137467551742
     +0.99314911462127619315386725332865849803748
     -0.99600176044243153397007841966456668673529
     +0.99810569378312892197857540308836723752396
     -0.99902526762195486779467805964888808853230
     +0.99951565607277744106705087759437019443449
     -0.99975659750860128702584244914060923599695
     +0.99987827131513327572617164259000321938762
     -0.99993906420644431683585223136895513185793
     +0.99996951776348210449861140509195350726552
     -0.99998475269937704874370963172444753832607
     +0.99999237447907321585539509450510782583380
     -0.99999618658947331202896495779561431380200
     +0.99999809308113089205186619151459489773168
     -0.99999904646891115771748687947054372632469
     +0.99999952321060573957523929299106456816808
     -0.99999976159734438057092470106258744748608
     +0.99999988079601916841665041840424924052652
     -0.99999994039712498374586288797675081784805
     +0.99999997019826758235557449619251141981337))
   (fl- x 1.0)))

(: taylor-sum-1.50 (Float -> Float))
;; Taylor expansion for 1.15 <= x <= 1.85
(define (taylor-sum-1.50 x)
  ((make-flpolyfun
    (+8.8622692545275801364908374167057259139877e-1
     +3.2338397448885013828869884268970307781332e-2
     +4.1481345368830116823003762311135634284890e-1
     -1.0729480456477221168754195638970966205456e-1
     +1.4464535904462154303833221025388452407000e-1
     -7.7523052299854203444677321416508970474227e-2
     +5.8610303817176289504188737819144057105466e-2
     -3.8001935554865130252051071015034155238048e-2
     +2.5837606455756203893700008736646246296106e-2
     -1.7222443113464625065830684260380430697612e-2
     +1.1522515392399228347728732942174590531578e-2
     -7.6902113642415786625887866176925021602843e-3
     +5.1316435019123875409072033543284598876153e-3
     -3.4228024973597060969796850048650543050513e-3
     +2.2825897637902674139310805303181845131811e-3
     -1.5220100711244283208129129687746583315715e-3
     +1.0147877421514778822410839485089945635144e-3
     -6.7657084106001236729184217880653696383285e-4
     +4.5106552539565954882790570494617826137956e-4
     -3.0071767120056376190660288649784379792333e-4
     +2.0048137704905741940098201147684156999930e-4
     -1.3365542345929399547565033127852991401613e-4
     +8.9104084561056640400421412266385236552344e-5
     -5.9402910632315351301225167218583393338649e-5
     +3.9602015464878783636172078334875896543825e-5
     -2.6401373662487025755188087547866875906025e-5))
   (fl- x 1.5)))

(: flgamma-integer (Float -> Float))
;; Computes Gamma(x) using factorial
(define (flgamma-integer x)
  (cond [(x . fl>= . 1.0)
         (cond [(x . fl<= . flonum-fact-table-size)
                (flvector-ref flonum-fact-table (fx- (fl->fx x) 1))]
               ;; 171! won't fit in a Float
               [else  +inf.0])]
        ;; Gamma(x) undefined for integer x <= 0; just need to determine which special to return
        [(equal? x -0.0)  -inf.0]
        [(equal? x +0.0)  +inf.0]
        [else  +nan.0]))

(: flgamma-large-negative (Float -> Float))
;; Computes Gamma(x) for non-integer x < -170
(define (flgamma-large-negative x)
  (cond [(x . fl< . -184.0)
         ;; Gamma(x) ~ 0.0 for non-integer x < -184; determine sign
         (if (even? (exact-truncate x)) -0.0 0.0)]
        [else
         ;; The standard argument reduction is horrible with -184 < x < -170
         ;; Fortunately, the doubling formula is great in this subdomain
         (fl* (fl* (fl* (flgamma (fl* 0.5 x))
                        (flgamma (fl+ (fl* 0.5 x) 0.5)))
                   (flexpt 2.0 (fl- x 1.0)))
              (fl/ 1.0 (flsqrt pi)))]))

(: flgamma-taylor (Float -> Float))
;; Computes Gamma(x) using Taylor expansion
;; Error is ~ 0.0 when 0.5 <= x <= 1.5
(define (flgamma-taylor x)
  (let loop ([x x] [y 1.0])
    (cond [(x . fl> . 1.5)  (loop (fl- x 1.0) (fl* y (fl- x 1.0)))]
          [(x . fl< . 0.5)  (loop (fl+ x 1.0) (fl/ y x))]
          [(x . fl< . 0.6)  (fl* y (taylor-sum-0.50 x))]
          [(x . fl< . 0.85)  (fl* y (taylor-sum-0.75 x))]
          [(x . fl< . 1.175)  (fl* y (taylor-sum-1.00 x))]
          [else  (fl* y (taylor-sum-1.50 x))])))

(: flgamma-reduce-negative (Float Float -> (Values Float Float)))
;; Argument reduction with Gamma(x-1) = Gamma(x) / x; used when x is a small negative number
(define (flgamma-reduce-negative x mx)
  (let loop ([x x] [y 1.0])
    (cond [(x . fl<= . mx)  (loop (fl+ x 1.0) (fl/ y x))]
          [else  (values x y)])))

(: flgamma-laurent (Float -> Float))
;; Calculates Gamma(x) using Laurent expansion
;; Error is ~ 0.0 when -0.001 < x < 0.01
(define (flgamma-laurent x)
  ;(printf "laurent ~v~n" x)
  (let-values ([(x y)  (flgamma-reduce-negative x -0.5)])
    (fl* y (laurent-sum-0.00 x))))

(: flgamma-lanczos (Float -> Float))
;; Computes Gamma(x) using a Lanczos approximation
(define (flgamma-lanczos x)
  ;(printf "lanczos ~v~n" x)
  (let*-values ([(x y)  (flgamma-reduce-negative x 0.0)]
                [(y)    (fl* y (lanczos-sum x))])
    (cond [(x . fl> . 140.0)
           (define xgh (fl+ x (fl- lanczos-g 0.5)))
           (define hp (flexpt xgh (fl- (fl* x 0.5) 0.25)))
           (fl* (fl* y (fl/ hp (flexp xgh))) hp)]
          [else
           (define xgh (fl+ x (fl- lanczos-g 0.5)))
           (fl* y (fl/ (flexpt xgh (fl- x 0.5)) (flexp xgh)))])))

(define: flgamma-hash : (HashTable Float Float) (make-weak-hash))

(: flgamma (Float -> Float))
(define (flgamma x)
  (cond [(integer? x)  (flgamma-integer x)]
        ;; Lanczos produces +nan.0 for huge inputs; avoid
        [(x . fl> . +flgamma-max.0)  +inf.0]
        ;; Limit as x -> -inf doesn't exist
        [(x . fl= . -inf.0)  +nan.0]
        [(eqv? x +nan.0)   +nan.0]
        [else
         (hash-ref!
          flgamma-hash x
          (λ ()
            (cond [(x . fl< . -170.0)  (flgamma-large-negative x)]
                  ;; If near a pole, use Laurent
                  [(and (x . fl< . 0.5)
                        (let ([dx  (fl- x (flround x))])
                          (and (dx . fl> . -0.001) (dx . fl< . 0.01))))
                   (flgamma-laurent x)]
                  ;; If small, use Taylor
                  [(and (x . fl> . -4.5) (x . fl< . 4.5))  (flgamma-taylor x)]
                  [else  (flgamma-lanczos x)])))]))

(: gamma (case-> (One -> One)
                 (Integer -> Positive-Integer)
                 (Float -> Float)
                 (Real -> (U Positive-Integer Flonum))))
(define (gamma x)
  (cond [(double-flonum? x)  (flgamma x)]
        [(exact-integer? x)
         (cond [(x . > . 0)  (factorial (- x 1))]
               [else  (raise-argument-error 'gamma "Real, not Zero or Negative-Integer" x)])]
        [else  (flgamma (fl x))]))
