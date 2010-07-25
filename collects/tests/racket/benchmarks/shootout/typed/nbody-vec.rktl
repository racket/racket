;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; Imperative-style implementation based on the SBCL implementation by
;; Patrick Frankenberger and Juho Snellman, but using only native Scheme
;; idioms like 'named let' and 'do' special form.
;;
;; Contributed by Anthony Borla, then converted for Racket
;; by Matthew Flatt and Brent Fulgham
;; Made unsafe and optimized by Sam TH
#|
Correct output N = 1000 is

-0.169075164
-0.169087605
|#

(require racket/cmdline racket/require 
         (only-in racket/flonum flvector)
	 (for-syntax racket/base)
	 (filtered-in
	  (lambda (name)
	    (regexp-replace
	     #rx"unsafe-fl" name "fl"))
	  racket/unsafe/ops))

;; ------------------------------
;; define planetary masses, initial positions & velocity

(define +pi+ 3.141592653589793) ;; define locally to enable inlining
(define +days-per-year+ 365.24)

(define +solar-mass+ (* 4.0 +pi+ +pi+))

(define +dt+ 0.01)

(define make-body flvector)
(define-syntax-rule (deffield n getter setter)
  (begin (define-syntax-rule (getter b) (flvector-ref b n))
         (define-syntax-rule (setter b x) (flvector-set! b n x))))
(deffield 0 body-x set-body-x!)
(deffield 1 body-y set-body-y!)
(deffield 2 body-z set-body-z!)
(deffield 3 body-vx set-body-vx!)
(deffield 4 body-vy set-body-vy!)
(deffield 5 body-vz set-body-vz!)
(deffield 6 body-mass set-body-mass!)

(define *sun*
  (make-body 0.0 0.0 0.0 0.0 0.0 0.0 +solar-mass+))

(define *jupiter*
  (make-body 4.84143144246472090
             -1.16032004402742839
             -1.03622044471123109e-1
             (fl* 1.66007664274403694e-3 +days-per-year+)
             (fl* 7.69901118419740425e-3 +days-per-year+)
             (fl* -6.90460016972063023e-5 +days-per-year+)
             (fl* 9.54791938424326609e-4 +solar-mass+)))

(define *saturn*
  (make-body 8.34336671824457987
             4.12479856412430479
             -4.03523417114321381e-1
             (fl* -2.76742510726862411e-3 +days-per-year+)
             (fl* 4.99852801234917238e-3 +days-per-year+)
             (fl* 2.30417297573763929e-5 +days-per-year+)
             (fl* 2.85885980666130812e-4 +solar-mass+)))

(define *uranus*
  (make-body 1.28943695621391310e1
             -1.51111514016986312e1
             -2.23307578892655734e-1
             (fl* 2.96460137564761618e-03 +days-per-year+)
             (fl* 2.37847173959480950e-03 +days-per-year+)
             (fl* -2.96589568540237556e-05 +days-per-year+)
             (fl*  4.36624404335156298e-05 +solar-mass+)))

(define *neptune*
  (make-body 1.53796971148509165e+01
             -2.59193146099879641e+01
             1.79258772950371181e-01
             (fl* 2.68067772490389322e-03 +days-per-year+)
             (fl* 1.62824170038242295e-03 +days-per-year+)
             (fl* -9.51592254519715870e-05 +days-per-year+)
             (fl* 5.15138902046611451e-05 +solar-mass+)))

(define *system* (vector *sun* *jupiter* *saturn* *uranus* *neptune*))
(define *system-size* 5)
;; -------------------------------
(: offset-momentum ( -> Void))
(define (offset-momentum)
  (let loop-i ([i 0] [px 0.0] [py 0.0] [pz 0.0])
    (if (unsafe-fx= i *system-size*)
      (begin
        (set-body-vx! (unsafe-vector-ref *system* 0) (fl/ (fl- 0.0 px) +solar-mass+))
        (set-body-vy! (unsafe-vector-ref *system* 0) (fl/ (fl- 0.0 py) +solar-mass+))
        (set-body-vz! (unsafe-vector-ref *system* 0) (fl/ (fl- 0.0 pz) +solar-mass+)))
      (let ([i1 (unsafe-vector-ref *system* i)])
        (loop-i (unsafe-fx+ i 1)
                (fl+ px (fl* (body-vx i1) (body-mass i1)))
                (fl+ py (fl* (body-vy i1) (body-mass i1)))
                (fl+ pz (fl* (body-vz i1) (body-mass i1))))))))

;; -------------------------------
(: energy ( -> Float))
(define (energy)
  (let loop-o ([o 0] [e 0.0])
    (if (unsafe-fx= o *system-size*)
      e
      (let* ([o1 (unsafe-vector-ref *system* o)]
             [e (fl+ e (fl* (fl* 0.5 (body-mass o1))
                        (fl+ (fl+ (fl* (body-vx o1) (body-vx o1))
                              (fl* (body-vy o1) (body-vy o1)))
                           (fl* (body-vz o1) (body-vz o1)))))])
        (let loop-i ([i (unsafe-fx+ o 1)] [e e])
          (if (unsafe-fx= i *system-size*)
            (loop-o (unsafe-fx+ o 1) e)
            (let* ([i1   (unsafe-vector-ref *system* i)]
                   [dx   (fl- (body-x o1) (body-x i1))]
                   [dy   (fl- (body-y o1) (body-y i1))]
                   [dz   (fl- (body-z o1) (body-z i1))]
                   [dist (flsqrt (fl+ (fl+ (fl* dx dx) (fl* dy dy)) (fl* dz dz)))]
                   [e    (fl- e (fl/ (fl* (body-mass o1) (body-mass i1)) dist))])
              (loop-i (unsafe-fx+ i 1) e))))))))

;; -------------------------------
(: advance ( -> Void))
(define (advance)
  (let loop-o ([o 0])
    (unless (unsafe-fx= o *system-size*)
      (let* ([o1 (unsafe-vector-ref *system* o)])
        (let loop-i ([i  (unsafe-fx+ o 1)]
                     [vx (body-vx o1)]
                     [vy (body-vy o1)]
                     [vz (body-vz o1)])
          (if (unsafe-fx< i *system-size*)
            (let* ([i1    (unsafe-vector-ref *system* i)]
                   [dx    (fl- (body-x o1) (body-x i1))]
                   [dy    (fl- (body-y o1) (body-y i1))]
                   [dz    (fl- (body-z o1) (body-z i1))]
                   [dist2 (fl+ (fl+ (fl* dx dx) (fl* dy dy)) (fl* dz dz))]
                   [mag   (fl/ +dt+ (fl* dist2 (flsqrt dist2)))]
                   [dxmag (fl* dx mag)]
                   [dymag (fl* dy mag)]
                   [dzmag (fl* dz mag)]
                   [om (body-mass o1)]
                   [im (body-mass i1)])
              (set-body-vx! i1 (fl+ (body-vx i1) (fl* dxmag om)))
              (set-body-vy! i1 (fl+ (body-vy i1) (fl* dymag om)))
              (set-body-vz! i1 (fl+ (body-vz i1) (fl* dzmag om)))
              (loop-i (unsafe-fx+ i 1)
                      (fl- vx (fl* dxmag im))
                      (fl- vy (fl* dymag im))
                      (fl- vz (fl* dzmag im))))
            (begin (set-body-vx! o1 vx)
                   (set-body-vy! o1 vy)
                   (set-body-vz! o1 vz)
                   (set-body-x! o1 (fl+ (body-x o1) (fl* +dt+ vx)))
                   (set-body-y! o1 (fl+ (body-y o1) (fl* +dt+ vy)))
                   (set-body-z! o1 (fl+ (body-z o1) (fl* +dt+ vz)))))))
      (loop-o (unsafe-fx+ o 1)))))

;; -------------------------------

(let ([n (command-line #:args (n) (assert (string->number (assert n string?)) exact-integer?))])
  (offset-momentum)
  (printf "~a\n" (real->decimal-string (energy) 9))
  (for ([i (in-range n)]) (advance))
  (printf "~a\n" (real->decimal-string (energy) 9)))
