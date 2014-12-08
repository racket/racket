#lang racket/base

;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; Imperative-style implementation based on the SBCL implementation by
;; Patrick Frankenberger and Juho Snellman, but using only native Scheme
;; idioms like 'named let' and 'do' special form.
;;
;; Contributed by Anthony Borla, then converted for Racket
;; by Matthew Flatt and Brent Fulgham

#|
Correct output N = 1000 is

-0.169075164
-0.169087605
|#

(require racket/cmdline)

;; ------------------------------
;; define planetary masses, initial positions & velocity

(define +pi+ 3.141592653589793)
(define +days-per-year+ 365.24)

(define +solar-mass+ (* 4.0 +pi+ +pi+))

(define +dt+ 0.01)

(define make-body vector)
(define-syntax-rule (deffield n getter setter)
  (begin (define-syntax-rule (getter b) (vector-ref b n))
         (define-syntax-rule (setter b x) (vector-set! b n x))))
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
             (* 1.66007664274403694e-3 +days-per-year+)
             (* 7.69901118419740425e-3 +days-per-year+)
             (* -6.90460016972063023e-5 +days-per-year+)
             (* 9.54791938424326609e-4 +solar-mass+)))

(define *saturn*
  (make-body 8.34336671824457987
             4.12479856412430479
             -4.03523417114321381e-1
             (* -2.76742510726862411e-3 +days-per-year+)
             (* 4.99852801234917238e-3 +days-per-year+)
             (* 2.30417297573763929e-5 +days-per-year+)
             (* 2.85885980666130812e-4 +solar-mass+)))

(define *uranus*
  (make-body 1.28943695621391310e1
             -1.51111514016986312e1
             -2.23307578892655734e-1
             (* 2.96460137564761618e-03 +days-per-year+)
             (* 2.37847173959480950e-03 +days-per-year+)
             (* -2.96589568540237556e-05 +days-per-year+)
             (*  4.36624404335156298e-05 +solar-mass+)))

(define *neptune*
  (make-body 1.53796971148509165e+01
             -2.59193146099879641e+01
             1.79258772950371181e-01
             (* 2.68067772490389322e-03 +days-per-year+)
             (* 1.62824170038242295e-03 +days-per-year+)
             (* -9.51592254519715870e-05 +days-per-year+)
             (* 5.15138902046611451e-05 +solar-mass+)))

(define *system* (vector *sun* *jupiter* *saturn* *uranus* *neptune*))
(define *system-size* 5)

;; -------------------------------
(define (offset-momentum)
  (let loop-i ([i 0] [px 0.0] [py 0.0] [pz 0.0])
    (if (= i *system-size*)
      (begin
        (set-body-vx! (vector-ref *system* 0) (/ (- px) +solar-mass+))
        (set-body-vy! (vector-ref *system* 0) (/ (- py) +solar-mass+))
        (set-body-vz! (vector-ref *system* 0) (/ (- pz) +solar-mass+)))
      (let ([i1 (vector-ref *system* i)])
        (loop-i (add1 i)
                (+ px (* (body-vx i1) (body-mass i1)))
                (+ py (* (body-vy i1) (body-mass i1)))
                (+ pz (* (body-vz i1) (body-mass i1))))))))

;; -------------------------------
(define (energy)
  (let loop-o ([o 0] [e 0.0])
    (if (= o *system-size*)
      e
      (let* ([o1 (vector-ref *system* o)]
             [e (+ e (* 0.5 
                        (body-mass o1)
                        (+ (* (body-vx o1) (body-vx o1))
                           (* (body-vy o1) (body-vy o1))
                           (* (body-vz o1) (body-vz o1)))))])
        (let loop-i ([i (add1 o)] [e e])
          (if (= i *system-size*)
            (loop-o (add1 o) e)
            (let* ([i1   (vector-ref *system* i)]
                   [dx   (- (body-x o1) (body-x i1))]
                   [dy   (- (body-y o1) (body-y i1))]
                   [dz   (- (body-z o1) (body-z i1))]
                   [dist (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))]
                   [e    (- e (/ (* (body-mass o1) (body-mass i1)) dist))])
              (loop-i (add1 i) e))))))))

;; -------------------------------
(define (advance)
  (let loop-o ([o 0])
    (unless (= o *system-size*)
      (let* ([o1  (vector-ref *system* o)]
             [o1x (body-x o1)]
             [o1y (body-y o1)]
             [o1z (body-z o1)]
             [om  (body-mass o1)])
        (let loop-i ([i  (add1 o)]
                     [vx (body-vx o1)]
                     [vy (body-vy o1)]
                     [vz (body-vz o1)])
          (if (< i *system-size*)
            (let* ([i1    (vector-ref *system* i)]
                   [dx    (- o1x (body-x i1))]
                   [dy    (- o1y (body-y i1))]
                   [dz    (- o1z (body-z i1))]
                   [dist2 (+ (* dx dx) (* dy dy) (* dz dz))]
                   [mag   (/ +dt+ (* dist2 (sqrt dist2)))]
                   [dxmag (* dx mag)]
                   [dymag (* dy mag)]
                   [dzmag (* dz mag)]
                   [im    (body-mass i1)])
              (set-body-vx! i1 (+ (body-vx i1) (* dxmag om)))
              (set-body-vy! i1 (+ (body-vy i1) (* dymag om)))
              (set-body-vz! i1 (+ (body-vz i1) (* dzmag om)))
              (loop-i (add1 i)
                      (- vx (* dxmag im))
                      (- vy (* dymag im))
                      (- vz (* dzmag im))))
            (begin (set-body-vx! o1 vx)
                   (set-body-vy! o1 vy)
                   (set-body-vz! o1 vz)
                   (set-body-x! o1 (+ o1x (* +dt+ vx)))
                   (set-body-y! o1 (+ o1y (* +dt+ vy)))
                   (set-body-z! o1 (+ o1z (* +dt+ vz)))))))
      (loop-o (add1 o)))))

;; -------------------------------

(let ([n (command-line #:args (n) (string->number n))])
  (offset-momentum)
  (printf "~a\n" (real->decimal-string (energy) 9))
  (for ([i (in-range n)]) (advance))
  (printf "~a\n" (real->decimal-string (energy) 9)))
