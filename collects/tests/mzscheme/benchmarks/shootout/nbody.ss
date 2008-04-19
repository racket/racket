#!/usr/bin/mzscheme -qu
;; The Great Computer Language Shootout
;; http://shootout.alioth.debian.org/
;;
;; Imperative-style implementation based on the SBCL implementation by
;; Patrick Frankenberger and Juho Snellman, but using only native Scheme
;; idioms like 'named let' and 'do' special form.
;;
;; Contributed by Anthony Borla, then converted for mzscheme
;; by Matthew Flatt and Brent Fulgham

#|
Correct output N = 1000 is

-0.169075164
-0.169087605
|#

#lang scheme/base
(require scheme/cmdline)

;; ------------------------------
;; define planetary masses, initial positions & velocity

(define +pi+ 3.141592653589793)
(define +days-per-year+ 365.24)

(define +solar-mass+ (* 4 +pi+ +pi+))

(define-struct body (x y z vx vy vz mass)
  #:mutable)

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

;; -------------------------------
(define (offset-momentum system)
  (let loop-i ((i system) (px 0.0) (py 0.0) (pz 0.0))
    (if (null? i)
        (begin
          (set-body-vx! (car system) (/ (- px) +solar-mass+))
          (set-body-vy! (car system) (/ (- py) +solar-mass+))
          (set-body-vz! (car system) (/ (- pz) +solar-mass+)))
        (loop-i (cdr i)
                (+ px (* (body-vx (car i)) (body-mass (car i))))
                (+ py (* (body-vy (car i)) (body-mass (car i))))
                (+ pz (* (body-vz (car i)) (body-mass (car i))))))))

;; -------------------------------
(define (energy system)
  (let loop-o ((o system) (e 0.0))
    (if (null? o)
        e
        (let ([e (+ e (* 0.5 (body-mass (car o))
                         (+ (* (body-vx (car o)) (body-vx (car o)))
                            (* (body-vy (car o)) (body-vy (car o)))
                            (* (body-vz (car o)) (body-vz (car o))))))])
          
          (let loop-i ((i (cdr o)) (e e))
            (if (null? i)
                (loop-o (cdr o) e)
                (let* ((dx (- (body-x (car o)) (body-x (car i))))
                       (dy (- (body-y (car o)) (body-y (car i))))
                       (dz (- (body-z (car o)) (body-z (car i))))
                       (distance (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
                  (let ([e  (- e (/ (* (body-mass (car o)) (body-mass (car i))) distance))])
                    (loop-i (cdr i) e)))))))))

;; -------------------------------
(define (advance system dt)
  (let loop-o ((o system))
    (unless (null? o)
      (let loop-i ((i (cdr o)))
        (unless (null? i)
          (let* ((o1 (car o))
                 (i1 (car i))
                 (dx (- (body-x o1) (body-x i1)))
                 (dy (- (body-y o1) (body-y i1)))
                 (dz (- (body-z o1) (body-z i1)))
                 (distance (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
                 (mag (/ dt (* distance distance distance)))
                 (dxmag (* dx mag))
                 (dymag (* dy mag))
                 (dzmag (* dz mag))
                 (om (body-mass o1))
                 (im (body-mass i1)))
            (set-body-vx! o1 (- (body-vx o1) (* dxmag im)))
            (set-body-vy! o1 (- (body-vy o1) (* dymag im)))
            (set-body-vz! o1 (- (body-vz o1) (* dzmag im)))
            (set-body-vx! i1 (+ (body-vx i1) (* dxmag om)))
            (set-body-vy! i1 (+ (body-vy i1) (* dymag om)))
            (set-body-vz! i1 (+ (body-vz i1) (* dzmag om)))
            (loop-i (cdr i)))))
      (loop-o (cdr o))))
  
  (let loop-o ((o system))
    (unless (null? o)
      (let ([o1 (car o)])
        (set-body-x! o1 (+ (body-x o1) (* dt (body-vx o1))))
        (set-body-y! o1 (+ (body-y o1) (* dt (body-vy o1))))
        (set-body-z! o1 (+ (body-z o1) (* dt (body-vz o1))))
        (loop-o (cdr o))))))

;; -------------------------------

(let ((n (command-line #:args (n) (string->number n)))
      (system (list *sun* *jupiter* *saturn* *uranus* *neptune*)))
  
  (offset-momentum system)
  
  (printf "~a~%" (real->decimal-string (energy system) 9))
  
  (do ((i 1 (+ i 1)))
      ((< n i))
    (advance system 0.01))
  
  (printf "~a~%" (real->decimal-string (energy system) 9)))
