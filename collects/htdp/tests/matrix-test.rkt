;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname matrix-test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;(require htdp/matrix-invisible)
(require (lib "matrix.rkt" "htdp"))

(define r1 '((a00 a01 a02)
             (a10 a11 a12)))

(define m1 (rectangle->matrix r1))

(check-expect (matrix? m1) true)
(check-expect (matrix-rows m1) 2)
(check-expect (matrix-cols m1) 3)
(check-expect (matrix->rectangle m1) r1)

(define x (random 2))
(define y (random 3))
  
(check-expect (matrix-ref (make-matrix 2 3 '(a00 a01 a02 a10 a11 a12)) x y)
              (matrix-ref m1 x y))

(check-expect
 (matrix-ref (build-matrix 2 3 (lambda (row col) (matrix-ref m1 row col))) x y)
 (matrix-ref m1 x y))

(define lpons (list (make-posn 0 0)
                    (make-posn 1 0)
                    (make-posn 0 1)
                    (make-posn 1 2)
                    (make-posn 1 1)
                    (make-posn 0 2)))

(define m2 
  (foldl (lambda (x m) (matrix-set m (posn-x x) (posn-y x) 1)) m1 lpons))

(check-expect 1  (matrix-ref m2 (random 2) (random 3)))


(define (is1 x) (= x 1))
(check-expect (matrix-where? m2 is1)
              (list (make-posn 0 0) (make-posn 0 1) (make-posn 0 2)
                    (make-posn 1 0) (make-posn 1 1) (make-posn 1 2)))

(define (is2 x) (= x 2))
(check-expect (matrix-where? m2 is2) empty)

(define m1-minor (matrix-minor m1 1 1))

(check-expect (matrix-ref m1-minor 0 0) 'a00)
(check-expect (matrix-ref m1-minor 0 1) 'a02)

(check-expect (matrix-ref m1 0 0) 'a00)
;; --- IMPERATIVE ---
;; see comment in matrix-unit.rkt
;; ---------------------------------
;; (define M-imperative (build-matrix 2 3 (λ (i j) (* (expt 2 i) (expt 3 j)))))
;; (define M-modified   (matrix-set! M-imperative 0 0 'xxx))
;; (check-expect (matrix-ref M-modified 0 0) 'xxx)
;; (check-expect (matrix-ref M-imperative 0 0) 'xxx)
