;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname matrix-client) (read-case-sensitive #t) (teachpacks ((lib "world.ss" "teachpack" "htdp") (lib "testing.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "world.ss" "teachpack" "htdp") (lib "testing.ss" "teachpack" "htdp")))))
(require (lib "matrix.ss" "htdp"))
(require (lib "testing.ss" "htdp"))

(define m1 (make-matrix 2 3 (list 'a 'b 'c 'd 'e 'f)))
(define m2 (matrix-set m1 1 1 'x))

(check-expect (matrix-ref m1 0 0) 'a)
(check-expect (matrix-ref m1 0 1) 'b)
(check-expect (matrix-ref m1 0 2) 'c)
(check-expect (matrix-ref m1 1 0) 'd)
(check-expect (matrix-ref m1 1 1) 'e)
(check-expect (matrix-ref m1 1 2) 'f)

(check-expect (matrix-ref m2 1 1) 'x)

(define matrix1 (make-matrix 2 3 '(a00 a01 a02 a10 a11 a12)))

(define r1 '((a00 a01 a02)
             (a10 a11 a12)))

(check-expect (matrix-render matrix1) (matrix-render (rectangle->matrix r1)))
(check-expect (matrix-ref matrix1 0 0) 'a00)
(check-expect (matrix-ref matrix1 0 1) 'a01)
(check-expect (matrix-ref matrix1 0 2) 'a02)
(check-expect (matrix-ref matrix1 1 0) 'a10)
(check-expect (matrix-ref matrix1 1 1) 'a11)
(check-expect (matrix-ref matrix1 1 2) 'a12)

(check-expect (matrix-render (build-matrix 2 3 (lambda (i j) (matrix-ref matrix1 i j))))
              (matrix-render matrix1))

(define matrix2 (make-matrix 2 2 '(a00 a01 a10 a11)))

(check-expect (matrix-render (matrix-minor matrix2 0 0))
              (matrix-render (make-matrix 1 1 '(a11))))

(check-expect (matrix-render (matrix-minor matrix2 1 1))
              (matrix-render (make-matrix 1 1 '(a00))))

;; ===========================================================================

;; Matrix -> Number 
;; compute the determinat of a square (n x n) matrix 
(define (determinant M)
  (local 
    ((define n (matrix-n M))
     (define (series i)
       (* (expt -1 i) (matrix-ref M 0 i) (determinant (matrix-minor M 0 i)))))
    (if (= n 1)
        (matrix-ref M 0 0)
        (foldl + 0 (build-list n series)))))



(check-expect (determinant (rectangle->matrix '((1 1)
                                                (1 1))))
              0)

(check-expect (determinant (rectangle->matrix '((2 1)
                                                (1 1))))
              1)

;; ===========================================================================
;; matrix with structure inside 

(define-struct p (x y))

(define matrix3 
  (rectangle->matrix 
   (list (list (make-p 0 0) (make-p 0 1))
         (list (make-p 1 0) (make-p 1 1)))))

(define matrix4 (matrix-set matrix3 0 0 "intentionally failing check"))

(check-expect matrix3 matrix3)

(check-expect matrix3 matrix4)
"the above test should fail"

(generate-report)
