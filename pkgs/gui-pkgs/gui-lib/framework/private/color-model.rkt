#lang scheme/unit
(require "sig.rkt")

  (import)
  (export framework:color-model^)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                 ;;;
  ;;;           matrix ops            ;;;
  ;;;                                 ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; matrix inversion using cramer's rule
  
  ;; submatrix : (list-of (list-of num)) int int -> (list-of (list-of num))
  ;; submatrix "crosses out" row i and column j from the matrix, returning a new one
  
  (define (submatrix source i j)
    (let row-loop ([row 0])
      (cond 
        [(eq? row (length source)) null]
        [(eq? row i) (row-loop (+ row 1))]
        [else
         (cons 
          (let col-loop ([col 0])
            (cond 
              [(eq? col (length (car source))) null]
              [(eq? col j) (col-loop (+ col 1))]
              [else
               (cons (list-ref (list-ref source row) col)
                     (col-loop (+ col 1)))]))
          (row-loop (+ row 1)))])))
  
  ;;(equal? (submatrix test-matrix 1 2)
  ;;        '((1 2 6) (7 8 4)))
  
  ;; det : (list-of (list-of num)) -> num
  
  (define (det matrix)
    (if (null? matrix)
        1
        (let loop ([row 0] [sign 1])
          (if (= row (length matrix))
              0
              (+ (* sign
                    (list-ref (list-ref matrix row) 0)
                    (det (submatrix matrix row 0)))
                 (loop (+ row 1) (- sign)))))))
  
  ;;(define square-test-matrix '((3 20 3) (37 0 8) (2 1 4)))
  
  ;;(= (det square-test-matrix) -2553)
  
  ;; invert : (list-of (list-of num)) -> (list-of (list-of num))
  
  (define (matrix-invert matrix)
    (let-values ([(width height) (matrix-dimension matrix)])
      (when (not (= width height))
        (error 'invert "matrix is not square: ~s" matrix))
      (let ([delta-inv (/ 1 (det matrix))])
        (let row-loop ([row 0] [sign 1])
          (if (= row (length matrix))
              null
              (cons 
               (let col-loop ([col 0] [sign sign])
                 (if (= col (length (car matrix)))
                     null
                     (cons (* delta-inv
                              sign
                              (det (submatrix matrix col row)))
                           (col-loop (+ col 1) (- sign)))))
               (row-loop (+ row 1) (- sign))))))))
  
  ;;(equal? (matrix-invert square-test-matrix)
  ;;        '((8/2553 77/2553 -160/2553) (44/851 -2/851 -29/851) (-1/69 -1/69 20/69)))
  
  ;; matrix-dimension : (list-of (list-of num)) -> (values num num)  
  ;; takes a matrix, returns width and height
  
  (define (matrix-dimension matrix)
    (when (not (pair? matrix))
      (error 'matrix-dimension "matrix argument is not a list: ~s" matrix))
    (let ([height (length matrix)])
      (when (= height 0)
        (error 'matrix-dimension "matrix argument is empty: ~s" matrix))
      (when (not (pair? (car matrix)))
        (error 'matrix-dimension "matrix row is not a list: ~s" (car matrix)))
      (let ([width (length (car matrix))])
        (when (= width 0)
          (error 'matrix-dimension "matrix argument has width 0: ~s" matrix))
        (let loop ([rows matrix])
          (if (null? rows)
              (values width height)
              (begin
                (when (not (pair? (car rows)))
                  (error 'matrix-dimension "row is not a list: ~s" (car rows)))
                (when (not (= width (length (car rows))))
                  (error 'matrix-dimension "rows have different widths: ~s and ~s" width (length (car rows))))
                (loop (cdr rows))))))))
  
  ;; transpose : (list-of (list-of num)) -> (list-of (list-of num))
  (define (transpose vector) (apply map list vector))
  
  
  ;; test code
  ;;(equal? (transpose '((3 2 1) (9 8 7))) '((3 9) (2 8) (1 7)))
  
  ;; inner-product : (list-of num) (list-of num) -> num
  (define (inner-product a b)
    (foldl + 0 (map * a b)))
  
  ;; test code
  ;; (= (inner-product '(4 1 3) '(0 3 4)) 15)
  
  ;; matrix-multiply: (list-of (list-of num)) (list-of (list-of num)) -> (list-of (list-of num))
  ;; multiplies the two matrices.
  (define (matrix-multiply a b)
    (let-values ([(width-a height-a) (matrix-dimension a)]
                 [(width-b height-b) (matrix-dimension b)])
      (when (not (= width-a height-b))
        (error 'matrix-multiply "matrix dimensions do not match for multiplication"))
      (let ([b-t (transpose b)])
        (map (λ (row)
               (map (λ (col)
                      (inner-product row col))
                    b-t))
             a))))
  
  ;; test code
  ;; (equal? (matrix-multiply '((1 2 3 4) (9 8 3 2)) '((0) (2) (0) (3)))
  ;;         '((16) (22)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                 ;;;
  ;;;           color model           ;;;
  ;;;                                 ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; ITU reccommendation phosphors:
  
  ;;    red green blue
  ;;x   0.64 0.29 0.15
  ;;y   0.33 0.60 0.06
  ;;
  ;; white point: 
  ;; c : x-w = 0.313, y-w = 0.329, big-y-w = 100.0
  
  (define x-r 0.64)
  (define y-r 0.33)
  (define x-g 0.29)
  (define y-g 0.60)
  (define x-b 0.15)
  (define y-b 0.06)
  
  (define z-r (- 1 x-r y-r))
  (define z-g (- 1 x-g y-g))
  (define z-b (- 1 x-b y-b))
  
  (define x-w 0.313)
  (define y-w 0.329)
  (define big-y-w 100.0)
  
  (define-struct xyz (x y z))
  
  (define (xy-big-y->xyz x y big-y)
    (let ([sigma (/ big-y y)])
      (make-xyz
       (* x sigma)
       (* y sigma)
       (* (- 1 x y) sigma))))
  
  (define xyz-white (xy-big-y->xyz x-w y-w big-y-w))
  
  ;;`((,(xyz-x xyz-white) ,x-r ,x-g ,x-b)
  ;;  (,(xyz-y xyz-white) ,y-r ,y-g ,y-b)
  ;;  (,(xyz-z xyz-white) ,z-r ,z-g ,z-b))
  
  ;; sigmas were calculated by soving a set of linear equations based upon ntsc standard phosphors
  
  (define pre-matrix `((,x-r ,x-g ,x-b)
                       (,y-r ,y-g ,y-b)
                       (,z-r ,z-g ,z-b)))
  
  (define-values (sigma-r sigma-g sigma-b)
    (let* ([inversion 
            (matrix-invert pre-matrix)]
           [sigmas
            (matrix-multiply inversion `((,(xyz-x xyz-white))
                                         (,(xyz-y xyz-white))
                                         (,(xyz-z xyz-white))))])
      (apply values (car (transpose sigmas)))))
  
  ;; (printf "should be equal to xyz-white: \n~a\n"
  ;;    (matrix-multiply pre-matrix `((,sigma-r) (,sigma-g) (,sigma-b))))
  
  (define rgb->xyz-matrix
    (map (λ (row)
           (map (λ (row-elt scalar) (* row-elt scalar 1/255)) row `(,sigma-r ,sigma-g ,sigma-b)))
         pre-matrix))
  
  (define xyz->rgb-matrix
    (matrix-invert rgb->xyz-matrix))
  
  ;;(printf "should be identity: \n~a\n" (matrix-multiply rgb->xyz-matrix xyz->rgb-matrix))
  
  (define (rgb->xyz r g b)
    (apply make-xyz (car (transpose (matrix-multiply rgb->xyz-matrix (transpose `((,r ,g ,b))))))))
  
  ;;(print-struct #t)
  ;; (printf "should be xyz-white: \n~a\n" (rgb->xyz 255 255 255))
  
  (define (xyz->rgb x y z)
    (car (transpose (matrix-multiply xyz->rgb-matrix (transpose `((,x ,y ,z)))))))
  
  ;;l* = 116(y/big-y-n)^1/3 - 16, y/big-y-n > 0.01
  ;;u* = 13 l*(u-p - u-p-n)
  ;;v* = 13 l*(v-p - v-p-n)
  ;;
  ;;u-p = (4x)/(x+15y+3z)   v-p = (9y)/(x+15y+3z)
  ;;u-p-n = (same but with -n) v-p-n = (same but with -n)
  
  ;; the following transformation is undefined if the y component
  ;; is zero.  So if it is, we bump it up a little.
  
  (define (xyz-tweak xyz)
    (let* ([y (xyz-y xyz)])
      (make-xyz (xyz-x xyz) (if (< y 0.01) 0.01 y) (xyz-z xyz))))
  
  (define-struct luv (l u v))
  
  (define (xyz-denom xyz)
    (+ (xyz-x xyz) (* 15 (xyz-y xyz)) (* 3 (xyz-z xyz))))
  
  (define (xyz-u-p xyz)
    (/ (* 4 (xyz-x xyz)) (xyz-denom xyz)))
  
  (define (xyz-v-p xyz)
    (/ (* 9 (xyz-y xyz)) (xyz-denom xyz)))
  
  (define (xyz->luv xyz)
    (let ([xyz (xyz-tweak xyz)])
      (let* ([l (- (* 116 (expt (/ (xyz-y xyz) (xyz-y xyz-white))
                                1/3))
                   16)]
             [u-p (xyz-u-p xyz)]
             [u-p-white (xyz-u-p xyz-white)]
             [v-p (xyz-v-p xyz)]
             [v-p-white (xyz-v-p xyz-white)])
        (make-luv l (* 13 l (- u-p u-p-white)) (* 13 l (- v-p v-p-white))))))
  
  (define (luv-distance a b)
    (expt (+ (expt (- (luv-l a) (luv-l b)) 2)
             (expt (- (luv-u a) (luv-u b)) 2)
             (expt (- (luv-v a) (luv-v b)) 2))
          1/2))
  
  (define (rgb-color-distance r-a g-a b-a r-b g-b b-b)
    (let* ([luv-a (xyz->luv (rgb->xyz r-a g-a b-a))]
           [luv-b (xyz->luv (rgb->xyz r-b g-b b-b))])
      (luv-distance luv-a luv-b)))
  
  ;;(rgb-color-distance 0 0 0 0 0 0)
  ;; (print-struct #t)
  ;; (xyz->luv (make-xyz 95.0 100.0 141.0))
  ;; (xyz->luv (make-xyz 60.0 80.0 20.0))
