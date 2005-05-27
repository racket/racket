;; This is data from the Cavendish experiment

(define raw-data
  '((0.0	-14.7	3.6)
    (1.0	8.6	3.6)
    (2.1	28.8	3.0)
    (3.1	46.7	3.4)
    (4.2	47.4	3.5)
    (5.2	36.5	3.4)
   (6.2	37.0	10.3)
   (7.2	5.1	3.4)
   (8.2	-11.2	3.4)
   (9.1	-22.4	3.5)
   (10.0	-35.5	3.6)
   (11.0	-33.6	3.9)
   (12.0	-21.1	3.9)
   (12.9	-15.0	4.2)
   (13.8	-1.6	2.7)
   (14.9	19.5	3.2)
   (15.9	27.5	2.8)
   (17.0	32.6	3.5)
   (17.9	27.5	2.7)
   (18.9	20.2	3.3)
   (20.0	13.8	3.4)
   (21.0	-1.3	4.2)
   (22.0	-24.5	6.7)
   (23.0	-25.0	3.3)
   (24.0	-25.0	3.1)
   (25.0	-20.2	3.6)
   (26.0	-9.9	3.2)
   (27.0	5.8	3.2)
   (28.0	14.7	3.0)
   (29.0	21.8	3.5)
   (30.0	29.8	2.7)
   (31.0	21.4	4.1)
   (32.0	24.6	2.7)
   (32.9	25.8	12.0)
   (33.8	0.6	2.9)
   (34.7	-16.6	3.2)
   (35.7	-24.0	3.7)
   (36.6	-24.6	3.8)
   (37.7	-19.8	3.5)))

; first column is time data, second is result, third is error. to parse them out...
(require (lib "math.ss"))

(define times
  (map car raw-data))
(define vals
  (map cadr raw-data))
(define errors
  (map caddr raw-data))

(define experemental-data (map list->vector raw-data))

(require (lib "plot.ss" "plot"))

(plot (mix (points experemental-data (symbol 'circle))
           (error-bars experemental-data))
      (x-min 0) (x-max 40)
      (y-min -40) (y-max 50)
      (width 400) (height 300))

(define 
  (theta x a tau phi T theta0)
  (+ 
   theta0
   (* a 
      (exp (/ x tau -1))
      (sin (+ phi (/ (* 2 pi x) T))))))

(define result
  (fit 
   theta
   ((a 40) (tau 15) (phi -0.5) (T 15) (theta0 10))
   experemental-data))
              
  
(plot (mix
       (points (map vector times vals) (symobol 'square) (color 'black))
       (error-bars experemental-data)
       (line (fit-result-function result) (color 'green)))
      (x-min -5) (x-max 40)
      (y-min -40) (y-max 50))
       
(fit-result-final-params result)
;(pretty-print result)