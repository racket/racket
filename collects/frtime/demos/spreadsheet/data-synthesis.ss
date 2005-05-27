(module data-synthesis (lib "frtime.ss" "frtime")

(require "distributions.ss"
         (lib "math.ss")
         )

;; num list -> num
; INPUT: Many numbers DATA
; OUTPUT: That DATA's sample mean (a.k.a mean, average)
(define (sample-mean data)
  (let ([n (length data)]
        [sum-y (apply + data)])
    (/ sum-y n)))

;;num list -> num
; INPUT: List of numbers DATA
; OUTPUT: The sample variance of DATA
(define (sample-variance data)
  (let* ([n (length data)]
         [sm (sample-mean data)]
         [d2 (map (lambda (x)
                    (sqr (- x sm)))
                  data)])
    (/ (apply + d2) 
           (- n 1))))


;; NOTE:
;;  The sample-mean and sample-variance of set of data are unbiased
;;  and efficient estimators of the mean and variance of the probability
;;  distribution that produced that data.  Therefore, our best fitting
;;  normal distribution to that data is a normal distribution having
;;  mean equal to the sample mean and variance equal to the sample-variance.

(define (fit-normal-distribution data)
  (make-normal-distribution (sample-mean data)
                            (sqrt (sample-variance data))))

(define (fit-uniform-distribution data)
  (let ([mean (sample-mean data)]
        [variance (sample-variance data)])
;    (print (format "theta1: ~a" (- mean
;                                  (sqrt (* 3 variance)))))
;    (print (format "theta2: ~a" (+  mean
;                                   (sqrt (* 3 variance)))))
    (make-uniform-distribution (- mean
                                  (sqrt (* 3 variance)))
                               (+  mean
                                   (sqrt (* 3 variance))))))

; My Gamma work is screwy. See distributions.ss
;(define (fit-gamma-distribution data)
;  (let ([mean (sample-mean data)]
;        [variance (sample-variance data)])
;    (make-gamma-distribution (/ (sqr mean)
;                                variance)
;                             (/ variance mean))))

(define (fit-exponential-distribution data)
  (make-exponential-distribution (sample-mean data)))

(define (synthesize-random-data dist size)
            (if (zero? size)
                '()
                (cons ((distribution-rand dist))
                      (synthesize-random-data dist (sub1 size)))))
        


(provide (all-defined))
  
  )