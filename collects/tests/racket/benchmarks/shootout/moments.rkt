; Moments.scm

#lang racket/base

(require (only-in mzlib/list sort)
         (only-in mzlib/string real->decimal-string)
         racket/flonum)

(define (to-str n) (real->decimal-string n 6))

(let loop ((line (read-line)) (numlist '()) (sum 0.0))
  (cond ((not (eof-object? line))
         (let ((num (string->number line)))
           (loop (read-line) (cons num numlist) (+ num sum))))
        (else
         (unless (null? numlist)
           (let* ((n (length numlist))
                  (mean (/ sum n)))
             (let loop ((nums numlist)
                        (average_deviation 0.0)
                        (variance 0.0)
                        (skew 0.0)
                        (kurtosis 0.0)
                        (deviation 0.0))
               (if (not (null? nums))
                   (loop (cdr nums)
                         (+ average_deviation (abs deviation))
                         (+ variance (expt deviation 2))
                         (+ skew (expt deviation 3))
                         (+ kurtosis (expt deviation 4))
                         (- (car nums) mean))
                   (let* ((average_deviation (/ average_deviation (exact->inexact n)))
                          (variance (/ variance (- n 1)))
                          (standard_deviation (flsqrt variance))
                          (numlist (sort numlist (lambda (x y) (< x y)))))

                     (cond ((> variance 0.0)
                            (set! skew (/ skew (* n variance standard_deviation)))
                            (set! kurtosis (- (/ kurtosis (* n variance variance))
                                              3.0))))

                     (let* ((mid (quotient n 2))
                            (median (if (zero? (modulo n 2))
                                        (/ (+ (car (list-tail numlist mid))
                                              (car (list-tail numlist (- mid 1))))
                                           2.0)
                                        (car (list-tail numlist mid))))
                            (standard_deviation (/ (round (* standard_deviation 1000000))
                                                   1000000)))
                       
                       (for-each display
                                 `("n:                  " ,n                   "\n"
                                   "median:             " ,(to-str median)  "\n"
                                   "mean:               " ,(to-str mean)    "\n"
                                   "average_deviation:  " ,(to-str average_deviation ) "\n"
                                   "standard_deviation: " ,(to-str standard_deviation) "\n"
                                   "variance:           " ,(to-str variance)"\n"
                                   "skew:               " ,(to-str skew)    "\n"
                                   "kurtosis:           " ,(to-str kurtosis)"\n" )))))))))))
