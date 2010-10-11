;; Moments.scm

(require (only-in mzlib/list sort)
         (only-in mzlib/string real->decimal-string)
         racket/flonum)

(: to-str (Real -> String))
(define (to-str n) (real->decimal-string n 6))

(let: loop : Void
      ((line : (U String EOF) (read-line))
       (numlist : (Listof Float) '())
       (sum : Float 0.0))
      (cond ((not (eof-object? line))
             (let ((num (assert (string->number line) flonum?)))
               (loop (read-line) (cons num numlist) (+ num sum))))
            (else
             (unless (null? numlist)
               (let* ((n (length numlist))
                      (mean (/ sum (exact->inexact n))))
                 (let: loop : Void
                       ((nums : (Listof Float) numlist)
                        (average_deviation : Float 0.0)
                        (variance : Float 0.0)
                        (skew : Float 0.0)
                        (kurtosis : Float 0.0)
                        (deviation : Float 0.0))
                       (if (not (null? nums))
                           (loop (cdr nums)
                                 (+ average_deviation (abs deviation))
                                 (+ variance (expt deviation 2))
                                 (+ skew (expt deviation 3))
                                 (+ kurtosis (expt deviation 4))
                                 (- (car nums) mean))
                           (let* ((average_deviation (/ average_deviation (exact->inexact n)))
                                  (variance (/ variance (- n 1.0)))
                                  (standard_deviation (flsqrt variance))
                                  (numlist ((inst sort Float Float) numlist (lambda: ((x : Float) (y : Float))
                                                                                     (< x y)))))

                             (cond ((> variance 0.0)
                                    (set! skew (exact->inexact (/ skew (* n variance standard_deviation))))
                                    (set! kurtosis (- (/ kurtosis (* n variance variance))
                                                      3.0))))

                             (let* ((mid (quotient n 2))
                                    (median (if (zero? (modulo n 2))
                                                (/ (+ (car (list-tail numlist mid))
                                                      (car (list-tail numlist (- mid 1))))
                                                   2.0)
                                                (car (list-tail numlist mid))))
                                    (standard_deviation (exact->inexact (/ (round (* standard_deviation 1000000))
                                                                           1000000))))

                               (for-each display
                                         `("n:                  " ,n                   "\n"
                                           "median:             " ,(to-str median)  "\n"
                                           "mean:               " ,(to-str mean)    "\n"
                                           "average_deviation:  " ,(to-str average_deviation ) "\n"
                                           "standard_deviation: " ,(to-str standard_deviation) "\n"
                                           "variance:           " ,(to-str variance)"\n"
                                           "skew:               " ,(to-str skew)    "\n"
                                           "kurtosis:           " ,(to-str kurtosis)"\n" )))))))))))
