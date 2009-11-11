; Moments.scm

(module moments mzscheme
  (require (only mzlib/list sort)
           (only mzlib/string real->decimal-string))

  (define (to-str n) (real->decimal-string n 6))

  (let* ((sum 0.0)
	 (numlist (let loop ((line (read-line)) (numlist '()))
		    (cond ((eof-object? line) numlist)
			  (else
			   (let ((num (string->number line)))
			     (set! sum (+ num sum))
			     (loop (read-line) (cons num numlist))))))))
    (unless (null? numlist)
      (let ((n (length numlist)))
        (let ((mean (/ sum n))
              (average_deviation 0.0)
              (standard_deviation 0.0)
              (variance 0.0)
              (skew 0.0)
              (kurtosis 0.0)
              (median 0.0)
              (deviation 0.0))
          (let loop ((nums numlist))
            (if (not (null? nums))
                (begin
                  (set! deviation (- (car nums) mean))
                  (set! average_deviation (+ average_deviation (abs deviation)))
                  (set! variance (+ variance (expt deviation 2.0)))
                  (set! skew (+ skew (expt deviation 3.0)))
                  (set! kurtosis (+ kurtosis (expt deviation 4)))
                  (loop (cdr nums)))))

          (set! average_deviation (/ average_deviation (exact->inexact n)))
          (set! variance (/ variance (- n 1)))
          (set! standard_deviation (sqrt variance))

          (cond ((> variance 0.0)
                 (set! skew (/ skew (* n variance standard_deviation)))
                 (set! kurtosis (- (/ kurtosis (* n variance variance))
                                   3.0))))

          (set! numlist (sort numlist (lambda (x y) (< x y))))

          (let ((mid (quotient n 2)))
            (if (zero? (modulo n 2))
                (set! median (/ (+ (car (list-tail numlist mid))
                                   (car (list-tail numlist (- mid 1))))
                                2.0))
                (set! median (car (list-tail numlist mid)))))


          (set! standard_deviation (/ (round (* standard_deviation 1000000))
                                      1000000))

          (for-each display
                    `("n:                  " ,n                   "\n"
                      "median:             " ,(to-str median)  "\n"
                      "mean:               " ,(to-str mean)    "\n"
                      "average_deviation:  " ,(to-str average_deviation ) "\n"
                      "standard_deviation: " ,(to-str standard_deviation) "\n"
                      "variance:           " ,(to-str variance)"\n"
                      "skew:               " ,(to-str skew)    "\n"
                      "kurtosis:           " ,(to-str kurtosis)"\n" )))))))
