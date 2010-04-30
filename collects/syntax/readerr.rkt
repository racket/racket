; Don't change this module to #lang, since it's used by syntax/module-reader
(module readerr racket/private/base
  (provide raise-read-error
	   raise-read-eof-error)

  (define (raise-read-error msg source-name line col pos span)
    (-raise-read-error make-exn:fail:read msg source-name line col pos span))
    
  (define (raise-read-eof-error msg source-name line col pos span)
    (-raise-read-error make-exn:fail:read:eof msg source-name line col pos span))

  (define (-raise-read-error make-exn:fail:read msg source-name line col pos span)
    (let ([bad-type
           (lambda (which what)
             (raise-type-error 'raise-read-error
                               what
                               which
                               msg source-name line col pos span))]
          [ordinal? (lambda (x)
                      (or (not x) 
                          (and (number? x) (exact? x) (positive? x) (integer? x))))]
          [ordinal "positive exact integer"]
	  [cardinal? (lambda (x)
		       (or (not x)
			   (and (number? x) (exact? x) (not (negative? x)) (integer? x))))]
	  [cardinal "non-negative exact integer"])
    
      (unless (string? msg)
        (bad-type 0 "string"))
      (unless (ordinal? line)
        (bad-type 2 ordinal))
      (unless (cardinal? col)
        (bad-type 3 cardinal))
      (unless (ordinal? pos)
        (bad-type 4 ordinal))
      (unless (cardinal? span)
        (bad-type 5 cardinal))
      
      (raise
       (make-exn:fail:read
        (format "~a~a"
                (cond [(not (error-print-source-location)) ""]
                      [(and line col)
                       (format "~a:~a:~a: " source-name line col)]
                      [pos
                       (format "~a::~a: " source-name pos)]
                      [else
                       (format "~a: " source-name)])
                msg)
        (current-continuation-marks)
	(list (make-srcloc
               source-name line col pos span)))))))
