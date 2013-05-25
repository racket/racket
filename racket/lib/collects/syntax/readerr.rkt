; Don't change this module to #lang, since it's used by syntax/module-reader
(module readerr racket/private/base
  (provide raise-read-error
           raise-read-eof-error)
  
  (define (raise-read-error msg source-name line col pos span #:extra-srclocs [extra-srclocs '()])
    (-raise-read-error make-exn:fail:read msg source-name line col pos span 
                       extra-srclocs
                       'raise-read-error))
  
  (define (raise-read-eof-error msg source-name line col pos span)
    (-raise-read-error make-exn:fail:read:eof msg source-name line col pos span '() 
                       'raise-read-eof-error))
  
  (define (-raise-read-error make-exn:fail:read msg source-name line col pos span extra-srclocs name)
    (define (bad-type which what)
      (raise-argument-error name what which
                            msg source-name line col pos span))
    
    (unless (string? msg) (bad-type 0 "string"))
    (unless (ordinal? line) (bad-type 2 ordinal))
    (unless (cardinal? col) (bad-type 3 cardinal))
    (unless (ordinal? pos) (bad-type 4 ordinal))
    (unless (cardinal? span) (bad-type 5 cardinal))
    (unless (and (list? extra-srclocs) (andmap srcloc? extra-srclocs))
      (raise-argument-error name "(list/c srcloc?)" extra-srclocs))
    
    (raise
     (make-exn:fail:read
      (format "~a~a"
              (cond [(not (error-print-source-location)) ""]
                    [(srcloc->string (srcloc source-name line col pos span))
                     =>
                     (lambda (s)
                       (format "~a: " s))]
                    [else ""])
              msg)
      (current-continuation-marks)
      (cons (make-srcloc source-name line col pos span)
            extra-srclocs))))
  
  (define (ordinal? x)
    (or (not x) 
        (exact-positive-integer? x)))
  (define ordinal "(or/c exact-positive-integer? #f)")
  (define (cardinal? x)
    (or (not x)
        (exact-nonnegative-integer? x)))
  (define cardinal "(or/c exact-nonnegative-integer? #f)"))
