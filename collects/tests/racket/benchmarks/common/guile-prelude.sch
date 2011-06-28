
(use-syntax (ice-9 syncase))

(define (msecs v)
  (quotient (* v 1000) internal-time-units-per-second))

(define (time* thunk)
  (let ((start (times))
        (start-gc (gc-run-time)))
    (let ((v (thunk)))
      (let ((end (times))
            (end-gc (gc-run-time)))
        (display "user: ")
        (display (msecs (- (tms:utime end) (tms:utime start))))
        (display " system: ")
        (display (msecs (- (tms:stime end) (tms:stime start))))
        (display " real: ")
        (display (msecs (- (tms:clock end) (tms:clock start))))
        (display " gc: ")
        (display (msecs (- end-gc start-gc)))
        (newline)))))

(define-syntax time
  (syntax-rules ()
    ((_ expr) (time* (lambda () expr)))))

(define bitwise-and logand)
(define bitwise-ior logior)
(define bitwise-not lognot)
