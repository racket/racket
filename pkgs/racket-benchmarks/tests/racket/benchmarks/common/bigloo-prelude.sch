(define orig-time time)

(define-macro (time expr)
  `(time-it (lambda () ,expr)))

(define (time-it thunk)
  (multiple-value-bind (res rtime stime utime)
   (orig-time thunk)
   (print "real: " rtime " sys: " stime " user: " utime)
   res))

(define (error . x) #f)

(define bitwise-or bit-or)
(define bitwise-and bit-and)
(define bitwise-not bit-not)
