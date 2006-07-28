(define-macro (time expr)
  `(time-it (lambda () ,expr)))

(define (time-it thunk)
  (thunk))

(define (error . x) #f)


