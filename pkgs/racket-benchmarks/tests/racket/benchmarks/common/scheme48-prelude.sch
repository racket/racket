(define (time* thunk)
   (let ((start-cpu (run-time))
         (start-real (real-time)))
     (let ((result (thunk)))
       (let ((end-cpu (run-time))
             (end-real (real-time)))
         (let ((cpu (- end-cpu start-cpu))
               (real (- end-real start-real)))
           (display "cpu time: ")
           (display cpu)
           (display " real time: ")
           (display real)
           (newline)
           result)))))

(define-syntax time
  (syntax-rules ()
    ((_ expr) (time* (lambda () expr)))))

(define (error . args) (+ 1 args))


