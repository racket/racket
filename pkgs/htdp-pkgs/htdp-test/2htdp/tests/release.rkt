#lang scheme 

(require 2htdp/universe 2htdp/image "test-aux.rkt")

(define (main r)
  (big-bang 1
            (on-draw (lambda (n)
                       (if (string? n)
                           (text (string-append "stopped: " n) 11 'red)
                           (text "hold down a" 11 'blue)))
                     500 500)
            (on-tick (lambda (x) (if (string? x) x (add1 x)))
                     r)
	    (stop-when (lambda (x)
			 (if (string? x)
			     (>= (string-length x) 3)
			     (>= x 5))))
            (on-key  (lambda (n key)
                       (if (string? n)
                           (string-append n key)
                           (if (key=? "a" key)
                               ""
                               n))))
            (on-release (lambda (n key)
                          ;; you can release a key only if it was pressed
                          (if (key=? "a" key) 
                              1
                              n)))))
(testing (main 1))
