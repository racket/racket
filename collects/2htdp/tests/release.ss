#lang scheme 

(require 2htdp/universe)
(require 2htdp/image)

(define (main r)
  (big-bang 1
            (on-draw (lambda (n)
                       (if (string? n)
                           (text (string-append "stopped: " n) 11 'red)
                           (circle (+ 100 n) 'solid 'red)))
                     500 500)
            (on-tick (lambda (x) (if (string? x) x (add1 x)))
                     r)
            (on-key  (lambda (n key)
                       (if (string? n)
                           (string-append n key)
                           (if (key=? "a" key)
                               ""
                               n))))
            #;
            (on-release (lambda (n key)
                          ;; you can release a key only if it was pressed
                          (if (key=? "a" key) 
                              1
                              n)))))
