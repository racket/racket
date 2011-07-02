#lang racket

(require 2htdp/universe 2htdp/image (only-in lang/imageeq image=?))

(define (draw-number n)
  (place-image (text (number->string n) 44 'red) 50 50 (empty-scene 100 100)))

(define (draw-stop n)
  (place-image stop 50 50 (empty-scene 100 100)))
(define stop (text "STOP" 44 'red))


(define dir "images0")
(unless (directory-exists? dir)
  (make-directory dir))
(parameterize ([current-directory dir])
  (for-each delete-file (directory-list)))
(with-output-to-file (format "./~a/index.html" dir)
  (lambda ()
    (displayln "<html><body><img src=\"i-animated.gif\" /></body></html>"))
  #:exists 'replace)
(define final-world 
  (big-bang 0
            (on-tick add1)
            (stop-when (curry = 5) draw-stop)
            (on-draw draw-number)
            (record? dir)))
(sleep 1)

(define i (bitmap "images0/i1.png"))
(define j (draw-stop 5))

(unless (image=? (crop 0 0 100 100 i) j)
  (fprintf (current-error-port)
           "this test needs to be revised -- the way 'world' writes images adds an extra pixel -- think! \n"))

