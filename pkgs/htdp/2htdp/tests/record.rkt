#lang racket

(require 2htdp/universe)
(require 2htdp/image)

(define (draw-number n)
  (place-image (text (number->string n) 44 'red)
               50 50
               (empty-scene 100 100)))

;; Nat Nat String -> Nat 
;; make the clock tick n times, expected expected-n files in dir 
(define (create-n-images n expected-n dir)
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
             (stop-when (curry = n))
             (on-draw draw-number)
             (record? dir)))
  (sleep 1)
  (define number-of-png
    (parameterize ([current-directory dir])
      (define dlst (directory-list))
      ; (displayln dlst)
      (length
       (filter (lambda (f) (regexp-match "\\.png" (path->string f)))
               dlst))))
  (unless (= expected-n number-of-png)
    (error 'record? "(~s, ~s) didn't record proper number of images: ~s" n dir 
           number-of-png)))

(create-n-images 3 3 "images3/")
(create-n-images 0 0 "images0/")
