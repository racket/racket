#lang racket/gui
(require images/flomap)

(require rackunit)

;; checks pr 13717

(define bmp
  (flomap->bitmap
   (build-flomap 1 10 10
                 (lambda (k x y) (/ (+ x y) 200)))))

(define bmp-buf1 (make-bytes (* 10 10 4)))
(define bmp-buf2 (make-bytes (* 10 10 4)))

(send bmp get-argb-pixels 0 0 10 10 bmp-buf1)
(send bmp set-argb-pixels 0 0 10 10 bmp-buf1)
(send bmp get-argb-pixels 0 0 10 10 bmp-buf2)

(check-equal? bmp-buf1 bmp-buf2)