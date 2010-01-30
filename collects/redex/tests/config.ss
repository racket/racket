#lang scheme
(provide set-show-bitmaps? get-show-bitmaps?)
(define show-bitmaps? #t)
(define (set-show-bitmaps? sb?) (set! show-bitmaps? sb?))
(define (get-show-bitmaps?) show-bitmaps?)