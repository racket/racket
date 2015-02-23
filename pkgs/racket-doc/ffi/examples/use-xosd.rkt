#! /usr/bin/env racket

#lang racket/base

(require "xosd.rkt")

(define x (xosd-create))

;; (xost-set-bar-length x 12)
(xosd-set-pos            x 'middle)
(xosd-set-align          x 'center)
(xosd-set-shadow-offset  x 7)
(xosd-set-outline-offset x 2)
(xosd-set-colour         x "yellow")
(xosd-set-shadow-colour  x "black")
(xosd-set-outline-colour x "blue")
(xosd-set-font           x "-adobe-courier-bold-r-*-*-34-*-*-*-*-*-*-*")

(printf ">>> xosd=~s, lines=~s, colour=~s\n"
        x (xosd-get-number-lines x) (xosd-get-colour x))

(xosd-display-string x "Xosd Test")

;; this doesn't work for some reason
;; (xosd-set-timeout x 1)
;; (xosd-wait-until-no-display x)

(sleep 2)
(xosd-set-timeout x 0)

(let loop ([n 10])
  (unless (zero? n)
    (xosd-show x)
    (sleep .05)
    (xosd-hide x)
    (sleep .05)
    (loop (sub1 n))))

(let ([f (lambda (disp)
           (let loop ([n 100])
             (when (> n 0) (disp x n) (sleep .1) (loop (- n 5)))))])
  (xosd-set-bar-length x 10)
  (f xosd-display-percentage)
  (sleep 1)
  (xosd-set-bar-length x 20)
  (f xosd-display-slider)
  (xosd-hide x)
  (sleep 1)
  (xosd-display-string x "FOO")
  (f (lambda (x n)
       (xosd-hide x) (xosd-set-vertical-offset x n) (xosd-show x)))
  (f (lambda (x n)
       (xosd-hide x) (xosd-set-vertical-offset x (- 100 n)) (xosd-show x)))
  (f (lambda (x n)
       (xosd-hide x) (xosd-set-vertical-offset x n) (xosd-show x)))
  (f (lambda (x n)
       (xosd-hide x) (xosd-set-vertical-offset x (- 100 n)) (xosd-show x))))
(xosd-hide x)
(sleep 1)

(set! x (xosd-create 4))
(xosd-set-pos   x 'middle)
(xosd-set-align x 'center)
(xosd-set-font  x "-adobe-courier-bold-r-*-*-25-*-*-*-*-*-*-*")
(xosd-set-shadow-offset  x 7)
(xosd-set-outline-offset x 2)
(xosd-display-string x "This is the first line" 0)
(xosd-display-string x "and the second line" 1)
(xosd-display-string x "the third one" 2)
(xosd-display-string x "and finally the fourth line" 3)
(sleep 2) (xosd-scroll x 1)
(sleep 1) (xosd-scroll x 1)
(sleep 1) (xosd-scroll x 1)
(sleep 1) (xosd-scroll x 1)
(sleep 1)
