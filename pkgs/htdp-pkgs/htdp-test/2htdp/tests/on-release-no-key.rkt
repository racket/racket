;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname on-release-no-key) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; Any key inflates the balloon

(require 2htdp/image) 
(require 2htdp/universe)

(define large 50)

(define (balloon b) 
  (if (<= b 10)
      (text "press any key now" 22 'red)
      (circle b "solid" "red")))

(define (blow-up b k) large)

(define (deflate b) (max (- b 1) 1))

(big-bang 20
          (on-release blow-up)
          (on-tick deflate)
          (to-draw balloon 200 200)
          (stop-when (lambda (w) (>= w large))))
