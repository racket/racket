;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname stop-when-error) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

(define (render x) (circle x "solid" "blue"))

(check-error (big-bang 10
                       (to-draw render)
                       (on-tick sub1)
                       (stop-when zero? (circle 10 "solid" "red")))
             "stop-when: expected a function as second argument; given #<image>")