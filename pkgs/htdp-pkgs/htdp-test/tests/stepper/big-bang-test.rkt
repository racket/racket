;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname big-bang-test) (read-case-sensitive #t) (teachpacks ((lib "universe.ss" "teachpack" "2htdp") (lib "image.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.ss" "teachpack" "2htdp") (lib "image.ss" "teachpack" "2htdp")))))
(define (draw-world x) (rectangle 100 (round (* x 0.1)) "solid" "red"))

(draw-world 500)

(big-bang 0 
          [on-tick add1]
          [to-draw draw-world 500 500])
