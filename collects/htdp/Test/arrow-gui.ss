;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname arrow-gui) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require htdp/arrow-gui)
(require htdp/gui)

(define (left b e) (draw-message msg "left"))
(define (right b e) (draw-message msg "right"))
(define (up b e) (draw-message msg "up"))
(define (down b e) (draw-message msg "down"))

(define msg (make-message (make-string 22 #\space)))
(check-expect (window? (create-window (list (list msg)))) true)
(check-expect (connect left right up down) true)
