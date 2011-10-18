;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname hangman-error) (read-case-sensitive #t) (teachpacks ((lib "hangman.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "hangman.ss" "teachpack" "htdp")))))
(define (reveal-list chosen status guess)
  '(a))

(define (draw-next-part body-part)
  (begin
    "this revealed an omission in the teachpack"
    (printf "body-part ~s\n" body-part)))

(start 200 200)
(check-error (hangman-list reveal-list draw-next-part) 
             "draw-next-part: is expected to return a boolean, but it returned (void)")
