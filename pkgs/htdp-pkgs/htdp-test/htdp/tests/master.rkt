;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname master) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require htdp/master)

; (load "tester.rkt")

;; check-guess : color color color color -> symbol

(define (check-guess target1 target2 guess1 guess2)
  (cond
    ((and (eq? target1 guess1) (eq? target2 guess2))
     'perfect)
    ((or  (eq? target1 guess1) (eq? target2 guess2))
     'one_color-at_correct_position)
    ((or  (eq? target1 guess2) (eq? target2 guess1))
     'the_colors_occur)
    (else 'nothing_correct)))

;; Tests: 
(eq? (check-guess 'white 'blue 'white 'blue) 'perfect)
(eq? (check-guess 'white 'blue 'red 'blue) 'one_color-at_correct_position)
(eq? (check-guess 'white 'blue 'blue 'red) 'the_colors_occur)
(eq? (check-guess 'white 'blue 'red 'green) 'nothing_correct)

(check-expect (master check-guess) true)
