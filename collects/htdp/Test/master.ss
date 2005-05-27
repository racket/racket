; (load "tester.ss")

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

(master check-guess)
; (master 1)
; (master first)