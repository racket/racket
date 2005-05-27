(define-signature masterTS (go))

(define (masterT N)
  (unit/sig masterTS (import (m : masterS) plt:userspace^)
    ;; check-guess-two : sym sym sym sym -> sym
    ;; to determine whether targetI and guessI are the same
    ;; or whether at least some of guessI occur in targetI 
    (define (check-guess-two target1 target2 guess1 guess2)
      (cond
	((and (eq? target1 guess1) (eq? target2 guess2))
	 'perfect_guess)
	((or (eq? target1 guess1) (eq? target2 guess2))
	 'one_color_at_correct_position)
	((or (eq? target2 guess1) (eq? target1 guess2))
	 'the_colors_occur)
	(else 'nothing_correct)))

    ;; check-guess-multiple : (listof sym) (listof sym) -> sym
    ;; to determine whether guesses and choices are the same
    ;; or whether at least some of guesses occur in choices
    (define (check-guess-multiple choices guesses)
      (cond
	((equal? choices guesses) 'perfect_guess)
	(else (let* ((same-position (filter identity (map eq? choices guesses)))
		     (common (filter (lambda (x) (memq x choices)) guesses)))
		(cond
		  ((pair? same-position) 'some_colors_are_in_proper_position)
		  ((pair? common) 'some_colors_occur_in_chosen_sequence)
		  (else 'all_wrong))))))

    (define (go)
      (m:repl (if (= N 2) check-guess-two check-guess-multiple)))))

(compound-unit/sig (import (PLT : plt:userspace^))
  (link
    (TEST : masterTS  ((masterT 3) MASTER PLT))
    (MASTER : masterS ((masterU 3) ERR PLT))
    (ERR  : errorS    (errorU)))
  (export (open TEST)))
	  
