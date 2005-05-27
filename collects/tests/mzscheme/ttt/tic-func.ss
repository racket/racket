;; --------------------------------------------------------------------------
;; tic-func.ss 
;; This program plays through all possibilities of a tic-tac-toe
;; game, given the first move of a player. It only prints how many
;; states are being processed and how many states are terminal at
;; each stage of the game. But it is constructed so that it can
;; print how to get to a winning terminal state. 

;; It relies on list-library.ss. 

;; representations of fields, states, and collections of states
(define null '())
(define-structure (entry x y who))
(define entry-field
  (lambda (an-entry)
    (list (entry-x an-entry) (entry-y an-entry))))
;(define-type state  (listof (structure:entry num num (union 'x 'o)))
;(define-type states (listof state))

(define PLAYER 'x)
(define OPPONENT 'o)

(define tic-tac-toe
  (lambda (x y)
    (tic (list (list (make-entry x y PLAYER))))))

(define make-move
  (lambda (other-move p/o tag)
    (lambda (states)
      (printf "~s: processing ~s states of length ~s ~n"
	tag (length states) (length (car states)))
      (let ((t (print&remove-terminals states)))
	(printf "terminal states removed: ~s~n"
	  (- (length states) (length t)))
	(if (null? t) 
	    (void)
	    (other-move (apply append (map p/o t))))))))

(define tic (make-move (lambda (x) (tac x)) (lambda (x) (opponent x)) 'tic))

(define tac (make-move (lambda (x) (tic x)) (lambda (x) (player x)) 'tac))

(define make-players
  (let ()
    (define rest-of-fields
      (lambda (used-fields)
	(set-minus ALL-FIELDS used-fields)))
    (lambda (player/opponent)
      (lambda (astate)
	(map (lambda (counter-move)
	       (let ((counter-x (car counter-move))
		     (counter-y (cadr counter-move)))
		 (cons (make-entry counter-x counter-y player/opponent)
		       astate)))
	     (rest-of-fields (map entry-field astate)))))))

(define player (make-players PLAYER))

(define opponent (make-players OPPONENT))

(define terminal?
  (let () (define filter-p/o
	    (lambda (p/o astate)
	      (map entry-field
		(filter (lambda (x) (eq? (entry-who x) p/o)) astate))))
    (lambda (astate)
      (and (>= (length astate) 5)
	(let ((PLAYERf (filter-p/o PLAYER astate))
	      (OPPONENTf (filter-p/o OPPONENT astate)))
	  (or 
	    (= (length astate) 9)
	    (ormap (lambda (ts) (subset? ts PLAYERf)) TERMINAL-STATES)
	    (ormap (lambda (ts) (subset? ts OPPONENTf)) TERMINAL-STATES)))))))

(define print&remove-terminals
  (let ()

	  (define print-state1
	    (lambda (x)
	      (display x)
	      (newline)))

	  (define print-state2
	    (lambda (astate)
	      (cond
		((null? astate) (printf "------------~n"))
		(else (print-state (cdr astate))
		  (let ((x (car astate)))
		    (printf " ~s @ (~s,~s) ~n"
		      (entry-who x) (entry-x x) (entry-y x)))))))

	  (define print-state
	    (lambda (x)
	      ;(display ".")
	      (void)))

    (collect null (lambda (_ astate rest)
		    (if (terminal? astate)
			(begin (print-state astate) rest)
			(cons astate rest))))))
;; fields
(define T
  (lambda (alof)
    (cond
      ((null? alof) null)
      (else (cons (list (cadr (car alof)) (car (car alof)))
		  (T (cdr alof)))))))

(define row1 (list (list 1 1) (list 1 2) (list 1 3)))
(define row2 (list (list 2 1) (list 2 2) (list 2 3)))
(define row3 (list (list 3 1) (list 3 2) (list 3 3)))
(define col1 (list (list 1 1) (list 2 1) (list 3 1)))
(define col2 (list (list 1 2) (list 2 2) (list 3 2)))
(define col3 (list (list 1 3) (list 2 3) (list 3 3)))
(define posd (list (list 1 1) (list 2 2) (list 3 3)))
(define negd (list (list 1 3) (list 2 2) (list 3 1)))

(define TERMINAL-STATES (list row1 row2 row3 col1 col2 col3 posd negd))

(define ALL-FIELDS (append row1 row2 row3))
