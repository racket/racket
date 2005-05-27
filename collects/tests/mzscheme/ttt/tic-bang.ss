;; --------------------------------------------------------------------------
;; tic-bang.ss
;; This is an imperative version.

;; This program plays through all possibilities of a tic-tac-toe
;; game, given the first move of a player. It only prints how many
;; states are being processed and how many states are terminal at
;; each stage of the game.

;; This program lacks the capability to print how a situation arose. 

;; It relies on list-library.ss. 

;; representations of fields, states, and collections of states
(define BLANK 0)

(define new-state
  (lambda ()
    (make-2vec 3 BLANK)))

(define update-state
  (lambda (state x y token)
    (2vec-set! state x y token)
    state))

(define blank?
  (lambda (astate i j)
    (eq? (2vec-ref astate i j) BLANK)))

(define clone-state
  (lambda (state)
    (let ((s (new-state)))
      (let loop ((i 0) (j 0))
	(cond
	  ((and (= i 3) (= j 0)) (void))
	  ((< j 3) (update-state s i j (2vec-ref state i j)) (loop i (+ j 1)))
	  ((< i 3) (loop (+ i 1) 0))
	  (else 'bad)))
      s)))

;(define-type state (2vector (union 'x 'o '_)))
;(define-type states (listof state))

(define PLAYER 1)
(define OPPONENT 2)

(define tic-tac-toe
  (lambda (x y)
    (tic (list (update-state (new-state) (- x 1) (- y 1) PLAYER)))))

(define make-move
  (lambda (other-move p/o tag)
    (lambda (states)
      (printf "~s: processing ~s states ~n" tag (length states))
      (let ((t (print&remove-terminals states)))
	(printf "terminal states removed: ~s~n" 
		(- (length states) (length t)))
	(if (null? t) 
	    (void)
	    (other-move (apply append (map p/o t))))))))

(define tic (make-move (lambda (x) (tac x)) (lambda (x) (opponent x)) 'tic))

(define tac (make-move (lambda (x) (tic x)) (lambda (x) (player x)) 'tac))

(define make-players
  (lambda (p/o)
    (lambda (astate)
      (let loop ((i 0) (j 0))
	(cond
	  ((and (= i 3) (= j 0)) null)
	  ((< j 3) (if (blank? astate i j)
		       (cons (update-state (clone-state astate) i j p/o)
			     (loop i (+ j 1)))
		       (loop i (+ j 1))))
	  ((< i 3) (loop (+ i 1) 0))
	  (else (error 'make-player "ouch")))))))

(define player (make-players PLAYER))

(define opponent (make-players OPPONENT))

(define print&remove-terminals
  (local ((define print-state
	    (lambda (x)
	      ;(display ".")
	      (void))))

    (collect null (lambda (_ astate rest)
		    (if (terminal? astate)
			(begin (print-state astate) rest)
			(cons astate rest))))))

(define terminal?
  (lambda (astate)
    (or (terminal-row 0 astate)
	(terminal-row 1 astate)
	(terminal-row 2 astate)
	(terminal-col 0 astate)
	(terminal-col 1 astate)
	(terminal-col 2 astate)
	(terminal-posdg astate)
	(terminal-negdg astate))))

(define terminal-row 
  (lambda (n state)
    (and (not (blank? state n 0))
	 (= (2vec-ref state n 0) (2vec-ref state n 1) (2vec-ref state n 2)))))

(define terminal-col
  (lambda (n state)
    (and (not (blank? state 0 n))
	 (= (2vec-ref state 0 n) (2vec-ref state 1 n) (2vec-ref state 2 n)))))

(define terminal-posdg
  (lambda (state)
    (and (not (blank? state 0 0))
	 (= (2vec-ref state 0 0) (2vec-ref state 1 1) (2vec-ref state 2 2)))))

(define terminal-negdg
  (lambda (state)
    (and (not (blank? state 0 2))
	 (= (2vec-ref state 0 2) (2vec-ref state 1 1) (2vec-ref state 2 0)))))
