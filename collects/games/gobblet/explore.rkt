;; This is the main search engine for auto-play.
;; See `make-search' for the main entry point.

(module explore mzscheme
  (require mzlib/unitsig
           mzlib/etc
           mzlib/list
           "sig.rkt"
           "test.rkt")

  (provide explore-unit)

  ;; Debugging:
  (define-syntax (log-printf stx)
    (syntax-case stx ()
      [(_ n i arg ...)
       (<= (syntax-e #'n) 0) ; adjust this number for print levels
       #'(begin
	   (when (i  . < . 100)
	     (printf arg ...))
	   (void))]
      [(_ n i arg ...)
       #'(void)]))

  (define explore-unit
    (unit/sig explore^
      (import config^ model^)

      (define-struct config (max-depth memory canonicalize rate-board canned-moves))
      
      ;; make-search : (canonicalize-proc -> (board sym num num -> num))
      ;;               (canonicalize-proc hash-table -> (board sym compact xform -> plan)) 
      ;;               -> search-proc
      ;;  where canonicalize-proc = (board sym -> (cons compact xform))
      ;;  and search-proc is below.
      ;; Returns a search procedure that embeds a canonicalization table and 
      ;; a memory of canned moves from the `make-canned-moves' procedure.
      ;; The `make-canned-moves' proc can add to the given hash table, mapping
      ;;  canonical compact boards to (listof (cons num plan)), where the
      ;;  num for a plan is a rating for how good the plan is; +inf.0 means
      ;;  forced win, and -inf.0 means forced loss. A plan is created
      ;;  with `make-plan', described below.
      (define (make-search make-rate-board make-canned-moves)
	;; Long-term memory (i.e., spans searches)
	(define init-memory (make-hash-table 'equal))
	(define canonicalize (make-canonicalize))
	(define rate-board (make-rate-board canonicalize))
	(define canned-moves (make-canned-moves canonicalize init-memory))
	(when learn?
	  (load-memory init-memory canonicalize))
	;; search-proc : num num num sym board (listof board) -> play
	;;  Finds a move given search parameters, whose turn it is,
	;;  the current board, and a list of past boards (not
	;;  including the current one, used to avoid cycles in the game).
	;; The result is a play, which can be applied to a board with
	;;  `apply-play'.
	(lambda (timeout max-steps one-step-depth
			 me board history)
	  (let* ([result #f]
		 [once-sema (make-semaphore)]
		 [result-sema (make-semaphore)]
		 ;; Short-term memory (i.e., discarded after this search)
		 [memory (make-hash-table 'equal)])
	    ;; Record game-history boards as loop ties
	    (let loop ([history history][me (other me)])
	      (unless (null? history)
		(let ([key+xform (canonicalize (car history) me)])
		  (hash-table-put! memory (car key+xform) LOOP-TIE))
		(loop (cdr history) (other me))))
	    ;; Copy canned and learned info into short-term memory:
	    (hash-table-for-each init-memory (lambda (k v) (hash-table-put! memory k v)))
	    ;; Search in a background thread:
	    (let ([t (thread
		      (lambda ()
			;; Try just one chunk of lookaheads, then loop
			;; for more ambitious searches (if there's time)
			(let loop ([steps (if (= timeout +inf.0)
					      max-steps
					      1)]
				   [max-depth (if (= timeout +inf.0)
						  one-step-depth
						  2)])
			  (set! result
				;; ======== Here's where we get a move ============
				(let ([v (multi-step-minmax
					  steps
					  3 ; span
					  (make-config 
					   (min max-depth one-step-depth)
					   memory canonicalize rate-board canned-moves)
					  0 ; indent 
					  init-memory
					  me board)])
				  (log-printf 1 0 "> ~a/~a Result: ~a\n" 
					      steps (min max-depth one-step-depth)
					      (play->string v))
				  v))
			  ;; We have at least one result, now.
			  (semaphore-post once-sema)
			  ;; If we could learn more by searching deeper, then
			  ;;  do so.
			  (unless (or (and (= steps max-steps)
					   (one-step-depth . <= . max-depth))
				      ((car result) . = . +inf.0)
				      ((car result) . = . -inf.0))
			    (if (one-step-depth . <= . max-depth)
				(loop (add1 steps) 2)
				(loop steps (add1 max-depth)))))
			(semaphore-post result-sema)))])
	      ;; Sync with the background thread and return the result:
	      (sync/timeout timeout result-sema)
	      (semaphore-wait once-sema)
	      (kill-thread t)
	      (when (null? (cdr result))
		(error 'search "didn't find a move!?"))
	      (cdr result)))))

      ;; `make-plan' takes a piece size, the source position (#f and #f for off
      ;;  the board), the destination position, a xform inidcating how to
      ;;  transform the positions into canonical positions, and a number
      ;;  that estimates how many more steps until the end of game.
      (define-struct plan (size from-i from-j to-i to-j xform turns))

      ;; apply-play : board play -> board
      ;; A play is (list piece from-i from-j to-i to-j turns)
      ;;  where turns is an estimate of how many moves remain in
      ;;  the game; the turns part is not used here (it can be left
      ;;  out), but it is returned by a search-proc.
      (define (apply-play board m)
	(move board 
	      (list-ref m 0)
	      (list-ref m 1)
	      (list-ref m 2)
	      (list-ref m 3)
	      (list-ref m 4)
	      (lambda (new-board)
		new-board)
	      (lambda ()
		(error 'apply-play "bad move: ~a" m))))

      ;; ------------------------------------------------------------
      ;; Checking and combining move plans
            
      (define delay-loss? #t)

      ;; Discourage loops:
      (define LOOP-TIE `((-1000.0 loop!)))
      
      ;; Translates a plan into the given coordinate xform
      (define (xlate m xform)
	(let-values ([(from-i from-j)
		      (if (plan-from-i m)
			  (unapply-xform xform (apply-xform (plan-xform m)
							    (plan-from-i m)
							    (plan-from-j m)))
			  (values #f #f))]
		     [(to-i to-j)
		      (unapply-xform xform (apply-xform (plan-xform m)
							(plan-to-i m)
							(plan-to-j m)))])
	  (make-plan (plan-size m) from-i from-j to-i to-j xform (plan-turns m))))

      (define (found-win? v)
	(and (pair? v)
	     (= (caar v) +inf.0)))

      (define (immediate? v)
	(and (pair? v) (zero? (get-depth (car v)))))

      (define (found-lose? v)
	(and (pair? v)
	     (= (caar v) -inf.0)))

      (define (get-depth a)
	(if (plan? (cdr a))
	    (plan-turns (cdr a))
	    0))

      ;; Keeps the best move --- up to `span' of them --- in `a' and `b'.
      ;;  The two lists are sorted, and the result should keep them sorted.
      ;;  For -inf.0 ratings, prefer the move farthest from the end of the
      ;;  game, otherwise prefer the move closest.
      (define (best span a b)
	(cond
          ;; First, cases where span, a, or b goes to zero/null: 
          [(zero? span) null]
          [(null? a)
           (if (null? b)
               null
               (cons (car b) (best (sub1 span) null (cdr b))))]
          [(null? b)
           (cons (car a) (best (sub1 span) null (cdr a)))]
          ;; Pick best from first of a and first of b:
          ;; - Case 1: a is rated better
          [(> (caar a) (caar b))
           (cons (car a) (best (sub1 span) (cdr a) b))]
          ;; - Case 2: b is rated better
          [(< (caar a) (caar b))
           (cons (car b) (best (sub1 span) a (cdr b)))]
          ;; - Case 3: same ratings, so pick based on distance to end-of-game
          ;;  - Subcase 1: we're picking between losses, and we want to delay the loss
          [(and delay-loss? 
                (= (caar a) -inf.0))
           (if (> (get-depth (car a)) (get-depth (car b)))
               (cons (car a) (best (sub1 span) (cdr a) b))
               (cons (car b) (best (sub1 span) a (cdr b))))]
          ;;  - Subcase 2: a reaches the end first
          [(< (get-depth (car a)) (get-depth (car b)))
           (cons (car a) (best (sub1 span) (cdr a) b))]
          ;;  - Subcase 3: b reaches the end first (or no later than a)
          [else (cons (car b) (best (sub1 span) a (cdr b)))]))

      ;; --- TESTS ---
      #;
      (let* ([plan1 (make-plan 0 0 0 0 0 0 1)]
	     [plan2 (make-plan 0 0 0 0 0 0 2)]
	     [plan1s (list (cons 2 plan1) (cons 1 plan1))])
	;; Check empty/zero combinations:
	(test null (best 20 null null))
	(test plan1s (best 2 plan1s null))
	(test plan1s (best 2 null plan1s))
	(test plan1s (best 20 null plan1s))
	(test null (best 0 plan1s plan1s))
	;; Check rating choice
	(test (list (cons 2 plan1)) (best 1 (list (cons 2 plan1)) (list (cons 1 plan1))))
	(test (list (cons 1 plan1)) (best 1 (list (cons 1 plan1)) (list (cons -inf.0 plan2))))
	(test (list (cons 1 plan2)) (best 1 (list (cons -inf.0 plan1)) (list (cons 1 plan2))))
	(test (list (cons 10 plan2) (cons 2 plan1)) (best 2 
							  (list (cons 2 plan1) (cons 1 plan2)) 
							  (list (cons 10 plan2) (cons 1 plan1))))
	;; Check time-til-end choice:
	(test (list (cons 1 plan1)) (best 1 (list (cons 1 plan1)) (list (cons 1 plan2))))
	(test (list (cons -inf.0 plan2)) (best 1 (list (cons -inf.0 plan1))
                                               (list (cons -inf.0 plan2)))))

      ;; ------------------------------------------------------------
      ;;  Multi-step minmax (non-exhaustive):
      
      (define hit-count 0)
      (define depth-count 0)
      (define explore-count 0)
      (define enter-count 0)
      (define move-count 0)

      ;; ...state and search params... -> (values (listof (cons num plan)) xform)
      ;; Minimax search up to the given max-depth, returning up to span
      ;; choices of move.
      (define (minmax depth span config me board last-to-i last-to-j)
	(set! hit-count (add1 hit-count))
	(let* ([board-key+xform ((config-canonicalize config) board me)]
	       [board-key (car board-key+xform)]
	       [xform (cdr board-key+xform)]
	       [key (vector board-key (- (config-max-depth config) depth) span)])
	  (let ([choices
		 (cond
		  ;; Check for known win/loss at arbitrary depth:
		  [(hash-table-get (config-memory config) board-key (lambda () #f)) 
                   => (lambda (x) x)]
		  ;; Check for known result at specific remaining depth:
		  [(hash-table-get (config-memory config) key (lambda () #f)) 
                   => (lambda (x) x)]
		  ;; Check for immediate loss (only rating matters; plan is never used)
		  [(winner? board (other me))
		   (hash-table-put! (config-memory config) board-key '((-inf.0)))
		   '((-inf.0))]
		  ;; Check for immediate loss (only rating matters)
		  [(winner? board me)
		   (hash-table-put! (config-memory config) board-key '((+inf.0)))
		   '((+inf.0))]
		  ;; Check for depth
		  [(depth . >= . (config-max-depth config))
		   (set! depth-count (add1 depth-count))
		   (let ([l (list 
                             (list ((config-rate-board config) board me last-to-i last-to-j)))])
		     (hash-table-put! (config-memory config) key l)
		     l)]
		  ;; Otherwise, we explore this state...
		  [else 
		   (set! depth-count (add1 depth-count))
		   (set! explore-count (add1 explore-count))
		   ;; In case we get back here while we're looking, claim an unknown tie:
		   (hash-table-put! (config-memory config) board-key LOOP-TIE)
		   (let* ([choices
			   (map (lambda (g)
				  ;; Make sure each canned move is in our coordinate system:
				  (cons (car g) (xlate (cdr g) xform)))
				((config-canned-moves config) board me board-key xform))]
			  [choices
			   (if (found-win? choices)
			       choices
			       (try-all-enters choices depth span config
					       me board xform))]
			  [choices
			   (if (found-win? choices)
			       choices
			       (try-all-moves choices depth span config
					      me board xform))]
			  [choices (if (null? choices)
				       ;; No moves! We lose
				       '((-inf.0))
				       ;; We have at least one move
				       choices)])
		     (hash-table-remove! (config-memory config) board-key)
		     (let ([key (if (and ((caar choices) . < . +inf.0)
					 ((caar choices) . > . -inf.0))
				    ;; Result is only valid to current depth limit:
				    key
				    ;; Win or loss: result is valid to any depth:
				    board-key)])
		       (hash-table-put! (config-memory config) key choices)
		       choices))])])
	    (values choices xform))))

      ;; try-all-enters : ... -> (listof (cons num plan))
      ;; Try moving each available off-board piece onto each board position
      (define (try-all-enters choices depth span config me board xform)
	(let loop ([enters (pick-enters board me)]
		   [choices choices])
	  (if (null? enters)
	      choices
	      ;; For this piece....
	      (let ([p (list-ref (if (eq? me 'red) red-pieces yellow-pieces) 
				 (car enters))])
		(loop (cdr enters)
		      ;; ... try every target position:
		      (fold-board/choices
		       span choices
		       (lambda (i j)
			 (try-move depth config 
				   board me
				   p #f #f i j xform))))))))
      
      ;; try-all-moves : ... -> (listof (cons num plan))
      ;; Try moving each on-board piece onto each other board position
      (define (try-all-moves choices depth span config me board xform)
	;; From each source...
	(fold-board/choices
	 span choices
	 (lambda (from-i from-j)
	   ;; ... if it has my piece...
	   (let ([l (board-ref board from-i from-j)])
	     (if (and (pair? l)
		      (eq? me (piece-color (car l))))
		 ;; ... try every target position:
		 (fold-board/choices
		  span choices
		  (lambda (to-i to-j)
		    (try-move depth config 
			      board me 
			      (car l) from-i from-j to-i to-j xform)))
		 ;; Can't move from here:
		 null)))))

      ;; Try the move, and if it's ok, call `minmax' with the other
      ;; player and invert the result
      (define (try-move depth config 
			board me
			p from-i from-j to-i to-j xform)
	(move board p from-i from-j to-i to-j
	      (lambda (new-board)
		;; Move is ok; rate it
		(set! move-count (add1 move-count))
		;; Min-max recur for other player:
		(let-values ([(his-choices sxform)
			      (minmax (add1 depth) 1 config
				      (other me) new-board
				      to-i to-j)])
		  #;
		  (when (zero? depth)
                    (show-recur (piece-size p) from-i from-j to-i to-j his-choices))
		  ;; Construct a plan for this choice, and rate it
		  ;;  opposite of the minmax result
		  (list (cons (- (caar his-choices))
			      (make-plan (piece-size p) from-i from-j to-i to-j 
					 xform 
					 (add1 (get-depth (car his-choices))))))))
	      (lambda ()
		;; Move isn't ok
		null)))

      ;; pick-enters: board -> (listof num)
      (define (pick-enters board me)
	(let loop ([avail-pieces (available-off-board board me)]
		   [played-sizes null])
	  (cond
	   [(null? avail-pieces) null]
	   [(memq (caar avail-pieces) played-sizes)
	    (loop (cdr avail-pieces)
		  played-sizes)]
	   [else
	    (cons 
	     ;; piece to move:
	     (caar avail-pieces)
	     ;; Try pieces from other stacks:
	     (loop (cdr avail-pieces)
		   (cons (caar avail-pieces) played-sizes)))])))

      ;; Like `fold-board', but auto combines choices and
      ;;  handles shortcut for known immediate wins
      (define (fold-board/choices span choices f)
	(fold-board
	 (lambda (i j choices)
	   (if (and (found-win? choices)
		    (immediate? choices))
	       choices
	       (best span
		     choices
		     (f i j))))
	 choices))

      ;; --- TESTS ---
      #;
      (let* ([plan (make-plan 0 0 0 0 0 0 1)])
	;; fold-board/choices
	(test (if (= BOARD-SIZE 3) 
		  (list (cons 4 plan) (cons 3 plan))
		  (list (cons 6 plan) (cons 5 plan)))
	      (fold-board/choices 2 null (lambda (i j)
					   (list (cons (+ i j) plan)))))

	;; pick-enters
	(let* ([one-red (move empty-board (list-ref red-pieces (sub1 BOARD-SIZE)) 
                              #f #f 0 0 values void)]
	       [two-red (move one-red (list-ref red-pieces (- BOARD-SIZE 2))
                              #f #f 1 1 values void)]
	       [three-red (move two-red (list-ref red-pieces (sub1 BOARD-SIZE))
                                #f #f 2 2 values void)]
	       [place-all (lambda (l)
			    (cdr
			     (fold-board (lambda (i j l+b)
					   (if (null? (car l+b))
					       l+b
					       (cons (cdr (car l+b))
						     (move (cdr l+b) (caar l+b) 
                                                           #f #f i j values void))))
					 (cons l empty-board))))])
	  (test (if (= BOARD-SIZE 3) '(2 1 0) '(3))
		(pick-enters empty-board 'red))
	  (test (if (= BOARD-SIZE 3) '(2 1 0) '(3 2))
		(pick-enters one-red 'red))
	  (test (if (= BOARD-SIZE 3) '(2 1 0) '(3 1))
		(pick-enters two-red 'red))
	  (test (if (= BOARD-SIZE 3) '(1 0) '(3 2 1))
		(pick-enters three-red 'red))

	  (let ([all-red-pieces (apply append 
                                       (vector->list (make-vector (sub1 BOARD-SIZE) red-pieces)))])
	    (test null (pick-enters (place-all all-red-pieces) 'red))
	    (test '(2) (pick-enters (place-all (remq (list-ref red-pieces 2) 
						     all-red-pieces))
				    'red))
	    (test (if (= BOARD-SIZE 3) '(1 0) '(1))
		  (pick-enters (place-all (remq (list-ref red-pieces 0)
						(remq (list-ref red-pieces 1) 
						      all-red-pieces)))
			       'red)))))
      
      ;; ------------------------------------------------------------
      ;;  Multi-step minmax (non-exhaustive):

      ;; Apply minmax, and if steps > 1, rate resulting moves by applying
      ;; minmax to them. Meanwhile, in learning mode, record any resulting
      ;; move that is known to lead to winning or losing.
      (define (multi-step-minmax steps span config indent init-memory me board)
	(define first-move? 
	  ((fold-board (lambda (i j v) (+ v (length (board-ref board i j)))) 0) . < . 2))
	(define now (current-inexact-milliseconds))
	(set! hit-count 0)
	(set! depth-count 0)
	(set! explore-count 0)
	(set! enter-count 0)
	(set! move-count 0)
	(log-printf 1 indent "~a> ~a Exploring for ~a\n" (make-string indent #\space) steps me)
	(let-values ([(vs xform)
		      (minmax 0
			      (if (or (steps . <= . 1) first-move?)
				  1
				  span)
			      config
			      me
			      board #f #f)])
	  (log-printf 2 indent "~a>> Done ~a ~a ~a ~a+~a [~a secs]\n" 
		      (make-string indent #\space) 
		      hit-count depth-count explore-count enter-count move-count
		      (float->string (/ (- (current-inexact-milliseconds) now) 1000)))
	  (let ([plays
		 (map (lambda (v)
			;; Transform each result, and turn it into a list
			(cons (car v) 
			      (let ([m (xlate (cdr v) xform)])
				(list (list-ref (if (eq? me 'red)
						    red-pieces
						    yellow-pieces)
						(plan-size m))
				      (plan-from-i m)
				      (plan-from-j m)
				      (plan-to-i m)
				      (plan-to-j m)
				      (get-depth v)))))
		      (filter (lambda (v) (plan? (cdr v))) vs))])
	    (log-printf 3 indent "~a>> Best Plays: ~a\n" 
			(make-string indent #\space) (plays->string 
						      (make-string (+ 15 indent) #\space)
						      plays))

	    ;; Record what we've learned...
	    (when (and learn?
		       (= steps 1))
              (record-result plays board me config init-memory))

	    (if (or (steps . <= . 1) first-move?)
		(car plays)
		(let ([nexts 
                       ;; See what the other player thinks about our candidate moves,
                       ;;  and pick the one that looks worst to the other player.
		       (if ((caar plays) . < . +inf.0)
			   (sort
			    (map
			     (lambda (play)
			       (log-printf 4 indent " ~a>>> Checking: ~a\n" 
					   (make-string indent #\space) (play->string play))
			       (if (= -inf.0 (car play))
				   (begin
				     (log-printf 4 indent " ~a>>>> losing\n" 
                                                 (make-string indent #\space))
				     play)
				   (let ([r (cons (- (car (multi-step-minmax 
							   (sub1 steps) span config 
							   (+ 3 indent) init-memory
							   (other me)
							   (apply-play board (cdr play)))))
						  (cdr play))])
				     (log-printf 4 indent " ~a>>>> deeper = ~a\n" 
						 (make-string indent #\space)
                                                 (float->string (car r)))
				     r)))
			     plays)
			    (lambda (a b) (> (car a) (car b))))
			   (list (car plays)))])
		  (car nexts))))))

      ;; ------------------------------------------------------------
      ;;  Multi-run memory:

      (define learn? #f)
      (define MEMORY-FILE
        (and learn?
             (build-path (find-system-path 'addon-dir)
                         (format "gobblet-memory-~a.rktd" BOARD-SIZE))))

      (define (record-result plays board me config init-memory)
        (when (or (found-win? plays)
                  (found-lose? plays))
          (let ([board-key+xform ((config-canonicalize config) board me)])
            (hash-table-get init-memory 
                            (car board-key+xform)
                            (lambda ()
                              ;; This is new...
                              (with-output-to-file MEMORY-FILE
                                (lambda ()
                                  (let ([m (cdar plays)])
                                    (printf "(~a ~a ~a)\n#|\n~a|#\n" 
                                            (if (found-win? plays) 'win 'lose)
                                            (car board-key+xform)
                                            (list
                                             (piece-color (list-ref m 0))
                                             (piece-size (list-ref m 0))
                                             (list-ref m 1) (list-ref m 2)
                                             (list-ref m 3) (list-ref m 4)
                                             (list-ref m 5))
                                            (board->string 0 board))))
                                'append))))))
      
      ;; to load what we've learned from previous runs
      (define (load-memory init-memory canonicalize)
	(with-handlers ([exn:fail:filesystem? void])
	  (with-input-from-file MEMORY-FILE
	    (lambda ()
	      (let loop ()
		(let ([v (read)])
		  (unless (eof-object? v)
		    (let ([board-key+xform (canonicalize (cadr v) #f)])
		      (hash-table-put! init-memory
				       (car board-key+xform)
				       (list
					(cons (if (eq? 'win (car v)) +inf.0 -inf.0)
					      (let ([n (caddr v)])
						(make-plan
						 (cadr n)
						 (list-ref n 2) (list-ref n 3)
                                                 (list-ref n 4) (list-ref n 5)
						 (cdr board-key+xform)
						 (list-ref n 6)))))))
		    (loop))))))))

      ;; ------------------------------------------------------------
      ;;  Debugging helpers

      (define (float->string v)
	(let ([s (string-append (number->string v) "000000")])
	  (substring s 0 (min 6 (string-length s)))))

      (define (play->string p)
	(format "~a (~a,~a)->(~a,~a) [~a/~a]"
		(piece-size (list-ref p 1))
		(list-ref p 2) (list-ref p 3)  (list-ref p 4) (list-ref p 5)
		(float->string (car p))
		(list-ref p 6)))

      (define (plays->string is p)
	(if (null? p)
	    "()"
	    (let ([s (plays->string is (cdr p))])
	      (if (null? (cdr p))
		  (play->string (car p))
		  (string-append (play->string (car p))
				 "\n"
				 is
				 s)))))

      (define (show-recur sz from-i from-j to-i to-j sv)
	(if (not (plan? (cdar sv)))
	    (printf "   Recur ~a (~a,~a)->(~a,~a) ; ??? = ~a/~a\n"
		    sz from-i from-j to-i to-j 
		    (caar sv) (get-depth (car sv)))
	    (printf "   Recur ~a (~a,~a)->(~a,~a) ; (~a,~a)->(~a,~a) = ~a/~a\n"
		    sz from-i from-j to-i to-j 
		    (plan-from-i (cdar sv)) (plan-from-j (cdar sv)) 
		    (plan-to-i (cdar sv)) (plan-to-j (cdar sv))
		    (caar sv) (get-depth (car sv))))))))
