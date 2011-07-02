;; Checks that all paths in a tree of games leads to the expected
;; winner. It also generates information for known plays to be used to
;; speed up future games (i.e., converts learned strategy to a compact
;; form).

(module check mzscheme
  (require mzlib/unitsig
           mzlib/etc
           mzlib/list
           "sig.rkt"
           "model.rkt"
           "explore.rkt"
           "heuristics.rkt")

  (define board-size 3)
  (define cannon-size +inf.0)

  (invoke-unit/sig
   (compound-unit/sig
    (import)
    (link
     [CONFIG : config^ ((unit/sig config^
			  (import)
			  (define BOARD-SIZE board-size)))]
     [MODEL : model^ (model-unit CONFIG)]
     [HEURISTICS : heuristics^ (heuristics-unit CONFIG MODEL EXPLORE)]
     [EXPLORE : explore^ (explore-unit CONFIG MODEL)]
     [ROBOT : () ((unit/sig ()
		    (import config^ explore^ model^ heuristics^)
		    
		    (define (mk-search)
		      (make-search make-3x3-rate-board make-3x3-canned-moves))
		    
		    (define FLUSH-CACHE-COUNT 10)

		    (let ([search (mk-search)]
			  [cnt 0]
			  [move-map (make-hash-table 'equal)]
			  [canonicalize (make-canonicalize)])
		      (let loop ([board empty-board]
				 [depth 0]
				 [history null])
			(set! cnt (+ cnt 1))
			(when (= cnt FLUSH-CACHE-COUNT)
			  ;; Keep the canonlicalization information in `search'
			  ;; from getting too big.
			  (set! cnt 0)
			  (set! search (mk-search)))
			(printf "------------\n~a\n" (board->string depth board))
			(cond
			 [(winner? board 'red) 0]
			 [(winner? board 'yellow)
			  (error '! "yellow wins")]
			 [else
			  (let ([key+xform (canonicalize board 'red)])
			    (list-ref
			     (hash-table-get 
			      move-map (car key+xform)
			      (lambda ()
				(let ([play (search 300.0 1 2 'red board history)])
				  (let ([new-board (apply-play board play)])
				    (let ([max-depth
					   (if (winner? new-board 'red)
					       0
					       (max
						(let ([pss (available-off-board new-board 'yellow)])
						  (apply
						   max
						   (map
						    (lambda (ps)
						      (fold-board
						       (lambda (i j v)
							 (move new-board (list-ref yellow-pieces (car ps))
							       #f #f i j
							       (lambda (newer-board)
								 (max v
								      (loop newer-board
									    (add1 depth)
									    (list* new-board board history))))
							       (lambda () v)))
						       0))
						    pss)))
						(fold-board
						 (lambda (from-i from-j v)
						   (let ([ps (board-ref new-board from-i from-j)])
						     (if (and (pair? ps)
							      (eq? 'yellow (piece-color (car ps))))
							 (fold-board
							  (lambda (to-i to-j v)
							    (move new-board (car ps)
								  from-i from-j to-i to-j
								  (lambda (newer-board)
								    (max v
									 (loop newer-board
									       (add1 depth)
									       (list* new-board board history))))
								  (lambda () v)))
							  v)
							 v)))
						 0)))])
				      (let ([l (list (piece-size (car play))
						     (and (list-ref play 1)
							  (apply-xform (cdr key+xform)
								       (list-ref play 1) (list-ref play 2)))
						     (apply-xform (cdr key+xform)
								  (list-ref play 3) (list-ref play 4))
						     (add1 max-depth))])
					(hash-table-put! move-map (car key+xform) l)
					l))))))
			     3))]))
		      (hash-table-for-each move-map
					   (lambda (k v)
					     (when (> (list-ref v 3) 1)
					       (printf "~s\n" (cons k v)))))))
		  CONFIG EXPLORE MODEL HEURISTICS)])
    (export))))
