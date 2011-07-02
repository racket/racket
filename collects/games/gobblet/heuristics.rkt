;; Supplies canned moves and board-rating functions for the state
;; explorer.

(module heuristics mzscheme
  (require mzlib/unitsig
           mzlib/etc
           mzlib/list
           "sig.rkt"
           "plays-3x3.rkt")

  (provide heuristics-unit)

  (define heuristics-unit
    (unit/sig heuristics^
      (import config^ model^ explore^)

      (define (make-3x3-canned-moves canonicalize init-memory)
	;; Add known good plays to init-memory. These plays define
	;; a perfect red player.
	(for-each (lambda (play)
		    (let ([key+xform (canonicalize (list->bytes (vector->list (car play))) #f)])
		      (hash-table-put! init-memory
				       (car key+xform)
				       (let-values ([(from-i from-j)
						     (if (list-ref play 2)
							 (unapply-xform (cdr key+xform) (list-ref play 2))
							 (values #f #f))]
						    [(to-i to-j)
						     (unapply-xform (cdr key+xform) (list-ref play 3))])
					 (list
					  (cons +inf.0
						(make-plan
						 (list-ref play 1)
						 from-i from-j to-i to-j
						 (cdr key+xform)
						 (list-ref play 4))))))))
		  3x3-plays)
	(lambda (board me k xform)
	  null))

      (define (make-3x3-no-canned-moves canonicalize init-memory) 
	(lambda (board me k xform)
	  null))

      (define (make-3x3-rate-board canon)
	(lambda (board me to-i to-j)
	  (+ (random)
	     ;; Occupying the middle cell seems good
	     (rate-cell board me 1 1))))

      (define (make-4x4-canned-moves canon init-memory) 
	(lambda (board me k xform)
	  null))

      (define (make-4x4-rate-board canon)
	(lambda (board me to-i to-j)
	  (+ (random) 
	     (if (and (top-color? board to-i to-j (other me))
		      (3-in-a-row? board to-i to-j (other me)))
		 -10
		 0)
	     ;; Controlling the middle cells seems good
	     (rate-cell board me 1 1)
	     (rate-cell board me 1 2)
	     (rate-cell board me 2 1)
	     (rate-cell board me 2 2))))

      (define (rate-cell board me i j)
	(let ([l (board-ref board i j)])
	  (if (pair? l)
	      (if (eq? (piece-color (car l)) me)
		  2
		  -2)
	      0)))

      (define (top-color? board i j c)
	(let ([l (board-ref board i j)])
	  (and (pair? l)
	       (eq? (piece-color (car l)) c)))))))
