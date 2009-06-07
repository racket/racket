(require-library "pretty.ss")

(unit/sig loa^
  (import mzlib:function^
	  mred^
	  loa:computer-player^
	  loa:grid^)

  (define color "VIOLET RED")

  (define black-pen (send the-pen-list find-or-create-pen "BLACK" 1 'solid))
  (define black-brush (send the-brush-list find-or-create-brush "BLACK" 'solid))
  (define white-pen (send the-pen-list find-or-create-pen color 1 'solid))
  (define white-brush (send the-brush-list find-or-create-brush color 'solid))

  (define loa-canvas% grid-canvas%)

  (define (get-connected-region board snip)
    (let ([ht (make-hash-table)])
      (let loop ([snip snip])
	(hash-table-get
	 ht
	 snip
	 (lambda ()
	   (hash-table-put! ht snip #t)
	   (let* ([x (send snip get-x)]
		  [y (send snip get-y)]
		  [check
		   (lambda (nx ny)
		     (let* ([next-snip (send board get-snip-at nx ny)]
			    [condition
			     (and next-snip
				  (eq? (send next-snip get-color)
				       (send snip get-color)))])
		       (printf "at (~a, ~a) looking (~a, ~a): ~a~n" x y nx ny condition)
		       (when condition
			 (loop next-snip))))])
	     (check (+ x 1) y)
	     (check (- x 1) y)
	     (check x (+ y 1))
	     (check x (- y 1))
	     (check (+ x 1) (+ y 1))
	     (check (- x 1) (+ y 1))
	     (check (+ x 1) (- y 1))
	     (check (- x 1) (- y 1))))))
      (hash-table-map ht (lambda (x y) x))))

  (define (get-connected-regions board)
    (let loop ([regions null]
	       [snip (send board find-first-snip)])
      (cond
       [(not snip) regions]
       [(ormap (lambda (region) (member snip region))
	       regions)
	(loop regions
	      (send snip next))]
       [else
	(loop (cons (get-connected-region board snip) regions)
	      (send snip next))])))

  (define loa-pasteboard%
    (class/d grid-pasteboard% (size)
      ((inherit get-snip-at get-all-snips-at
		animate-to find-first-snip
		remove)
       (public get-size get-color-at
       (override get-moves moved))

      (define (get-size) size)

      (define (make-move snip x y)
	(send snip set-x x)
	(send snip set-y y)
	(animate-to snip x y))

      (define (do-computer-move)
	(let-values ([(snip x y) (computer-move this)])
	  (make-move snip x y)))

      (define (moved moved-snips)
	(for-each (lambda (moved-snip)
		    (for-each (lambda (overlapping-snip)
				(unless (eq? overlapping-snip moved-snip)
				  (remove overlapping-snip)))
			      (get-all-snips-at (send moved-snip get-x) (send moved-snip get-y))))
		  moved-snips)
	(let ([semaphore (make-semaphore)])
	  (thread
	   (lambda ()
	     (do-computer-move)
	     (semaphore-post semaphore)))
	  (yield semaphore)))

      (define (get-color-at i j)
	(let loop ([snip (find-first-snip)])
	  (cond
	   [(not snip) #f]
	   [else (if (and (= i (send snip get-x))
			  (= j (send snip get-y)))
		     (send snip get-color)
		     (loop (send snip next)))])))

      (define get-moves
	(let* ([get-color (lambda (board i j) (get-color-at i j))]
	       [f 
	       (invoke-unit/sig (require-library "moves.ss" "games" "loa")
				loa:move-import^
				mzlib:function^)])
	  (lambda (snip)
	    (f (void) (send snip get-x) (send snip get-y)))))

      (super-init size size)))

  (define loa-checker%
    (class grid-snip% (color x y)

      (inherit get-width get-height)
      (public
	[get-color
	 (lambda () color)])
      (override
	[draw
	 (lambda (dc x y left top right bottom dx dy draw-caret)
	   (let ([width (get-width)]
		 [height (get-height)])
	     (if (eq? color 'black)
		 (begin (send dc set-pen black-pen)
			(send dc set-brush black-brush))
		 (begin (send dc set-pen white-pen)
			(send dc set-brush white-brush)))
	     (send dc draw-ellipse x y width height)))])

      (sequence
	(super-init x y))))


  (define frame (make-object frame% "Lines of Action" #f))
  (define loa-pasteboard (make-object loa-pasteboard% 8))
  (define loa-canvas (make-object loa-canvas% frame loa-pasteboard))

  (send loa-canvas min-width 300)
  (send loa-canvas min-height 300)

  (define (make color x y)
    (send loa-pasteboard insert
	  (make-object loa-checker% color x y)))

  (let loop ([n 6])
    (unless (zero? n)
      (make 'white 0 n)
      (make 'white 7 n)
      (make 'black n 0)
      (make 'black n 7)
      (loop (- n 1))))

  (send frame show #t))

)
