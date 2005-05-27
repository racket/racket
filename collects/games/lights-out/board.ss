(module board mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "etc.ss")
           "boards.ss")

  (provide
   new-board      ;; : (-> board)  querys user
   random-board)  ;; : (num -> board)

  (define (new-board)
    (letrec ([dialog (make-object dialog% "New Board")]
	     [mode 'prebuilt]
	     [update-mode
	      (lambda ()
		(send below-panel change-children
		      (case mode
			[(random)
			 (lambda x (list random-panel))]
			[(prebuilt)
			 (lambda x (list prebuilt-panel))]
			[(empty)
			 (lambda x (list random-panel))])))]
	     [radio-box
	      (make-object radio-box% #f (list "Prebuilt" "Random" "Empty") dialog
			   (lambda (rb evt)
			     (cond
			      [(= 0 (send rb get-selection))
			       (set! mode 'prebuilt)]
			      [(= 1 (send rb get-selection))
			       (set! mode 'random)]
			      [(= 2 (send rb get-selection))
			       (set! mode 'empty)])
			     (update-mode))
			   '(horizontal))]
	     [below-panel (make-object vertical-panel% dialog)]
	     [prebuilt-panel (make-object vertical-panel% below-panel '(border))]
	     [prebuilt
	      (make-object choice%
		#f
		(map board-name boards)
		prebuilt-panel
		(lambda (choice evt)
		  (void)))]
	     [random-panel (make-object vertical-panel% below-panel '(border))]
	     [random-slider
	      (make-object slider%
		"Board Size" 3 8 random-panel
		(lambda (slider evt)
		  (void))
		6)]
	     [button-panel (make-object horizontal-panel% dialog)]
	     [cancel? #t]
	     [ok (make-object button% "OK" 
			      button-panel
			      (lambda x
				(set! cancel? #f)
				(send dialog show #f)))]
	     [cancel (make-object button% "Cancel"
				  button-panel
				  (lambda x
				    (send dialog show #f)))])
      (update-mode)
      (send button-panel set-alignment 'right 'center)
      (set! new-board
	    (lambda ()
	      (set! cancel? #t)
	      (send dialog show #t)
	      (if cancel?
		  #f
		  (case mode
		    [(random)
		     (random-board (send random-slider get-value))]
		    [(empty)
		     (build-vector 
		      (send random-slider get-value)
		      (lambda (x) (make-vector (send random-slider get-value) 'o)))]
		    [(prebuilt)
		     (board-board (list-ref boards (send prebuilt get-selection)))]))))
      (new-board)))
  
  '(define (build-vector n f)
    (list->vector
     (let loop ([n n])
       (cond
	[(zero? n) null]
	[else (cons (f (- n 1)) (loop (- n 1)))]))))
  
  (define (random-board n)
    (let* ([choices
	    (let loop ([i n]
		       [res null])
	      (cond
	       [(zero? i) res]
	       [else
		(loop (- i 1)
		      (let loop ([j n]
				 [res res])
			(cond
			 [(zero? j) res]
			 [else (loop (- j 1)
				     (cons (cons (- i 1) (- j 1)) res))])))]))]
	   [board (build-vector n (lambda (x) (make-vector n 'o)))]
	   [flip
	    (lambda (i j)
	      (when (and (<= 0 i (- n 1))
			 (<= 0 j (- n 1)))
		(vector-set! (vector-ref board j) i
			     (case (vector-ref (vector-ref board j) i)
			       [(x) 'o]
			       [(o) 'x]))))]
	   [sim-click
	    (lambda (i j)
	      (flip i j)
	      (flip (- i 1) j)
	      (flip (+ i 1) j)
	      (flip i (+ j 1))
	      (flip i (- j 1)))]
	   
	   [number-of-clicks
	    (let loop ([n (* (+ n 1) 2)])
	      (cond
	       [(zero? n) 0]
	       [else (+ (random 2)
			(loop (- n 1)))]))])
      
      (let loop ([clicks number-of-clicks])
	(unless (zero? clicks)
	  (let ([choice (random (length choices))]
		[continue? (not (zero? (random 3)))]
		[choice-coordinates #f])
	    (set! choices
		  (let loop ([choices choices]
			     [n choice])
		    (cond
		     [(zero? n) 
					;(printf "choose: ~a~n" (car choices))
		      (set! choice-coordinates (car choices))
		      (cdr choices)]
		     [else (cons (car choices) (loop (cdr choices) (- n 1)))])))
	    (sim-click (car choice-coordinates)
		       (cdr choice-coordinates))
	    (loop (- clicks 1)))))
      board)))