(module waterworld mzscheme
  (require (lib "class.ss"))
  (require (lib "file.ss"))
  (require (lib "list.ss"))
  (require (lib "etc.ss"))
  (require (lib "mred.ss" "mred"))
  (require (lib "external.ss" "browser"))
  
  (define *progname* "WaterWorld")

  (define *prefs-file* 
	(let*-values 
	 ([(sys-prefs-file) (find-system-path 'pref-file)]
	  [(prefs-dir nm mbd) (split-path sys-prefs-file)])
	 (build-path prefs-dir ".ww-prefs.ss")))

  (define (get-ww-pref sym default)
    (get-preference sym (lambda () default) #t *prefs-file*))

  (define (put-ww-prefs ss)
    (let ([syms (map car ss)]
	  [vals (map cadr ss)])
      (put-preferences syms vals
		       (lambda _
			 (message-box
			  "WaterWorld error"
			  "Error saving preferences"
			  #f
			  '(ok)))
		       *prefs-file*)))

  ; base dimensions
  (define *base-tile-edge-length* 48) ; length of triangular tile edge

  ; scaled dimensions
  (define *large-tile-scaling-factor* 2.0)
  (define *small-tile-scaling-factor* 1.0)
  (define *tile-scaling-factor* #f)
  (define (tile-scale n)
    (inexact->exact (round (* *tile-scaling-factor* n))))

  (define *tile-edge-length* #f)
  (define *half-edge-length* #f)
  (define *tile-height* #f)
  (define *half-tile-height* #f)
  (define *even-slope* #f)
  (define *odd-slope* #f)

  ; height of equilateral triangle
  (define *base-tile-height* 
    (inexact->exact (ceiling (* (/ (expt 3 1/2) 2.0) 
				*base-tile-edge-length*))))
  (define *base-half-tile-height* (/ *base-tile-height* 2))

  (define *teaching-board-height* 4)
  (define *teaching-board-width* 6)
  (define *teaching-tile-size* 'large)

  (define *default-rows* *teaching-board-height*)
  (define *default-cols* *teaching-board-width*)
  (define *default-density* 20)
  (define *default-tile-size* 'large)
  (define *default-autoclick* 'yes)

  (define *current-rows* (get-ww-pref 'ww:numrows *default-rows*))
  (define *current-cols* (get-ww-pref 'ww:numcols *default-cols*))
  (define *current-density* (get-ww-pref 'ww:density *default-density*))
  (define *current-tile-size* (get-ww-pref 'ww:tile-size *default-tile-size*))
  (define *current-autoclick* (get-ww-pref 'ww:autoclick *default-autoclick*))

  (define *last-game-dir* #f) 

  (define *click-sem* (make-semaphore 1))

  (define *default-message-1*
    "Hold Shift to indicate a pirate (instead of safe water)")
  (define *default-message-2*
    "Hold Control to make an assertion (not just a guess)")
  (define *checking-message*
    "Considering your move ...")

  (define (make-bitmap s)
    (make-object bitmap% 
		 (build-path 
		  (collection-path "waterworld") s) 'gif))
  (define *jolly-bitmap* #f)
  (define *jolly-ce-bitmap* #f)
  (define *jolly-large-bitmap* (make-bitmap "jolly-large.gif"))
  (define *jolly-small-bitmap* (make-bitmap "jolly-small.gif"))
  (define *jolly-large-ce-bitmap* (make-bitmap "jolly-large-ce.gif"))
  (define *jolly-small-ce-bitmap* (make-bitmap "jolly-small-ce.gif"))

  (define *jolly-small-desc* (list *jolly-small-bitmap* *jolly-small-ce-bitmap* 16 16))
  (define *jolly-large-desc* (list *jolly-large-bitmap* *jolly-large-ce-bitmap* 23 27))

  (define *jolly-width* #f)
  (define *jolly-height* #f) 
  (define *jolly-column-offset* #f)
  (define *jolly-row-offset-fraction* 1/4)
  (define *jolly-even-row-offset* #f)
  (define *jolly-odd-row-offset* #f)

  (define (set-tile-dimensions!)
    (set! *tile-scaling-factor*
	 (case *current-tile-size*
	   [(large) *large-tile-scaling-factor*]
	   [(small) *small-tile-scaling-factor*]
	   [else 
	    (error (format "Unknown tile size: ~a" *current-tile-size*))]))
    (set! *tile-edge-length* (tile-scale *base-tile-edge-length*))
    (set! *half-edge-length* (/ *tile-edge-length* 2))
    (set! *tile-height*  (inexact->exact 
			  (ceiling (* (/ (expt 3 1/2) 2.0) 
				      *tile-edge-length*))))
    (set! *half-tile-height* (/ *tile-height* 2))
    (set! *even-slope* (/ *tile-height* (- *half-edge-length*)))
    (set! *odd-slope* (- *even-slope*))
    (let-values 
     ([(bmp ce-bmp w h) (apply values 
			       (case *current-tile-size*
				 ((large) *jolly-large-desc*) 
				 ((small) *jolly-small-desc*)
				 (else "This is unreachable")))])
     (set! *jolly-bitmap* bmp)
     (set! *jolly-ce-bitmap* ce-bmp)
     (set! *jolly-width* w)
     (set! *jolly-height* h))
    (set! *jolly-column-offset* 
	  (round (/ (- *tile-edge-length* *jolly-width*) 2)))
    (set! *jolly-even-row-offset* 
	  (round (* (- *tile-height* *jolly-height*) 
		    (- 1 *jolly-row-offset-fraction*))))
    (set! *jolly-odd-row-offset* 
	  (round (* (- *tile-height* *jolly-height*) 
		    *jolly-row-offset-fraction*))))

  (set-tile-dimensions!)
    
  (define *excluded-labels* '("I" "O"))
  (define *teaching-mode-labels*
    (let ([offset (char->integer #\A)])
      (list->vector 
       (filter (lambda (c) (not (member c *excluded-labels*)))
	       (build-list 26 (lambda (n) 
				(string (integer->char 
					 (+ n offset)))))))))
  (define (make-color s)
    (make-object color% s))
  (define *alpha-color*
    (make-color "seagreen"))
  (define *zero-color*
    (make-color "gray"))
  (define *non-zero-color*
    (make-color "red"))
  (define (make-colored-brush s)
    (make-object brush% (make-color s) 'solid))
  (define *concealed-brush* 
    (make-colored-brush "lightblue"))
  (define *exposed-brush* 
    (make-colored-brush "white"))
  (define *counterexample-brush*
    (make-colored-brush "red"))

  ; limits
  (define *min-rows* 3)
  (define *max-small-rows* 18)
  (define *max-large-rows* 10)
  (define *min-cols* 3)
  (define *max-small-cols* 20)
  (define *max-large-cols* 12)
  (define *min-density* 0)
  (define *max-density* 100)

  (define *need-to-reset-size* #f)

  ; misc

  (define (trim s)
    (let ([len (string-length s)])
      (let loop ([start 0])
	(if (>= start len)
	    ""
	    (if (char-whitespace? (string-ref s start))
		(loop (add1 start))
		(let loop2 ([end (sub1 len)])
		  (if (or (<= end start)
			  (not (char-whitespace? (string-ref s end))))
		      (substring s start (add1 end))
		      (loop2 (sub1 end)))))))))

  (define (fold-string . ss)
    (foldr
     (lambda (s a)
       (if a
	   (format "~a~n~a"
		   s a)
	   s))
     #f
     ss))

  ; classes

  (define location%
    (class object% 
      (init-field 
       safe? 
       row 
       column 
       [concealed? #t]
       [unsafe-count 0])
      (field 
       [neighbors #f]
       [revealed-neighbors 0]
       [unsafe-revealed-neighbors 0]
       [counterexample-safe? #f]
       [in-counterexample-set? #f])
      (public*
	[get-row
	 (lambda () row)]
	[get-column
	 (lambda () column)]
	[get-safe?
	 (lambda () safe?)]
	[make-unsafe!
	 (lambda () (set! safe? #f))]
	[get-counterexample-safe?
	 (lambda () counterexample-safe?)]
	[set-counterexample-safe!
	 (lambda (v) (set! counterexample-safe? v))]
	[get-in-counterexample-set?
	 (lambda () in-counterexample-set?)]
	[set-in-counterexample-set!
	 (lambda (v) (set! in-counterexample-set? v))]
	[set-unsafe-count!
	 (lambda (n) (set! unsafe-count n))]
	[get-unsafe-count
	 (lambda () unsafe-count)]
	[get-concealed?
	 (lambda () concealed?)]
	[get-neighbors
	 (lambda () neighbors)]
	[get-revealed-neighbors 
	 (lambda () revealed-neighbors)]
	[incr-revealed-neighbors! 
	 (lambda () (set! revealed-neighbors (add1 revealed-neighbors)))]
	[incr-unsafe-revealed-neighbors! 
	 (lambda () (set! unsafe-revealed-neighbors (add1 unsafe-revealed-neighbors)))]
	[set-unsafe-revealed-neighbors! 
	 (lambda (n) (set! unsafe-revealed-neighbors n))]
	[get-unsafe-revealed-neighbors
	 (lambda ()
	    unsafe-revealed-neighbors)]
	[set-neighbors!
	 (lambda (ns) (set! neighbors ns))]
	[expose
	 (lambda ()
	   (if concealed?
	       (begin
		 (set! concealed? #f)
		 #t) ; indicates to caller it was concealed
	       #f))])
      (super-instantiate ())))

  (define (remove-duplicates lst)
    (if (null? lst) 
	'()
	(let ([the-car (car lst)])
	  (cons the-car
		(remq the-car
		      (remove-duplicates (cdr lst)))))))

  (define board%
    (class object%
      (init-field (rows *current-rows*) (columns *current-cols*)) 
      (field [board-vector #f]
	     [pirates-left #f]
	     [pirates-ratio #f]
	     [unsafe-count #f]
	     [num-concealed (* rows columns)]
	     [frontier-table #f]
	     [teaching-mode? #f]
	     [canvas #f]
	     [current-counterexample #f])
      (private*
       [do-board-map 
	(lambda (f update?)
	  (when board-vector
		(let ([olen (vector-length board-vector)]
		      [ilen (vector-length (vector-ref board-vector 0))])
		  (let oloop ([i 0])
		    (when (< i olen)
			  (let ([row-vec (vector-ref board-vector i)])    
			    (let iloop ([j 0])
			      (when (< j ilen)
				    (let ([r (f (vector-ref row-vec j))])
				      (when update? 
					    (vector-set! row-vec j r)))
				    (iloop (add1 j))))
			    (oloop (add1 i))))))))]
       [sum-location-unsafe
	(lambda (s)
	  (foldr (lambda (loc accum)
		   (if (send loc get-safe?)
		       accum
		       (add1 accum)))
		 0
		 s))]
       [set-unsafe-count!
	(lambda ()
	  (set! unsafe-count 
		(inexact->exact (round (* (/ *current-density* 100.0) 
					  rows columns)))))]
       [decrement-concealed!
	(lambda ()
	  (set! num-concealed (sub1 num-concealed))
	  (calc-pirates-ratio!))]
       [decrement-pirates-left!
	(lambda ()
	  (set! pirates-left (sub1 pirates-left))
	  (calc-pirates-ratio!))]
       [num-unsafe-in-assignment
	(lambda (assn)
	  (foldl 
	   (lambda (a total)
	     (if a
		 (add1 total)
		 total))
	   0
	   (map cdr assn)))]
       [beyond-frontier
	(lambda ()
	  (let ([locs null])
	    (board-for-each ; OK, mutation here
	     (lambda (loc)
	       (when (and (send loc get-concealed?)
			  (not (in-frontier? loc)))
		     (set! locs (cons loc locs)))))
	    locs))]
       [get-frontier-elements
	(lambda ()
	  (hash-table-map frontier-table (lambda (key _) key)))]
       [get-revealed-border
	(lambda ()
	  (filter (lambda (elt)
		    (and (not (send elt get-concealed?))
			 (send elt get-safe?)))
		  (remove-duplicates
		   (apply append 
			  (map (lambda (elt)
				 (get-neighbors elt))
			       (get-frontier-elements))))))]
       [get-border-counts
	(lambda (border)
	  ; assoc list, in which for each list element:
	  ; the car is a revealed neighbor of the frontier
	  ; the cdr is the difference between the number
	  ;  of unsafe neighbors (what the user sees)
	  ;  and the number of already-revealed neighbors;
	  ;  a consistent frontier assignment must contribute
	  ;  that difference 
	  (map (lambda (loc)
		 (cons loc
		       (- (send loc get-unsafe-count)
			  (send loc get-unsafe-revealed-neighbors))))
	       border))]
       [locally-consistent?
	(lambda (loc assns)
	  (let ([very-locally-consistent?
		 (lambda (nbr)
		   (let* ([nbr-nbrs (send nbr get-neighbors)]
			  [nbr-contribution
			   (foldr
			    (lambda (nbr-nbr accum)
			      (if (and (not (send nbr-nbr
						  get-concealed?))
				       (not (send nbr-nbr get-safe?)))
				  (add1 accum)
				  accum))
			    0
			    nbr-nbrs)]
			  [assn-contribution
			   (foldr
			    (lambda (a accum)
			      (let ([entry (assq a assns)])
				(if (and entry
					 (cdr entry))
				    (add1 accum)
				    accum)))
			    0
			    nbr-nbrs)])
		     (<= (+ nbr-contribution
			    assn-contribution)
			 (send nbr get-unsafe-count))))]
		[revealed-neighbors 
		 (filter (lambda (nbr) 
			   (and (not (send nbr get-concealed?))
				(send nbr get-safe?)))
			 (send loc get-neighbors))])
	    (andmap very-locally-consistent? revealed-neighbors)))]
       [check-assignment
	(lambda (curr fr assns escape-info)
	  (if (or (not (cdar assns)) ; #f consistent if (cdr assns) was
		  (locally-consistent? curr assns))
	      (gen-locally-consistent-assignments fr assns escape-info)
	      null))]
       [gen-locally-consistent-assignments 
	(lambda (fr assns escape-info)
	  (if (null? fr)
	      ; escape-info is true iff we're looking for just
	      ;  one consistent assignment
	      (if escape-info
		  (let-values
		   ([(loc safe? border
			  border-frontier-neighbors
			  border-counts
			  thunk
			  k)
		     (apply values escape-info)])
		   (if (and (let* ([loc-entry (assq loc assns)]
				   [loc-assn (and loc-entry (cdr loc-entry))])
			      ; really a counterexample
			      (and loc-entry (eq? loc-assn safe?)))
			    (check-consistency assns
					       border 
					       border-frontier-neighbors
					       border-counts))
			 ; show counterexample, then escape
		       (k (counterexample-prompt assns loc safe? thunk))
		       (list assns)))
		  (list assns))
	      (let* ([curr (car fr)]
		     [rest-fr (cdr fr)])
		(apply 
		 append
		 (map (lambda (b)
			(check-assignment
			 curr rest-fr (cons (cons curr b) assns)
			 escape-info))
		      (list #f #t))))))]
       [gen-all-consistent-assignments
	(lambda ()
	  (let* ([fr (frontier-list)]
		 [assns null]
		 [border (get-revealed-border)]
		 [border-frontier-neighbors 
		  (map (lambda (b)
			 (cons b (filter (lambda (loc) (in-frontier? loc))
					 (send b get-neighbors))))
		       border)]
		 [border-counts (get-border-counts border)])
	    (filter (lambda (assn)
		      (check-consistency assn
					 border
					 border-frontier-neighbors
					 border-counts))
		    (gen-locally-consistent-assignments fr assns #f))))]
       [all-assignments-consistent?
	; do all consistent assignments have the same
	;  value for particular location
	(lambda (loc)
	  (let* ([all-assns (gen-all-consistent-assignments)]
		 [first-assn (car all-assns)] ; must be at least one
		 [loc-val (cdr (assq loc first-assn))])
	    (let loop ([assns (cdr all-assns)])
	      (if (null? assns)
		  #t
		  (if (eq? (cdr (assq loc (car assns)))
			   loc-val)
		      (loop (cdr assns))
		      #f)))))]
       [uniform-consistent-frontiers
	(lambda ()
	  (let ([all-assns (gen-all-consistent-assignments)])
	    (let loop ([assns all-assns]
		       [count #f])
	      (if (null? assns)
		  all-assns
		  (if count
		      (if (= (num-unsafe-in-assignment (car assns))
			     count)
			  (loop (cdr assns) count)
			  #f)
		      (loop (cdr assns) (num-unsafe-in-assignment 
					 (car assns))))))))]
       [dump-assignment
	(lambda (assn)
	  (printf "*** dumping assignment ***~n")
	  (let loop ([curr assn])
	    (unless (null? curr)
		    (let ([loc (caar curr)]
			  [val (cdar curr)])
		      (printf "row: ~a col: ~a val: ~a~n"
			      (send loc get-row) 
			      (send loc get-column) 
			      val))
		    (loop (cdr curr)))))]
       [check-consistency
	(lambda (assignment border border-frontier-neighbors border-counts)
          (if (> (num-unsafe-in-assignment assignment) pirates-left)
	      #f ; can't put more in assignment than total remaining
	      (let* ([assignment-counts 
                      ; the unsafe counts for revealed tiles with frontier
		      ;  neighbor that the frontier assignment *would* yield
		      (map
		       (lambda (loc-nbrs)
			 (cons (car loc-nbrs)
			       (let loop ([nbrs (cdr loc-nbrs)])
				 (if (null? nbrs)
				     0
				     (let* ([first-nbr (car nbrs)]
					    [mem-front
					     (assq first-nbr assignment)])
				       (if (cdr mem-front)
					; increment hypothetical unsafe count
					   (add1 (loop (cdr nbrs)))
					   (loop (cdr nbrs))))))))
		       border-frontier-neighbors)])
		(let ([unsafe-count-consistent?
		       (let loop ([acs assignment-counts])
			 (if (null? acs)
			     #t
			     (let ([first-ac (car acs)])
			       (if (eq? (cdr (assq (car first-ac)
						   border-counts))
					(cdr first-ac))
				   (loop (cdr acs))
				   #f))))])
		  unsafe-count-consistent?))))]
       [counterexample-prompt
	(lambda (assignment loc safe? thunk)
	  (let ([mbox-result
		 (message-box/custom 
		  *progname*
		  (fold-string
		   "There is a counterexample to your claim"
		   (if teaching-mode?
		       (format 
			"that the tile labeled ~a is ~a."
			(vector-ref 
			 *teaching-mode-labels*
			 (+ (* (send loc get-row) 
			       *teaching-board-width*)
			    (send loc get-column)))
			(if safe? "safe" "unsafe"))
		       (format "that the tile on row ~a, column ~a is ~a."
			       (add1 (send loc get-row))
			       (add1 (send loc get-column))
			       (if safe? "safe" "unsafe")))
		   ""
		   "Do you wish to see the counterexample?"
		   "")
		  "Yes, show me"        ; button 1
		  "No, finish click"    ; button 2
		  "No, don't click"     ; button 3
		  #f
		  '(default=1)
		  3)])
	    (case mbox-result
	      [(1)
	       (set! current-counterexample assignment)
		; clear any existing counterexample
	       (board-for-each
		(lambda (loc)
		  (send loc set-in-counterexample-set! #f)))
		; set locations in assignment
	       (for-each
		(lambda (assn)
		  (let ([loc (car assn)])
		    (send loc set-counterexample-safe! (not (cdr assn)))
		    (send loc set-in-counterexample-set! #t)))
		assignment)
		; set locations not in assignment
	       (let ([unsafe-needed
		      (- pirates-left
			 (num-unsafe-in-assignment assignment))])
		 (board-for-each
		  (lambda (loc)
		    (unless (or (assq loc assignment)
				(not (send loc get-concealed?)))
			    (send loc set-in-counterexample-set! #t)
			    (if (> unsafe-needed 0)
				(begin
				  (send loc set-counterexample-safe! #f)
				  (set! unsafe-needed (sub1 unsafe-needed)))
				(send loc set-counterexample-safe! #t))))))
	       (send canvas set-in-counterexample! #t)
	       (draw)]
	      [(2) (thunk)]
	      [(3) (void)])))]
       [forced-location?
        ; returns #t if loc is required to be safe?
	(lambda (loc safe?)
	  (cond
	   [(= pirates-left 0) safe?]
	   [(= pirates-left num-concealed) (not safe?)]
	   [else #f]))]
       [find-consistent-frontier
	(lambda (loc safe? thunk)
	  (let/ec k
            (let* ([fr (frontier-list)]
		   [assns null]
		   [border (get-revealed-border)]
		   [border-frontier-neighbors 
		    (map (lambda (b)
			   (cons b (filter (lambda (loc) (in-frontier? loc))
					   (send b get-neighbors))))
			 border)]
		   [border-counts (get-border-counts border)])
	      (gen-locally-consistent-assignments 
	       fr assns 
	       (list loc safe? 
		     border
		     border-frontier-neighbors
		     border-counts
		     thunk
		     k))
	      (thunk))))])
      (public*
       [ww-messages
	(lambda (s1 s2)
	  (and canvas
	       (send canvas ww-messages s1 s2)))]
       [reset-ww-messages!
	(lambda ()
	  (and canvas
	       (send canvas reset-ww-messages!)))]
       [get-num-concealed
	(lambda () num-concealed)]
       [draw
	(lambda ()
	  (board-for-each 
	   (lambda (loc) 
	     (send canvas paint-tile loc))))]
       [clear-counterexample!
	(lambda ()
	  (send canvas set-in-counterexample! #f)
	  (draw))]
       [set-teaching-mode!
	(lambda (mode)
	  (set! teaching-mode? mode))]
       [set-canvas!
	(lambda (cnv)
	  (set! canvas cnv))]
       [update-settings!
	(lambda ()
	  (set! rows *current-rows*)
	  (set! columns *current-cols*)
	  (set-unsafe-count!))]
       [calc-unsafe!
	(lambda ()
	  (board-for-each
	   (lambda (loc)
	     (let* ([neighbors (get-neighbors loc)]
		    [unsafe/revealed
		     (foldl 
		      (lambda (nbr tot)
			(let ([c? (send nbr get-concealed?)]
			      [s? (send nbr get-safe?)])
			  (if s?
			      tot ; safe
			      (if c?
				  ; unsafe but concealed
				  (list (add1 (car tot)) (cadr tot))
				  ; unsafe revealed
				  (list (add1 (car tot)) (add1 (cadr tot)))))))
		      (list 0 0)
		      neighbors)])
	       (send loc set-unsafe-revealed-neighbors! (cadr unsafe/revealed))
	       (send loc set-unsafe-count! (car unsafe/revealed))))))]
       [do-expose-row-col
	(lambda (loc r c safe?)
	  (let ([neighbors (get-neighbors loc)]
		[actually-safe? (send loc get-safe?)])
	    (when (send loc expose)
		  (send frame draw-tile r c) ; really should notify a controller
		  (update-frontier! loc)
		  (for-each
		   (lambda (nbr)
		     (send nbr incr-revealed-neighbors!)
		     (unless actually-safe?
			     (send nbr incr-unsafe-revealed-neighbors!)))
		   neighbors)
		  (decrement-concealed!)
		  (if (not actually-safe?)
		      (decrement-pirates-left!)
		      (let* ([unsafe-count 
			      (sum-location-unsafe neighbors)])
			(send loc set-unsafe-count! unsafe-count)
			(when (eq? *current-autoclick* 'yes)
			      (when (= 0 unsafe-count)
				    (for-each
				     (lambda (nloc)
				       (when (send nloc get-concealed?)
					     (expose-row-col 
					      (send nloc get-row)
					      (send nloc get-column)
					      #f
					      #f
					      #t)))
				     neighbors))))))))]
       [guess-demerit
	(lambda (loc safe? thunk)
	  (thunk)
	  (ww-messages
	   (string-append
	    "Aaargh! Yer guessed "
	    (if (eq? safe? (send loc get-safe?))
		"right"
		"wrong") " when ya ought notta guessed!")
	   ""))]
       [check-guess
	(lambda (loc safe? thunk)
	  (thunk)
	  (unless (eq? safe? (send loc get-safe?))
		  (ww-messages
		   "Yer guess waren't so good, matey!" "")))]
       [expose-row-col
	(lambda (r c safe? assert auto-clicked?)
	  (let* ([loc (get-location r c)]
		 [expose-thunk (lambda () 
				 (do-expose-row-col 
				  loc r c safe?))])
	    (if auto-clicked?
		(expose-thunk)
		(if assert
                    ; assertion, not guess
		    (cond
		     [(forced-location? loc safe?)
		      (expose-thunk)]
		     [(forced-location? loc (not safe?))
		      (counterexample-prompt (list (cons loc safe?))
					     loc safe? expose-thunk)]
		     [(in-frontier? loc)
		      ; if there's any consistent frontier
		      ;  with the opposite assertion
		      ;  the assertion must be wrong
		      (find-consistent-frontier loc safe? expose-thunk)]
		     [else ; special cases for beyond frontier
		      (cond
		       [(uniform-consistent-frontiers)
			=>
			(lambda (frontiers)
			  (let ([k (num-unsafe-in-assignment 
				    (car frontiers))])
			    (if (= k pirates-left)
                                ; no pirates beyond frontier
				(when safe?
				    (expose-thunk)
				    (counterexample-prompt 
				     (car frontiers)
				     loc #f expose-thunk))
				(let ([b-f (beyond-frontier)])
				  (if (= (- pirates-left k)
					 (length b-f))
				      ; all beyond frontier unsafe
				      (if safe?
					  (counterexample-prompt 
					   (cons 
					    (cons loc #t)
					    (append
					     (map
					      (lambda (c)
						(cons c #t))
					      b-f)
					     (car frontiers)))
					   loc #t
					   expose-thunk)
					  (expose-thunk))
                                      ; must be a counterexample
				      (counterexample-prompt 
				       (cons
					(cons loc safe?)
					(car frontiers))
				       loc safe?
				      expose-thunk))))))]
		       [else ; a counterexample must exist
			(find-consistent-frontier loc safe? expose-thunk)])])
                    ; guess, not assertion
		    (cond
		     [(or (forced-location? loc safe?)
			  (forced-location? loc (not safe?)))
		      (guess-demerit loc safe? expose-thunk)]
		     [(in-frontier? loc)
		      (if (all-assignments-consistent? loc) ;safe?)
			    (guess-demerit loc safe? expose-thunk)
			    (check-guess loc safe? expose-thunk))]
		     [(uniform-consistent-frontiers)
		      =>
		      (lambda (frontiers)
			(let ([k (num-unsafe-in-assignment (car frontiers))])
			    (if (or (= k pirates-left)
				    (= (- pirates-left k)
				       (length (beyond-frontier))))
				(guess-demerit loc safe? expose-thunk))
			        (check-guess loc safe? expose-thunk)))]
		     [else
		      (check-guess loc safe? expose-thunk)])))))]
       [get-rows
	(lambda () rows)]
       [get-columns
	(lambda () columns)] 
       [set-rows!
	(lambda (rs)
	  (set! rows rs))]
       [set-columns!
	(lambda (cs)
	  (set! columns cs))]
       [set-size! 
	(lambda (rs cs)
	  (set-rows! rs)
	  (set-columns! cs))]
       [get-location
	(lambda (r c)
	  (let ([row-vector (vector-ref board-vector r)])
	    (vector-ref row-vector c)))]
       [get-neighbor-unsafe-count
	(lambda (r c)
	  (sum-location-unsafe (get-neighbors r c)))]
       [get-neighbors 
	(case-lambda 
	 [(loc)
	  (or (send loc get-neighbors)
	      (let ([nbrs
		     (get-neighbors
		      (send loc get-row)
		      (send loc get-column))])
		; opportunity to cache neighbor info 
		(send loc set-neighbors! nbrs)
		nbrs))]
	 [(r c)
	  (let* ([this-row (vector-ref board-vector r)]
		 [eligible-rows (list (sub1 r) r (add1 r))]
		 [eligible-cols ; row above, this row, row below
		  (if (even? (+ r c))
		      `(() (,(sub1 c) ,(add1 c)) (,c))
		      `((,c) (,(sub1 c) ,(add1 c)) ()))]
		 [neighbors
		  (let oloop ([rs eligible-rows]
			      [cs eligible-cols])
		    (if (null? rs)
			'()
			(let iloop ([cols (car cs)] ; length of cs = length of rs
				    [curr-row (car rs)])
			  (if (null? cols)
			      (oloop (cdr rs) (cdr cs))
			      (let ([curr-col (car cols)])
				(if (and (>= curr-row 0)
					 (< curr-row rows)
					 (>= curr-col 0)
					 (< curr-col columns))
				    (cons (vector-ref (vector-ref board-vector curr-row) curr-col)
					  (iloop (cdr cols) curr-row))
				    (iloop (cdr cols) curr-row)))))))])
	    neighbors)])]
       [get-pirates-left
	(lambda ()
	  pirates-left)]
       [get-pirates-ratio
	(lambda ()
	  pirates-ratio)]
       [calc-pirates-ratio!
	(lambda ()
	  (set! pirates-ratio 
		(if (= 0 num-concealed)
		    #f
		    (inexact->exact
		     (round (* 100.0 (/ pirates-left num-concealed)))))))]
       [board-for-each
	(lambda (f)
	  (do-board-map f #f))]
       [board-map!
	(lambda (f)
	  (do-board-map f #t))]
       [new-game
	(lambda ()
	  (set! board-vector
		(build-vector
		 rows
		 (lambda (r)
		   (build-vector 
		    columns
		    (lambda (c)
		      (instantiate location% ()
				   (safe? #t) (row r) (column c)))))))
		; create unsafe location pseudo-randomly
	  (random-seed (modulo (current-milliseconds) 1000))
	  (let loop ([i 0])
	    (when (< i unsafe-count)
		  (let ([rand-loc (get-location (random rows) (random columns))])
		    (if (send rand-loc get-safe?)
			(begin
			  (send rand-loc make-unsafe!)
			  (loop (add1 i)))
			(loop i)))))
	  (send frame set-default-label!)
	  (calc-neighbors!)
	  (reset-pirate-counts!)
	  (clear-counterexample!)
	  (reset-ww-messages!)
	  (reset-frontier!))]
       [reset-pirate-counts!
	(lambda () 
	  (set! pirates-left unsafe-count)
	  (set! num-concealed (* rows columns))
	  (calc-pirates-ratio!))]
       [calc-neighbors!
	(lambda ()
	  (board-for-each
	   (lambda (loc)
	     (get-neighbors loc))))]
       [calc-frontier!
	(lambda ()
	  (reset-frontier!)
	  (board-for-each
	   (lambda (loc)
	     (unless (send loc get-concealed?)
               (let* ([row (send loc get-row)]
		      [col (send loc get-column)]
		      [neighbors
		       (or (send loc get-neighbors)
			   (let ([nbrs (get-neighbors row col)])
			     (send loc set-neighbors! nbrs)
			     nbrs))]
		      [frontier-neighbors 
		       (filter (lambda (nbr) (send nbr get-concealed?))
			       neighbors)])
		 (for-each
		  (lambda (nbr)
		    (add-to-frontier nbr))
		  frontier-neighbors))))))]
       [reset-frontier!
	(lambda ()
	  (set! frontier-table (make-hash-table)))]
       [in-frontier?
	(lambda (loc)
	  (hash-table-get frontier-table loc (lambda _ #f)))]
       [add-to-frontier
	(lambda (loc)
	  (hash-table-put! frontier-table loc #t))]
       [remove-from-frontier
	(lambda (loc)
	  (hash-table-remove! frontier-table loc))]
       [update-frontier!
	(lambda (loc)
          ; this is called when a location has been exposed 
          ; if loc was in the frontier, remove it
          (when (in-frontier? loc)
	    (remove-from-frontier loc))
	  ; invariant: the neighbors of each location have been 
	  ;  calculated before calling this method
	  ; for each exposed location, add its neighbors to the 
	  ;  frontier
	  (for-each
	   (lambda (nb)
	     (unless (or (not (send nb get-concealed?))
			 (in-frontier? nb))
		     (add-to-frontier nb)))
	   (send loc get-neighbors)))]
       [frontier-list
	(lambda ()
	  (hash-table-map frontier-table (lambda (k v) k)))]
       [dump-frontier
	(lambda ()
	  (printf "Current frontier:~n")
	  (hash-table-for-each frontier-table
			       (lambda (loc _)
				 (printf "row: ~a  col: ~a~n"
					 (send loc get-row)
					 (send loc get-column)))))]
       [dump-border
	(lambda ()
	  (printf "Current border:~n")
	  (for-each
	   (lambda (loc)
	     (printf "row: ~a  col: ~a~n"
		     (send loc get-row)
		     (send loc get-column)))
	   (get-revealed-border)))]
       [load-from-file 
	(lambda (filename)
	  (if (not (file-exists? filename))
	      (message-box *progname*
			   (format "WaterWorld game file \"~a\" does not exist" filename)
			   frame)
	      (with-input-from-file filename
		(lambda ()
		  (let ([game (read)])
		    (let*-values
		     ([(_ row-info col-info locations-info)
		       (apply values game)]
		      [(locations) (cdr locations-info)]
		      [(unsafe-tally) 0]
		      [(pirates-left-tally) 0]
		      [(concealed-tally) 0])
		     (set! rows (cadr row-info))
		     (set! columns (cadr col-info))
		     (send canvas set-board-size! rows columns)
		     (send canvas update-teaching-mode!)
		     (set! board-vector
			   (build-vector
			    rows
			    (lambda (r)
			      (build-vector 
			       columns
			       (lambda (c) #f)))))
		     (board-map!
		      (lambda (_)
			(let-values 
			 ([(_ loc-row-info loc-col-info 
			      loc-safe-info loc-concealed-info)
			   (apply values (car locations))])
			 (let ([safe? (cadr loc-safe-info)]
			       [row (cadr loc-row-info)]
			       [column (cadr loc-col-info)]
			       [concealed? (cadr loc-concealed-info)])
			   (unless safe?
				   (set! unsafe-tally (add1 unsafe-tally))
				   (when concealed?
					 (set! pirates-left-tally (add1 pirates-left-tally))))
			   (when concealed?
				 (set! concealed-tally (add1 concealed-tally)))
			   (set! locations (cdr locations))
			   (instantiate location% ()
					(safe? safe?)
					(row row)
					(column column)
					(concealed? concealed?))))))
		     (set! *current-rows* rows)
		     (set! *current-cols* columns)
		     (set! *current-density* (inexact->exact 
					      (round (/ (* 100.0 unsafe-tally) 
							(* rows columns)))))
		     (set! pirates-left pirates-left-tally)
		     (set! num-concealed concealed-tally)
		     (clear-counterexample!)
		     (set-unsafe-count!)
		     (set-unsafe-count!)
		     (calc-frontier!)
		     (calc-unsafe!)
		     (calc-pirates-ratio!)))))))]
       [save-to-file 
	(lambda (filename)
	  (when (file-exists? filename)
		(delete-file filename))
	  (with-output-to-file filename
	    (lambda ()
	      (printf "(game~n")
	      (printf "  (rows ~a)~n" rows)
	      (printf "  (columns ~a)~n" columns)
	      (printf "  (locations")
	      (board-for-each
	       (lambda (loc)
		 (printf "~n    (location (row ~a) (column ~a) (safe? ~a) (concealed? ~a))" 
			 (send loc get-row) (send loc get-column) 
			 (send loc get-safe?) (send loc get-concealed?))))
	      (printf "))~n"))))])
      (super-instantiate ())
      (set-unsafe-count!)
      (reset-pirate-counts!)))
  
  (define ww-frame%
    (class frame% 
	   (init-field board)
	   (inherit set-label show get-label get-x get-y)
	   (field
	    [frame-label *progname*]
	    [game-over-frame-label 
	     (string-append frame-label
			    " [game over]")]
	    [current-filename #f]
	    [canvas #f]
	    [new-game-panel #f]
	    [status-panel #f]
	    [message-panel #f]
	    [pirates-left-msg #f]
	    [pirates-ratio-msg #f]
	    [status-msg-1 #f]
	    [status-msg-2 #f]
	    [clear-counterexample-button #f]
	    [new-game-button #f])
	   (private*
	    [draw-location-tile
	     (lambda (loc) 
	       (send canvas paint-tile loc))])
	   (public*
	    [set-default-label!
	     (lambda () (set-label frame-label))]
	    [get-top-status-line
	     (lambda () (send status-msg-1 get-label))]
	    [set-game-over-label!
	     (lambda () (set-label game-over-frame-label))]
	    [set-ce-button-state!
	     (lambda (v)
	       (send clear-counterexample-button enable v)
	       (if v
		   (send clear-counterexample-button focus)
		   (send new-game-button focus)))]
	    [new-game
	     (lambda ()
	       (if *need-to-reset-size*
		   (begin
		     (set! *need-to-reset-size* #f)
		     (set-tile-dimensions!)
		     (update-board-size! *current-rows* *current-cols*)
	             (send board update-settings!)		     	
		     (send board new-game)
		     (reset-frame!))
		   (send board new-game))
	       (update-status!)
	       (draw-board))]
	    [set-filename!
	     (lambda (s)
	       (set! current-filename s))]
	    [reset-bottom-panels!
	     (lambda ()
	       (for-each
		(lambda (panel)
		  (when panel
			(send panel show #f)))
		(list new-game-panel status-panel message-panel))
	       (set! new-game-panel 
		     (instantiate horizontal-panel% ()
				  (parent this)
				  (stretchable-height #f)
				  (vert-margin 2)
				  (alignment '(center center))))
	       (set! new-game-button
		     (instantiate button% ()
				  (label "New game")
				  (parent new-game-panel)
				  (callback (lambda (b ev) 
					      (new-game)))))
	       (set! clear-counterexample-button
		     (instantiate button% ()
				  (label "Clear counterexample")
				  (parent new-game-panel)
				  (enabled #f)
				  (callback 
				   (lambda (b ev) 
				     (send board 
					   clear-counterexample!)))))
	       (set! status-panel 
		     (instantiate horizontal-panel% ()
				  (parent this)
				  (border 2)
				  (horiz-margin 2)
				  (vert-margin 2)
				  (stretchable-height #f)
				  (style '(border))))
	       (send status-panel stretchable-height #f)
	       (let ([make-status-vpane 
		      (lambda ()
			(let ([vp (instantiate vertical-pane% ()
					       (parent status-panel))])
			  (send vp set-alignment 'center 'center)
			  vp))])
		 (set! pirates-left-msg
		       (instantiate message% ()
				    (label "Pirates left: 00000") 
				    (parent (make-status-vpane))))
		 (set! pirates-ratio-msg
		       (instantiate message% () 
				    (label "Ratio: 100%") 
				    (parent (make-status-vpane)))))
	       (set! message-panel 
		     (instantiate horizontal-panel% ()
				  (parent this)
				  (horiz-margin 2)
				  (vert-margin 2)
				  (stretchable-height #f)))
	       (send message-panel stretchable-height #f)
	       (let* ([msg-vpane 
		       (instantiate vertical-pane% ()
				    (parent message-panel))]
		      [mk-status-msg
		       (lambda (msg)
			 (instantiate message% ()
				      (label msg)
				      (stretchable-width #t)
				      (parent msg-vpane)))])
		 (set! status-msg-1
		       (mk-status-msg *default-message-1*))
		 (set! status-msg-2
		       (mk-status-msg *default-message-2*))))]
	    [update-status!
	     (lambda ()
	       (send pirates-left-msg set-label
		     (format "Pirates left: ~a" (send board get-pirates-left)))
	       (let ([ratio (send board get-pirates-ratio)])
		 (send pirates-ratio-msg set-label
		       (if ratio
			   (format "Ratio: ~a~a" ratio "%")
			   ""))))]
	    [ww-messages
	     (lambda (s1 s2)
	       (send status-msg-1 set-label s1)
	       (send status-msg-2 set-label s2))]
	    [update-board-size!
	     (lambda (rs cs)
	       (send canvas set-board-size! rs cs)
	       (send board set-size! rs cs))]
	    [dump-board ; for debugging
	     (lambda ()
	       (printf "** board dump **~n")
	       (send board 
		     board-for-each 
		     (lambda (loc) 
		       (let ([row (send loc get-row)]
			     [col (send loc get-column)]
			     [safe? (send loc get-safe?)]
			     [concealed? (send loc get-concealed?)])
			 (printf "row=~a col=~a safe?=~a concealed?=~a~n" 
				 row col safe? concealed?))))
	       (printf "** end of dump **~n"))]
	    [expose-row-col
	     (lambda (r c safe? assert)
	       (send board expose-row-col r c safe? assert #f))]
	    [draw-tile
	     (lambda (r c)
	       (draw-location-tile (send board get-location r c)))]
	    [draw-board
	     (lambda ()
	       (send board draw))])
	    (private*
	     [reset-frame! 
	      (lambda ()
		(let ([new-frame
		       (instantiate ww-frame% ()
				    (board board)
				    (label (get-label))
				    (style '(no-resize-border))
				    (x (max 0 (get-x)))
				    (y (max 0 (get-y))))])
		  (send new-frame update-board-size! 
			(send board get-rows) (send board get-columns))
		  (send new-frame update-status!)
		  (send new-frame draw-board)
		  (show #f)
		  (send new-frame show #t)
		  (set! frame new-frame)))]
	     [get-game-filename
	      (lambda ()
		(let ([fn (get-file 
			   "WaterWorld game files"
			   this
			   (or *last-game-dir*
			       (build-path (collection-path "waterworld") 
					   "games"))
			   #f
			   "ss"
			   '()
			   '(("Scheme files" "*.ss")))])
		  (when fn
			(let-values 
			 ([(base n d) (split-path fn)])
			 (set! *last-game-dir* base)))
		  fn))]
	     [save-game
	      (lambda () 
		(if current-filename
		    (send board save-to-file current-filename)
		    (save-game-as)))]
	     [save-game-as
	      (lambda () 
		(let ([filename (get-game-filename)])
		  (when filename
			(set! current-filename filename)
			(send board save-to-file current-filename))))]
	     [open-game
	      (lambda () 
		(let ([filename (get-game-filename)])
		  (when filename
			(send board load-from-file filename)
			(reset-frame!)
			(send frame set-filename! filename))))]
	     [open-settings
	      (lambda () 
		(let* ([settings-frame 
			(instantiate frame% ()
				     (label "WaterWorld settings")
				     (style '(no-resize-border)))]
		       [main-panel (instantiate vertical-panel% () 
						(parent settings-frame) 
						(alignment '(center center)))]
		       [msg-width 100]
		       [panel-sep 4]
		       [make-hpanel
			(lambda ()
			  (instantiate horizontal-panel% () 
				       (parent main-panel)
				       (vert-margin panel-sep)
				       (alignment '(center center))))]
		       [row-panel (make-hpanel)]
		       [make-msg
			(lambda (msg panel)
			  (instantiate message% () 
				       (min-width msg-width)
				       (label msg) (parent panel)))]
		       [row-msg (make-msg "Number of rows" row-panel)]
		       [make-canvas
			(lambda (panel)
			  (let ([txt (instantiate text% ())])
			    (instantiate editor-canvas% () 
					 (editor txt)
					 (min-height 30)
					 (min-width 50)
					 (stretchable-width #f)
					 (parent panel) 
					 (style '(no-hscroll no-vscroll)))))]
		       [row-canvas (make-canvas row-panel)]
		       [col-panel (make-hpanel)]
		       [col-msg (make-msg "Number of columns" col-panel)]
		       [col-text (instantiate text% ())]
		       [col-canvas (make-canvas col-panel)]
		       [density-panel (make-hpanel)]
		       [density-msg (make-msg "Pirate density (%)" density-panel)]
		       [density-text (instantiate text% ())]
		       [density-canvas (make-canvas density-panel)]
		       [tile-panel (make-hpanel)]
		       [tile-msg (make-msg "           Tile size" tile-panel)]
		       [tile-map '(large small)]  ; list position corresponds to radio button index
		       [tile-radio (instantiate radio-box% () 
						(label #f) (parent tile-panel) 
						(choices '("Large" "Small"))
						(callback (lambda (rb ev) #f))
						(style '(horizontal)))]
		       [auto-panel (make-hpanel)]
		       [auto-msg (make-msg "Autoclick empty cells?" auto-panel)]
		       [auto-map '(yes no)] ; list position corresponds to radio button index
		       [auto-radio (instantiate radio-box% () 
						(label #f) (parent auto-panel) 
						(choices '("Yes" "No"))
						(callback (lambda (rb ev) #f))
						(style '(horizontal)))]
		       [list-pos 
			(lambda (lst sym)
			  (let loop ([i 0]
				     [lst lst])
			    (if (null? lst)
				#f
				(if (eq? sym (car lst))
				    i
				    (loop (add1 i) (cdr lst))))))]
		       [get-canv-text
			(lambda (canv)
			  (send (send canv get-editor)
				get-text))]
		       [valid-number?
			(lambda (s)
			  (with-handlers
			   ([void (lambda _ #f)])
			   (string->number (trim s))))]
		       [test-range
			(lambda (v min max)
			  (or (not v)
			      (< v min)
			      (> v max)))]
		       [range-error
			(lambda (lab v min max)
			  (message-box
			   "Settings error"
			   (format "~a value \"~a\" is not a number or is out of the range [~a..~a]"
				   lab v min max)
			   settings-frame
			   '(ok)))]
		       [validate-and-save
			(lambda ()
			  (let* ([row (get-canv-text row-canvas)]
				 [col (get-canv-text col-canvas)]
				 [density (get-canv-text density-canvas)]
				 [autoclick 
				  (if (eq? (send auto-radio get-selection) 0)
				      'yes
				      'no)]
				 [tile-size 
				  (if (eq? (send tile-radio get-selection) 0)
				      'large
				      'small)]
				 [row-num (valid-number? row)]
				 [col-num (valid-number? col)]
				 [density-num (valid-number? density)])
					; validate
			    (let ([max-rows (if (eq? tile-size 'large)
						*max-large-rows*
						*max-small-rows*)]
				  [max-cols (if (eq? tile-size 'large)
						*max-large-cols*
						*max-small-cols*)])
			      (cond
			       [(test-range row-num *min-rows* max-rows)
				(range-error "Row" row *min-rows* max-rows)]
			       [(test-range col-num *min-cols* max-cols)
				(range-error "Column" col *min-cols* max-cols)]
			       [(test-range density-num *min-density* *max-density*)
				(range-error "Density percentage" density *min-density* *max-density*)]
			       [else ; save
				(let ([prefs 
				       `((ww:numrows ,row-num)
					 (ww:numcols ,col-num)
					 (ww:density ,density-num)
					 (ww:tile-size ,tile-size)
					 (ww:autoclick ,autoclick))])
				  (put-ww-prefs prefs)
				  (set! *current-rows* row-num)
				  (set! *current-cols* col-num)
				  (set! *current-density* density-num)
				  (set! *current-tile-size* tile-size)
				  (set! *current-autoclick* autoclick)
				  (set! *need-to-reset-size* #t)
				  (send settings-frame show #f))]))))]
		       [notice-panel (make-hpanel)]
		       [notice-msg (make-msg "Some settings take effect on next game" notice-panel)]
		       [init-text
			(lambda (canv v)
			  (let* ([editor (send canv get-editor)]
				 [len (string-length (send editor get-text))])
			    (send editor insert v 0 len)))]
		       [init-num-text
			(lambda (canv v)
			  (init-text canv (number->string v)))]
		       [buttons-panel (make-hpanel)]
		       [ok-button (instantiate button% ()
					       (label "OK")
					       (min-width 50)
					       (parent buttons-panel)
					       (callback (lambda (b ev) 
							   (validate-and-save))))]
		       [spacer 
			(instantiate message% () 
				     (min-width 20)
				     (label "") (parent buttons-panel))]
		       [cancel-button (instantiate button% ()
						   (label "Cancel")
						   (min-width 50)
						   (parent buttons-panel)
						   (callback (lambda (b ev) 
							       (send settings-frame show #f))))]
		       [spacer2 
			(instantiate message% () 
				     (min-width 20)
				     (label "") (parent buttons-panel))]
		       [defaults-button (instantiate button% ()
						     (label "Defaults")
						     (min-width 50)
						     (parent buttons-panel)
						     (callback 
						      (lambda (b ev) 
							(send tile-radio set-selection
							      (list-pos tile-map *default-tile-size*))
							(send auto-radio set-selection
							      (list-pos auto-map *default-autoclick*))
							(init-num-text row-canvas *default-rows*)
							(init-num-text col-canvas *default-cols*)
							(init-num-text density-canvas *default-density*))))])
		  
		  (init-num-text row-canvas *current-rows*)
		  (init-num-text col-canvas *current-cols*)
		  (init-num-text density-canvas *current-density*)
		  (send tile-radio set-selection
			(if (eq? *current-tile-size* 'large) 0 1))
		  (send auto-radio set-selection
			(if (eq? *current-autoclick* 'yes) 0 1))
		  (send settings-frame show #t)))]
	     [exit-game
	      (lambda () 
		(send this show #f)
		(exit))]
	     [how-to-play
	      (lambda () 
		(let ([url (string-append 
			    "file://"
			    (build-path 
			     (collection-path "waterworld")
			     "ww.html"))])
		  (send-url url)))])
	    (super-instantiate ())
	    (let* ([menu-bar (instantiate menu-bar% () (parent this))]
		   [game-menu (instantiate menu% ()
					   (label "&Game")
					   (parent menu-bar))]
		   [help-menu
		    (instantiate menu% ()
				 (label "&Help")
				 (parent menu-bar))]
		   [game-menu-items
		    `(("&New"
		       ,(lambda (m ev) (new-game)))
		      ("&Open..."
		       ,(lambda (m ev) (open-game)))
		      ("&Save"
		       ,(lambda (m ev) (save-game)))
		      ("Save &as..."
		       ,(lambda (m ev) (save-game-as)))
		      ("S&ettings..."
		       ,(lambda (m ev) (open-settings)))
		      ("E&xit"
		       ,(lambda (m ev) (exit-game))))]
		   [help-menu-items
		    `(("How to &play"
		       ,(lambda (m ev) (how-to-play))))])
	      (for-each
	       (lambda (it) 
		 (instantiate menu-item% ()
			      (label (car it))
			      (parent game-menu)
			      (callback (cadr it))))
	       game-menu-items)
	      (for-each
	       (lambda (it) 
		 (instantiate menu-item% ()
			      (label (car it))
			      (parent help-menu)
			      (callback (cadr it))))
	       help-menu-items)
	      (set! canvas (instantiate board-canvas% ()
					(frame this) 
					(board board)
					(tile-size *current-tile-size*)
					(stretchable-height #f)
					(stretchable-width #f)))
	      (reset-bottom-panels!))))
    
    (define board-canvas%
      (class canvas% 
	     (init-field frame) 
	     (init-field board)
	     (init-field tile-size)
	     (init-field (teaching-mode? #f))
	     (inherit get-dc min-client-width min-client-height 
		      stretchable-width stretchable-height)
	     (field
	      [board-height (send board get-rows)]
	      [board-width (send board get-columns)]
	      [intercepts-vector #f]
	      [triangle-points-even ; right-side-up, pointy side up
	       (map
		(lambda (xy)
		  (make-object point% (car xy) (cadr xy)))
		`((0 ,*tile-height*)
		  (,*half-edge-length* 0)
		  (,*tile-edge-length* 
		   ,*tile-height*)))]
	      [triangle-points-odd ; inverted triangle, pointy side down
	       (map
		(lambda (xy)
		  (make-object point% (car xy) (cadr xy)))
		`((0 0)
		  (,*half-edge-length* ,*tile-height*)
		  (,*tile-edge-length* 0)))]
	      [in-counterexample? #f])
	     (public*
	      [ww-messages
	       (lambda (s1 s2)
		 (send frame ww-messages s1 s2))]
	      [reset-ww-messages!
	       (lambda ()
		 (ww-messages *default-message-1* *default-message-2*))]
	      [end-checking-ww-messages
	       (lambda ()
		 (when (equal? (send frame get-top-status-line)
			       *checking-message*)
		       (reset-ww-messages!)))]
	      [set-in-counterexample!
	       (lambda (v)
		 (set! in-counterexample? v)
		 (send frame set-ce-button-state! v))]
	      [x-y->row-column
	       (lambda (x y)
		 (let* ([id (lambda (x) x)]
			[raw-col (floor (/ x *half-edge-length*))]
			[row (floor (/ y *tile-height*))]
			[calc-x-y
			 (lambda (m col-fun-1 col-fun-2)
			   (let ([b (get-intercept row raw-col)])
			     (if (> y (+ (* m x) b))
				 (values (col-fun-1 raw-col) row)
				 (values (col-fun-2 raw-col) row))))])
		   (if (even? (+ raw-col row))
		       (calc-x-y *even-slope* id sub1)
		       (calc-x-y *odd-slope* sub1 id))))]
	      [draw-alpha-label
	       (lambda (r c x y)
		 (let ([color (send dc get-text-foreground)])
		   (send dc set-text-foreground *alpha-color*)
		   (send dc draw-text 
			 (vector-ref *teaching-mode-labels*
				     (+ (* r *teaching-board-width*) c))
			 (+ x 14)
			 (if (even? (+ r c))
			     (+ y *tile-edge-length* -34)
			     (+ y 2)))
		   (send dc set-text-foreground color)))]
	      [paint-polygon
	       (lambda (row col xoff yoff even-tile?)
		 (send dc draw-polygon 
		       (if even-tile?
			   triangle-points-even
			   triangle-points-odd)
		       (* col *half-edge-length*)
		       (* row *tile-height*))
		 (when teaching-mode?
		       (draw-alpha-label row col xoff yoff)))]
	      [paint-jolly
	       (lambda (bitmap row col even-tile?)
		 (send dc draw-bitmap bitmap
		       (+ (* col *half-edge-length*) 
			  *jolly-column-offset*)
		       (+ (* row *tile-height*)
			  (if even-tile?
			      *jolly-even-row-offset* 
			      *jolly-odd-row-offset*))))]
	      [paint-counterexample-tile
	       (lambda (row col safe?)
		 (let ([even-tile? (even? (+ row col))])
		   (send dc set-brush *counterexample-brush*)
		   (let ([xoff (* col *half-edge-length*)]
			 [yoff (* row *tile-height*)])
		     (paint-polygon row col xoff yoff even-tile?)
		     (unless safe?
			     (paint-jolly *jolly-ce-bitmap* row col even-tile?)))))]
	      [paint-tile
	       (lambda (loc)
		 (let ([row (send loc get-row)]
		       [col (send loc get-column)])
		   (if (and in-counterexample? 
			    (send loc get-in-counterexample-set?))
		       (paint-counterexample-tile 
			row col 
			(send loc get-counterexample-safe?))
		       (let ([safe? (send loc get-safe?)]
			     [concealed? (send loc get-concealed?)]
			     [neighbor-count-thunk 
			      (lambda ()
				(send board get-neighbor-unsafe-count row col))]
			     [even-tile? (even? (+ row col))])
			 (if concealed?
			     (send dc set-brush *concealed-brush*)
			     (send dc set-brush *exposed-brush*))
			 (let ([xoff (* col *half-edge-length*)]
			       [yoff (* row *tile-height*)])
			   (paint-polygon row col xoff yoff even-tile?)
			   (unless concealed?
				   (if safe?
				       (let* ([ns (neighbor-count-thunk)]
					      [ns-string (number->string ns)]
					      [fg-color (send dc get-text-foreground)])
					 (send dc set-text-foreground 
					       (if (zero? ns)
						   *zero-color*
						   *non-zero-color*))
					 (send dc draw-text 
					       ns-string
					       (+ xoff *half-edge-length* -4) 
					       (+ yoff *half-tile-height* 
						  (if even-tile? 
						      -4
						      -12)))
					 (send dc set-text-foreground fg-color))
				       (paint-jolly *jolly-bitmap* row col even-tile?))))))))]
	      [set-intercepts!
	       (lambda ()
		 (set! intercepts-vector
		       (build-vector
			board-height
			(lambda (r)
			  (build-vector 
			   (add1 board-width)
			   (lambda (c)
					; y - mx
			     (let ([x (* (add1 c) *half-edge-length*)])
			       (if (even? (+ r c))
				   (- (* r *tile-height*)
				      (* *even-slope* x))
				   (- (* (add1 r) *tile-height*)
				      (* *odd-slope* x))))))))))]
	      [get-intercept
	       (lambda (r c)
		 (vector-ref (vector-ref intercepts-vector r) c))]
	      [set-board-width!
	       (lambda (w)
		 (set! board-width w)
		 (set-client-width!))]
	      [set-board-height!
	       (lambda (h)
		 (set! board-height h)
		 (set-client-height!))]
	      [set-board-size!
	       (lambda (h w)
		 (set-board-height! h) 
		 (set-board-width! w)
		 (set-intercepts!)
		 (update-teaching-mode!))]
	      [get-teaching-mode
	       (lambda () teaching-mode?)]
	      [update-teaching-mode!
	       (lambda ()
		 (set! teaching-mode? 
		       (and (= board-height *teaching-board-height*)
			    (= board-width *teaching-board-width*)
			    (eq? tile-size *teaching-tile-size*)))
		 (send board set-teaching-mode! teaching-mode?))]
	      [set-client-height!
	       (lambda ()
		 (min-client-height (add1 (* board-height *tile-height*))))]
	      [set-client-width!
	       (lambda ()
		 (min-client-width (* (/ (add1 board-width) 2) 
				      *tile-edge-length*)))]
	      [set-client-size!
	       (lambda ()
		 (set-client-height!)
		 (set-client-width!))])
	     (override*
	      [on-event        ; handle a click
	       (lambda (e)
		 (when (and (not in-counterexample?)
			    (send e button-down?))
		   (when (semaphore-try-wait? *click-sem*)
			 (let-values
			  ([(col row) (x-y->row-column
				       (send e get-x)
				       (send e get-y))])
			  (when (and (>= col 0)
				     (< col board-width)
				     (>= row 0)
				     (< row board-height)
				     (> (send board get-num-concealed) 0))
				(ww-messages *checking-message* "")
				(yield)
				(begin-busy-cursor)
				(send frame expose-row-col
				      row col
				      (not (send e get-shift-down))
				      (send e get-control-down))
				(send frame update-status!)
				(end-checking-ww-messages)
				(end-busy-cursor)
				(when (zero? (send board get-num-concealed))
				      (send frame set-game-over-label!))))
			 (semaphore-post *click-sem*))))]
	      [on-paint
	       (lambda () 
		 (send frame draw-board))])
	     (super-instantiate (frame))
	     
	     ;; tie board and canvas
	     (send board set-canvas! this)
	     
	     ;; Make canvas size always match the board size:
	     (set-client-size!)
	     (set-intercepts!)
	     (stretchable-width #f)
	     (stretchable-height #f)
	     
             ; use teaching board if right dimensions
	     (update-teaching-mode!)
	     
	     (define dc (get-dc))))
    
    (define frame 
      (instantiate ww-frame%
		   ()
		   (board (instantiate board% ()))
		   (label *progname*)
		   (style '(no-resize-border))))
    
    (send frame new-game)
    (send frame show #t))
