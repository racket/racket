(unit/sig BOARD^

  (import [GUI : GUI^]
	  [SOLVE : SOLVE^]
	  [fw : framework^]
	  paint-by-numbers:problem^
	  [all : paint-by-numbers:all-problems^]
	  mzlib:pretty-print^
	  mred^)

  (define default-font (send the-font-list find-or-create-font 10 'roman 'normal 'normal #f))
  (fw:preferences:set-un/marshall 'paint-by-numbers:font
				  (lambda (font)
				    (list (send font get-point-size)
					  (send font get-face)
					  (send font get-family)
					  (send font get-style)
					  (send font get-weight)
					  (send font get-underlined)))
				  (lambda (lst) (apply (ivar the-font-list find-or-create-font) lst)))
  (fw:preferences:set-default 'paint-by-numbers:font default-font (lambda (f) (is-a? f font%)))
				    
  (define problems (car all:problemss))

  (define game-name "Paint by Numbers")
  (define editor-name "Paint by Numbers Designer")
  (define biggest-editor 35)

  (define (setup-progress max)
    (let* ([e (make-eventspace)]
	   [f (parameterize ([current-eventspace e])
		(make-object frame% "Solver Setup Progress"))]
	   [g (make-object gauge% #f max f)]
	   [counter 0])
      (send g min-width 300)
      (send f show #t)
      (lambda ()
	(set! counter (+ 1 counter))
	(cond
	 [(= counter max)
	  (collect-garbage)
	  (send f show #f)]
	 [else 
	  (send g set-value counter)]))))

  (define show-help
    ((require-library "show-help.ss" "games")
     (list "games" "paint-by-numbers")
     "Paint by Numbers Help"))

  (define (configure-font frame)
    (let ([font (get-font-from-user
		 "Choose a font for the labels"
		 frame
		 (fw:preferences:get 'paint-by-numbers:font))])
      (when font
	(fw:preferences:set 'paint-by-numbers:font font))))

  (define (size-font inc)
    (let ([old-font (fw:preferences:get 'paint-by-numbers:font)])
      (fw:preferences:set 'paint-by-numbers:font
			  (if (send old-font get-face)
			      (send the-font-list find-or-create-font
				    (inc (send old-font get-point-size))
				    (send old-font get-face)
				    (send old-font get-family)
				    (send old-font get-style)
				    (send old-font get-weight)
				    (send old-font get-underlined))
			      (send the-font-list find-or-create-font
				    (inc (send old-font get-point-size))
				    (send old-font get-family)
				    (send old-font get-style)
				    (send old-font get-weight)
				    (send old-font get-underlined))))))

  (define (add-font-items frame menu)
    (make-object menu-item%
      "Choose Font"
      menu
      (lambda x (configure-font frame)))
    (make-object menu-item%
      "Make Board Bigger"
      menu
      (lambda x (size-font add1))
      #\b)
    (make-object menu-item%
      "Make Board Tinier"
      menu
      (lambda x (size-font sub1))
      #\t))
		 
  (define generic-frame%
    (class (fw:frame:standard-menus-mixin fw:frame:basic%) (name)

      (inherit set-label get-label get-area-container)
      (private
	[filename #f])
      (public
	[update-filename
	 (lambda (new-name)
	   (set! filename new-name)

	   (let* ([short-name (if new-name
				  (let-values ([(_1 name _2) (split-path new-name)])
				    name)
				  #f)]
		  [new-label (if short-name
				 (format "~a - ~a" short-name name)
				 game-name)])
	     (unless (string=? new-label (get-label))
	       (set-label new-label))))])
      (public
	[get-pbn-filename (lambda () filename)])

      (public
	[do-save void]
	[get-canvas void])

      (private
	[save-as
	 (lambda ()
	   (let ([fn (put-file)])
	     (when fn
	       (update-filename fn)
	       (do-save))))])
      (rename [super-file-menu:between-new-and-open file-menu:between-new-and-open])

      (override
       [file-menu:new-string (lambda () "Puzzle")]
       [file-menu:new
	(lambda (_1 _2)
	  (player))]
       [file-menu:between-new-and-open
	(lambda (menu)
	  (make-object menu-item% "Design a Puzzle..." menu
		       (lambda (_1 _2)
			 (editor #f)))
	  (make-object menu-item% "Design a Puzzle from a Bitmap..." menu
		       (lambda (_1 _2)
			 (editor #t)))
	  (make-object separator-menu-item% menu))]
       [file-menu:save
	(lambda (_1 _2)
	  (if filename
	      (do-save)
	      (save-as)))]
       [file-menu:save-as
	(lambda (_1 _2)
	  (save-as))]
       [file-menu:open
	(lambda (_1 _2)
	  (let ([fn (get-file)])
	    (when fn
	      (let* ([state (call-with-input-file fn read)]
		     [type (car state)])
		(case type
		  [(editor)
		   (let ([name (cadr state)]
			 [problem
			  (make-problem name
					(caddr state)
					(cadddr state)
					(list->vector (map list->vector (car (cddddr state)))))])
		   (editor problem))]
		  [(player)
		   (let ([name (cadr state)]
			 [problem
			  (make-problem name
					(caddr state)
					(cadddr state)
					(car (cddddr state)))])
		     (player problem (cadr (cddddr state))))]
		  [else
		   (message-box "Error"
				(format "Unknown save file ~a" fn))])))))]

       [edit-menu:undo
	(lambda (_1 _2)
	  (send (get-canvas) undo))]

       [edit-menu:redo
	(lambda (_1 _2)
	  (send (get-canvas) redo))])

      (sequence
	(super-init name))
      (public
	[top-panel (make-object horizontal-panel% (get-area-container))]
	[help-button
	 (make-object button% "Help" top-panel (lambda (_1 _2) (show-help)))])
      (sequence
	(send top-panel stretchable-height #f)
	(send top-panel set-alignment 'right 'center))))

  (define pbn-frame%
    (class generic-frame% ([_problem (car problems)])

      (private
	[problem _problem])

      (inherit get-pbn-filename)
      (override
	[do-save
	 (lambda ()
	   (call-with-output-file (get-pbn-filename)
	     (lambda (port)
	       (pretty-print
		(list 'player
		      (problem-name problem)
		      (problem-rows problem)
		      (problem-cols problem)
		      (problem-solution problem)
		      (send canvas get-grid))
		port))
	     'truncate
	     'text))])

      (inherit can-close? show)

      (inherit stretchable-width stretchable-height update-filename)
      (private
	[set-problem
	 (lambda (prlmb)
	   (update-filename #f)
	   (send wrong-item enable (problem-solution prlmb))
	   (send editor-item enable (problem-solution prlmb))
	   (let ([rows (problem-rows prlmb)]
		 [cols (problem-cols prlmb)])
	     (set! problem prlmb)
	     (when canvas (send canvas close-up))
	     (set! canvas (make-object GUI:paint-by-numbers-canvas% canvas-panel rows cols))
	     (send canvas-panel change-children (lambda (l) (list canvas))))
	   (stretchable-width #f)
	   (stretchable-height #f))]

	[show-wrong
	 (lambda ()
	   (let loop ([i (length (problem-cols problem))])
	     (unless (zero? i)
	       (let loop ([j (length (problem-rows problem))])
		 (unless (zero? j)
		   (let* ([m (- i 1)]
			  [n (- j 1)]
			  [board-entry (get-entry m n)]
			  [real-answer (vector-ref (vector-ref (problem-solution problem) m) n)])
		     (unless (or (eq? board-entry real-answer)
				 (eq? board-entry 'unknown)
				 (eq? real-answer 'unknown))
		       (send canvas set-to-error m n)))
		   (loop (- j 1))))
	       (loop (- i 1)))))]
	
	[get-entry
	 (lambda (i j)
	   (send canvas get-rect i j))]

	[set-entry
	 (lambda (i j nv)
	   (send canvas set-rect i j nv)
	   (send canvas paint-rect i j))]

	[really-solve?
	 (lambda ()
	   (fw:gui-utils:get-choice
	    (format "~
  Solving can be a very computationally intense task;~
~nyou may run out of memory and crash. ~
~nReally continue? (Be sure to save your work!)")
	    "Yes"
	    "No"
	    "Really Solve?"
	    #f))]

	[solve
	 (lambda ()
	   (when (really-solve?)
	     (send canvas all-unknown)
	     (send canvas on-paint)
	     (SOLVE:solve
	      (problem-rows problem)
	      (problem-cols problem)
	      set-entry
	      setup-progress)))])

      (sequence
	(super-init game-name))

      (private
	[wrong-item #f]
	[solve-item #f]
	[editor-item #f])
      (inherit get-menu-bar)
      (sequence
	(let* ([mb (get-menu-bar)]
	       [pbn-menu (make-object menu% "Nonogram" mb)])
	  (set! solve-item (make-object menu-item% "Solve" pbn-menu
					(lambda (_1 _2)
					  (solve))
					#\l))
	  (set! wrong-item (make-object menu-item% "Show Mistakes" pbn-menu
					(lambda (_1 _2)
					  (show-wrong)) #\h))
	  (set! editor-item (make-object menu-item% "Edit this Puzzle" pbn-menu
					 (lambda (_1 _2)
					   (editor problem))))

	  (make-object separator-menu-item% pbn-menu)
	  (add-font-items this pbn-menu)))

      (inherit top-panel help-button get-area-container)
      (private
	[gap (make-object horizontal-panel% top-panel)]
	[set-choice
	 (make-object choice%
		      "Set"
		      all:set-names
		      top-panel
		      (lambda (choice evt)
			(set! problems (list-ref all:problemss (send choice get-selection)))
			(send board-choice clear)
			(for-each (lambda (problem) (send board-choice append (problem-name problem)))
				  problems)
			(set-problem (car problems))))]
	[board-choice (make-object choice%
				   "Board"
				   (map problem-name problems)
				   top-panel
				   (lambda (choice evt)
				     (set-problem (list-ref problems (send choice get-selection)))))]
	[canvas/spacer-panel (make-object horizontal-panel% (get-area-container))]
	[canvas-panel (make-object vertical-pane% canvas/spacer-panel)]
	[spacer (make-object grow-box-spacer-pane% canvas/spacer-panel)]
	[canvas #f])
      (sequence
	(send top-panel change-children (lambda (l) (list set-choice board-choice gap help-button))))

      (override
	[get-canvas
	 (lambda ()
	   canvas)])

      (rename [super-on-close on-close])
      (override
       [on-close
	(lambda ()
	  (when canvas (send canvas close-up)))])

      (sequence
	(set-problem problem)
	(show #t))))

  (define editor-frame%
    (class generic-frame% (indicator)
      (inherit get-pbn-filename)
      (override
       [do-save
	(lambda ()
	  (let ([fn (get-pbn-filename)])
	    (call-with-output-file fn
	      (lambda (port)
		(pretty-print
		 (list 'editor
		       (let-values ([(base name dir?) (split-path fn)])
			 name)
		       (send canvas get-row-numbers)
		       (send canvas get-col-numbers)
		       (let ([grid (send canvas get-grid)])
			 (map (lambda (l) (map (lambda (x) (if (eq? x 'on) 'on 'off)) l)) grid)))
		 port))
	      'truncate
	      'text)))])

      (private
	[test-puzzle
	 (lambda ()
	   (player
	    (make-problem "<editor test>"
			  (send canvas get-row-numbers)
			  (send canvas get-col-numbers)
			  (send canvas get-grid))))])

      (private
	[canvas #f])
      (override
	[get-canvas
	 (lambda ()
	   canvas)])


      (rename [super-on-close on-close])
      (override
       [on-close
	(lambda ()
	  (when canvas (send canvas close-up))
	  (super-on-close))])

      (sequence
	(super-init editor-name))

      (inherit get-area-container)
      (private
	[space/canvas-panel (make-object horizontal-panel% (get-area-container))]
	[canvas-panel (make-object vertical-pane% space/canvas-panel)]
	[spacer (make-object grow-box-spacer-pane% canvas-panel)])

      (sequence
	(cond
	 [(pair? indicator)
	  (when canvas
	    (send canvas close-up))
	  (set! canvas
		  (make-object GUI:design-paint-by-numbers-canvas%
		    canvas-panel
		    (car indicator)
		    (cdr indicator)))]
	 [(is-a? indicator bitmap%)
	  (when canvas
	    (send canvas close-up))
	  (set! canvas
		(make-object GUI:design-paint-by-numbers-canvas%
		  canvas-panel
		  (min biggest-editor (send indicator get-width))
		  (min biggest-editor (send indicator get-height))))
	  (when (or (> (send indicator get-width) biggest-editor)
		    (> (send indicator get-height) biggest-editor))
	    (message-box
	     "Paint by Numbers"
	     (format "WARNING: Bitmap is larger than ~ax~a. Truncating."
		     biggest-editor biggest-editor)))
	  (send canvas set-bitmap indicator)]
	 [(problem? indicator)
	  (when canvas
	    (send canvas close-up))
	  (set! canvas
		(make-object GUI:design-paint-by-numbers-canvas%
		  canvas-panel
		  (length (problem-cols indicator))
		  (length (problem-rows indicator))))
	  (send canvas set-grid
		(map vector->list (vector->list (problem-solution indicator))))]))

      (inherit get-menu-bar)
      (sequence
	(let* ([mb (get-menu-bar)]
	       [pbn-menu (make-object menu% "Nonogram" mb)])
	  (make-object menu-item% "Test Puzzle" pbn-menu (lambda (_1 _2) (test-puzzle)))

	  (make-object separator-menu-item% pbn-menu)
	  (add-font-items this pbn-menu)))))

  (define (editor bitmap?)
    (let* ([default 15]
	   [get-sizes
	    (lambda ()
	      (let* ([d (make-object dialog% "Size")]
		     [m (make-object message% "How big should the designer be?" d)]
		     [wp (make-object horizontal-panel% d)]
		     [wm (make-object message% "Width" wp)]
		     [gw (make-object slider% #f 1 biggest-editor wp void default)]
		     [hp (make-object horizontal-panel% d)]
		     [hm (make-object message% "Height" hp)]
		     [gh (make-object slider% #f 1 biggest-editor hp void default)]
		     [bp (make-object horizontal-panel% d)]
		     [cancelled? #f]
		     [cancel (make-object button% "Cancel" bp (lambda (_1 _2)
								(set! cancelled? #t)
								(send d show #f)))]
		     [ok (make-object button% "OK" bp (lambda (_1 _2) (send d show #f)) '(border))])

		(let ([label-width (max (send wm get-width)
					(send hm get-width))])
		  (send wm min-width label-width)
		  (send hm min-width label-width))

		(send bp set-alignment 'right 'center)
      
		(send d show #t)
		(if cancelled?
		    #f
		    (cons (send gw get-value)
			  (send gh get-value)))))]
	   [get-bitmap
	    (lambda ()
	      (let* ([fn (get-file "Select a bitmap")]
		     [bm (make-object bitmap% fn)])
		(if (send bm ok?)
		    bm
		    (begin
		      (message-box
		       "Paint by Numbers"
		       (format (format "Unreadable file: ~a" fn)))
		      #f))))]
	   [indicator
	    (cond
	     [(boolean? bitmap?)
	      (if bitmap?
		  (get-bitmap)
		  (get-sizes))]
	     [(problem? bitmap?)
	      bitmap?])])
		
      (when indicator
	(send (make-object editor-frame% indicator) show #t))))

  (define player
    (case-lambda
     [() (player (car problems))]
     [(problem)
      (let ([f (make-object pbn-frame% problem)])
	(send f show #t))]
     [(problem state)
      (let ([f (make-object pbn-frame% problem)])
	(send (send f get-canvas) set-grid state)
	(send f show #t))]))

  (player)
  ;(editor #f)

  (yield (make-semaphore)))