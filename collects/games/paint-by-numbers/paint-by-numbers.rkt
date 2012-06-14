#lang racket/base

  (require "gui.rkt"
           (prefix-in solve: "solve.rkt")
           "all-problems.rkt"
           "problem.rkt"
           "../show-scribbling.rkt"
           framework
           mzlib/class
           mzlib/unit
           mzlib/pretty
           mzlib/list
           mred)
  
  (provide game@)

  (application-preferences-handler void) 
  ;; reset this after the framework sets it.
  ;; we don't want to open the preferences window, 
  ;; because other games then cannot call 
  ;; preferences:set-default

  
  (define default-font (send the-font-list find-or-create-font 10 'roman 'normal 'normal #f))
  (preferences:set-default 'paint-by-numbers:font default-font (lambda (f) (is-a? f font%)))
  (preferences:set-un/marshall 'paint-by-numbers:font
                               (lambda (font)
                                 (list (send font get-point-size)
                                       (send font get-family)
                                       (send font get-style)
                                       (send font get-weight)
                                       (send font get-underlined)))
                               (lambda (lst)
                                 (let ([size (first lst)]
                                       [family (second lst)]
                                       [style (third lst)]
                                       [weight (fourth lst)]
                                       [underline (fifth lst)])
                                   (cond
                                     [(and (number? size)
                                           (<= 1 size 72)
                                           (memq family '(default decorative roman 
                                                                  script swiss modern symbol system))
                                           (memq style '(normal italic slant))
                                           (memq weight '(normal bold light))
                                           (boolean? underline))
                                      
                                      (or (send the-font-list find-or-create-font size family style weight underline)
                                          default-font)]
                                     [else default-font]))))
  
  (define problems (car problemss))
  
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
  
  (define show-this-help
    (show-scribbling
     '(lib "games/scribblings/games.scrbl")
     "paint-by-numbers"))
  
  (define (configure-font frame)
    (let ([font (get-font-from-user
		 "Choose a font for the labels"
		 frame
		 (preferences:get 'paint-by-numbers:font))])
      (when font
	(preferences:set 'paint-by-numbers:font font))))
  
  (define (size-font inc)
    (let ([old-font (preferences:get 'paint-by-numbers:font)])
      (preferences:set 'paint-by-numbers:font
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
    (class (frame:standard-menus-mixin frame:basic%) 
      (inherit set-label get-label get-area-container)
      (define filename #f)
      (define/override (edit-menu:create-preferences?) #f)
      
      ;; don't open the preferences window, 
      ;; because other games then cannot call 
      ;; preferences:set-default
      (define/override (edit-menu:preferences-callback item control) (void))
      
      [define/public update-filename
        (lambda (new-name)
          (set! filename new-name)
          
          (let* ([short-name (if new-name
                                 (let-values ([(_1 name _2) (split-path new-name)])
                                   name)
                                 #f)]
                 [new-label (if short-name
                                (format "~a - ~a" short-name (get-label))
                                game-name)])
            (unless (string=? new-label (get-label))
              (set-label new-label))))]
      
      [define/public get-pbn-filename (lambda () filename)]
      
      [define/public (do-save) (void)]
      [define/public (get-canvas) (void)]
      
      [define/private save-as
        (lambda ()
          (let ([fn (put-file)])
            (when fn
              (update-filename fn)
              (do-save))))]
      
      [define/override file-menu:new-string (lambda () "New Puzzle")]
      [define/override file-menu:new-callback
	(lambda (_1 _2)
	  (player))]
      [define/override file-menu:between-new-and-open
	(lambda (menu)
	  (make-object menu-item% "Design a Puzzle..." menu
            (lambda (_1 _2)
              (editor #f)))
	  (make-object menu-item% "Design a Puzzle from a Bitmap..." menu
            (lambda (_1 _2)
              (editor #t)))
	  ;(make-object separator-menu-item% menu)
          )]
      [define/override file-menu:save-callback
	(lambda (_1 _2)
	  (if filename
	      (do-save)
	      (save-as)))]
      [define/override file-menu:save-as-callback
	(lambda (_1 _2)
	  (save-as))]
      [define/override file-menu:open-callback
	(lambda (_1 _2)
	  (let ([fn (get-file)])
	    (when fn
	      (let* ([state (call-with-input-file fn read)]
		     [type (car state)])
		(case type
		  [(editor)
		   (let* ([name (cadr state)]
                          [problem
                           (make-problem name
                                         (caddr state)
                                         (cadddr state)
                                         (list->vector (map list->vector (car (cddddr state)))))])
                     (editor problem))]
		  [(player)
		   (let* ([name (cadr state)]
                          [problem
                           (make-problem name
                                         (caddr state)
                                         (cadddr state)
                                         (car (cddddr state)))])
		     (player problem (cadr (cddddr state))))]
		  [else
		   (message-box "Error"
				(format "Unknown save file ~a" fn))])))))]
      
      [define/override edit-menu:undo-callback
	(lambda (_1 _2)
	  (send (get-canvas) undo))]
      
      [define/override edit-menu:redo-callback
	(lambda (_1 _2)
	  (send (get-canvas) redo))]
      
      (super-new (style '(no-resize-border)))
      
      [field [top-panel (make-object horizontal-panel% (get-area-container))]
             [help-button
              (make-object button% "Help" top-panel (lambda (_1 _2) (show-this-help)))]]
      (send top-panel stretchable-height #f)
      (send top-panel set-alignment 'right 'center)))
  
  (define pbn-frame%
    (class generic-frame% 
      (init-field (problem (car problems)))
      
      (inherit get-pbn-filename)
      [define/override do-save
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
            'text))]
      
      (inherit can-close? show)
      
      (inherit stretchable-width stretchable-height update-filename)
      [define/private set-problem
        (lambda (prlmb)
          (update-filename #f)
          (send wrong-item enable (problem-solution prlmb))
          (send editor-item enable (problem-solution prlmb))
          (let ([rows (problem-rows prlmb)]
                [cols (problem-cols prlmb)])
            (set! problem prlmb)
            (when canvas (send canvas close-up))
            (set! canvas (instantiate paint-by-numbers-canvas% ()
                           (parent canvas-panel)
                           (row-numbers rows)
                           (col-numbers cols)))
            (send canvas-panel change-children (lambda (l) (list canvas))))
          (stretchable-width #f)
          (stretchable-height #f))]
      
      [define/private show-wrong
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
      
      [define/private get-entry
        (lambda (i j)
          (send canvas get-rect i j))]
      
      [define/private set-entry
        (lambda (i j nv)
          (send canvas set-rect i j nv)
          (send canvas paint-rect i j))]
      
      [define/private really-solve?
        (lambda ()
          (gui-utils:get-choice
           (format "~
           Solving can be a very computationally intense task;~
           \nyou may run out of memory and crash. ~
           \nReally continue? (Be sure to save your work!)")
           "Yes"
           "No"
           "Really Solve?"
           #f))]
      
      [define/private solve
        (lambda ()
          (when (really-solve?)
            (send canvas all-unknown)
            (send canvas on-paint)
            (solve:solve
             (problem-rows problem)
             (problem-cols problem)
             (lambda (i j nv) (set-entry i j nv))
             setup-progress)))]
      
      (super-new (label game-name))
      
      [define wrong-item #f]
      [define solve-item #f]
      [define editor-item #f]
      (inherit get-menu-bar)
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
        (add-font-items this pbn-menu))
      
      (inherit-field top-panel help-button)
      (inherit get-area-container)
      [define gap (make-object horizontal-panel% top-panel)]
      [define set-choice
        (make-object choice%
          "Set"
          set-names
          top-panel
          (lambda (choice evt)
            (set! problems (list-ref problemss (send choice get-selection)))
            (send board-choice clear)
            (for-each (lambda (problem) (send board-choice append (problem-name problem)))
                      problems)
            (set-problem (car problems))))]
      [define board-choice (make-object choice%
                             "Board"
                             (map problem-name problems)
                             top-panel
                             (lambda (choice evt)
                               (set-problem (list-ref problems (send choice get-selection)))))]
      [define canvas/spacer-panel (make-object horizontal-panel% (get-area-container))]
      [define canvas-panel (make-object vertical-pane% canvas/spacer-panel)]
      [define canvas #f]
      (send top-panel change-children (lambda (l) (list set-choice board-choice gap help-button)))
      
      (define/override get-canvas
        (lambda ()
          canvas))
      
      (define/augment on-close
        (lambda ()
          (inner (void) on-close)
	  (when canvas (send canvas close-up))))
      
      (set-problem problem)
      (show #t)))
  
  (define editor-frame%
    (class generic-frame% 
      (init-field indicator)
      (inherit get-pbn-filename)
      [define/override do-save
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
	      'text)))]
      
      [define/private test-puzzle
        (lambda ()
          (player
           (make-problem "<editor test>"
                         (send canvas get-row-numbers)
                         (send canvas get-col-numbers)
                         (send canvas get-grid))))]
      
      [define canvas #f]
      [define/override get-canvas
        (lambda ()
          canvas)]
      
      
      [define/augment on-close
	(lambda ()
	  (inner (void) on-close)
	  (when canvas (send canvas close-up)))]
      
      (super-instantiate () (label editor-name))
      
      (inherit get-area-container)
      [define space/canvas-panel (make-object horizontal-panel% (get-area-container))]
      [define canvas-panel (make-object vertical-pane% space/canvas-panel)]
      
      (cond
        [(pair? indicator)
         (when canvas
           (send canvas close-up))
         (set! canvas
               (instantiate design-paint-by-numbers-canvas% ()
                 (parent canvas-panel)
                 (width (car indicator))
                 (height (cdr indicator))))]
        [(is-a? indicator bitmap%)
         (when canvas
           (send canvas close-up))
         (set! canvas
               (instantiate design-paint-by-numbers-canvas% ()
                 (parent canvas-panel)
                 (width (min biggest-editor (send indicator get-width)))
                 (height (min biggest-editor (send indicator get-height)))))
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
               (instantiate design-paint-by-numbers-canvas% ()
                 (parent canvas-panel)
                 (width (length (problem-cols indicator)))
                 (height (length (problem-rows indicator)))))
         (send canvas set-grid
               (map vector->list (vector->list (problem-solution indicator))))])
      
      (inherit get-menu-bar)
      (let* ([mb (get-menu-bar)]
             [pbn-menu (make-object menu% "Nonogram" mb)])
        (make-object menu-item% "Test Puzzle" pbn-menu (lambda (_1 _2) (test-puzzle)))
        
        (make-object separator-menu-item% pbn-menu)
        (add-font-items this pbn-menu))))
  
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
      (let ([f (instantiate pbn-frame% () (problem problem))])
	(send f show #t))]
     [(problem state)
      (let ([f (instantiate pbn-frame% () (problem problem))])
	(send (send f get-canvas) set-grid state)
	(send f show #t))]))
  
  (define game@
    (unit
      (import)
      (export)
      (player)
      ;(editor #f)
      ))
