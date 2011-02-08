; FIXME:
; - object rotation axis could be random per type

(module jewel mzscheme

  (require mzlib/unit
	   mzlib/string
           mzlib/class
           mzlib/file
           mred
           sgl/gl
           sgl/gl-vectors
	   (only sgl/sgl get-gl-version-number)
	   "shapes.scm"
	   "array.scm"
	   "text.scm"
	   "../show-scribbling.ss"
  )

  (provide game@)


  (define game@
    (unit
      (import)
      (export)

  ; -----------------------------------------------------------------
  ; global constants 
  ; -----------------------------------------------------------------  
  
  ; defines whether animation is frozen
  (define freeze #f)
  ; animation frame time interval
  (define timer-interval 30)

  ; number of points achieved in the game
  (define jewel-score   #f)
  ; game level
  (define jewel-level   #f)
  ; number of available moves 
  (define jewel-nmoves  #f)
  ; stage in the game
  (define jewel-stage   #f)
  ; how quickly life is decreased
  (define jewel-decay   #f)
  ; life points
  (define jewel-life    #f)
  ; difficulty level
  (define jewel-difficulty 0)

  ; table of high scores, loaded from a file
  (define high-scores (vector
                       '("NOBODY" "0" "1")
                       '("NOBODY" "0" "1")
                       '("NOBODY" "0" "1") 
                       '("NOBODY" "0" "1")
                       '("NOBODY" "0" "1")
                       '("NOBODY" "0" "1")
                       '("NOBODY" "0" "1")
                       '("NOBODY" "0" "1")
                       '("NOBODY" "0" "1")
                       '("NOBODY" "0" "1")
                       ))
  
  (define startlife    1000.0)
  (define lifevisible  (* startlife 2.0))
  (define credit       10.0)
  (define initialdecay 0.4)
  (define decayadd     0.02)
  (define nextlevel    10)
  (define dist         20)
  
  ; Values can be:
  ; PLAYING, GAME-OVER, DIFFICULTY
  (define gamestate    #f)
  ; Values can be:
  ; ACTION-LOOKING, ACTION-WAITING, 
  ; ACTION-REMOVING, ACTION-DROPPING
  ; ACTION-SWAPPING, ACTION-UNSWAPPING
  (define action-mode  #f)
  ; mouse over this element
  (define cposx        0)
  (define cposy        0)
  ; rate of vanishing and rate of falling
  (define vanishrate   0.05)
  (define fallrate     0.05)
  ; swapping is done in how many steps
  (define swaptime     20)
  ; when mouse button is pressed, this stores the position
  (define down-x       #f)
  (define down-y       #f)
  ; tells whether the mouse button is pressed
  (define isdown?      #f)
  ; has something been selected for swapping
  (define tryswap?     #f)
  (define font-scale   0.09)
  ; for keyboard-based game:
  (define bubble-x     #f)
  (define bubble-y     #f)
  (define revert-bubble-x #f)
  (define revert-bubble-y #f)
  (define locked?      #f)

  (define need-help? #t)
  
  (define white        #(1.0 1.0 1.0 1.0))
  (define white2       #(0.7 0.7 0.7 1.0))
  (define grey         #(0.75 0.75 0.75 1.0))
  (define grey2        #(0.75 0.75 0.75 1.0))
  (define bubble-color #(0.8 0.6 1.0 0.4))
  (define bubble-lock-color #(1.0 1.0 1.0 0.4))
  (define spacing      1.76)
  (define shiftx       3.0)
  (define shifty       0.0)
  (define scorex      -8.0)
  (define scorey       5.5)
  (define scorez       8.0)
  (define linespace    2.5)
  (define ex           8)
  (define ey           8)
  (define objectlists  #f)
  (define lightpos     #(-2.0 4.0 4.0 0.0))
  (define light1pos    #(22.0 2.0 4.0 0.0))
  (define light2pos    #(0.0  0.0 4.0 0.0))
  (define diff-color   0)
  (define diff-shape   0)

  (define color-map #( #(0.2 0.2 1.0 1.0) ; blue
                       #(1.0 0.5 0.0 1.0) ; orange
                       #(1.0 1.0 0.0 1.0) ; yellow
                       #(1.0 0.0 1.0 1.0) ; magenta
                       #(0.0 0.8 0.2 1.0) ; green
                       #(0.8 0.1 0.0 1.0) ; red
                       #(1.0 1.0 1.0 1.0) ; white
                     ))
    
  ; 4 x 4 matrices
  (define unproject_matrix
    '#( #(0.266667  0.0  0.0  0.0)
        #(0.0  0.2  0.0  0.0)
        #(0.0  0.0  -26.881655  30.769228)
        #(0.0  0.0  -0.846154   1.0) )
  )
  ; 4 element vector
  (define viewport #f)
  
  
  ; -----------------------------------------------------------------
  ; Defining window toolkit classes
  ; -----------------------------------------------------------------
  
  ; defines a new main window
  (define jewel-frame%
    (class* frame% ()
    
      (define/augment (on-close)
        (jewel-quit-game)
      )
    
      (define/override (on-subwindow-char window event)
        (let*
          ( (c (send event get-key-code))
	    (needed-help? need-help?))
	  (set! need-help? #f)
          (cond
            ; ESCAPE character exits
            ( (eq? c 'escape)
              (if (not (equal? gamestate 'GAME-OVER))
                (begin
                  (set! freeze #f)
                  (send *TIMER* start timer-interval)
                  (set! gamestate 'GAME-OVER)
                )
                (begin
                  (jewel-quit-game)
                  (send *MAIN_WINDOW* show #f)
                )
              )
            )
            ( (eq? c #\space)
              (if (equal? gamestate 'GAME-OVER)
                (begin
                  (difficulty-ask)
                )
		(jewel-key-lock)
              )
            )
	    ( (or (eq? c #\h) (eq? c #\H))
	      (show-jewel-help) )
            ( (or (eq? c #\p) (eq? c #\P))
              (if (equal? gamestate 'PLAYING)
                (begin
                  (set! freeze (not freeze))
                  (if freeze
                    (send *TIMER* stop)
                    (send *TIMER* start timer-interval)
                  )
                )
              )
            )
            ( (and (equal? gamestate 'DIFFICULTY)
                   (member c '(#\0 #\1 #\2 #\3 #\4))
              )
              (case c
                ( (#\0)
                  (jewel-start-game 0) )
                ( (#\1)
                  (jewel-start-game 1) )
                ( (#\2)
                  (jewel-start-game 2) )
                ( (#\3)
                  (jewel-start-game 3) )
                ( (#\4)
                  (jewel-start-game 4) )
              )
            )

	    (else
	     (case c
	       [(up) (jewel-key-move 0 -1)]
	       [(down) (jewel-key-move 0 +1)]
	       [(left) (jewel-key-move -1 0)]
	       [(right) (jewel-key-move +1 0)]
	       [(release) (set! need-help? needed-help?)]
	       [else (set! need-help? #t)]))
          )
        )
      )
    
      (super-instantiate () )
    )
  )
  
  
  ; defines a new OpenGL canvas, handling mouse and rendering, etc  
  (define jewel-canvas%
    (class* canvas% ()
      (inherit with-gl-context swap-gl-buffers)

      (define initialised #f)

      (init-field (expose #f)
                  (realize #f)
                  (configure     #f)
                  (mouse-press   #f)
                  (mouse-motion  #f)
                  (mouse-release #f)
      )
    
      (define/override (on-event e)
        (with-gl-context
          (lambda ()
            (cond
              ; mouse down
              ( (send e button-down? 'right)
                (mouse-press 'right (send e get-x) (send e get-y))
              )
              ( (send e button-down? 'middle)
                (mouse-press 'middle (send e get-x) (send e get-y))
              )
              ( (send e button-down? 'left)
                (mouse-press 'left (send e get-x) (send e get-y))
              )
              ; mouse up
              ( (send e button-up? 'right)
                (mouse-release 'right (send e get-x) (send e get-y))
              )
              ( (send e button-up? 'middle)
                (mouse-release 'middle (send e get-x) (send e get-y))
              )
              ( (send e button-up? 'left)
                (mouse-release 'left (send e get-x) (send e get-y))
              )
              ; mouse motion
              ( (eq? (send e get-event-type) 'motion)
                (mouse-motion (send e get-x) (send e get-y))
              )
            )
          )
        )
      )
	      

      (define/override (on-paint)
        (with-gl-context
          (lambda ()
            (if (and initialised expose)
              (expose)
            )
            (swap-gl-buffers)
          )
        )
      )

      (define/override (on-size width height)
        (with-gl-context
          (lambda ()
            (if (not initialised)
              (begin
                (realize)
                (set! initialised #t)
              )
            )
            (configure width height)
          )
        )
      )

      (let ([cfg (new gl-config%)])
	(send cfg set-multisample-size 4)
	(send cfg set-stencil-size 1)
	(super-new (style '(gl no-autoclear)) (gl-config cfg)))

      (inherit get-dc)
      (unless (send (get-dc) get-gl-context)
	(message-box "Error"
		     (string-append "Jewel requires OpenGL, but there was an error initializing"
				    " the OpenGL context. Maybe OpenGL is not supported by" 
				    " the current display.")
		     #f
		     '(ok stop))
	(exit))

    )
  )

  
  ; -----------------------------------------------------------------
  ; element handling functions
  ; -----------------------------------------------------------------
  (define element-db #f)
  (define move-db #f)

  ; initialise one element
  (define (element-init iy ix)
    (let
      ( (elem (vector-ref (vector-ref element-db iy) ix))
        (move (vector-ref move-db (+ iy 2)))
        (type (random 7))
      )
      (hash-table-put! elem 'type     type )
      (hash-table-put! elem 'angle    (random 360) )
      (hash-table-put! elem 'ax       0.0 )
      (hash-table-put! elem 'ay       1.0 )
      (hash-table-put! elem 'az       0.0 )
      (hash-table-put! elem 'fall     0.0 )
      (hash-table-put! elem 'speed    0.0 )
      (hash-table-put! elem 'vanish   1.0 )
      (hash-table-put! elem 'dx       0.0 )
      (hash-table-put! elem 'dy       0.0 )
      (hash-table-put! elem 'swapping 0 )

      (cond
        ; one color per type
        ; one shape for all type
        ( (= jewel-difficulty 1)
          (hash-table-put! elem 'color type)
          (hash-table-put! elem 'shape diff-shape)
        )
        ; one color for all type
        ; one shape per type
        ( (= jewel-difficulty 2)
          (hash-table-put! elem 'color diff-color)
          (hash-table-put! elem 'shape type)
        )
        ; one color per type
        ; random shape
        ( (= jewel-difficulty 3)
          (hash-table-put! elem 'color type)
          (hash-table-put! elem 'shape (random 7))
        )
        ; random color
        ; one shape per type
        ( (= jewel-difficulty 4)
          (hash-table-put! elem 'color (random 7))
          (hash-table-put! elem 'shape type)
        )
        ; default
        ; one color per type
        ; one shape per type
        ( else
          (hash-table-put! elem 'color type)
          (hash-table-put! elem 'shape type)
        )
      )
      
      ; set the element type in the move database
      (vector-set! move (+ ix 2) type)
    )
  )


  ; initialise the element database, N x N matrix
  (define (element-init-db)
    ; initialise the move database
    (set! move-db (make-vector (+ ey 4) #f))
    (do ((iy 0 (+ iy 1))) ((= iy (+ ey 4)))
      (vector-set! move-db iy (make-vector (+ ex 4) -1))
    )
  
    (set! element-db (make-vector ey #f))
    (do ((iy 0 (+ iy 1))) ((= iy ey))
      (let*
        ( (row (make-vector ex #f)) )
        (vector-set! element-db iy row)
        (do ((ix 0 (+ ix 1))) ((= ix ex))
          (let* 
            ( (elem (make-hash-table 'equal)) )
            (vector-set! row ix elem)
            (element-init iy ix)
          )
        )
      ); end of let
    )
  )
  

  (define (element-get iy ix prop)
    (hash-table-get (vector-ref (vector-ref element-db iy) ix)
                    prop (lambda () #f))
  )


  (define (element-set! iy ix prop value)
    (let*
      ( (elem (vector-ref (vector-ref element-db iy) ix)) )
      (hash-table-put! elem prop value)
    )
  )


  (define (element-swap! iy ix jy jx)
    (let*
      ( (ri (vector-ref element-db iy))
        (rj (vector-ref element-db jy))
        (tt (vector-ref ri ix))
        ; move array
        (mi (vector-ref move-db (+ iy 2)))
        (mj (vector-ref move-db (+ jy 2)))
        (mt (vector-ref mi (+ ix 2)))
      )
      (vector-set! ri ix (vector-ref rj jx))
      (vector-set! rj jx tt)
      ; move array 
      (vector-set! mi (+ ix 2) (vector-ref mj (+ jx 2)))
      (vector-set! mj (+ jx 2) mt)
    )
  )


  ; copy from i to j
  (define (element-copy! iy ix jy jx)
    (let*
      ( (elem1 (vector-ref (vector-ref element-db iy) ix))
        (elem2 (vector-ref (vector-ref element-db jy) jx))
      )
      (hash-table-for-each 
        elem1
        (lambda (key val) (hash-table-put! elem2 key val))
      )
      ; move array
      (array-set! move-db (+ jy 2) (+ jx 2)
                  (array-ref move-db (+ iy 2) (+ ix 2))
      )
    )
  )

  ; -----------------------------------------------------------------
  ; score number handling functions
  ; -----------------------------------------------------------------

  (define score-numbers (make-hash-table 'equal))
  (define score-key     0)
  (define score-fade    0.01)

  (define (score-add x y z fade value)
    (let*
      ( (elem (make-hash-table 'equal)) )
      (hash-table-put! elem 'x x)
      (hash-table-put! elem 'y y)
      (hash-table-put! elem 'z z)
      (hash-table-put! elem 'fade fade)
      (hash-table-put! elem 'value value)
    
      (hash-table-put! score-numbers score-key elem)
      (set! score-key (+ score-key 1))
    )
  )
  
  (define (score-set! elem prop val)
    (hash-table-put! elem prop val)
  )

  (define (score-del! score-key)
    (hash-table-remove! score-numbers score-key)
  )

  (define (score-get elem prop)
    (hash-table-get elem prop)
  )

  (define (score-for-each proc table)
    (hash-table-for-each
      table
      (lambda (key val) (proc key val))
    )
  )

  ; -----------------------------------------------------------------
  ; High score reading, writing and rendering
  ; -----------------------------------------------------------------

  ; split a string line at the ch character(s) into tokens
  ; for example:
  ; "hello ladies and gentleman" -> ("hello" "ladies" "and" "gentleman")
  (define (text-split str ch empty)
    (let* 
      ((idx (string-length str))
       (last #f)
       (slist '())
      )
      (do () ( (not (>= idx 0)) )
        (set! last idx)
        (do () ( (not (and (> idx 0) 
                           (not (char=? (string-ref str (- idx 1)) ch))
                      )
               ) )
          (set! idx (- idx 1))
        )
        (if (>= idx 0)
          (begin
            (if (or empty
                    (and (not empty) (> (- last idx) 0)) )
              (set! slist (cons (substring str idx last) slist))
            )
            (set! idx (- idx 1))
          )
        )
      )
      slist
    )
  )


  (define (high-score-read)
    (let ([l (get-preference 'plt:jewel:scores (lambda () null))])
      (let loop ([l l][i 0])
	(unless (or (not (pair? l))
		    (not (list? (car l)))
		    (not (= (length (car l)) 3))
		    (= i 10))
	  (vector-set! high-scores i (map clean-string (car l)))
	  (loop (cdr l) (+ i 1))))))


  (define (high-score-write)
    (put-preferences '(plt:jewel:scores) (list (vector->list high-scores))))
  
  (define (get-user)
    (let ([s (get-text-from-user "High Score"
				 "High Scorer's Name:"
				 *MAIN_WINDOW*
				 (or (getenv "USER")
				     (getenv "USERNAME")))])
      (if s
	  (clean-string s)
	  "UKNOWN")))

  (define (clean-string s)
    (regexp-replace* #rx"[^-A-Z0-9+]" 
		     (let ([s (string-upcase s)])
		       (substring s 0 (min (string-length s) 10)))
		     " "))

  (define (high-score-set)
    (let*
      ( (score #f)
        (exit? #f)
      )

      (do ((i 0 (+ i 1))) ((or exit? (= i (vector-length high-scores))))
        (set! score (vector-ref high-scores i))
        (if (> jewel-score (string->number (list-ref score 1)))
          (begin
            (do ((j (- (vector-length high-scores) 1) (- j 1)))
                ((= j i))
              (vector-set! high-scores j (vector-ref high-scores (- j 1)))
            )
            (vector-set! high-scores i
                         (list (get-user)
                               (number->string jewel-score)
                               (number->string jewel-level)))
            (set! exit? #t)
          )
        )
      )
    )
  )
  
  
  (define (high-score-render)
    (let*
      ( (highxname  2.5)
        (highxscore 6.0)
        (highxlevel 5.5)
        (score #f)
        (dimmer #(0.0 0.0 0.0 0.5))
        (boxleft  -3.5)
        (boxright  8.2)
        (boxtop    5.7)
        (boxbottom (- boxtop))
        (boxz      8.0)
      )
      (glPushMatrix)
      (glTranslatef -3.0 5.5 8.1)
      (glScalef 0.6 0.6 0.6)
    
      (glPushMatrix)
      (glTranslatef highxname 0.0 0.0)
      (string-draw "NAME")
      (glTranslatef highxscore 0.0 0.0)
      (string-draw "SCORE")
      (glTranslatef highxlevel 0.0 0.0)
      (string-draw "LEVEL")
      (glPopMatrix)
    
      (do ((i 0 (+ i 1))) ((= i (vector-length high-scores)))
        (glTranslatef 0.0 -1.8 0.0)
        (glPushMatrix)
        (set! score (vector-ref high-scores i))
        (string-draw (number->string (+ i 1)) )
        (glTranslatef highxname 0.0 0.0)
        (if (< (string-length (list-ref score 0)) 6)
          (string-draw (list-ref score 0) )
          (string-draw (substring (list-ref score 0) 0 6) )
        )
        (glTranslatef (+ highxscore 1.0) 0.0 0.0)
        (string-draw (list-ref score 1) )
        (glTranslatef highxlevel 0.0 0.0)
        (string-draw (list-ref score 2) )
        (glPopMatrix)
      )
      (glPopMatrix)

      ; draw a dim square over the jewels
      (glEnable GL_BLEND)
      (when (>= (get-gl-version-number) 13)
	(glEnable GL_MULTISAMPLE))
      (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
      (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE 
                    (vector->gl-float-vector dimmer))
      (glDisable GL_LIGHT0)
      (glDisable GL_LIGHT1)
      (glBegin GL_QUADS)
      (glVertex3f boxleft boxtop boxz)
      (glVertex3f boxleft boxbottom boxz)
      (glVertex3f boxright boxbottom boxz)
      (glVertex3f boxright boxtop boxz)
      (glEnd)
      (glEnable GL_LIGHT0)
      (glEnable GL_LIGHT1)
      (glBlendFunc GL_ONE GL_ONE)
      (glDisable GL_BLEND)
    )
  )
  
  
  ; -----------------------------------------------------------------
  ; Difficulty level
  ; -----------------------------------------------------------------
  
  (define (difficulty-ask)
    (set! gamestate 'DIFFICULTY)
  )
  
  (define (difficulty-render)
    (let*
      ( (highxname  2.5)
        (highxscore 6.0)
        (highxlevel 5.5)
        (dimmer     #(0.0 0.0 0.0 0.5))
        (boxleft   -3.5)
        (boxright   8.2)
        (boxtop     5.7)
        (boxbottom  (- boxtop))
        (boxz       8.0)
        (levels     #("BEGINNER"
                      "MATCH EVERYTHING"
                      "EASY"
                      "MATCH COLORS"
                      "MEDIUM"
                      "MATCH SHAPES"
                      "HIGH"
                      "MATCH COLORS"
                      "EXTREME"
                      "MATCH SHAPES"
                     ))
      )
      (glPushMatrix)
      (glTranslatef -3.0 5.5 8.1)
      (glScalef 0.6 0.6 0.6)
    
      (glPushMatrix)
      (glTranslatef highxname 0.0 0.0)
      (string-draw "SELECT DIFFICULTY")
      (glPopMatrix)
    
      (do ((i 0 (+ i 1))) ((= i (vector-length levels)))
        (glTranslatef 0.0 -1.8 0.0)
        (glPushMatrix)
        (if (= (remainder i 2) 0)
          (string-draw (number->string (/ i 2)) )
        )
        (glTranslatef highxname 0.0 0.0)
        (string-draw (vector-ref levels i) )
        (glPopMatrix)
      )
      (glPopMatrix)

      ; draw a dim square over the jewels
      (glEnable GL_BLEND)
      (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
      (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE 
                    (vector->gl-float-vector dimmer))
      (glDisable GL_LIGHT0)
      (glDisable GL_LIGHT1)
      (glBegin GL_QUADS)
      (glVertex3f boxleft boxtop boxz)
      (glVertex3f boxleft boxbottom boxz)
      (glVertex3f boxright boxbottom boxz)
      (glVertex3f boxright boxtop boxz)
      (glEnd)
      (glEnable GL_LIGHT0)
      (glEnable GL_LIGHT1)
      (glBlendFunc GL_ONE GL_ONE)
      (glDisable GL_BLEND)
    )
  )
  
  
  ; -----------------------------------------------------------------
  ; Initialisation
  ; -----------------------------------------------------------------

  (define (jewel-quit-game)
    (send *TIMER* stop)
    (high-score-write)
;    (display "\nTHE END\n")
  )
  
  
  (define (jewel-init-game)
    (element-init-db)
    
    (set! jewel-stage   0)
    (set! jewel-score   0)
    (set! jewel-level   0)
    (set! jewel-nmoves  0)
    (set! score-numbers (make-hash-table 'equal))

    (set! gamestate 'GAME-OVER)
    ;read high scores
    (high-score-read)
  )
  
  
  (define (jewel-start-game diff)
    (set! jewel-difficulty diff)
    (set! jewel-life    startlife)
    (set! jewel-decay   initialdecay)
    (set! jewel-stage   0)
    (set! jewel-score   0)
    (set! jewel-level   0)
    (set! jewel-nmoves  0)

    ; make the current configuration to vanish
    (do ((iy 0 (+ iy 1))) ((= iy ey))
      (do ((ix 0 (+ ix 1))) ((= ix ex))
        (element-set! iy ix 'vanish 0.999)
      )
    )

    (set! diff-color  (random 7))
    (set! diff-shape  (random 7))
    (set! gamestate   'PLAYING)
    (set! action-mode 'ACTION-REMOVING)
  )
  
  
  (define (jewel-realize)
    (let*
      ( ( scale 0.88) )

      (glEnable GL_CULL_FACE)
      (glEnable GL_LIGHTING)
      (glEnable GL_LIGHT0)
      (glEnable GL_LIGHT1)

      (glLightfv GL_LIGHT0 GL_SPECULAR (vector->gl-float-vector white))
      (glLightfv GL_LIGHT0 GL_DIFFUSE  (vector->gl-float-vector grey))

      (glLightfv GL_LIGHT1 GL_SPECULAR (vector->gl-float-vector white2))
      (glLightfv GL_LIGHT1 GL_DIFFUSE  (vector->gl-float-vector grey2))

      (glLightfv GL_LIGHT2 GL_SPECULAR (vector->gl-float-vector white))
      (glLightfv GL_LIGHT2 GL_DIFFUSE  (vector->gl-float-vector grey))

      (glEnable GL_DEPTH_TEST)

      (glShadeModel GL_SMOOTH)
      (glClearColor 0.0 0.0 0.0 1.0)
      (glClear GL_COLOR_BUFFER_BIT)
      (glClear GL_DEPTH_BUFFER_BIT) 

      (glDisable GL_BLEND)
      (glBlendFunc GL_ONE GL_ONE)

      (glLineWidth 2.0)
      (glDisable GL_LINE_SMOOTH)

      ; initialise objects
      (set! objectlists (glGenLists 8))
      (glNewList (+ objectlists 0) GL_COMPILE)
      (makebucky (* scale 0.9))
      (glEndList)

      (glNewList (+ objectlists 1) GL_COMPILE)
      (makebevelcube scale)
      (glEndList)

      (glNewList (+ objectlists 2) GL_COMPILE)
      (makepyramid (* scale 0.7))
      (glEndList)

      (glNewList (+ objectlists 3) GL_COMPILE)
      (makeicosahedron (* scale 0.9))
;      (makespiky (* scale 0.9))
      (glEndList)
  
      (glNewList (+ objectlists 4) GL_COMPILE)
      (makecylinder (* scale 0.9))
      (glEndList)

      (glNewList (+ objectlists 5) GL_COMPILE)
      (makediamond (* scale 0.9))
      (glEndList)

      (glNewList (+ objectlists 6) GL_COMPILE)
      (makeuvsphere (* scale 0.9))
      (glEndList)
    
      (glNewList (+ objectlists 7) GL_COMPILE)
      (makedisc (* scale 1.2))
      (glEndList)
    
      ; initialise fonts
      (string-init font-scale)
    )
  )


  (define (jewel-configure width height)
    (glViewport 0 0 width height)
    (set! viewport (make-vector 4 0))
    (vector-set! viewport 2 width)
    (vector-set! viewport 3 height)
  
    ; projection matrix
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (if (< width height)
      (let ( (h (/ height width)) )
        (glFrustum -1.0 1.0 (- h) h 5.0 60.0)
      )
      (let ( (h (/ width height)) )
        (glFrustum (- h) h -1.0 1.0 5.0 60.0)
      )
    )
    ; modelview matrix
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity)
    (glTranslatef 0.0 0.0 -40.0)
  )

  ; -----------------------------------------------------------------
  ; Handling animation and game control
  ; -----------------------------------------------------------------

  ; determine which elements to replace
  (define (replace)
    (let*
      ( (falls (make-vector ex 1)) )
    
      (do ((iy (- ey 1) (- iy 1))) ((< iy 0))
        (do ((ix 0 (+ ix 1))) ((= ix ex))
          
          (if (= (element-get iy ix 'vanish) 0.0)
            (let ( (finished -1) )
              (do ((k (- iy 1) (- k 1))) ((or (< k 0) (> finished -1)))
                (if (not (= (element-get k ix 'vanish) 0.0))
                  (set! finished k)
                )
              )
              (if (>= finished 0)
                (begin
                  (element-copy! finished ix iy ix)
                  (element-set! finished ix 'vanish 0.0)
                  (element-set! iy ix 'fall (- iy finished))
                )
                (begin
                  ; initializes new elements
                  (element-init iy ix)
                  (element-set! iy ix 'fall (+ iy (vector-ref falls ix)))
                  (vector-set! falls ix (+ 1 (vector-ref falls ix)))
                )
              )
            )
          ) ; end of if
        
        ) ; end of do
      ) ; end of do
    ) ; end of let
  )

  
  (define (addlife chain len x y)
    (let*
      ( (value (+ chain len))
        (sx (+ (* (+ (- x (/ ex 2.0)) 0.5) spacing) shiftx))
        (sy (+ (* (- (/ ey 2.0) y) spacing) shifty))
        (sz 0.0)
      )
      (score-add sx sy sz 1.0 value)
      (set! jewel-score (+ jewel-score value))
      (set! jewel-stage (+ jewel-stage len))
      (set! jewel-life  (+ jewel-life  (* value credit)))
      (if (>= jewel-stage nextlevel)
        (begin
          (set! jewel-stage (- jewel-stage nextlevel))
          (set! jewel-level (+ jewel-level 1))
          (set! jewel-decay (+ jewel-decay decayadd))
        )
      )
    )
  )


  (define (declife)
    (unless (eq? gamestate 'GAME-OVER)
      (set! jewel-life (- jewel-life jewel-decay))
      (if (< jewel-life 0.0)
          (let* ( (score #f) (exit? #f) )
            ; set life points to zero
            (set! jewel-life 0.0)
            ; set high score if any
            (high-score-set)
            ; end of game
            (set! gamestate 'GAME-OVER)
            )
          )
      )
  )
  
  ;check for minimum three adjacent elements
  (define (findwins checking)
    (let*
      ( (hadsome #f) )
      ; check the rows for three identical elements
      (do ((iy 0 (+ iy 1))) ((= iy ey))
        (let*
          ( (identical 1) )
          (do ((ix 1 (+ ix 1))) ((= ix (+ ex 1)))
            ; if in range horizontally and
            ; type of the current and the previous element is equal
            (if (and (< ix ex)
                     (= (element-get iy ix 'type)
                        (element-get iy (- ix 1) 'type)))
              (set! identical (+ identical 1))
              ; else three or more identical has been found in a row
              (if (>= identical 3)
                (begin
                  (set! hadsome #t)
                  (if (not checking)
                    (let*
                      ( (x (- ix 1 (/ identical 2.0)))
                        (y (+ iy 0.5))
                      )
                      (addlife 0 (- identical 1) x y)
                    )
                  )
                  ; set the found elements to vanish
                  (do ((k identical (- k 1))) ((= k 0))
                    (element-set! iy (- ix k) 'vanish 0.999)
                  )
                  (set! identical 1)
                )
                (set! identical 1)
              )
            )
          )
        )
      ) ; end of checking rows
       
      ; checking columns for three identical elements
      (do ((ix 0 (+ ix 1))) ((= ix ex))
        (let*
          ( (identical 1) )
          (do ((iy 1 (+ iy 1))) ((= iy (+ ey 1)))
            ; if in range vertically and
            ; type of the current and the previous element is equal
            (if (and (< iy ey)
                     (= (element-get iy ix 'type)
                        (element-get (- iy 1) ix 'type)))
              (set! identical (+ identical 1))
              ; else three or more identical has been found in a row
              (if (>= identical 3)
                (begin
                  (set! hadsome #t)
                  (if (not checking)
                    (let*
                      ( (x ix)
                        (y (- iy 0.5 (/ identical 2.0)))
                      )
                      (addlife 0 (- identical 1) x y)
                    )
                  )
                  ; set the found elements to vanish
                  (do ((k identical (- k 1))) ((= k 0))
                    (element-set! (- iy k) ix 'vanish 0.999)
                  )
                  (set! identical 1)
                )
                (set! identical 1)
              )
            )
          )
        )
      ) ; end of checking columns

      hadsome
    )
  )

  
  ; possible moves
  (define chkpos
    '#( #(1 -1 0 1)
        #(-1 -1 -1 1)
        #(0 -1 1 1)
        #(0 -2 0 1)

        #(1 1 -1 0)
        #(1 -1 -1 -1)
        #(1 0 -1 1)
        #(2 0 -1 0)
      
        #(-1 1 0 -1)
        #(1 1 1 -1)
        #(0 1 -1 -1)
        #(0 2 0 -1)
      
        #(-1 -1 1 0)
        #(-1 1 1 1)
        #(-1 0 1 -1)
        #(-2 0 1 0)
      )
  )

  ; check whether any move is possible in the game field
  (define (anymove?)
    (let*
      ( (moves 0)
        (type  #f)
      )
    
      ; check for all combination
      (do ((iy 0 (+ iy 1))) ((= iy ey))
        (do ((ix 0 (+ ix 1))) ((= ix ex))
          (if (not (= (element-get iy ix 'type)
                      (array-ref move-db (+ iy 2) (+ ix 2))))
            (begin
              (display "wrong iy: ")(display iy)
              (display " ix: ")(display ix)(newline)
            )
          )
          ; all 16, possible combinations
          (do ((k 0 (+ k 1))) ((= k 16))
            (set! type (array-ref move-db (+ iy 2) (+ ix 2)))
            (if (and (= type (array-ref move-db 
                                        (+ iy 2 (array-ref chkpos k 1))
                                        (+ ix 2 (array-ref chkpos k 0))))
                     (= type (array-ref move-db 
                                        (+ iy 2 (array-ref chkpos k 3))
                                        (+ ix 2 (array-ref chkpos k 2))))
                )
              (begin
                #|
                (display "move ")(display type)(newline)
                (display iy)(display " - ")(display ix)(newline)
                (display (+ iy (array-ref chkpos k 1)))(display " - ")
                  (display (+ ix (array-ref chkpos k 0)))(newline)
                (display (+ iy (array-ref chkpos k 3)))(display " - ")
                  (display (+ ix (array-ref chkpos k 2)))(newline)
                |#
                (set! moves (+ moves 1))
              )
            )
          )
        )
      )

      (set! jewel-nmoves moves)
    
      moves
    )
  )

  
  ; function that is called by the timer
  ; handles the switching between states
  (define (jewel-control-game)
  
    ; continuous rotation of elements
    (do ((iy 0 (+ iy 1))) ((= iy ey))
      (do ((ix 0 (+ ix 1))) ((= ix ex))
        (element-set! iy ix 'angle (+ 3.0 (element-get iy ix 'angle)))
      )
    )
    
     ; fading of score numbers
    (score-for-each 
      (lambda (key elem)
        (let*
          ( (fade (- (score-get elem 'fade) score-fade)) )
          (if (< fade 0.0)
            (score-del! key)
            (score-set! elem 'fade fade)
          )
        )
      )
      score-numbers
    )
    
    (case action-mode
      ( (ACTION-LOOKING)
        (if (equal? gamestate 'PLAYING)
          (if (findwins #f)
            (set! action-mode 'ACTION-REMOVING)
            ; check if any move is possible at all ???
            (begin
              (if (= (anymove?) 0)
                ; set all elements to vanish
                (begin
                  (do ((iy 0 (+ iy 1))) ((= iy ey))
                    (do ((ix 0 (+ ix 1))) ((= ix ex))
                      (element-set! iy ix 'vanish 0.999)
                    )
                  )
                  (set! action-mode 'ACTION-REMOVING)
                )
                ; switch to ACTION-WAITING
                (set! action-mode 'ACTION-WAITING)
              )
            )
          )
        )
      )
      ( (ACTION-WAITING)
        (if (equal? gamestate 'PLAYING)
          (begin
            (declife)
            (if tryswap?
              (set! action-mode 'ACTION-SWAPPING)
            )
          )
        )
      )
      ( (ACTION-SWAPPING ACTION-UNSWAPPING)
        (if (equal? action-mode 'ACTION-UNSWAPPING)
          (declife)
        )
        (set! tryswap? #f)
        (let*
          ( (hadsome 0) (swap #f)
            (ax #f) (ay #f) (bx #f) (by #f)
          )
          (do ((iy 0 (+ iy 1))) ((= iy ey))
            (do ((ix 0 (+ ix 1))) ((= ix ex))
              (set! swap (element-get iy ix 'swapping))
              (if (not (= swap 0))
                (begin
                  (set! hadsome 1)
                  (set! swap (+ swap 1))
                  (if (= swap swaptime)
                    (begin
                      (element-set! iy ix 'swapping 0)
                      (set! hadsome 2)
                      ; for the first time it has no meaning
                      (set! ax bx)
                      (set! ay by)
                      ; it always stores the result in bx by
                      (set! bx ix)
                      (set! by iy)
                    )
                    (element-set! iy ix 'swapping swap)
                  )
                )
              )
            )
          )
        
          (if (= hadsome 2)
            (cond
              ( (findwins #f) 
		(set! locked? #f)
		(set! action-mode 'ACTION-REMOVING) )
              ( (equal? action-mode 'ACTION-SWAPPING)
                ; swap back
                (element-swap! ay ax by bx)
                ; set swapping
                (element-set! ay ax 'swapping 1)
                (element-set! ay ax 'dx (- (element-get ay ax 'dx)))
                (element-set! ay ax 'dy (- (element-get ay ax 'dy)))
                (element-set! by bx 'swapping 1)
                (element-set! by bx 'dx (- (element-get by bx 'dx)))
                (element-set! by bx 'dy (- (element-get by bx 'dy)))
                ; unswapping state
		(when revert-bubble-x
		  (set! bubble-x revert-bubble-x)
		  (set! bubble-y revert-bubble-y))
                (set! action-mode 'ACTION-UNSWAPPING)
              )
              ( else 
		(set! locked? #f)
		(set! action-mode 'ACTION-WAITING) )
            )
          )
        )
      )
      ; remove elements from the scene
      ; after removal switch to dropping
      ( (ACTION-REMOVING)
        (let*
          ( (hadsome 0)
            (vanish #f)
          )
          (do ((iy 0 (+ iy 1))) ((= iy ey))
            (do ((ix 0 (+ ix 1))) ((= ix ex))
              (set! vanish (element-get iy ix 'vanish))
              (if (< vanish 1.0)
                (begin
                  (set! vanish (- vanish vanishrate))
                  (if (< vanish 0.0)
                    (begin
                      (element-set! iy ix 'vanish 0.0)
                      (set! hadsome (+ hadsome 1))
                    )
                    (element-set! iy ix 'vanish vanish)
                  )
                )
              )
            )
          )
          (if (> hadsome 0)
            (begin
              (replace)
              (set! action-mode 'ACTION-DROPPING)
            )
          )
        )
      ) ; end of ACTION-REMOVING
      ; drop in new elements to the scene
      ; after dropping switch to looking
      ( (ACTION-DROPPING)
        (let*
          ( (hadsome 0)
            (fall #f)
            (speed #f)
          )
          (do ((iy 0 (+ iy 1))) ((= iy ey))
            (do ((ix 0 (+ ix 1))) ((= ix ex))
              (set! fall (element-get iy ix 'fall))
              (if (> fall 0.0)
                (begin
                  (set! hadsome (+ hadsome 1))
                  (set! fall (- fall (element-get iy ix 'speed)))
                  (set! speed (element-get iy ix 'speed))
                  (element-set! iy ix 'speed (+ speed fallrate))
                  (if (<= fall 0.0)
                    (begin
                      (element-set! iy ix 'fall 0.0)
                      (element-set! iy ix 'speed 0.0)
                    )
                    (element-set! iy ix 'fall fall)
                  )
                )
              )
            )
          )
          (if (= hadsome 0)
            (set! action-mode 'ACTION-LOOKING)
          )
        ); end of let
      ) ; end of ACTION-DROPPING
    )
    
    ; generate an expose event, redraw the opengl window
    (queue-callback 
      (lambda x (send *OPENGL_WINDOW* on-paint))
      #f
    )
  )


  ; -----------------------------------------------------------------
  ; Rendering functions
  ; -----------------------------------------------------------------

  (define (setmaterial color-vect)
    (glMaterialfv GL_FRONT_AND_BACK GL_AMBIENT_AND_DIFFUSE
                  (vector->gl-float-vector color-vect))
    (glMaterialfv GL_FRONT_AND_BACK GL_SPECULAR
                  (vector->gl-float-vector white))
    (glMaterialfv GL_FRONT_AND_BACK GL_SHININESS 
                  (vector->gl-float-vector #(25.0)))
  )


  (define (show-life)
    (let*
      ( (sections 24)
        (section-yellow 4)
        (section-red    1)
        (b (/ (* 3.1415927 2.0) 24))
        (a 0)
        (s #f)
      )
      (glPushMatrix)
      (glTranslatef -6.5 -3.0 5.0)
      (glRotatef 11.0 0.0 1.0 0.0)
      
      ; circle outline
      (setmaterial (vector-ref color-map 0))
      (glNormal3f 0.0 0.0 1.0)
      (glBegin GL_LINE_LOOP)
      (do ((i 0 (+ i 1))) ((= i sections))
        (glVertex3f (* (sin a) 2.0)
                    (* (cos a) 2.0)
                    0.0)
        (set! a (+ a b))
      )
      (glEnd)
      
      ; show triangle fan
      (glBegin GL_TRIANGLE_FAN)
      (glEnable GL_NORMALIZE)
      (glVertex3f 0.0 0.0 3.0)
      (if (< jewel-life lifevisible)
        (let*
          ( (x #f) (y #f) )
          (set! a (/ (* 3.1415927 2.0 jewel-life) lifevisible))
          (set! x (* (sin a) 2.0))
          (set! y (* (cos a) 2.0))
          (glNormal3f x y 0.7)
          (glVertex3f x y 0.0)
          (set! s (floor (/ (* jewel-life sections) lifevisible)))
        )
        (set! s sections)
      )
      ; color of the section
      (cond 
        ( (> s section-yellow)
          ; green
          (setmaterial (vector-ref color-map 4))
        )
        ( (> s section-red)
          ; yellow
          (setmaterial (vector-ref color-map 2))
        )
        ( else
          ; red
          (setmaterial (vector-ref color-map 5))
        )
      )
      (do ((i s (- i 1))) ((< i 0))
        (cond
          ( (= i section-yellow) (setmaterial (vector-ref color-map 2)))
          ( (= i section-red)    (setmaterial (vector-ref color-map 5)))
        )
        (set! a (* (- i 0.5) b))
        (glNormal3f (sin a) (cos a) 0.7)
        (set! a (* i b))
        (glVertex3f (* (sin a) 2.0)
                    (* (cos a) 2.0)
                    0.0)
      )
    
      (glEnd)
      
      (glPopMatrix)
    )
  )
  
  ; main OpenGL rendering, called by expose event
  (define (jewel-redraw)
    (glClear GL_COLOR_BUFFER_BIT)
    (glClear GL_DEPTH_BUFFER_BIT) 
    
    (glLightfv GL_LIGHT0 GL_POSITION (vector->gl-float-vector lightpos))
    (glLightfv GL_LIGHT1 GL_POSITION (vector->gl-float-vector light1pos))
    (glLightfv GL_LIGHT2 GL_POSITION (vector->gl-float-vector light2pos))
    
    (if (equal? gamestate 'PLAYING)
      (show-life)
    )
    
    (glPushMatrix)
    
    (let*
      ( (t spacing)
        (x #f)
        (y (* t (- (/ ey 2.0) 0.5)))
        (xt 0.0) (yt 0.0) (zt 0.0)
        (k  (* ex ey))
        (nx #f) (ny #f) (nz #f)
        (obj #f)
        (ang #f)
        (s #f)
        (counter 0)
		   
      )

      ;; This shouldn't do anything, but it fixes drawing in 
      ;; Snow Leopard. Bug in the game or in Snow Leopard?
      (glEnable GL_LIGHT2)
      (glDisable GL_LIGHT2)
              
      (glEnable  GL_BLEND)
      (do ((iy 0 (+ iy 1))) ((= iy ey))
        (set! x (* (- t) (- (/ ex 2.0) 0.5)))
        (do ((ix 0 (+ ix 1))) ((= ix ex))
          (set! nx (+ x shiftx))
          (set! ny y)
          (set! nz (* (- 1.0 (element-get iy ix 'vanish)) 50.0))
          (if (not (= (element-get iy ix 'swapping) 0))
            (begin
              (set! ang (/ (* (element-get iy ix 'swapping) 3.1415927)
                           2.0
                           swaptime))
              (set! s (* t (cos ang)))
              (set! nx (+ nx (* s (element-get iy ix 'dx))))
              (set! ny (+ ny (* s (element-get iy ix 'dy))))
              (set! s (* t (sin (* ang 2.0))))
              (if (= (remainder counter 2) 1)
                (set! s (- s))
              )
              (set! counter (+ counter 1))
              (set! nz (+ nz s))
            )
          )
          (set! ny (+ ny (* (element-get iy ix 'fall) t)))
                
          (glTranslatef (- nx xt) (- ny yt) (- nz zt))
          (set! xt nx)
          (set! yt ny)
          (set! zt nz)

          (if (and (equal? gamestate 'PLAYING)
                   (= cposx ix) (= cposy iy))
            (begin
              (glEnable GL_LIGHT2)
            )
          )
          
          (glPushMatrix)
          (glRotatef (element-get iy ix 'angle)
                     (element-get iy ix 'ax)
                     (element-get iy ix 'ay)
                     (element-get iy ix 'az))
          
          (setmaterial (vector-ref color-map (element-get iy ix 'color)))
          (glCallList (+ objectlists (element-get iy ix 'shape)))
          
          (glPopMatrix)

          (if (and (equal? gamestate 'PLAYING)
                   (= cposx ix) (= cposy iy))
            (glDisable GL_LIGHT2)
          )

	  (when (and (equal? gamestate 'PLAYING)
		     (not (memq action-mode '(ACTION-REMOVING ACTION-DROPPING ACTION-LOOKING)))
		     bubble-x bubble-y
		     (= ix bubble-x)
		     (= iy bubble-y))
	    (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
	    (glPushMatrix)
	    (setmaterial (if locked? bubble-lock-color bubble-color))
	    (glCallList (+ objectlists 7))
	    (glPopMatrix)
	    (glBlendFunc GL_ONE GL_ONE))
          
          (set! x (+ x t))
        )
        (set! y (- y t))
      )
    )
    
    (glPopMatrix)
    
    ; draw the flying scores
    (glDisable GL_DEPTH_TEST)
    (score-for-each
      (lambda (key elem)
        (let*
          ( (color (make-vector 4 1.0))
            (fade (score-get elem 'fade))
            (x    (score-get elem 'x))
            (y    (score-get elem 'y))
            (z    (score-get elem 'z))
            (val  (score-get elem 'value))
          )
          (vector-set! color 0 fade)
          (vector-set! color 1 fade)
          (vector-set! color 2 fade)
          (glPushMatrix)
          (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE
                        (vector->gl-float-vector color))
          (glTranslatef x (- y (- 1.0 fade)) z)
          (string-draw (string-append "+" (number->string val)) )
          (glPopMatrix)
        )
      )
      score-numbers
    )
    (glEnable GL_DEPTH_TEST)
    
    ; draw the scores on the left hand side
    (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE
                  (vector->gl-float-vector (vector-ref color-map 5)))
    (glPushMatrix)
    (glTranslatef scorex scorey scorez)
    (glScalef 0.8 0.8 0.8)
    (string-draw "SCORE")
    (glTranslatef 0.0 (- linespace) 0.0)
    (string-draw "LEVEL")
    (glTranslatef 0.0 (- linespace) 0.0)
    (string-draw "MOVES")
    (cond
      ( (equal? gamestate 'GAME-OVER)
        (glTranslatef 0.0 (* -2.5 linespace) 0.0)
        (glScalef 0.6 0.6 0.6)
        (string-draw "GAME OVER")
	(glTranslatef 0.0 (* -1.5 linespace) 0.0)
        (glScalef 0.5 0.5 0.5)
	(glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE
		      (vector->gl-float-vector grey))
        (string-draw "SPACE BAR TO START")
	(glTranslatef 0.0 (* -0.6 linespace) 0.0)
        (string-draw "H FOR HELP")
	(glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE
		      (vector->gl-float-vector (vector-ref color-map 5)))
      )
      ( (and (equal? gamestate 'PLAYING)
	     need-help?)
	(glTranslatef 0.0 (* -3.75 linespace) 0.0)
        (glScalef 0.3 0.3 0.3)
	(glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE
		      (vector->gl-float-vector grey))
        (string-draw "DRAG JEWEL WITH MOUSE OR USE ARROW KEYS AND SPACE BAR   H FOR HELP")
	(glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE
		      (vector->gl-float-vector (vector-ref color-map 5))))
    )
    (glPopMatrix)
    
    (glPushMatrix)
    (glTranslatef scorex
                  (- scorey (* linespace 0.4)) 
                  scorez)
    (glScalef 0.8 0.8 0.8)
    (string-draw (number->string jewel-score) )
    (glTranslatef 0.0 (- linespace) 0.0)
    (string-draw (number->string jewel-level) )
    (glTranslatef 0.0 (- linespace) 0.0)
    (string-draw (number->string jewel-nmoves) )  
    (glPopMatrix)

    (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE
                  (vector->gl-float-vector (vector-ref color-map 6)))
    ; if not playing cover with dim square
    (if (equal? gamestate 'GAME-OVER)  
      (high-score-render)
    )
  
    (if (equal? gamestate 'DIFFICULTY)
      (difficulty-render)
    )
  )

  ; -----------------------------------------------------------------
  ; Mouse handling
  ; -----------------------------------------------------------------

  (define last-click-x #f)
  (define last-click-y #f)
  (define num-unproductive-clicks 0)

  (define (jewel-mouse-down button x y)
    (set! down-x x)
    (set! down-y y)
    (set! isdown? #t)
    (set! num-unproductive-clicks (add1 num-unproductive-clicks))
    (set! need-help? (num-unproductive-clicks . > . 5))
    (let ([pos (getpos x y)])
      (when pos
	(set! last-click-x (vector-ref pos 0))
	(set! last-click-y (vector-ref pos 1))))
    (set! bubble-x #f)
    (set! bubble-y #f)
    (set! revert-bubble-x #f)
    (set! revert-bubble-y #f)
  )


  (define (jewel-mouse-up button x y)
    (set! isdown? #f)
  )
  
  (define (jewel-key-move dx dy)
    (if (and locked?
	     (equal? gamestate   'PLAYING)
	     (equal? action-mode 'ACTION-WAITING)
	     bubble-x
	     bubble-y
	     (<= 0 (+ bubble-x dx) (sub1 ex))
	     (<= 0 (+ bubble-y dy) (sub1 ey)))
	(let ([bx bubble-x]
	      [by bubble-y])
	  (set! revert-bubble-x bx)
	  (set! revert-bubble-y by)
	  (set! bubble-x (+ bubble-x dx))
	  (set! bubble-y (+ bubble-y dy))
	  (try-to-swap bx by dx dy))
	(begin
	  (set! revert-bubble-x #f)
	  (set! revert-bubble-y #f)
	  (set! bubble-x
		(cond
		 [bubble-x (max 0 (min (sub1 ex) (+ bubble-x dx)))]
		 [last-click-x last-click-x]
		 [(dx . >= . 0) 0]
		 [else (- ex 1)]))
	  (set! bubble-y
		(cond
		 [bubble-y (max 0 (min (sub1 ex) (+ bubble-y dy)))]
		 [last-click-y last-click-y]
		 [(dy . >= . 0) 0]
		 [else (- ey 1)]))))
    (set! isdown? #f)
  )

  (define (jewel-key-lock)
    (set! locked? (not locked?))
    (set! isdown? #f)
    ) 
  
  
  ; from the mouse position determine which object will be selected
  (define (getpos mx my)
  
    (let*
      ( (screen (make-vector 4))
        (v0 (vector-ref viewport 0))
        (v1 (vector-ref viewport 1))
        (v2 (vector-ref viewport 2))
        (v3 (vector-ref viewport 3))
        (world #f)
      )
      (vector-set! screen 0 (- (/ (* (- mx v0) 2.0) v2) 1.0))
      (vector-set! screen 1 (- (/ (* (- my v1) 2.0) v3) 1.0))
      (vector-set! screen 2 0.27272727)
      (vector-set! screen 3 1.0)
    
      (set! world (array-mult-vector unproject_matrix screen))
    
      (if (= (vector-ref world 3) 0.0)
        (make-vector 2 -1)
        (let*
          ( (w0 (vector-ref world 0))
            (w1 (vector-ref world 1))
            (w2 (vector-ref world 2))
            (w3 (vector-ref world 3))
            (z (/ w2 w3))
            (x (+ (/ (- (* z (/ w0 w3)) shiftx) spacing) (/ ex 2)))
            (y (+ (/ (- (* z (/ w1 w3)) shifty) spacing) (/ ey 2)))
            (vect (make-vector 2 0))
          )
          (if (>= x 0.0)
            (set! x (inexact->exact (truncate x)))
            (set! x -1)
          )
          (if (>= y 0.0)
            (set! y (inexact->exact (truncate y)))
            (set! y -1)
          )
          (vector-set! vect 0 x)
          (vector-set! vect 1 y)
          
          vect
        )
      )
    )
  )


  (define (jewel-mouse-motion x y)
    (let*
      ( (pos (getpos x y))
        (dx  (if isdown? (- x down-x) 0.0))
        (dy  (if isdown? (- y down-y) 0.0))
        (px  #f) 
        (py  #f)
      )
      (set! cposx (vector-ref pos 0))
      (set! cposy (vector-ref pos 1))
    
      ; if mouse was pressed, 
      ;    we are playing,
      ;    no action is happening
      ;    and mouse is moved, so try to swap
      (if (and isdown? 
               (equal? gamestate   'PLAYING)
               (equal? action-mode 'ACTION-WAITING)
               (> (+ (* dx dx) (* dy dy)) (* dist dist))
          )
        (begin
          (set! isdown? #f)
          (if (> (abs dx) (abs dy))
            (begin
              (if (< dx 0) 
                (set! dx -1)
                (set! dx 1)
              )
              (set! dy 0)
            )
            (begin
              (if (< dy 0)
                (set! dy -1)
                (set! dy 1)
              )
              (set! dx 0)
            )
          )
          
          (set! pos (getpos down-x down-y))
          (set! px  (vector-ref pos 0))
          (set! py  (vector-ref pos 1))
          (when (not (or (< px 0.0)
			 (>= px ex)
			 (< py 0.0)
			 (>= py ey)
			 (< (+ px dx) 0.0)
			 (>= (+ px dx) ex)
			 (< (+ py dy) 0.0)
			 (>= (+ py dy) ey)))
	    (set! num-unproductive-clicks 0)
	    (set! need-help? #f)
	    (try-to-swap px py dx dy)
          )
        )
      )
    )
  )

  (define (try-to-swap px py dx dy)
    (element-swap! py px (+ py dy) (+ px dx))
    ;; mark the elements for the swap
    (element-set! py px 'swapping 1)
    (element-set! py px 'dx dx)
    (element-set! py px 'dy (- dy))
    (element-set! (+ py dy) (+ px dx) 'swapping 1)
    (element-set! (+ py dy) (+ px dx) 'dx (- dx))
    (element-set! (+ py dy) (+ px dx) 'dy dy)
    (set! tryswap? #t))

  ; -----------------------------------------------------------------
  ; MAIN
  ; -----------------------------------------------------------------

  (jewel-init-game)

  (define show-jewel-help
    (show-scribbling '(lib "games/scribblings/games.scrbl") "jewel"))

  (define *MAIN_WINDOW* 
          (new jewel-frame%
               (label "Jewel")
               (min-width 640)
               (min-height 480)
               (stretchable-width #f)
               (stretchable-height #f)
               (style '(no-resize-border))
          )
  )

  (define *OPENGL_WINDOW* 
          (new jewel-canvas% (parent *MAIN_WINDOW*)
                             (min-width 100)
                             (min-height 100)
                             (expose        jewel-redraw)
                             (realize       jewel-realize)
                             (configure     jewel-configure)
                             (mouse-press   jewel-mouse-down)
                             (mouse-motion  jewel-mouse-motion)
                             (mouse-release jewel-mouse-up)
          )
  )

  (send *MAIN_WINDOW* show #t)
    
  (define *TIMER* (new timer% 
                       (notify-callback jewel-control-game)
                       (interval        timer-interval)
                       (just-once?      #f)
                   )
  )

)))
