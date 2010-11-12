(module gl-board mzscheme
  (require sgl
           sgl/gl
           sgl/gl-vectors
           mzlib/class
           mzlib/list
           mred)
  
  (provide gl-board%)

  (define-struct space (draw info))
  (define-struct piece (x y z draw info enabled?))
  (define-struct heads-up (w h draw info))
  
  (define (get-info x)
    (cond
      ((piece? x) (piece-info x))
      ((space? x) (space-info x))
      (else #f)))
  
  ;; interpolate : real gl-double-vector gl-double-vector -> gl-double-vector
  ;; returns the point on the p1-p2 line with the given z coordinate.
  (define (interpolate z p1 p2)
    (let* ((v (gl-double-vector- p2 p1))
           (c (/ (- z (gl-vector-ref p1 2))
                 (gl-vector-ref v 2))))
      (gl-double-vector+ p1 (gl-double-vector* c v))))
  
  (define (get-viewport)
    (glGetIntegerv GL_VIEWPORT 4))

  (define (get-projection)
    (glGetDoublev GL_PROJECTION_MATRIX 16))
    
  (define (get-modelview)
    (glGetDoublev GL_MODELVIEW_MATRIX 16))

  (define gl-board%
    (class canvas%
      (inherit with-gl-context swap-gl-buffers refresh get-width get-height get-dc focus)
      
      ;; min-x, max-x, min-y, max-y, lift: real
      ;; move: info gl-double-vector ->
      ;; min-x, max-x, min-y and max-y specify the dimensions of the board.
      ;; lift specifies how far a piece is picked up when being moved.
      ;; move is called when a piece is moved to a space (possibly it's current space),
      ;; when a space is clicked on, and when a space is dragged to another space.
      ;; move is given the info of the piece or space selected, and coordinates
      ;; it is moved to.
      (init-field min-x max-x min-y max-y lift (move void) (who "this game"))
      
      (define spaces null)
      (define pieces null)
      (define heads-ups null)
      
      (define/public (get-pieces) (map piece-info pieces))
      (define/public (get-spaces) (map space-info spaces))
      (define/public (get-heads-ups) (map heads-up-info heads-ups))

      ;; add-space: (->) info ->
      ;; Adds a space to this board.  The draw thunk should draw the space 
      ;; when called.  The value of the info argument will be given to
      ;; the move function when the space is selected.
      (define/public (add-space draw info)
        (set! spaces (cons (make-space draw info) spaces)))
      
      ;; add-piece: real real real (->) info ->
      ;; Adds a piece to this board.  The draw thunk should draw the piece
      ;; when called.  The value of the info argument will be given to
      ;; the move function when the piece is selected.  The piece is translated
      ;; by the x, y and z arguments before drawing.
      (define/public (add-piece x y z draw info)
        (set! pieces (cons (make-piece x y z draw info #t) pieces)))

      ;; set-space-draw: info (->) ->
      ;; Sets the drawing method of all spaces whose info is equal to space to d.
      (define/public (set-space-draw space d)
        (for-each
         (lambda (s)
           (when (equal? (space-info s) space)
             (set-space-draw! s d)))
         spaces))
      
      ;; set-piece-draw: info (boolean ->) ->
      ;; Sets the drawing method of all pieces whose info is equal to piece to d.
      (define/public (set-piece-draw piece d)
        (for-each
         (lambda (p)
           (when (equal? (piece-info p) piece)
             (set-piece-draw! p d)))
         pieces))

      ;; set-heads-up-draw: info (->) ->
      ;; Sets the drawing method of all heads-up objects whose info is equal to piece to d.
      (define/public (set-heads-up-draw piece d)
        (for-each
         (lambda (p)
           (when (equal? (heads-up-info p) piece)
             (set-heads-up-draw! p d)))
         heads-ups))

      ;; enabled/disables dragging of a piece
      (define/public (enable-piece info on?)
	(let ([p (ormap (lambda (p)
			  (and (equal? info (piece-info p)) p))
			pieces)])
	  (if p
	      (set-piece-enabled?! p (and on? #t))
	      (raise-mismatch-error 'enable-piece "no matching piece: " info))))
      
      ;; enabled?: info -> boolean
      (define/public (enabled? info)
        (let ((p (ormap (lambda (p)
                          (and (equal? info (piece-info p)) p))
                        pieces)))
          (if p
              (piece-enabled? p)
              (raise-mismatch-error 'enabled? "no matching piece: " info))))
      
      ;; remove-piece: info ->
      ;; Removes all pieces whose info is equal? to p-i from this board.
      (define/public (remove-piece p-i)
        (set! pieces (filter
                      (lambda (x)
                        (not (equal? p-i (piece-info x))))
                      pieces)))
      
      (define/public (add-heads-up w h draw info)
        (set! heads-ups (append heads-ups
                                (list (make-heads-up w h draw info)))))
      
      (define/public (remove-heads-up info)
        (set! heads-ups (filter
                         (lambda (x)
                           (not (equal? info (heads-up-info x))))
                         heads-ups)))
      
      ;; How far the light is from the board's center
      (define light-distance (* 4.0 (max (- max-x min-x) (- max-y min-y))))
      ;; The board's center
      (define center-x (/ (+ max-x min-x) 2))
      (define center-y (/ (+ max-y min-y) 2))

      (define eye-distance (* 2.0 (max (- max-x min-x) (- max-y min-y))))
      (define delta-eye-distance (/ eye-distance 30.0))
      (define fov 30)
      (init-field [theta 45])
      (init-field [phi 0]
                  [delta-x 0]
                  [delta-y 0])
      
      ;; Transformation used to draw shadows.
      (define shadow-projection 
        (let ((ld light-distance))
          (gl-double-vector ld 0 0 0
                            0 ld 0 0 
                            0 0 ld (- 1)
                            0 0 0 0)))
      
      ;; Either #f or the currently selected piece.
      (define mouse-state #f)
      
      ;; dragging-correction : (union #f 3-element-gl-double-vector)
      (define dragging-correction #f)
      
      ;; dragging : (union #f 3-element-gl-double-vector)
      ;; The mouse's location while dragging
      (define dragging #f)
            
      ;; draw-spaces : bool ->
      ;; Draws the board.  If select? is true, then names are loaded for selection
      ;; with each space named after its index in the spaces list.
      (define/private (draw-spaces select?)
        (gl-normal 0.0 0.0 1.0)
        (let loop ((i 0)
                   (s spaces))
          (unless (null? s)
            (when select?
              (gl-load-name i))
            ((space-draw (car s)))
            (loop (add1 i)
                  (cdr s)))))
      
      ;; draw-pieces : bool ->
      ;; Draws the pieces.  If select? is true, then names are loaded for selection
      ;; with each piece named after its index plus the number of spaces.
      (define/private (draw-pieces select? shadow?)
        (let loop ((i (length spaces))
                   (ps (if (and (piece? mouse-state) 
                                dragging)
                           (cons (make-piece (gl-vector-ref dragging 0)
                                             (gl-vector-ref dragging 1)
                                             (gl-vector-ref dragging 2)
                                             (piece-draw mouse-state)
                                             (piece-info mouse-state)
                                             #f)
                                 pieces)
                           pieces)))
          (unless (null? ps)
            (let ((p (car ps)))
              (unless (and dragging (eq? mouse-state p))  ;; Don't draw the dragged piece
                ;; in its home location.
                (when select?
                  (gl-load-name i))
                (gl-push-matrix)
                (gl-translate (piece-x p) (piece-y p) (piece-z p))
                ((piece-draw p) shadow?)
                (gl-pop-matrix))
              (loop (add1 i)
                    (cdr ps))))))

      (inherit get-client-size)

      (define/private (draw-heads-up sh)
        (let-values ([(w) (apply + (map heads-up-w heads-ups))]
                     [(h) (apply max 0 (map heads-up-h heads-ups))])
          (let ([dy sh]
                [x (/ (- w) 2)])
            (let loop ([heads-ups heads-ups]
                       [x x])
              (unless (null? heads-ups) 
                (let ([hu (car heads-ups)])
                  (gl-push-matrix)
                  (gl-translate (+ x (/ (heads-up-w hu) 2))
                                dy
                                0)
                  ((heads-up-draw hu))
                  (gl-pop-matrix)
                  (loop (cdr heads-ups)
                        (+ x (heads-up-w hu)))))))))
      
      (define/override (on-paint)
        (with-gl-context
         (lambda ()
           (gl-clear 'color-buffer-bit 'depth-buffer-bit 'stencil-buffer-bit)

           (draw-spaces #f)

           ;; draw the board, putting 1 in the stencil buffer for each exposed
           ;; pixel of a piece.
           (gl-enable 'stencil-test)
           (gl-stencil-func 'always 1 1)
           (gl-stencil-op 'keep 'keep 'replace)
           (draw-pieces #f #f)
           (gl-disable 'stencil-test)
           
           ;; Very simple shadowing on the board, only blending the shadow
           ;; with pixels stenciled to 0, i.e. avoid the pieces.
	   (gl-enable 'stencil-test)
	   (gl-stencil-func 'equal 0 1)
           ;; Once a pixel has been shadows, use saturating incr to set its
           ;; value to 1, preventing multi-shadowing.
	   (gl-stencil-op 'keep 'keep 'incr)
           (gl-disable 'depth-test)
	   (gl-enable 'blend)
	   (gl-blend-func 'dst-color 'zero)
           (gl-color 0.5 0.5 0.5)
           (gl-push-matrix)
           (gl-translate center-x center-y light-distance)
           (gl-mult-matrix shadow-projection)
           (gl-translate (- center-x) (- center-y) (- light-distance))
           (draw-pieces #f #t)
           (gl-enable 'depth-test)
	   (gl-disable 'blend)
	   (gl-disable 'stencil-test)
           (gl-pop-matrix)

           (gl-clear 'depth-buffer-bit 'stencil-buffer-bit)
           (gl-matrix-mode 'projection)
           (gl-push-matrix)
           (gl-load-identity)
           (gl-perspective 45 (/ (get-width) (get-height)) 5 10)
           (gl-matrix-mode 'modelview)
           (gl-push-matrix)
           (gl-load-identity)
           (gl-translate 0 0 (- 10))
           (draw-heads-up -3.5)
           (gl-pop-matrix)
           (gl-matrix-mode 'projection)
           (gl-pop-matrix)
           (gl-matrix-mode 'modelview)

           (gl-flush)
           (swap-gl-buffers))))
      
      (define/override (on-size w h)
        (with-gl-context
         (lambda ()
           (gl-viewport 0 0 w h)
           (setup-view/proj)))
        (refresh))
      
      (define/private (setup-view/proj)
        (gl-matrix-mode 'projection)
        (gl-load-identity)
        (gl-perspective fov (/ (get-width) (get-height))
                        (/ eye-distance 2) (* 2 eye-distance))
        (gl-matrix-mode 'modelview)
        (gl-load-identity)
        (gl-translate delta-x delta-y (- eye-distance))
        (gl-rotate (- theta) 1 0 0)
        (gl-rotate phi 0 0 1)
        (gl-translate (- center-x) (- center-y) 0))
      
      ;; pick: real real -> (union piece space #f)
      ;; Returns the piece or space at screen coordinates x y and #f is there
      ;; is no such object.
      (define/private (pick x y)
        (let ((vp (get-viewport))
              (proj (get-projection))
              (selection (glSelectBuffer 512)))
          (gl-render-mode 'select)
          (gl-matrix-mode 'projection)
          (gl-push-matrix)
          (gl-load-identity)
          (gl-pick-matrix x (- (gl-vector-ref vp 3) y 1) 1.0 1.0 vp)
          (gl-mult-matrix proj)
          (gl-matrix-mode 'modelview)
          (gl-init-names)
          (gl-push-name 0)
          (draw-spaces #t)
          (draw-pieces #t #f)
          (gl-matrix-mode 'projection)
          (gl-pop-matrix)
          (gl-matrix-mode 'modelview)
          (gl-flush)
          (let* ((hits (gl-render-mode 'render))
                 (results (sort (gl-process-selection (select-buffer->gl-uint-vector selection)
                                                      hits)
                                (lambda (a b)
                                  (< (gl-selection-record-min-z a)
                                     (gl-selection-record-min-z b))))))
            (cond
              ((null? results) #f)
              (else
               (let ((index (car (gl-selection-record-stack (car results)))))
                 (cond
                   ((< index (length spaces))
                    (list-ref spaces index))
                   (else
                    (list-ref pieces (- index (length spaces)))))))))))
         
               
      ;; screen-world: real real real -> gl-double-vector
      ;; Given a screen x and y, return the world x, y, z 
      ;; corresponding to the given world z.
      (define/private (screen->world x y z)
        (let* ((v (get-viewport))
               (m (get-modelview))
               (p (get-projection))
               (real-y (- (gl-vector-ref v 3) y 1)))
          (interpolate z
                       (gl-un-project x real-y 0.0 m p v)
                       (gl-un-project x real-y 1.0 m p v))))
              
      (define/private (drag x y)
        (with-gl-context
         (lambda ()
           (set! dragging
                 (gl-double-vector- (screen->world x y lift)
                                    dragging-correction))))
        (refresh))

      (define/private (start-dragging x y)
        (with-gl-context
         (lambda ()
           (let ((v (pick x y)))
             (when (and (piece? v) (piece-enabled? v))
               (set! mouse-state v)
               (set! dragging-correction
                     (gl-double-vector-
                      (screen->world x y lift)
                      (gl-double-vector (piece-x v) (piece-y v) lift)))
               (drag x y))))))
      
      (define/private (stop-dragging x y)
        (move (get-info mouse-state) dragging)
        (set! mouse-state #f)
        (set! dragging-correction #f)
        (set! dragging #f)
        (refresh))
      
      (define/override (on-event e)
        (cond
          ((send e button-down? 'left)
           (start-dragging (send e get-x) (send e get-y)))
          ((and mouse-state
                (or (send e button-up? 'left)
                    (and (send e moving?)
                         (not (and (send e dragging?) (send e get-left-down))))))
           (stop-dragging (send e get-x) (send e get-y)))
          ((and (send e dragging?) (send e get-left-down))
           (unless dragging
             (start-dragging (send e get-x) (send e get-y)))
           (when dragging
             (drag (send e get-x) (send e get-y))))))
      
      (define/override (on-char e)
        (case (send e get-key-code)
          ((left) 
           (if (send e get-meta-down)
               (set! delta-x (- delta-x 0.1))
               (set! phi (+ phi 3))))
          ((right) 
           (if (send e get-meta-down)
               (set! delta-x (+ delta-x 0.1))
               (set! phi (- phi 3))))
          ((up) 
           (if (send e get-meta-down)
               (set! delta-y (+ delta-y 0.1))
               (set! theta (- theta 3))))
          ((down)
           (if (send e get-meta-down)
               (set! delta-y (- delta-y 0.1))
               (set! theta (+ theta 3))))
          ((#\+) (unless (< fov 4)
		   (set! fov (- fov 2))))
          ((#\=) (set! eye-distance (- eye-distance delta-eye-distance)))
          ((#\_) (unless (> fov 176)
		   (set! fov (+ fov 2))))
	  ((#\-) (set! eye-distance (+ eye-distance delta-eye-distance))))
        (with-gl-context
         (lambda ()
           (setup-view/proj)))
        (refresh))
      
      (let ([cfg (new gl-config%)])
	(send cfg set-multisample-size 4)
	(send cfg set-stencil-size 1)
	(super-new (style '(gl no-autoclear)) (gl-config cfg)))
      
      (unless (send (get-dc) get-gl-context)
	(message-box "Error"
		     (format (string-append
			      "~a requires OpenGL, but there was an error initializing"
			      " the OpenGL context. Maybe OpenGL is not supported by" 
			      " the current display.")
			     who)
		     #f
		     '(ok stop))
	(exit))

      ;; initial setup
      (with-gl-context
       (lambda ()
         (gl-shade-model 'smooth)
         (when (>= (gl-get-gl-version-number) 13)
           (gl-enable 'multisample))
         (gl-enable 'lighting)
         (gl-enable 'light0)
         (gl-enable 'depth-test)
         ;(gl-light-model 'light-model-local-viewer 1.0)
         ;(gl-light-model-v 'light-model-ambient (gl-float-vector 0.0 0.0 0.0 0.0))
         (gl-light-v 'light0 'position (gl-float-vector center-x center-y light-distance 1.0))
         ;(gl-light-v 'light0 'ambient (gl-float-vector 0.7 0.7 0.7 1.0))
         ;(gl-light-v 'light0 'diffuse (gl-float-vector 0.9 0.9 0.9 1.0))
         ;(gl-light-v 'light0 'specular (gl-float-vector 0.0 0.0 0.0 1.0))
         (gl-clear-color 1.0 1.0 1.0 1.0)
         (setup-view/proj)))
      (focus)))
  )
