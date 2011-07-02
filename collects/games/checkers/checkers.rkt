#lang mzscheme

(require games/gl-board-game/gl-board
         mzlib/class
         mzlib/math
         mred
         mzlib/unit
         sgl/gl-vectors
         sgl
         sgl/gl
         srfi/25/array
         mrlib/include-bitmap
         "honu-bitmaps.rkt")
(provide game@)

(define-struct image (width height rgba))

(define (argb->rgba argb)
  (let* ([length (bytes-length argb)]
         [rgba (make-gl-ubyte-vector length)])
    (let loop ((i 0))
      (when (< i length)
        (gl-vector-set! rgba (+ i 0) (bytes-ref argb (+ i 1)))
        (gl-vector-set! rgba (+ i 1) (bytes-ref argb (+ i 2)))
        (gl-vector-set! rgba (+ i 2) (bytes-ref argb (+ i 3)))
        (gl-vector-set! rgba (+ i 3) (bytes-ref argb (+ i 0)))
        (loop (+ i 4))))
    rgba))

(define (bitmap->argb bmp)
  (let* ([width (send bmp get-width)]
         [height (send bmp get-height)]
         [argb (make-bytes (* 4 width height) 255)]
         [dc (make-object bitmap-dc% bmp)])
    (send dc get-argb-pixels 0 0 width height argb #f)
    (when (send bmp get-loaded-mask)
      (send dc set-bitmap (send bmp get-loaded-mask))
      (send dc get-argb-pixels 0 0 width height argb #t))
    (send dc set-bitmap #f)
    argb))

(define (bitmap->image bmp)
  (make-image (send bmp get-width) (send bmp get-height)
              (argb->rgba (bitmap->argb bmp))))

(define light-square-img (bitmap->image (include-bitmap "light.jpg")))
(define light-square-color (gl-float-vector .7216 .6471 .5176 1))
(define dark-square-img (bitmap->image (include-bitmap "dark.jpg")))
(define dark-square-color (gl-float-vector .4745 .3569 .2627 1))

(define (color-name->vector name darken?)
  (let ([color (send the-color-database find-color name)]
        [adj (if darken? sqr values)])
    (unless color
      (error 'color-name->vector "could not find ~e" name))
    (gl-float-vector (adj (/ (send color red) 255))
                     (adj (/ (send color green) 255))
                     (adj (/ (send color blue) 255))
                     1.0)))

(define light-checker-img (bitmap->image honu-down-bitmap))
(define dark-checker-img (bitmap->image honu-bitmap))

(define-struct space-info (x y light?))
(define-struct piece-info (x y color king?) (make-inspector))
(define-struct moves (list forced-jump?))

(define-signature model^
  (move))
(define-signature view^
  (add-space add-piece remove-piece move-piece set-turn show))

(define-unit view@
  (import model^)
  (export view^)

  (define (get-space-draw-fn space)
    (let* ([list-id (get-square-dl (space-info-light? space)
                                   (send texture-box get-value))]
           [sx (space-info-x space)]
           [sy (space-info-y space)])
      (lambda ()
        (gl-push-matrix)
        (gl-translate sx sy 0)
        (gl-call-list list-id)
        (gl-pop-matrix))))

  (define (add-space space)
    (send board add-space (get-space-draw-fn space) space))

  (define (get-piece-draw-fn piece glow?)
    (let ([list-id (get-checker-dl (eq? 'red (piece-info-color piece))
                                   (piece-info-king? piece)
                                   (send texture-box get-value))])
      (if glow?
        (lambda (for-shadow?)
          (gl-material-v 'front 'emission (gl-float-vector 0.15 0.15 0.15 1.0))
          (gl-call-list ((if for-shadow? cdr car) list-id))
          (gl-material-v 'front 'emission (gl-float-vector 0.0 0.0 0.0 1.0)))
        (lambda (for-shadow?)
          (gl-call-list ((if for-shadow? cdr car) list-id))))))

  (define add-piece
    (case-lambda
      [(piece) (add-piece piece #f)]
      [(piece glow?)
       (send board add-piece
             (+ .5 (piece-info-x piece)) (+ .5 (piece-info-y piece)) 0.0
             (get-piece-draw-fn piece glow?)
             piece)]))

  (define (move-piece from to-x to-y)
    (remove-piece from)
    (add-piece (make-piece-info to-x to-y
                                (piece-info-color from)
                                (piece-info-king? from))))

  (define (remove-piece p)
    (send board remove-piece p))

  (define (internal-move old move-to)
    (when (piece-info? old) (move old move-to)))

  (define (set-turn turn moves)
    (let ([pieces (send board get-pieces)])
      (for-each (lambda (p)
                  (send board set-piece-draw p (get-piece-draw-fn p #f))
                  (send board enable-piece p #f))
                pieces)
      (for-each (lambda (p)
                  (send board set-piece-draw p (get-piece-draw-fn p #t))
                  (send board enable-piece p #t))
                (moves-list moves)))
    (send msg set-label
          (if (null? (moves-list moves))
            (format "~a wins!" (if (eq? turn 'red) "Black" "Red"))
            (format "~a's turn~a"
                    (if (eq? turn 'red) "Red" "Black")
                    (if (moves-forced-jump? moves) " - must take jump" "")))))

  (define f (new frame% (label "Checkers") (width 800) (height 600)))
  (define board
    (new gl-board% (parent f) (who "Checkers")
         (min-x 0.0) (max-x 8.0) (min-y 0.0) (max-y 8.0)
         (lift .35)
         (move internal-move)))
  (define hp (new horizontal-pane% (parent f) (stretchable-height #f)))
  (define msg
    (new message% (label "") (parent hp) (stretchable-width #t)))
  (define texture-box
    (new check-box% (label "Textured") (parent hp)
         (callback
          (lambda (box _)
            (for-each
             (lambda (s)
               (send board set-space-draw s (get-space-draw-fn s)))
             (send board get-spaces))
            (for-each
             (lambda (p)
               (send board set-piece-draw p
                     (get-piece-draw-fn p (send board enabled? p))))
             (send board get-pieces))
            (send board refresh)))))
  (new grow-box-spacer-pane% [parent hp])
  (send texture-box set-value #t)

  (define q
    (send board with-gl-context (lambda () (gl-new-quadric))))

  (define-values (dark-tex light-tex dark-checker-tex light-checker-tex)
    (send board with-gl-context
          (lambda ()
            (let ((x (glGenTextures 4)))
              (values (gl-vector-ref x 0)
                      (gl-vector-ref x 1)
                      (gl-vector-ref x 2)
                      (gl-vector-ref x 3))))))

  (define (init-tex tex img)
    (send board with-gl-context
          (lambda ()
            (glBindTexture GL_TEXTURE_2D tex)
            (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
            (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
            (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
            (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
            (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA
                          (image-width img) (image-height img) 0
                          GL_RGBA GL_UNSIGNED_BYTE (image-rgba img)))))

  (init-tex light-tex light-square-img)
  (init-tex dark-tex dark-square-img)
  (init-tex dark-checker-tex dark-checker-img)
  (init-tex light-checker-tex light-checker-img)

  (define (make-piece-dl color height tex shadow?)
    (send board with-gl-context
          (lambda ()
            (let ([list-id (gl-gen-lists 1)])
              (gl-quadric-draw-style q 'fill)
              (gl-quadric-normals q 'smooth)
              (gl-new-list list-id 'compile)

              (when shadow? (gl-disable 'lighting))
              (gl-material-v 'front 'specular (gl-float-vector 1.0 1.0 1.0 1.0))
              (gl-material 'front 'shininess 120.0)  

              (gl-material-v 'front 'ambient-and-diffuse color)
              (gl-cylinder q .35 .35 height 25 1)
              (gl-push-matrix)
              (gl-translate 0.0 0.0 height)

              (when (and tex (not shadow?))
                (gl-enable 'texture-2d)
                (glBindTexture GL_TEXTURE_2D tex)
                (glTexEnvf GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
                (gl-quadric-texture q #t))

              (gl-disk q 0.0 .35 25 1)

              (when (and tex (not shadow?))
                (gl-quadric-texture q #f)
                (glTexEnvf GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
                (gl-disable 'texture-2d))

              (gl-pop-matrix)

              (when shadow? (gl-enable 'lighting))
              (gl-end-list)
              list-id))))

  (define (make-tex-square-dl tex)
    (send board with-gl-context
          (lambda ()
            (let ([list-id (gl-gen-lists 1)])
              (gl-new-list list-id 'compile)
              (gl-enable 'texture-2d)
              (glBindTexture GL_TEXTURE_2D tex)
              (gl-material-v 'front 'ambient-and-diffuse
                             (gl-float-vector 1 1 1 1))
              (gl-begin 'polygon)
              (gl-tex-coord 0.0 0.0)
              (gl-vertex 0.0 0.0 0.0)
              (gl-tex-coord 1.0 0.0)
              (gl-vertex 1.0 0.0 0.0)
              (gl-tex-coord 1.0 1.0)
              (gl-vertex 1.0 1.0 0.0)
              (gl-tex-coord 0.0 1.0)
              (gl-vertex 0.0 1.0 0.0)
              (gl-end)
              (gl-disable 'texture-2d)
              (gl-end-list)
              list-id))))

  (define (make-square-dl color)
    (send board with-gl-context
          (lambda ()
            (let ([list-id (gl-gen-lists 1)])
              (gl-new-list list-id 'compile)
              (gl-material-v 'front 'ambient-and-diffuse color)
              (gl-begin 'polygon)
              (gl-vertex 0.0 0.0 0.0)
              (gl-vertex 1.0 0.0 0.0)
              (gl-vertex 1.0 1.0 0.0)
              (gl-vertex 0.0 1.0 0.0)
              (gl-end)
              (gl-end-list)
              list-id))))

  (define checkers
    (map (lambda (x)
           (let ([color (if (car x)
                            (color-name->vector "firebrick" #t)
                            (gl-float-vector 0.15 0.15 0.15 1.0))]
                 [height (if (cadr x) .4 .2)]
                 [tex (if (caddr x)
                        (if (car x) light-checker-tex dark-checker-tex)
                        #f)])
             (cons x (cons (make-piece-dl color height tex #f)
                           (make-piece-dl color height tex #t)))))
         '((#f #f #f)
           (#f #f #t)
           (#f #t #f)
           (#f #t #t)
           (#t #f #f)
           (#t #f #t)
           (#t #t #f)
           (#t #t #t))))
  (define (get-checker-dl light? king? tex?)
    (cdr (assoc (list light? king? tex?) checkers)))

  (define dark-square (cons (make-tex-square-dl dark-tex)
                            (make-square-dl dark-square-color)))
  (define light-square (cons (make-tex-square-dl light-tex)
                             (make-square-dl light-square-color)))
  (define (get-square-dl light? tex?)
    (let ((getter (if tex? car cdr)))
      (getter (if light? light-square dark-square))))

  (define (show) (send f show #t)))

(define-unit model@
  (import view^)
  (export model^)

  (define turn 'red)
  (define board (make-array (shape 0 8 0 8) #f))

  (let loop ([i 0] [j 0])
    (cond
      [(and (< j 8) (< i 8))
       (cond
         [(even? (+ i j))
          (add-space (make-space-info j i #f))
          (cond [(< i 3)
                 (array-set! board j i (cons 'red #f))
                 (add-piece (make-piece-info j i 'red #f))]
                [(> i 4)
                 (array-set! board j i (cons 'black #f))
                 (add-piece (make-piece-info j i 'black #f))])]
         [else (add-space (make-space-info j i #t))])
       (loop i (add1 j))]
      [(< i 8) (loop (add1 i) 0)]))

  (define (other-color c)
    (if (eq? c 'red) 'black 'red))

  (define (single-move-ok? direction from-x from-y to-x to-y)
    (and (= to-y (+ direction from-y))
         (= 1 (abs (- from-x to-x)))))

  (define (can-move? direction from-x from-y)
    (and (<= 0 (+ from-y direction) 7)
         (or (and (<= 0 (+ from-x 1) 7)
                  (not (array-ref board (+ from-x 1) (+ from-y direction))))
             (and (<= 0 (+ from-x -1) 7)
                  (not (array-ref board (+ from-x -1) (+ from-y direction)))))))

  (define (get-jumped-piece color direction from-x from-y to-x to-y)
    (and (= to-y (+ direction direction from-y))
         (= 2 (abs (- from-x to-x)))
         (let* ([jumped-x (+ from-x (/ (- to-x from-x) 2))]
                [jumped-y (+ from-y direction)]
                [jumped-piece (array-ref board jumped-x jumped-y)])
           (and jumped-piece
                (eq? (other-color color) (car jumped-piece))
                (make-piece-info jumped-x jumped-y
                                 (car jumped-piece) (cdr jumped-piece))))))

  (define (can-jump? direction from-color from-x from-y)
    (let ([to-y (+ direction direction from-y)]
          [to-x1 (+ from-x 2)]
          [to-x2 (- from-x 2)])
      (and (<= 0 to-y 7)
           (or (and (<= 0 to-x1 7)
                    (not (array-ref board to-x1 to-y))
                    (get-jumped-piece from-color direction
                                      from-x from-y
                                      to-x1 to-y))
               (and (<= 0 to-x2)
                    (not (array-ref board to-x2 to-y))
                    (get-jumped-piece from-color direction
                                      from-x from-y
                                      to-x2 to-y))))))

  (define (fold-board f v)
    (let iloop ([i 0] [v v])
      (if (= i 8)
        v
        (let jloop ([j 0] [v v])
          (if (= j 8)
            (iloop (add1 i) v)
            (jloop (add1 j) (if (even? (+ i j)) (f i j v) v)))))))

  (define (get-jump-moves)
    (let ([direction (if (eq? turn 'red) 1 -1)])
      (fold-board
       (lambda (i j l)
         (let ([p (array-ref board i j)])
           (if (and p
                    (eq? (car p) turn)
                    (or (can-jump? direction turn i j)
                        (and (cdr p)
                             (can-jump? (- direction) turn i j))))
             (cons (make-piece-info i j turn (cdr p)) l)
             l)))
       null)))

  (define (get-moves)
    (let ([jumps (get-jump-moves)])
      (if (pair? jumps)
        (make-moves jumps #t)
        (make-moves
         (let ([direction (if (eq? turn 'red) 1 -1)])
           (fold-board
            (lambda (i j l)
              (let ([p (array-ref board i j)])
                (if (and p
                         (eq? (car p) turn)
                         (or (can-move? direction i j)
                             (and (cdr p) (can-move? (- direction) i j))))
                  (cons (make-piece-info i j turn (cdr p)) l)
                  l)))
            null))
         #f))))

  (define (move from to)
    (let* ([to-x (inexact->exact (floor (gl-vector-ref to 0)))]
           [to-y (inexact->exact (floor (gl-vector-ref to 1)))]
           [from-x (piece-info-x from)]
           [from-y (piece-info-y from)]
           [from-color (piece-info-color from)]
           [from-king? (piece-info-king? from)]
           [to-king? (or from-king? (= to-y (if (eq? 'red from-color) 7 0)))]
           [direction (if (eq? turn 'red) 1 -1)])
      (when (and (eq? turn from-color)
                 (<= 0 to-x 7)
                 (<= 0 to-y 7)
                 (not (array-ref board to-x to-y)))
        (cond [(and (null? (get-jump-moves))
		    (or (single-move-ok? direction from-x from-y to-x to-y)
			(and from-king?
			     (single-move-ok? (- direction) from-x from-y
                                              to-x to-y))))
	       (move-piece from to-x to-y)
               (set! turn (other-color from-color))
               (array-set! board to-x to-y (cons from-color to-king?))
               (array-set! board from-x from-y #f)
               (when (and to-king? (not from-king?))
                 (remove-piece (make-piece-info to-x to-y from-color from-king?))
                 (add-piece (make-piece-info to-x to-y from-color to-king?)))
	       (set-turn turn (get-moves))]
              [(or (get-jumped-piece from-color direction from-x from-y
                                     to-x to-y)
		   (and from-king?
			(get-jumped-piece from-color (- direction) from-x from-y to-x to-y)))
               =>
               (lambda (j)
                 (remove-piece j)
                 (move-piece from to-x to-y)
                 (array-set! board (piece-info-x j) (piece-info-y j) #f)
                 (array-set! board from-x from-y #f)
                 (array-set! board to-x to-y (cons from-color to-king?))
                 (when (and to-king? (not from-king?))
                   (remove-piece (make-piece-info to-x to-y from-color from-king?))
                   (add-piece (make-piece-info to-x to-y from-color to-king?)))
                 (cond
                   [(or (can-jump? direction from-color to-x to-y)
                        (and from-king?
                             (can-jump? (- direction) from-color to-x to-y)))
                    (set-turn turn
                              (make-moves (list (make-piece-info
                                                 to-x to-y from-color to-king?))
                                          #t))]
                   [else
                    (set! turn (other-color from-color))
                    (set-turn turn (get-moves))]))]))))

  (set-turn turn (get-moves))
  )

(define-unit show@
  (import view^)
  (export)
  (show))

(define game@
  (compound-unit/infer (import) (export) (link view@ model@ show@)))
