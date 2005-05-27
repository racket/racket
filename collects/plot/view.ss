(module view mzscheme
  (require 
   (lib "plplot.ss" "plot")
   (lib "math.ss" "plot")
   (lib "class.ss")
   (lib "file.ss")
   (lib "mred.ss" "mred")
   (lib "math.ss")
   ;(lib "4.ss" "srfi")
   )
  
  
  ; macro for creating a field in a class with a getter and a setter
  (define-syntax (fields-with-accessors stx) 
    (define (join-identifier prefix ident)
      (datum->syntax-object 
       ident 
       (string->symbol (string-append (symbol->string prefix )(symbol->string (syntax-e ident)))) ))
    (syntax-case stx ()
      [(_ (field init) ... )
       (let ((accessors (map (lambda (id) (join-identifier 'get- id)) (syntax-e #'(field ...))))
             (setters (map (lambda (id) (join-identifier 'set- id)) (syntax-e #'(field ...)))))
         (with-syntax (((accessor ... ) accessors)
                       ((setter ...) setters))
           #'(fields-with-accessors-helper (accessor setter field init) ...)))]))
  
  ; for accessors
  (define-syntax fields-with-accessors-helper
    (syntax-rules ()
      [(_ (accessor setter field init) ...)
       (begin 
         (init-field (field init)) ...
         (define (accessor) field) ...
         (define (setter val) (set! field val)) ...) ]))
  

  ; base class for a plot view
  ; 
  (define plot-view%
    (class* image-snip% ()
      (public 
        set-line-color
        set-line-width                       
        set-plot-environment
        reset-to-default
        get-x-min
        get-x-max
        get-y-min
        get-y-max
        
        get-x-label
        get-y-label
        get-title 
        start-plot 
        finish-plot
        
        get-renderer
        
        get-height
        get-width)
      
      (init-field
       renderer)
      
      (fields-with-accessors
       (height 300)
       (width 400)
       (x-min -5)
       (x-max 5)
       (y-min -5)
       (y-max 5)
       (x-label "X axis")
       (y-label "Y axis")
       (title "")
       (device 'png)
       (fgcolor '( 0 0 0))
       (bgcolor '(255 255 255))
       (lncolor '(255 0 0 ))
       )   
     
      (define bitmap #f)
      (define x-size 400)
      (define y-size 300)
     
      (inherit 
        set-bitmap
        load-file)
      
      (define (get-renderer) renderer)
      
      ; set the initial environment 
      (define (set-plot-environment x-min x-max y-min y-max just other)
        (pl-set-plot-environment x-min x-max y-min y-max just other))
      
      ; changes the *initial* colormap to match the colors
      ; this should probably be done dynamically
      (define (init-colors)
        (apply pl-set-colormap0-index 0 bgcolor) ; 0 to white
        (apply pl-set-colormap0-index 1 fgcolor) ; 1 to black
        (apply pl-set-colormap0-index 15 lncolor)) ; 15 to red
      
      ; these are the colors to whitch the plot will be initialzed
      (define colors '((white 0) (black 1) (yellow 2) (green 3) 
                       (aqua 4) (pink 5) (wheat 6) (grey 7) 
                       (brown 8) (blue 9) (violet 10) (cyan 11)
                       (turquoise 12) (magenta 13) (salmon 14) (red 15)))
      
      ; set-line-width : number -> nothing
      (define (set-line-width width) (pl-set-line-width width))
      
      ; reset-to-default : void
      ; resets some of the state to default
      (define (reset-to-default) 
        (init-colors)
        (set-line-color 'black)
        (set-line-width 0))
                                        
      ;set-line-color : symbol -> nothing
      (define (set-line-color color)
        (let ((index (cond [(assq color colors ) => cadr]
                           [else (error (string-append "color \"" color "\" not found"))])))
          (pl-select-colormap0-index index)))
      
      ; start the plot
      ; does housekeeping/setup for plplot
      (define (start-plot)
        (cond
          [(eq? device 'png)
           (set! bitmap (make-temporary-file))
           (init-colors)
           (pl-setup-page  width height)
           (pl-set-device "png")
           (pl-set-output-file (path->string bitmap))
           (pl-init-plot)]
;          [(eq? device 'mem)
;           (init-colors)
;           (set! bitmap (make-u8vector (* x-size y-size 4) 255))
;           (pl-setup-memory x-size y-size bitmap)
;           (pl-set-device "mem")
;           (pl-init-plot)]
          [else
           (error "Incorrect device specified")]))

      ; finish the plot.. loads the file
      (define (finish-plot)
        (cond
          [(eq? device 'png)
           (pl-finish-plot)
           (load-file bitmap)
           (delete-file bitmap)]
;          [(eq? device 'mem)
;           (pl-finish-plot)
;           (set-bitmap (bits->bitmap-dc% bitmap))]
          [else
           (error "Incorrect device specified")]))
      
      ;(define (bits->bitmap-dc% bitmap)
      ;  (let ((bmdc (instantiate bitmap-dc% () (bitmap (make-object bitmap% x-size y-size #f))))
      ;        (result-string (u8vec->scheme-string bitmap)))
      ;    (send bmdc set-argb-pixels 0 0  x-size y-size result-string)
      ;    (begin0
      ;      (send bmdc get-bitmap)
      ;      (send bmdc set-bitmap #f))))
           
      (super-instantiate ())))
        
  ;; a 2d plot view
  (define 2d-view% 
    (class* plot-view% ()
      (public 
        set-labels 
        plot-y-errors
        plot-vector
        plot-vectors      
        plot-points
        plot-line
        plot-contours
        plot-shades
        fill
                )
      
      ; set-labels : string string string -> nothing
      ; sets the x, y and title lables
      (define (set-labels x-label y-label title)
        (pl-set-labels x-label y-label title))

      ; plot-contours: listoflistof number, listof-number, listof-number, listof-number
      (define (plot-contours z x-vals y-vals levels)
        (pl-2d-contour-plot z x-vals y-vals levels))
      
      ; plot-shades: listoflistof number, listof-number, listof-number, listof-number
      (define (plot-shades z x-vals y-vals levels)
        (pl-2d-shade-plot z x-vals y-vals levels))
  
      ; plot-line : (listof vector) -> void
      ; plots a line with the given points
      (define (plot-line points)        
        (pl-plot-line (length points) 
                      (map vector-x points)
                      (map vector-y points)))
      
      ; plot-points : (listof vector) -> void
      ; plots given points with a . symbol
      (define (plot-points points sym)
        (pl-plot-points (length points) 
                        (map vector-x points)
                        (map vector-y points)
                        sym))

      (define v-head-ratio 1/4) ; size of the vector head
      (define rot (* 5 pi 1/6))
      
      ; plot-vectors: (listof (list vector vector)) - > void
      (define (plot-vectors from delta)
        (for-each (lambda (f d) (send this plot-vector f d)) from delta))
      
      ; plot-vector : vector vector -> nothing
      (define (plot-vector from delta)
        (unless (= 0 (vector-magnitude delta))
          (let* ((x (vector-x from)) (x2 (+ x (vector-x delta)))
                 (y (vector-y from)) (y2 (+ y (vector-y delta)))
                 (ang (atan  (vector-y delta) (vector-x delta)))
                 (len (vector-magnitude delta))
                 (x3 (+ x2 (* len v-head-ratio (cos (+ ang rot))))) 
                 (x4 (+ x2 (* len v-head-ratio (cos (- ang rot)))))
                 (y3 (+ y2 (* len v-head-ratio (sin (+ ang rot))))) 
                 (y4 (+ y2 (* len v-head-ratio (sin (- ang rot))))))         
            (plot-line (list from 
                             (vector x2 y2) 
                             (vector x3 y3) 
                             (vector x4 y4) 
                             (vector x2 y2))))))
      
      ; fill : (list-of number) (list-of number) -> void
      (define (fill xs ys)
        (pl-fill (length xs) xs ys))
      
      ; plot-y-errors (listof (vector x y-min y-max)) ->nothing
      ; plots y error bars given a vector containing the x y and z (error magnitude) points
      (define (plot-y-errors errlist)
        (pl-y-error-bars (length errlist) (map vector-x errlist) (map vector-y errlist) (map vector-z errlist)))
      
      
      
      (inherit start-plot set-plot-environment finish-plot 
               get-x-min get-x-max get-y-min get-y-max get-renderer 
               get-x-label get-y-label get-title)
      (define (plot)
        (start-plot)
        (set-plot-environment (get-x-min) (get-x-max) (get-y-min) (get-y-max) 0 1)
        (set-labels (get-x-label) (get-y-label) (get-title))
        (with-handlers ((exn? (lambda (ex) (finish-plot) (raise ex))))
          ((get-renderer) this))
        (finish-plot)
        this)

      (super-instantiate ())
      (plot)))
  
  ; 3d view
  ; for making meshes and stuff
  
  (define 3d-view%
    (class* plot-view% ()
      (public         
        plot-polygon        
        plot-line
        plot-surface
        plot-3dmesh
        get-z-min
        get-z-max
        get-alt
        get-az)
      
      
      (fields-with-accessors      
       (z-min -5) 
       (z-max 5) 
       (alt 30) 
       (az 45)
       (z-label "Z-Axis"))
      
      ; define the 3d world
      (define (world3d x y z xmin xmax ymin ymax zmin zmax alt az)
        (pl-world-3d x y z xmin xmax ymin ymax zmin zmax alt az))
      
      ; set up the axies box
      (define (box3d 
               xopts xlabel xticks nxsub
               yopts ylabel yticks nysub
               zopts zlabel zticks nzsub)
        (pl-box3 xopts xlabel xticks nxsub
                 yopts ylabel yticks nysub
                 zopts zlabel zticks nzsub))
      
      ; draw a simple 3d surface plot
      (define (plot-surface x y z)
        (pl-plot3d x y z))
      
      ; plot-3dmesh
      (define (plot-3dmesh x y z lines? colored? contours? sides? levels)
        (pl-mesh3dc x y z lines? colored? contours? sides? levels))
      
      (inherit start-plot set-plot-environment finish-plot get-x-min 
               get-x-max get-y-min get-y-max get-renderer get-x-label get-y-label)
      (define (plot)
        (start-plot)
        (set-plot-environment -1 1 -1 1 0 -2)
        (world3d 1 1 1 (get-x-min) (get-x-max) (get-y-min) (get-y-max) z-min z-max alt az)
        (box3d 
         "bnstu" (get-x-label) 0 0
         "bnstu" (get-y-label) 0 0
         "bnstu" z-label 0 0)
        (with-handlers ((exn? (lambda (ex) (finish-plot) (raise ex))))
          ((get-renderer) this))
        (finish-plot)
        this)
      
      ; plot a polygon in 3 space
      (define (plot-polygon x y z draw ifc)
        (pl-poly3  x y z draw ifc))
      
      
      ; plot a line in 3 space
      ; x y and z are lists of equal length
      (define (plot-line x y z)
        (pl-line3 x y z))
      
      
      (super-instantiate ())
      (plot)))
 
  (provide
   2d-view%
   3d-view%))
