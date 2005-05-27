(module renderers mzscheme
  (require 
   (lib "math.ss" "plot")
   (lib "renderer-helpers.ss" "plot")
   (lib "class.ss")
   (lib "plot-extend.ss" "plot"))
  
  ;line : (number -> number) [number] [symbol] [number] -> (2dplotview -> nothing)
  (define-plot-type line 
    func 2dplotview (x-min x-max) ((samples 150) (color 'red) 
                                             (width 1) 
                                             (mode 'standard) 
                                             (mapping 'cartesian)
                                             (t-min -5) (t-max 5))      
      (let*
          ((t-min (if (or (eq? mapping 'polar) (eq? mode 'parametric))
                      t-min x-min))
           (t-max (if (or (eq? mapping 'polar)(eq? mode 'parametric))
                      t-max x-max))  ; maybe let-values?
           (points
            (case mode
              ((standard) (map (lambda (x) (vector x (func x))) 
                               (x-values samples x-min x-max)))
              ((parametric) (map func (x-values samples t-min t-max))))))
        (send* 2dplotview 
          (set-line-color color) (set-line-width width)
          (plot-line 
           (case mapping
             ((cartesian) points)
             ((polar) (map 
                       (lambda (point)  ; convert to cartesian from theta, r
                         (vector 
                          (* (vector-y point) (cos (vector-x point)))
                          (* (vector-y point) (sin (vector-x point))))))))))))
  
    ; error-bars : (listof (vector x y err)) [symbol] -> (2dplotview -> nothing)
  (define-plot-type error-bars
                    errs 2dplotview () ((color 'red))
      (let* ((y-list (map vector-y errs))
             (e-list (map vector-z errs))
             (y-mins (map (lambda (y e) (- y e)) y-list e-list ))
             (y-maxs (map (lambda (y e) (+ y e)) y-list e-list )))              
        (send 2dplotview set-line-color color)
        (send 2dplotview plot-y-errors (map vector (map vector-x errs)
                                            y-mins y-maxs))))
  

  ; field : (vector -> vector) [number] [symbol] [number] [symbol] -> (2dplotview -> nothing)
  ; plots a vector field
  ; styles are 
  ; scaled -> vector field with scaled vectors
  ; normalized -> all vectors same size, indicates direction 
  ; real -> all vectors drawn to scale
  (define-plot-type vector-field
     vfun 2dplotview 
      (x-min x-max y-min y-max)
      ([samples 20] [color 'black] [width 1] [style 'scaled])
      (let* ((points (xy-list samples x-min x-max y-min y-max))
             (results (map vfun points))
             (new-results 
              (case style                  
                [(real) results]
                [(scaled) (scale-vectors results
                                         (sample-size samples x-min x-max)
                                         (sample-size samples y-min y-max))]
                [(normalized) (normalize-vectors
                               results
                               (sample-size samples x-min x-max)
                               (sample-size samples y-min y-max))]
                [else (error (string-append "Unknown vector field style passed to field-renderer: " (symbol->string style)))])))
        (send* 2dplotview  
          (set-line-color color) (set-line-width width)
          (plot-vectors points new-results))))

  ; contour : (nubmer number -> number) [number] [symbol] [number] [number u listof-number] ->  (2dplotview -> void)
  ; renders a contour plot given function and contour levels
  (define-plot-type contour
                    fun3d 2dplotview 
      (x-min x-max y-min y-max)
      ((samples 50) (color 'black) (width 1) (levels 10))
      (let* ((x-vals (x-values samples x-min x-max))
             (y-vals (x-values samples y-min y-max))
             (grid (zgrid fun3d x-vals y-vals samples))
             (z-max (apply max (map (lambda (row) (apply max row)) grid)))
             (z-min (apply min (map (lambda (row) (apply min row)) grid)))
             (c-levels (if (list? levels) levels (x-values levels z-min z-max))))
        (send* 2dplotview 
          (set-line-color color) (set-line-width width)
          (plot-contours grid x-vals y-vals c-levels))))
           
  ; shade : (number number -> number) [number] [symbol] [number] [number / listof-number] ->  (2dplotview -> nothing)
  ; renders a shade plot given function and shade levels    
  (define-plot-type shade
                    fun3d 2dplotview (x-min x-max y-min y-max)
                    ((samples 50) (levels 10))
      (let* ((x-vals (x-values samples x-min x-max))
             (y-vals (x-values samples y-min y-max))
             (grid (zgrid fun3d x-vals y-vals samples))
             (z-max (apply max (map (lambda (row) (apply max row)) grid)))
             (z-min (apply min (map (lambda (row) (apply min row)) grid)))
             (c-levels (x-values levels z-min z-max)))
        (send* 2dplotview 
          (plot-shades grid x-vals y-vals c-levels))))
           
  ; points : (listof vector) [symbol] -> (2dplotview -> nothing)
  ; plots a set of points using a specific character
  (define-plot-type points
                    lop 2dplotview ((sym 'square) (color 'black))
      (send* 2dplotview 
       (set-line-color color)
       (plot-points lop 
		    (cond [(assq sym point-syms) => cadr]
			  [else (error "Symbol not found in table!")]))))
  
  ; the symbol-> char table
  (define point-syms
    '((square 16) (odot 9) (bullet 17) (circle 4)))


	
       
       
  ;; 3D PLOTTERS
  ; plot a surface
  (define-plot-type surface 
     fun3d 3dplotview (x-min x-max y-min y-max)
     ((samples 50) (color 'black) (width '1))
      (let* ((x-vals (x-values samples x-min x-max))
             (y-vals (x-values samples y-min y-max))
             (grid (zgrid fun3d x-vals y-vals samples)))
          (send* 3dplotview 
            (set-line-color color) (set-line-width width) 
            (plot-surface x-vals y-vals grid))))
  
  (define-plot-type mesh3d
     fun3d 3dplotview (x-min x-max y-min y-max z-min z-max)
     ((samples 50) (width '1) (levels 10) (color #t) (lines #t)
      (contours #t) (sides #f))
      (let* ((x-vals (x-values samples x-min x-max))
             (y-vals (x-values samples y-min y-max))
             (grid (zgrid fun3d x-vals y-vals samples))
             (c-levels (if (list? levels) levels (x-values levels z-min z-max))))
          (send* 3dplotview 
            (set-line-width width) 
            (plot-3dmesh x-vals y-vals grid lines color contours sides c-levels))))
  
  (provide (all-defined)))

  
