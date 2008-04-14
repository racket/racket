#lang scheme/base

(require (prefix-in orig: 
                    (only-in "plot.ss"
                             plot plot3d
                             points line error-bars
                             vector-field contour
                             shade surface
                             mix fit
                             derivative gradient make-vec)))

(provide plot
         plot3d
         plot-color?
         points
         line
         error-bars
         vector-field
         contour
         shade
         surface
         (rename-out [orig:mix mix]
                     [orig:fit fit]
                     [orig:derivative derivative]
                     [orig:gradient gradient]
                     [orig:make-vec make-vec]))

(define-syntax-rule (out-fit-struct)
  (begin
    (require "plot.ss")
    (provide (struct-out fit-result))))

(out-fit-struct)

(define-syntax-rule (define-plot plot orig:plot 
                      [arg-extra ...]
                      [init-extra ...])
  (define (plot data
                #:width [width 400]
                #:height [height 400]
                #:x-min [x-min -5]
                #:x-max [x-max 5]
                #:y-min [y-min -5]
                #:y-max [y-max 5]
                #:x-label [x-label "X axis"]
                #:y-label [y-label "Y axis"]
                #:title [title ""]
                #:fgcolor [fgcolor '(0 0 0)]
                #:bgcolor [bgcolor '(255 255 255)]
                #:lncolor [lncolor '(255 0 0)]
                arg-extra ...)
    (orig:plot data 
               [width width]
               [height height]
               [x-min x-min]
               [x-max x-max]
               [y-min y-min]
               [y-max y-max]
               [x-label x-label]
               [y-label y-label]
               [title title]
               [fgcolor fgcolor]
               [bgcolor bgcolor]
               [lncolor lncolor]
               init-extra ...)))

(define-plot plot orig:plot [] [])

(define-plot plot3d orig:plot3d
  [#:z-min [z-min -5]
           #:z-max [z-max 5]
           #:z-label [z-label "Z axis"]
           #:alt [alt 30]
           #:az [az 45]]
  [[z-min z-min]
   [z-max z-max]
   [z-label z-label]
   [alt alt]
   [az az]])

(define (plot-color? v)
  (memq v '(white black yellow green aqua pink
                  wheat grey blown blue violet cyan
                  turquoise magenta salmon red)))

(define (points vecs 
                #:sym [sym 'square]
                #:color [color 'black])
  (orig:points vecs [sym sym] [color color]))

(define (line f
              #:samples [samples 150]
              #:width [width 1]
              #:color [color 'red]
              #:mode [mode 'standard]
              #:mapping [mapping 'cartesian]
              #:t-min [t-min -5]
              #:t-max [t-max 5])
  (orig:line f
             [samples samples]
             [width width]
             [color color]
             [mode mode]
             [mapping mapping]
             [t-min t-min]
             [t-max t-max]))

(define (error-bars vec
                    #:color [color 'black])
  (orig:error-bars vec [color color]))

(define (vector-field f
                      #:width [width 1]
                      #:color [color 'red]
                      #:style [style 'scaled])
  (orig:vector-field f
                     [width width]
                     [color color]
                     [style style]))

(define (contour f
                 #:samples [samples 50]
                 #:width [width 1]
                 #:color [color 'black]
                 #:levels [levels 10])
  (orig:contour f
                [samples samples]
                [width width]
                [color color]
                [levels levels]))

(define (shade f
               #:samples [samples 50]
               #:levels [levels 10])
  (orig:shade f
              [samples samples]
              [levels levels]))

(define (surface f
                 #:samples [samples 50]
                 #:width [width 1]
                 #:color [color 'black])
  (orig:surface f
                [samples samples]
                [width width]
                [color color]))

