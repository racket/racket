#lang racket/base
(require mzlib/etc 
         racket/list 
         ffi/unsafe
         racket/runtime-path
         racket/class
         (for-syntax racket/base))

(define-runtime-path plplot-path '(so "libplplot"))
(define-runtime-path font-dir "fonts")

(define libplplot (ffi-lib plplot-path))

(define plplotlibdir (get-ffi-obj "plplotLibDir" libplplot _string))

;; set the lib dir to contain the fonts:
(let ([path font-dir])
  ;; free current pointer, if any:
  (let ([p (get-ffi-obj "plplotLibDir" libplplot _pointer)])
    (when p (free p)))
  ;; install new value:
  (set-ffi-obj! "plplotLibDir" libplplot _bytes
		;; malloc the string, since the GC won't see the static variable:
		(let* ([gced-bytes (path->bytes path)]
		       [len (bytes-length gced-bytes)]
		       [p (malloc (add1 len) 'raw)]
		       [malloced-bytes (make-sized-byte-string p len)])
		  (bytes-copy! malloced-bytes 0 gced-bytes)
		  ;; set nul terminator:
		  (ptr-set! p _byte len 0)
		  malloced-bytes)))

(define-cstruct _dc_Dev
  ([user_data _pointer]
   [drawLine _fpointer]
   [drawLines _fpointer]
   [fillPoly _fpointer]
   [setWidth _fpointer]
   [setColor _fpointer]
   [setColorRGB _fpointer]
   [startPage _fpointer]
   [endPage _fpointer]
   [endDoc _fpointer]))

(define _PLINT _int)

(define _plflt _double*)
(define _plint _int)

(define (_list-of type . len?)
  (let ([len (and (pair? len?) (car len?))])
    (make-ctype _pointer
      (lambda (l) (list->cblock l type))
      (if len
        (lambda (b) (cblock->list b type len))
        (lambda (b) (error "this list type does not specify a size"))))))

(define (_matrix-of type)
  (_list-of (_list-of type)))

(define-syntax define*
  (syntax-rules ()
    [(_ (name . args) body ...)
     (begin (provide name) (define (name . args) body ...))]
    [(_ name expr)
     (begin (provide name) (define name expr))]))

(define* pl-setup-page
  (get-ffi-obj "c_plspage" libplplot
               (_fun (xp : _plflt = 0.0)
                     (yp : _plflt = 0.0)
                     (xleng : _plint)
                     (yleng : _plint)
                     (xoff : _plint = 0)
                     (yoff : _plint = 0)
                     -> _void)))

(define* pl-set-device
  (get-ffi-obj "c_plsdev" libplplot (_fun _string -> _void)))

(define* pl-set-output-file
  (get-ffi-obj "c_plsfnam" libplplot (_fun _string -> _void)))

(define* pl-init-plot
  (get-ffi-obj "c_plinit" libplplot (_fun -> _dc_Dev-pointer)))

(define* pl-finish-plot
  (get-ffi-obj "c_plend" libplplot (_fun -> _void)))

(define* pl-set-plot-environment
  (get-ffi-obj "c_plenv" libplplot
    (_fun _plflt _plflt _plflt _plflt _plint _plint -> _void)))

(define* pl-set-labels
  (get-ffi-obj "c_pllab" libplplot
    (_fun _string _string _string -> _void)))

(define* pl-plot-line
  (get-ffi-obj "c_plline" libplplot
    (_fun _plint (x : (_list i _plflt)) (y : (_list i _plflt)) -> _void)))

(define* pl-plot-segment
  (get-ffi-obj "c_pljoin" libplplot
    (_fun _plflt _plflt _plflt _plflt -> _void)))

(define* pl-set-background-color
  (get-ffi-obj "c_plscolbg" libplplot
    (_fun _plint _plint _plint -> _void)))


(define* pl-select-colormap0-index
  (get-ffi-obj "c_plcol0" libplplot
    (_fun _plint -> _void)))

(define* pl-set-colormap0-index
  (get-ffi-obj "c_plscol0" libplplot
    (_fun _plint _plint _plint _plint -> _void)))

(define* pl-set-line-width
  (get-ffi-obj "c_plwid" libplplot
    (_fun _plint -> _void)))

(define* pl-write-text
  (get-ffi-obj "c_plptex" libplplot
               (_fun _plflt _plflt _plflt _plflt _plflt _string -> _void)))

;;(define* pl-2d-countour-plot ...)
;;(define* pl-2d-shade-plot ...)

(define* pl-x-error-bars
  (get-ffi-obj "c_plerrx" libplplot
    (_fun _plint (_list i _plflt)
          (_list i _plflt)
          (_list i _plflt) -> _void)))

(define* pl-y-error-bars
  (get-ffi-obj "c_plerry" libplplot
    (_fun _plint (_list i _plflt)
          (_list i _plflt)
          (_list i _plflt) -> _void)))

(define* pl-plot-points
  (get-ffi-obj "c_plpoin" libplplot
    (_fun _plint (x : (_list i _plflt)) (y : (_list i _plflt)) _plint
          -> _void)))

(define* pl-fill
  (get-ffi-obj "c_plfill" libplplot
   (_fun  (n         : _plint = (length x-values))
          (x-values  : (_list i _plflt))
          (y-values  : (_list i _plflt))
          -> _void)))

(define* pl-world-3d
  (get-ffi-obj "c_plw3d" libplplot
    (_fun
     _plflt _plflt _plflt _plflt _plflt _plflt _plflt _plflt _plflt _plflt _plflt
     ->
     _void)))

;; bit-masks for some of the functions..
(define-values (DRAW_LINEX DRAW_LINEY MAG_COLOR BASE_CONT TOP_CONT SURF_CONT DRAW_SIDES DRAW_FACETED MESH)
  (apply values (build-list 9 (lambda (s) (arithmetic-shift 1 s)))))

(define DRAW_LINEXY (bitwise-ior DRAW_LINEX DRAW_LINEY))


(define* pl-plot3d
  (get-ffi-obj "c_plot3d" libplplot
   (_fun
    (x-values  : (_list i _plflt))
    (y-values  : (_list i _plflt))
    (z-values  : (_matrix-of _plflt))
    (nx        : _int = (length x-values))
    (ny        : _int = (length y-values))
    (draw-opt1 : _int = DRAW_LINEXY)
    (draw-opt2 : _int = 0)
    -> _void))) ;; these are documented in the plplot ref manual, and will be obseleted.

(define* pl-mesh3d
  (get-ffi-obj "c_plot3d" libplplot
   (_fun
    (x-values  : (_list i _plflt))
    (y-values  : (_list i _plflt))
    (z-values  : (_matrix-of _plflt))
    (nx        : _int = (length x-values))
    (ny        : _int = (length y-values))
    (draw-opt1 : _int = DRAW_LINEXY)
    -> _void)))

; ;; this function needs to go.
; (define* pl-plot-points
;   (get-ffi-obj "c_plpoin" libplplot
;    (_fun
;     (nx        : _int = (length x-values))
;     (x-values  : (_list i _plflt))
;     (y-values  : (_list i _plflt))
;     (code      : _int))))

(define* pl-box3
  (get-ffi-obj "c_plbox3" libplplot
   (_fun
    (x-ops     : _string) (x-title   : _string) (x-spacing : _plflt) (x-ticks   : _int)
    (y-ops     : _string) (y-title   : _string) (y-spacing : _plflt) (y-ticks   : _int)
    (z-ops     : _string) (z-title   : _string) (z-spacing : _plflt) (z-ticks   : _int)
    -> _void)))

(define* pl-line3
  (get-ffi-obj "c_plline3" libplplot
   (_fun
    (n-points : _int = (length x-values))
    (x-values  : (_list i _plflt))
    (y-values  : (_list i _plflt))
    (z-values  : (_list i _plflt))
    -> _void)))


(define* pl-poly3
  (get-ffi-obj "c_plpoly3" libplplot
   (_fun
    (n-points : _int = (length x-values))
    (x-values  : (_list i _plflt))
    (y-values  : (_list i _plflt))
    (z-values  : (_list i _plflt))
    (draw-mask : (_list i _int))
    (direction : _int)
    -> _void)))

;; need the CStruct PLcGrid ;
;;    PLFLT *xg, *yg, *zg;
;;    PLINT nx, ny, nz;
(define-cstruct _PLcGrid ((xg _pointer)
                          (yg _pointer)
                          (zg _pointer)
                          (nx _int)
                          (ny _int)
                          (nz _int)))

(define pl-2d-contour-plot-int
  (get-ffi-obj "c_plcont" libplplot
   (_fun
    (matrix     : (_matrix-of _plflt))
    (nx         : _int = (PLcGrid-nx grid))
    (ny         : _int = (PLcGrid-ny grid))
    (t1         : _plint = 1)
    (t2         : _int = (PLcGrid-nx grid))
    (t3         : _plint = 1)
    (t4         : _int = (PLcGrid-ny grid))
    (levels     : (_list i _plflt))
    (nlevels    : _int = (length levels))
    (pltr       : _fpointer = (get-ffi-obj "pltr1" libplplot _fpointer))
    (grid       : _PLcGrid-pointer)
    -> _void)))

(define* (pl-2d-contour-plot z-vals x-vals y-vals levels)
  (let ((grid-obj (make-PLcGrid (list->cblock x-vals _plflt) (list->cblock y-vals _plflt) #f
                                (length x-vals) (length y-vals) 0)))
    (pl-2d-contour-plot-int z-vals levels grid-obj)))



(define pl-2d-shade-plot-int
  (get-ffi-obj "c_plshades" libplplot
   (_fun
    (matrix     : (_matrix-of _plflt))
    (nx         : _int = (PLcGrid-nx grid))
    (ny         : _int = (PLcGrid-ny grid))
    (null-val   : _pointer = #f)
    (x_min      : _plflt = 0)
    (x_max      : _plflt = 0)
    (y_min      : _plflt = 0)
    (y_max      : _plflt = 0)
    (levels     : (_list i _plflt))
    (nlevels    : _int = (length levels))
    (fill_width : _int = 1)
    (cont_col   : _int = 1)
    (cont_width : _int = 0)
    (fill_fun   : _fpointer = (get-ffi-obj "c_plfill" libplplot _fpointer))
    (rectan     : _int = 1)
    (pltr       : _fpointer = (get-ffi-obj "pltr1" libplplot _fpointer))
    (grid       : _PLcGrid-pointer)
    -> _void)))

(define* (pl-2d-shade-plot z-vals x-vals y-vals levels)
  ;; this can prolly be inlined above..
  (let ((grid-obj (make-PLcGrid (list->cblock x-vals _plflt) (list->cblock y-vals _plflt) #f
                                 (length x-vals) (length y-vals) 0)))
    (pl-2d-shade-plot-int z-vals levels grid-obj)))


;; set up color map numbers
(define plscmap1n
  (get-ffi-obj "c_plscmap1n" libplplot
    (_fun _int -> _void)))

;; set up the map
(define plscmap1l
  (get-ffi-obj "c_plscmap1l" libplplot
    (_fun
     (itype     : _plint)
     (npts      : _int = (length intencity))
     (intencity : (_list i _plflt))
     (coord1    : (_list i _plflt))
     (coord2    : (_list i _plflt))
     (coord3    : (_list i _plflt))
     (rev       : _pointer = #f)
     -> _void)))

(define pl-mesh3dc-int 
  (get-ffi-obj "c_plmeshc" libplplot
    (_fun
     (x-values  : (_list i _plflt))
     (y-values  : (_list i _plflt))
     (z-values  : (_matrix-of _plflt))
     (x-len     : _int = (length x-values))
     (y-len     : _int = (length y-values))
     (opts      : _int)
     (levels    : (_list i _plflt))
     (n-levels  : _int = (length levels))
     -> _void)))

(define* (pl-mesh3dc x-vals y-vals z-vals contours? lines? colored? sides? levels)
  (let ((opts (foldl
               (lambda (mask use? current) (bitwise-ior current (if use? mask 0)))
               0
               (list DRAW_LINEXY MAG_COLOR BASE_CONT DRAW_SIDES)
               (list contours? lines? colored? sides?))))
    (plscmap1n 256)
    (plscmap1l 0 '(0.0 1.0) '(240 0) '(.6 .6) '(.8 .8))
    (pl-mesh3dc-int x-vals y-vals z-vals opts levels)))

(define (dc-draw-line dest x1 y1 x2 y2)
  (send (ptr-ref dest _racket) draw-line x1 y1 x2 y2))
(define (dc-draw-multi dest xs ys n go)
  (let ([xs (cast xs _pointer (_vector o _short n))]
        [ys (cast ys _pointer (_vector o _short n))])
    (go (ptr-ref dest _racket)
          (for/list ([x (in-vector xs)]
                     [y (in-vector ys)])
            (cons x y)))))
(define (dc-draw-lines dest xs ys n)
  (dc-draw-multi dest xs ys n
                 (lambda (dc l) (send dc draw-lines l))))
(define (dc-fill-poly dest xs ys n)
  (dc-draw-multi dest xs ys n
                 (lambda (dc l) (send dc draw-polygon l))))
(define (dc-set-width dest w)
  (send (ptr-ref dest _racket) set-width w))
(define (dc-set-color dest index)
  (send (ptr-ref dest _racket) set-index-color index))
(define (dc-set-color/rgb dest r g b)
  (send (ptr-ref dest _racket) set-rgb-color r g b))
(define (dc-start-page dest)
  (send (ptr-ref dest _racket) start-page))
(define (dc-end-page dest)
  (send (ptr-ref dest _racket) end-page))
(define (dc-end-doc dest)
  (send (ptr-ref dest _racket) end-doc)
  (free-immobile-cell dest))

(define draw_line (function-ptr dc-draw-line
                                (_fun _pointer _short _short _short _short -> _void)))
(define draw_lines (function-ptr dc-draw-lines
                                 (_fun _pointer _pointer _pointer _PLINT -> _void)))
(define fill_poly (function-ptr dc-fill-poly
                                (_fun _pointer _pointer _pointer _PLINT -> _void)))
(define set_width (function-ptr dc-set-width
                                (_fun _pointer _int -> _void)))
(define set_color (function-ptr dc-set-color
                                (_fun _pointer _int -> _void)))
(define set_color_rgb (function-ptr dc-set-color/rgb
                                    (_fun _pointer _int _int _int -> _void)))
(define start_page (function-ptr dc-start-page
                                 (_fun _pointer -> _void)))
(define end_page (function-ptr dc-end-page
                               (_fun _pointer -> _void)))
(define end_doc (function-ptr dc-end-doc
                              (_fun _pointer -> _void)))

(provide init-dev!)
(define (init-dev! dev obj)
  (set-dc_Dev-user_data! dev (malloc-immobile-cell obj))
  (set-dc_Dev-drawLine! dev draw_line)
  (set-dc_Dev-drawLines! dev draw_lines)
  (set-dc_Dev-fillPoly! dev fill_poly)
  (set-dc_Dev-setWidth! dev set_width)
  (set-dc_Dev-setColor! dev set_color)
  (set-dc_Dev-setColorRGB! dev set_color_rgb)
  (set-dc_Dev-startPage! dev start_page)
  (set-dc_Dev-endPage! dev end_page)
  (set-dc_Dev-endDoc! dev end_doc))
