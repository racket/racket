#lang frtime
(require frtime/animation
         frtime/gui)

;; Written by Evan Perillo
;; never run two display-envs at the same time or the images will overlap and not look nice
;; any relevant values can be set using the gui
;; 4 types of optical-devices can be used, concave-mirror, convex-mirror, concave-lens, and convex-lens 
;; An example is already written at the bottom, just click run.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TOP LEVEL FUNCTIONS;;;;;;;;


;creates and displays an environment with an optical device
;display-env : slider-input -> animation
(define (display-env)
  (fresh-anim)
  (fresh-anim 599 599)
  (display-shapes (set-env (make-choice "Type of device"
                                        (list "Concave Mirror" "Convex Lens" "Convex Mirror" "Concave Lens")) 
                           (make-slider "Device Position" 1 599 399) 
                           mouse-pos 
                           (make-slider "Height" 1 100 50) 
                           (make-slider "Focal Length" 1 110 60) 
                           "blue")))


(define (concave-mirror x-val)   ;; makes a concave mirror at x,  y value determined by Base-Line
  (make-object "Concave Mirror"  ;; gives a name to the optical device
    x-val             ;; sets it at x value indicated
    599               ;; determines if it converges or diverges
    (lambda (h ref-h) (make-posn (posn-x mouse-pos) ref-h)) ;; stores information regarding how the light reflects off of this object in a lambda function
    (lambda (f d-h) (make-posn f Base-Line))
    -1                ;; determines whether this is a mirror or a Lens, ie. whether focus is negative or positive x value 
    ))

(define (convex-lens x-val)
  (make-object "Convex Lens" x-val 599 
    (lambda (h ref-h) (make-posn (posn-x mouse-pos) h))
    (lambda (f d-h) (make-posn f Base-Line))
    1))

(define (convex-mirror x-val)
  (make-object "Convex Mirror" x-val 0 
    (lambda (h ref-h) (make-posn (posn-x mouse-pos) ref-h))
    (lambda (f d-h) (make-posn f d-h))
    -1))

(define (concave-lens x-val)
  (make-object "Concave Lens" x-val 0 
    (lambda (h ref-h) (make-posn (posn-x mouse-pos) h))
    (lambda (f d-h) (make-posn f d-h))
    1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MID LEVEL FUNCTIONS;;;;;;;;


;set-env: number(Op-d type) number(Op-D x value) mouse-input number(object height) number(focal-length) string -> void (animation)         
(define (set-env num x obj-pos obj-height f-length object-color)
  (let* ([obj (get-object num x)]
         [x-pos (posn-x obj-pos)]
         [x-val (object-x-val obj)]
         [y-val (object-y-val obj)]
         [Focal-Length f-length]
         [F (+ x-val (* (object-focus obj) Focal-Length))] 
         [2F (- F (- x-val F))]
         [Fl (- x-val Focal-Length)]
         [2Fl (- Fl Focal-Length)]
         [Object-Height obj-height]
         [Height (- Base-Line Object-Height)]
         [Reflected-Height (+ Base-Line Object-Height)]
         [Double-Height (- Base-Line (* 2 Object-Height))]
         [Object-Distance (- x-val x-pos)] 
         [Ray1-Posn ((object-ray1 obj) Height Reflected-Height)]
         [Ray2-Posn ((object-ray2 obj) F Double-Height)])
    
    (define (in-bounds proc result)
      (if (< x-pos x-val)
          proc
          result)) 
    
    (define (create-line proc)
      (in-bounds proc empty))
    
    (define (return-string proc)
      (in-bounds proc "None"))
    
    
    (define (show-info proc)
      (if (>= x-pos x-val)
          proc
          ""))
    
    (define (get-image-type)
      (cond
        [(string=? (object-type obj) "Convex Mirror") (return-string "Virtual Erect Smaller")]
        [(string=? (object-type obj) "Concave Lens") (return-string "Virtual Erect Smaller")]
        [(string=? (object-type obj) "Concave Mirror") (get-image-type-converging 2F F)]
        [(string=? (object-type obj) "Convex Lens") (get-image-type-converging 2Fl Fl)]))
    
    (define (get-image-type-converging a b)
      (return-string
       (cond 
         [(< (posn-x mouse-pos) a) "Real Inverted Smaller"]
         [(= (posn-x mouse-pos) a) "Real Inverted Equal"]
         [(and (> (posn-x mouse-pos) a) 
               (< (posn-x mouse-pos) b)) "Real Inverted Larger"]
         [(= (posn-x mouse-pos) b) "None"]
         [(and (> (posn-x mouse-pos) b)) "Virtual Erect Larger"])))
    
;;;;;;;;list of shapes to be drawn;
    (append
     (list
      (make-line (make-posn x-val 175) (make-posn x-val 375) "black") ;optical device
      
      (create-line (make-line (make-posn x-pos Base-Line)             ;object
                              (make-posn x-pos Height) object-color)) ;object
      
      (create-line (make-line (make-posn x-pos Height)                ;incident ray 1
                              (make-posn x-val Base-Line) "green"))   ;incident ray 1
      
      (if (= x-pos x-val) null                                                                        ;reflected ray 1 
          (create-line (make-line (make-posn x-val Base-Line)                                         ;reflected ray 1
                                  (get-end-posn Ray1-Posn (make-posn x-val Base-Line) 599) "green"))) ;reflected ray 1
      
      (create-line (make-line (make-posn x-pos Height)                ;incident ray 2
                              (make-posn x-val Height) "red"))        ;incident ray 2
      
      (create-line (make-line (make-posn x-val Height)                                        ;reflected ray 2
                              (get-end-posn Ray2-Posn (make-posn x-val Height) y-val) "red")) ;reflected ray 2 
      

      
      (make-circle (make-posn F Base-Line) 2 "black")   ;
      (make-circle (make-posn 2F Base-Line) 2 "black")  ; Focal Points
      (make-circle (make-posn Fl Base-Line) 2 "black")  ; 
      (make-circle (make-posn 2Fl Base-Line) 2 "black") ;  
      
      (make-graph-string (make-posn 20 20)
                         (format "Type of Image Created: ~a"
                                 (get-image-type))
                         "black")
      (make-graph-string (make-posn 20 40)
                         (format "ho: ~a    do: ~a"
                                 (return-string Object-Height)
                                 (return-string Object-Distance))
                         object-color)
      (make-graph-string (make-posn 20 60)
                         (format "~a     ~a"
                                 (show-info "hi: None")
                                 (show-info "di: None"))
                         "black")
      (make-graph-string (make-posn 20 80)
                         (format "~a"
                                 (show-info "Magnification: None"))
                         "red")
      (make-graph-string (make-posn 450 20)
                         (object-type obj)
                         "black")
      (make-graph-string (make-posn 450 40)
                         "By: Evan Perillo"
                         "black"))
     
  ;;;;;;Reflected Object;;;;;;;;;;
        (if (>= x-pos x-val)
          (list null)         
           (let ((intersect  (intersection Ray2-Posn (make-posn x-val Height) Ray1-Posn (make-posn x-val Base-Line))))
            (if (string? intersect) (create-line (list null))
                (create-line 
                 (list
                  (make-line intersect (make-posn (posn-x intersect) Base-Line) "black")
                  (make-graph-string (make-posn 20 60)
                                     (format "hi: ~a    di: ~a"
                                             (exact->inexact (- Base-Line (posn-y intersect))) 
                                             (exact->inexact (- x-val (posn-x intersect))))
                                     "black")
                  (make-graph-string (make-posn 20 80)
                                     (format "Magnification: ~a"
                                             (/ (- Base-Line (posn-y intersect))
                                                Object-Height))
                                     "red"))))))
        )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOTTOM LEVEL FUNCTIONS ;;;

(define Base-Line 275)

(define-struct object (type x-val y-val ray1 ray2 focus))

(define (get-object num x)
  (cond [(= 0 num) (concave-mirror x)]
        [(= 1 num) (convex-lens x)]
        [(= 2 num) (convex-mirror x)]
        [(= 3 num) (concave-lens x)]))

(define (solve-x m1 m2 b1 b2)
  (if (= m1 m2)
      "DNE"
      (/ (- b2 b1) (- m1 m2))))

(define (solve-eq x m b)
  (if (string? x)
      "DNE"
      (+ (* m x) b)))

(define (get-b y x m)
  (- y (* x m)))

(define (get-x y m b)
  (/ (- y b) m))

(define (get-slope a b)
  (/ (- (posn-y b) (posn-y a))
     (- (posn-x b) (posn-x a))))

(define (get-end-posn posn-a posn-b y-val)
  
  (define (get-end-x posn-a posn-b)
    (let ((slope (get-slope posn-a posn-b)))
      (get-x y-val
             slope
             (get-b (posn-y posn-a) 
                    (posn-x posn-a) 
                    slope))))
  (make-posn (get-end-x posn-a posn-b) y-val))

  ;intersection : posn posn posn posn -> posn or "DNE"
  (define (intersection line1a line1b line2a line2b)   
    (let* ((m1 (get-slope line1a line1b))
                 (m2 (get-slope line2a line2b))
                 (b1 (get-b (posn-y line1a) (posn-x line1a) m1))
                 (b2 (get-b (posn-y line2a) (posn-x line2a) m2))
                 (x (solve-x m1 m2 b1 b2))
                 (y (solve-eq x m1 b1)))
      (if (string? x) "DNE" (make-posn x y))))

;;;;;;;;;;;;;;;;Examples;;;;;;;;;;;;;;

(display-env) ;creates and displays an optical device environment
