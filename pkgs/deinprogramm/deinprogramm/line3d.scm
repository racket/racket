;; ###############################################
;; ###############################################
;;
;;       Mini-3D-Engine
;;
;;  3D-Object are represented with line primitives
;;  
;;                 Martin Bokeloh, Sebastian Veith
;; ###############################################
;; ###############################################


;; -----------------------------------
;; some linear algebra tools
;; -----------------------------------

;; 3D-vector
(define-record-procedures vec3
  make-vec3 vec3?
  (vec3-x
   vec3-y
   vec3-z))

;; return a+b
;; add-vec3 : vec3 vec3 -> vec3
(define add-vec3
  (lambda (a b)
    (make-vec3
     (+ (vec3-x a) (vec3-x b))
     (+ (vec3-y a) (vec3-y b))
     (+ (vec3-z a) (vec3-z b)))))

;; return a-b
;; sub-vec3 : vec3 vec3 -> vec3
(define sub-vec3
  (lambda (a b)
    (make-vec3
     (- (vec3-x a) (vec3-x b))
     (- (vec3-y a) (vec3-y b))
     (- (vec3-z a) (vec3-z b)))))

;; return v*s
;; mult-vec3 : vec3 number -> vec3
(define mult-vec3
  (lambda (v s)
    (make-vec3
     (* (vec3-x v) s)
     (* (vec3-y v) s)
     (* (vec3-z v) s))))

;; return v/s
;; div-vec3 : vec3 number -> vec3
(define div-vec3
  (lambda (v s)
    (mult-vec3 v (/ 1 s))))

;; return a*b
;; dotproduct-vec3 : vec3 vec3 -> Number
(define dotproduct-vec3
  (lambda (a b)
    (+
     (* (vec3-x a) (vec3-x b))
     (* (vec3-y a) (vec3-y b))
     (* (vec3-z a) (vec3-z b)))))

;; compute quadratic euclidian norm
;; normquad-vec3 : vec3 -> Number
(define normquad-vec3
  (lambda (a)
    (+
     (* (vec3-x a) (vec3-x a))
     (* (vec3-y a) (vec3-y a))
     (* (vec3-z a) (vec3-z a)))))

;; compute euclidian norm
;; norm-vec3 : vec3 -> Number
(define norm-vec3
  (lambda (a)
    (sqrt (normquad-vec3 a))))

;; normalize vector
;; normalize-vec3 : vec3 -> vec3
(define normalize-vec3
  (lambda (a)
    (div-vec3 a (norm-vec3 a))))

;; cross product (computes a vector perpendicular to both input vectors)
;; crossproduct-vec3 : vec3 vec3 -> vec3
(define crossproduct-vec3
  (lambda (a b)
    (make-vec3
     (- (* (vec3-y a) (vec3-z b)) (* (vec3-z a) (vec3-y b)))
     (- (* (vec3-z a) (vec3-x b)) (* (vec3-x a) (vec3-z b)))
     (- (* (vec3-x a) (vec3-y b)) (* (vec3-y a) (vec3-x b))))))

;; 4D-vector
(define-record-procedures vec4
  make-vec4 vec4?
  (vec4-x
   vec4-y
   vec4-z
   vec4-w))

;; expands a 3d-vector to a 4d-vector (v,s)
;; expand-vec3 : vec3 number -> vec4
(define expand-vec3
  (lambda (v s)
    (make-vec4 (vec3-x v) (vec3-y v) (vec3-z v) s)))

;; return a+b
;; add-vec4 : vec4 vec4 -> vec4
(define add-vec4
  (lambda (a b)
    (make-vec4
     (+ (vec4-x a) (vec4-x b))
     (+ (vec4-y a) (vec4-y b))
     (+ (vec4-z a) (vec4-z b))
     (+ (vec4-w a) (vec4-w b)))))

;; return a-b
;; sub-vec4 : vec4 vec4 -> vec4
(define sub-vec4
  (lambda (a b)
    (make-vec4
     (- (vec4-x a) (vec4-x b))
     (- (vec4-y a) (vec4-y b))
     (- (vec4-z a) (vec4-z b))
     (- (vec4-w a) (vec4-w b)))))

;; return v*s
;; mult-vec4 : vec4 number -> vec4
(define mult-vec4
  (lambda (v s)
    (make-vec4
     (* (vec4-x v) s)
     (* (vec4-y v) s)
     (* (vec4-z v) s)
     (* (vec4-w v) s))))

;; return v/s
;; div-vec4 : vec4 number -> vec4
(define div-vec4
  (lambda (v s)
    (mult-vec4 v (/ 1 s))))

;; return a*b
;; dotproduct-vec4 : vec4 vec4 -> Number
(define dotproduct-vec4
  (lambda (a b)
    (+
     (* (vec4-x a) (vec4-x b))
     (* (vec4-y a) (vec4-y b))
     (* (vec4-z a) (vec4-z b))
     (* (vec4-w a) (vec4-w b)))))

;; compute quadratic euclidian norm
;; normquad-vec4 : vec4 -> Number
(define normquad-vec4
  (lambda (a)
    (+
     (* (vec4-x a) (vec4-x a))
     (* (vec4-y a) (vec4-y a))
     (* (vec4-z a) (vec4-z a))
     (* (vec4-w a) (vec4-w a)))))

;; compute euclidian norm
;; norm-vec4 : vec4 -> Number
(define norm-vec4
  (lambda (a)
    (sqrt (normquad-vec4 a))))

;; normalize vector
;; normalize-vec4 : vec4 -> vec4
(define normalize-vec4
  (lambda (a)
    (/ a (norm-vec4 a))))

;; 4x4 matrix (implemented with 4 row vectors; vec4)
(define-record-procedures matrix4x4
  make-matrix4x4 matrix4x4?
  (matrix4x4-1
   matrix4x4-2
   matrix4x4-3
   matrix4x4-4))

;; create 4x4 from 4 3d-vectors
;; create-matrix4x4 : vec3 vec3 vec3 vec3 -> matrix4x4
(define create-matrix4x4
  (lambda (v1 v2 v3 v4)
    (make-matrix4x4
     (expand-vec3 v1 0 )
     (expand-vec3 v2 0 )
     (expand-vec3 v3 0 )
     (expand-vec3 v4 1 ))))

;; return a^T
;; transpose-matrix4x4 : matrix4x4 -> matrix4x4
(define transpose-matrix4x4 
  (lambda (a)
    (make-matrix4x4
     (make-vec4 (vec4-x (matrix4x4-1 a))
                (vec4-x (matrix4x4-2 a))
                (vec4-x (matrix4x4-3 a))
                (vec4-x (matrix4x4-4 a)))
     (make-vec4 (vec4-y (matrix4x4-1 a))
                (vec4-y (matrix4x4-2 a))
                (vec4-y (matrix4x4-3 a))
                (vec4-y (matrix4x4-4 a)))
     (make-vec4 (vec4-z (matrix4x4-1 a))
                (vec4-z (matrix4x4-2 a))
                (vec4-z (matrix4x4-3 a))
                (vec4-z (matrix4x4-4 a)))
     (make-vec4 (vec4-w (matrix4x4-1 a))
                (vec4-w (matrix4x4-2 a))
                (vec4-w (matrix4x4-3 a))
                (vec4-w (matrix4x4-4 a))))))

;; multiply 4x4 matrix with vec4
;; multiply-matrix-vec4 : matrix4x4 vec4 -> vec4
(define multiply-matrix-vec4
  (lambda (m v)
    (make-vec4 (dotproduct-vec4 (matrix4x4-1 m) v)
               (dotproduct-vec4 (matrix4x4-2 m) v)
               (dotproduct-vec4 (matrix4x4-3 m) v)
               (dotproduct-vec4 (matrix4x4-4 m) v))))

;; multiply homogenous matrix with (vec3,1) and project onto plane w=1
;; transform-vec3 : matrix4x4 vec3 -> vec3
(define transform-vec3
  (lambda (m v)
    (let ((v4 (make-vec4 (vec3-x v) (vec3-y v) (vec3-z v) 1)))
      (div-vec3 (make-vec3 (dotproduct-vec4 (matrix4x4-1 m) v4)
                           (dotproduct-vec4 (matrix4x4-2 m) v4)
                           (dotproduct-vec4 (matrix4x4-3 m) v4))
                (dotproduct-vec4 (matrix4x4-4 m) v4)))))


;; return a*b
;; multiply-matrix : matrix4x4 matrix4x4 -> matrix4x4
(define multiply-matrix
  (lambda (a b)
    (let ( (b^T (transpose-matrix4x4 b)) )
      (make-matrix4x4
       (make-vec4 (dotproduct-vec4 (matrix4x4-1 a) (matrix4x4-1 b^T))
                  (dotproduct-vec4 (matrix4x4-1 a) (matrix4x4-2 b^T))
                  (dotproduct-vec4 (matrix4x4-1 a) (matrix4x4-3 b^T))
                  (dotproduct-vec4 (matrix4x4-1 a) (matrix4x4-4 b^T)))
       (make-vec4 (dotproduct-vec4 (matrix4x4-2 a) (matrix4x4-1 b^T))
                  (dotproduct-vec4 (matrix4x4-2 a) (matrix4x4-2 b^T))
                  (dotproduct-vec4 (matrix4x4-2 a) (matrix4x4-3 b^T))
                  (dotproduct-vec4 (matrix4x4-2 a) (matrix4x4-4 b^T)))
       (make-vec4 (dotproduct-vec4 (matrix4x4-3 a) (matrix4x4-1 b^T))
                  (dotproduct-vec4 (matrix4x4-3 a) (matrix4x4-2 b^T))
                  (dotproduct-vec4 (matrix4x4-3 a) (matrix4x4-3 b^T))
                  (dotproduct-vec4 (matrix4x4-3 a) (matrix4x4-4 b^T)))
       (make-vec4 (dotproduct-vec4 (matrix4x4-4 a) (matrix4x4-1 b^T))
                  (dotproduct-vec4 (matrix4x4-4 a) (matrix4x4-2 b^T))
                  (dotproduct-vec4 (matrix4x4-4 a) (matrix4x4-3 b^T))
                  (dotproduct-vec4 (matrix4x4-4 a) (matrix4x4-4 b^T)))))))

;; create a matrix which translates (moves) by a 3d-vector
;; create-translation-matrix: vec3 -> matrix4x4
(define create-translation-matrix
  (lambda (translation)
    (make-matrix4x4
     (make-vec4 1 0 0 (vec3-x translation))
     (make-vec4 0 1 0 (vec3-y translation))
     (make-vec4 0 0 1 (vec3-z translation))
     (make-vec4 0 0 0 1))))

;; create a matrix which rotates around the x-axis
;; create-rotation-x-matrix: Number -> matrix4x4
(define create-rotation-x-matrix
  (lambda (angle)
    (make-matrix4x4
     (make-vec4 1 0 0 0)
     (make-vec4 0 (cos angle) (sin angle) 0)
     (make-vec4 0 (-(sin angle)) (cos angle) 0)
     (make-vec4 0 0 0 1))))

;; create a matrix which rotates around the y-axis
;; create-rotation-y-matrix: Number -> matrix4x4
(define create-rotation-y-matrix
  (lambda (angle)
    (make-matrix4x4
     (make-vec4 (cos angle) 0 (sin angle) 0)
     (make-vec4 0 1 0 0)
     (make-vec4 (-(sin angle)) 0 (cos angle) 0)
     (make-vec4 0 0 0 1))))

;; create a matrix which rotates around the z-axis
;; create-rotation-z-matrix: Number -> matrix4x4
(define create-rotation-z-matrix
  (lambda (angle)
    (make-matrix4x4
     (make-vec4 (cos angle) (sin angle) 0 0)
     (make-vec4 (-(sin angle)) (cos angle) 0 0)
     (make-vec4 0 0 1 0)
     (make-vec4 0 0 0 1))))

(define PI 3.14159265)
(define PI/2 (/ PI 2))
(define PI/4 (/ PI 4))

; output a vector
; print-vec4 : vec4 -> string
(define print-vec4
  (lambda (v)
    (string-append (number->string (vec4-x v)) "\t"
                   (number->string (vec4-y v)) "\t"
                   (number->string (vec4-z v)) "\t"
                   (number->string (vec4-w v)))))

; output a matrix
; print-matrix4x4 : matrix4x4 -> string
(define print-matrix4x4 
  (lambda (m)
    (let ((m^T (transpose-matrix4x4 m)))
      (string-append (print-vec4 (matrix4x4-1 m^T)) "\n"
                     (print-vec4 (matrix4x4-2 m^T)) "\n"
                     (print-vec4 (matrix4x4-3 m^T)) "\n"
                     (print-vec4 (matrix4x4-4 m^T)) "\n"))))

;; ---------------------------------------------
;; camera and projection
;; ---------------------------------------------

; create a look-at modelview matrix
; M = (v1 v2 v3 v4)
;     (0  0  0  1 )
; v1 = (lookat - position) x upvector
; v2 = ((lookat - position) x upvector) x (lookat - position)
; v3 = (lookat - position)
; v4 = (0 0 0)
; create-lookat-matrix : vec3 vec3 vec3 -> matrix4x4
(define create-lookat-matrix
  (lambda (position lookat upvector)
    (let* ((viewdirection (normalize-vec3 (sub-vec3 position lookat)))
           (normed-upvector (normalize-vec3 upvector))
           (rightvector (crossproduct-vec3 viewdirection normed-upvector)))
      (multiply-matrix
       (create-matrix4x4
        (normalize-vec3 rightvector)
        (normalize-vec3 (crossproduct-vec3 rightvector viewdirection))
        viewdirection
        (make-vec3 0 0 0))
       (create-translation-matrix (mult-vec3 position -1))))))

; projection with a specified vertical viewing angle
; create-projection-matrix :  number -> matrix4x4
(define create-projection-matrix
  (lambda (vertical-fov/2)
    (let ((f (/ (cos vertical-fov/2) (sin vertical-fov/2))))
      (make-matrix4x4
       (make-vec4 f 0 0 0)
       (make-vec4 0 f 0 0)
       (make-vec4 0 0 0 0)
       (make-vec4 0 0 1 0)))))

; transforms camera-space into image-space
; create-viewport-matrix : number number -> number
(define create-viewport-matrix
  (lambda (screenwidth screenheight)
    (let ((screenwidth/2 (/ screenwidth 2))
          (screenheight/2 (/ screenheight 2)))
      (make-matrix4x4
       (make-vec4 screenwidth/2 0 0 screenwidth/2)
       (make-vec4 0 screenheight/2 0 screenheight/2)
       (make-vec4 0 0 1/2 0)
       (make-vec4 0 0 0 1)))))

; create a complete camera matrix
; create-camera-matrix : 
(define create-camera-matrix
  (lambda (position lookat vertical-fov screenwidth screenheight)
    (multiply-matrix
     (multiply-matrix
      (create-viewport-matrix screenwidth screenheight)
      (create-projection-matrix (* (/ vertical-fov 360) PI)))
     (create-lookat-matrix position lookat (make-vec3 0 1 0)))))

;; ----------------------------------------------
;; scene
;; ----------------------------------------------

; defines a colored line between two points (3D)
(define-record-procedures line3d
  make-line3d line3d?
  (line3d-a line3d-b line3d-color))

; creates a box centered at (0,0,0) with the given dimensions.
; create-box : number number number color -> list(line3d)
(define create-box
  (lambda (width height depth color)
    (let ((corner1 (make-vec3 (- width) (- height) (- depth)))
          (corner2 (make-vec3    width  (- height) (- depth)))
          (corner3 (make-vec3    width     height  (- depth)))
          (corner4 (make-vec3 (- width)    height  (- depth)))
          (corner5 (make-vec3 (- width) (- height)    depth))
          (corner6 (make-vec3    width  (- height)    depth))
          (corner7 (make-vec3    width     height     depth))
          (corner8 (make-vec3 (- width)    height     depth)))
      (list
       (make-line3d corner1 corner2 color)
       (make-line3d corner2 corner3 color)
       (make-line3d corner3 corner4 color)
       (make-line3d corner4 corner1 color)
       (make-line3d corner5 corner6 color)
       (make-line3d corner6 corner7 color)
       (make-line3d corner7 corner8 color)
       (make-line3d corner8 corner5 color)
       (make-line3d corner1 corner5 color)
       (make-line3d corner1 corner5 color)
       (make-line3d corner2 corner6 color)
       (make-line3d corner3 corner7 color)
       (make-line3d corner4 corner8 color)))))

; apply transformation to every given line
; transform-primitive-list: list(line3d) matrix4x4 -> list(line3d)
(define transform-primitive-list
  (lambda (l mat)
    (cond
      ((pair? l) (transform-primitive-list-helper l mat empty))
      ((empty? l) empty))))

; transform-primitive-list-helper : list(line3d) matrix4x4 list(line3d) -> list(line3d)
(define transform-primitive-list-helper
  (lambda (l mat result)
    (cond
      ((pair? l)
       (transform-primitive-list-helper (rest l) mat 
                                        (make-pair (make-line3d (transform-vec3 mat (line3d-a (first l)))
                                                                (transform-vec3 mat (line3d-b (first l)))
                                                                (line3d-color (first l))) result)))
      ((empty? l) result))))

;; ---------------------------------------------
;; rendering
;; ---------------------------------------------

; w-clip epsilon
(define clip-epsilon -0.1)

;; clip line on plane w=clip-epsilon
;; clipline: vec4 vec4 color -> image
(define clipline
  (lambda (screenWidth screenHeight inside outside color)
    (let* ((delta-vec (sub-vec4 outside inside))
           (f (/ (- clip-epsilon (vec4-w inside)) (- (vec4-w outside) (vec4-w inside))))
           ; compute intersection with clipping plane
           (clipped-point (add-vec4 inside (mult-vec4 delta-vec f))) 
           ; project points by normalising to w=1
           (inside-projected (div-vec4 inside (vec4-w inside)))
           (clipped-point-projected (div-vec4 clipped-point (vec4-w clipped-point))))
      (line screenWidth screenHeight (vec4-x inside-projected) (vec4-y inside-projected)
            (vec4-x clipped-point-projected) (vec4-y clipped-point-projected) color))))


; render line with clipping
; render-clipped-line3d : N N vec4 vec4 matrix4x4 -> image
(define render-clipped-line3d
  (lambda (screenWidth screenHeight l camera-matrix)
    (let* ((point-a (line3d-a l))
           (point-b (line3d-b l))
           (point-a-transformed (multiply-matrix-vec4 camera-matrix 
                                                      (make-vec4 (vec3-x point-a) (vec3-y point-a) (vec3-z point-a) 1)))
           (point-b-transformed (multiply-matrix-vec4 camera-matrix 
                                                      (make-vec4 (vec3-x point-b) (vec3-y point-b) (vec3-z point-b) 1)))
           (projected-point1 (transform-vec3 camera-matrix (line3d-a l)))
           (projected-point2 (transform-vec3 camera-matrix (line3d-b l))))
      (cond
        ((and (< (vec4-w point-a-transformed) clip-epsilon)
              (< (vec4-w point-b-transformed) clip-epsilon))
         (line screenWidth screenHeight (vec3-x projected-point1) (vec3-y projected-point1)
               (vec3-x projected-point2) (vec3-y projected-point2) (line3d-color l)))
        ((and (>= (vec4-w point-a-transformed) clip-epsilon)
              (< (vec4-w point-b-transformed) clip-epsilon))
         (clipline screenWidth screenHeight point-b-transformed point-a-transformed (line3d-color l)))
        ((and (>= (vec4-w point-b-transformed) clip-epsilon)
              (< (vec4-w point-a-transformed) clip-epsilon))
         (clipline screenWidth screenHeight point-a-transformed point-b-transformed (line3d-color l)))
        (else (line screenWidth screenHeight -1 0 0 0 (line3d-color l)))))))

; render line without clipping (not used anymore)
; render-line3d : N N line3d matrix4x4 -> image
(define render-line3d
  (lambda (screenWidth screenHeight l camera-matrix)
    (let ((projected-point1 (transform-vec3 camera-matrix (line3d-a l)))
          (projected-point2 (transform-vec3 camera-matrix (line3d-b l))))
      (line screenWidth screenHeight (vec3-x projected-point1) (vec3-y projected-point1)
            (vec3-x projected-point2) (vec3-y projected-point2) (line3d-color l)))))

; render scene into an image
; render-scene: N N list(line3d) matrix4x4 -> image
(define render-scene
  (lambda (screenWidth screenHeight scene camera-matrix)
    (cond
      ((empty? scene)(line screenWidth screenHeight 0 0 0 0 "white"))
      ((pair? scene)
       (render-scene-helper screenWidth screenHeight (rest scene) camera-matrix 
                            (render-clipped-line3d screenWidth screenHeight (first scene) camera-matrix))))))

; render-scene-helper: list(line3d) matrix4x4 image -> image
(define render-scene-helper
  (lambda (screenWidth screenHeight scene camera-matrix screen)
    (cond
      ((empty? scene) screen)
      ((pair? scene) (render-scene-helper screenWidth screenHeight (rest scene) camera-matrix 
                                          (overlay screen
                                                   (render-clipped-line3d screenWidth screenHeight (first scene) camera-matrix) 0 0))))))
