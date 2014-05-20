#lang racket/base

(require racket/class
         racket/math
         racket/draw)

(provide draw-arrow)

(define largest 16383)
(define smallest -16383)

(define arrow-head-angle (/ pi 8))
(define cos-arrow-head-angle (cos arrow-head-angle))
(define sin-arrow-head-angle (sin arrow-head-angle))

; If alpha is the angle between the x axis and the Start->End vector:
;
; p2-x = end-x + arrow-head-size * cos(alpha + pi - arrow-head-angle)
;      = end-x - arrow-head-size * cos(alpha - arrow-head-angle)
;      = end-x - arrow-head-size * (cos(alpha) * cos(arrow-head-angle) + sin(alpha) * sin(arrow-head-angle))
;      = end-x - arrow-head-size-cos-arrow-head-angle * cos-alpha - arrow-head-size-sin-arrow-head-angle * sin-alpha
;      = end-x - arrow-head-size-cos-arrow-head-angle-cos-alpha - arrow-head-size-sin-arrow-head-angle-sin-alpha
;
; p2-y = end-y + arrow-head-size * sin(alpha + pi - arrow-head-angle)
;      = end-y - arrow-head-size * sin(alpha - arrow-head-angle)
;      = end-y - arrow-head-size * (sin(alpha) * cos(arrow-head-angle) - cos(alpha) * sin(arrow-head-angle))
;      = end-y - arrow-head-size-cos-arrow-head-angle * sin-alpha + arrow-head-size-sin-arrow-head-angle * cos-alpha
;      = end-y - arrow-head-size-cos-arrow-head-angle-sin-alpha + arrow-head-size-sin-arrow-head-angle-cos-alpha
;
; p3-x = end-x + arrow-head-size * cos(alpha + pi + arrow-head-angle)
;      = end-x - arrow-head-size * cos(alpha + arrow-head-angle)
;      = end-x - arrow-head-size * (cos(alpha) * cos(arrow-head-angle) - sin(alpha) * sin(arrow-head-angle))
;      = end-x - arrow-head-size-cos-arrow-head-angle * cos-alpha + arrow-head-size-sin-arrow-head-angle * sin-alpha
;      = end-x - arrow-head-size-cos-arrow-head-angle-cos-alpha + arrow-head-size-sin-arrow-head-angle-sin-alpha
;
; p3-y = end-y + arrow-head-size * sin(alpha + pi + arrow-head-angle)
;      = end-y - arrow-head-size * sin(alpha + arrow-head-angle)
;      = end-y - arrow-head-size * (sin(alpha) * cos(arrow-head-angle) + cos(alpha) * sin(arrow-head-angle))
;      = end-y - arrow-head-size-cos-arrow-head-angle * sin-alpha - arrow-head-size-sin-arrow-head-angle * cos-alpha
;      = end-y - arrow-head-size-cos-arrow-head-angle-sin-alpha - arrow-head-size-sin-arrow-head-angle-cos-alpha

; dc<%> real real real real real real -> void
; draw one arrow
(define (draw-arrow dc uncropped-pre-start-x uncropped-pre-start-y uncropped-pre-end-x uncropped-pre-end-y dx dy
                    #:pen-width [pen-width #f]
                    #:arrow-head-size [arrow-head-size 8]
                    #:arrow-root-radius [arrow-root-radius 2.5])
  
  (define arrow-head-size-cos-arrow-head-angle (* arrow-head-size cos-arrow-head-angle))
  (define arrow-head-size-sin-arrow-head-angle (* arrow-head-size sin-arrow-head-angle))
  
  (define arrow-root-diameter (* 2 arrow-root-radius))
  (define the-pen-width (or pen-width (send (send dc get-pen) get-width)))
  (let ([uncropped-start-x (+ uncropped-pre-start-x dx (- (/ the-pen-width 2)))]
        [uncropped-start-y (+ uncropped-pre-start-y dy)]
        [uncropped-end-x (+ uncropped-pre-end-x dx (- (/ the-pen-width 2)))]
        [uncropped-end-y (+ uncropped-pre-end-y dy)]
        [old-smoothed (send dc get-smoothing)])
    (let*-values ([(start-x start-y) (crop-to uncropped-start-x uncropped-start-y uncropped-end-x uncropped-end-y)]
                  [(end-x end-y) (crop-to uncropped-end-x uncropped-end-y uncropped-start-x uncropped-start-y)])
      (send dc set-smoothing 'aligned)
      (define saved-pen (send dc get-pen))
      (when pen-width
        (send dc set-pen
              (let ([p (send dc get-pen)])
                (send the-pen-list find-or-create-pen 
                      (send p get-color)
                      pen-width
                      (send p get-style)
                      (send p get-cap)
                      (send p get-join)))))
      (send dc draw-line start-x start-y end-x end-y)
      (send dc set-pen saved-pen)
      (when (and (< smallest start-x largest)
                 (< smallest start-y largest))
        (send dc draw-ellipse 
              (- start-x arrow-root-radius) (- start-y arrow-root-radius)
              arrow-root-diameter arrow-root-diameter))
      (when (and (< smallest end-x largest)
                 (< smallest end-y largest))
        (unless (and (= start-x end-x) (= start-y end-y))
          (let* ([offset-x (- end-x start-x)]
                 [offset-y (- end-y start-y)]
                 [arrow-length (sqrt (+ (* offset-x offset-x) (* offset-y offset-y)))]
                 [cos-alpha (/ offset-x arrow-length)]
                 [sin-alpha (/ offset-y arrow-length)]
                 [arrow-head-size-cos-arrow-head-angle-cos-alpha (* arrow-head-size-cos-arrow-head-angle cos-alpha)]
                 [arrow-head-size-cos-arrow-head-angle-sin-alpha (* arrow-head-size-cos-arrow-head-angle sin-alpha)]
                 [arrow-head-size-sin-arrow-head-angle-cos-alpha (* arrow-head-size-sin-arrow-head-angle cos-alpha)]
                 [arrow-head-size-sin-arrow-head-angle-sin-alpha (* arrow-head-size-sin-arrow-head-angle sin-alpha)]
                 ; pt1 is the tip of the arrow, pt2 is the first point going clockwise from pt1
                 [pt1 (make-object point% end-x end-y)]
                 [pt2 (make-object point%
                        (- end-x arrow-head-size-cos-arrow-head-angle-cos-alpha arrow-head-size-sin-arrow-head-angle-sin-alpha)
                        (+ end-y (- arrow-head-size-cos-arrow-head-angle-sin-alpha) arrow-head-size-sin-arrow-head-angle-cos-alpha))]
                 [pt3 (make-object point%
                        (+ end-x (- arrow-head-size-cos-arrow-head-angle-cos-alpha) arrow-head-size-sin-arrow-head-angle-sin-alpha)
                        (- end-y arrow-head-size-cos-arrow-head-angle-sin-alpha arrow-head-size-sin-arrow-head-angle-cos-alpha))])
            (send dc draw-polygon (list pt1 pt2 pt3)))))
      (send dc set-smoothing old-smoothed))))

;; crop-to : number number number number -> (values number number)
;; returns x,y if they are in the range defined by largest and smallest
;; otherwise returns the coordinates on the line from x,y to ox,oy
;; that are closest to x,y and are in the range specified by
;; largest and smallest
(define (crop-to x y ox oy)
  (cond
    [(and (< smallest x largest) (< smallest y largest))
     (values x y)]
    [else
     (let* ([xy-pr (cons x y)]
            [left-i (find-intersection x y ox oy smallest smallest smallest largest)]
            [top-i (find-intersection x y ox oy smallest smallest largest smallest)]
            [right-i (find-intersection x y ox oy largest smallest largest largest)]
            [bottom-i (find-intersection x y ox oy smallest largest largest largest)]
            [d-top (and top-i (dist top-i xy-pr))]
            [d-bottom (and bottom-i (dist bottom-i xy-pr))]
            [d-left (and left-i (dist left-i xy-pr))]
            [d-right (and right-i (dist right-i xy-pr))])
       (cond
         [(smallest? d-top d-bottom d-left d-right)
          (values (car top-i) (cdr top-i))]
         [(smallest? d-bottom d-top d-left d-right)
          (values (car bottom-i) (cdr bottom-i))]
         [(smallest? d-left d-top d-bottom d-right)
          (values (car left-i) (cdr left-i))]
         [(smallest? d-right d-top d-bottom d-left)
          (values (car right-i) (cdr right-i))]
         [else 
          ;; uh oh... if this case happens, that's bad news...
          (values x y)]))]))

;; smallest? : (union #f number)^4 -> boolean
;; returns #t if can is less and o1, o2, and o3
;; if can is #f, return #f. If o1, o2, or o3 is #f, assume that can is smaller than them
(define (smallest? can o1 o2 o3)
  (and can
       (andmap (λ (x) (< can x))
               (filter (λ (x) x)
                       (list o1 o2 o3)))))


;; inside? : (union #f (cons number number)) -> (union #f (cons number number))
;; returns the original pair if the coordinates are between smallest and largest
;; and returns #f if the pair is #f or the coordinates are outside.
(define (inside? pr)
  (and pr
       (let ([x (car pr)]
             [y (cdr pr)])
         (if (and (< smallest x largest)
                  (< smallest y largest))
             pr
             #f))))

;; find-intersection : (number^2)^2 -> (union (cons number number) #f)
;; finds the intersection between the lines specified by
;; (x1,y1) -> (x2,y2) and (x3,y3) -> (x4,y4)
(define (find-intersection x1 y1 x2 y2 x3 y3 x4 y4)
  (cond
    [(and (= x1 x2) (= x3 x4))
     #f]
    [(and (= x1 x2) (not (= x3 x4)))
     (let* ([m2 (/ (- y3 y4) (- x3 x4))]
            [b2 (- y3 (* m2 x3))])
       (cons x1
             (+ (* m2 x1) b2)))]
    [(and (not (= x1 x2)) (= x3 x4))
     (let* ([m1 (/ (- y1 y2) (- x1 x2))]
            [b1 (- y1 (* m1 x1))])
       (cons x3
             (+ (* m1 x3) b1)))]
    [(and (not (= x1 x2)) (not (= x3 x4)))
     (let* ([m1 (/ (- y1 y2) (- x1 x2))]
            [b1 (- y1 (* m1 x1))]
            [m2 (/ (- y3 y4) (- x3 x4))]
            [b2 (- y3 (* m2 x3))])
       (if (= m1 m2)
           #f
           (let* ([x (/ (- b1 b2) (- m2 m1))]
                  [y (+ (* m1 x) b1)])
             (cons x y))))]))

;; dist : (cons number number) (cons number number) -> number
(define (dist p1 p2)
  (sqrt (+ (sqr (- (car p1) (car p2)))
           (sqr (- (cdr p1) (cdr p2))))))

;; localled defined test code....
;; use module language to run tests
(module+ test
  (require rackunit)
  (check-equal? (find-intersection 0 1 0 10 0 2 0 20) #f)
  (check-equal? (find-intersection 0 1 0 10 0 0 10 10) (cons 0 0))
  (check-equal? (find-intersection 0 0 10 10 0 1 0 10) (cons 0 0))
  (check-equal? (find-intersection 0 0 3 3 2 2 4 4) #f)
  (check-equal? (find-intersection -3 3 3 -3 -3 -3 3 3) (cons 0 0))
  (check-equal? (smallest? 3 1 2 3) #f)
  (check-equal? (smallest? 0 1 2 3) #t)
  (check-equal? (smallest? 1 0 2 3) #f)
  (check-equal? (smallest? 1 0 #f 4) #f)
  (check-equal? (smallest? 1 #f #f 4) #t)
  (check-equal? (smallest? 1 #f #f #f) #t)
  (check-equal? (dist (cons 1 1) (cons 4 5)) 5))