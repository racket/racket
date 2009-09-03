#lang scheme

(require htdp/image htdp/error "check-aux.ss")

;                                                                        
;                                                                        
;   ;;;;;                                     ;;;;;                      
;     ;                                       ;                          
;     ;                                       ;                          
;     ;   ;;; ;    ;;;;   ;;;;   ;;;          ;      ;   ;  ; ;;    ;;;  
;     ;   ; ;;;   ;   ;  ;   ;  ;   ;         ;;;;;  ;   ;  ;;  ;  ;   ; 
;     ;   ; ; ;   ;   ;  ;   ;  ;;;;;         ;      ;   ;  ;   ;   ;;   
;     ;   ; ; ;   ;   ;  ;   ;  ;             ;      ;   ;  ;   ;     ;  
;     ;   ; ; ;   ;  ;;  ;  ;;  ;             ;      ;  ;;  ;   ;  ;   ; 
;   ;;;;; ; ; ;    ;; ;   ;; ;   ;;;;         ;       ;; ;  ;   ;   ;;;  
;                            ;                                           
;                        ;;;;                                            
;                                                                        

(provide (all-from-out htdp/image))

(provide
 ;; Scene is Image with pinhole in origin 
 nw:rectangle ;; Number Number Mode Color -> Image
 place-image  ;; Image Number Number Scene -> Scene
 empty-scene  ;; Number Number -> Scene 
 scene+line   ;; Scene Number Number Number Number Color -> Scene 
 ;; cut all pieces that are outside the given rectangle 
 )

(define (nw:rectangle width height mode color)
  (check-pos 'rectangle width "first")
  (check-pos 'rectangle height "second")
  (check-mode 'rectangle mode "third")
  (check-color 'rectangle color "fourth")
  (put-pinhole (rectangle width height mode color) 0 0))

(define (place-image image x y scene)
  (check-image 'place-image image "first")
  (check-arg 'place-image (number? x) 'integer "second" x)
  (check-arg 'place-image (number? y) 'integer "third" y)
  (check-scene 'place-image scene "fourth")
  (let ([x (number->integer x)]
        [y (number->integer y)])
    (place-image0 image x y scene)))

(define (empty-scene width height)
  (check-pos 'empty-scene width "first")
  (check-pos 'empty-scene height "second")    
  (put-pinhole 
   (overlay (rectangle width height 'solid 'white)
            (rectangle width height 'outline 'black))
   0 0))

(define (scene+line img x0 y0 x1 y1 c)
  ;; img and c are checked via calls to add-line from image.ss
  (check-arg 'scene+line (scene? img) "scene" "first" "plain image")
  (check-arg 'scene+line (number? x0) "number" "second" x0)
  (check-arg 'scene+line (number? y0) "number" "third" y0)
  (check-arg 'scene+line (number? x1) "number" "fourth" x1)
  (check-arg 'scene+line (number? y1) "number" "fifth" y1)
  (let ([x0 (number->integer x0)]
        [x1 (number->integer x1)]
        [y0 (number->integer y0)]
        [y1 (number->integer y1)])
    (add-line-to-scene0 img x0 y0 x1 y1 c)))

;; Image Number Number Image -> Image 
(define (place-image0 image x y scene)
  (define sw (image-width scene))
  (define sh (image-height scene))
  (define ns (overlay/xy scene x y image))
  (define nw (image-width ns))
  (define nh (image-height ns))
  (if (and (= sw nw) (= sh nh)) ns (shrink ns 0 0 (- sw 1) (- sh 1)))) 

;; Image Number Number Number Number Color -> Image
(define (add-line-to-scene0 img x0 y0 x1 y1 c)
  (define w (image-width img))  
  (define h (image-height img))
  (cond
    [(and (<= 0 x0) (< x0 w) (<= 0 x1) (< x1 w) (<= 0 y0) (< y0 w) (<= 0 y1) (< y1 w))
     (add-line img x0 y0 x1 y1 c)]
    [(= x0 x1) ;; vertical 
     (if (<= 0 x0 w) (add-line img x0 (app y0 h) x0 (app y1 h) c) img)]
    [(= y0 y1) ;; horizontal 
     (if (<= 0 y0 h) (add-line img (app x0 w) y0 (app x1 w) y0 c) img)]
    [else 
     (local ((define lin (points->line x0 y0 x1 y1))
             (define dir (direction x0 y0 x1 y1))
             (define-values (upp low lft rgt) (intersections lin w h))
             (define (add x y) (add-line img x0 y0 x y c)))
       (cond
         [(and (< 0 x0 w) (< 0 y0 h)) ;; (x0,y0) is in the interior
          (case dir
            [(upper-left)  (if (number? upp) (add upp 0) (add 0 lft))]
            [(lower-left)  (if (number? low) (add low h) (add 0 lft))]
            [(upper-right) (if (number? upp) (add upp 0) (add h rgt))]
            [(lower-right) (if (number? low) (add low h) (add w rgt))]
            [else (error 'dir "contract violation: ~e" dir)])]
         [(and (< 0 x1 w) (< 0 y1 h)) ;; (x1,y1) in interior; symmetry!
          (add-line-to-scene0 img x1 y1 x0 y0 c)]
         [else 
          (cond
            [(and (number? upp) (number? low)) (add-line img upp 0 low h c)]
            [(and (number? upp) (number? lft)) (add-line img upp 0 0 lft c)]
            [(and (number? upp) (number? rgt)) (add-line img upp 0 w rgt c)]
            [(and (number? low) (number? lft)) (add-line img low h 0 lft c)]
            [(and (number? low) (number? rgt)) (add-line img low h w rgt c)]
            [(and (number? lft) (number? rgt)) (add-line img 0 lft w rgt c)]
            [else img])]))]))

;; Nat Nat -> Nat 
;; y if in [0,h], otherwise the closest boundary
(define (app y h)
  (cond
    [(and (<= 0 y) (< y h)) y]
    [(< y 0) 0]
    [else (- h 1)]))

;; Nat Nat Nat Nat -> (union 'upper-left 'upper-right 'lower-left 'lower-right)
;; how to get to (x1,y1) from (x0,y0)
(define (direction x0 y0 x1 y1)
  (string->symbol
   (string-append 
    (if (<= y0 y1) "lower" "upper") "-" (if (<= x0 x1) "right" "left"))))

#| TESTS
'direction 
(equal? (direction 10 10 0 0) 'upper-left)
(equal? (direction 10 10 20 20) 'lower-right)
(equal? (direction 10 10 0 20) 'lower-left)
(equal? (direction 10 10 20 0) 'upper-right)
|#

;; -----------------------------------------------------------------------------
;; LINEs 

;; Number Number -> LINE
;; create a line from a slope and the intersection with the y-axis
(define-struct lyne (slope y0))

;; Nat Nat Nat Nat -> LINE
;; determine the line function from the four points (or the attributes)
;; ASSUME: (not (= x0 x1))
(define (points->line x0 y0 x1 y1)
  (local ((define slope  (/ (- y1 y0) (- x1 x0))))
    (make-lyne slope (- y0 (* slope x0)))))

;; LINE Number -> Number 
(define (of ln x) (+ (* (lyne-slope ln) x) (lyne-y0 ln)))

;; LINE Nat Nat -> [Opt Number] [Opt Number] [Opt Number] [Opt Number]
;; where does the line intersect the rectangle [0,w] x [0,h]
;; (values UP LW LF RT) means the line intersects with 
;;  the rectangle [0,w] x [0,h] at (UP,0) or (LW,h) or (0,LF) or (w,RT)
;;  when a field is false, the line doesn't interesect with that side 
(define (intersections l w h)
  (values
   (opt (X l 0) w) (opt (X l h) w) (opt (lyne-y0 l) h) (opt (of l w) h)))

;; Number Number -> [Opt Number]
(define (opt z lft) (if (<= 0 z lft) z false))

;; LINE Number -> Number 
;; the x0 where LINE crosses y(x) = h
;; assume: LINE is not a horizontal
(define (X ln h) (/ (- h (lyne-y0 ln)) (lyne-slope ln)))

;; --- TESTS --- 
#|
(define line1 (points->line 0 0 100 100))
(= (of line1 0) 0)
(= (of line1 100) 100)
(= (of line1 50) 50)

(= (X (make-lyne 1 0) 0) 0)
(= (X (make-lyne 1 0) 100) 100)

(equal? (call-with-values 
         (lambda () (intersections (points->line -10 -10 110 110) 100 100))
         list)
        (list 0 100 0 100))
(equal? (call-with-values 
         (lambda () (intersections (points->line 0 10 100 80) 100 100))
         list)
        (list false false 10 80))
|#

;; -----------------------------------------------------------------------------

;                                                                               
;                                                                               
