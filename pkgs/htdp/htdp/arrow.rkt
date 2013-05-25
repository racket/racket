#lang scheme/gui

(require htdp/error
         htdp/big-draw
         lang/prim
         mzlib/etc
         mzlib/class)

(provide
 control
 control-up-down
 control-left-right
 )

(define-higher-order-primitive control-up-down control-up-down/proc 
  (_ _ up-down draw))

(define-higher-order-primitive control-left-right control-left-right/proc 
  (_ _ left-right draw))

(define-higher-order-primitive control control/proc 
  (_ _ left-right up-down draw))  


;; CONSTANTS ---------------------------------------------------------------
(define MY-ICONS "/home/matthias/icons/")
(define TITLE "Controller")

(define (mk-image-constant kind)
  (make-object bitmap%
    (build-path (collection-path "icons") (format "arrow.~a.gif" kind)) 'gif))

;(define LEFT-ARROW  (mk-image-constant "marble.left"))
;(define RIGHT-ARROW (mk-image-constant "marble.right"))
;(define UP-ARROW    (mk-image-constant "marble.up"))
;(define DOWN-ARROW  (mk-image-constant "marble.down"))

(define LEFT-ARROW  (mk-image-constant "blue.left"))
(define RIGHT-ARROW (mk-image-constant "blue.right"))
(define UP-ARROW    (mk-image-constant "blue.up"))
(define DOWN-ARROW  (mk-image-constant "blue.down"))

;; LAYOUT ------------------------------------------------------------------

;; layout = (listof (listof (union #f bitmap%)))  

(define FOUR 
  `( (,#f         ,UP-ARROW   ,#f)
     (,LEFT-ARROW ,#f         ,RIGHT-ARROW)
     (,#f         ,DOWN-ARROW ,#f) ))

(define UP-DOWN
  `( (,UP-ARROW   )
     (,DOWN-ARROW ) ))

(define LEFT-RIGHT
  `( (,LEFT-ARROW ,RIGHT-ARROW ) ))

;; make-button-table : 
;;   panel% layout (bitmap% -> (_ _ -> X))
;;   -> 
;;   (listof (listof (union panel% button%)))
;; to translate a layout table into a button table 
;;   each button is controlled by (control a-bitmap)
(define (make-button-table panel control layout)
  (define (make-row a-row)
    (define row-panel (make-object horizontal-panel% panel))
    (define (make-item an-item)
      (if an-item
          (make-object button% an-item row-panel (control an-item))
          (let ([panel (make-object horizontal-panel% row-panel)])
            (send panel min-width 30))))
    ;; --- 
    (map make-item a-row))
  ;; --- 
  (map make-row layout))

;; GUI ---------------------------------------------------------------------

;; make-controller :
;;   symbol layout number X (number X -> true) (number X -> true) (X -> true)-> void
;; effect: create a left-right controller that invokes move on delta
(define (make-controller tag layout shape delta left-right-action up-down-action draw-shape)
  (check-arg  tag
              (and (number? delta) (integer? delta) (>= delta 1))
              "positive integer"
              '2nd
              delta)
  (check-proc tag left-right-action 2 "move-left-right" "two arguments")
  (check-proc tag up-down-action 2 "move-up-down" "two arguments")
  (check-proc tag draw-shape 1 "draw" "one argument")
  ;; --- 
  (local ((define frame (make-object frame% TITLE #f 10 10))
          (define panel (make-object vertical-panel% frame))
          ;; control : bitmap% -> (_ _ -> void)
          ;; to check which button was clicked 
          (define (control an-item)
            (lambda (x y)
              ;; DESIGN DECISION:
              ;; by handing over the number first, nesting the moves becomes easier
              (evcase an-item
                      (UP-ARROW
                       (set! shape (up-down-action (- delta) shape)))
                      (DOWN-ARROW
                       (set! shape (up-down-action delta shape)))
                      (LEFT-ARROW
                       (set! shape (left-right-action (- delta) shape)))
                      (RIGHT-ARROW
                       (set! shape (left-right-action delta shape))))
              (draw-shape shape))))
    (make-button-table panel control layout)
    (send frame show #t)
    #t))

;; EXPORTS:

(define (void2 x y) (void))

;; control-left-right/proc : XShape number (number XShape -> XShape) (XShape -> true) -> true
;; effect: create a window from which a user can control L/R moves
(define (control-left-right/proc shape delta lr draw)
  (make-controller 'control-left-right LEFT-RIGHT shape delta lr void2 draw))

;; control-up-down : X number (number X -> true) (X -> true) -> true
;; effect: create a window from which a user can control U/D moves      
(define (control-up-down/proc shape delta ud draw)
  (make-controller 'control-up-down UP-DOWN shape delta void2 ud draw))

;; control/proc : X number (number X -> true) (number X -> true) (X -> true) -> true
;; effect: create a window from which a user can control moves
(define (control/proc shape delta lr ud draw)
  (make-controller 'control FOUR shape delta lr ud draw))

