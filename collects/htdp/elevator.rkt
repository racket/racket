#lang scheme/gui

(require htdp/big-draw
         htdp/error
         lang/prim
         lang/posn
         mzlib/etc)

;; Implementation:
;;   Stephanie Weirich (1994),
;;   Mark Krentel (1995),
;;   Matthias Felleisen (1996)

(provide run)

(define-higher-order-primitive run run/proc (elevator-controller))

;;  There are really  three distinct levels: graphics, hardware,
;;  and software. Don't mix them!

;;
;;  HARDWARE/GRAPHICS level for Elevator.
;;  This level provides the basic primitives for writing an elevator.
;;  The hardware level are the guards for the graphics level.
;;
;;  Hardware functions:
;;
;;     make-elevator  max-floor
;;     current-floor  move-up-floor    move-down-floor
;;     update-input   wait-for-input
;;     open?          open-doors       close-doors
;;     up-call?       down-call?       demand?
;;     clear-up-call  clear-down-call  clear-demand
;;     busy-wait      info
;;

;;  Compute the layout and draw the window.

(define  init-graphics
  (lambda  ()
    (init-layout-and-window)
    (init-shaft)
    (init-calls)
    (init-info)
    (init-demands)
    (init-stop)
    (init-car)
    (busy-wait  800)
    (draw-open-doors)))

;;
;;  Overall layout for the window.
;;  Currently need max-floor >= 4.
;;

(define  BORDER  100)
(define  SHAFT-INFO-GAP   40)
(define  STOP-DEMAND-GAP  50)

(define  init-layout-and-window
  (lambda  ()
    (let* 
        ([shaft  (make-posn  BORDER  BORDER)]
         [call   (add-hv  shaft  SHAFT-WIDTH  0)]
         [top    (add-hv  call  CALL-WIDTH  0)]
         [bot    (add-hv  top  0  (* (max-floor) FLOOR-SIZE))]
         [info   (add-hv  top  SHAFT-INFO-GAP  0)]
         [stop   (add-hv  bot  SHAFT-INFO-GAP  (- STOP-HEIGHT))]
         [demand (add-hv  stop  0  (- 0 STOP-DEMAND-GAP DEMAND-HEIGHT))]
         [horiz  (+  (posn-x info)  STOP-WIDTH  BORDER)]
         [vert   (+  (posn-y bot)  BORDER)])
      (set!  SHAFT-ORIGIN   shaft)
      (set!  CALL-ORIGIN    call)
      (set!  INFO-ORIGIN    info)
      (set!  STOP-ORIGIN    stop)
      (set!  DEMAND-ORIGIN  demand)
      (init-window  horiz  vert))))

;;
;;  Window primitives for lines, strings, mouse clicks.
;;  These should be the only functions that use elev-win.
;;

(define  elev-win  #f)
(define  d-line    #f)
(define  c-line    #f)
(define  d-string  #f)
(define  c-string  #f)
(define  mouse-click?    (lambda () (ready-mouse-click elev-win)))
(define  wait-for-click  (lambda () (get-mouse-click elev-win)))

(define  init-window
  (lambda  (horiz  vert)
    (set!  elev-win  (open-viewport "Elevator Simulation" horiz vert))
    (set!  d-line  (draw-line  elev-win))
    (set!  c-line  (clear-line  elev-win))
    (set!  d-string  (draw-string  elev-win))
    (set!  c-string  (clear-string  elev-win))))

(define  d-string-bf
  (lambda  (posn  string)
    (d-string  posn  string)
    (d-string  (add-hv posn 1 0)  string)))

(define  c-string-bf
  (lambda  (posn  string)
    (c-string  posn  string)
    (c-string  (add-hv posn 1 0)  string)))

;;
;;  Helper functions for position, offsets, etc.
;;  SIXLib should provide better primitives here.
;;

(define  under?   (lambda  (p1  p2)  (>  (posn-y p1)  (posn-y p2))))

(define  add-posn
  (lambda  (p1  p2)
    (make-posn  (+  (posn-x p1)  (posn-x p2))
                (+  (posn-y p1)  (posn-y p2)))))

(define  add-hv
  (lambda  (p  horiz  vert)
    (make-posn  (+  (posn-x p)  horiz) 
                (+  (posn-y p)  vert))))

;;
;;  Low-level primitives for lines, rectangles, etc.
;;

(define  paint-rect
  (lambda  (origin  horiz  vert)
    (when  (>= vert 0)
      (let ([right  (add-hv origin horiz 0)]
            [next   (add-hv origin 0 +1)])
        (d-line  origin  right)
        (paint-rect  next  horiz  (- vert 1))))))

(define  outline-rect
  (lambda  (origin  horiz  vert  thick)
    (let* ([x0  (posn-x origin)]  [y0  (posn-y origin)])
      (recur  loop  ([lf   x0]  [rt   (+ x0 horiz thick -1)]
                                [top  y0]  [bot  (+ y0 vert  thick -1)]
                                [n  thick])
        (when  (> n 0)
          (d-line  (make-posn lf top)  (make-posn rt top))
          (d-line  (make-posn rt top)  (make-posn rt bot))
          (d-line  (make-posn rt bot)  (make-posn lf bot))
          (d-line  (make-posn lf bot)  (make-posn lf top))
          (loop  (+ lf 1)  (- rt 1)  (+ top 1)  (- bot 1)  (- n 1)))))))

(define  clear-inside-rect
  (lambda  (origin  horiz  vert  thick)
    (recur  loop  ([lf  (add-hv origin thick thick)]
                   [rt  (add-hv origin (- horiz 1) thick)]
                   [n  thick])
      (when  (< n vert)
        (c-line  lf  rt)
        (loop  (add-hv lf 0 +1)  (add-hv rt 0 +1) (+ n 1))))))

;;
;;  Elevator Shaft and Car 
;;

(define  SHAFT-ORIGIN #f)
(define  STOP-ORIGIN  #f)
(define  SHAFT-WIDTH  64)
(define  FLOOR-SIZE   64)
(define  CAR-WIDTH    44)
(define  CAR-HEIGHT   44)
(define  MIN-DOOR-SEP  3)
(define  NUMBER-POSN  (make-posn -22 (floor (/ FLOOR-SIZE 2))))

(define  init-shaft
  (lambda  ()
    (let ([height  (* (max-floor) FLOOR-SIZE)])
      (outline-rect  SHAFT-ORIGIN  SHAFT-WIDTH  height  2)
      (recur  loop  ([p  (add-posn SHAFT-ORIGIN NUMBER-POSN)]
                     [n  (max-floor)])
        (when  (>= n 1)
          (d-string-bf  p  (number->string n))
          (loop  (add-hv p 0 FLOOR-SIZE)  (- n 1)))))))

(define  init-car
  (lambda  ()
    (let* ([origin  (car-posn 1)])
      (outline-rect  origin  CAR-WIDTH  CAR-HEIGHT  1)
      (outline-rect  origin  (floor (/ CAR-WIDTH 2))  CAR-HEIGHT  1))))

(define  car-posn
  (lambda  (n)
    (add-hv  SHAFT-ORIGIN
             (floor (/ (- SHAFT-WIDTH CAR-WIDTH) 2))
             (+  (floor (/ (- FLOOR-SIZE  CAR-HEIGHT) 2))
                 (*  (- (max-floor) n)  FLOOR-SIZE)))))

(define  move-car-door
  (lambda  (horiz  delta)
    (let* ([origin   (car-posn (current-floor))]
           [old-top  (add-hv origin horiz +1)]
           [old-bot  (add-hv origin horiz (- CAR-HEIGHT 1))]
           [new-top  (add-hv old-top delta 0)]
           [new-bot  (add-hv old-bot delta 0)])
      (d-line  new-top  new-bot)
      (c-line  old-top  old-bot))))

(define  draw-open-doors
  (lambda  ()
    (recur  loop  ([lf  (floor   (/ CAR-WIDTH 2))]
                   [rt  (ceiling (/ CAR-WIDTH 2))])
      (when  (<  MIN-DOOR-SEP  lf)
        (move-car-door  lf  -1)
        (move-car-door  rt  +1)
        (busy-wait)
        (loop  (- lf 1)  (+ rt 1))))
    (draw-little-man)))

(define  draw-close-doors
  (lambda  ()
    (recur  loop  ([lf  MIN-DOOR-SEP]
                   [rt  (- CAR-WIDTH MIN-DOOR-SEP)])
      (when  (>=  (- rt lf)  2)
        (move-car-door  lf  +1)
        (move-car-door  rt  -1)
        (busy-wait)
        (loop  (+ lf 1)  (- rt 1))))))

(define  move-fwd-edge
  (lambda  (origin  delta)
    (let* ([new-lf  (add-hv origin 0 delta)]
           [new-rt  (add-hv origin CAR-WIDTH delta)]
           [old-1-lf  (add-hv origin +1 0)]
           [old-1-rt  (add-hv origin (- (floor (/ CAR-WIDTH 2)) 1) 0)]
           [old-2-lf  (add-hv origin (+ (ceiling (/ CAR-WIDTH 2)) 1) 0)]
           [old-2-rt  (add-hv origin (- CAR-WIDTH 1) 0)])
      (d-line  new-lf  new-rt)
      (c-line  old-1-lf  old-1-rt)
      (c-line  old-2-lf  old-2-rt))))

(define  move-back-edge
  (lambda  (origin  delta)
    (let*  ([new-lf  (add-hv origin 0 delta)]
            [new-rt  (add-hv origin CAR-WIDTH delta)]
            [old-lf  origin]
            [old-rt  (add-hv origin CAR-WIDTH 0)])
      (d-line  new-lf  new-rt)
      (c-line  old-lf  old-rt))))

(define  draw-up-floor
  (lambda  ()
    (let ([goal  (car-posn (+ (current-floor) 1))])
      (recur  loop  ([cur  (car-posn (current-floor))])
        (when  (under?  cur  goal)
          (move-fwd-edge  cur  -1)
          (move-back-edge  (add-hv cur 0 CAR-HEIGHT)  -1)
          (busy-wait)
          (loop  (add-hv cur 0 -1)))))))

(define  draw-down-floor
  (lambda  ()
    (let ([goal  (car-posn (- (current-floor) 1))])
      (recur  loop  ([cur  (car-posn (current-floor))])
        (when  (under?  goal  cur)
          (move-fwd-edge  (add-hv cur 0 CAR-HEIGHT)  +1)
          (move-back-edge  cur  +1)
          (busy-wait)
          (loop  (add-hv cur 0 +1)))))))

;;  This is probably going too far ...
;;  But he's more than just a list of lines!

(define  MAN-POSN  (make-posn 18 14))

(define  LITTLE-MAN
  (list  (make-posn 5 0)   (make-posn 9 0)    ; head
         (make-posn 9 0)   (make-posn 9 4)
         (make-posn 9 4)   (make-posn 5 4)
         (make-posn 5 4)   (make-posn 5 0)
         (make-posn 7 4)   (make-posn 7 12)   ; body
         (make-posn 7 12)  (make-posn 2 23)   ; legs
         (make-posn 7 12)  (make-posn 12 23)
         (make-posn 0 23)  (make-posn 2 23)   ; feet
         (make-posn 12 23) (make-posn 14 23)
         (make-posn 1 8)   (make-posn 13 8)   ; arms
         (make-posn 0 9)   (make-posn 1 8)    ; hands
         (make-posn 13 8)  (make-posn 14 7)))

(define  draw-little-man
  (lambda  ()
    (let ([origin  (add-posn (car-posn (current-floor)) MAN-POSN)])
      (recur  loop  ([l  LITTLE-MAN])
        (unless  (null? l)
          (d-line  (add-posn  origin  (car l))  
                   (add-posn  origin  (cadr l)))
          (loop  (cddr l)))))))

;;
;;  Call Buttons 
;;

(define  CALL-ORIGIN  #f)
(define  CALL-WIDTH   50)
(define  CALL-HEIGHT  (floor (/ FLOOR-SIZE 2)))

(define  UP-CALL-SHAPE  
  (list  (make-posn 13 28)  (make-posn 25 4)   (make-posn 37 28)))
(define  DOWN-CALL-SHAPE
  (list  (make-posn 13 4)   (make-posn 25 28)  (make-posn 37 4)))

(define  UP-CALL-POSN
  (lambda  (floor)
    (add-hv  CALL-ORIGIN  0  (* FLOOR-SIZE (- (max-floor) floor)))))

(define  DOWN-CALL-POSN
  (lambda  (floor)  
    (add-hv  (UP-CALL-POSN floor)  0  CALL-HEIGHT)))

(define  init-calls
  (lambda  ()
    (recur  loop  ([n  1])
      (when  (<=  n  (max-floor))
        (outline-rect  (UP-CALL-POSN n)  CALL-WIDTH  FLOOR-SIZE  2)
        (outline-call  (UP-CALL-POSN n)  UP-CALL-SHAPE)
        (outline-call  (DOWN-CALL-POSN n)  DOWN-CALL-SHAPE)
        (loop  (+ n 1))))))

(define  draw-clear-up
  (lambda  (floor)
    (clear-call    (UP-CALL-POSN floor)  UP-CALL-SHAPE)
    (outline-call  (UP-CALL-POSN floor)  UP-CALL-SHAPE)))

(define  draw-clear-down
  (lambda  (floor)
    (clear-call    (DOWN-CALL-POSN floor)  DOWN-CALL-SHAPE)
    (outline-call  (DOWN-CALL-POSN floor)  DOWN-CALL-SHAPE)))

(define  paint-up-call
  (lambda  (floor)
    (paint-call  (UP-CALL-POSN floor)  UP-CALL-SHAPE)))

(define  paint-down-call
  (lambda  (floor)
    (paint-call  (DOWN-CALL-POSN floor)  DOWN-CALL-SHAPE)))

(define  outline-call
  (lambda  (origin  l)
    (let* ([p  (add-posn origin (car l))]
           [q  (add-posn origin (cadr l))] 
           [r  (add-posn origin (caddr l))])
      (d-line  p  q)
      (d-line  q  r)
      (d-line  r  p))))

(define  clear-call
  (lambda  (origin  l)
    (let* ([p  (add-posn origin (car l))]
           [q  (add-posn origin (cadr l))] 
           [r  (add-posn origin (caddr l))]
           [top  (min (posn-y p) (posn-y q))]
           [bot  (max (posn-y p) (posn-y q))])
      (recur  loop  ([y  (+ top 1)])
        (when  (< y bot)
          (let ([lf  (ceiling (x-val p q y))]
                [rt  (floor   (x-val q r y))])
            (c-line  (make-posn lf y)  (make-posn rt y))
            (loop  (+ y 1))))))))

(define  paint-call
  (lambda  (origin  l)
    (let* ([p  (add-posn origin (car l))]
           [q  (add-posn origin (cadr l))] 
           [r  (add-posn origin (caddr l))]
           [top  (min (posn-y p) (posn-y q))]
           [bot  (max (posn-y p) (posn-y q))])
      (recur  loop  ([y  (+ top 1)])
        (when  (< y bot)
          (let ([lf  (ceiling (x-val p q y))]
                [rt  (floor   (x-val q r y))])
            (d-line  (make-posn lf y)  (make-posn rt y))
            (loop  (+ y 1))))))))

(define  x-val
  (lambda  (a  b  y)
    (let ([ax  (posn-x a)]  [ay  (posn-y a)]
                            [bx  (posn-x b)]  [by  (posn-y b)])
      (+  ax  (*  (/  (-  y  ay)  (-  by  ay))
                  (-  bx  ax))))))

;;
;;  Demand Buttons
;;

(define  DEMAND-ORIGIN  #f)
(define  DEMAND-WIDTH   40)
(define  DEMAND-HEIGHT  40)
(define  DEMAND-TEXT  (make-posn 18 25))

(define  DEMAND-POSN
  (let ([HORIZ  (+ DEMAND-WIDTH 20)]  
        [VERT   (+ DEMAND-HEIGHT 20)])
    (lambda  (i)
      (let ([x  (remainder (sub1 i) 2)]
            [y  (quotient  (sub1 i) 2)])
        (add-hv  DEMAND-ORIGIN  (* x HORIZ)  (* -1 y VERT))))))

(define  init-demands
  (lambda  ()
    (recur  loop  ([n  1])
      (when  (<= n (max-floor))
        (outline-rect  (DEMAND-POSN n)  DEMAND-WIDTH  DEMAND-HEIGHT  2) 
        (d-string-bf  (add-posn (DEMAND-POSN n) DEMAND-TEXT)  (number->string n))
        (loop  (+ n 1))))))

(define  paint-demand
  (lambda  (k)
    (let ([nw  (DEMAND-POSN k)])
      (paint-rect  nw  DEMAND-WIDTH  DEMAND-HEIGHT)
      (c-string-bf  (add-posn nw DEMAND-TEXT)  (number->string k)))))

(define  draw-clear-demand
  (lambda  (k)
    (clear-inside-rect  (DEMAND-POSN k)  DEMAND-WIDTH  DEMAND-HEIGHT  2)
    (d-string-bf  (add-posn (DEMAND-POSN k) DEMAND-TEXT)  (number->string k))))

;;
;;  "Stop Program" Button
;;

(define  STOP-WIDTH  100)
(define  STOP-HEIGHT  40)
(define  STOP-TEXT  (make-posn 15 25))

(define  init-stop
  (lambda  ()
    (outline-rect  STOP-ORIGIN  STOP-WIDTH  STOP-HEIGHT  2)
    (d-string  (add-posn STOP-ORIGIN STOP-TEXT)  "Stop Program")))

;;
;;  Mouse Clicks
;;  Look for up/down calls, demands and stop-program button.
;;  If you can get access to a real-time clock, then change the
;;  delay loop to use sleep-for or real-time.
;;  The units are (fake) milliseconds.
;;  SCALE is the multiplier for waiting time.
;;  SCALE > 1 slows down the simulation.
;;

(define  DEFAULT-WAIT    25) 
(define  SCALE  0.75)

(define  busy-wait
  (lambda  l
    (let* ([cur-time   (current-milliseconds)]
           [wait-time  (if (null? l) DEFAULT-WAIT (car l))]
           [new-time   (+ cur-time (* SCALE wait-time))])
      (recur  loop  ()
        (check-buttons)
        (yield)
        (when (< (current-milliseconds) new-time)
          (loop))))))

(define  check-buttons
  (lambda  ()
    (let ([click  (mouse-click?)])
      (when  click
        (process-click  click)
        (check-buttons)))))

(define  wait-for-button
  (lambda  ()
    (let ([click  (wait-for-click)])
      (process-click  click)
      (check-buttons))))

(define  process-click
  (lambda  (click)
    (recur  loop  ([n  1])
      (cond
        [(>  n  (max-floor))  (void)]
        [(click-here? click (UP-CALL-POSN n) CALL-WIDTH CALL-HEIGHT)
         (push-up-call  n)]
        [(click-here? click (DOWN-CALL-POSN n) CALL-WIDTH CALL-HEIGHT)
         (push-down-call  n)]
        [(click-here? click (DEMAND-POSN n) DEMAND-WIDTH DEMAND-HEIGHT)
         (push-demand  n)]
        [(click-here? click STOP-ORIGIN STOP-WIDTH STOP-HEIGHT)
         (push-stop)]
        [else  (loop (+ n 1))]))))

(define  click-here?
  (lambda  (click  origin  horiz  vert)
    (let* ([x0  (posn-x origin)]
           [y0  (posn-y origin)]
           [x   (posn-x (mouse-click-posn click))]
           [y   (posn-y (mouse-click-posn click))])
      (and  (<= x0 x (+ x0 horiz))
            (<= y0 y (+ y0 vert))))))

;;
;;  Info
;;  Just floor, goal, dir. 
;;

(define  INFO-ORIGIN  #f)
(define-struct einfo  (sym  posn  label  prev) #:mutable)

(define  INFO-LIST
  (list  (make-einfo  'floor  (make-posn 0 12)   "floor = "  #f)
         (make-einfo  'goal   (make-posn 0 28)  "goal = "   #f)
         (make-einfo  'dir    (make-posn 0 44)  "dir = "    #f)))

(define  init-info
  (lambda  ()
    (let  loop  ([l  INFO-LIST])
      (unless  (null? l)
        (set-einfo-prev!  (car l)  "")
        (loop  (cdr l))))
    (info  'floor  1)
    (info  'goal  1)
    (info  'dir  'none)))

(define  my-lookup
  (lambda  (sym)
    (let  loop  ([l  INFO-LIST])
      (cond
        [(null? l)  (error 'info "Unknown info type: ~e" sym)]
        [(eq? sym (einfo-sym (car l)))  (car l)]
        [else  (loop  (cdr l))]))))

(define  info
  (lambda  (sym  obj)
    (let* ([item  (my-lookup sym)]
           [posn  (add-posn  INFO-ORIGIN  (einfo-posn item))]
           [str   (if (string? obj) obj (format "~s" obj))]
           [full-str  (string-append (einfo-label item) str)])
      (unless  (string=?  full-str  (einfo-prev item))
        (c-string  posn  (einfo-prev item))
        (d-string  posn  full-str)
        (set-einfo-prev!  item  full-str)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                           ;;
;;  Functions to show to the outside world.  ;;
;;                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  This is really the (virtual) elevator hardware level.
;;  This includes state and the basic elevator operations.
;;  You write an elevator from these primitives.
;;
;;  This could be a separate module, but you'd just write all of
;;  these functions twice.  Also, they make nice guards for the
;;  graphics functions.

;;  Elevator State
;;
;;  the-floor = integer 1..THE-MAX-FLOOR
;;  the-doors = 'open, 'closed
;;  up-call-vec, down-call-vec, demand-vec = vectors 1..THE-MAX-FLOOR
;;     for buttons, #t = pushed, #f = not pushed

(define  max-floor  #f)
(define  the-floor  #f)
(define  the-doors  #f)
(define  up-call-vec  #f)
(define  down-call-vec  #f)
(define  demand-vec  #f)
(define  exit-continuation  #f)

;;  Initialize the hardware state and draw the picture.
;;  f = THE-MAX-FLOOR,  k = exit continuation

(define  make-elevator
  (lambda  (f  k)
    (let ([n  (add1 f)])
      (set!  max-floor  (lambda () f))
      (set!  exit-continuation  k)
      (set!  the-floor  1)
      (set!  the-doors  'open)
      (set!  up-call-vec    (build-vector n (lambda (i) #f)))
      (set!  down-call-vec  (build-vector n (lambda (i) #f)))
      (set!  demand-vec     (build-vector n (lambda (i) #f)))
      (init-graphics))))

(define  push-stop  
  (lambda  ()  (exit-continuation  'game-over)))

;;  Functions that use the-floor.
;;  Only (move-up-floor) and (move-down-floor) are allowed to use
;;  the-floor and THE-MAX-FLOOR directly.

(define  current-floor  (lambda  ()  the-floor))

(define  move-up-floor
  (lambda  ()
    (if  (=  the-floor  (max-floor))
         (error  'move-up-floor  "Elevator already at MAX-FLOOR")
         (begin  (info  'dir  'up)
                 ;;  (info  'floor  (format "~s~s" the-floor '+))
                 (when  (open?)  (close-doors))
                 (draw-up-floor)
                 (set!  the-floor  (add1 the-floor))
                 (info  'floor  the-floor)))))

(define  move-down-floor
  (lambda  ()
    (if  (=  the-floor  1)
         (error  'move-down-floor  "Elevator already at ground floor")
         (begin  (info  'dir  'down)
                 ;;  (info  'floor  (format "~s~s" the-floor '-))
                 (when  (open?)  (close-doors))
                 (draw-down-floor)
                 (set!  the-floor  (sub1 the-floor))
                 (info  'floor  the-floor)))))

;;  Functions that use the-doors.
;;  Again, only (open-doors) and (close-doors) are allowed to use
;;  the-doors directly.

(define  open?  (lambda  ()  (eq? the-doors 'open)))

(define  open-doors
  (lambda  ()
    (unless  (open?)
      (draw-open-doors)
      (set!  the-doors  'open))))

(define  close-doors
  (lambda  ()
    (when  (open?)
      (draw-close-doors)
      (set!  the-doors  'closed))))

;;  Functions that use buttons: up/down-calls, demands.
;;  Again, these are the only functions that are allowed to use
;;  up/down-calls and demands directly.

(define  up-call?    (lambda  (floor)  (vector-ref up-call-vec floor)))
(define  down-call?  (lambda  (floor)  (vector-ref down-call-vec floor)))
(define  demand?     (lambda  (floor)  (vector-ref demand-vec floor)))

(define  update-input  check-buttons)
(define  wait-for-input  wait-for-button)

(define  clear-up-call
  (lambda  (floor)
    (when  (up-call?  floor)
      (draw-clear-up  floor)
      (vector-set!  up-call-vec  floor  #f))))

(define  clear-down-call
  (lambda  (floor)
    (when  (down-call?  floor)
      (draw-clear-down  floor)
      (vector-set!  down-call-vec  floor  #f))))

(define  clear-demand
  (lambda  (floor)
    (when  (demand?  floor)
      (draw-clear-demand  floor)
      (vector-set!  demand-vec  floor  #f))))

;;  The push functions are not visible outside, but they need to
;;  be here because graphics calls them.

(define  push-up-call
  (lambda  (floor)
    (when  (not  (up-call? floor))
      (paint-up-call  floor)
      (vector-set!  up-call-vec  floor  #t))))

(define  push-down-call
  (lambda  (floor)
    (when  (not  (down-call? floor))
      (paint-down-call  floor)
      (vector-set!  down-call-vec  floor  #t))))

(define  push-demand
  (lambda  (floor)
    (when  (not  (demand? floor))
      (paint-demand  floor)
      (vector-set!  demand-vec  floor  #t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ;;
;;  SOFTWARE level for elevator.       ;;
;;                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Here, you design your own elevator, using the primitives from
;;  the graphics-module and the user's Control function.

(define  THE-MAX-FLOOR  8)

(define  start-program
  (lambda  ()
    (dynamic-wind
     (lambda ()
       (unless  (graphics-open?)  (open-graphics)))
     (lambda ()
       (call/cc
        (lambda  (k)
          (make-elevator  THE-MAX-FLOOR  k)
          (elevator  'open  #f  'up))))
     close-graphics)
    (lambda () #t)))

;;
;;  Main Loop.
;;
;;  This version gives the user complete control.
;;  All we do is ask the user for a goal, move one floor closer,
;;  check to see if we're at the goal (stop and open doors if so),
;;  and ask for another goal. 
;;  Any kind of fairness or ignoring the down-calls when moving up
;;  is totally up to Control.
;;
;;  floor, goal = integer 1..THE-MAX-FLOOR
;;  state = 'arrive, 'open, 'wait, 'move
;;    'arrive = just arrived at this floor, check if it's the goal
;;    'open = open doors, wait, ignore goal, get new one
;;    'wait = no requests, wait until there is one
;;    'move = start moving toward the goal
;;  dir = 'up, 'down
;;
;;  We call new-goal at each floor.
;;

(define  OPEN-DOOR-WAIT-TIME  1500)

(define  elevator
  (lambda  (state  goal  dir)
    (let ([floor  (current-floor)])
      (cond
        [(eq?  state  'arrive)
         (if  (=  floor  goal)
              (elevator  'open  goal  dir)
              (elevator  'move  (new-goal dir)  dir))]
        [(eq?  state  'open) 
         (begin
           (open-doors)
           (clear-all-buttons  floor)
           (busy-wait  OPEN-DOOR-WAIT-TIME)
           (update-input)
           (clear-all-buttons  floor)
           (elevator  'wait  (new-goal dir)  dir))]
        [(eq?  state  'wait)
         (if  (=  floor  goal)
              (begin
                (update-input)
                (clear-all-buttons  floor)
                (wait-for-request)
                (elevator  'wait  (new-goal dir)  dir))
              (elevator  'move  goal  dir))]
        [(=  goal  floor)  
         (elevator  'open  goal  dir)]
        [(<  goal  floor)
         (begin  
           (move-down-floor)
           (elevator  'arrive  goal  'down))]
        [(>  goal  floor)
         (begin  
           (move-up-floor)
           (elevator  'arrive  goal  'up))]
        [else  (error 'elevator "Internal error in main loop")]))))

;;  Don't get stuck on the same floor forever.

(define  clear-all-buttons
  (lambda  (floor) 
    (clear-up-call  floor)
    (clear-down-call  floor) 
    (clear-demand  floor)))

;;  Don't return until at least one button is pushed.

(define  wait-for-request
  (lambda  ()
    (recur  loop  ([n  1])
      (cond
        [(> n (max-floor))  (wait-for-input)]
        [(or  (up-call? n)  (down-call? n)  (demand? n))  #t]
        [else  (loop  (add1 n))]))))

;;  Call the user's function Control and check that the floor is valid.

(define  list-of-floors
  (lambda  ()
    (recur  loop  ([f  (max-floor)]  [l  null])
      (cond
        [(= f 0)  l]
        [(or (up-call? f) (down-call? f) (demand? f))
         (loop  (sub1 f)  (cons f l))]
        [else  (loop  (sub1 f)  l)]))))

(define  new-goal
  (lambda  (dir)
    (update-input)
    (let 
        ([ans  (Next-Floor  dir (current-floor)  (list-of-floors))]) 
      (if  (and  (integer? ans)  (exact? ans)  (<= 1 ans (max-floor)))
           (begin  (info  'goal  ans)  ans)
           (error 'Next-Floor "~e is not a valid floor number" ans)))))

;;  Functions to show the user.
;;  Remember, the elevator calls Next-Floor, not the other way.

(define Next-Floor (lambda x (error 'Next-Floor "undefined")))

(define  run/proc 
  (lambda (f)
    (check-proc 'run f 3 'first "3 arguments")
    (set! Next-Floor f)
    (start-program)))
