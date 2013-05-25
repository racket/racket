#lang scheme/base

#|
 workshop experience: 

   stop-when should have a different signature:
    stop-when : (World -> Boolean) (World -> Image) 

   (stop-when C F) should have this semantics:
    on each assignment to world,
    (begin 
      (set! current-world new-world)
      (when (C current-world) (render (F current-world))))
|#
;; Thu Aug 28 15:54:23 EDT 2008: big-bang can now be re-run after the world
;;                               has stopped

;; Tue Aug 12 08:54:45 EDT 2008: ke=? changed to key=? 
;; Fri Jul  4 10:25:47 EDT 2008: added ke=? and key-event? 
;; Mon Jun 16 15:38:14 EDT 2008: removed end-of-time and provided stop-when 
;;                               also allow repeated setting of callbacks now
;;                               If this is changed back, stop-when will fail

;; Wed Apr 23 11:42:25 EDT 2008: fixed reverse bug in animation 
;; Thu Mar 20 17:15:54 EDT 2008: fixed place-image0, which used shrink off-by-1
;; Mon Sep 17 09:40:39 EDT 2007: run-simulation now allows recordings, too
;; Mon Aug  6 19:50:30 EDT 2007: exporting both add-line from "image.rkt" and scene+line 
;; Fri May  4 18:05:33 EDT 2007: define-run-time-path 
;; Thu May  3 22:06:16 EDT 2007: scene # image; pasteboard% for text%
;; Sat Apr 28 13:31:02 EDT 2007: fixed the image and animated-gif thing, using Matthew's lib
;; Fri Dec 22 11:51:53 EST 2006: cleaned up the callback code with macro
;; Thu Dec 21 13:59:23 EST 2006: fixed add-line and place-image to accept numbers
;; Wed Dec 20 18:17:03 EST 2006: recording events and creating images

#|
At Thu, 21 Dec 2006 14:10:35 -0500, Matthias Felleisen wrote:
2. The history mechanism collects a record in a list for every event.
    This means say 30 tick events per second, plus mice and keyboard  
    callbacks.
    Say we get 50 events at the upper limit per second.
    After playing for one minute, the event list contains 3,000 records.
    After playing for ten minutes, the event list contains 30,000 records.
    Each record consists of, on the average, 3 numbers, so it's like gathering
    a list of 100,000 numbers.

    Is this going to become a bottleneck?

That's a largish list. It could only get that big with mouse-motion
events, right?

I suggest that when you receive three mouse-motion events in a row,
drop the middle one, unless the time between the middle one and the
oldest one is greater than 100 msecs. (Dropping the middle one means
that you keep the endpoints, which are likely to be the interesting
ones.)

Matthew
|#

;; Sun Dec 09 23:17:41 EST 2006: add-line fixed so it cuts off lines before drawing
;; Mon Mar 27 10:29:28 EST 2006: integrated Felix's mouse events
;; Wed Jan 25 13:38:42 EST 2006: on-redraw: proc is now called on installation
;; Tue Jan  3 11:17:50 EST 2006: changed add-line behavior in "world.rkt"
;; Sat Dec 10 19:39:03 EST 2005: fixed name, changed interface to on-key-event
;; Fri Dec  9 21:39:03 EST 2005: remoevd (update ... produce ...); added on-redraw 
;; Thu Dec  1 17:03:03 EST 2005: fixed place-image; all coordinates okay now

(require scheme/class
         scheme/local
         scheme/bool
         (except-in mred make-color)
         htdp/error
         htdp/image
         mrlib/cache-image-snip
         lang/prim
         lang/private/rewrite-error-message
         (for-syntax scheme/base))

(require mrlib/gif)
(require mzlib/runtime-path)

(require mrlib/bitmap-label
         string-constants)

;; --- provide ---------------------------------------------------------------


;                                                   
;                                                   
;   ;;;;                         ;         ;        
;   ;   ;                                  ;        
;   ;   ;                                  ;        
;   ;   ;  ; ;;;   ;;;   ;   ; ;;;      ;;;;   ;;;  
;   ;  ;   ;;  ;  ;   ;  ;   ;   ;     ;   ;  ;   ; 
;   ;;;    ;      ;   ;   ; ;    ;     ;   ;  ;;;;; 
;   ;      ;      ;   ;   ; ;    ;     ;   ;  ;     
;   ;      ;      ;   ;   ; ;    ;     ;  ;;  ;     
;   ;      ;       ;;;     ;     ;;;    ;; ;   ;;;; 
;                                                   
;                                                   
;                                                   


;; image manipulation functions:
;; =============================
(provide (all-from-out htdp/image))

;; world manipulation functions: 
;; =============================
(provide      ;; forall(World):
 big-bang     ;; Number Number Number World [Boolean] -> true
 )

(provide-higher-order-primitive
 on-tick-event (tock) ;; (World -> World) -> true
 )

(provide-higher-order-primitive
 on-redraw (world-to-image) ;; (World -> Image) -> true
 )

;; KeyEvent is one of: 
;; -- Char 
;; -- Symbol 

(provide
  key-event? ;; Any -> Boolean
  key=? ;; KeyEvent KeyEvent -> Boolean
 )

(provide-higher-order-primitive
 on-key-event (control) ;; (World KeyEvent -> World) -> true
 )

;; A MouseEventType is one of:
;; - 'button-down
;; - 'button-up
;; - 'drag
;; - 'move
;; - 'enter
;; - 'leave

(provide-higher-order-primitive
 on-mouse-event (clack)  ;; (World Number Number MouseEvent -> World) -> true
 )

(provide-higher-order-primitive
 stop-when (last-world)  ;; (World -> Boolean) -> true
 )

(provide-higher-order-primitive
 run-simulation (_ _ _ create-scene) ; (Number Number Number (Nat -> Scene) -> true)
 )

(provide
 run-movie ;; [Listof Image] -> true 
 )

;; ---------------------------------------------------------------------------


;                                                                        
;                                                                        
;   ;;;;;                                           ;  ;  ;;;;         ; 
;   ;                                    ;          ;  ;  ;  ;         ; 
;   ;                                    ;          ;  ;  ;  ;         ; 
;   ;      ;  ;   ; ;;    ;;;   ; ;;;  ;;;;;        ; ; ; ;  ;      ;;;; 
;   ;;;;;  ;  ;   ;;  ;  ;   ;  ;;  ;    ;          ; ; ; ;  ;     ;   ; 
;   ;       ;;    ;   ;  ;   ;  ;        ;           ;; ;;   ;     ;   ; 
;   ;       ;;    ;   ;  ;   ;  ;        ;           ;   ;   ;     ;   ; 
;   ;      ;  ;   ;   ;  ;   ;  ;        ;           ;   ;   ;     ;  ;; 
;   ;;;;;  ;  ;   ;;;;    ;;;   ;         ;;         ;   ;   ;;;    ;; ; 
;                 ;                                                      
;                 ;                                                      
;                                                                        

;; Number Number Number World [Boolean] -> true
;; create the visible world (canvas)
(define big-bang
  (lambda x 
    (define args (length x))
    (if (or (= args 5) (= args 4))
        (apply big-bang0 x) 
        (tp-error 'big-bang "expects 4 or 5 arguments, given ~a" args))))

(define *running?* #f)
(define big-bang0
  (case-lambda 
    [(w h delta world) (big-bang w h delta world #f)]
    [(w h delta world animated-gif) 
     (check-size/0 'big-bang w "first")
     (check-size/0 'big-bang h "second")
     ;; ============================================
     ;; WHAT IF THEY ARE NOT INTs?
     ;; ============================================
     (check-arg 'big-bang
                (and (number? delta) (<= 0 delta 1000))
                "number of seconds between 0 and 1000"
                "third"
                delta)
     (check-arg 'big-bang 
                (boolean? animated-gif)
                "boolean"
                "fifth"
                animated-gif)
     (let ([w (coerce w)]
           [h (coerce h)])
       (when *running?*  (error 'big-bang "the world is still running, cannot start another world"))
       (set! *running?* #t)
       (callback-stop!)
       ;; (when (vw-init?) (error 'big-bang "big-bang already called once"))
       (install-world delta world) ;; call first to establish a visible world
       (set-and-show-frame w h animated-gif) ;; now show it
       (unless animated-gif (set! add-event void)) ;; no recording if image undesired
       (set! *the-delta* delta)
       #t)]))

;; Number -> Int 
(define (coerce x) (inexact->exact (floor x)))

(define *the-delta* 0.0)

(define (on-tick-event f)
  (check-proc 'on-tick-event f 1 "on-tick-event" "one argument")
  (check-world 'on-tick-event)
  (set-timer-callback f)
  (send the-time start
        (let* ([w (ceiling (* 1000 the-delta))])
          (if (exact? w) w (inexact->exact w))))
  #t)

(define (on-redraw f)
  (check-proc 'on-redraw f 1 "on-redraw" "one argument")
  (check-world 'on-redraw)
  (set-redraw-callback f)
  (redraw-callback)
  #t)

(define (key-event? k)
  (or (char? k) (symbol? k)))

(define (key=? k m)
  (check-arg 'key=? (key-event? k) 'KeyEvent "first" k)
  (check-arg 'key=? (key-event? m) 'KeyEvent "first" m)
  (eqv? k m))

(define (on-key-event f)
  (check-proc 'on-key-event f 2 "on-key-event" "two arguments")
  (check-world 'on-key-event)
  (set-key-callback f (current-eventspace))
  #t)

(define (on-mouse-event f)
  (check-proc 'on-mouse-event f 4 "on-mouse-event" "four arguments")
  (check-world 'on-mouse-event)
  (set-mouse-callback f (current-eventspace))
  #t)

(define (stop-when f)
  (check-proc 'stop-when f 1 "stop-when" "one argument")
  (check-world 'stop-when)
  (set-stop-when-callback f)
  #t)

(define (run-movie movie)
  (check-arg 'run-movie (list? movie) "list (of images)" "first" movie)
  (for-each (lambda (cand) 
              (check-image 'run-movie cand "first" "list of images"))
            movie)
  (let* ([fst (car movie)]
         [wdt (image-width fst)]
         [hgt (image-height fst)])
    (big-bang wdt hgt (/ 1 27) movie)
    (let run-movie ([movie movie])
      (cond
        [(null? movie) #t]
        [(pair? movie)
         (update-frame (car movie))
         (sleep/yield .05)
         (run-movie (cdr movie))]))))

(define run-simulation 
  (lambda x 
    (define args (length x))
    (if (or (= args 5) (= args 4))
        (apply run-simulation0 x) 
        (tp-error 'run-simulation "expects 4 or 5 arguments, given ~a" args))))


(define run-simulation0
  (case-lambda
    [(width height rate f record?)
     (check-size/0 'run-simulation width "first")
     (check-size/0 'run-simulation height "second")
     (check-arg 'run-simulation (number? rate) 'number "third" rate)
     (check-proc 'run-simulation f 1 "fourth" "one argument")
     (check-arg 'run-simulation (boolean? record?) 'number "fifth [and optional]" record?)
     (big-bang width height rate 1 record?)
     (on-redraw f)
     (on-tick-event add1)]
    [(width height rate f)
     (run-simulation width height rate f #f)]))

;; ---------------------------------------------------------------------------

;                                     
;                                     
;     ;;;  ;                    ;     
;    ;     ;                    ;     
;   ;      ;                    ;     
;   ;      ; ;;    ;;;    ;;;;  ;   ; 
;   ;      ;;  ;  ;   ;  ;      ;  ;  
;   ;      ;   ;  ;;;;;  ;      ; ;   
;   ;      ;   ;  ;      ;      ;;;   
;    ;     ;   ;  ;      ;      ;  ;  
;     ;;;  ;   ;   ;;;;   ;;;;  ;   ; 
;                                     
;                                     
;                                     

;; Symbol Any String -> Void
(define (check-size/0 tag c rank)
  (check-arg tag (and (number? c) (>= (coerce c) 0))
             "natural number" rank c))

;; Symbol Any String String *-> Void
(define (check-image tag i rank . other-message)
  (if (and (pair? other-message) (string? (car other-message)))
      (check-arg tag (image? i) (car other-message) rank i)
      (check-arg tag (image? i) "image" rank i)))

;; Symbol Any String -> Void
(define (check-scene tag i rank)
  (if (image? i)
      (unless (scene? i)
        (tp-error tag "expects a scene, given image whose pinhole is at (~s,~s) instead of (0,0)"
               (pinhole-x i) (pinhole-y i)))
      (check-arg tag #f "image" rank i)))

;; Symbol Any String -> Void
(define (check-sym/string-color tag width rank)
  (check-arg tag (or (symbol? width) (string? width)) 
             "color symbol or string" rank width))

;; Symbol (union Symbol String) Nat -> Void
(define (check-mode tag s rank)
  (check-arg tag (or (eq? s 'solid)
                     (eq? s 'outline)
                     (string=? "solid" s)
                     (string=? "outline" s)) "mode (solid or outline)" rank s))

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

;; Image Number Number Image -> Image 
#;
(define (place-image0 image x y scene)
  (define sw (image-width scene))
  (define sh (image-height scene))
  (define ns (overlay/xy scene x y image))
  (define nw (image-width ns))
  (define nh (image-height ns))
  (if (and (= sw nw) (= sh nh)) ns (shrink ns 0 0 sw sh)))

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
    [(and (<= 0 x0 w) (<= 0 x1 w) (<= 0 y0 w) (<= 0 y1 w))
     (shrink (add-line img x0 y0 x1 y1 c) 0 0 (- w 1) (- h 1))]
    [(= x0 x1) ;; vertical that leaves bounds 
     (if (<= 0 x0 w) (add-line img x0 (app y0 h) x0 (app y1 h) c) img)]
    [(= y0 y1) ;; horizontal that leaves bounds 
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
            [(upper-right) (if (number? upp) (add upp 0) (add w rgt))]
            [(lower-right) (if (number? low) (add low h) (add w rgt))]
            [else (tp-error
                   'dir
                   "expected a symbol for the direction, such as 'upper-left 'lower-right 'upper-right or 'lower-right, given ~a"
                   dir)])]
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
;; ---------------------------------------------------------------------------


;                                     
;                                     
;  ;  ;  ;              ;;;         ; 
;  ;  ;  ;                ;         ; 
;  ;  ;  ;                ;         ; 
;  ; ; ; ;  ;;;   ; ;;;   ;      ;;;; 
;  ; ; ; ; ;   ;  ;;  ;   ;     ;   ; 
;   ;; ;;  ;   ;  ;       ;     ;   ; 
;   ;   ;  ;   ;  ;       ;     ;   ; 
;   ;   ;  ;   ;  ;       ;     ;  ;; 
;   ;   ;   ;;;   ;       ;;;    ;; ; 
;                                     
;                                     
;                                     

(define unique-world (cons 1 1))
(define (check-world tag)
  (when (eq? unique-world the-world) 
    (tp-error tag "big-bang has not been called before calling ~a" tag)))

(define the-world unique-world)
(define the-world0 unique-world)

;; Nat World -> Void
;; effects: init event-history, the-delta, the-world, the-world0
(define (install-world delta w)
  (reset-event-history)
  (set! the-delta delta)
  (set! the-world w)
  (set! the-world0 w)
  (vw-setup))

;; Number > 0
;; the rate of at which the clock ticks 
(define the-delta 1000)

;; Text-- The One and Only Visible World
(define visible-world #f)

;; Bool -> Void
(define (vw-setup)
  (set! visible-world (new pasteboard%))
  (send visible-world set-cursor (make-object cursor% 'arrow)))

;; -> Boolean 
(define (vw-init?) (is-a? visible-world pasteboard%))

;; Image -> Void
;; show the image in the visible world
(define (update-frame pict)
  (send visible-world begin-edit-sequence)
  (send visible-world lock #f)
  (let ([s (send visible-world find-first-snip)])
    (when s
      (send visible-world delete s)))
  (let ([c (send visible-world get-canvas)])
    (let-values ([(px py)
                  (if (is-a? pict cache-image-snip%)
                      (send pict get-pinhole)
                      (values 0 0))]
                 [(cw ch)
                  (send c get-client-size)])
      (send visible-world insert (send pict copy) (- px) (- py))))
  (send visible-world lock #t)
  (send visible-world end-edit-sequence))

;; Nat Nat Boolean -> Void
;; effect: create, show and set the-frame
;; assume: visible-world is a pasteboard%, i.e., install-world has been called. 
(define (set-and-show-frame w h animated-gif)
  (define the-play-back-custodian (make-custodian))
  (define frame (create-frame the-play-back-custodian))
  (set! WIDTH w)
  (set! HEIGHT h)
  (when animated-gif
    (add-stop-and-image-buttons frame the-play-back-custodian))
  (add-editor-canvas frame visible-world w h)
  (send frame show #t))

(define WIDTH 0) 
(define HEIGHT 0)

;; [Box (union false Thread)] -> Frame
;; create a frame that shuts down the custodian on close
(define (create-frame the-play-back-custodian)
  (new (class frame%
         (super-new)
         (define/augment (on-close)  
           (callback-stop!)
           (custodian-shutdown-all the-play-back-custodian)))
       (label "DrRacket")
       (stretchable-width #f)
       (stretchable-height #f)
       (style '(no-resize-border metal))))

;; Frame [Box (union false Thread)] -> Void
;; adds the stop animation and image creation button, 
;; whose callbacks runs as a thread in the custodian
(define IMAGES "Images")
(define-runtime-path s:pth '(lib "icons/break.png"))
(define-runtime-path i:pth '(lib "icons/file.gif"))
(define (add-stop-and-image-buttons  frame the-play-back-custodian)
  (define p (new horizontal-pane% [parent frame][alignment '(center center)]))
  (define S ((bitmap-label-maker (string-constant break-button-label) s:pth) '_))
  (define I ((bitmap-label-maker IMAGES i:pth) '_))
  (define stop-button
    (new button% [parent p] [label S] [style '(border)]
         [callback (lambda (this-button e) 
                     (callback-stop!)
                     (send this-button enable #f)
                     (send image-button enable #t))]))
  (define image-button 
    (new button% [parent p] [enabled #f] [label I] [style '(border)]
         [callback (lambda (b e)
                     (parameterize ([current-custodian the-play-back-custodian])
                       (define th (thread play-back))
                       (send b enable #f)))]))
  (void))

;; Frame Editor Nat Nat -> Void
;; adds the visible wold to the frame and hooks it up with the callbacks
(define (add-editor-canvas frame visible-world w h)
  (define c 
    (new (class editor-canvas%
           (super-new)
           (define/override (on-char e) (key-callback (send e get-key-code)))
           (define/override (on-event e) (mouse-callback e)))
         (parent frame)
         (editor visible-world)
         (style '(no-hscroll no-vscroll))
         (horizontal-inset INSET)
         (vertical-inset INSET)))
  (send c min-client-width (+ w INSET INSET))
  (send c min-client-height (+ h INSET INSET))
  (send c focus))

;; Amount of space around the image in the world window:
(define INSET 5)

;                                                                                      
;                                                                                      
;  ;;;;;;                                    ;;;;;                                  ;; 
;   ;   ;                        ;            ;   ;                                  ; 
;   ; ;   ;;; ;;;  ;;;  ;; ;;   ;;;;;         ;   ;   ;;;    ;;;;   ;;;   ;; ;;   ;; ; 
;   ;;;    ;   ;  ;   ;  ;;  ;   ;            ;   ;  ;   ;  ;   ;  ;   ;   ;;    ;  ;; 
;   ; ;    ;   ;  ;;;;;  ;   ;   ;            ;;;;   ;;;;;  ;      ;   ;   ;     ;   ; 
;   ;       ; ;   ;      ;   ;   ;            ;  ;   ;      ;      ;   ;   ;     ;   ; 
;   ;   ;   ; ;   ;      ;   ;   ;   ;        ;   ;  ;      ;   ;  ;   ;   ;     ;   ; 
;  ;;;;;;    ;     ;;;; ;;; ;;;   ;;;        ;;;   ;  ;;;;   ;;;    ;;;   ;;;;;   ;;;;;
;                                                                                      
;                                                                                      
;                                                                                      
;                                                                                      

(define TICK 'tick)
(define MOUSE 'mouse)
(define KEY 'key)
;; Evt =   (list utick) 
;;       | (list KEY (union Char Symbol)) 
;;       | (list MOUSE MouseEventType)
;; [Listof Evt]
(define event-history '())

;; -> Void 
(define (reset-event-history)
  (set! event-history '()))

;; Symbol  Any *-> Void
(define (add-event type . stuff)
  (set! event-history (cons (cons type stuff) event-history)))


;; zfill: natural-number natural-number -> string
;; Converts a-num to a string, adding leading zeros to make it at least as long as a-len.
(define (zfill a-num a-len)
  (let ([n (number->string a-num)])
    (string-append (build-string (max (- a-len (string-length n)) 0)
                                 (lambda (i) #\0))
                   n)))

;; --> Void
;; re-play the history of events, creating a png per step, create animated gif
;; effect: write to user-chosen file 
(define (play-back)
  ;; --- state transitions 
  (define (world-transition world fst)
    (case (car fst)
      [(tick)  (timer-callback0 world)]
      [(key)   (key-callback0 world (cadr fst))]
      [(mouse) (mouse-callback0 world (cadr fst) (caddr fst) (cadddr fst))]
      [else (tp-error 'play-back "bad type of event: ~s" fst)]))
  ;; --- creating images 
  (define total (+ (length event-history) 1))
  (define image-count 0)
  (define bitmap-list '())
  (define (save-image img)
    (define-values (w h) (send img get-size)) 
    (define (make-bitmap)
      (define bm (make-object bitmap% w h))
      (define dc (make-object bitmap-dc% bm))
      (send dc clear)
      (send img draw dc 0 0 0 0 w h 0 0 #f)
      bm)
    (define bm (make-bitmap)) 
    (set! bitmap-list (cons bm bitmap-list))
    (set! image-count (+ image-count 1))
    (send bm save-file (format "i~a.png" (zfill image-count (string-length (number->string total)))) 'png))
  ;; --- choose place 
  (define target:dir
    (let* ([cd (current-directory)]
           [dd (get-directory "Select directory for images" #f cd)])
      (if dd dd cd)))
  (parameterize ([current-directory target:dir])
    (let replay ([ev (reverse event-history)][world the-world0])
      (define img (redraw-callback0 world))
      (update-frame (text (format "~a/~a created" image-count total) 18 'red))
      (save-image img)
      (cond
        [(null? ev)
         (update-frame (text (format "creating ~a" ANIMATED-GIF-FILE) 18 'red))
         (create-animated-gif (reverse bitmap-list))
         (update-frame img)]
        [else
         (let ([world1 (world-transition world (car ev))])
           (replay (cdr ev) world1))]))))

;; [Listof (-> bitmap)] -> Void
;; turn the list of thunks into animated gifs 
;; effect: overwrite the ANIMATED-GIF-FILE (in current directory)
(define (create-animated-gif bitmap-list)
  (define intv (if (> +inf.0 *the-delta* 0) (inexact->exact (floor (* 100 *the-delta*))) 5))
  (when (file-exists? ANIMATED-GIF-FILE)
    (delete-file ANIMATED-GIF-FILE))
  (write-animated-gif bitmap-list intv ANIMATED-GIF-FILE #:one-at-a-time? #t #:loop? #f))

(define ANIMATED-GIF-FILE "i-animated.gif")


;                                                                 
;                                                                 
;     ;;;        ;;;    ;;;     ;                    ;            
;    ;             ;      ;     ;                    ;            
;   ;              ;      ;     ;                    ;            
;   ;       ;;;;   ;      ;     ; ;;    ;;;;   ;;;;  ;   ;   ;;;  
;   ;      ;   ;   ;      ;     ;;  ;  ;   ;  ;      ;  ;   ;   ; 
;   ;      ;   ;   ;      ;     ;   ;  ;   ;  ;      ; ;     ;;   
;   ;      ;   ;   ;      ;     ;   ;  ;   ;  ;      ;;;       ;  
;    ;     ;  ;;   ;      ;     ;   ;  ;  ;;  ;      ;  ;   ;   ; 
;     ;;;   ;; ;   ;;;    ;;;   ;;;;    ;; ;   ;;;;  ;   ;   ;;;  
;                                                                 
;                                                                 
;                                                                 

;; callbacks: timer, mouse, key, redraw, stop-when 

;; Definition = (define-callback Symbol String Symbol Expression ...)
;; effect: (define-callback introduces three names: name, name0, set-name
(define-syntax (define-callback stx)
  (syntax-case stx ()
    [(_ n msg (f esp ...) para body ...)
     (let* ([n:str (symbol->string (syntax-e (syntax n)))]
            [callback (lambda (before after)
                        (string->symbol 
                         (string-append before n:str "-callback" after)))]
            [name (datum->syntax stx (callback "" ""))]
            [name0 (datum->syntax stx (callback "" "0"))]
            [set-name (datum->syntax stx (callback "set-" ""))])
       #`(define-values (#,name #,name0 #,set-name)
           (values 
            void void 
            (lambda (f esp ...)
              #;
              (when (callback-set? #,name) 
                (error (format "the ~a has already been specified") msg))
              (set! #,name0 f)
              (set! #,name (lambda para body ...))))))]))

;; -> Void
(define (callback-stop!)
  (send the-time stop)
  (set! timer-callback void)
  (set! mouse-callback void)
  (set! key-callback void)
  (set! stop-when-callback (lambda () #f))
  (set! redraw-callback void)
  (set! *running?* #f))

;; Any -> Boolean
;; is the callback set to the default value 
(define (callback-set? cb) (not (eq? cb void)))

;; Timer
(define the-time (new timer% [notify-callback (lambda () (timer-callback))]))

;; f : [World -> World]
(define-callback timer "tick-event hander" (f) ()
  (with-handlers ([exn:break? break-handler][exn? exn-handler])
    (set! the-world (f the-world))
    (add-event TICK)
    (redraw-callback)))

;; f : [World -> Image]
(define-callback redraw "redraw function" (f) ()
  (with-handlers ([exn:break? break-handler][exn? exn-handler])
    (define result (f the-world))
    (define fname (object-name f))
    (define tname (if fname fname 'your-redraw-function))
    (if (image? result)
        (check-result tname scene? "scene" result
                      (format "image with pinhole at (~s,~s)"
                              (pinhole-x result) (pinhole-y result)))
        (check-result tname (lambda (x) (image? x)) "scene" result))
    (update-frame result)
    ;; if this world is the last one, stop the world
    (when (stop-when-callback)
      (callback-stop!))))

;; f : [World -> Boolean]
(define-callback stop-when "is end of world check" (f) ()
  (define result (f the-world))
  (define fname (object-name f))
  (define tname (if fname fname 'your-redraw-function))
  (check-result fname boolean? "boolean" result)
  result)

;; f : [World KeyEvent -> World]
;; esp : EventSpace 
;; e : KeyEvent 
(define-callback key "key-event handler" (f evt-space) (e)
  (parameterize ([current-eventspace evt-space])
    (queue-callback 
     (lambda ()
       (with-handlers ([exn:break? break-handler][exn? exn-handler])
         (let ([new-world (f the-world e)])
           (unless (equal? new-world the-world)
             (set! the-world new-world)
             (add-event KEY e)
             (redraw-callback))))))))

;; f : [World Nat Nat MouseEventType -> World]
;; esp : EventSpace 
;; e : MouseEvent
(define-callback mouse "mouse event handler" (f evt-space) (e)
  (parameterize ([current-eventspace evt-space])
    (queue-callback
     (lambda ()
       (define x (- (send e get-x) INSET))
       (define y (- (send e get-y) INSET))
       (define m (mouse-event->symbol e))
       (when (and (<= 0 x WIDTH) (<= 0 y HEIGHT))
         (with-handlers ([exn:break? break-handler][exn? exn-handler])
           (let ([new-world (f the-world x y m)])
             (unless (equal? new-world the-world)
               (set! the-world new-world)
               (add-event MOUSE x y m)
               (redraw-callback)))))))))

;; MouseEvent -> MouseEventType
(define (mouse-event->symbol e)
  (cond [(send e button-down?) 'button-down]
        [(send e button-up?)   'button-up]
        [(send e dragging?)    'drag]
        [(send e moving?)      'move]
        [(send e entering?)    'enter]
        [(send e leaving?)     'leave]
        [else ; (send e get-event-type)
         (tp-error 'on-mouse-event
                (format 
                 "Unknown event type: ~a"
                 (send e get-event-type)))]))

;; --- library 
(define (exn-handler e)
  (callback-stop!)
  (raise e))

(define (break-handler . _) 
  (printf "animation stopped")
  (callback-stop!)
  the-world)

;; Number -> Integer
(define (number->integer x)
  (inexact->exact (floor x)))

