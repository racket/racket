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

;; Mon Sep 17 09:40:39 EDT 2007: run-simulation now allows recordings, too
;; Mon Aug  6 19:50:30 EDT 2007: exporting both add-line from image.ss and scene+line 
;; Fri May  4 18:05:33 EDT 2007: define-run-time-path 
;; Thu May  3 22:06:16 EDT 2007: scene # image; pasteboard% for text%
;; Sat Apr 28 13:31:02 EDT 2007: fixed the image and animated-gif thing, using Matthew's lib
;; Fri Dec 22 11:51:53 EST 2006: cleaned up the callback code with macro
;; Thu Dec 21 13:59:23 EST 2006: fixed add-line and place-image to accept numbers
;; Wed Dec 20 18:17:03 EST 2006: recording events and creating images
;; Sun Dec 09 23:17:41 EST 2006: add-line fixed so it cuts off lines before drawing
;; Mon Mar 27 10:29:28 EST 2006: integrated Felix's mouse events
;; Wed Jan 25 13:38:42 EST 2006: on-redraw: proc is now called on installation
;; Tue Jan  3 11:17:50 EST 2006: changed add-line behavior in world.ss 
;; Sat Dec 10 19:39:03 EST 2005: fixed name, changed interface to on-key-event
;; Fri Dec  9 21:39:03 EST 2005: remoevd (update ... produce ...); added on-redraw 
;; Thu Dec  1 17:03:03 EST 2005: fixed place-image; all coordinates okay now
(module world mzscheme
  (require (lib "class.ss")
           (lib "etc.ss")
           (lib "mred.ss" "mred")
           (lib "error.ss" "htdp")
           (lib "image.ss" "htdp")
           (only (lib "htdp-beginner.ss" "lang") image?)
           (lib "cache-image-snip.ss" "mrlib")
           (lib "prim.ss" "lang"))
  
  (require (lib "gif.ss" "mrlib"))
  (require (lib "runtime-path.ss"))
  
  (require (lib "bitmap-label.ss" "mrlib")
           (lib "string-constant.ss" "string-constants"))
  
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
  (provide (all-from (lib "image.ss" "htdp")))
  
  (provide
    ;; Scene is Image with pinhole in origin 
   nw:rectangle ;; Number Number Mode Color -> Image
   place-image  ;; Image Number Number Scene -> Scene
   empty-scene  ;; Number Number -> Scene 
   scene+line   ;; Scene Number Number Number Number Color -> Scene 
   ;; cut all pieces that are outside the given rectangle 
   )
  
  ;; world manipulation functions: 
  ;; =============================
  (provide      ;; forall(World):
   big-bang	;; Number Number Number World [Boolean] -> true
   end-of-time  ;; String u Symbol -> World
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
   run-simulation (_ _ _ create-scene) ; (Number Number Number (Nat -> Scene) -> true)
   )
  
  (provide
   run-movie ;; [Listof Image] -> true 
   )
  
  ;; ---------------------------------------------------------------------------
  
  ;                                                                             
  ;                                                                             
  ;   ;;;;;                                                  ;;;;;               
  ;   ;                                    ;                  ;                 
  ;   ;                                    ;                  ;                 
  ;   ;      ;  ;   ; ;;    ;;;   ; ;;;  ;;;;;   ;            ;   ;;; ;    ;;;; 
  ;   ;;;;;  ;  ;   ;;  ;  ;   ;  ;;  ;    ;     ;            ;   ; ;;;   ;   ; 
  ;   ;       ;;    ;   ;  ;   ;  ;        ;                  ;   ; ; ;   ;   ; 
  ;   ;       ;;    ;   ;  ;   ;  ;        ;                  ;   ; ; ;   ;   ; 
  ;   ;      ;  ;   ;   ;  ;   ;  ;        ;     ;            ;   ; ; ;   ;  ;; 
  ;   ;;;;;  ;  ;   ;;;;    ;;;   ;         ;;   ;          ;;;;; ; ; ;    ;; ; 
  ;                 ;                                                         ; 
  ;                 ;                                                      ;;;; 
  ;                                                                              
  
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
          (error 'big-bang msg))))
  (define msg
    (string-append
     "big-bang consumes 4 or 5 arguments:\n"
     "-- (big-bang <width> <height> <rate> <world0>)\n"
     "-- (big-bang <width> <height> <rate> <world0> <animated-gif>)\n"
     "see Help Desk."))
    
  (define big-bang0
    (case-lambda 
      [(w h delta world) (big-bang w h delta world #f)]
      [(w h delta world animated-gif) 
       (check-pos 'big-bang w "first")
       (check-pos 'big-bang h "second")
       ;; ============================================
       ;; WHAT IF THEY ARE NOT INTs?
       ;; ============================================
       (check-arg 'big-bang
                  (and (number? delta) (<= 0 delta 1000))
                  "number [of seconds] between 0 and 1000"
                  "third"
                  delta)
       (check-arg 'big-bang 
                  (boolean? animated-gif)
                  "boolean expected"
                  "fifth"
                  animated-gif)
       (let ([w (coerce w)]
	     [h (coerce h)])
	 (when (vw-init?) (error 'big-bang "big-bang already called once"))
	 (install-world delta world) ;; call first to establish a visible world
	 (set-and-show-frame w h animated-gif) ;; now show it
	 (unless animated-gif (set! add-event void)) ;; no recording if image undesired
	 (set! *the-delta* delta)
	 #t)]))

  ;; Number -> Int 
  (define (coerce x) (inexact->exact (floor x)))

  (define *the-delta* 0.0)
  
  (define (end-of-time s)
    (printf "end of time: ~a~n" s)
    (callback-stop!)
    the-world)
  
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
  
  (define (run-movie movie)
    (check-arg 'run-movie (list? movie) "list (of images)" "first" movie)
    (for-each (lambda (cand) 
                (check-image 'run-movie cand "first" "list of images"))
              movie)
    (let* ([fst (car movie)]
	   [wdt (image-width fst)]
	   [hgt (image-height fst)]
	   [nxt (lambda (w) (if (null? w) (end-of-time "") (cdr w)))])
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
          (error 'run-simulation msg-run-simulation))))
  (define msg-run-simulation
    (string-append
     "consumes 4 or 5 arguments:\n"
     "-- (run-simulation <width> <height> <rate> <world-to-world-function>)\n"
     "-- (run-simulation <width> <height> <rate> <world-to-world-function> <create-animated-gif?>)\n"
     "see Help Desk."))


  (define run-simulation0
    (case-lambda
      [(width height rate f record?)
       (check-pos 'run-simulation width "first")
       (check-pos 'run-simulation height "second")
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
  (define (check-pos tag c rank)
    (check-arg tag (and (number? c) (> (coerce c) 0))
               "positive integer" rank c))
  
  ;; Symbol Any String String *-> Void
  (define (check-image tag i rank . other-message)
    (if (and (pair? other-message) (string? (car other-message)))
        (check-arg tag (image? i) (car other-message) rank i)
        (check-arg tag (image? i) "image" rank i)))

  ;; Symbol Any String -> Void
  (define (check-scene tag i rank)
    (if (image? i)
        (unless (scene? i)
          (error tag "scene expected, given image whose pinhole is at (~s,~s) instead of (0,0)"
                 (pinhole-x i) (pinhole-y i)))
        (check-arg tag #f "image" rank i)))
  
  (define (scene? i) (and (= 0 (pinhole-x i)) (= 0 (pinhole-y i))))
  
  ;; Symbol Any String -> Void
  (define (check-color tag width rank)
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
  (define (place-image0 image x y scene)
    (define sw (image-width scene))
    (define sh (image-height scene))
    (define ns (overlay/xy scene x y image))
    (define nw (image-width ns))
    (define nh (image-height ns))
    (if (and (= sw nw) (= sh nh)) ns (shrink ns 0 0 sw sh)))
  
  ;; Image Number Number Number Number Color -> Image
  (define (add-line-to-scene0 img x0 y0 x1 y1 c)
    (define w (image-width img))  
    (define h (image-height img))
    (cond
      [(and (<= 0 x0) (< x0 w) (<= 0 x1) (< x1 w) (<= 0 y0) (< y0 w) (<= 0 y1) (< y1 w))
       (add-line img x0 y0 x1 y1 c)]
      [(= x0 x1) ;; vertical 
       (printf "vertical\n")
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
  
  'direction 
  (equal? (direction 10 10 0 0) 'upper-left)
  (equal? (direction 10 10 20 20) 'lower-right)
  (equal? (direction 10 10 0 20) 'lower-left)
  (equal? (direction 10 10 20 0) 'upper-right)
  
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
      (error tag "evaluate (big-bang Number Number Number World) first")))
  
  (define the-world unique-world)
  (define the-world0 unique-world)
  
  ;; Nat World -> Void
  ;; effects: init event-history, the-delta, the-world, the-world0
  (define (install-world delta w)
    (set! event-history '())
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
    (when animated-gif
      (add-stop-and-image-buttons frame the-play-back-custodian))
    (add-editor-canvas frame visible-world w h)
    (send frame show #t))
  
  ;; [Box (union false Thread)] -> Frame
  ;; create a frame that shuts down the custodian on close
  (define (create-frame the-play-back-custodian)
    (new (class frame%
           (super-new)
           (define/augment (on-close)  
             (custodian-shutdown-all the-play-back-custodian)
             (callback-stop!)))
         (label "DrScheme")
         (stretchable-width #f)
         (stretchable-height #f)
         (style '(no-resize-border metal))))
  
  ;; Frame [Box (union false Thread)] -> Void
  ;; adds the stop animation and image creation button, 
  ;; whose callbacks runs as a thread in the custodian
  (define IMAGES "Images")
  (define-runtime-path s:pth '(lib "break.png" "icons"))
  (define-runtime-path i:pth '(lib "file.gif" "icons"))
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
  ;   ;;;;;                                     ;   ;      ; ;;;    
  ;   ;                             ;           ;   ;      ;   ;    
  ;   ;                             ;           ;   ;      ;   ;    
  ;   ;      ;   ;   ;;;   ; ;;   ;;;;;         ;   ;   ;;;;   ;    
  ;   ;;;;;  ;   ;  ;   ;  ;;  ;    ;           ;;;;;  ;   ;   ;    
  ;   ;       ; ;   ;;;;;  ;   ;    ;           ;   ;  ;   ;   ;    
  ;   ;       ; ;   ;      ;   ;    ;           ;   ;  ;   ;   ;    
  ;   ;       ; ;   ;      ;   ;    ;           ;   ;  ;  ;;   ;    
  ;   ;;;;;    ;     ;;;;  ;   ;     ;;         ;   ;   ;; ;   ;;;  
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
  ;; reset to '() by big-bang
  
  ;; Symbol  Any *-> Void
  (define (add-event type . stuff)
    (set! event-history (cons (cons type stuff) event-history)))
  
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
        [else (error 'play-back "bad type of event: ~s" fst)]))
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
      (set! bitmap-list (cons make-bitmap bitmap-list))
      (set! image-count (+ image-count 1))
      (send bm save-file (format "i~a.png" image-count) 'png))
    ;; --- choose place 
    (define target:dir
      (let* ([cd (current-directory)]
             [dd (get-directory "Select directory for images" #f cd)])
        (if dd dd cd)))
    (parameterize ([current-directory target:dir])
      (let replay ([ev event-history][world the-world0])
        (define img (redraw-callback0 world))
        (update-frame (text (format "~a/~a created" image-count total) 18 'red))
        (save-image img)
        (cond
          [(null? ev) (update-frame (text "creating i-animated.gif" 18 'red))
                      (create-animated-gif (reverse! bitmap-list))
                      (update-frame img)]
          [else (replay (cdr ev) (world-transition world (car ev)))]))))

  ;; [Listof (-> bitmap)] -> Void
  ;; turn the list of thunks into animated gifs 
  ;; effect: overwrite the ANIMATED-GIF-FILE (in current directory)
  (define (create-animated-gif bitmap-list)
    (define intv (if (> +inf.0 *the-delta* 0) (inexact->exact (floor (* 100 *the-delta*))) 5))
    (when (file-exists? ANIMATED-GIF-FILE)
      (delete-file ANIMATED-GIF-FILE))
    (write-animated-gif bitmap-list intv ANIMATED-GIF-FILE #:one-at-a-time? #t))
  
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
  
  ;; callbacks: timer, mouse, key, redraw
  
  ;; Definition = (define-callback Symbol String Symbol Expression ...)
  ;; effect: (define-callback introduces three names: name, name0, set-name
  (define-syntax (define-callback stx)
    (syntax-case stx ()
      [(_ n msg (f esp ...) para body ...)
       (let* ([n:str (symbol->string (syntax-e (syntax n)))]
              [callback (lambda (before after)
                          (string->symbol 
                           (string-append before n:str "-callback" after)))]
              [name (datum->syntax-object stx (callback "" ""))]
              [name0 (datum->syntax-object stx (callback "" "0"))]
              [set-name (datum->syntax-object stx (callback "set-" ""))])
         #`(define-values (#,name #,name0 #,set-name)
             (values 
              void void 
              (lambda (f esp ...)
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
    (set! redraw-callback void))
  
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
      (update-frame result)))

  ;; f : [World KeyEvent -> World]
  ;; esp : EventSpace 
  ;; e : KeyEvent 
  (define-callback key "key-event handler" (f evt-space) (e)
    (parameterize ([current-eventspace evt-space])
      (queue-callback 
       (lambda ()
         (with-handlers ([exn:break? break-handler][exn? exn-handler])
           (set! the-world (f the-world e))
           (add-event KEY e)
           (redraw-callback))))))
  
  ;; f : [World Nat Nat MouseEventType -> World]
  ;; esp : EventSpace 
  ;; e : MouseEvent
  (define-callback mouse "mouse event handler" (f evt-space) (e)
    (parameterize ([current-eventspace evt-space])
      (queue-callback
       (lambda ()
         (with-handlers ([exn:break? break-handler][exn? exn-handler])
           (define x (- (send e get-x) INSET))
           (define y (- (send e get-y) INSET))
           (define m (mouse-event->symbol e))
           (set! the-world (f the-world x y m))
           (add-event MOUSE x y m)
           (redraw-callback))))))
  
  ;; MouseEvent -> MouseEventType
  (define (mouse-event->symbol e)
    (cond [(send e button-down?) 'button-down]
          [(send e button-up?)   'button-up]
          [(send e dragging?)    'drag]
          [(send e moving?)      'move]
          [(send e entering?)    'enter]
          [(send e leaving?)     'leave]
          [else ; (send e get-event-type)
           (error 'on-mouse-event
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
  )
