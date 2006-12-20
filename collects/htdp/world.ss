;; Sun Dec 09 23:17:41 EST 2006: add-line fixed so it cuts off lines before drawing
;; Mon Mar 27 10:29:28 EST 2006: integrated Felix's mouse events
;; Wed Jan 25 13:38:42 EST 2006: on-redraw: proc is now called on installation
;; Tue Jan  3 11:17:50 EST 2006: changed add-line behavior in world.ss 
;; Sat Dec 10 19:39:03 EST 2005: fixed name, changed interface to on-key-event
;; Fri Dec  9 21:39:03 EST 2005: remoevd (update ... produce ...); added on-redraw 
;; Thu Dec  1 17:03:03 EST 2005: fixed place-image; all coordinates okay now
(module world mzscheme
  (require
   (lib "class.ss")
   (lib "etc.ss")
   (lib "list.ss")
   (lib "process.ss")
   (lib "mred.ss" "mred")
   (lib "error.ss" "htdp")
   (lib "image.ss" "htdp")
   (prefix beg: (lib "htdp-beginner.ss" "lang"))
   (lib "prim.ss" "lang"))
  
  (require (lib "bitmap-label.ss" "mrlib")
           (lib "string-constant.ss" "string-constants"))
  
  ;; --- provide ---------------------------------------------------------------
  (provide (all-from-except (lib "image.ss" "htdp") add-line))
  
  (provide      ;; forall(World):
   big-bang	;; Number Number Number World -> true
   begin-recording ;; String -> true
   end-of-time	    ;; String u Symbol -> World
   
   nw:rectangle ;; Number Number Mode Color -> Image
   place-image  ;; Image Number Number Scence -> Scene
   empty-scene  ;; Number Number -> Scene 
   run-movie    ;; (Listof Image) -> true 
   (rename add-line-to-scene add-line)
   ;; Scene Number Number Number Number Color -> Scene 
   ;; cut all pieces that are outside the given rectangle 
   )
  
  (provide-higher-order-primitive
   run-simulation (_ _ _ create-scene) ;; (Nat Nat Number (Nat -> Image) -> true)
   )
  
  (define (run-simulation width height rate f)
    (check-pos 'run-simulation width "first")
    (check-pos 'run-simulation height "second")
    (check-arg 'run-simulation (number? rate) 'number "third" rate)
    (check-proc 'run-simulation f 1 "fourth" "one argument")
    (big-bang width height rate 1)
    (on-redraw f)
    (on-tick-event add1))
  
  (provide-higher-order-primitive
   on-tick-event (tock) ;; (World -> World) -> true
   )
  
  (provide-higher-order-primitive
   on-redraw (world-image) ;; (World -> Image) -> true
   )
  
  ;; KeyEvent is one of: 
  ;; -- Char 
  ;; -- Symbol 
  
  (provide-higher-order-primitive ;; (World KeyEvent -> World) -> true 
   on-key-event 
   (draw)
   )
  
  ;; A MouseEventType is one of:
  ;; - 'button-down
  ;; - 'button-up
  ;; - 'drag
  ;; - 'move
  ;; - 'enter
  ;; - 'leave
  
  (provide-higher-order-primitive ;; (World Number Number MouseEvent -> World) -> true 
   on-mouse-event 
   (clack)
   )
  
  ;; ---------------------------------------------------------------------------
  
  ;; Symbol Any String -> Void
  (define (check-pos tag c rank)
    (check-arg tag (and (number? c) (integer? c) (>= c 0)) "positive integer" rank c))
  
  ;; Symbol Any String [String] -> Void
  (define (check-image tag i rank . other-message)
    (if (and (pair? other-message) (string? (car other-message)))
        (check-arg tag (beg:image? i) (car other-message) rank i)
        (check-arg tag (beg:image? i) "image" rank i)))
  
  ;; Symbol Any String -> Void
  (define (check-color tag width rank)
    (check-arg tag (or (symbol? width) (string? width)) "color symbol or string" rank width))
  
  (define (check-mode tag s rank)
    (check-arg tag (or (eq? s 'solid)
		       (eq? s 'outline)
		       (string=? "solid" s)
		       (string=? "outline" s)) "mode (solid or outline)" rank s))
  
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
    (check-image 'place-image scene "fourth" "scene")
    (let ([x (number->integer x)]
	  [y (number->integer y)])
      (define sw (image-width scene))
      (define sh (image-height scene))
      (define ns (overlay/xy scene x y image))
      (define nw (image-width ns))
      (define nh (image-height ns))
      (if (and (= sw nw) (= sh nh)) 
	  ns
	  (shrink ns 0 0 sw sh))))
  
  (define (number->integer x) (inexact->exact (floor x)))
  
  (define (add-line-to-scene img x0 y0 x1 y1 c)
    #|
    (check-image 'add-line image "first")
    (check-pos 'add-line x0 "second")
    (check-pos 'add-line y0 "third")
    (check-pos 'add-line x1 "fourth")
    (check-pos 'add-line x2 "fifth")
    (check-color 'add-line x2 "sixth")
    |#
    (local ((define w (image-width img))  
            (define h (image-height img)))
      (cond
        [(and (<= 0 x0 w) (<= 0 x1 w) (<= 0 y0 w) (<= 0 y1 w)) 
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
              (add-line-to-scene img x1 y1 x0 y0 c)]
             [else 
              (cond
                [(and (number? upp) (number? low)) (add-line img upp 0 low h c)]
                [(and (number? upp) (number? lft)) (add-line img upp 0 0 lft c)]
                [(and (number? upp) (number? rgt)) (add-line img upp 0 w rgt c)]
                [(and (number? low) (number? lft)) (add-line img low h 0 lft c)]
                [(and (number? low) (number? rgt)) (add-line img low h w rgt c)]
                [(and (number? lft) (number? rgt)) (add-line img 0 lft w rgt c)]
                [else img])]))])))
  
  ;; Nat Nat -> Nat 
  ;; y if in [0,h], otherwise the closest boundary
  (define (app y h)
    (cond
      [(<= 0 y h) y]
      [(< y 0) 0]
      [else    h]))
  
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
  
  ;; -----------------------------------------------------------------------------
  
  (define (empty-scene width height)
    (check-pos 'empty-scene width "first")
    (check-pos 'empty-scene height "second")    
    (move-pinhole 
     (rectangle width height 'outline 'black)
     (/ width -2) (/ height -2)))
  
  ;; display all images in list in the canvas
  (define (run-movie movie)
    (check-arg 'run-movie (list? movie) "list (of images)" "first" movie)
    (for-each (lambda (cand) (check-image 'run-movie cand "first" "list of images"))
              movie)
    (let run-movie ([movie movie])
      (cond [(null? movie) #t]
            [(pair? movie)
             (update-frame (car movie))
             (sleep/yield .05)
             (run-movie (cdr movie))])))
  
  ;; ---------------------------------------------------------------------------
  
  ;; The One and Only Visible World
  (define the-frame #f)
  (define the-button #f)
  (define txt (new text%))
  
  ;; World (type parameter)
  (define the-world0 (cons 1 1))
  [define the-world the-world0]
  
  (define (check-world tag)
    (when (eq? the-world0 the-world) (error tag SEQUENCE-ERROR)))
  
  ;; Number > 0
  [define the-delta 1000]
  
  ;; Amount of space around the image in the world window:
  (define INSET 5)
  
  ;; Number Number Number World -> true
  ;; create the visible world (canvas)
  (define (big-bang w h delta world)
    (check-pos 'big-bang w "first")
    (check-pos 'big-bang h "second")
    (check-arg 'big-bang
               (and (number? delta) (<= 0 delta 1000))
               "number [of seconds] between 0 and 1000"
               "first"
               delta)
    (when the-frame (error 'big-bang "big-bang already called once"))
    (set! the-delta delta) 
    (set! the-world world)
    (set! the-frame
          (new (class frame%
                 (super-new)
                 (define/augment (on-close)        
                   ;; shut down the timer when the window is destroyed
                   (send the-time stop)
                   (inner (void) on-close)))
               (label "DrScheme")
               (stretchable-width #f)
               (stretchable-height #f)
               (style '(no-resize-border metal))))
    (let ([p (new horizontal-pane% [parent the-frame][alignment '(center center)])])
      (new button% 
           [parent p]
           [label ((bitmap-label-maker (string-constant break-button-label) (build-path (collection-path "icons") "break.png")) '___)]
           [style '(border)]
           [callback (lambda (this-button e) 
                       (send the-frame on-close)
                       (send this-button enable #f)
                       (send the-button enable #t))])                       
      (set! the-button (new button% 
                            [parent p]
                            [enabled #f]
                            [label ((bitmap-label-maker "Images" (build-path (collection-path "icons") "file.gif")) '___)]
                            [style '(border)]
                            [callback (lambda (x e) (printf "hello world\n"))])))
    (let ([c (new (class editor-canvas%
		    (super-new)
		    (define/override (on-char e)
		      (on-char-proc (send e get-key-code)))
		    (define/override (on-event e)
		      (on-mouse-proc e)))
                  (parent the-frame) 
                  (editor txt)
                  (style '(no-hscroll no-vscroll))
                  (horizontal-inset INSET)
                  (vertical-inset INSET))])
      (send c min-client-width (+ w INSET INSET))
      (send c min-client-height (+ h INSET INSET))
      (send c focus))
    (send txt set-cursor (make-object cursor% 'arrow))
    (send txt hide-caret #t)
    (send the-frame show #t)
    #t)
  
  ;; --- time events 
  [define the-time (new timer% [notify-callback (lambda () (timer-callback))])]
  
  ;; (World -> World)
  [define timer-callback void]
  
  [define (on-tick-event f)
    (check-proc 'on-tick-event f 1 "on-tick-event" "one argument")
    (check-world 'on-tick-event)
    (if (eq? timer-callback void)
        (set! timer-callback 
              (lambda ()
                (with-handlers ([exn:break? break-handler]
                                [exn? exn-handler])
                  (set! the-world (f the-world))
                  (on-redraw-proc))))          
        (error 'on-tick "the timing action has been set already"))
    (send the-time start
          (let* ([w (ceiling (* 1000 the-delta))])
            (if (exact? w) w (inexact->exact w))))
    #t]
  
  ;; --- key and mouse events 
  
  ;; KeyEvent -> Void
  [define on-char-proc void]
  
  [define (on-key-event f)
    (check-proc 'on-key-event f 2 "on-key-event" "two arguments")
    (check-world 'on-key-event)
    (let ([esp (current-eventspace)])
      (if (eq? on-char-proc void)
          (begin 
            (set! on-char-proc 
                  (lambda (e)
                    (parameterize ([current-eventspace esp])
                      (queue-callback 
                       (lambda ()
                         (with-handlers ([exn:break? break-handler]
                                         [exn? exn-handler])
                           (set! the-world (f the-world e))
                           (on-redraw-proc))))
                      #t)))
            #t)
          (error 'on-event "the event action has been set already")))]
  
  (define (end-of-time s)
    (printf "end of time: ~a~n" s)
    (stop-it)
    the-world)
  
  ;; MouseEvent -> Void
  (define on-mouse-proc void)
  
  (define (on-mouse-event f)
    (check-proc 'on-mouse-event f 4 "on-mouse-event" "four arguments")
    (check-world 'on-mouse-event)
    (let ([esp (current-eventspace)])
      (if (eq? on-mouse-proc void)
          (begin
            (set! on-mouse-proc 
                  (lambda (e)
                    (parameterize ([current-eventspace esp])
                      (queue-callback
                       (lambda ()
                         (with-handlers ([exn:break? break-handler]
                                         [exn? exn-handler])
                           (set! the-world (f the-world 
                                              (- (send e get-x) INSET)
                                              (- (send e get-y) INSET)
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
                                                             (send e get-event-type)))]
                                                    )))
                           (on-redraw-proc))))
                      #t)))
            #t)
          (error 'on-mouse-event "the mouse event action has been set already"))))
  #|
  Note an alternative to the above cond is to just
  send get-event-type, which produces one of the following:

  ? 'enter -- mouse pointer entered the window
  ? 'leave -- mouse pointer left the window
  ? 'left-down -- left mouse button pressed
  ? 'left-up -- left mouse button released
  ? 'middle-down -- middle mouse button pressed
  ? 'middle-up -- middle mouse button released
  ? 'right-down -- right mouse button pressed (Mac OS: click with control key pressed)
  ? 'right-up -- right mouse button released (Mac OS: release with control key pressed)
  ? 'motion -- mouse moved, with or without button(s) pressed
  |#                             
  
  ;; --- library 
  (define (exn-handler e)
    (stop-it)
    (raise e))
  
  (define (break-handler . _) 
    (printf "animation stopped")
    (stop-it)
    the-world)
  
  ;; -> Void
  (define (stop-it)
    (send the-time stop)
    (stop-recording "blah")
    (set! on-char-proc void)
    (set! timer-callback void))
  
  (define on-redraw-proc void)
  
  (define (on-redraw f)
    (check-proc 'on-redraw f 1 "on-redraw" "one argument")
    (check-world 'on-redraw)
    (if (eq? on-redraw-proc void)
        (begin
          (set! on-redraw-proc
                (lambda ()
                  (with-handlers ([exn:break? break-handler]
                                  [exn? exn-handler])
                    (define img (f the-world))
                    (check-result 'on-redraw (lambda (x) (beg:image? x)) "image" img)
                    (when recording? (save-image img))
                    (update-frame img)
                    #t)))
          (on-redraw-proc))
        (error 'on-redraw "the redraw function has already been specified")))
  
  (define (update-frame pict)
    (send txt begin-edit-sequence)
    (send txt lock #f)
    (send txt delete 0 (send txt last-position) #f)
    (send txt insert (send pict copy) 0 0 #f)
    (send txt lock #t)
    (send txt end-edit-sequence))
  
  ;; -------------------------------------------------------------------------
  ;; recording a sequence of interactions via images 
  (define recording? #f)
  (define rec-where "")
  ;; String -> true
  (define (begin-recording d)
    (check-arg 'record! (string? d) 'string "first" d)
    (set! rec-where d)
    (set! recording? #t)
    #t)
  ;; String -> true
  (define (stop-recording d)
    (check-arg 'record! (string? d) 'string "first" d)
    (set! recording? #f)
    (let ()
      (define files 
	(sort
         (filter (lambda (x) (regexp-match #rx"i[0-9]*.png" x))
                 (map path->string (directory-list)))
         (lambda (x y)
           (<= (string->number (car (regexp-match #rx"[0-9]+" x)))
               (string->number (car (regexp-match #rx"[0-9]+" y)))))))
      #;
      (define cmdline (format "convert -delay 5 ~a ~a.gif" 
                              (apply string-append (map (lambda (x) (format " ~a" x)) files))
                              d))
      #;
      (system cmdline)
      #t))
  
  (define image-count 0)
  ;; Image -> Void
  ;; save image in a file named i<number>.png 
  (define (save-image img)
    (send (send img get-bitmap) save-file (format "i~a.png" image-count) 'png)
    (set! image-count (+ image-count 1)))
  
  (define SEQUENCE-ERROR "evaluate (big-bang Number Number Number World) first")
  )

