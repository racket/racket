#lang scheme/base

;; Mon Mar 27 10:29:28 EST 2006: integrated Felix's mouse events
;; Wed Jan 25 13:38:42 EST 2006: on-redraw: proc is now called on installation
;; Tue Jan  3 11:17:50 EST 2006: changed add-line behavior in world.rkt
;; Sat Dec 10 19:39:03 EST 2005: fixed name, changed interface to on-key-event
;; Fri Dec  9 21:39:03 EST 2005: remoevd (update ... produce ...); added on-redraw 
;; Thu Dec  1 17:03:03 EST 2005: fixed place-image; all coordinates okay now

  (require
   (except-in mred make-color)
   mzlib/class
   htdp/error
   "image.rkt"
   (prefix-in beg: lang/htdp-beginner)
   lang/prim
   deinprogramm/signature/signature-syntax)
  
  ;; --- provide ---------------------------------------------------------------
  (provide (all-from-out "image.rkt"))
  
  (provide      ;; forall(World):
   big-bang     ;; Number Number Number World -> true
   end-of-time  ;; String u Symbol -> World
   )
  
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
  
  ;; A MouseEventKind is one of:
  ;; "enter" -- mouse pointer entered the window
  ;; "leave" -- mouse pointer left the window
  ;; "left-down" -- left mouse button pressed
  ;; "left-up" -- left mouse button released
  ;; "middle-down" -- middle mouse button pressed
  ;; "middle-up" -- middle mouse button released
  ;; "right-down" -- right mouse button pressed (Mac OS: click with control key pressed)
  ;; "right-up" -- right mouse button released (Mac OS: release with control key pressed)
  ;; "motion" -- mouse moved, with or without button(s) pressed

  
  (provide-higher-order-primitive ;; (World Number Number MouseEventKind -> World) -> true 
   on-mouse-event 
   (clack)
   )

  (provide mouse-event-kind)
 
  (define mouse-event-kind
    (signature
     (one-of "enter" "leave" "motion" "left-down" "left-up" "middle-down" "middle-up" "right-down" "right-up")))
  
  ;; ---------------------------------------------------------------------------
  
  ;; Symbol Any String -> Void
  (define (check-pos tag c rank)
    (check-arg tag (and (number? c) (integer? c) (>= c 0)) "positive integer" rank c))
  
  ;; ---------------------------------------------------------------------------
  
  ;; The One and Only Visible World
  (define the-frame #f)
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
               (label "DrRacket")
               (stretchable-width #f)
               (stretchable-height #f)
               (style '(no-resize-border metal))))
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
		    (cond
		     ((event->string e)
		      => (lambda (s)
			   (parameterize ([current-eventspace esp])
			     (queue-callback 
			      (lambda ()
				(with-handlers ([exn:break? break-handler]
						[exn? exn-handler])
				  (set! the-world (f the-world s))
				  (on-redraw-proc))))))))
		    #t))
            #t)
          (error 'on-event "the event action has been set already")))]

  (define (event->string e)
    (if (char? e)
	(string e)
	(case e
	  ((left) "left")
	  ((right) "right")
	  ((up) "up")
	  ((down) "down")
	  ((wheel-up) "wheel-up")
	  ((wheel-down) "wheel-down")
	  (else #f))))
  
  [define (end-of-time s)
    (printf "end of time: ~a\n" s)
    (stop-it)
    the-world]
  
  ;; MouseEvent -> Void
  [define on-mouse-proc void]
  
  [define (on-mouse-event f)
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
                                              (send e get-x)
                                              (send e get-y)
					      (symbol->string (send e get-event-type))))
                           (on-redraw-proc))))
                      #t)))
            #t)
          (error 'on-mouse-event "the mouse event action has been set already")))]
  
  ;; --- library 
  [define (exn-handler e)
    (send the-time stop)
    (set! on-char-proc void)
    (set! timer-callback void)
    (raise e)]
  
  [define (break-handler . _) 
    (printf "animation stopped")
    (stop-it)
    the-world]
  
  ;; -> Void
  (define (stop-it)
    (send the-time stop)
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
  
  (define SEQUENCE-ERROR "evaluate (big-bang Number Number Number World) first")
