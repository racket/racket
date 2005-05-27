#| TODO
   I need
    color? ;; Symbol -> Boolean 
|#
(module world mzscheme
  (require ; (lib "unitsig.ss")
   (lib "etc.ss")
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "error.ss" "htdp")
   (lib "image.ss" "htdp")
   (prefix beg: (lib "htdp-beginner.ss" "lang"))
   (lib "prim.ss" "lang"))
  
  ;; --- provide ---------------------------------------------------------------
  (provide (all-from (lib "image.ss" "htdp")))
  
  (provide     ;; forall(World):
   big-bang    ;; Number Number Number World -> true 
   end-of-time ;; -> World

   nw:rectangle ;; Number Number Mode Color -> Image
   place-image  ;; Image Number Number Scence -> Scene
   empty-scene  ;; Number Number -> Scene 
   run-movie    ;; (Listof Image) -> true 
   )
  
  (provide
   update produce ;; (update <exp> produce <exp>)
   )
  
  (provide-higher-order-primitive
   on-tick-event (tock) ;; (World -> World) -> true
   )
  
  (provide-higher-order-primitive ;; (KeyEvent World -> World) -> true 
   on-key-event 
   (tock)
   )
  ;; ---------------------------------------------------------------------------

  ;; Symbol Any String -> Void
  (define (check-pos tag c rank)
    (check-arg tag (and (number? c) (>= c 0)) "positive number" rank c))
  
  ;; Symbol Any String [String] -> Void
  (define (check-image tag i rank . other-message)
    (if (and (pair? other-message) (string? (car other-message)))
        (check-arg tag (beg:image? i) (car other-message) rank i)
        (check-arg tag (beg:image? i) "image" rank i)))
  
  ;; Symbol Any String -> Void
  (define (check-color tag width rank)
    (check-arg tag (symbol? width) "color symbol" rank width))
  
  (define (check-mode tag s rank)
    (check-arg tag (or (eq? s 'solid) (eq? s 'outline)) "'solid or 'outline" rank s))

  (define (nw:rectangle width height mode color)
    (check-pos 'rectangle width "first")
    (check-pos 'rectangle height "second")
    (check-mode 'rectangle mode "third")
    (check-color 'rectangle color "fourth")
    (move-pinhole (rectangle width height mode color) (/ width -2) (/ height -2)))
  
  (define (place-image image x y scene)
    (check-image 'place-image image "first")
    (check-pos 'place-image x "second")
    (check-pos 'place-image y "third")
    (check-image 'place-image scene "fourth" "scene")
    (overlay/xy scene x y image))
  
  (define (empty-scene width height)
    (check-pos 'empty-scene width "first")
    (check-pos 'empty-scene height "second")    
    (move-pinhole 
     (rectangle width height 'outline 'black)
     (/ width -2) (/ height -2))
   )
  
  ;; display all images in list in the canvas
  (define (run-movie movie)
    (check-arg 'run-movie (list? movie) "list (of images)" "first" movie)
    (for-each (lambda (cand) (check-image 'run-movie cand "first" "list of images"))
              movie)
    (let run-movie ([movie movie])
      (cond [(null? movie) #t]
            [(pair? movie)
             (update (car movie) produce #t)
             (sleep/yield .05)
             (run-movie (cdr movie))])))
  
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

  ;; Number Number Number World -> true
  ;; create the visible world (canvas)
  (define (big-bang w h delta world)
    (check-arg 'big-bang (and (integer? w) (> w 0)) "positive integer" "first" w)
    (check-arg 'big-bang (and (integer? h) (> h 0)) "positive integer" "second" h)
    (check-arg 'big-bang
               (and (number? delta) (>= delta 0))
               "number [of seconds] between 0 and 1000000"
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
               (label "DrScheme")))
    (send 
     (new (class editor-canvas%
            (super-new)
            (define/override (on-char e)
              (on-char-proc (send e get-key-code))))
          (parent the-frame) 
          (editor txt)
          (style '(no-hscroll no-vscroll))
          ;; this 20 stuff is a hack, for now
          (min-width (+ w 20))
          (min-height (+ h 20)))
     focus)
    (send txt hide-caret #t)
    (send the-frame show #t)
    #t)
  
  ;; --- time events 
  [define the-time (new timer% [notify-callback (lambda () (timer-callback))])]
  
  ;; (World -> World)
  [define timer-callback void]
  
  ;; (World -> World) -> true
  ;; set the click handler 
  [define (on-tick-event f)
    (check-proc 'on-tick-event f 1 "on-tick-event" "one argument")
    (check-world 'on-tick-event)
    (if (eq? timer-callback void)
        (set! timer-callback 
              (lambda ()
                (with-handlers ([exn:break? break-handler]
                                [exn? exn-handler])
                  (set! the-world (f the-world)))))
        (error 'on-tick "the timing action has been set already"))
    (send the-time start
          (let* ([w (ceiling (* 1000 the-delta))])
            (if (exact? w) w (inexact->exact w))))
    #t]
  
  ;; --- key events 
  ;; KeyEvent = (union Symbol Char)
  
  ;; KeyEvent -> Void
  [define on-char-proc void]
  
  ;; (KeyEvent World -> World) -> true
  ;; effect: set on-char-proc so that it deals with keyboard events
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
                           (set! the-world (f e the-world))))
                       #t))))
            #t)
          (error 'on-event "the event action has been set already")))]
  
  [define (end-of-time)
    (printf "end of time~n")
    (stop-it)
    the-world]
  
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
  
  ;; --- putting images into the canvas
  (define-syntax (produce stx) 
    (raise-syntax-error 'produce "produce must be inside a use of update" stx))
  
  (define-syntax (update s)
    (syntax-case s (produce)
      [(_ pict produce exp) (syntax (begin (update-frame pict) exp))]
      [(_ pict pict2 ... produce exp)
       (raise-syntax-error 'update "you can place only one picture in the canvas" s)]
      [(_ stmt produce) 
       (raise-syntax-error 'update "produce must be followed by an expression" s)]
      [(_ stmt produce exp exp2 ...)
       (raise-syntax-error 'update "produce must be followed by exactly one expression" s)]
      [_
       (raise-syntax-error 'update "use as (update <image> produce <expression>)")]))
  
  (define (update-frame pict)
    (unless the-frame (error 'update SEQUENCE-ERROR))
    (send txt begin-edit-sequence)
    (send txt lock #f)
    (send txt delete 0 (send txt last-position) #f)
    (send txt insert (send pict copy) 0 0 #f)
    (send txt lock #t)
    (send txt end-edit-sequence))
  
  (define SEQUENCE-ERROR "evaluate (big-bang Number Number Number World) first")
  )

