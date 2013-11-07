#lang racket/gui

(require "check-aux.rkt"
         "timer.rkt"
         "last.rkt"
         "checked-cell.rkt"
         "stop.rkt"
         "universe-image.rkt"
         "pad.rkt"
         (only-in 2htdp/image scale overlay/align)
         htdp/error
         mzlib/runtime-path
         mrlib/bitmap-label
         string-constants
         mrlib/gif)

(provide world% aworld%)

;                                     
;                                     
;                                     
;   ;   ;                  ;        ; 
;   ;   ;                  ;        ; 
;   ;   ;                  ;        ; 
;   ;   ;   ;;;   ; ;;     ;     ;;;; 
;   ;   ;  ;   ;  ;;  ;    ;    ;   ; 
;   ; ; ;  ;   ;  ;   ;    ;    ;   ; 
;   ;; ;;  ;   ;  ;        ;    ;   ; 
;   ;   ;  ;   ;  ;        ;    ;   ; 
;   ;   ;   ;;;   ;        ;;    ;;;; 
;                                     
;                                     
;                                     

(define MIN-WIDT-FOR-GAME-PAD 300)

;; -----------------------------------------------------------------------------
;; packages for broadcasting information to the universe 

(define-values (make-package package? package-world package-message)
  (let ()
    (struct package (world message) #:transparent)
    (define (make-package w m)
      (check-arg 'make-package (sexp? m) 'sexp "second" m)
      (package w m))
    (values make-package package? package-world package-message)))

(provide
 make-package  ;; World S-expression -> Package
 package?      ;; Any -> Package
 package-world ;; Package -> World 
 )

(define world%
  (last-mixin
   (clock-mixin
    (class* object% (start-stop<%>)
      (inspect #f)
      (init-field world0)
      (init-field name state register check-with on-key on-release on-pad on-mouse record?)
      (init on-receive on-draw stop-when)
      
      ;; -----------------------------------------------------------------------
      (field
       [to-draw on-draw]
       [world
        (new checked-cell% [value0 world0] [ok? check-with]
             [display (and state (or name "your world program's state"))])])
      
      
      ;; -----------------------------------------------------------------------
      (field [*out* #f] ;; (U #f OutputPort), where to send messages to 
             [*rec* (make-custodian)]) ;; Custodian, monitor traffic)
      
      (define/private (register-with-host)
        (define FMT "\nworking off-line\n")
        (define FMTtry 
          (string-append "unable to register with ~a after ~s tries" 
                         FMT))                         
        (define FMTcom 
          (string-append "unable to register with ~a due to protocol problems" 
                         FMT))
        ;; Input-Port -> [-> Void]
        ;; create closure (for thread) to receive messages and signal events
        (define (RECEIVE in)
          (define (RECEIVE)
            (sync 
             (handle-evt
              in
              (lambda (in) 
                (define dis (text "the universe disappeared" 11 'red))
                (with-handlers ((tcp-eof? 
                                 (compose (handler #f)
                                          (lambda (e)
                                            (set! draw (lambda (w) dis))
                                            (pdraw)
                                            e))))
                  ;; --- "the universe disconnected" should come from here ---
                  (define msg (tcp-receive in))
                  (cond
                    [(sexp? msg) (prec msg) (RECEIVE)] ;; break loop if EOF
                    [#t (error 'RECEIVE "sexp expected, received: ~e" msg)]))))))
          RECEIVE)
        ;; --- now register, obtain connection, and spawn a thread for receiving
        (parameterize ([current-custodian *rec*])
          ;; try to register with the server n times 
          (let try ([n TRIES])
            (printf "trying to register with ~a ...\n" register)
            (with-handlers ((tcp-eof? (lambda (x) (printf FMTcom register)))
                            (exn:fail:network? 
                             (lambda (x)
                               (if (= n 1) 
                                   (printf FMTtry register TRIES)
                                   (begin (sleep PAUSE) (try (- n 1)))))))
              (define-values (in out) (tcp-connect register SQPORT))
              (tcp-register in out name)
              (printf "... successful registered and ready to receive\n")
              (set! *out* out)
              (thread (RECEIVE in))))))
      
      (define/private (broadcast msg)
        (when *out* 
          (check-result 'send sexp? "Sexp expected; given ~e\n" msg)
          (tcp-send *out* msg)))
      
      ;; -----------------------------------------------------------------------
      (field
       (draw   (cond
                 [(procedure? to-draw) to-draw]
                 [(pair? to-draw)      (first to-draw)]
                 [else to-draw]))
       (live   (not (boolean? draw)))
       (width  (if (pair? to-draw) (second to-draw) #f))
       (height (if (pair? to-draw) (third to-draw) #f)))
      
      ;; the visible world 
      (field [enable-images-button void] ;; used if stop-when call produces #t
             [disable-images-button void]
             [visible (new pasteboard%)])
      
      (define/private (show-canvas)
        (send visible set-cursor (make-object cursor% 'arrow))
        (let ([fst-scene (ppdraw)])
          (if (2:image? fst-scene)
              (let ([first-width  (+ (image-width fst-scene) 1)]
                    [first-height (+ (image-height fst-scene) 1)])
                (unless (and width height)
                  (check-scene-dimensions (name-of draw 'your-draw) first-width first-height)
                  (set! width first-width)
                  (set! height first-height)))
              (let ([first-width  (image-width fst-scene)]
                    [first-height (image-height fst-scene)])
                (unless (and width height)
                  (set! width first-width)
                  (set! height first-height))))
          (when pad
            (unless (>= width MIN-WIDT-FOR-GAME-PAD)
              (error 'big-bang
                     "a game pad requires a scene whose width is greater or equal to ~a, given ~e"
                     MIN-WIDT-FOR-GAME-PAD fst-scene))
            (set! game-pad-image (scale (/ width (image-width game-pad)) game-pad)))
          (create-frame)
          (show fst-scene)))
      
      (define/private (add-game-pad scene)
        (if (boolean? pad) scene (overlay/align 'left 'bottom game-pad-image scene)))
      
      (define/public (deal-with-key %)
        (if (and (not on-key) (not on-pad) (not on-release))
            %
            (class %
              (super-new)
              (define/override (on-char e) 
                (when live
                  (let ([e:str (key-event->parts e)])
                    (cond
                      [(string=? e:str "release") (prelease (key-release->parts e))]
                      [(and pad (pad-event? e:str)) (ppad e:str)]
                      [else (pkey e:str)])))))))
      
      (define/public (deal-with-mouse %)
        (if (not on-mouse) 
            ;; No mouse handler => discard mouse events (so snip are not selected
            ;;  in the pasteboard, for example
            (class %
              (super-new)
              (define/override (on-event e)
                (void)))
            ;; Mouse handler => handle mouse events
            (class %
              (super-new)
              (define/override (on-event e)
                (define-values (x y me) (mouse-event->parts e))
                (when live
                  (cond
                    [(and (<= 0 x width) (<= 0 y height)) (pmouse x y me)]
                    [(member me '("leave" "enter")) (pmouse x y me)]
                    [else (void)]))))))
      
      ;; allows embedding of the world-canvas in other GUIs
      (define/public (create-frame)
        (create-frame/universe))
      
      ;; effect: create, show and set the-frame
      (define/pubment (create-frame/universe)
        (define play-back:cust (make-custodian))
        (define frame (new (class frame%
                             (super-new)
                             (define/augment (on-close)  
                               (callback-stop! 'frame-stop)
                               (custodian-shutdown-all play-back:cust)))
                           (label (if name (format "~a" name) "World"))
                           (alignment '(center center))
                           (style '(no-resize-border metal))))
        
        (define editor-canvas
          (new (deal-with-key (deal-with-mouse editor-canvas%))
               (parent frame)
               (editor visible)
               (stretchable-width #f)
               (stretchable-height #f)
               (style '(no-hscroll no-vscroll))
               (horizontal-inset INSET)
               (vertical-inset INSET)))
        (send editor-canvas min-client-width (+ width INSET INSET))
        (send editor-canvas min-client-height (+ height INSET INSET))
        (set!-values (enable-images-button disable-images-button)
                     (inner (values void void) create-frame/universe frame play-back:cust))
        (send editor-canvas focus)
        (send frame show #t))
      
      ;; Image -> Void
      ;; show the image in the visible world
      (define/public (show pict0)
        (define pict (add-game-pad pict0))
        (send visible begin-edit-sequence)
        (send visible lock #f)
        (let ([s (send visible find-first-snip)]
              [c (send visible get-canvas)])
          (when s (send visible delete s))
          (send visible insert (disable-cache (send pict copy)) 0 0)
          (send visible lock #t)
          (send visible end-edit-sequence)
          ;; The following flush trades streaming performance (where updates
          ;; could be skipped if they're replaced fast enough) for 
          ;; responsiveness (where too many updates might not get 
          ;; through if the canvas is mostly in suspended-refresh 
          ;; mode for scene changes):
          #;
          (send c flush)))
      
      ;; ----------------------------------------------------------------------
      ;; callbacks 
      (field
       (key     (if on-key on-key (lambda (w ke) w)))
       (pad     on-pad)
       (game-pad-image #f)
       (release (if on-release on-release (lambda (w ke) w)))
       (mouse  on-mouse)
       (rec    on-receive))
      
      (define drawing #f) ;; Boolean; is a draw callback scheduled?
      (define (set-draw#!) (set! draw# (random 3)) (set! drawing #f))
      (define draw# 0) 
      (set-draw#!)
      
      (define-syntax def/cback
        (syntax-rules ()
          [(_ pub (name arg ...) transform) 
           (def/cback pub (name arg ...) transform (object-name transform))]
          [(_ pub (name arg ...) transform tag)
           ;; Any ... -> Boolean
	   (begin
	     (pub name)
		   
           (define (name arg ...) 
             (queue-callback 
              (lambda ()
                (define H (handler #t))
                (with-handlers ([exn? H])
                  ; (define tag (object-name transform))
                  (define nw (transform (send world get) arg ...))
                  (define (d) 
                    (with-handlers ((exn? H))
                      (pdraw))
                    (set-draw#!))
                  ;; ---
                  ;; [Listof (Box [d | void])]
                  (define w '()) 
                  ;; set all to void, then w to null 
                  ;; when a high priority draw is scheduledd
                  ;; --- 
                  (when (package? nw)
                    (broadcast (package-message nw))
                    (set! nw (package-world nw)))
                  (cond
                    [(stop-the-world? nw)
                     (set! nw (stop-the-world-world nw))
                     (send world set tag nw)
                     (last-draw)
                     (callback-stop! 'name)
                     (enable-images-button)]
                    [else 
                     [define changed-world? (send world set tag nw)]
                     [define stop? (stop (send world get))]
                     ;; this is the old "Robby optimization" see checked-cell:
                     ; unless changed-world? 
                     (cond
                       [(and draw (not stop?))
                        (cond
                          [(not drawing)
                           (set! drawing #t)
                           (let ([b (box d)])
                             (set! w (cons b w))
                             ;; low priority, otherwise it's too fast
                             (queue-callback (lambda () ((unbox b))) #f))]
                          [(< draw# 0)
                           (set-draw#!)
                           (for-each (lambda (b) (set-box! b void)) w)
                           (set! w '())
                           ;; high!!  the scheduled callback didn't fire
                           (queue-callback (lambda () (d)) #t)]
                          [else 
                           (set! draw# (- draw# 1))])]
                       [stop?
                        (last-draw)
                        (callback-stop! 'name)
                        (enable-images-button)])
                     changed-world?]))))))]))
      
      ;; tick, tock : deal with a tick event for this world 
      (def/cback pubment (ptock) (lambda (w) (pptock w)) (name-of-tick-handler))
      (define/public (pptock w) (void))
      (define/public (name-of-tick-handler)
        "the on-tick handler")
      
      ;; key events 
      (def/cback pubment (pkey ke) key)
      
      ;; key events 
      (def/cback pubment (ppad ke) pad)
      
      ;; release events 
      (def/cback pubment (prelease ke) release)
      
      ;; mouse events 
      (def/cback pubment (pmouse x y me) mouse)
      
      ;; receive revents 
      (def/cback pubment (prec msg) rec)
      
      ;; ----------------------------------------------------------------------
      ;; -> Void 
      ;; draw : render the given world or this world (if #f)
      (define/private (pdraw) 
        (show (ppdraw)))
      
      ;; -> Scene
      ;; produce the scene for the this state
      (define/public (ppdraw)
        (check-scene-result (name-of draw 'your-draw) (draw (send world get))))
      
      ;; ---------------------------------------------------------------------------------------------
      ;; stop-when 
      (field [stop (let ((s (if (procedure? stop-when) stop-when (first stop-when))))
                     (lambda (x)
                       (define result (s x))
                       (check-result (name-of s 'your-stop-when) boolean? "boolean" result)
                       result))]
             [last-picture (if (pair? stop-when) (second stop-when) #f)])
      
      (define/private (last-draw)
        (when last-picture (set! draw last-picture))
        (pdraw))
      
      ;; ---------------------------------------------------------------------------------------------
      ;; start & stop
      (define/public (callback-stop! msg)
        (stop! (send world get)))
      
      (define (handler re-raise)
        (lambda (e)
          (disable-images-button)
          (stop! (if re-raise e (send world get)))))
      
      (define/public (start!)
        (with-handlers ([exn? (handler #t)])
          (when width ;; and height
            (check-scene-dimensions "your to-draw clause" width height))
          (when register (register-with-host))
          (define w (send world get))
          (cond
            [(stop w) 
             (when last-picture (set! draw last-picture))
             (show-canvas)
             (stop! w)]
            [(stop-the-world? w) 
             (when last-picture (set! draw last-picture))
             (show-canvas)
             (stop! (stop-the-world-world w))]
            [else (show-canvas)])))
      
      (define/public (stop! w)
        (set! live #f)
        (custodian-shutdown-all *rec*))
      
      ;; -------------------------------------------------------------------------
      ;; initialize the world and run 
      (super-new)
      (start!)))))

; (define make-new-world (new-world world%))

;; -----------------------------------------------------------------------------
(define-runtime-path break-btn:path '(lib "icons/break.png"))
(define break-button:label 
  ((bitmap-label-maker (string-constant break-button-label) break-btn:path) '_))

(define-runtime-path image-button:path '(lib "icons/file.gif"))
(define image-button:label ((bitmap-label-maker "Images" image-button:path) '_))

(define aworld%
  (class world% (super-new)
    (inherit-field world0 draw rate width height record?)
    (inherit show callback-stop!)
    
    ;; -> String or false 
    (define/private (recordable-directory)
      (and (path-string? record?) (directory-exists? record?) record?))
    
    ;; Frame Custodian ->* (-> Void) (-> Void)
    ;; adds the stop animation and image creation button, 
    ;; whose callbacks runs as a thread in the custodian
    (define/augment (create-frame/universe frm play-back-custodian)
      (define p (new horizontal-pane% [parent frm][alignment '(center center)]))
      (define (pb)
        (parameterize ([current-custodian play-back-custodian])
          (thread (lambda () (play-back)))
          (stop)))
      (define (switch)
        (send stop-button enable #f)
        (if (recordable-directory) (pb) (send image-button enable #t)))
      (define (stop) 
        (send image-button enable #f)
        (send stop-button enable #f))
      (define-syntax-rule (btn l a y ...)
        (new button% [parent p] [label l] [style '(border)] 
             [callback (lambda a y ...)]))
      (define stop-button 
        (btn break-button:label (b e) (callback-stop! 'stop-images) (switch)))
      (define image-button 
        (btn image-button:label (b e) (pb)))
      (send image-button enable #f)
      (values switch stop))
    
    ;; an argument-recording ppdraw
    (field [image-history '()]) ;; [Listof Evt]
    (define/override (ppdraw)
      (define image (super ppdraw))
      (set! image-history (cons image image-history))
      image)
    
    ;; --> Void
    ;; re-play the history of events; create a png per step; create animated gif
    ;; effect: write to user-chosen directory
    (define/private (play-back)
      ;; World EventRecord -> World 
      (define (world-transition world fst) (apply (car fst) world (cdr fst)))
      ;; --- creating images 
      (define total (+ (length image-history) 1))
      (define digt# (string-length (number->string total)))
      (define imag# 0)
      (define bmps '())
      ;; Image -> Void
      (define (save-image img)
        (define bm (make-object bitmap% width height))
        (define dc (make-object bitmap-dc% bm))
        (send dc clear)
        (send img draw dc 0 0 0 0 width height 0 0 #f)
        (set! imag# (+ imag# 1))
        (send bm save-file (format "i~a.png" (zero-fill imag# digt#)) 'png)
        (set! bmps (cons bm bmps))
        img)
      ;; --- choose place 
      (define img:dir
        (or (recordable-directory)
            (get-directory "image directory:" #f (current-directory))))
      (when img:dir
        (parameterize ([current-directory img:dir])
          (define imageN 
            (if (empty? image-history)
                (save-image (draw world0))
                (first (map save-image image-history))))
          (show (text (format "creating ~a" ANIMATED-GIF-FILE) 18 'red))
          (create-animated-gif rate bmps)
          (show imageN))))))

;; Number [Listof (-> bitmap)] -> Void
;; turn the list of thunks into animated gifs 
;; effect: overwrite the ANIMATED-GIF-FILE (in current directory)
;; [Listof (-> bitmap)] -> Void
;; turn the list of thunks into animated gifs 
;; effect: overwrite the ANIMATED-GIF-FILE (in current directory)
(define (create-animated-gif R bitmap-list)
  (when (file-exists? ANIMATED-GIF-FILE) (delete-file ANIMATED-GIF-FILE))
  (write-animated-gif bitmap-list (if (> +inf.0 R 0) (number->integer R) 5)
                      ANIMATED-GIF-FILE
                      #:one-at-a-time? #t
                      #:loop? #f))

(define ANIMATED-GIF-FILE "i-animated.gif")

