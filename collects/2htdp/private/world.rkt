#lang scheme/gui

(require "check-aux.ss"
         "timer.ss"
         "last.ss"
         "checked-cell.ss"
         "stop.ss"
         "universe-image.ss"
         "utilities.rkt"
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

;; -----------------------------------------------------------------------------
;; packages for broadcasting information to the universe 

(define-struct package (world message) #:transparent)

;; World Sexp -> Package 
(define (create-package w m)
  (check-arg 'make-package (sexp? m) 'sexp "second" m)
  (make-package w m))

(provide
 (rename-out (create-package make-package)) ;; World S-expression -> Package
 package? ;; Any -> Package
 )

(define world%
  (last-mixin
   (clock-mixin
    (class* object% (start-stop<%>)
      (inspect #f)
      
      (init-field
       world0            ;; World
       (name #f)         ;; (U #f String)
       (state #f)        ;; Boolean 
       (register #f)     ;; (U #f IP)
       (check-with True) ;; Any -> Boolean 
       (tick K))         ;; (U (World -> World) (list (World -> World) Nat))
      
      (init
       (on-key K)        ;; World KeyEvent -> World 
       (on-release K)    ;; World KeyEvent -> World 
       (on-mouse K)      ;; World Nat Nat MouseEvent -> World 
       (on-receive #f)   ;; (U #f (World S-expression -> World))
       (on-draw #f)      ;; (U #f (World -> Scene) (list (World -> Scene) Nat Nat))
       (stop-when False) ;; World -> Boolean 
       (record? #f))     ;; Boolean 
      
      ;; -----------------------------------------------------------------------
      (field
       [world
        (new checked-cell% [msg "World"] [value0 world0] [ok? check-with]
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
                 [(procedure? on-draw) on-draw]
                 [(pair? on-draw)      (first on-draw)]
                 [else on-draw]))
       (live   (not (boolean? draw)))
       (width  (if (pair? on-draw) (second on-draw) #f))
       (height (if (pair? on-draw) (third on-draw) #f)))
      
      ;; the visible world 
      (field [enable-images-button void] ;; used if stop-when call produces #t
             [disable-images-button void]
             [visible (new pasteboard%)])
      
      (define (show-canvas)
        (send visible set-cursor (make-object cursor% 'arrow))
        (let ([fst-scene (ppdraw)])
          (if (2:image? fst-scene)
              (begin
                (set! width  (if width width (+ (image-width fst-scene) 1)))
                (set! height (if height height (+ (image-height fst-scene) 1))))
              (begin
                (set! width  (if width width (image-width fst-scene)))
                (set! height (if height height (image-height fst-scene)))))              
          (create-frame)
          (show fst-scene)))
      
      ;; effect: create, show and set the-frame
      (define/pubment (create-frame)
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
          (new (class editor-canvas%
                 (super-new)
                 ;; deal with keyboard events 
                 (define/override (on-char e) 
                   (when live
                     (let ([e:str (key-event->parts e)])
                       (if (string=? e:str "release")
                           (prelease (key-release->parts e))
                           (pkey e:str)))))
                 ;; deal with mouse events if live and within range 
                 (define/override (on-event e)
                   (define-values (x y me) (mouse-event->parts e))
                   (when live
                     (cond
                       [(and (<= 0 x width) (<= 0 y height)) (pmouse x y me)]
                       [(member me '("leave" "enter")) (pmouse x y me)]
                       [else (void)]))))
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
                     (inner (values void void) create-frame frame play-back:cust))
        (send editor-canvas focus)
        (send frame show #t))
      
      ;; Image -> Void
      ;; show the image in the visible world
      (define/public (show pict)
        (send visible begin-edit-sequence)
        (send visible lock #f)
        (let ([s (send visible find-first-snip)]
              [c (send visible get-canvas)])
          (when s (send visible delete s))
          (send visible insert (send pict copy) 0 0))
        (send visible lock #t)
        (send visible end-edit-sequence))
      
      ;; ----------------------------------------------------------------------
      ;; callbacks 
      (field
       (key    on-key)
       (release on-release)
       (mouse  on-mouse)
       (rec    on-receive))
      
      (define drawing #f) ;; Boolean; is a draw callback scheduled?
      (define (set-draw#!) (set! draw# (random 3)) (set! drawing #f))
      (define draw# 0) 
      (set-draw#!)
      
      (define-syntax-rule (def/pub-cback (name arg ...) transform)
        ;; Any ... -> Boolean
        (define/public (name arg ...) 
          (queue-callback 
           (lambda ()
             (with-handlers ([exn? (handler #t)])
               (define tag (format "~a callback" 'transform))
               (define nw (transform (send world get) arg ...))
               (define (d) (pdraw) (set-draw#!))
               ;; ---
               ;; [Listof (Box [d | void])]
               (define w '()) 
               ;; set all to void, then w to null 
               ;; when a high priority draw is scheduledd
               ;; --- 
               (when (package? nw)
                 (broadcast (package-message nw))
                 (set! nw (package-world nw)))
               (if (stop-the-world? nw)
                   (begin
                     (set! nw (stop-the-world-world nw))
                     (send world set tag nw)
                     (when last-picture
                       (set! draw last-picture))
                     (when draw (pdraw))
                     (callback-stop! 'name)
                     (enable-images-button))
                   (let ([changed-world? (send world set tag nw)])
                     ;; this is the old "Robby optimization" see checked-cell:
                     ; unless changed-world? 
                     (when draw 
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
                          (set! draw# (- draw# 1))]))
                     (when (pstop)
                       (when last-picture 
                         (set! draw last-picture)
                         (pdraw))
                       (callback-stop! 'name)
                       (enable-images-button))
                     changed-world?)))))))
      
      ;; tick, tock : deal with a tick event for this world 
      (def/pub-cback (ptock) tick)
      
      ;; key events 
      (def/pub-cback (pkey ke) key)
      
      ;; release events 
      (def/pub-cback (prelease ke) release)
      
      ;; mouse events 
      (def/pub-cback (pmouse x y me) mouse)
      
      ;; receive revents 
      (def/pub-cback (prec msg) rec)
      
      ;; ----------------------------------------------------------------------
      ;; -> Void 
      ;; draw : render the given world or this world (if #f)
      (define/private (pdraw) 
        (show (ppdraw)))
      
      ;; -> Scene
      ;; produce the scene for the this state
      (define/private (ppdraw)
        (check-scene-result (name-of draw 'your-draw) (draw (send world get))))
      
      ;; -----------------------------------------------------------------------
      ;; stop-when 
      (field [stop (if (procedure? stop-when) stop-when (first stop-when))]
             [last-picture (if (pair? stop-when) (second stop-when) #f)])
      
      (define/private (pstop)
        (define result (stop (send world get)))
        (check-result (name-of stop 'your-stop-when) boolean? "boolean" result)
        result)
      
      ;; ----------------------------------------------------------------------
      ;; start & stop
      (define/public (callback-stop! msg)
        (stop! (send world get)))
      
      (define (handler re-raise)
        (lambda (e)
          (disable-images-button)
          (stop! (if re-raise e (send world get)))))
      
      (define/public (start!)
        (queue-callback
         (lambda ()
           (with-handlers ([exn? (handler #t)])
             (when draw (show-canvas))
             (when register (register-with-host))))))
      
      (define/public (stop! w)
        (set! live #f)
        (custodian-shutdown-all *rec*))
      
      ;; -------------------------------------------------------------------------
      ;; initialize the world and run 
      (super-new)
      (start!)
      (let ([w (send world get)])
        (cond
          [(stop w) (stop! (send world get))]
          [(stop-the-world? w) 
           (stop! (stop-the-world-world (send world get)))]))))))

;; -----------------------------------------------------------------------------
(define-runtime-path break-btn:path '(lib "icons/break.png"))
(define break-button:label 
  ((bitmap-label-maker (string-constant break-button-label) break-btn:path) '_))

(define-runtime-path image-button:path '(lib "icons/file.gif"))
(define image-button:label ((bitmap-label-maker "Images" image-button:path) '_))

(define aworld%
  (class world% (super-new)
    (inherit-field world0 tick key release mouse rec draw rate width height)
    (inherit show callback-stop!)
    
    ;; Frame Custodian ->* (-> Void) (-> Void)
    ;; adds the stop animation and image creation button, 
    ;; whose callbacks runs as a thread in the custodian
    (define/augment (create-frame frm play-back-custodian)
      (define p (new horizontal-pane% [parent frm][alignment '(center center)]))
      (define (switch)
        (send stop-button enable #f)
        (send image-button enable #t))
      (define (stop) 
        (send image-button enable #f)
        (send stop-button enable #f))
      (define-syntax-rule (btn l a y ...)
        (new button% [parent p] [label l] [style '(border)] 
             [callback (lambda a y ...)]))
      (define stop-button 
        (btn break-button:label (b e) (callback-stop! 'stop-images) (switch)))
      (define image-button 
        (btn image-button:label (b e)
             (parameterize ([current-custodian play-back-custodian])
               (thread (lambda () (play-back)))
               (stop))))
      (send image-button enable #f)
      (values switch stop))
    
    (field [event-history '()]) ;; [Listof Evt]
    ;; Symbol  Any *-> Void
    (define/private (add-event type . stuff)
      (set! event-history (cons (cons type stuff) event-history)))
    
    ;; --- new callbacks ---
    (define-syntax-rule (def/over-cb (pname name arg ...))
      (define/override (pname arg ...) 
        (when (super pname arg ...) (add-event name arg ...))))
    
    (def/over-cb (ptock tick))
    (def/over-cb (pkey key e))
    (def/over-cb (prelease release e))
    (def/over-cb (pmouse mouse x y me))
    (def/over-cb (prec rec m))
    
    ;; --> Void
    ;; re-play the history of events; create a png per step; create animated gif
    ;; effect: write to user-chosen directory
    (define/private (play-back)
      ;; World EventRecord -> World 
      (define (world-transition world fst) (apply (car fst) world (cdr fst)))
      ;; --- creating images 
      (define total (+ (length event-history) 1))
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
        (set! bmps (cons bm bmps)))
      ;; --- choose place 
      (define img:dir (get-directory "image directory:" #f (current-directory)))
      (when img:dir
        (parameterize ([current-directory img:dir])
          (define last 
            (foldr (lambda (event world)
                     (save-image (draw world))
                     (show (text (format "~a/~a created" imag# total) 18 'red))
                     (world-transition world event))
                   world0 
                   event-history))
          (show (text (format "creating ~a" ANIMATED-GIF-FILE) 18 'red))
          (create-animated-gif rate (reverse bmps))
          (show (draw last)))))))

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
