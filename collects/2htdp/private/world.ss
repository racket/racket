#lang scheme/gui

(require "check-aux.ss"
         "timer.ss"
         "last.ss"
         htdp/image
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
       world0 ;; World
       (tick K))      ;; (U (World -> World) (list (World -> World) Nat))
      
      (init
       (on-key K)        ;; World KeyEvent -> World 
       (on-mouse K)      ;; World Nat Nat MouseEvent -> World 
       (on-receive #f)   ;; (U #f (World S-expression -> World))
       (on-draw #f)      ;; (U #f (World -> Scene) (list (World -> Scene) Nat Nat))
       (stop-when False) ;; World -> Boolean 
       (record? #f)      ;; Boolean 
       (register #f))    ;; (U #f String (list String Symbol))
      
      ;; -----------------------------------------------------------------------
      (field (world  world0))
      
      ;; (U World Package) -> Boolean 
      ;; does the new world differ from the old? 
      ;; effect: if so, set world
      (define/private (set-world new-world)
        (when (package? new-world)
          (broadcast (package-message new-world))
          (set! new-world (package-world new-world)))
        (if (equal? world new-world)
            #t
            (begin
              (set! world new-world)
              #f)))
      
      ;; -----------------------------------------------------------------------
      (field [*out* #f] ;; (U #f OutputPort), where to send messages to 
             [*rec* (make-custodian)] ;; Custodian, monitor traffic
             [host  (cond
                      [(string? register) register]
                      [(pair? register) (car register)]
                      [else register])]
             [name  (cond
                      [(string? register) (gensym 'world)]
                      [(pair? register) (second register)]
                      [else register])])
      
      (define/private (register-with-host)
        (define FMTtry "unable to register with ~a after ~s tries")
        (define FMTcom "unable to register with ~a due to protocol problems")
        ;; try to register with the server n times 
        (define (register n)
          (printf "trying to register with ~a ...\n" host)
          (with-handlers ((tcp-eof? 
                           (lambda (x) 
                             (error 'register FMTcom host)))
                          (exn:fail:network? 
                           (lambda (x)
                             (if (= n 1) 
                                 (error 'register FMTtry host TRIES)
                                 (begin (sleep PAUSE)
                                        (register (- n 1)))))))
            (define-values (in out) (tcp-connect host SQPORT))
            (tcp-send out `(REGISTER ,(if name name (gensym 'world))))
            (if (eq? (tcp-receive in) 'okay) 
                (values in out)
                (raise tcp-eof))))
        ;; --- now register, obtain connection, and spawn a thread for receiving
        (parameterize ([current-custodian *rec*])
          (define-values (in out) (register TRIES))
          (define dis (text "the universe disappeared" 11 'red))
          (define (RECEIVE)
            (sync 
             (handle-evt
              in
              (lambda (in) 
                (with-handlers ((tcp-eof? (compose (handler #f)
                                                   (lambda (e)
                                                     (set! draw (lambda (w) dis))
                                                     (pdraw)
                                                     e))))
                  ;; --- "the universe disconnected" should come from here ---
                  (define msg (tcp-receive in))
                  (cond
                    [(sexp? msg) (prec msg) (RECEIVE)] ;; break loop if EOF
                    [#t (error 'RECEIVE "sexp expected, received: ~e" msg)]))))))
          (printf "... successful registered and ready to receive\n")
          (set! *out* out)
          (thread RECEIVE)))
      
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
          (set! width  (if width width (image-width fst-scene)))
          (set! height (if height height (image-height fst-scene)))
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
                           (label (if name (format "~a's World" name) "World"))
                           (stretchable-width #f)
                           (stretchable-height #f)
                           (style '(no-resize-border metal))))
        (define editor-canvas 
          (new (class editor-canvas%
                 (super-new)
                 ;; deal with keyboard events 
                 (define/override (on-char e) 
                   (when live (pkey (send e get-key-code))))
                 ;; deal with mouse events if live and within range 
                 (define/override (on-event e)
                   (define l (mouse-event->parts e))
                   (when live
                     (when (and (<= 0 (first l) width) (<= 0 (second l) height))
                       (pmouse . l)))))
               (parent frame)
               (editor visible)
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
      
      ;; -----------------------------------------------------------------------
      ;; callbacks 
      (field
       (key    on-key)
       (mouse  on-mouse)
       (rec    on-receive))
      
      (define-syntax-rule (def/pub-cback (name arg ...) transform)
        ;; Any ... -> Boolean
        (define/public (name arg ...) 
          (queue-callback 
           (lambda ()
             (with-handlers ([exn:break? (handler #f)][exn? (handler #t)])
               (define changed-world? (set-world (transform world arg ...)))
               (unless changed-world? 
                 (when draw (pdraw))
                 (when (pstop) 
                   (callback-stop! 'name)
                   (enable-images-button)))
               changed-world?)))))
      
      ;; tick, tock : deal with a tick event for this world 
      (def/pub-cback (ptock) tick)
      
      ;; key events 
      (def/pub-cback (pkey ke) key)
      
      ;; mouse events 
      (def/pub-cback (pmouse x y me) mouse)
      
      ;; receive revents 
      (def/pub-cback (prec msg) rec)
      
      ;; -----------------------------------------------------------------------
      ;; draw : render this world 
      (define/private (pdraw) (show (ppdraw)))
      
      (define/private (ppdraw)
        (check-scene-result (name-of draw 'your-draw) (draw world)))
      
      ;; -----------------------------------------------------------------------
      ;; stop-when 
      (field [stop  stop-when])
      
      (define/private (pstop)
        (define result (stop world))
        (check-result (name-of stop 'your-stop-when) boolean? "boolean" result)
        result)
      
      ;; -----------------------------------------------------------------------
      ;; start & stop
      (define/public (callback-stop! msg)
        (stop! world))
      
      (define (handler re-raise)
        (lambda (e)
          (disable-images-button)
          (stop! (if re-raise e world))))
      
      (define/public (start!)
        (when draw (show-canvas))
        (when host (register-with-host)))
      
      (define/public (stop! w)
        (set! live #f)
        (custodian-shutdown-all *rec*))
      
      ;; -------------------------------------------------------------------------
      ;; initialize the world and run 
      (super-new)
      (start!)
      (when (stop-when world) (stop! world))))))

;; -----------------------------------------------------------------------------
(define-runtime-path break-btn:path '(lib "icons/break.png"))
(define break-button:label 
  ((bitmap-label-maker (string-constant break-button-label) break-btn:path) '_))

(define-runtime-path image-button:path '(lib "icons/file.gif"))
(define image-button:label ((bitmap-label-maker "Images" image-button:path) '_))

(define aworld%
  (class world% (super-new)
    (inherit-field world0 tick key mouse rec draw rate width height)
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
