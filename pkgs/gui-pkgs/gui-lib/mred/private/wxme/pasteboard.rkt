#lang racket/base
(require racket/class
         racket/port
         racket/file
         "../syntax.rkt"
         "const.rkt"
         "private.rkt"
         racket/snip/private/private
         "editor.rkt"
         "editor-data.rkt"
         "undo.rkt"
         racket/snip/private/snip
         racket/snip/private/snip-flags
         racket/snip/private/style
         "standard-snip-admin.rkt"
         "keymap.rkt"
         (only-in "cycle.rkt"
                  printer-dc%
                  set-pasteboard%!)
         "wordbreak.rkt"
         "stream.rkt"
         "wx.rkt")

(provide pasteboard%
         add-pasteboard-keymap-functions)

;; ----------------------------------------

(define LINE-HEIGHT 16.0)

(define DOT-WIDTH 5.0)
(define HALF-DOT-WIDTH 2.0)

(define (inbox? lx x)
  (and ((- lx HALF-DOT-WIDTH) . <= . x)
       ((+ (- lx HALF-DOT-WIDTH) DOT-WIDTH) . >= . x)))

(define black-brush (send the-brush-list find-or-create-brush "black" 'xor))
(define white-brush (send the-brush-list find-or-create-brush "white" 'solid))
(define invisi-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
(define invisi-brush (send the-brush-list find-or-create-brush "black" 'transparent))
(define rb-pen (send the-pen-list find-or-create-pen (get-highlight-background-color) 1 'xor-dot))
(define rb-brush (send the-brush-list find-or-create-brush (get-highlight-background-color) 'solid))

(define arrow (make-object cursor% 'arrow))

;; ----------------------------------------

(define-struct loc (x y w h r b hm vm
                      startx starty
                      selected? need-resize?
                      snip)
  #:mutable)

;; ----------------------------------------

(defclass pasteboard% editor%
  (inherit-field s-admin
                 s-custom-cursor
                 s-custom-cursor-overrides?
                 s-own-caret?
                 s-caret-snip
                 s-keymap
                 s-style-list
                 s-noundomode
                 s-modified?
                 s-offscreen
                 s-filename
                 s-temp-filename?
                 s-user-locked?
                 s-need-on-display-size?)
  (inherit on-change
           get-default-style
           set-modified
           on-paint
           wait-sequence-lock
           begin-sequence-lock
           end-sequence-lock
           do-own-caret
           on-focus
           scroll-editor-to
           do-set-caret-owner
           install-copy-buffer
           begin-copy-buffer
           end-copy-buffer
           free-old-copies
           do-write-headers-footers
           read-snips-from-file
           do-own-x-selection
           do-buffer-paste
           add-undo-rec
           get-dc
           on-local-event
           on-local-char
           on-edit-sequence
           after-edit-sequence
           on-display-size)

  (define dragable? #t)
  (define selection-visible? #t)

  (define snips #f)
  (define last-snip #f)

  (define (in-snip-locs)
    (make-do-sequence
     (lambda () (values snip->loc
                        snip->next
                        snips
                        values
                        #f
                        #f))))

  (define snip-admin (new standard-snip-admin% [editor this]))
  
  (define last-time 0)
  (define start-x 0.0)
  (define start-y 0.0)
  (define last-x 0.0)
  (define last-y 0.0)

  (define orig-x 0.0)
  (define orig-y 0.0)
  (define orig-w 0.0)
  (define orig-h 0.0)
  
  (define max-width 'none)
  (define min-width 'none)
  (define max-height 'none)
  (define min-height 'none)

  (define keep-size? #f)
  (define dragging? #f)
  (define rubberband? #f)

  (define rb-x 0.0)
  (define rb-y 0.0)
  (define rb-w 0.0)
  (define rb-h 0.0)

  (define need-resize? #f)

  (define resizing #f) ; a snip
  (define sizedxm 0.0)
  (define sizedym 0.0)

  (define scroll-step LINE-HEIGHT)

  (define total-width 0.0)
  (define total-height 0.0)
  (define real-width 0.0)
  (define real-height 0.0)

  (define update-left 0.0)
  (define update-right 0.0)
  (define update-right-end #f)
  (define update-top 0.0)
  (define update-bottom 0.0)
  (define update-bottom-end #f)
  (define update-nonempty? #f)
  (define no-implicit-update? #f)

  (define size-cache-invalid? #f)
  (define write-locked 0)
  (define flow-locked? #f)

  (define sequence 0)

  (define delayedscrollbias 'none)
  (define delayedscrollsnip #f)
  (define delayedscroll-x 0.0)
  (define delayedscroll-y 0.0)
  (define delayedscroll-w 0.0)
  (define delayedscroll-h 0.0)

  (define sequence-streak? #f)

  (define changed? #f)

  (define prev-mouse-snip #f)

  (super-new)

  ;; ----------------------------------------

  (define/private (rubber-band-update x y w h)
    (when (and s-admin
               (not (zero? w))
               (not (zero? h)))
      (let-values ([(x w)
                    (if (w . < . 0)
                        (values (+ x w) (- w))
                        (values x w))]
                   [(y h)
                    (if (h . < . 0)
                        (values (+ y h) (- h))
                        (values y h))])
        (let ([r (+ x w)]
              [b (+ y h)])
          (let-boxes ([vx 0.0]
                      [vy 0.0]
                      [vw 0.0]
                      [vh 0.0])
              (send s-admin get-view vx vy vw vh)
            (let ([x (max x vx)]
                  [y (max y vy)]
                  [r (min r (+ vx vw))]
                  [b (min b (+ vy vh))])
              (unless (or (x . >= . r)
                          (y . >= . b))
                (set! rb-x x)
                (set! rb-y y)
                (set! rb-w (- r x))
                (set! rb-h (- b y))
                (update rb-x rb-y rb-w rb-h))))))))

  (def/override (adjust-cursor [mouse-event% event])
    (if (not s-admin)
        #f
        (let-boxes ([scrollx 0.0]
                    [scrolly 0.0]
                    [dc #f])
            (set-box! dc (send s-admin get-dc scrollx scrolly))
          (if (not dc)
              #f
              (let ([x (+ (send event get-x) scrollx)]
                    [y (+ (send event get-y) scrolly)])
                (or (and (not s-custom-cursor-overrides?)
                         (or (and s-caret-snip (send event dragging?)
                                  (let-boxes ([x 0.0]
                                              [y 0.0])
                                      (get-snip-location s-caret-snip x y)
                                    (let ([c (send s-caret-snip adjust-cursor dc
                                                   (- x scrollx) (- y scrolly)
                                                   x y event)])
                                      c)))
                             ;; find snip:
                             (let ([snip (find-snip x y)])
                               (and snip
                                    (eq? snip s-caret-snip)
                                    (let-boxes ([x 0.0] [y 0.0])
                                        (get-snip-location snip x y)
                                      (let ([c (send snip adjust-cursor dc (- x scrollx) (- y scrolly) 
                                                     x y event)])
                                        c))))))
                    s-custom-cursor
                    arrow))))))

  (def/override (on-event [mouse-event% event])
    (when s-admin
      (let-values ([(dc x y scrollx scrolly)
                    ;; first, find clicked-on snip:
                    (let ([x (send event get-x)]
                          [y (send event get-y)])
                      (let-boxes ([scrollx 0.0]
                                  [scrolly 0.0]
                                  [dc #f])
                          (set-box! dc (send s-admin get-dc scrollx scrolly))
                        ;; FIXME: old code returned if !dc
                        (values dc (+ x scrollx) (+ y scrolly) scrollx scrolly)))])
        (let ([snip (find-snip x y)])
          (when (and prev-mouse-snip
                     (not (eq? snip prev-mouse-snip)))
            (let ([loc (snip->loc prev-mouse-snip)])
              (send prev-mouse-snip on-event
                    dc (- (loc-x loc) scrollx) (- (loc-y loc) scrolly) 
                    (loc-x loc) (loc-y loc)
                    event)))
          (set! prev-mouse-snip #f)
          (when (and snip
                     (has-flag? (snip->flags snip) HANDLES-ALL-MOUSE-EVENTS)
                     (not (eq? snip s-caret-snip)))
            (let ([loc (snip->loc snip)])
              (set! prev-mouse-snip snip)
              (send snip on-event
                    dc (- (loc-x loc) scrollx) (- (loc-y loc) scrolly) 
                    (loc-x loc) (loc-y loc)
                    event)))
          (if (and s-caret-snip
                   (or (not (send event button-down?))
                       (eq? snip s-caret-snip)))
              (let ([loc (snip->loc s-caret-snip)])
                (send s-caret-snip on-event
                      dc (- (loc-x loc) scrollx) (- (loc-y loc) scrolly) 
                      (loc-x loc) (loc-y loc)
                      event))
              (on-local-event event))))))

  (def/override (on-default-event [mouse-event% event])
    (when s-admin
      (let-boxes ([scrollx 0.0]
                  [scrolly 0.0]
                  [dc #f])
          (set-box! dc (send s-admin get-dc scrollx scrolly))
        (when dc
          (let-boxes ([x (+ (send event get-x) scrollx)]
                      [y (+ (send event get-y) scrolly)])
              
              (interactive-adjust-mouse x y)
            
            (when (or (send event button-down?)
                      (and (send event moving?) (not (send event dragging?)))
                      (send event button-up?))
              (set! keep-size? #f)
              
              (when dragging?
                (if resizing
                    (begin
                      (begin-edit-sequence)
                      ;; move & resize back without undo
                      (when (or (sizedxm . < . 0.0)
                                (sizedym . < . 0.0))
                        (move-to resizing orig-x orig-y))
                      (resize resizing orig-w orig-h)
                      (set! dragging? #f)
                      ;; re-move and re-size with undo:
                      (do-event-resize last-x last-y)
                      (after-interactive-resize resizing)
                      (end-edit-sequence)
                      (set! resizing #f))
                    (finish-dragging event)))

              (when rubberband?
                (set! rubberband? #f)
                (rubber-band-update start-x start-y (- last-x start-x) (- last-y start-y))
                (add-selected start-x start-y (- last-x start-x) (- last-y start-y))
                (update-all)))

            (if (or (send event button-down?)
                    (and (send event dragging?)
                         (not dragging?)
                         (not rubberband?)))

                (let ([snip (find-snip x y)])
                  (if dragable?
                      (begin
                        (if snip
                            (let ([loc (snip->loc snip)])
                              (set! orig-x (loc-x loc))
                              (set! orig-y (loc-y loc))
                              (set! orig-w (loc-w loc))
                              (set! orig-h (loc-h loc))
                              (if (not (loc-selected? loc))
                                  (begin
                                    (unless (send event get-shift-down)
                                      (no-selected))
                                    (set-caret-owner #f)
                                    (add-selected snip)
                                    (init-dragging event))
                                  (let ([interval (abs (- (send event get-time-stamp)
                                                          last-time))])
                                    (if (and (send event button-down?)
                                             (interval . < . (if s-keymap
                                                                 (send s-keymap get-double-click-interval)
                                                                 (get-double-click-threshold))))
                                        (on-double-click snip event)
                                        (let-boxes ([dx sizedxm]
                                                    [dy sizedym]
                                                    [f? #f])
                                            (set-box! f? (find-dot loc x y dx dy))
                                          (set! sizedxm dx)
                                          (set! sizedym dy)
                                          (when f?
                                            (set! resizing snip))
                                          (init-dragging event)))))
                              (when (send event button-down?)
                                (set! last-time (send event get-time-stamp))))
                            (begin
                              (unless (send event get-shift-down)
                                (no-selected))
                              (set-caret-owner #f)
                              (set! rubberband? #t)))
                        (set! start-x x)
                        (set! last-x x)
                        (set! start-y y)
                        (set! last-y y))
                      ;; not dragable:
                      (set-caret-owner snip)))

                ;; not a new click:
                (when dragable?
                  (when (send event dragging?)
                    (cond
                     [rubberband?
                      (begin-edit-sequence)
                      ;; erase old
                      (rubber-band-update start-x start-y (- last-x start-x) (- last-y start-y))
                      ;; draw new:
                      (rubber-band-update start-x start-y (- x start-x) (- y start-y))
                      (end-edit-sequence)]
                     [resizing
                      (do-event-resize x y)]
                     [else
                      (do-event-move x y)]))
                  (set! last-x x)
                  (set! last-y y))))))))

  (def/public (on-double-click [snip% snip] [mouse-event% evt])
    (when (has-flag? (snip->flags snip) HANDLES-EVENTS)
      (no-selected)
      (set-caret-owner snip)))

  (def/override (on-char [key-event% event])
    (when s-admin
      (let-boxes ([scrollx 0.0]
                  [scrolly 0.0]
                  [dc #f])
          (set-box! dc (send s-admin get-dc scrollx scrolly))
        (when dc
          (let ([x (+ (send event get-x) scrollx)]
                [y (+ (send event get-y) scrolly)])
            (if s-caret-snip
                (let ([loc (snip->loc s-caret-snip)])
                  (send s-caret-snip on-char
                        dc (loc-x loc) (loc-y loc) (- x scrollx) (- y scrolly)
                        event))
                (on-local-char event)))))))

  (def/override (on-default-char [key-event% event])
    (when s-admin
      (let ([code (send event get-key-code)])
        (case code
          [(#\rubout #\backspace)
           (delete)]
          [(right)
           (move 1 0)]
          [(left)
           (move -1 0)]
          [(up)
           (move 0 -1)]
          [(down)
           (move 0 1)]))))

  (define/private (init-dragging e)
    (define (phase1)
      (if resizing
          (if (not (can-interactive-resize? resizing))
              (set! resizing #f)
              (begin
                (on-interactive-resize resizing)
                (phase2)))
          (when (can-interactive-move? e)
            (on-interactive-move e)
            (phase2))))
    (define (phase2)
      (set! dragging? #t)
      (set! keep-size? #t)
      (let loop ([s #f])
        (let ([s (find-next-selected-snip s)])
          (when s
            (let ([loc (snip->loc s)])
              (set-loc-startx! loc (loc-x loc))
              (set-loc-starty! loc (loc-y loc)))
            (loop s)))))
    (phase1))

  (define/private (finish-dragging e)
    (begin-edit-sequence)

    ;; move back without undo and remember final
    (let loop ([s #f])
      (let ([s (find-next-selected-snip s)])
        (when s
          (let* ([loc (snip->loc s)]
                 [x (loc-startx loc)]
                 [y (loc-starty loc)])
            (set-loc-startx! loc (loc-x loc))
            (set-loc-starty! loc (loc-y loc))
            (move-to s x y))
          (loop s))))

    (set! dragging? #f)

    ;; move to final position with undo:
    (let loop ([s #f])
      (let ([s (find-next-selected-snip s)])
        (when s
          (let* ([loc (snip->loc s)])
            (move-to s (loc-startx loc) (loc-starty loc)))
          (loop s))))

    (after-interactive-move e)
    (end-edit-sequence))

  (define/private (do-event-move event-x event-y)
    (let ([dx (- event-x start-x)]
          [dy (- event-y start-y)])
      (begin-edit-sequence)
      
      (let loop ([s #f])
        (let ([s (find-next-selected-snip s)])
          (when s
            (let ([loc (snip->loc s)])
              (let-boxes ([x (+ (loc-startx loc) dx)]
                          [y (+ (loc-starty loc) dy)])
                  (interactive-adjust-move s x y)
                (move-to s x y)))
            (loop s))))

      (end-edit-sequence)))

  (define/private (do-event-resize event-x event-y)
    (let ([dx (- event-x start-x)]
          [dy (- event-y start-y)])
      (let-boxes ([w (max 0.0 (+ orig-w (* dx sizedxm)))]
                  [h (max 0.0 (+ orig-h (* dy sizedym)))])
          (interactive-adjust-resize resizing w h)
        (let ([w (max 0.0 w)]
              [h (max 0.0 h)])
          (let ([x (+ orig-x
                      (if (sizedxm . < . 0)
                          (- orig-w w)
                          0.0))]
                [y (+ orig-y
                      (if (sizedym . < . 0)
                          (- orig-h h)
                          0.0))])
  
            (begin-edit-sequence)
  
            (when (resize resizing w h)
              (when (or (sizedxm . < . 0)
                        (sizedym . < . 0))
                (move-to resizing x y)))

            (end-edit-sequence))))))

  (def/public (interactive-adjust-mouse [(make-box real?) x] [(make-box real?) y])
    (set-box! x (max 0.0 (unbox x)))
    (set-box! y (max 0.0 (unbox y))))

  (def/public (interactive-adjust-resize [snip% s] [(make-box real?) w] [(make-box real?) h])
    (void))

  (def/public (interactive-adjust-move [snip% s][(make-box real?) x] [(make-box real?) y])
    (set-box! x (max 0.0 (unbox x)))
    (set-box! y (max 0.0 (unbox y))))
  
  ;; ----------------------------------------
  
  (def/public (set-selected [snip% snip])
    (begin-edit-sequence)
    (no-selected)
    (add-selected snip)
    (end-edit-sequence))

  (define/private (do-select snip on?)
    (let ([loc (and snip (snip->loc snip))])
      (when (and loc
                 (not (eq? (loc-selected? loc) on?)))
        (set! write-locked (add1 write-locked))
        (if (can-select? snip on?)
            (begin
              (on-select snip on?)
              (set! write-locked (sub1 write-locked))
              (set-loc-selected?! loc on?)
              (after-select snip on?)
              (update-location loc))
            (set! write-locked (sub1 write-locked))))))

  (def/public (remove-selected [snip% snip])
    (do-select snip #f))
  
  (define/private (add-selected-region x y w h)
    (let-values ([(x w)
                  (if (w . < . 0)
                      (values (+ x w) (- w))
                      (values x w))]
                 [(y h)
                  (if (h . < . 0)
                      (values (+ y h) (- h))
                      (values y h))])
      (let ([r (+ x w)]
            [b (+ y h)])
        
        (begin-edit-sequence)

        (let loop ([s snips])
          (when s
            (let ([loc (snip->loc s)])
              (when (and
                     loc
                     (not (loc-selected? loc))
                     ((loc-x loc) . <= . r)
                     ((loc-y loc) . <= . b)
                     ((loc-r loc) . >= . x)
                     ((loc-b loc) . >= . y))
                (add-selected s)))
            (loop (snip->next s))))

        (end-edit-sequence))))

  (define/public (add-selected . args)
    (case-args
     args
     [([real? x] [real? y] [real? w] [real? h])
      (add-selected-region x y w h)]
     [([snip% snip])
      (do-select snip #t)]
     (method-name 'pasteboard% 'add-selected)))
  
  (def/override (select-all)
    (begin-edit-sequence)
    (let loop ([s snips])
      (when s
        (add-selected s)
        (loop (snip->next s))))
    (end-edit-sequence))
				  
  (def/public (no-selected)
    (begin-edit-sequence)
    (let loop ([s snips])
      (when s
        (remove-selected s)
        (loop (snip->next s))))
    (end-edit-sequence))

  ;; ----------------------------------------

  (define/private (do-insert snip before x y)
    (unless (or s-user-locked?
                (not (zero? write-locked))
                (send snip is-owned?))
      (when (not (snip->snipclass snip))
        (error (method-name 'pasteboard% 'insert)
               "cannot insert a snip without a snipclass: ~e" 
               snip))

      (set! write-locked (add1 write-locked))
      (begin-edit-sequence)
      (let ([ok?
             (or (can-insert? snip before x y)
                 (begin
                   (end-edit-sequence)
                   (set! write-locked (sub1 write-locked))
                   #f))])
        (when ok?
          (on-insert snip before x y)
          (set! write-locked (sub1 write-locked))

          (let ([snip (if (send snip is-owned?)
                          ;; disaster: can/on-insert made the snip owned
                          (new image-snip%)
                          snip)])
            
            (let ([search (and before
                               (snip->loc before)
                               before)])
  
              (set-snip-next! snip search)
              (if search
                  (begin
                    (set-snip-prev! snip (snip->prev search))
                    (set-snip-prev! search snip))
                  (begin
                    (set-snip-prev! snip last-snip)
                    (set! last-snip snip)))
              (if (snip->prev snip)
                  (set-snip-next! (snip->prev snip) snip)
                  (set! snips snip)))
            
            (let ([loc (make-loc
                        x y 0.0 0.0 0.0 0.0 0.0 0.0
                        0.0 0.0
                        #f #t
                        snip)])
              (set-snip-loc! snip loc)
            
              (set-snip-style! snip (send s-style-list convert (snip->style snip)))
              (when (eq? (snip->style snip)
                         (send s-style-list basic-style))
                (let ([s (get-default-style)])
                  (when s
                    (set-snip-style! snip s))))
              
              (send snip size-cache-invalid)
              
              (snip-set-admin snip snip-admin)
              
              (when (zero? s-noundomode)
                (let ([is (make-object insert-snip-record% snip sequence-streak?)])
                  (add-undo-rec is)))
              (when (positive? sequence)
                (set! sequence-streak? #t))
              
              (set! changed? #t)
              
              (unless s-modified?
                (set-modified #t))
            
              (set! need-resize? #t)
              (update-location loc)
              
              (set! write-locked (add1 write-locked))
              (end-edit-sequence)
              (set! write-locked (sub1 write-locked))
              
              (when (zero? sequence)
                (update-needed))
              
              (after-insert snip before x y)))))))

  (define/override (insert . args)
    (case-args
     args
     [([snip% snip] [(make-or-false snip%) [before #f]])
      (let-values ([(x y) (get-center)])
        (do-insert snip before x y))]
     [([snip% snip] [(make-or-false snip%) before] [real? x] [real? y])
      (do-insert snip before x y)]
     [([snip% snip] [real? x] [real? y])
      (do-insert snip #f x y)]
     (method-name 'pasteboard% 'insert)))

  (define/private (delete-some del?)
    (unless (or s-user-locked?
                (not (zero? write-locked)))
      (let ([del (make-object delete-snip-record% sequence-streak?)])
        (when (positive? sequence)
          (set! sequence-streak? #t))

        (begin-edit-sequence)

        (let loop ([s snips])
          (when s
            (let ([next (snip->next s)])
              (when (del? s)
                (-delete s del))
              (loop next))))
        
        (when (zero? s-noundomode)
          (add-undo-rec del))

        (end-edit-sequence))))

  (define/public (delete . args)
    (case-args
     args
     [()
      (delete-some (lambda (s) 
                     (let ([l (and s (snip->loc s))])
                       (and l ;; deleted already!
                            (loc-selected? l)))))]
     [([snip% s])
      (unless (or s-user-locked?
                  (not (zero? write-locked)))
        (let ([del (make-object delete-snip-record% sequence-streak?)])
          (when (positive? sequence)
            (set! sequence-streak? #t))
          (-delete s del)
          (when (zero? s-noundomode)
            (add-undo-rec del))))]
     (method-name 'pasteboard% 'insert)))

  (def/public (erase)
    (delete-some (lambda (s) #t)))
  
  (define/private (-delete del-snip del)
    (when (snip->loc del-snip)
      (when (and prev-mouse-snip
                 (object=? del-snip prev-mouse-snip))
        (set! prev-mouse-snip #f))
      (set! write-locked (add1 write-locked))
      (begin-edit-sequence)
      (let ([ok? (or (can-delete? del-snip)
                     (begin
                       (end-edit-sequence)
                       (set! write-locked (sub1 write-locked))
                       #f))])
        (and 
         ok?
         (begin
           (on-delete del-snip)
           (set! write-locked (sub1 write-locked))
           
           (let ([update-cursor?
                  (and (and s-caret-snip
                            (object=? del-snip s-caret-snip))
                       (begin
                         (send s-caret-snip own-caret #f)
                         (set! s-caret-snip #f)
                         #t))])

             (update-snip del-snip)
             
             (if (snip->prev del-snip)
                 (set-snip-next! (snip->prev del-snip) (snip->next del-snip))
                 (set! snips (snip->next del-snip)))
             (if (snip->next del-snip)
                 (set-snip-prev! (snip->next del-snip) (snip->prev del-snip))
                 (set! last-snip (snip->prev del-snip)))

             (let ([loc (snip->loc del-snip)])
               (set-snip-loc! del-snip #f)
               (when del
                 (send del insert-snip del-snip (snip->next del-snip) (loc-x loc) (loc-y loc))))
             
             (set-snip-next! del-snip #f)
             (set-snip-prev! del-snip #f)
             
             (set-snip-flags! del-snip (add-flag (snip->flags del-snip) CAN-DISOWN))
             (snip-set-admin del-snip #f)
             (set-snip-flags! del-snip (remove-flag (snip->flags del-snip) CAN-DISOWN))
             (unless del
               (unless (send del-snip get-admin)
                 (set-snip-flags! del-snip (remove-flag (snip->flags del-snip) OWNED))))
             
             (unless s-modified?
               (set-modified #t))

             (after-delete del-snip)
             (set! changed? #t)

             (set! need-resize? #t)

             (set! write-locked (add1 write-locked))
             (end-edit-sequence)
             (set! write-locked (sub1 write-locked))

             (when (zero? sequence)
               (update-needed))
             
             (when update-cursor?
               (when s-admin
                 (send s-admin update-cursor)))

             #t))))))

  (def/public (remove [snip% del-snip])
    (unless (or s-user-locked?
                (not (zero? write-locked)))
      (-delete del-snip #f)))

  ;; ----------------------------------------


  (def/public (move-to [snip% snip] [real? x] [real? y])
    (unless (or s-user-locked?
                (not (zero? write-locked)))
      (let ([loc (snip->loc snip)])
        (when (and loc
                   (not (and
                         (= (loc-x loc) x)
                         (= (loc-y loc) y))))
          (set! write-locked (add1 write-locked))
          (begin-edit-sequence)
          (if (not (can-move-to? snip x y dragging?))
              (begin
                (end-edit-sequence)
                (set! write-locked (sub1 write-locked)))
              (begin
                (on-move-to snip x y dragging?)
                (set! write-locked (sub1 write-locked))

                (update-location loc)

                (unless dragging?
                  (let ([rec (make-object move-snip-record%
                                          snip
                                          (loc-x loc)
                                          (loc-y loc) 
                                          #f
                                          sequence-streak?)])
                    (when (positive? sequence)
                      (set! sequence-streak? #t))
                    (when (zero? s-noundomode)
                      (add-undo-rec rec))))
                
                (set-loc-x! loc x)
                (set-loc-y! loc y)
                (set-loc-r! loc (+ x (loc-w loc)))
                (set-loc-b! loc (+ y (loc-h loc)))
                (set-loc-hm! loc (+ x (/ (loc-w loc) 2)))
                (set-loc-vm! loc (+ y (/ (loc-h loc) 2)))
                (update-location loc)

                (when (and (not dragging?)
                           (not s-modified?))
                  (set-modified #t))

                (after-move-to snip x y dragging?)

                (set! need-resize? #t)

                (set! write-locked (add1 write-locked))
                (end-edit-sequence)
                (set! write-locked (sub1 write-locked))

                (set! changed? #t)

                (when (zero? sequence)
                  (update-needed))))))))
  
  (define/public (move . args)
    (case-args
     args
     [([snip% snip] [real? dx] [real? dy])
      (unless (or s-user-locked?
                  (not (zero? write-locked)))
        (let ([loc (snip->loc snip)])
          (when loc
            (move-to snip (+ (loc-x loc) dx) (+ (loc-y loc) dy)))))]
     [([real? dx] [real? dy])
      (unless (or s-user-locked?
                  (not (zero? write-locked)))
        (begin-edit-sequence)
        (for ([loc (in-snip-locs)])
          (when (loc-selected? loc)
            (move (loc-snip loc) dx dy)))
        (end-edit-sequence))]
     (method-name 'pasteboard% 'move)))

  (def/public (resize [snip% snip] [real? w] [real? h])
    (if (not s-admin)
        #f
        (let ([loc (snip->loc snip)])
          (if (not loc)
              #f
              (let ([oldw (loc-w loc)]
                    [oldh (loc-h loc)])
                (set! write-locked (add1 write-locked))
                (begin-edit-sequence)
                (if (not (can-resize? snip w h))
                    (begin
                      (end-edit-sequence)
                      (set! write-locked (sub1 write-locked))
                      #f)
                    (begin
                      (on-resize snip w h)
                      (set! write-locked (sub1 write-locked))

                      (update-location loc)

                      (let ([rv? 
                             (and (send snip resize w h)
                                  (begin
                                    (when (not dragging?)
                                      (when (zero? s-noundomode)
                                        (let ([rs (make-object resize-snip-record%
                                                               snip oldw oldh
                                                               sequence-streak?)])
                                          (add-undo-rec rs))
                                        (when (positive? sequence)
                                          (set! sequence-streak? #t))))
                                    #t))])
                        (when (and rv?
                                   (not dragging?)
                                   (not s-modified?))
                          (set-modified #t))

                        (after-resize snip w h rv?)

                        (update-location loc)

                        (set! write-locked (add1 write-locked))
                        (end-edit-sequence)
                        (set! write-locked (sub1 write-locked))

                        (set! changed? #t)

                        (when (zero? sequence)
                          (update-needed))

                        rv?))))))))

  ;; ----------------------------------------

  (define/private (do-change-style style delta snip)
    (unless (or s-user-locked?
                  (not (zero? write-locked)))
      (let ([rec (make-object style-change-snip-record% sequence-streak?)])
        (when (positive? sequence)
          (set! sequence-streak? #t))

        (let ([style (or style
                         (and (not delta)
                              (or (get-default-style)
                                  (send s-style-list basic-style))))])

          (begin-edit-sequence)

          (let ([didit?
                 (if snip
                     (begin
                       (send rec add-style-change snip (snip->style snip))
                       (set-snip-style!
                        snip
                        (or style
                            (send s-style-list find-or-create-style (snip->style snip) delta)))
                       (send snip size-cache-invalid)
                       (update-snip snip)
                       #t)
                     (for/fold ([didit? #f]) ([loc (in-snip-locs)])
                       (if (loc-selected? loc)
                           (let ([snip (loc-snip loc)])
                             (send rec add-style-change (loc-snip loc) (snip->style snip))
                             (set-snip-style!
                              snip
                              (or style
                                  (send s-style-list find-or-create-style (snip->style snip) delta)))
                             (send snip size-cache-invalid)
                             (set-loc-need-resize?! loc #t)
                             (set! need-resize? #t)
                             (update-location loc)
                             #t)
                           didit?)))])

            (when didit?
              (when (zero? s-noundomode)
                (add-undo-rec rec))

              (set! changed? #t)
              (when (not s-modified?)
                (set-modified #t))))
          
          (end-edit-sequence)))))

  (define/public (change-style . args)
    (case-args
     args
     [()                                  (do-change-style #f    #f    #f)]
     [([not delta])                       (do-change-style #f    #f    #f)]
     [([style-delta% delta])              (do-change-style #f    delta #f)]
     [([style-delta% delta] [snip% snip]) (do-change-style #f    delta snip)]
     [([style<%> style]     [snip% snip]) (do-change-style style #f    snip)]
     [([style-delta% delta] [not snip])   (do-change-style #f    delta #f)]
     [([style<%> style]     [not snip])   (do-change-style style #f    #f)]
     [([not style]          [snip% snip]) (do-change-style style #f    snip)]
     [([not style]          [not snip])   (do-change-style #f    #f    snip)]
     (method-name 'pasteboard% 'change-style)))

  ;; ----------------------------------------

  (define/private (set-between snip before after)
    (unless (or s-user-locked?
                (not (zero? write-locked))
                (not (snip->loc snip))
                (and before (object=? snip before))
                (and after (object=? snip after))
                (and before (not (snip->loc before)))
                (and after (not (snip->loc after))))
      (set! write-locked (add1 write-locked))
      (if (not (can-reorder? snip (or before after) (and before #t)))
          (set! write-locked (sub1 write-locked))
          (begin
            (on-reorder snip (or before after) (and before #t))
            (set! write-locked (sub1 write-locked))

            ;; remove snip from current pos:
            (if (snip->prev snip)
                (set-snip-next! (snip->prev snip) (snip->next snip))
                (set! snips (snip->next snip)))
            (if (snip->next snip)
                (set-snip-prev! (snip->next snip) (snip->prev snip))
                (set! last-snip (snip->prev snip)))

            ;; insert before `before' or after `after':
            (if before
                (begin
                  (set-snip-prev! snip (snip->prev before))
                  (set-snip-next! snip before)
                  (set-snip-prev! before snip)
                  (if (snip->prev snip)
                      (set-snip-next! (snip->prev snip) snip)
                      (set! snips snip)))
                (begin
                  (set-snip-next! snip (snip->next after))
                  (set-snip-prev! snip after)
                  (set-snip-next! after snip)
                  (if (snip->next snip)
                      (set-snip-prev! (snip->next snip) snip)
                      (set! last-snip snip))))

            (set! changed? #t)
            (unless s-modified?
              (set-modified #t))

            (update-snip snip)

            (after-reorder snip (or before after) (and before #t))))))

  (def/public (set-before [snip% snip] [(make-or-false snip%) before])
    (set-between snip (or before snips) #f))

  (def/public (set-after [snip% snip] [(make-or-false snip%) after])
    (set-between snip #f (or after last-snip)))

  (def/public (raise [snip% snip])
    (set-between snip (snip->prev snip) #f))

  (def/public (lower [snip% snip])
    (set-between snip #f (snip->next snip)))

  ;; ----------------------------------------

  (define/private (snip-set-admin snip a)
    (let ([orig-admin (snip->admin snip)])
      ;; lock during set-admin! [???]
      (send snip set-admin a)

      (if (not (eq? (send snip get-admin) a))
          ;; something went wrong
          (cond
           [(and (not a)
                 (eq? (snip->admin snip) orig-admin))
            ;; force admin to null
            (set-snip-admin! snip #f)
            snip]
           [a
            ;; snip didn't accept membership into this editor; give up on it
            (let ([naya (new snip%)])
              (set-snip-prev! naya (snip->prev snip))
              (set-snip-next! naya (snip->next snip))
              (if (snip->prev snip)
                  (set-snip-next! (snip->prev naya) naya)
                  (set! snips naya))
              (if (snip->next snip)
                  (set-snip-prev! (snip->next naya) naya)
                  (set! last-snip naya))
              (set-snip-admin! snip #f)
              (send naya set-admin a)
              naya)]
           [else snip])
          snip)))
  
  ;; ----------------------------------------
  
  (define/override (really-can-edit? op)
    (if (and (not (eq? op 'copy))
             (positive? write-locked))
        #f
        (case op
          [(clear cut copy kill)
           (and (find-next-selected-snip #f)
                #t)]
          [(select-all)
           (and snips #t)]
          [else #t])))
  
  ;; ----------------------------------------
  
  (define/private (find-dot loc x y dxm dym)
    (define (check-y can-mid?)
      (cond
       [(inbox? (loc-y loc) y)
        (set-box! dym -1) #t]
       [(and can-mid? (inbox? (loc-vm loc) y))
        (set-box! dym 0) #t]
       [(inbox? (loc-b loc) y)
        (set-box! dym 1) #t]
       [else #f]))
    (cond
     [(inbox? (loc-x loc) x)
      (set-box! dxm -1)
      (check-y #t)]
     [(inbox? (loc-hm loc) x)
      (set-box! dxm 0)
      (check-y #f)]
     [(inbox? (loc-r loc) x)
      (set-box! dxm 1)
      (check-y #t)]
     [else #f]))

  (def/public (find-snip [real? x] [real? y] [(make-or-false snip%) [after #f]])
    (let ([dummy (box 0)])
      (let loop ([s (if after 
                        (if (snip->loc after)
                            (snip->next after)
                            #f)
                        snips)])
        (and s
             (let ([loc (snip->loc s)])
               (cond
                [(and ((loc-x loc) . <= . x)
                      ((loc-y loc) . <= . y)
                      ((loc-r loc) . >= . x)
                      ((loc-b loc) . >= . y))
                 s]
                [(and (loc-selected? loc)
                      (find-dot loc x y dummy dummy))
                 s]
                [else (loop (snip->next s))]))))))
  
  (def/override (find-first-snip) snips)
  
  (def/public (is-selected? [snip% asnip])
    (let ([loc (snip->loc asnip)])
      (and loc
           (loc-selected? loc))))
  
  (def/public (find-next-selected-snip [(make-or-false snip%) start])
    (let loop ([s (if start 
                      (if (snip->loc start)
                          (snip->next start)
                          #f)
                      snips)])
      (and s
           (if (loc-selected? (snip->loc s))
               s
               (loop (snip->next s))))))

  ;; ----------------------------------------
  
  (define/private (draw dc dx dy cx cy cw ch show-caret bg-color)
    (when s-admin
      (set! write-locked (add1 write-locked))
      (set! flow-locked? #t)

      (let ([dcx (+ cx dx)]
            [dcy (+ cy dy)]
            [cr (+ cx cw)]
            [cb (+ cy ch)])
        (let ([dcr (+ dcx cw)]
              [dcb (+ dcy ch)])

          (when bg-color
            (let ([save-pen (send dc get-pen)]
                  [save-brush (send dc get-brush)])
              
              (let ([wb (if (and (= 255 (send bg-color red))
                                 (= 255 (send bg-color green))
                                 (= 255 (send bg-color blue)))
                            white-brush
                            (send the-brush-list find-or-create-brush bg-color 'solid))])
                (send dc set-brush wb)
                (send dc set-pen invisi-pen)
                (send dc draw-rectangle dcx dcy cw ch)
                (send dc set-brush save-brush)
                (send dc set-pen save-pen))))

          (on-paint #t dc cx cy cr cb dx dy 
                    (if (not s-caret-snip)
                        show-caret
                        'no-caret))

          (let loop ([snip last-snip]
                     [old-style #f])
            (if snip
                (let ([loc (snip->loc snip)])
                  (when (and ((loc-x loc) . <= . cr)
                             ((loc-y loc) . <= . cb)
                             ((loc-r loc) . >= . cx)
                             ((loc-b loc) . >= . cy))
                    (send (snip->style snip) switch-to dc old-style)
                    (let ([old-style (snip->style snip)])
                      (let ([x (+ (loc-x loc) dx)]
                            [y (+ (loc-y loc) dy)])
                        
                        (send snip draw
                              dc x y dcx dcy dcr dcb dx dy 
                              (if (eq? snip s-caret-snip)
                                  show-caret
                                  'no-caret))

                        (when (and (eq? show-caret 'show-caret)
                                   s-own-caret?
                                   selection-visible?
                                   (loc-selected? loc))
                          (let ([oldbrush (send dc get-brush)]
                                [oldpen (send dc get-pen)])
                            (send dc set-brush black-brush)
                            (send dc set-pen invisi-pen)

                            (let ([r (+ (loc-r loc) dx)]
                                  [b (+ (loc-b loc) dy)]
                                  [hm (+ (loc-hm loc) dx)]
                                  [vm (+ (loc-vm loc) dy)]
                                  [rect
                                   (lambda (x y)
                                     (send dc draw-rectangle 
                                           (- x HALF-DOT-WIDTH) (- y HALF-DOT-WIDTH)
                                           DOT-WIDTH DOT-WIDTH))])
                              (rect x y)
                              (rect hm y)
                              (rect r y)
                              (rect r vm)
                              (rect r b)
                              (rect hm b)
                              (rect x b)
                              (rect x vm))

                            (send dc set-pen oldpen)
                            (send dc set-brush oldbrush))))))

                  (loop (snip->prev snip) old-style))
                (let ([bs (send s-style-list basic-style)])
                  (send bs switch-to dc old-style))))

          (on-paint #f dc cx cy cr cb dx dy 
                    (if (not s-caret-snip)
                        show-caret
                        'no-caret))

          (when rubberband?
            (let ([a (send dc get-alpha)])
              (send dc set-alpha (* a 0.5))
              (send dc set-brush rb-brush)
              (send dc set-pen invisi-pen)
              (send dc draw-rectangle (+ rb-x dx) (+ rb-y dy) rb-w rb-h)
              (send dc set-pen rb-pen)
              (send dc set-alpha a)
              (send dc set-brush invisi-brush)
              (send dc draw-rectangle (+ rb-x dx) (+ rb-y dy) rb-w rb-h)))

          (set! flow-locked? #f)
          (set! write-locked (sub1 write-locked))))))

  ;; called by the administrator to trigger a redraw
  (def/override (refresh [real? left] [real? top] [nonnegative-real? width] [nonnegative-real? height]
                         [caret-status? show-caret]
                         [(make-or-false color%) bg-color])

    (cond
     [(not s-admin) (void)]
     [(or (width . <= . 0) (height . <= . 0)) (void)]
     [(or flow-locked? (positive? sequence))
      ;; we're busy. invalidate so that everything is refreshed later.
      (update left top width height)]
     [else
      (let-boxes ([x 0.0]
                  [y 0.0]
                  [dc #f])
          (set-box! dc (send s-admin get-dc x y))
        (when dc
          (begin-sequence-lock)
          
          (send s-offscreen ready-offscreen width height)

          ;; make sure all location information is integral,
          ;; so we can shift the coordinate system and generally
          ;; update on pixel boundaries
          (let ([x (->long (floor x))]
                [y (->long (floor y))]
                [bottom (->long (ceiling (+ top height)))]
                [right (->long (ceiling (+ left width)))]
                [top (->long (floor top))]
                [left (->long (floor left))])
            (let ([width (- right left)]
                  [height (- bottom top)]
                  [ps? (or (dc . is-a? . post-script-dc%)
                           (dc . is-a? . printer-dc%))])

              (if (and bg-color
                       (not (send s-offscreen is-in-use?))
                       (send s-offscreen get-bitmap)
                       (send (send s-offscreen get-bitmap) ok?)
                       (send (send s-offscreen get-dc) ok?)
                       (not ps?))
                  ;; draw to offscreen
                  (begin
                    (send s-offscreen set-in-use #t)
                    (draw (send s-offscreen get-dc) (- left) (- top) left top width height show-caret bg-color)
                    
                    (send dc draw-bitmap-section 
                          (send (send s-offscreen get-dc) get-bitmap) 
                          (- left x) (- top y) 
                          0 0 width height 'solid)
                    
                    (send s-offscreen set-last-used #f)
                    (send s-offscreen set-in-use #f))
                  ;; draw directly
                  (let ([pen (send dc get-pen)]
                        [brush (send dc get-brush)]
                        [font (send dc get-font)]
                        [fg (send dc get-text-foreground)]
                        [bg (send dc get-text-background)]
                        [bgmode (send dc get-text-mode)]
                        [rgn (send dc get-clipping-region)])

                    (send dc suspend-flush)

                    (send dc set-clipping-rect (- left x) (- top y) width height)
                    
                    (dynamic-wind
                        void
                        (lambda ()
                          (draw dc (- x) (- y) left top width height show-caret bg-color))
                        (lambda ()
                          (send dc set-clipping-region rgn)
                          
                          (send dc set-brush brush)
                          (send dc set-pen pen)
                          (send dc set-font font)
                          (send dc set-text-foreground fg)
                          (send dc set-text-background bg)
                          (send dc set-text-mode bgmode)
                          
                          (send dc resume-flush)))))))

          (end-sequence-lock)))]))
  ;; ----------------------------------------

  (define/private (loc-resize loc dc)
    (let-boxes ([ww 0.0]
                [hh 0.0])
        (send (loc-snip loc) get-extent dc (loc-x loc) (loc-y loc) ww hh #f #f #f #f)
      (set-loc-w! loc ww)
      (set-loc-h! loc hh)
      (set-loc-r! loc (+ (loc-x loc) ww))
      (set-loc-b! loc (+ (loc-y loc) hh))
      (set-loc-hm! loc (+ (loc-x loc) (/ ww 2)))
      (set-loc-vm! loc (+ (loc-y loc) (/ hh 2)))
      (set-loc-need-resize?! loc #f)))
  
  (define/private (check-recalc)
    (when s-admin
      (let ([dc (send s-admin get-dc)])
        (when dc
          (when need-resize?
            (let-values ([(r b)
                          (for/fold ([r 0.0]
                                     [b 0.0])
                              ([loc (in-snip-locs)])
                            (when size-cache-invalid?
                              (send (loc-snip loc) size-cache-invalid)
                              (set-loc-need-resize?! loc #t))
                            (when (loc-need-resize? loc)
                              (loc-resize loc dc))
                            (values (max r (+ (loc-r loc) HALF-DOT-WIDTH))
                                    (max b (+ (loc-b loc) HALF-DOT-WIDTH))))])

              (set! real-width (max (min r (if (symbol? max-width) +inf.0 max-width))
                                    (if (symbol? min-width) -inf.0 min-width)))
              (set! real-height (max (min b (if (symbol? max-height) +inf.0 max-height))
                                     (if (symbol? min-height) -inf.0 min-height)))

              (set! need-resize? #f)))

          (set! size-cache-invalid? #f)

          (when (not keep-size?)
            (when (or (not (= real-width total-width))
                      (not (= real-height total-height)))
              (set! total-width real-width)
              (set! total-height real-height)
              (send s-admin resized #f)))))))

  (define/private (update x y w h)
    (unless (and delayedscrollsnip
                 (zero? sequence)
                 (not flow-locked?)
                 (let ([s delayedscrollsnip])
                   (set! delayedscrollsnip #f)
                   (scroll-to s
                              delayedscroll-x delayedscroll-y
                              delayedscroll-w delayedscroll-h
                              #t delayedscrollbias)))
      (let ([r (if (symbol? w) x (+ x w))]
            [b (if (symbol? h) y (+ y h))])
        (let ([x (max x 0.0)]
              [y (max y 0.0)]
              [r (max r 0.0)]
              [b (max b 0.0)])
          
          (set! no-implicit-update? #f)

          (if (not update-nonempty?)
              (begin
                (set! update-top y)
                (set! update-left x)
                (set! update-bottom b)
                (set! update-bottom-end (and (symbol? h) h))
                (set! update-right r)
                (set! update-right-end (and (symbol? w) w))
                (set! update-nonempty? #t))
              (begin
                (set! update-top (min y update-top))
                (set! update-left (min x update-left))
                (set! update-bottom (max b update-bottom))
                (when (symbol? h)
                  (if (eq? h 'display-end)
                      (set! update-bottom-end 'display-end)
                      (unless (eq? update-bottom-end 'display-end)
                        (set! update-bottom-end 'end))))
                (set! update-right (max r update-right))
                (when (symbol? w)
                  (if (eq? w 'display-end)
                      (set! update-right-end 'display-end)
                      (unless (eq? update-right-end 'display-end)
                        (set! update-right-end 'end))))))

          (unless (or (positive? sequence)
                      (not s-admin)
                      flow-locked?)
            (check-recalc)

            (let-boxes ([vx 0.0] [vy 0.0] [vw 0.0] [vh 0.0])
                (when (or (eq? update-bottom-end 'display-end)
                          (eq? update-right-end 'display-end))
                  (send s-admin get-max-view x y w h))
              (case update-bottom-end
                [(end) (set! update-bottom (max update-bottom real-height))]
                [(display-end) (set! update-bottom (max update-bottom vh))])
              (case update-right-end
                [(end) (set! update-right (max update-right real-width))]
                [(display-end) (set! update-right (max update-right vw))]))

            (set! update-nonempty? #f)

            (when changed?
              (set! changed? #f)
              (set! write-locked (add1 write-locked))
              (on-change)
              (set! write-locked (sub1 write-locked)))

            (when (or (not (= update-top update-bottom))
                      (not (= update-left update-right)))
              (let ([w (+ (- update-right update-left) 1)]
                    [h (+ (- update-bottom update-top) 1)])
                (when (and (w . > . 0) (h . > . 0))
                  (send s-admin needs-update update-left update-top w h)))))))))

  
  (define/private (update-location loc)
    (when s-admin
      (when (loc-need-resize? loc)
        (let ([dc (send s-admin get-dc)])
          (when dc
            (loc-resize loc dc))
          ;; otherwise, still need resize...
          ))
      (update (- (loc-x loc) HALF-DOT-WIDTH)
              (- (loc-y loc) HALF-DOT-WIDTH)
              (+ (loc-w loc) DOT-WIDTH)
              (+ (loc-h loc) DOT-WIDTH))))
  
  (define/private (update-snip snip)
    (let ([loc (snip->loc snip)])
      (when loc
        (update-location loc))))

  (define/private (update-selected)
    (begin-edit-sequence)
    (for ([loc (in-snip-locs)])
      (when (loc-selected? loc)
	(update-location loc)))
    (end-edit-sequence))

  (define/private (update-all)
    (update 0.0 0.0 -1.0 -1.0))

  (define/private (update-needed)
    (when (or (and update-nonempty?
                   (not no-implicit-update?))
              delayedscrollsnip)
      (update update-left update-top 0 0)))

  (def/override (invalidate-bitmap-cache [real? [x 0.0]] 
                                         [real? [y 0.0]]
                                         [(make-alts nonnegative-real? (symbol-in end display-end)) [w 'end]]
                                         [(make-alts nonnegative-real? (symbol-in end display-end)) [h 'end]])
    (update x y w h))

  ;; ----------------------------------------

  (def/override (own-caret [any? ownit?])
    (when (do-own-caret ownit?)
      (update-selected)
      (on-focus ownit?)))

  (def/override (blink-caret)
    (when s-caret-snip
      (let-boxes ([dc #f]
                  [dx 0.0]
                  [dy 0.0])
          (set-box! dc (send s-admin get-dc dx dy))
        (when dc
          (let-boxes ([x 0.0]
                      [y 0.0]
                      [ok? #f])
              (set-box! ok? (get-snip-location s-caret-snip y))
            (when ok?
              (send s-caret-snip blink-caret dc (- x dx) (- y dy))))))))

  (def/override (size-cache-invalid)
    (set! size-cache-invalid? #t)
    (set! need-resize? #t))

  (def/override (get-extent [maybe-box? w] [maybe-box? h])
    (check-recalc)
    (when w (set-box! w total-width))
    (when h (set-box! h total-height)))

  ;; ----------------------------------------

  (def/public (scroll-to [snip% snip] [real? localx] [real? localy]
                         [nonnegative-real? w] [nonnegative-real? h]
                         [any? refresh?]
                         [(symbol-in start end none) [bias 'none]])
    (cond
     [(positive? sequence)
      (set! delayedscrollsnip snip)
      (set! delayedscroll-x localx)
      (set! delayedscroll-y localy)
      (set! delayedscroll-w w)
      (set! delayedscroll-h h)
      #f]
     [s-admin
      (let-boxes ([x 0.0]
                  [y 0.0])
          (get-snip-location snip x y)
        (if (scroll-editor-to (+ x localx) (+ y localy) w h refresh? bias)
            (begin
              (set! update-top 0.0)
              (set! update-left 0.0)
              (set! update-bottom -1.0)
              (set! update-right -1.0)
              (set! update-nonempty? #t)
              #t)
            #f))]
     [else #f]))
  
  (def/override (set-caret-owner [(make-or-false snip%) snip] 
                                 [(symbol-in immediate display global) [dist 'immediate]])
    (when (do-set-caret-owner snip dist)
      (update-all)
      (on-focus (not snip))))

  (def/override (resized [snip% snip] [any? redraw-now?])
    (let ([loc (snip->loc snip)])
      (when (and loc
                 (not (loc-need-resize? loc)))
        (set! changed? #t)

        (let ([niu? (or (not update-nonempty?)
                        no-implicit-update?)])
          
          (when (not redraw-now?)
            (set! sequence (add1 sequence)))
          (begin-edit-sequence)
          
          (update-location loc)

          (set-loc-need-resize?! loc #t)
          (set! need-resize? #t)

          (update-location loc)

          (end-edit-sequence)
          (when (not redraw-now?)
            (set! sequence (sub1 sequence)))
          (when niu?
            (set! no-implicit-update? #t))))))
  
  (def/override (recounted [snip% snip] [any? redraw-now?])
    (resized snip redraw-now?)
    #t)

  (def/override (needs-update [snip% snip]
                              [real? localx] [real? localy]
                              [nonnegative-real? w] [nonnegative-real? h])
    (let-boxes ([x 0.0]
                [y 0.0])
        (get-snip-location snip x y)
      (update (+ x localx) (+ y localy) w h)))
  
  (def/override (release-snip [snip% snip])
    (if (-delete snip #f) 
        (begin
          (when (and (not (snip->admin snip))
                     (has-flag? (snip->flags snip) OWNED))
            (set-snip-flags! snip (remove-flag (snip->flags snip) OWNED)))
          #t)
        #f))

  ;; ----------------------------------------

  (def/override (scroll-line-location [exact-integer? line])
    (* line scroll-step))

  (def/override (num-scroll-lines)
    (->long (/ (- (+ total-height scroll-step) 1) scroll-step)))
  
  (def/override (find-scroll-line [real? y])
    (let ([y (max 0 y)])
      (->long (/ y scroll-step))))

  (def/public (set-scroll-step [real? s])
    (unless (= scroll-step s)
      (set! scroll-step s)
      (when s-admin
        (send s-admin resized #t))))

  (def/public (get-scroll-step)
    scroll-step)

  ;; ----------------------------------------
  
  (def/override (set-min-width [(make-alts real? (symbol-in none)) w])
    (set! min-width (if (and (real? w) (w . <= . 0)) 'none w))
    (set! need-resize? #t)
    (update-all))
  
  (def/override (set-max-width [(make-alts real? (symbol-in none)) w])
    (set! max-width (if (and (real? w) (w . <= . 0)) 'none w))
    (set! need-resize? #t)
    (update-all))
  
  (def/override (set-min-height [(make-alts real? (symbol-in none)) h])
    (set! min-height (if (and (real? h) (h . <= . 0)) 'none h))
    (set! need-resize? #t)
    (update-all))
  
  (def/override (set-max-height [(make-alts real? (symbol-in none)) h])
    (set! max-height (if (and (real? h) (h . <= . 0)) 'none h))
    (set! need-resize? #t)
    (update-all))
  
  (def/override (get-min-width) min-width)
  (def/override (get-max-width) max-width)
  (def/override (get-min-height) min-height)
  (def/override (get-max-height) max-height)

  ;; ----------------------------------------

  (def/override (copy-self)
    (let ([pb (new pasteboard%)])
      (copy-self-to pb)
      pb))
  
  (def/override (copy-self-to [editor<%> pb])
    (when (pb . is-a? . pasteboard%)
      (super copy-self-to pb)
      (send pb set-dragable (get-dragable))
      (send pb set-selection-visible (get-selection-visible))
      (send pb set-scroll-step (get-scroll-step))))
  
  ;; ----------------------------------------

  (def/override (get-descent) 0.0)
  (def/override (get-space) 0.0)

  (def/public (get-center)
    (let-boxes ([x 0.0]
                [y 0.0]
                [w 0.0]
                [h 0.0])
        (if (not s-admin)
            (begin
              (set-box! w total-width)
              (set-box! h total-height))
            (send s-admin get-view x y w h #t))
      (let ([w (if (w . > . 1000.0)
                   500.0 ; don't believe it
                   w)]
            [h (if (h . > . 1000.0)
                   500.0 ; don't believe it
                   h)])
        (values (/ w 2)
                (/ h 2)))))

  ;; ----------------------------------------

  (def/override (get-flattened-text)
    (let ([p (open-output-string)])
      (let loop ([s snips])
        (when s
          (display (send s get-text 0 (snip->count s) #t) p)
          (loop (snip->next s))))
      (get-output-string p)))

  (def/override (clear) (delete))

  (def/override (cut [any? [extend? #f]] [exact-integer? [time 0]])
    (copy extend? time)
    (clear))

  (def/public (do-copy [exact-integer? time] [bool? extend?])
    (set-common-copy-region-data! #f)
    (let ([sl (if (and extend?
                       copy-style-list)
                  copy-style-list
                  s-style-list)])
      (let loop ([snip snips])
        (when snip
          (let ([loc (snip->loc snip)])
            (when (loc-selected? loc)
              (let ([asnip (send snip copy)])
                (send asnip set-admin #f)
                (set-snip-style! asnip (send sl convert (snip->style asnip)))
                (cons-common-copy-buffer! asnip)
                (cons-common-copy-buffer2! (get-snip-data snip)))))
          (loop (snip->next snip))))
      (install-copy-buffer time sl)))
  
  (def/override (copy [bool? [extend? #f]] [exact-integer? [time 0]])
    (begin-copy-buffer)
    (when (not extend?)
      (free-old-copies))
    (do-copy time extend?)
    (end-copy-buffer))

  (define/private (do-generic-paste cb time)
    (unless (or s-user-locked?
                (positive? write-locked))
      (let-values ([(start) snips]
                   [(cx cy) (get-center)])

        (do-buffer-paste cb time #f)
        
        (if (and s-admin
                 (not (eq? snips start)))
            (let ([dc (get-dc)])
              (when dc
                ;; get top/left/bottom/right of pasted group:
                (let loop ([snip snips]
                           [left +inf.0]
                           [top +inf.0]
                           [right -inf.0]
                           [bottom -inf.0])
                  (if (eq? snip start)
                      (let ([dx (- cx (/ (+ left right) 2))]
                            [dy (- cy (/ (+ top bottom) 2))])
                        ;; shift the pasted group to center:
                        (move dx dy))
                      (let ([loc (snip->loc snip)])
                        (add-selected snip)
                        (when (loc-need-resize? loc)
                          (loc-resize loc dc))
                        (loop (snip->next snip)
                              (min (loc-x loc) left)
                              (min (loc-y loc) top)
                              (max (loc-r loc) right)
                              (max (loc-b loc) bottom)))))))
            ;; just select them:
            (let loop ([snip snips])
              (unless (eq? snip start)
                (add-selected snip)
                (loop (snip->next snip))))))))

  (def/public (do-paste [exact-integer? time])
    (do-generic-paste the-clipboard time))

  (def/public (do-paste-x-selection [exact-integer? time])
    (do-generic-paste the-x-selection-clipboard time))
  
  (define/private (generic-paste x-sel? time)
    (unless (or s-user-locked?
                (positive? write-locked))
      (begin-edit-sequence)
      (no-selected)
      (if x-sel?
          (do-paste-x-selection time)
          (do-paste time))
      (end-edit-sequence)))
  
  (def/override (paste [exact-integer? [time 0]])
    (generic-paste #f time))

  (def/override (paste-x-selection [exact-integer? [time 0]])
    (generic-paste #t time))

  (define/override (insert-paste-snip snip data)
    (insert snip snip)
    (set-snip-data snip data))

  (define/override (insert-paste-string str)
    (let ([snip (new string-snip%)])
      (set-snip-style! snip (or (get-default-style)
                                (send s-style-list basic-style)))
      (send snip insert str)
      (insert-paste-snip snip #f)))
  
  (def/override (kill [exact-integer? [time 0]])
    (cut time))

  (define/override (own-x-selection on? update? force?)
    (do-own-x-selection on? force?))
  
  ;; ----------------------------------------

  (def/override (get-snip-location [snip% thesnip]
                                   [maybe-box? [x #f]]
                                   [maybe-box? [y #f]]
                                   [bool? [bottom-right? #f]])
    (if (and bottom-right?
             (not s-admin))
        #f
        (begin
          (when bottom-right?
            (check-recalc))

          (let ([loc (snip->loc thesnip)])
            (and loc
                 (begin
                   (when x (set-box! x (+ (loc-x loc)
                                          (if bottom-right?
                                              (loc-w loc)
                                              0.0))))
                   (when y (set-box! y (+ (loc-y loc)
                                          (if bottom-right?
                                              (loc-h loc)
                                              0.0))))
                   #t))))))

  ;; ----------------------------------------

  (def/override (get-snip-data [snip% snip])
    (let ([loc (snip->loc snip)]
          [sup (super get-snip-data snip)])
      (if (not loc)
          sup
          (let ([data (new location-editor-data%
                           [x (loc-x loc)]
                           [y (loc-y loc)])])
            (send data set-next sup)
            data))))

  (def/override (set-snip-data [snip% snip] [(make-or-false editor-data%) data])
    (let loop ([data data])
      (when data
        (let ([c (send data get-dataclass)])
          (when c
            (let ([name (send c get-classname)])
              (when (equal? name "wxloc")
                (move-to snip (send data get-x) (send data get-y))))))
        (loop (send data get-next)))))

  (def/override (insert-port [input-port? f] 
                             [(symbol-in guess same copy standard text text-force-cr) [format 'guess]]
                             [any? [replace-styles? #f]])
    (if (or s-user-locked? 
            (not (zero? write-locked)))
        'standard
        (do-insert-file (method-name 'pasteboard% 'insert-file) f replace-styles?)))
  
  (define/private (do-insert-file who f clear-styles?)
    (when (not (detect-wxme-file who f #f))
      (error who "not a WXME file"))
    (let* ([b (make-object editor-stream-in-file-base% f)]
           [mf (make-object editor-stream-in% b)])
      (when (not (and (read-editor-version mf b #f #t)
                      (read-editor-global-header mf)
                      (send mf ok?)
                      (read-from-file mf clear-styles?)
                      (read-editor-global-footer mf)
                      (begin
                        ;; if STD-STYLE wasn't loaded, re-create it:
                        (send s-style-list new-named-style "Standard" (send s-style-list basic-style))
                        (send mf ok?))))
        (error who "error loading the file")))
    'standard)

  (def/override (save-port [output-port? f]
                           [(symbol-in guess same copy standard text text-force-cr) [format 'same]]
                           [any? [show-errors? #t]])

    (let* ([b (make-object editor-stream-out-file-base% f)]
           [mf (make-object editor-stream-out% b)])
      (when (not (and (write-editor-version mf b)
                      (write-editor-global-header mf)
                      (send mf ok?)
                      (write-to-file mf)
                      (write-editor-global-footer mf)
                      (send mf ok?)))
        (error (method-name 'pasteboard% 'save-port) "error writing the file"))
      #t))

  (def/override (write-to-file [editor-stream-out% f])
    (and (do-write-headers-footers f #t)
         (write-snips-to-file f s-style-list #f snips #f #f this)
         (do-write-headers-footers f #f)))

  (def/override (read-from-file [editor-stream-in% f]
                                [bool? [overwritestyle? #f]])
    (if (or s-user-locked? 
            (not (zero? write-locked)))
        #f
        (read-snips-from-file f overwritestyle?)))

  (define/override (do-read-insert snip)
    (insert snip #f)
    #t)

  (def/override (set-filename [(make-or-false path-string?) name][any? [temp? #f]])
    (set! s-filename (if (string? name)
                         (string->path name)
                         name))
    (set! s-temp-filename? temp?)
    (let loop ([snip snips])
      (when snip
        (when (has-flag? (snip->flags snip) USES-BUFFER-PATH)
          ;; just a notification
          (send snip set-admin snip-admin))
        (loop (snip->next snip)))))

  ;; ----------------------------------------
  
  (def/override (style-has-changed [(make-or-false style<%>) style])
    (when (not style)
      (set! changed? #t)
      (update-all)))
  
  ;; ----------------------------------------

  (def/override (begin-edit-sequence [any? [undoable? #t]] [any? [interrupt-seqs? #t]])
    (define ready! (wait-sequence-lock))
    (when (or (positive? s-noundomode)
              (not undoable?))
      (set! s-noundomode (add1 s-noundomode)))
    (when (and (zero? sequence)
               (zero? write-locked))
      (on-edit-sequence))
    (set! sequence (add1 sequence))
    (ready!))

  (def/override (end-edit-sequence)
    (set! sequence (sub1 sequence))
    (when (and (zero? sequence)
               (zero? write-locked))
      (set! sequence-streak? #f)
      (update-needed)
      (after-edit-sequence))
    (when (positive? s-noundomode)
      (set! s-noundomode (sub1 s-noundomode)))
    (when (and (zero? sequence)
               s-need-on-display-size?)
      (set! s-need-on-display-size? #f)
      (on-display-size)))

  (def/override (refresh-delayed?)
    (or (positive? sequence)
        (not s-admin)
        (send s-admin refresh-delayed?)))

  (def/override (in-edit-sequence?)
    (positive? sequence))

  (def/override (locations-computed?)
    (not need-resize?))

  ;; ----------------------------------------

  (def/public (get-dragable) dragable?)

  (def/public (set-dragable [bool? d?])
    (set! dragable? d?))

  (def/public (get-selection-visible) selection-visible?)

  (def/public (set-selection-visible [bool? v])
    (set! selection-visible? v))

  ;; ----------------------------------------

  (def/public (can-insert? [snip% a] [(make-or-false snip%) b] [real? x] [real? y])
    #t)
  (def/public (on-insert [snip% a] [(make-or-false snip%) b] [real? x] [real? y])
    (void))
  (def/public (after-insert [snip% a] [(make-or-false snip%) b] [real? x] [real? y])
    (void))

  (def/public (can-delete? [snip% s])
    #t)
  (def/public (on-delete [snip% s])
    (void))
  (def/public (after-delete [snip% s])
    (void))

  (def/public (can-move-to? [snip% s] [real? x] [real? y] [bool? dragging?])
    #t)
  (def/public (on-move-to [snip% s] [real? x] [real? y] [bool? dragging?])
    (void))
  (def/public (after-move-to [snip% s] [real? x] [real? y] [bool? dragging?])
    (void))

  (def/public (can-resize? [snip% s] [real? w] [real? h])
    #t)
  (def/public (on-resize [snip% s] [real? w] [real? h])
    (void))
  (def/public (after-resize [snip% s] [real? w] [real? h] [any? resized?])
    (void))

  (def/public (can-select? [snip% s] [bool? on?])
    #t)
  (def/public (on-select [snip% s] [bool? on?])
    (void))
  (def/public (after-select [snip% s] [bool? on?])
    (void))

  (def/public (can-reorder? [snip% s] [(make-or-false snip%) other] [bool? before?])
    #t)
  (def/public (on-reorder [snip% s] [(make-or-false snip%) other] [bool? before?])
    (void))
  (def/public (after-reorder [snip% s] [(make-or-false snip%) other] [bool? before?])
    (void))

  (def/public (can-interactive-move? [mouse-event% e])
    #t)
  (def/public (on-interactive-move [mouse-event% e])
    (void))
  (def/public (after-interactive-move [mouse-event% e])
    (void))

  (def/public (can-interactive-resize? [snip% s])
    #t)
  (def/public (on-interactive-resize [snip% s])
    (void))
  (def/public (after-interactive-resize [snip% s])
    (void))

  (define/override (do-begin-print dc fit?)
    (size-cache-invalid)
    (set! write-locked (add1 write-locked))
    (on-change)
    (set! write-locked (sub1 write-locked))
    #f)

  (define/override (do-end-print dc data)
    (size-cache-invalid)
    (set! write-locked (add1 write-locked))
    (on-change)
    (set! write-locked (sub1 write-locked)))

  (define/override (do-has-print-page? dc page)
    (do-has/print-page dc page #f))

  (def/override (print-to-dc [dc<%> dc] [exact-integer? [page -1]])
    (do-has/print-page dc page #t)
    (void))

  (define/private (do-has/print-page dc page print?)
    (check-recalc)
    
    (let-values ([(w h) (send dc get-size)])
      (let-boxes ([w w]
                  [h h]
                  [hm 0]
                  [vm 0])
          (begin
            (when (or (zero? (unbox w))
                      (zero? (unbox h)))
              (get-default-print-size w h))
            (unless (zero? page)
              (send (current-ps-setup) get-editor-margin hm vm)))
        (let ([W (- w (* 2 hm))]
              [H (- h (* 2 vm))]
              [eps? (zero? page)])
          (let-boxes ([w 0.0]
                      [h 0.0])
              (get-extent w h)

            (let ([hcount (if eps? 1 (->long (ceiling (/ w W))))]
                  [vcount (if eps? 1 (->long (ceiling (/ h H))))])

              (if (not print?)
                  (page . <= . (* hcount vcount))
                  (let-values ([(start end)
                                (cond
                                 [(zero? page) (values 1 1)]
                                 [(negative? page)
                                  (values 1 (* hcount vcount))]
                                 [else
                                  (values page page)])])
                    (for ([p (in-range start (add1 end))])
                      (let ([vpos (quotient (- p 1) hcount)]
                            [hpos (modulo (- p 1) hcount)])
                        (let ([x (* hpos W)]
                              [y (* vpos H)])
                          (when (page . <= . 0)
                            (send dc start-page))
                            
                          (draw dc (+ (- x) hm) (+ (- y) vm)
                                x y (+ x (if eps? w W)) (+ y (if eps? h H))
                                'no-caret
                                #f)

                          (when (page . <= . 0)
                            (send dc end-page)))))))))))))

  ;; ----------------------------------------
  )

(set-pasteboard%! pasteboard%)

;; ------------------------------------------------------------

(define/top (add-pasteboard-keymap-functions [keymap% tab])
  (void))
