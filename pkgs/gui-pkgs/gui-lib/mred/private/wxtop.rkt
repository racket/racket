(module wxtop racket/base
  (require racket/class
           racket/list
           (prefix-in wx: "kernel.rkt")
           (prefix-in wx: "wxme/editor-canvas.rkt")
           (prefix-in wx: "wxme/editor-snip.rkt")
           "lock.rkt"
           "helper.rkt"
           "const.rkt"
           "check.rkt"
           "wx.rkt"
           "wxwindow.rkt"
           "wxcontainer.rkt")

  (provide (protect-out active-main-frame
			set-root-menu-wx-frame!)
	   get-display-size
	   get-display-left-top-inset
	   get-display-count
	   (protect-out make-top-container%
			make-top-level-window-glue%
			wx-frame%
			wx-dialog%))

  ;; Weak boxed:
  (define active-main-frame (make-weak-box #f))
  
  (define root-menu-wx-frame #f)
  (define (set-root-menu-wx-frame! f)
    (set! root-menu-wx-frame f))

  (define get-display-size
    (lambda ([full-screen? #f] #:monitor [monitor 0])
      (unless (exact-nonnegative-integer? monitor)
	(raise-argument-error 'get-display-size "exact-nonnegative-integer?" monitor))
      (let/ec esc
        (let ([xb (box 0)]
              [yb (box 0)])
          (wx:display-size xb yb full-screen? monitor
                           (lambda () (esc #f #f)))
          (values (unbox xb) (unbox yb))))))

  (define get-display-left-top-inset
    (lambda ([advisory? #f] #:monitor [monitor 0])
      (unless (exact-nonnegative-integer? monitor)
	(raise-argument-error 'get-display-left-top-inset "exact-nonnegative-integer?" monitor))
      (let/ec esc
        (let ([xb (box 0)]
              [yb (box 0)])
          (wx:display-origin xb yb advisory? monitor (lambda () (esc #f #f)))
          (values (unbox xb) (unbox yb))))))

  (define get-display-count
    (lambda ()
      (wx:display-count)))

  (define-values (left-margin top-margin init-top-x init-top-y)
    (let-values ([(x y) (get-display-left-top-inset #f)]
		 [(x2 y2) (get-display-left-top-inset #t)])
      (values (- x2 x)
              (- y2 y)
              (+ 1 (- x2 x))
	      (+ 1 (- y2 y)))))

  (define top-x init-top-x)
  (define top-y init-top-y)
  
  ;; make-top-container%: adds the necessary functionality to wx:frame% and 
  ;; wx:dialog%.
  ;; input: base%: the base class from which to descend the new class.
  ;;          Intended to be either wx:frame% or wx:dialog%, but can
  ;;          be anything which contains all methods in the inherit section
  ;;          below.
  ;; returns: a new class, descended from base%, which possesses the added
  ;;            capabilities necessary to serve as the frame/dialog which
  ;;            contains container classes.
  (define (make-top-container% base% dlg?)
    (class (wx-make-container% (wx-make-window% base% #t))
      (init parent)
      (init-rest args)
      (inherit get-x get-y get-width get-height set-size
	       get-client-size is-shown? on-close enforce-size
               get-eventspace get-focus-window)
      ;; have we had any redraw requests while the window has been
      ;; hidden?
      (define pending-redraws? #t)

      (define perform-updates? #t)
      (define seq-count 0)
       
      (define ignore-redraw-request? #f)
       
      (define already-trying? #f)
      (define was-bad? #f)    ; hack around min-frame-size limitations
      (define last-width -1)
      (define last-height -1)
       
      ;; pointer to panel in the frame for use in on-size
      (define panel #f)

      (define use-default-position? (and (= -11111 (list-ref args 2))
                                         (= -11111 (list-ref args 1))))
       
      (define enabled? #t)
      (define focus #f)

      (define border-buttons null)

      (define parent-for-center parent)

      (define show-ht (make-hasheq))
      (define fake-show-ht (make-hasheq))
      
      (override*
       [enable
        (lambda (b)
          (set! enabled? (and b #t))
          (super enable b))])

      (public*

       [is-enabled?
        (lambda () enabled?)]

       [set-focus-window
        (lambda (w)
          (unless (eq? 'macosx (system-type))
            (set! border-buttons (filter weak-box-value border-buttons))
            (if (not w)
                ;; Non-border button losing focus?
                (when (and (focus . is-a? . wx:button%)
                           (not (memq focus (map weak-box-value border-buttons))))
                  (send focus defaulting #f))
                ;; Something gaining focus... adjust border buttons
                (begin
                  (for-each (lambda (bb)
                              (let ([b (weak-box-value bb)])
                                (when b
                                  (send b defaulting (or (not (w . is-a? . wx:button%))
                                                         (eq? b w))))))
                            border-buttons)
                  (when (w . is-a? . wx:button%)
                    (send w defaulting #t))))
            (set! focus w)))]
	
       [get-edit-target-window
        (lambda () (get-focus-window #t))]
       [get-focus-object
        (lambda ()
          (window->focus-object (get-focus-window)))]
       [get-edit-target-object
        (lambda ()
          (window->focus-object (get-focus-window #t)))]

       [window->focus-object
        (lambda (w)
          (and w
               (if (is-a? w wx:editor-canvas%)
                   (let loop ([m (send w get-editor)]
                              [prev w])
                     (if m
                         (let ([snip (send m get-focus-snip)])
                           (if (and snip (is-a? snip wx:editor-snip%))
                               (loop (send snip get-editor) m)
                               m))
                         w))
                   focus)))]

       [add-border-button
        (lambda (b)
          (set! border-buttons (filter weak-box-value border-buttons))
          (set! border-buttons (cons (make-weak-box b) border-buttons)))]

       ;; add-child: update panel pointer.
       ;; input: new-panel: panel in frame (descendant of
       ;;   panel%) 
       ;; returns: nothing
       ;; effects: sets panel to new-panel
       ;;          if new-panel is not a descendant of
       ;;            panel%, calls error; panel not updated.
       [add-child
        (lambda (new-panel)
          (set! panel new-panel)
          (set! pending-redraws? #t)
          (let-values ([(client-w client-h)
                        (get-two-int-values (lambda (a b) (get-client-size a b)))])
            (send panel set-size 0 0 client-w client-h))
          (self-redraw-request))]

       [area-parent (lambda () #f)]
	
       [get-top-panel
        (lambda ()
          panel)]

       [delay-updates
        (case-lambda
	  [() (not perform-updates?)]
	  [(f) 
	   (set! perform-updates? (not f))
	   (when perform-updates?
	     (when pending-redraws?
	       (force-redraw))
	     (when (positive? (hash-count fake-show-ht))
	       (let ([t fake-show-ht])
		 (set! fake-show-ht (make-hasheq))
		 (hash-for-each
		  t
		  (lambda (win v?)
		    (send win really-show #t)))))
	     (when (positive? (hash-count show-ht))
	       (let ([t show-ht])
		 (set! show-ht (make-hasheq))
		 (hash-for-each
		  t
		  (lambda (win v?)
		    (send win show v?))))))])]
       [begin-container-sequence
        (lambda ()
          (when (zero? seq-count)
            (delay-updates #t))
          (set! seq-count (add1 seq-count)))]
       [end-container-sequence
        (lambda ()
          (set! seq-count (sub1 seq-count))
          (when (zero? seq-count)
            (delay-updates #f)))]

       [show-child
        (lambda (child show?)
          (if perform-updates?
              (send child show show?)
              (hash-set! show-ht child show?)))]

       [show-control
        (lambda (child on?)
          (if (or perform-updates?
                  (not on?)
                  (child . is-a? . wx-frame%)
                  (child . is-a? . wx-dialog%))
              (begin
                (hash-remove! fake-show-ht child)
                (send child really-show on?))
              (begin
                (hash-set! fake-show-ht child #t)
                (send child fake-show on?))))]

       ;; force-redraw: receives a message from to redraw the
       ;; entire frame.
       ;; input: none
       ;; returns: nothing
       ;; effects: redraws the frame at its current size (changing size
       ;;            as necessary).
       [child-redraw-request
        ;; since there's only one panel, we assume that `from' is the
        ;; panel and the request should be granted
        (lambda (from) 
          (unless ignore-redraw-request?
            (self-redraw-request)))]
       [self-redraw-request
        (lambda ()
          (if (and (is-shown?) perform-updates?)
              (force-redraw)
              (set! pending-redraws? #t)))]
       [force-redraw
        (lambda ()
          (if panel
              (dynamic-wind
                (lambda () (set! ignore-redraw-request? #t))
                (lambda () (resized))
                (lambda () (set! ignore-redraw-request? #f)))
	       
              (set! pending-redraws? #f)))]

       [correct-size
        (lambda (frame-w frame-h)
          (if (not panel)
              (values frame-w frame-h)
              (let-values ([(f-client-w f-client-h) (get-two-int-values 
                                                     (lambda (a b) (get-client-size a b)))])
                (let* ([panel-info (send panel get-info)]
			
                       ;; difference between panel's full size & 
                       ;; frame's full size
                       [delta-w (max 0 (- (get-width) f-client-w))]
                       [delta-h (max 0 (- (get-height) f-client-h))]

                       ;; minimum frame size:
                       [min-w (+ delta-w (child-info-x-min panel-info))]
                       [min-h (+ delta-h (child-info-y-min panel-info))]
			
                       ;; correct size for frame
                       [new-w
                        (cond
                         [(< frame-w min-w) min-w]
                         [(and (> frame-w min-w) (not (child-info-x-stretch panel-info))) min-w]
                         [else frame-w])]
                       [new-h
                        (cond
                         [(< frame-h min-h) min-h]
                         [(and (> frame-h min-h) (not (child-info-y-stretch panel-info))) min-h]
                         [else frame-h])])
                  (values (max new-w 1) (max new-h 1)
                          (max min-w 1) (max min-h 1)
                          (child-info-x-stretch panel-info) (child-info-y-stretch panel-info))))))]

       [set-panel-size
        (lambda ()
          (when panel
            (let-values ([(f-client-w f-client-h) (get-two-int-values 
                                                   (lambda (a b) (get-client-size a b)))]
                         [(panel-info) (send panel get-info)]
                         [(sel) (lambda (nsize psize stretch?)
                                  (if stretch?
                                      (max nsize psize)
                                      psize))])
              (send panel set-size 0 0 
                    (sel f-client-w (child-info-x-min panel-info)
                         (child-info-x-stretch panel-info))
                    (sel f-client-h (child-info-y-min panel-info)
                         (child-info-y-stretch panel-info)))
              (set! pending-redraws? #f)
              (send panel on-container-resize))))]


       [resized
        (entry-point
         (lambda ()
           (unless already-trying?
             (let ([new-width (get-width)]
                   [new-height (get-height)])
               (let-values ([(correct-w correct-h min-w min-h sx? sy?) (correct-size new-width new-height)])
                 (cond
                  [(and (= new-width correct-w) (= new-height correct-h))
                   ;; Good size; do panel
                   (set! was-bad? #f)
                   (enforce-size min-w min-h
                                 (if sx? -1 min-w) (if sy? -1 min-h)
                                 1 1)
                   (set-panel-size)]
                  [(and (= last-width correct-w) (= last-height correct-h)
                        was-bad?)
                   ;; We give up; do panel
                   (set-panel-size)]
                  [else
                   ;; Too large/small; try to fix it, but give up after a while
                   (set! was-bad? #t)
                   (set! last-width correct-w)
                   (set! last-height correct-h)
                   (set! already-trying? #t)
                   (enforce-size -1 -1 -1 -1 1 1)
                   (set-size -11111 -11111 correct-w correct-h)
                   (enforce-size min-w min-h
                                 (if sx? -1 min-w) (if sy? -1 min-h)
                                 1 1)
                   (set! already-trying? #f)
                   (resized)]))))))])

      (public*
       [call-show
        (lambda (on? do-show)
          (when on?
            (position-for-initial-show))
          (as-exit ; as-exit because there's an implicit wx:yield for dialogs
           do-show))])
      
      (override*
       ;; show: add capability to set perform-updates
       ;; input: now : boolean
       ;; returns: nothing
       ;; effects: if we're showing for the first time, unblock updates
       ;;            and force an update.  If we're hiding, block updates.
       ;;          pass now to superclass's show.
       [show
        (lambda (on?)
          (call-show
           on?
           (lambda () (super show on?))))]

       [on-visible
        (lambda ()
          (send panel queue-visible)
          (super on-visible))]
       [on-active
        (lambda ()
          (send panel queue-active)
          (super on-active))]
	
       [move (lambda (x y) (set! use-default-position? #f) (super move x y))]
       [center (lambda (dir)
                 (when pending-redraws? (force-redraw))
                 (set! use-default-position? #f)
                 (super center dir parent-for-center))] ; 2nd argument is for Mac OS X
	
       ;; on-size: ensures that size of frame matches size of content
       ;; input: new-width/new-height: new size of frame
       ;; returns: nothing
       ;; effects: if new size is smaller than allowed size of
       ;;            contents, frame resized to smallest possible size.
       ;;            If frame is larger than contents and contents
       ;;            aren't stretchable, frame resized to size of
       ;;            contents.  Each direction is handled
       ;;            independently.
       [queue-on-size
        (lambda ()
          (unless (and already-trying? (not (eq? 'unix (system-type))))
            (parameterize ([wx:current-eventspace (get-eventspace)])
              (wx:queue-callback (lambda () (resized)) wx:middle-queue-key))))])

      (public*
       [position-for-initial-show
        (lambda ()
          (when pending-redraws?
            (force-redraw))
          (when use-default-position?
            (set! use-default-position? #f)
            (if dlg?
                (center 'both)
                (let*-values ([(w) (get-width)]
                              [(h) (get-height)]
                              [(sw sh) (get-display-size)]
                              [(x x-reset?) (if (< (+ top-x w) (+ sw left-margin))
                                                (values top-x #f)
                                                (values (max init-top-x (- sw w 10)) #t))]
                              [(y y-reset?) (if (< (+ top-y h) (+ sh top-margin))
                                                (values top-y #f)
                                                (values (max init-top-y (- sh h 20)) #t))])
                  (move x y)
                  (set! top-x (if x-reset? init-top-x (+ top-x 10)))
                  (set! top-y (if y-reset? init-top-y (+ top-y 20)))))))]
       [handle-traverse-key
        (lambda (e)
          (and panel
               (let ([code (send e get-key-code)])
                 (case code
                   [(#\return) 
                    (let ([o (get-focus-window)])
                      (if (and o (send o handles-key-code code #f #f))
                          #f
                          (let ([objs (container->children panel #f #f)])
                            (or (ormap
                                 (lambda (x)
                                   (and (is-a? x wx:button%)
                                        (send x has-border?)
                                        (let ([v (make-object wx:control-event% 'button)])
                                          (do-command x v)
                                          #t)))
                                 objs)
                                (not (is-a? o wx:editor-canvas%))))))]
                   [(escape #\.)
                    (and (is-a? this wx:dialog%)
                         (or (eq? code 'escape)
                             (and (memq (system-type) '(macos macosx))
                                  (send e get-meta-down)))
                         (let ([o (get-focus-window)])
                           (if (and o (send o handles-key-code code #f (send e get-meta-down)))
                               #f
                               (begin
                                 (when (on-close)
                                   (show #f))
                                 #t))))]
                   [(#\space)
                    (let ([o (get-focus-window)])
                      (cond
                       [(is-a? o wx:button%)
                        (do-command o (make-object wx:control-event% 'button))
                        #t]
                       [(is-a? o wx:check-box%) 
                        (send o set-value (not (send o get-value)))
                        (do-command o (make-object wx:control-event% 'check-box))
                        #t]
                       [(is-a? o wx:radio-box%)
                        (let ([s (send o button-focus -1)])
                          (unless (negative? s)
                            (send o set-selection s)
                            (do-command o (make-object wx:control-event% 'radio-box))))
                        #t]
                       [(is-a? o wx-tab-group<%>)
                        (let ([s (send o button-focus -1)])
                          (unless (negative? s)
                            (send o set-selection s)
                            (do-command (wx->mred o) (make-object wx:control-event% 'tab-panel))))
                        #t]
                       [else #f]))]
                   [(#\tab left up down right) 
                    (let ([o (get-focus-window)])
                      (if (and o (send o handles-key-code code #f #f))
                          #f
                          (let* ([shift? (send e get-shift-down)]
                                 [forward? (or (and (eq? code #\tab) (not shift?))
                                               (memq code '(right down)))]
                                 [normal-move
                                  (lambda ()
                                    (let* ([o (if (or (is-a? o wx:canvas%) 
                                                      (is-a? o wx:item%)
                                                      (is-a? o wx:tab-panel%))
                                                  (if (is-a? o wx-group-box<%>)
                                                      #f
                                                      o)
                                                  #f)]
                                           [candidates 
                                            (map object->position (container->children panel o #t))]
                                           [dests (filter-overlapping candidates)]
                                           [pos (if o (object->position o) (list 'x 0 0 1 1))]
                                           [o (traverse (cadr pos) (caddr pos) (cadddr pos) (list-ref pos 4)
                                                        (case code
                                                          [(#\tab) (if shift? 'prev 'next)]
                                                          [else code])
                                                        dests)])
                                      (when o
                                        (if (or (is-a? o wx:radio-box%)
                                                (is-a? o wx-tab-group<%>))
                                            (send o button-focus (max 0 (send o button-focus -1)))
                                            (begin
                                              (send o set-focus)
                                              (if (and (is-a? o wx-text-editor-canvas<%>)
                                                       (send o is-single-line?))
                                                  (let ([e (send o get-editor)])
                                                    (as-exit
                                                     (lambda ()
                                                       (send e set-position 0 (send e last-position) #f #t 'local))))
                                                  ;; Not a text field; a canvas?
                                                  (when (or (is-a? o wx:canvas%)
                                                            (is-a? o wx:editor-canvas%))
                                                    (as-exit (lambda () (send o on-tab-in))))))))))])
                            (if (and (not (eqv? code #\tab))
                                     (or (is-a? o wx:radio-box%)
                                         (is-a? o wx:tab-panel%)))
                                (let ([n (send o number)]
                                      [s (send o button-focus -1)]
                                      [v-move? (memq code '(up down))]
                                      [h-move? (memq code '(left right))]
                                      [v? (and (is-a? o wx:radio-box%)
                                               (send o vertical?))])
                                  (cond
                                   [(or (negative? s) 
                                        (and v? h-move?) 
                                        (and (not v?) v-move?))
                                    (normal-move)]
                                   [(and forward? (< s (sub1 n)))
                                    (send o button-focus (add1 s))]
                                   [(and (not forward?) (positive? s))
                                    (send o button-focus (sub1 s))]
                                   [else (normal-move)]))
                                (normal-move))
                            #t)))]
                   [else (if (and (wx:shortcut-visible-in-label?)
                                  (char? code)
                                  (or (char-alphabetic? code)
                                      (char-numeric? code))
                                  (not (send e get-shift-down))
                                  (not (send e get-control-down))
                                  (not (send e get-alt-down)))
                             (let ([o (get-focus-window)]
                                   [meta? (send e get-meta-down)])
                               (if (and o (send o handles-key-code code #t meta?))
                                   #f
                                   ;; Move selection/hit control based on & shortcuts
                                   (let* ([objs (container->children panel #f #t)]
                                          [re (key-regexp code)])
                                     (ormap
                                      (lambda (o)
                                        (let* ([win (wx->proxy o)]
                                               [l (send win get-label)])
                                          (cond
                                           [(and (string? l)
                                                 (regexp-match re l))
                                            (send o set-focus)
                                            (send o char-to)
                                            #t]
                                           [(is-a? o wx:radio-box%)
                                            (let ([n (send o number)])
                                              (let loop ([i 0])
                                                (if (= i n)
                                                    #f
                                                    (let ([l (send win get-item-label i)])
                                                      (if (and (string? l)
                                                               (regexp-match re l))
                                                          (begin
                                                            (send o button-focus i)
                                                            (send o char-to-button i)
                                                            #t)
                                                          (loop (add1 i)))))))]
                                           [else #f])))
                                      objs))))
                             #f)]))))])
      
      (apply super-make-object parent args)))

  (define (make-top-level-window-glue% style-pos %) ; implies make-window-glue%
    (class (make-window-glue% %)
      (init mred proxy)
      (init-rest args)
      (inherit is-shown? get-mred queue-visible get-eventspace)
      (define act-date/seconds 0)
      (define act-date/milliseconds 0)
      (define act-on? #f)
      (define activate-refresh-wins null)
      (define floating-window?
        (and ((length args) . >= . style-pos)
             (memq 'float (list-ref args style-pos))))
      (public*
       [on-exit (entry-point
                 (lambda ()
                   (and (is-shown?)
                        (let ([mred (get-mred)])
                          (and (and mred (as-exit (lambda () (send mred can-exit?))))
                               (as-exit (lambda () (send mred on-exit))))))))])
      (override*
       [on-close (entry-point
                  (lambda ()
                    (let ([mred (get-mred)])
                      (if mred
                          (if (as-exit (lambda () (send mred can-close?)))
                              (begin
                                (as-exit (lambda () (send mred on-close)))
                                (queue-visible)
                                #t)
                              #f)
                          #t))))]
       [on-activate (entry-point
                     (lambda (on?)
                       (set! act-on? on?)
                       (when on?
                         (set! act-date/seconds (current-seconds))
                         (set! act-date/milliseconds (current-milliseconds))
                         (when (and (wx:main-eventspace? (get-eventspace))
                                    (not (eq? this root-menu-wx-frame))
                                    (not floating-window?))
                           (set! active-main-frame (make-weak-box this))))
                       ;; Send refresh to subwindows that need it
                       (set! activate-refresh-wins (filter weak-box-value activate-refresh-wins))
                       (for-each (lambda (b)
                                   (let ([win (weak-box-value b)])
                                     (when win
                                       (send win refresh))))
                                 activate-refresh-wins)
                       ;; Windows needs trampoline:
                       (queue-window-callback
                        this
                        (lambda () (send (get-mred) on-activate on?)))
                       (as-exit
                        (lambda ()
                          (super on-activate on?)))))]
       [display-changed
        (λ () 
          (send (get-mred) display-changed))])
      (public*
       [is-act-on? (lambda () act-on?)]
       [add-activate-update (lambda (win) (set! activate-refresh-wins  
                                                (cons (make-weak-box win)
                                                      activate-refresh-wins)))]
       [get-act-date/seconds (lambda () act-date/seconds)]
       [get-act-date/milliseconds (lambda () act-date/milliseconds)])
      (apply super-make-object mred proxy args)))

  (define function-keys #hasheq((f1 . #t)
				(f2 . #t)
				(f3 . #t)
				(f4 . #t)
				(f5 . #t)
				(f6 . #t)
				(f7 . #t)
				(f8 . #t)
				(f9 . #t)
				(f10 . #t)
				(f11 . #t)
				(f12 . #t)
				(f13 . #t)
				(f14 . #t)
				(f15 . #t)
				(f16 . #t)
				(f17 . #t)
				(f18 . #t)
				(f19 . #t)
				(f20 . #t)
				(f21 . #t)
				(f22 . #t)
				(f23 . #t)
				(f24 . #t)))

  (define wx-frame%
    (make-top-level-window-glue% 
     6
     (class (make-top-container% wx:frame% #f)
       (init-rest args)
       (define-values (menu-bar is-mdi-parent?)
         (values #f #f))
       (public*
        [get-the-menu-bar (lambda () menu-bar)]
        [get-mdi-parent (lambda () is-mdi-parent?)]
        [set-mdi-parent (lambda (x) (and (set! is-mdi-parent? x) #t))])
       (override*
        [set-menu-bar
         (lambda (mb)
           (when mb (set! menu-bar mb))
           (super set-menu-bar mb))]
        [on-menu-command
         (entry-point
          (lambda (id)
            (let ([wx (wx:id-to-menu-item id)])
              (when wx
                (do-command (wx->mred wx) (make-object wx:control-event% 'menu))))))]
        [on-menu-click
         (entry-point
          (lambda ()
            ;; Windows: no trampoline needed
            (and menu-bar (send menu-bar on-demand))))]
        [on-toolbar-click 
         (entry-point
          (lambda ()
            (as-exit (lambda () (send (wx->mred this) on-toolbar-button-click)))))]
        [on-mdi-activate
         (entry-point
          (lambda (on?)
            (let ([mr (wx->mred this)])
              (queue-window-callback
               this
               (lambda () (send mr on-mdi-activate on?))))))])
       (public*
        [handle-menu-key
         (lambda (event)
           (and menu-bar 
                (send menu-bar all-enabled?)
                ;; It can't be a menu event without a
                ;; control, meta, alt key, or function key
                (or (send event get-control-down)
                    (send event get-meta-down)
                    (send event get-alt-down)
                    (hash-ref function-keys (send event get-key-code) #f))
                (begin
                  (send menu-bar on-demand)
                  (send menu-bar handle-key event))))])
       (apply super-make-object args))))

  (define wx-dialog%
    (make-top-level-window-glue% 
     6
     (class (make-top-container% wx:dialog% #t)
       (init-rest args)
       (apply super-make-object args)))))
