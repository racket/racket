(module wxtextfield racket/base
  (require racket/class
           (prefix-in wx: "kernel.rkt")
           (prefix-in wx: "wxme/text.rkt")
           (prefix-in wx: racket/snip/private/snip)
           (prefix-in wx: "wxme/editor-canvas.rkt")
           "lock.rkt"
           "const.rkt"
           "check.rkt"
           "helper.rkt"
           "gdi.rkt"
           "wx.rkt"
           "wxwindow.rkt"
           "wxitem.rkt"
           "wxcanvas.rkt"
           "wxpanel.rkt"
           "editor.rkt"
           "mrpopup.rkt")

  (provide (protect-out wx-text-field%))

  (define no-pen (send wx:the-pen-list find-or-create-pen "white" 1 'transparent))
  (define black-brush (send wx:the-brush-list find-or-create-brush "black" 'solid))

  (define password-string-snip%
    (class wx:string-snip%
      (inherit get-count
               get-style
               get-text)
      (super-new)

      (define delta 3)
      (define hdelta 2)
      (define (get-size)
        (max 4 (send (send (get-style) get-font) get-point-size)))
               
      (define/override (get-extent dc x y [w #f] [h #f] [descent #f] [space #f] [lspace #f] [rspace #f])
        (let ([s (get-size)])
          (when w (set-box! w (* (max 1.0 (- s hdelta hdelta)) (get-count))))
          (when h (set-box! h (+ s 2.0)))
          (when descent (set-box! descent 1.0))
          (when space (set-box! space 1.0))
          (when lspace (set-box! lspace 0.0))
          (when rspace (set-box! rspace 0.0))))
      (define/override (partial-offset dc x y pos)
        (let ([s (get-size)])
          (* (max 1.0 (- s hdelta hdelta)) pos)))
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (let ([s (get-size)]
              [b (send dc get-brush)]
              [p (send dc get-pen)]
              [m (send dc get-smoothing)])
          (send dc set-pen no-pen)
          (send dc set-brush black-brush)
          (send dc set-smoothing 'aligned)
          (for/fold ([x x]) ([i (in-range (get-count))])
            (send dc draw-ellipse (- (+ x delta) hdelta) (+ y delta 1) (- s delta delta) (- s delta delta))
            (+ x (- s hdelta hdelta)))
          (send dc set-pen p)
          (send dc set-brush b)
          (send dc set-smoothing m)))
      (define/override (split pos first second)
        (let ([a (new password-string-snip%)]
              [b (new password-string-snip%)]
              [c (get-count)])
          (send a insert (get-text 0 pos) pos)
          (send b insert (get-text pos c) (- c pos))
          (set-box! first a)
          (set-box! second b)))))

  (define text-field-text% 
    (class text%
      (init cb ret-cb control set-cb-mgrs! record-text pw?)
      (rename-super [super-on-char on-char])
      (inherit get-text last-position set-max-undo-history get-flattened-text)
      (define return-cb ret-cb)
      (define password? pw?)
      (define block-callback 1)
      (define callback
        (lambda (type)
          (as-exit (lambda () (record-text (get-flattened-text))))
          (when (zero? block-callback)
            (let ([e (make-object wx:control-event% type)])
              (as-exit (lambda ()
                         (cb control e)))))))
      (override*
       [on-char
        (entry-point
         (lambda (e)
           (let ([c (send e get-key-code)])
             (unless (and (or (eq? c #\return) 
                              (eq? c #\newline)
                              (eq? c #\u3)) ; numpad enter
                          return-cb
                          (return-cb (lambda () (callback 'text-field-enter) #t)))
               (as-exit (lambda () (super-on-char e)))))))]
       [on-new-string-snip
        (lambda ()
          (if password?
              (new password-string-snip%)
              (super on-new-string-snip)))])
      (augment*
       [after-insert
	(lambda args
	  (as-entry
	   (lambda ()
	     (callback 'text-field))))]
       [after-delete
	(lambda args
	  (as-entry
	   (lambda ()
	     (callback 'text-field))))])
      (set-cb-mgrs!
       (lambda (thunk)
         (dynamic-wind
           (lambda () (set! block-callback (add1 block-callback)))
           thunk
           (lambda () (set! block-callback (sub1 block-callback)))))
       (lambda () 
         (set! block-callback 0)))
      (super-make-object)
      (set-max-undo-history 'forever)))
  
  (define wx-text-editor-canvas% 
    (class* wx-editor-canvas% (wx-text-editor-canvas<%>)
      (init mred proxy control parent style)
      (super-make-object mred proxy parent -1 -1 100 30 #f style 100 #f)))

  (define wx-text-field%
    (class wx-horizontal-panel%
      (init mred proxy parent fun label value style _font)
      (send (send parent get-top-level) begin-container-sequence)
      ;; Make text field first because we'll have to exit
      ;;  for keymap initializer
      (define func fun)
      (define font (or _font normal-control-font))
      (define without-callback #f)
      (define callback-ready #f)
      (define e
        (make-object text-field-text%
                     func
                     (lambda (do-cb)
                       (if multi?
                           #f
                           (do-cb)))
                     this
                     (lambda (wc cr)
                       (set! without-callback wc)
                       (set! callback-ready cr))
                     (lambda (t)
                       (send c set-combo-text t))
                     (memq 'password style)))
      (as-exit
       (lambda ()
         ((current-text-keymap-initializer) (send e get-keymap))))
      (inherit alignment stretchable-in-y area-parent
	       get-min-size set-min-width set-min-height
               spacing)
      (rename-super [super-place-children place-children])
      (public*
       [command (lambda (e)             ; No entry/exit needed
                  (check-instance '(method text-field% command) wx:control-event% 'control-event% #f e)
                  (func this e)
                  (void))]

       [get-editor (lambda () e)]
	
       [get-value (lambda () (send e get-text))] ; note: not as-entry when called
       [set-value (lambda (v) (without-callback
                               (lambda () (send e insert v 0 (send e last-position)))))]

       [set-label (lambda (str) (when l (send l set-label str)))]
       [get-canvas-width (lambda ()
                           (let ([tw (box 0)])
                             (send c get-size tw (box 0))
                             (unbox tw)))]
        
       [set-field-background (lambda (col)
                               (send c set-canvas-background col))]
       [get-field-background (lambda ()
                               (send c get-canvas-background))])
      (override*
       ;; These might be called before we are fully initialized

       [set-cursor (lambda (c) (send e set-cursor c #t))]
       [set-focus (lambda () (when (object? c) (send c set-focus)))]
	
       [place-children
        (lambda (children-info width height)
          (if (null? children-info)
              null
              (let ([r (super-place-children children-info width height)])
                (if horiz?
                    ;; Line up label right with text:
                    (cons (list* (caar r) (+ (cadar r) dy) (cddar r))
                          (cdr r))
                    r))))])
      (super-make-object #f proxy parent (if (memq 'deleted style) '(deleted) null) #f)
      (unless (memq 'deleted style)
        (send (area-parent) add-child this))
      (define multi? (memq 'multiple style))
      (define horiz?
        (cond
         [(memq 'vertical-label style) #f]
         [(memq 'horizontal-label style) #t]
         [else (eq? (send (send parent get-window) get-label-position) 'horizontal)]))
      (define dy 0)
      (define p
        (if horiz?
            this
            (let ([p (make-object wx-vertical-pane% #f proxy this null #f)])
              (send p skip-subwindow-events? #t)
              (send (send p area-parent) add-child p)
              p)))
      (alignment 'left 'top)
      (unless horiz? (send p alignment 'left 'top))
      (unless multi? (stretchable-in-y #f))
      ;; For Windows:
      ; (wx:set-combo-box-font font)
      (spacing 3)
      (define l (and label
                     (make-object wx-message% #f proxy p label -1 -1 null font)))
      (define combo-callback #f)
      (define c
        (make-object (class wx-text-editor-canvas% 
                       (define/override (on-combo-select i)
                         (let ([len (length callbacks)])
                           (when (< -1 i len)
                             ((list-ref callbacks (- len i 1))))))
                       (super-new))
                     #f proxy this p
                     (append
                      '(control-border)
                      (if (memq 'combo style)
                          '(combo)
                          null)
                      (if multi?
                          (if (memq 'hscroll style)
                              null
                              '(hide-hscroll))
                          '(hide-vscroll hide-hscroll)))))
      (define callbacks null)
      (override*
       [pre-on-event (lambda (w e)
                       (or (super pre-on-event w e)
                           (and combo-callback
                                (eq? w c)
                                (send e button-down?)
                                (let ([w (box 0)]
                                      [h (box 0)])
                                  (send c get-client-size w h)
                                  (not (and (<= 0 (send e get-x) (unbox w))
                                            (<= 0 (send e get-y) (unbox h)))))
                                (begin
                                  (do-popup-callback)
                                  #t))))])
      (private*
       [do-popup-callback (lambda ()
                            (wx:queue-callback (lambda ()
                                                 (when (send c is-enabled-to-root?)
                                                   (combo-callback)
                                                   (send c popup-combo)))
                                               wx:middle-queue-key))])
      (public*
       [set-on-popup (lambda (proc) 
                       (set! combo-callback proc) 
                       (send c set-on-popup (lambda () (do-popup-callback))))]
       [clear-combo-items (lambda () (set! callbacks null) (send c clear-combo-items))]
       [append-combo-item (lambda (s cb)
                            (and (send c append-combo-item s)
                                 (set! callbacks (cons cb callbacks))
                                 #t))])
      (send c skip-subwindow-events? #t)
      (when l
        (send l skip-subwindow-events? #t)
        (send l x-margin 0))
      (send c set-x-margin 2)
      (send c set-y-margin 2)
      (send e set-line-spacing 0)
      (send e set-paste-text-only #t)
      (send e auto-wrap (and multi? (not (memq 'hscroll style))))
      (let ([f font]
            [s (send (send e get-style-list) find-named-style "Standard")])
        (send s set-delta (font->delta f)))
      (send c set-editor e)
      (send c set-line-count (if multi? 3 1))
      (unless multi? (send c set-single-line))

      (when (and l horiz?)
        ;; Minimize vertical space around the label:
        (send l y-margin 0)
        ;; Find amount to drop label down to line up the baselines:
        (let ([wbox (box 0)]
              [hbox (box 0)]
              [ybox (box 0)]
              [abox (box 0)])
          ;; To bottom of first line
          (send (send e get-admin) get-dc #f ybox)
          (set! dy (+ (abs (unbox ybox)) (send e line-location 0 #f)))
	    
          ;; Add diff for client size
          (send c get-client-size wbox hbox)
          (let ([d (- (send c get-height) (unbox hbox))])
            (set! dy (+ dy (quotient d 2))))
	    
          ;; Subtract descent of canvas-drawn text
          (let ([font (send (send (send e get-style-list) find-named-style "Standard") get-font)])
            (send c get-text-extent "hi" wbox hbox ybox #f font)
            (set! dy (- dy (unbox ybox))))
	    
          ;; Subtract ascent of label
          (send l get-text-extent "hi" wbox hbox ybox abox font)
          (set! dy (- dy (- (unbox hbox) (unbox ybox))))
	    
          ;; Subtract space above label
          (set! dy (- dy (quotient (- (send l get-height) (unbox hbox)) 2)))

          ;; Exact
          (set! dy (inexact->exact dy))))

      (when value
        (set-value value)
        (unless (string=? value "")
          (let* ([ew (box 0)]
                 [cw (box 0)]
                 [tw (box 0)])
            (send e get-extent ew #f)
            (send (send e get-admin) get-view #f #f cw #f)
            (send c get-size tw (box 0))
            (let ([new-min-width (+ (unbox ew) (- (unbox tw) (unbox cw)))])
              (send c set-min-width (inexact->exact new-min-width))))))
      (let ([min-size (get-min-size)])
        (set-min-width (car min-size))
        (set-min-height (cadr min-size)))
      (send (send parent get-top-level) end-container-sequence)
      (callback-ready))))
