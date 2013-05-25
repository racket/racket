#lang racket/base
(require racket/class
         racket/file
         "../syntax.rkt"
         "editor.rkt"
         "editor-admin.rkt"
         "private.rkt"
         racket/snip/private/prefs
         racket/snip/private/private
         (only-in "cycle.rkt" popup-menu%)
         (only-in "../kernel.rkt" queue-refresh-event)
         "wx.rkt")

(provide editor-canvas%)

;; FIXME: need contracts on public classes

;; ----------------------------------------

(define simple-scroll%
  (class object%
    (define horizontal #f)
    (define count 0)
    (define page-step 0)
    (define value 0)

    (init canvas
          style
          length
          steps-per-page
          position)

    (super-new)

    (set! count length)
    (set! page-step steps-per-page)
    (set! value position)

    (set! horizontal (and (memq 'horizontal style) #t))
    (set-scroll length steps-per-page position)
    
    (define/public (set-value position)
      (set! value (max 0 (min count position))))

    (define/public (set-scroll length steps-per-page position)
      (when (length . > . -1)
        (set! count length))
      (when (steps-per-page . > . 0)
        (set! page-step steps-per-page))
      (when (position . > . -1)
        (set! value position))
      
      (when (value . < . 0)
        (set! value 0))
      (when (value . > . count)
        (set! value count)))

    (define/public (get-value)
      value)))

;; ----------------------------------------

(define update-cursor-timer%
  (class timer%
    (inherit start stop)
    (init-field admin)

    (super-new)

    (define/override (notify)
      (stop)
      (when admin
        (send admin clear-update-cursor-timer)
        (send (send admin get-canvas) update-cursor-now)))

    (define/public (cancel)
      (set! admin #f))))

;; ----------------------------------------

(define BLINK-DELAY 500)

(define blink-timer%
  (class timer%
    (inherit stop)
    (init-field canvas)

    (super-new)
    
    (define/override (notify)
      (when canvas
        (send canvas blink-caret)))

    (define/public (kill)
      (set! canvas #f)
      (stop))))

;; ----------------------------------------

(define AUTO-DRAG-DELAY 100)

(define auto-drag-timer%
  (class timer% 
    (inherit start stop)
    (init-field canvas event)

    (super-new)

    (start AUTO-DRAG-DELAY #t)

    (define/override (notify)
      (when canvas
        (let ([e (make-object mouse-event% (send event get-event-type))])
          (send e set-alt-down (send event get-alt-down))
          (send e set-caps-down (send event get-caps-down))
          (send e set-control-down (send event get-control-down))
          (send e set-left-down (send event get-left-down))
          (send e set-meta-down (send event get-meta-down))
          (send e set-middle-down (send event get-middle-down))
          (send e set-right-down (send event get-right-down))
          (send e set-shift-down (send event get-shift-down))
          (send e set-x (send event get-x))
          (send e set-y (send event get-y))
          (send e set-time-stamp
                (+ (send e get-time-stamp) AUTO-DRAG-DELAY))
          (send canvas on-event event))))

    (define/public (kill)
      (set! canvas #f)
      (stop))))

;; ----------------------------------------

(define default-wheel-amt
  (let ([v (get-preference* 'GRacket:wheelStep)])
    (if (exact-integer? v)
        (max 3 (min 1000 v))
        3)))

(define (INIT-SB style)
  (append
   (if (or (memq 'no-hscroll style)
           (memq 'hide-hscroll style))
       null
       '(hscroll))
   (if (or (memq 'no-vscroll style)
           (memq 'hide-vscroll style))
       null
       '(vscroll))))

(define (memq? s l) (and (memq s l) #t))

(define (keep-style l s) (if (memq s l) (list s) null))

(defclass editor-canvas% canvas%

  (inherit refresh get-canvas-background get-dc
           get-client-size get-size set-cursor
           get-scroll-pos set-scroll-pos 
           get-scroll-page set-scroll-page
           get-scroll-range set-scroll-range
           is-shown-to-root?
           show-scrollbars
	   set-focus
           begin-refresh-sequence
           end-refresh-sequence)

  (define blink-timer #f)
  (define noloop? #f)

  (define focuson? #f)
  (define focusforcedon? #f)
  (define/public (get-focusforcedon?) focusforcedon?)
  (define lazy-refresh? #f)
  (define need-refresh? #f)

  (define auto-dragger #f)

  (define custom-cursor #f)
  (define custom-cursor-on? #f)

  (define scroll-to-last? #f)
  (define scroll-bottom-based? #f)
  (define scroll-offset 0)

  (define lastwidth -1)
  (define lastheight -1)

  (define last-x 0)
  (define last-y 0)

  (define bg-color #f)

  (define wheel-amt default-wheel-amt)
  (define xmargin 5)
  (define ymargin 5)

  (define/public (set-wheel-step v) (set! wheel-amt v))
  (define/public (get-wheel-step) wheel-amt)

  (set! noloop? #t)
  (init parent x y width height
        name style 
        [scrolls-per-page 100]
        [editor #f]
        [gl-config #f])

  (super-make-object parent
                     x y width height
                     (append (keep-style style 'border)
                             (INIT-SB style)
                             (keep-style style 'invisible)
                             (if (memq 'transparent style)
                                 '(transparent)
                                 '(no-autoclear))
                             (keep-style style 'control-border)
                             (keep-style style 'combo)
                             (keep-style style 'resize-corner)
                             (keep-style style 'no-focus))
                     name
                     gl-config)

  (define given-h-scrolls-per-page scrolls-per-page)

  (define allow-x-scroll? (not (memq 'no-hscroll style)))
  (define allow-y-scroll? (not (memq 'no-vscroll style)))

  (define fake-x-scroll? (or (not allow-x-scroll?)
                             (memq? 'hide-hscroll style)))
  (define fake-y-scroll? (or (not allow-y-scroll?)
                             (memq? 'hide-vscroll style)))

  (define auto-x? (and (not fake-x-scroll?)
                       (memq? 'auto-hscroll style)))
  (define auto-y? (and (not fake-y-scroll?)
                       (memq? 'auto-vscroll style)))

  (define xscroll-on? (and (not fake-x-scroll?) (not auto-x?)))
  (define yscroll-on? (and (not fake-y-scroll?) (not auto-y?)))

  (show-scrollbars xscroll-on? yscroll-on?)
  (super set-scrollbars 
         1 1 ;; Windows fake-{x,y}-scroll => -1 instead of 1 !? 
         1 1 ;;
         1 1 0 0 #f)
  
  (define hscroll
    (if fake-x-scroll?
        (new simple-scroll%
             [canvas this]
             [style '(horizontal)]
             [length 0]
             [steps-per-page 1]
             [position 0])
        #f))
  (define vscroll
    (if fake-y-scroll?
        (new simple-scroll%
             [canvas this]
             [style '(vertical)]
             [length 0]
             [steps-per-page 1]
             [position 0])
        #f))

  (define scroll-width (if fake-x-scroll? 1 1)) ;; else used to be 0
  (define scroll-height (if fake-y-scroll? 1 1))

  (define hscrolls-per-page 1)
  (define vscrolls-per-page 1)
  (define hpixels-per-scroll 0)

  (set! noloop? #f)

  (define admin (new canvas-editor-admin%
                     [canvas this]))
  (send admin adjust-std-flag)

  (define media editor)
  (when media (set-editor media))
  
  ;; FIXME: needed?
  (define/public (~)
    (when auto-dragger
      (send auto-dragger kill)
      (set! auto-dragger #f))
    (when blink-timer
      (send blink-timer kill)
      (set! blink-timer #f))
    (send admin set-canvas #f)
    #|(super ~)|#)
  
  (define/override (on-size)
    (unless noloop?
      (queue-refresh-event
       (get-eventspace)
       (lambda ()
         (unless (and media
                      (send media get-printing))
           (maybe-reset-size))))))

  (define/private (maybe-reset-size)
    (begin-refresh-sequence)
    (let-boxes ([w 0]
                [h 0])
        (get-size w h)
      (unless (and (= w lastwidth)
                   (= h lastheight))
        (reset-size)))
    (end-refresh-sequence))

  (define/private (reset-size)
    (reset-visual #f)
    (refresh))

  (define/public (set-x-margin x)
    (unless (= x xmargin)
      (set! xmargin x)
      (reset-size)))
  (define/public (set-y-margin y)
    (unless (= y ymargin)
      (set! ymargin y)
      (reset-size)))
  (define/public (get-x-margin) xmargin)
  (define/public (get-y-margin) ymargin)

  (define/override (set-canvas-background c)
    (super set-canvas-background c)
    (refresh))

  (define-syntax-rule (using-admin body ...)
    (let ([oldadmin (send media get-admin)])
      (unless (eq? admin oldadmin)
        (send media set-admin admin))
      (begin0
       (begin body ...)
       (when media
         (unless (eq? admin oldadmin)
           ;; FIXME: how do we know that this adminstrator
           ;; still wants the editor?
           (send media set-admin oldadmin))))))

  (define/private (get-eventspace)
    (send (send this get-top-level) get-eventspace))

  (define/private (on-focus focus?)
    (unless (eq? focus? focuson?)
      (set! focuson? focus?)
      (when (and media
                 (not (send media get-printing)))
        (using-admin
         (when media
           (send media own-caret focus?))))
      (when focuson?
        (unless blink-timer
          (set! blink-timer (parameterize ([current-eventspace (get-eventspace)])
                              (new blink-timer% [canvas this]))))
        (send blink-timer start BLINK-DELAY #t))))

  (define/public (blink-caret)
    (when focuson?
      (when media
        (using-admin
         (when media
           (send media blink-caret))))
      (send blink-timer start BLINK-DELAY #t)))

  (define/public (call-as-primary-owner thunk)
    (if media
        (using-admin
         (thunk))
        (thunk)))

  (define/override (on-set-focus)
    (super on-set-focus)
    (on-focus #t))
  (define/override (on-kill-focus)
    (super on-kill-focus)
    (on-focus #f))

  (define/public (is-focus-on?) focuson?)

  (define/public (force-display-focus on?)
    (let ([old-on? focusforcedon?])
      (set! focusforcedon? on?)
      (send admin adjust-std-flag)
      (when (not (equal? (or focuson? focusforcedon?)
                         (or focuson? old-on?)))
        (refresh))))

  (define/override (on-event event)
    ;; Turn off auto-dragger, if there is one
    (when auto-dragger
      (send auto-dragger kill)
      (set! auto-dragger #f))

    (let ([x (send event get-x)]
          [y (send event get-y)])
      (set! last-x x)
      (set! last-y y)

      #;
      (when (and (eq? 'windows (system-type))
		 (not focuson?)
		 (send event button-down?))
	(set-focus)
	(on-focus #t))

      (let ([out-of-client?
             (let-boxes ([cw 0]
                         [ch 0])
                 (get-client-size cw ch)
               (or (x . < . 0)
                   (y . < . 0)
                   (x . > . cw)
                   (y . > . ch)))])

        (when (and media
                   (not (send media get-printing)))
          (using-admin
           (when media
             (set-custom-cursor
              (and (or (not out-of-client?)
                       (send event dragging?))
                   (send media adjust-cursor event))))
           (when media
             (send media on-event event))))
        
        (when (send event dragging?)
          (when out-of-client?
            ;; Dragging outside the canvas: auto-generate more events because the buffer
            ;; is probably scrolling. But make sure we're shown.
            (when (is-shown-to-root?)
              (set! auto-dragger (parameterize ([current-eventspace (get-eventspace)])
                                   (new auto-drag-timer% 
                                        [canvas this]
                                        [event event])))))))))

  (define/private (update-cursor-now)
    (when media
      (let ([e (new mouse-event% [type 'motion])])
        (send e set-x last-x)
        (send e set-y last-y)
        (send e set-timestamp 0)

        (using-admin
         (when media
           (set-custom-cursor (send media adjust-cursor e)))))))

  (define/public (popup-for-editor b m) #f)

  (define/override (on-char event)
    (let ([code (send event get-key-code)])
      (case (and (positive? wheel-amt)
                 code)
        [(wheel-up wheel-down)
         (when (and allow-y-scroll?
                    (not fake-y-scroll?))
           (let-boxes ([x 0]
                       [y 0])
               (get-scroll x y)
             (let ([old-y y]
                   [y (max (+ y
                              (* wheel-amt
                                 (if (eq? code 'wheel-up)
                                     -1
                                     1)))
                           0)])
               (do-scroll x y #t x old-y))))]
        [(wheel-left wheel-right)
         (when (and allow-x-scroll?
                    (not fake-x-scroll?))
           (let-boxes ([x 0]
                       [y 0])
               (get-scroll x y)
             (let ([old-x x]
                   [x (max (+ x
                              (* wheel-amt
                                 (if (eq? code 'wheel-left)
                                     -1
                                     1)))
                           0)])
               (do-scroll x y #t old-x y))))]
        [else
         (when (and media (not (send media get-printing)))
           (using-admin
            (when media
              (send media on-char event))))])))

  (define/public (clear-margins)
    ;; This method is called by `on-paint' in `editor-canvas%'
    ;; before it calls the `on-paint' in `canvas%'. It's
    ;; essentially a compromise between autoclear mode and
    ;; no-autoclear mode.

    (when (or (positive? xmargin)
              (positive? ymargin))
      (let ([bg (get-canvas-background)])
        (when bg
          (let ([cw (box 0)]
                [ch (box 0)]
                [b (send the-brush-list find-or-create-brush bg 'solid)]
                [p (send the-pen-list find-or-create-pen "BLACK" 0 'transparent)]
                [dc (get-dc)])
            (get-client-size cw ch)
            (let ([ob (send dc get-brush)]
                  [op (send dc get-pen)]
                  [cw (unbox cw)]
                  [ch (unbox ch)])
              (send dc set-brush b)
              (send dc set-pen p)
              
              (send dc draw-rectangle 0 0 xmargin ch)
              (send dc draw-rectangle (- cw xmargin) 0 cw ch)
              (send dc draw-rectangle 0 0 cw ymargin)
              (send dc draw-rectangle 0 (- ch ymargin) cw ch)

              (send dc set-brush ob)
              (send dc set-pen op)))))))

  (define/override (on-paint)
    (set! need-refresh? #f)
    (if media
        (when (not (send media get-printing))
          (redraw 'view 'view 'view 'view #f))
        (let ([bg (get-canvas-background)])
          (when bg
            (let ([adc (get-dc)])
              (send adc set-background bg)
              (send adc clear)))))
    (super on-paint))

  (define/public (repaint)
    (unless need-refresh?
      (if (or lazy-refresh? (not (get-canvas-background)))
          (begin
            (set! need-refresh? #t)
            (refresh))
          (on-paint))))
  
  (define/private (paint-scrolls) (void))

  (define/public (set-lazy-refresh on?)
    (set! lazy-refresh? on?)
    (when (and (not on?)
               need-refresh?)
      (on-paint)))

  (define/public (get-lazy-refresh) lazy-refresh?)

  (define/public (set-custom-cursor cursor)
    (if (not cursor)
        (no-custom-cursor)
        (begin
          (set! custom-cursor-on? #t)
          (set! custom-cursor cursor)
          (set-cursor custom-cursor))))

  (define arrow #f)
  (define/public (no-custom-cursor)
    (when (not arrow)
      (set! arrow (make-object cursor% 'arrow)))
    (when custom-cursor-on?
      (set! custom-cursor-on? #f)
      (set-cursor arrow)))


  (define/public (get-dc-and-offset fx fy)
    (when (or fx fy)
      (let-boxes ([x 0]
                  [y 0])
          (get-scroll x y)
        (convert-scroll-to-location x y fx fy)))
    (get-dc))

  (define/private (convert-scroll-to-location x y fx fy)
    (when fx
      (set-box! fx (- (* x hpixels-per-scroll) xmargin)))
    (when fy
      (if (and media
               (or (positive? y)
                   scroll-bottom-based?))
          (let ([v (- (if (send media locked-for-read?)
                          0.0
                          (send media scroll-line-location (+ y scroll-offset)))
                      ymargin)])
            (set-box! fy v)
            (when (and scroll-bottom-based?
                       (or (positive? scroll-height)
                           scroll-to-last?))
              (let-boxes ([w 0] [h 0])
                  (get-client-size w h)
                (let ([h (max (- h (* 2 ymargin))
                              0)])
                  (set-box! fy (- (unbox fy) h))))))
          (set-box! fy (- ymargin)))))

  (define/public (get-view fx fy fw fh [unused-full? #f])
    (let ([w (box 0)]
          [h (box 0)])
      (get-client-size w h)
      (get-dc-and-offset fx fy)
      (when fx
        (set-box! fx (+ (unbox fx) xmargin)))
      (when fy
        (set-box! fy (+ (unbox fy) ymargin)))
      (when fh
        (set-box! fh (max 0 (- (unbox h) (* 2 ymargin)))))
      (when fw
        (set-box! fw (max 0 (- (unbox w) (* 2 xmargin)))))))

  (define/public (redraw localx localy fw fh clear?)
    (when (and media
               (not (send media get-printing)))
      (begin-refresh-sequence)
      (let-values ([(localx localy fw fh)
                    (if (eq? localx 'view)
                        (let-boxes ([x 0][y 0][w 0][h 0])
                            (get-view x y w h)
                          (values x y w h))
                        (values localx localy fw fh))])
        (when clear?
          (let ([bg (get-canvas-background)])
            (when bg
              (let ([adc (get-dc)])
                (let ([b (send adc get-brush)]
                      [p (send adc get-pen)])
                  (send adc set-brush bg 'solid)
                  (send adc set-pen bg 1 'transparent)
                  (send adc draw-rectangle localx localy fw fh)
                  (send adc set-brush b)
                  (send adc set-pen p))))))
        (let ([x (box 0)]
              [y (box 0)]
              [w (box 0)]
              [h (box 0)])
          (get-view x y w h)        
          (let ([x (unbox x)]
                [y (unbox y)]
                [w (unbox w)]
                [h (unbox h)])
            (let ([right (+ x w)]
                  [bottom (+ y h)])
              (let ([x (max x localx)]
                    [y (max y localy)]
                    [right (min right (+ localx fw))]
                    [bottom (min bottom (+ localy fh))])
                (let ([w (max 0 (- right x))]
                      [h (max 0 (- bottom y))])
                  (when (or (positive? w)
                            (positive? h))
                    (using-admin
                     (when media
                       (send media refresh
                             x y w h
                             (if (or focuson? focusforcedon?)
                                 'show-caret
                                 'show-inactive-caret)
                             (get-canvas-background)))))))))))
      (end-refresh-sequence)))


  (def/public (scroll-to [real? localx] [real? localy] [real? fw] [real? fh] [any? refresh?] 
                         [(symbol-in start none end) [bias 'none]])
    (let ([med media])
      (if (or (not med)
              (send med get-printing)
              (and (not allow-x-scroll?)
                   (not allow-y-scroll?)))
          #f
          (let-boxes ([x 0]
                      [y 0]
                      [iw 0]
                      [ih 0])
              (get-view x y iw ih)
            (if (or (zero? iw)
                    (zero? ih))
                #f
                (let ([find-dy (if scroll-bottom-based?
                                   ih
                                   0)])
                  (let-boxes ([cx 0]
                              [cy 0])
                      (get-scroll cx cy)
                    (let ([sy
                           (if allow-y-scroll?
                               (cond
                                [(or
                                  ;; doesn't fit and bias is set:
                                  (and (eq? bias 'start) (fh . > . ih))
                                  ;; fits, need to shift down into view:
                                  (and (fh . <= . ih) (localy . < . y) )
                                  ;; doesn't fit, no conflicting bias, can shift up to see more:
                                  (and (fh . > . ih) (not (eq? bias 'end)) (localy . < . y)))
                                 (- (send med find-scroll-line (+ find-dy localy))
                                    scroll-offset)]
                                [(or
                                  ;; doesn't fit, bias is set:
                                  (and (eq? bias 'end) (fh . > . ih))
                                  ;; fits, need to shift up into view:
                                  (and (fh . <= . ih) ((+ y ih) . < . (+ localy fh))))
                                 (let ([l (+ find-dy localy (- fh ih))])
                                   ;; find scroll pos for top of region to show:
                                   (let ([sy (send med find-scroll-line l)])
                                     ;; unless l is exactly the top of a line, move down to the next whole line:
                                     (let ([sy (if (= (send med scroll-line-location sy) l)
                                                   sy
                                                   (+ sy 1))])
                                       (- sy scroll-offset))))]
                                [(or
                                  ;; doesn't fit, no conflicting bias, maybe shift down to see more:
                                  (and (fh . > . ih)
                                       (not (eq? bias 'start))
                                       ((+ localy fh) .  > . (+ y ih))))
                                 ;; shift to one more than the first scroll position that shows last line
                                 (let ([my (+ (send med find-scroll-line (+ find-dy localy (- fh ih)))
                                              (- 1 scroll-offset))])
                                   ;; but only shift down the extra line if doing so doesn't skip the whole area
                                   (cond
                                    [((send med scroll-line-location my) . < . (+ find-dy localy fh))
                                     my]
                                    [(my . > . 0)
                                     (- my 1)]
                                    [else 0]))]
                                [else cy])
                               cy)]
                          [sx
                           (if allow-x-scroll?
                               (if (positive? hpixels-per-scroll)
                                   (cond
                                    [(or (and (eq? bias 'start) (fw . > . iw))
                                         (and (fw . < . iw) (localx . < . x))
                                         (and (fw . > . iw) (not (eq? bias 'end)) (localx . < . x)))
                                     (->long (/ localx hpixels-per-scroll))]
                                    [(or (and (eq? bias 'end) (fw . > . iw))
                                         (and (fw . < . iw) ((+ x iw) . < . (+ localx fw)))
                                         (and (fw . > . iw) (not (eq? bias 'start)) ((+ localx fw) . > . (+ x iw))))
                                     (+ (->long (/ (+ localx (- fw iw)) hpixels-per-scroll)) 1)]
                                    [else cx])
                                   0)
                               cx)])
                      (if (or (not (= sy cy))
                              (not (= sx cx)))
                          (begin
                            (when hscroll
                              (send hscroll set-value sx))
                            (when vscroll
                              (send vscroll set-value sy))
                            (do-scroll sx sy refresh? cx cy)
                            #t)
                          #f)))))))))

  (define/public (reset-visual reset-scroll?)
    (if (given-h-scrolls-per-page . < . 0)
        (begin
          (set! given-h-scrolls-per-page -2)
          #f)
        (let loop ([retval #f] [iters 0])
          (let-boxes ([sx 0]
                      [sy 0])
              (get-scroll sx sy)
            (let-boxes ([lw 0]
                        [lh 0])
                (get-size lw lh)
              (set! lastwidth lw)
              (set! lastheight lh)
              
              (let-values ([(x y vnum-scrolls hnum-scrolls vspp hspp)
                            (if (and media (or allow-x-scroll? allow-y-scroll?))

                                (let ([med media])
                                  (let-values ([(x y)
                                                (if reset-scroll?
                                                    (values 0 0)
                                                    (values sx sy))])
                                    
                                    (let-boxes ([w 0.0]
                                                [h 0.0])
                                        (get-view #f #f w h)
                                      (let-boxes ([total-width 0.0]
                                                  [total-height 0.0])
                                          (send med get-extent total-width total-height)

                                        (let-values ([(vnum-scrolls -scroll-offset)
                                                      (if (or (zero? h)
                                                              (and (not scroll-to-last?)
                                                                   (h . >= . total-height)))
                                                          (values 0 0)

                                                          (if scroll-bottom-based?
                                                              (let ([vnum-scrolls (- (send med num-scroll-lines) 1)])
                                                                (if scroll-to-last?
                                                                    (values vnum-scrolls 1)
                                                                    (let ([start (- (send med find-scroll-line (+ h 1)) 1)])
                                                                      (values (- vnum-scrolls start)
                                                                              (+ 1 start)))))
                                                              (let ([top (max 0
                                                                              (- (->long (- total-height
                                                                                            (if scroll-to-last?
                                                                                                0
                                                                                                h)))
                                                                                 1))])
                                                                (let ([vnum-scrolls (+ (send med find-scroll-line top) 1)]
                                                                      [nsl (send med num-scroll-lines)])
                                                                  (values (if (vnum-scrolls . >= . nsl)
                                                                              (- nsl 1)
                                                                              vnum-scrolls)
                                                                          0)))))])

                                          (set! scroll-offset -scroll-offset)

                                          (let-values ([(vnum-scrolls vspp)
                                                        (if (positive? vnum-scrolls)
                                                            (let ([num-lines (- (send med num-scroll-lines) 1)])
                                                              (values vnum-scrolls
                                                                      (max 1
                                                                           (- (->long
                                                                               (/ (* h num-lines)
                                                                                  total-height))
                                                                              1))))
                                                            (values 0 1))])

                                            (let-values ([(hnum-scrolls hspp)
                                                          (if (total-width . >= . w)
                                                              (let ([tw (->long (- total-width w))])
                                                                (set! hpixels-per-scroll 
                                                                      (let ([v (->long (/ w given-h-scrolls-per-page))])
                                                                        (if (zero? v) 2 v)))
                                                                (let ([tw
                                                                       (if (modulo tw hpixels-per-scroll)
                                                                           (+ tw (- hpixels-per-scroll (modulo tw hpixels-per-scroll)))
                                                                           tw)])
                                                                  (values (->long (/ tw hpixels-per-scroll))
                                                                          given-h-scrolls-per-page)))
                                                              (values 0 1))])

                                              (values x y vnum-scrolls hnum-scrolls vspp hspp))))))))

                                (begin0
                                 (values 0 0 0 0 1 1)
                                 (when (not media)
                                   (let ([dc (get-dc)])
                                     (let ([bg (get-canvas-background)])
                                       (when bg
                                         (send dc set-background bg)
                                         (send dc clear)))))))])

                (if (not (and (= scroll-width hnum-scrolls)
                              (= scroll-height vnum-scrolls)
                              (= vspp vscrolls-per-page)
                              (= hspp hscrolls-per-page)
                              (= x sx)
                              (= y sy)))
                    (begin
                      (when hscroll
                        (send hscroll set-scroll hnum-scrolls hspp x))
                      (when vscroll
                        (send vscroll set-scroll vnum-scrolls vspp y))
                      (let ([savenoloop? noloop?]
                            [save-h-s-p-p given-h-scrolls-per-page])
                        (set! noloop? #t)
                        (set! given-h-scrolls-per-page -1)
                        
                        (let ([xon? (and (not fake-x-scroll?) (not (zero? hnum-scrolls)))]
                              [yon? (and (not fake-y-scroll?) (not (zero? vnum-scrolls)))])
                          (let ([go-again?
                                 (if (or (and auto-x? (not (eq? xon? xscroll-on?)))
                                         (and auto-y? (not (eq? yon? yscroll-on?))))
                                     (begin
                                       (when auto-x?
                                         (set! xscroll-on? xon?))
                                       (when auto-y?
                                         (set! yscroll-on? yon?))
                                       (show-scrollbars xscroll-on? yscroll-on?)
                                       (on-scroll-on-change)
                                       #t)
                                     #f)])

                            (unless fake-x-scroll?
                              (let ([x (min x hnum-scrolls)])
                                (when (hspp . < . hscrolls-per-page)
                                  (set-scroll-page 'horizontal (min hspp 10000000)))
                                (when (x . < . sx)
                                  (set-scroll-pos 'horizontal x))
                                (when (not (= scroll-width hnum-scrolls))
                                  (set-scroll-range 'horizontal (min hnum-scrolls 10000000)))
                                (when (x . > . sx)
                                  (set-scroll-pos 'horizontal (min x 10000000)))
                                (when (hspp . > . hscrolls-per-page)
                                  (set-scroll-page 'horizontal (min hspp 10000000)))))
                            
                            (unless fake-y-scroll?
                              (let ([y (min y vnum-scrolls)])
                                (when (vspp . < . vscrolls-per-page)
                                  (set-scroll-page 'vertical (min vspp 10000000)))
                                (when (y . < . sy)
                                  (set-scroll-pos 'vertical (min y 10000000)))
                                (when (not (= scroll-height vnum-scrolls))
                                  (set-scroll-range 'vertical (min vnum-scrolls 10000000)))
                                (when (y . > . sy)
                                  (set-scroll-pos 'vertical (min y 10000000)))
                                (when (vspp . > . vscrolls-per-page)
                                  (set-scroll-page 'vertical (min vspp 10000000)))))

                            (let ([go-again? (or go-again?
                                                 (given-h-scrolls-per-page . < . -1))])
                              (set! given-h-scrolls-per-page save-h-s-p-p)
                              (set! noloop? savenoloop?)
                              (set! hscrolls-per-page hspp)
                              (set! vscrolls-per-page vspp)
                              (set! scroll-width hnum-scrolls)
                              (set! scroll-height vnum-scrolls)

                              (when (and go-again? (iters . > . 2))
                                ;; we're not reaching a fixpoint, so
                                ;; it seems that a horizontal scroll
                                ;; is needed iff there's a vertical
                                ;; scrollbar; force a fixpoint
                                (cond
                                 [(and auto-x? auto-y?)
                                  (set! xscroll-on? #f)
                                  (set! yscroll-on? #f)]
                                 ;; I don't think these cases are possible,
                                 ;; but in case I have it wrong, conservatively
                                 ;; force scrollbars on.
                                 [auto-x? (set! xscroll-on? #t)]
                                 [auto-y? (set! yscroll-on? #t)])
                                (show-scrollbars xscroll-on? yscroll-on?))

                              (if go-again?
                                  (loop #t (add1 iters))
                                  #t))))))

                    retval)))))))

  (define/private (do-scroll x y refresh? old-x old-y)
    (let ([savenoloop? noloop?])
      (set! noloop? #t)
      
      (maybe-reset-size)

      (when (and (x . > . -1)
                 (not fake-x-scroll?))
        (when (positive? scroll-width)
          (set-scroll-pos 'horizontal (min (->long (min x scroll-width)) 10000000))))
      
      (when (and (y . > . -1)
                 (not fake-y-scroll?))
        (when (positive? scroll-height)
          (set-scroll-pos 'vertical (min (->long (min y scroll-height))  10000000))))
      
      (set! noloop? savenoloop?)

      (when refresh?
        (if (and #f ;; special scrolling disabled: not faster with Cocoa, broken for Windows
                 (not need-refresh?)
                 (not lazy-refresh?)
                 (get-canvas-background)
                 (= x old-x)) ; could handle horizontal scrolling in the future
            (let-boxes ([fx 0]
                        [old-fy 0]
                        [new-fy 0])
                (begin
                  (convert-scroll-to-location x y fx new-fy)
                  (convert-scroll-to-location old-x old-y #f old-fy))
              (let-boxes ([vx 0][vy 0][vw 0][vh 0])
                  (get-view vx vy vw vh) ; editor coords
                (cond
                 [(and (new-fy . < . old-fy)
                       (old-fy . < . (+ new-fy vh)))
                  (let ([dc (get-dc)])
                    (send dc copy
                          xmargin ymargin
                          vw (- (+ new-fy vh) old-fy)
                          xmargin (+ ymargin (- old-fy new-fy)))
                    (redraw xmargin ymargin 
                            vw (- old-fy new-fy)
                            #t))]
                 [(and (old-fy . < . new-fy)
                       (new-fy . < . (+ old-fy vh)))
                  (let ([dc (get-dc)])
                    (send dc copy
                          xmargin (+ ymargin (- new-fy old-fy))
                          vw (- (+ old-fy vh) new-fy)
                          xmargin ymargin)
                    (let ([d (- (+ old-fy vh) new-fy)])
                      (redraw xmargin (+ ymargin d)
                              vw (- vh d)
                              #t)))]
                 [else (repaint)])))
            (repaint)))))

  (define/override (set-scrollbars x y x2 y2 x3 y3 x4 y4 ?) (void))

  (define/public (get-scroll x y)
    ;; get fake scroll values if available
    (set-box! x (if hscroll
                    (send hscroll get-value)
                    (get-scroll-pos 'horizontal)))
    (set-box! y (if vscroll
                    (send vscroll get-value)
                    (get-scroll-pos 'vertical))))

  (define/public (editor-canvas-on-scroll)
    (unless noloop?
      (repaint)))

  (define/public (on-scroll-on-change) 
    (void))

  (define/public (get-editor) media)

  (define/public (set-editor m [update? #t])
    (unless (eq? media m)
      (when media
        (when (eq? admin (send media get-admin))
          (send media set-admin 
                (or (send admin get-nextadmin)
                    (send admin get-prevadmin))))

        (let ([a (send admin get-nextadmin)])
          (when a
            (send a set-prevadmin (send admin get-prevadmin))
            (send a adjust-std-flag)))
        (let ([a (send admin get-prevadmin)])
          (when a
            (send a set-nextadmin (send admin get-nextadmin))
            (send a adjust-std-flag)))
        (send admin set-nextadmin #f)
        (send admin set-prevadmin #f)
        (when custom-cursor
          (no-custom-cursor)
          (set! custom-cursor #f)))
      (set! media m)
      (when media
        (let ([oldadmin (send media get-admin)])
          (if (and oldadmin
                   (not (send oldadmin get-s-standard)))
              (set! media #f)
              (if oldadmin
                  (begin
                    (unless (in-chain? admin oldadmin)
                      (send admin set-nextadmin oldadmin)
                      (send admin set-prevadmin (send oldadmin get-prevadmin))
                      (send oldadmin set-prevadmin admin)
                      (send oldadmin adjust-std-flag)
                      (let ([a (send admin get-prevadmin)])
                        (when a
                          (send a set-nextadmin admin)
                          (send a adjust-std-flag))))
                    ;; get the right cursor:
                    (send admin update-cursor))
                  (begin
                    (send admin set-nextadmin #f)
                    (send admin set-prevadmin #f)
                    (send media set-admin admin)
                    (send media own-caret focuson?))))))
      (send admin adjust-std-flag)
      (reset-visual #t)
      (when update?
        (repaint))))

  (define/private (in-chain? admin oldadmin)
    (or (let loop ([oldadmin oldadmin])
          (and oldadmin
               (or (eq? admin oldadmin)
                   (loop (send oldadmin get-prevadmin)))))
        (let loop ([oldadmin oldadmin])
          (and oldadmin
               (or (eq? admin oldadmin)
                   (loop (send oldadmin get-nextadmin)))))))

  (define/public (allow-scroll-to-last to-last?)
    (set! scroll-to-last? to-last?)
    (reset-visual #f)
    (repaint))

  (define/public (scroll-with-bottom-base bottom?)
    (set! scroll-bottom-based? bottom?)
    (reset-visual #f)
    (repaint)))

;; ----------------------------------------

(defclass canvas-editor-admin% editor-admin%
  (init-field canvas)

  (super-new)

  (inherit set-s-standard)
  
  (define reset? #f)
  (properties [[any? nextadmin] #f]
              [[any? prevadmin] #f])

  (define update-cursor-timer #f)

  (define update-block? #f)
  (define resized-block? #f)

  ;; FIXME: needed?
  (define/private (~)
    (when update-cursor-timer
      (send update-cursor-timer cancel)
      (set! update-cursor-timer #f))
    (set! canvas #f))

  (define/public (do-get-canvas) canvas)

  (define canvasless-offscreen #f)

  (define/override (get-dc [fx #f] [fy #f])
    (cond
     [(not canvas)
      (unless canvasless-offscreen
        (set! canvasless-offscreen (new bitmap-dc%)))
      (when fx (set-box! fx 0))
      (when fy (set-box! fy 0))
      canvasless-offscreen]
     [(let ([m (send canvas get-editor)])
        (and m (send m get-printing)))
      => (lambda (p)
           (when fx (set-box! fx 0))
           (when fy (set-box! fy 0))
           p)]
     [else
      (send canvas get-dc-and-offset fx fy)]))

  (define/override (get-view fx fy fh fw [full? #f])
    (cond
     [(not canvas)
      (when fx (set-box! fx 0))
      (when fy (set-box! fy 0))
      (when fh (set-box! fh 1))
      (when fw (set-box! fw 1))]
     [(let ([m (send canvas get-editor)])
        (and m (send m get-printing)))
      (when fx (set-box! fx 0))
      (when fy (set-box! fy 0))
      (when fh (set-box! fh 10000))
      (when fw (set-box! fw 10000))]
     [else
      (send canvas get-view fx fy fh fw full?)]))

  (define/override (get-max-view fx fy fw fh [full? #f])
    (if (or (and (not nextadmin)
                 (not prevadmin))
            (not canvas)
            (and (let ([m (send canvas get-editor)])
                   (and m (send m get-printing)))))
        (get-view fx fy fw fh full?)
        (let ([a (let loop ([a this])
                   (let ([a2 (send a get-prevadmin)])
                     (if a2
                         (loop a2)
                         a)))])
          (let-boxes ([cx 0] [cy 0] [cw 0] [ch 0])
              (send a get-view cx cy cw ch)
            (let loop ([a (send a get-nextadmin)]
                       [cx cx][cy cy][cr (+ cx cw)][cb (+ cy ch)])
              (if (not a)
                  (let ([cw (- cr cx)]
                        [ch (- cb cy)])
                    (when fx (set-box! fx cx))
                    (when fy (set-box! fy cy))
                    (when fw (set-box! fw cw))
                    (when fh (set-box! fh ch)))
                  (let-boxes ([x 0] [y 0] [w 0] [h 0])
                      (send a get-view x y w h)
                    (loop (send a get-nextadmin)
                          (min x cx)
                          (min y cy)
                          (max (+ x w) cr)
                          (max (+ y h) cb)))))))))

  (def/override (scroll-to [real? localx] [real? localy] [real? w] [real? h] [any? [refresh? #t]] 
                           [(symbol-in start none end) [bias 'none]])
    (let ([v (do-scroll-to localx localy w h refresh? bias #t #t #f)])
      (and v (car v))))

  (define/public (do-scroll-to localx localy w h refresh? bias prev? next? only-focus?)
    (and canvas
         (or (and (not (send canvas is-focus-on?))
                  (or
                   (and prev? 
                        prevadmin
                        (send prevadmin do-scroll-to localx localy w h refresh? bias #t #f #t))
                   (and next? 
                        nextadmin
                        (send nextadmin do-scroll-to localx localy w h refresh? bias #f #t #t))))
             (and (or (not only-focus?)
                      (send canvas is-focus-on?))
                  (list (send canvas scroll-to localx localy w h refresh? bias))))))

  (def/override (grab-caret [(symbol-in immediate display global) dist])
    (when canvas
      (when (eq? dist 'global)
        (send canvas set-focus))))

  (define/public all-in-chain
    (case-lambda
     [(proc) (all-in-chain proc #t #t)]
     [(proc backward? forward?)
      (proc this)
      (when (and forward? nextadmin)
        (send nextadmin all-in-chain proc #f #t))
      (when (and backward? prevadmin)
        (send prevadmin all-in-chain proc #t #f))]))

  (def/override (needs-update [real? localx] [real? localy]
                              [nonnegative-real? w] [nonnegative-real? h])
    (all-in-chain (lambda (a) (send a do-needs-update localx localy w h))))

  (define/public (do-needs-update localx localy w h)
    (when canvas
      (let ([is-shown? (send canvas is-shown-to-root?)])
        
        (cond
         [reset?
          (when is-shown? (send canvas repaint))
          (set! reset? #f)]
         [is-shown?
          (if (not (send canvas get-canvas-background))
              (send canvas repaint)
              (send canvas redraw localx localy w h #f))]))))

  (define/override (resized update?)
    (all-in-chain (lambda (a) (send a do-resized update?))))

  (define/public (do-resized update?)
    (when canvas 
      (when (send canvas reset-visual #f)
        (set! reset? #t))
      
      (when update?
        (send canvas repaint)
        (set! reset? #f))))

  (define/override (update-cursor)
    (all-in-chain (lambda (a) (send a do-update-cursor))))

  (define/public (do-update-cursor)
    (when (not update-cursor-timer)
      (set! update-cursor-timer (new update-cursor-timer% [admin this]))))

  (def/override (popup-menu [popup-menu% m] [real? x] [real? y])
    (and canvas
         (let ([e (send canvas get-editor)])
           (and e
                (let ([m (send canvas popup-for-editor e m)])
                  (let-boxes ([dx 0.0]
                              [dy 0.0])
                      (send canvas get-dc-and-offset dx dy)
                    (send canvas popup-menu m (->long (- x dx)) (->long (- y dy)))))))))
  
  (define/public (adjust-std-flag)
    ;; 1 indicates that this is the sole, main admin. 
    ;; this info is used for quick (xor) caret refreshing
    ;; by an editor buffer
    (set-s-standard (if (or nextadmin 
                            prevadmin 
                            (and canvas (send canvas get-focusforcedon?)))
                        -1
                        1)))

  (def/override (modified [bool? modified?]) (void)))
