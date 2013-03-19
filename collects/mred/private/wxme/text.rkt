#lang racket/base
(require racket/class
         racket/port
         racket/file
         (for-syntax racket/base)
         "../syntax.rkt"
         "const.rkt"
         "mline.rkt"
         "private.rkt"
         racket/snip/private/private
         racket/snip/private/prefs
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
                  set-text%!)
         "wordbreak.rkt"
         "stream.rkt"
         "wx.rkt")

(provide text%
         add-text-keymap-functions)

;; ----------------------------------------

(define flash-timer% 
  (class timer%
    (init editor)
    (define for-editor editor)
    (super-new)
    (define/override (notify)
      (send for-editor flash-off))))

;; ----------------------------------------

(define arrow (make-object cursor% 'arrow))
(define i-beam (make-object cursor% 'ibeam))

(define MAX-COUNT-FOR-SNIP 500)
(define A-VERY-BIG-NUMBER 1e50)

(define TAB-WIDTH 20.0)

;; Used when max-width is set, but padding takes up
;; all available space:
(define ZERO-LINE-WIDTH 0.1)

(define show-outline-for-inactive?
  (and (get-preference* 'GRacket:outline-inactive-selection) #t))

(define caret-pen (send the-pen-list find-or-create-pen "BLACK" 1 'xor))
(define outline-pen (send the-pen-list find-or-create-pen "BLACK" 0 'transparent))
(define outline-inactive-pen (send the-pen-list find-or-create-pen (get-highlight-background-color) 1 'solid))
(define outline-brush (send the-brush-list find-or-create-brush (get-highlight-background-color) 'solid))
(define outline-nonowner-brush outline-brush)
(define clear-brush (send the-brush-list find-or-create-brush "WHITE" 'solid))

(define (showcaret>= a b)
  (memq a (memq b '(no-caret show-inactive-caret show-caret))))

(define-struct clickback (start end f call-on-down? delta hilited? unhilite) #:mutable)

(define in-delayed-refresh (make-parameter #f))

(defclass text% editor%
  (inherit-field s-admin
                 s-offscreen
                 s-custom-cursor
                 s-custom-cursor-overrides?
                 s-keymap
                 s-own-caret?
                 s-style-list
                 s-user-locked?
                 s-modified?
                 s-noundomode
                 s-caret-snip
                 s-inactive-caret-threshold
                 s-filename
                 s-temp-filename?
                 s-need-on-display-size?)
  (inherit on-change
           on-local-event
           on-local-char
           free-old-copies
           install-copy-buffer
           begin-copy-buffer
           end-copy-buffer
           do-buffer-paste
           copy-ring-next
           do-write-headers-footers
           do-set-caret-owner
           perform-undo-list
           s-start-intercept
           s-end-intercept
           do-own-x-selection
           copy-out-x-selection
           add-undo-rec
           set-modified
           get-default-style
           get-snip-data
           set-snip-data
           read-snips-from-file
           on-paint
           on-focus
           default-style-name
           wait-sequence-lock
           begin-sequence-lock
           end-sequence-lock
           do-own-caret
           on-edit-sequence
           after-edit-sequence
           on-display-size)
  
  (define read-locked? #t)
  (define flow-locked? #t)
  (define write-locked? #t)

  (define hilite-on? #t)

  (define changed? #f) ;; set if on-change() needs to be called 

  (define flash? #f)
  (define flashautoreset? #f)
  (define flashdirectoff? #f)

  (define posateol? #f) ;; display the caret at the end of a line?
  (define flashposateol? #f)
  (define flashscroll? #f) ;; scroll back after unflashing?

  (define graphics-invalid? #f)
  (define flow-invalid? #f)
  (define snip-cache-invalid? #f)
  (define graphic-maybe-invalid? #f)
  (define graphic-maybe-invalid-force? #f)

  (define typing-streak? #f)
  (define deletion-streak? #f)
  (define delayed-streak? #f)
  (define vcursor-streak? #f)
  (define kill-streak? #f)
  (define anchor-streak? #f)
  (define extend-streak? #f)
  (define insert-force-streak? #f)
  (define delete-force-streak? #f)

  (define keep-anchor-streak? #f)

  (define streaks-pushed? #f)
  (define save-typing-streak? #f)
  (define save-deletion-streak? #f)
  (define save-delayed-streak? #f)
  (define save-vcursor-streak? #f)
  (define save-kill-streak? #f)
  (define save-anchor-streak? #f)
  (define save-extend-streak? #f)

  (define dragging? #f)
  (define tracking? #f)
  (define extra-line? #f) ;; empty line at end of file with no representative

  (define delayedscrollateol? #f)
  (define delayedscrollbox? #f)
  (define draw-cached-in-bitmap? #f)
  (define refresh-unset? #f)
  (define refresh-box-unset? #f)
  (define refresh-all? #f)

  (define tab-space-in-units? #f)
  (define sticky-styles? #t)
  (define overwrite-mode? #f)

  (define prev-mouse-snip #f)

  (def/public (set-styles-sticky [bool? s?]) (set! sticky-styles? (and s? #t)))
  (def/public (get-styles-sticky) sticky-styles?)

  (def/public (get-overwrite-mode) overwrite-mode?)
  (def/public (set-overwrite-mode [bool? v]) (set! overwrite-mode? (and v #t)))

  (def/public (get-sticky-styles) sticky-styles?)
  (def/public (set-sticky-styles [bool? v]) (set! sticky-styles? (and v #t)))

  (define need-x-copy? #f)

  (define caret-blinked? #f) ;; whether we want to hide an active caret or not

  (define initial-style-needed? #t)

  (define last-draw-caret 0)
  (define last-draw-x-sel? #f)

  (define max-width 0.0)
  (define min-width 0.0)
  (define max-height 0.0)
  (define min-height 0.0)
  (define wrap-bitmap-width 0.0)

  (define max-line-width 0.0)
  
  (define padding-l 0.0) ; space conceptually at the left of each line,
  (define padding-t 0.0) ; space conceptually added to the top of the first line,
  (define padding-r 0.0) ; etc. --- locations in mline do not take this
  (define padding-b 0.0) ;          padding into account

  (define auto-wrap-bitmap #f)

  (define delay-refresh 0)

  (define len 0) ; total length in "characters" == number of positions - 1

  (define startpos 0)
  (define endpos 0)
  (define extendstartpos 0)
  (define extendendpos 0) ; for extendstreak
  (define vcursorloc 0.0) ; for vcursor-streak

  (define flash-timer #f)
  (define flashstartpos 0)
  (define flashendpos 0)

  (define snips #f)
  (define last-snip #f) ; the contents of this edit session
  (define snip-count 0)

  (define snip-admin (new standard-snip-admin% [editor this]))
  
  (define line-root-box (box #f))
  (define first-line #f)
  (define last-line #f)
  (define num-valid-lines 0)

  (define extra-line-h 0.0)

  (define total-height 0.0) ; total height/width in canvas units, not including padding
  (define total-width 0.0)
  (define final-descent 0.0) ; descent of last line
  (define initial-space 0.0) ; space from first line
  (define initial-line-base 0.0) ; inverse descent from first line
  (define reported-padding (vector 0.0 0.0 0.0 0.0))

  (define/public (get-s-snips) snips)
  (define/public (get-s-last-snip) last-snip)
  (define/public (get-s-total-width) total-width)
  (define/public (get-s-total-height) total-height)

  (define/public (consistent-snip-lines who)
    (unless (eq? first-line (mline-first (unbox line-root-box)))
      (error who "bad first line"))
    (unless (eq? last-line (mline-last (unbox line-root-box)))
      (error who "bad last line"))
    (let loop ([line first-line]
               [snip snips]
               [snip-num 0])
      (unless (eq? snips (mline-snip first-line))
        (error who "bad start snip"))
      (let sloop ([snip snip][snip-num snip-num])
        (when (zero? (snip->count snip))
          (unless (zero? len)
            (error who "snip count is 0 at ~s" snip-num)))
        (unless (eq? line (snip->line snip))
          (error who "snip's line is wrong: ~s ~s" snip (snip->line snip)))
        (if (eq? snip (mline-last-snip line))
            (if (mline-next line)
                (begin
                  (unless (has-flag? (snip->flags snip) NEWLINE)
                    (error who "strange line ending"))
                  (loop (mline-next line) (snip->next snip) (add1 snip-num)))
                (unless (eq? last-snip snip)
                  (error who "bad last snip")))
            (begin
              (when (or (has-flag? (snip->flags snip) NEWLINE)
                        (has-flag? (snip->flags snip) HARD-NEWLINE))
                (error who "mid-line NEWLINE"))
              (sloop (snip->next snip) (add1 snip-num))))))
    #t)

  (define caret-style #f)

  (define dragstart 0)

  (define track-clickback #f)

  (define refresh-start 0)
  (define refresh-end 0)
  (define refresh-l 0.0)
  (define refresh-t 0.0)
  (define refresh-r 0.0) ; can be 'display-end
  (define refresh-b 0.0) ; can be 'display-end

  (define refresh-box-lock (make-semaphore 1)) ; protects refresh-{l,t,r,b} and refresh-box-unset?

  (define last-draw-l 0.0)
  (define last-draw-t 0.0)
  (define last-draw-r 0.0)
  (define last-draw-b 0.0)
  (define last-draw-red 0)
  (define last-draw-green 0)
  (define last-draw-blue 0)

  (define delayedscroll -1)
  (define delayedscrollend 0)
  (define delayedscrollbias 'none)
  (define delayedscrollsnip #f)
  (define delayedscroll-x 0.0)
  (define delayedscroll-y 0.0)
  (define delayedscroll-w 0.0)
  (define delayedscroll-h 0.0)

  (define clickbacks null)

  (define file-format 'standard)

  (define between-threshold 2.0)

  (define tab-space TAB-WIDTH) ; inexact
  
  (define read-insert 0)
  (define read-insert-start 0)

  (define prev-paste-start 0)
  (define prev-paste-end 0)
  (define save-prev-paste-start 0)
  (define save-prev-paste-end 0)

  (define revision-count 0.0)

  (define word-break standard-wordbreak)
  (define word-break-map the-editor-wordbreak-map)

  (define offscreen-key (gensym))

  (init [(ls line-spacing) 1.0]
        [tab-stops null]
        [auto-wrap #f])

  (super-new)

  (define line-spacing ls)
  (define/public (get-s-line-spacing) line-spacing)
  (define tabs (list->vector tab-stops))
  
  (make-only-snip)
  
  (set! read-locked? #f)
  (set! flow-locked? #f)
  (set! write-locked? #f)
  ;;; from here on, it is only method definitions, 
  ;;; so we can unlock the editor now. If code with
  ;;; effects is added below, be sure to move the
  ;;; unlocking.
  
  (def/override (~)
    (set! word-break-map standard-wordbreak)
    (let loop ([snip snips])
      (when snip
        (let ([next (snip->next snip)])
          (send snip ~)
          (loop next))))
    (set! snips #f)
    (set! clickbacks null))

  (def/override (copy-self)
    (let ([m (new text% [line-spacing line-spacing])])
      (copy-self-to m)
      m))

  (def/override (copy-self-to [editor<%> m])
    (when (m . is-a? . text%)
      ;; copy parameters, such as tab settings: */
      (send m set-tabs (vector->list tabs) tab-space tab-space-in-units?)
      (super copy-self-to m)
      (when (zero? (send m last-position))
        ;; make sure only snip in m has a good style (since we called
        ;; (send m->style-list copy) in copy-self-to).
        (let* ([sname (default-style-name)]
               [bs (send (send m get-s-style-list) find-named-style sname)])
          (set-snip-style! (send m get-s-snips) 
                           (or bs
                               (send (send m get-s-style-list) basic-style)))))

      (send m set-file-format (get-file-format))
      
      (send m set-wordbreak-func word-break)
      (send m set-wordbreak-map (get-wordbreak-map))
      (send m set-between-threshold (get-between-threshold))
      (send m hide-caret (caret-hidden?))
      (send m set-overwrite-mode (get-overwrite-mode))

      (send m set-autowrap-bitmap auto-wrap-bitmap)

      (send m set-sticky-styles sticky-styles?)))

  ;; ----------------------------------------

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
                (if tracking?
                    (or s-custom-cursor arrow)
                    (if (too-busy-to-refresh?)
                        ;; we're too busy; ask again later
                        (or (and s-custom-cursor-overrides? s-custom-cursor)
                            i-beam)
                        (begin
                          (begin-sequence-lock)
                          (begin0
                           (or (and (not s-custom-cursor-overrides?)
                                    (or (and s-caret-snip (send event dragging?)
                                             (let-boxes ([x 0.0]
                                                         [y 0.0])
                                                 (get-snip-position-and-location s-caret-snip #f x y)
                                               (let ([c (send s-caret-snip adjust-cursor dc
                                                              (- x scrollx) (- y scrolly)
                                                              x y event)])
                                                 c)))
                                        ;; find snip:
                                        (let-boxes ([onit? #f]
                                                    [how-close 0.0]
                                                    [pos 0])
                                            (set-box! pos (find-position x y #f onit? how-close))
                                          ;; FIXME: the following refinement of `onit?' seems pointless
                                          (let ([onit? (and onit?
                                                            (not (zero? how-close))
                                                            ((abs how-close) . > . between-threshold))])
                                            (let ([snip (and onit?
                                                             (do-find-snip pos 'after))])
                                              (and snip
                                                   (let-boxes ([x 0.0] [y 0.0])
                                                       (get-snip-position-and-location snip #f x y)
                                                     (let ([c (send snip adjust-cursor dc (- x scrollx) (- y scrolly) 
                                                                    x y event)])
                                                       c))))))))
                               s-custom-cursor
                               (if (x . >= . 0)
                                   (let ([cb? (find-clickback (find-position x y #f) y)])
                                     (if cb? arrow i-beam))
                                   i-beam))
                           (end-sequence-lock))))))))))

  (def/override (on-event [mouse-event% event])
    (when s-admin
      (when (and (not (send event moving?))
                 (not (send event entering?))
                 (not (send event leaving?)))
        (end-streaks '(except-key-sequence cursor delayed)))
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
          (let ([snip
                 (let-boxes ([onit? #f]
                             [how-close 0.0]
                             [now 0])
                     (set-box! now (find-position x y #f onit? how-close))
                   ;; FIXME: the following refinement of `onit?' seems pointless
                   (let ([onit? (and onit?
                                     (not (zero? how-close))
                                     ((abs how-close) . > . between-threshold))])
                     (if onit?
                         ;; we're in the snip's horizontal region...
                         (let ([snip (do-find-snip now 'after)])
                           ;; ... but maybe the mouse is above or below it.
                           (let-boxes ([top 0.0]
                                       [bottom 0.0]
                                       [dummy 0.0])
                               (begin
                                 (get-snip-location snip dummy top #f)
                                 (get-snip-location snip dummy bottom #t))
                             (if (or (top . > . y) (y . > . bottom))
                                 #f
                                 snip)))
                         #f)))])
            (when (send event button-down?)
              (set-caret-owner snip))
            (when (and prev-mouse-snip
                       (not (eq? snip prev-mouse-snip)))
              (let-boxes ([x 0.0] [y 0.0])
                  (get-snip-position-and-location prev-mouse-snip #f x y)
                (send prev-mouse-snip on-event dc (- x scrollx) (- y scrolly) x y event)))
            (set! prev-mouse-snip #f)
            (if (and s-caret-snip (has-flag? (snip->flags s-caret-snip) HANDLES-EVENTS))
                (let-boxes ([x 0.0] [y 0.0])
                    (get-snip-position-and-location s-caret-snip #f x y)
                  (send s-caret-snip on-event dc (- x scrollx) (- y scrolly) x y event))
                (begin
                  (when (and snip
                             (has-flag? (snip->flags snip) HANDLES-ALL-MOUSE-EVENTS))
                    (let-boxes ([x 0.0] [y 0.0])
                        (get-snip-position-and-location snip #f x y)
                      (set! prev-mouse-snip snip)
                      (send snip on-event dc (- x scrollx) (- y scrolly) x y event)))
                  (on-local-event event)))))))

  (def/override (on-default-event [mouse-event% event])
    (when s-admin
      (let-boxes ([scrollx 0.0]
                  [scrolly 0.0]
                  [dc #f])
          (set-box! dc (send s-admin get-dc scrollx scrolly))
        (let ([x (+ (send event get-x) scrollx)]
              [y (+ (send event get-y) scrolly)])
          (when dc

            (let-boxes ([now 0]
                        [ateol? #f]
                        [how-close 0.0])
                (set-box! now (find-position x y ateol? #f how-close))
              (let ([now (if (and (how-close . > . 0)
                                  (how-close . <= . between-threshold))
                             (add1 now)
                             now)])
                (cond
                 [(send event button-down?)
                  (set! tracking? #f)
                  (let ([click (and (x . >= . 0) (find-clickback now y))])
                    (if click
                        (if (clickback-call-on-down? click)
                            ((clickback-f click) this (clickback-start click) (clickback-end click))
                            (begin
                              (set! tracking? #t)
                              (set! track-clickback click)
                              (when s-admin
                                (send s-admin update-cursor))
                              (set-clickback-hilited?! track-clickback #t)))
                        (begin
                          (set! dragstart now)
                          (set! dragging? #t)
                          (when (send event get-shift-down)
                            (if (dragstart . > . startpos)
                                (set! dragstart startpos)
                                (set! dragstart endpos)))
                          (if (now . < . dragstart)
                              (set-position-bias-scroll 'start-only now dragstart ateol?)
                              (set-position-bias-scroll 'end-only dragstart now ateol?)))))]
                 [(send event dragging?)
                  (cond
                   [dragging?
                    (if (now . < . dragstart)
                        (when (or (not (= startpos now)) (not (= endpos dragstart)))
                          (set-position-bias-scroll 'start-only now dragstart ateol?))
                        (when (or (not (= endpos now)) (not (= startpos dragstart)))
                          (set-position-bias-scroll 'end-only dragstart now ateol?)))]
                   [tracking?
                    (let ([cb (if (x . >= . 0)
                                  (find-clickback now y)
                                  #f)])
                      (set-clickback-hilited?! track-clickback (eq? cb track-clickback)))])]
                 [(send event button-up?)
                  (cond
                   [dragging?
                    (set! dragging? #f)]
                   [tracking?
                    (set! tracking? #f)
                    (when (clickback-hilited? track-clickback)
                      (set-clickback-hilited?! track-clickback #f)
                      (let ([click track-clickback])
                        ((clickback-f click) this (clickback-start click) (clickback-end click))))
                    (when s-admin
                      (send s-admin update-cursor))])]
                 [(send event moving?)
                  (set! dragging? #f)
                  (when tracking?
                    (set! tracking? #f)
                    (when (clickback-hilited? track-clickback)
                      (set-clickback-hilited?! track-clickback #f)
                      (let ([click track-clickback])
                        ((clickback-f click) this (clickback-start click) (clickback-end click)))))
                  (when s-admin
                    (send s-admin update-cursor))]))))))))

  (def/override (on-char [key-event% event])
    (when s-admin
      (if (and s-caret-snip
               (has-flag? (snip->flags s-caret-snip) HANDLES-EVENTS))
          (let-boxes ([scrollx 0.0]
                      [scrolly 0.0]
                      [dc #f])
              (set-box! dc (send s-admin get-dc scrollx scrolly))
            (let-boxes ([x 0.0] [y 0.0])
                (get-snip-position-and-location s-caret-snip #f x y)
              (send s-caret-snip on-char dc (- x scrollx) (- y scrolly) x y event)))
          (let ([code (send event get-key-code)])
            (when (and (not (eq? 'release code))
                       (not (eq? 'shift code))
                       (not (eq? 'control code))
                       (not (eq? 'menu code))
                       (not (equal? code #\nul)))
              (hide-cursor))
            (on-local-char event)))))

  (def/override (on-default-char [key-event% event])
    (when s-admin
      (let ([code (send event get-key-code)]
            [ins (lambda (ch)
                   (if (and overwrite-mode? (= endpos startpos))
                       (insert ch startpos (add1 startpos))
                       (insert ch)))])
        (case code
          [(#\backspace) (delete)]
          [(#\rubout)
           (if (= endpos startpos)
               (when (endpos . < . len)
                 (delete endpos (add1 endpos)))
               (delete))]
          [(right left up down home end prior next)
           (move-position code (send event get-shift-down))]
          [(numpad0) (ins #\0)]
          [(numpad1) (ins #\1)]
          [(numpad2) (ins #\2)]
          [(numpad3) (ins #\3)]
          [(numpad4) (ins #\4)]
          [(numpad5) (ins #\5)]
          [(numpad6) (ins #\6)]
          [(numpad7) (ins #\7)]
          [(numpad8) (ins #\8)]
          [(numpad9) (ins #\9)]
          [(multiply) (ins #\*)]
          [(divide) (ins #\/)]
          [(add) (ins #\+)]
          [(subtract) (ins #\-)]
          [(decimal) (ins #\.)]
          [(#\u3) (ins #\return)] ; NUMPAD-ENTER
          [(#\return #\tab) (ins code)]
          [else
           (let ([vcode (if (char? code)
                            (char->integer code)
                            0)])
             (when (and (vcode . >= . 32)
                        (or (vcode . <= . #xd800)
                            (vcode . > . #xdf00)))
               (ins code)))]))))
  
  (def/override (own-caret [any? ownit?])
    (when (do-own-caret (and ownit? #t))
      (need-caret-refresh)
      (on-focus (and ownit? #t))))

  (def/override (blink-caret)
    (if s-caret-snip
        (when s-admin
          (let-boxes ([dx 0.0]
                      [dy 0.0]
                      [dc #f])
              (set-box! dc (send s-admin get-dc dx dy))
            (when dc
              (let-boxes ([x 0.0] [y 0.0])
                  (get-snip-location s-caret-snip x y)
                (send s-caret-snip blink-caret dc (- x dx) (- y dy))))))
        (if (too-busy-to-refresh?)
            ;; we're busy; go away
            (void)
            (when (and (= endpos startpos) 
                       (not flash?)
                       hilite-on?)
              (set! caret-blinked? (not caret-blinked?))
              (need-caret-refresh)))))

  (def/override (size-cache-invalid)
    (set! graphic-maybe-invalid? #t)
    (set! graphics-invalid? #t)
    (when (max-width . > . 0.0)
      (set! flow-invalid? #t))
    (set! snip-cache-invalid? #t))

  (def/override-final (locked-for-read?)
    read-locked?)
  (def/override-final (locked-for-flow?)
    flow-locked?)
  (def/override-final (locked-for-write?)
    write-locked?)

  ;; ----------------------------------------

  (def/public (can-insert? [exact-nonnegative-integer? start]
                           [exact-nonnegative-integer? len])
    #t)
  (def/public (on-insert [exact-nonnegative-integer? start]
                         [exact-nonnegative-integer? len])
    (void))
  (def/public (after-insert [exact-nonnegative-integer? start]
                            [exact-nonnegative-integer? len])
    (void))

  (def/public (can-delete? [exact-nonnegative-integer? start]
                           [exact-nonnegative-integer? len])
    #t)
  (def/public (on-delete [exact-nonnegative-integer? start]
                         [exact-nonnegative-integer? len])
    (void))
  (def/public (after-delete [exact-nonnegative-integer? start]
                            [exact-nonnegative-integer? len])
    (void))

  (def/public (can-change-style? [exact-nonnegative-integer? start]
                                 [exact-nonnegative-integer? len])
    #t)
  (def/public (on-change-style [exact-nonnegative-integer? start]
                               [exact-nonnegative-integer? len])
    (void))
  (def/public (after-change-style [exact-nonnegative-integer? start]
                                  [exact-nonnegative-integer? len])
    (void))

  (def/public (after-set-position) (void))

  (def/public (can-set-size-constraint?) #t)
  (def/public (on-set-size-constraint) (void))
  (def/public (after-set-size-constraint) (void))

  (def/public (after-split-snip [exact-nonnegative-integer? pos]) (void))
  (def/public (after-merge-snips [exact-nonnegative-integer? pos]) (void))

  ;; ----------------------------------------

  (def/override (begin-edit-sequence [any? [undoable? #t]] [any? [interrupt-seqs? #t]])
    (define ready! (wait-sequence-lock))

    (when (and (zero? delay-refresh)
               (not interrupt-seqs?))
      (push-streaks))

    (end-streaks '(delayed))

    (when (or (positive? s-noundomode)
              (not undoable?))
      (set! s-noundomode (add1 s-noundomode)))
    
    (if (zero? delay-refresh)
        (begin
          (when ALLOW-X-STYLE-SELECTION?
            (set! need-x-copy? #t))
          (set! delay-refresh 1)
          (on-edit-sequence))
        (set! delay-refresh (add1 delay-refresh)))
    
    (ready!))

  (def/override (end-edit-sequence)
    (if (zero? delay-refresh)
        (log-error "end-edit-sequence without begin-edit-sequence")
        (let ([new-delay-refresh (sub1 delay-refresh)])
          (cond
           [(zero? new-delay-refresh)
            (end-streaks null)
            (pop-streaks)
            (parameterize ([in-delayed-refresh #t])
              (redraw))
            (when s-need-on-display-size?
              (set! s-need-on-display-size? #f)
              (on-display-size))
            (set! delay-refresh 0)
            (when ALLOW-X-STYLE-SELECTION?
              (set! need-x-copy? #f))
            (after-edit-sequence)]
           [else
            (set! delay-refresh new-delay-refresh)])
          (when (positive? s-noundomode)
            (set! s-noundomode (sub1 s-noundomode))))))

  (def/override (refresh-delayed?)
    (or (and (delay-refresh . > . 0)
             (not (in-delayed-refresh)))
        (not s-admin)
        (send s-admin refresh-delayed?)))

  (def/override-final (in-edit-sequence?)
    (and (delay-refresh . > . 0)
         (not (in-delayed-refresh))))

  (def/override (locations-computed?)
    (not graphic-maybe-invalid?))

  (def/public (recalculate) (void))
  
  (def/public (get-position [maybe-box? start] [maybe-box? [end #f]])
    (when start (set-box! start startpos))
    (when end (set-box! end endpos)))
  
  (def/public (get-start-position) startpos)
  (def/public (get-end-position) endpos)

  (def/public (set-position [exact-nonnegative-integer? start]
                            [(make-alts exact-nonnegative-integer? (make-literal 'same)) [end 'same]]
                            [any? [ateol? #f]]
                            [any? [scroll? #t]]
                            [(symbol-in default x local) [seltype 'default]])
    (do-set-position #f 'none start end ateol? scroll? seltype #f))

  (def/public (set-position-bias-scroll [symbol? bias]
                                        [exact-nonnegative-integer? start]
                                        [(make-alts exact-nonnegative-integer? (make-literal 'same)) [end 'same]]
                                        [any? [ateol? #f]]
                                        [any? [scroll? #t]]
                                        [(symbol-in default x local) [seltype 'default]])
    (do-set-position #f bias start end ateol? scroll? seltype #f))

  (define/private (do-set-position setflash? bias start end ateol? scroll? seltype dont-end-cursor?)
    (unless flow-locked?
      (when (and (not setflash?)
                 (or (not flash?) (not flashautoreset?) (not flashdirectoff?)))
        (end-streaks (if dont-end-cursor? '(cursor delayed) '(delayed))))
      
      (unless (or (start . < . 0) 
                  (and (number? end)
                       (start . > . end)))
        (let* ([start (min start len)]
               [end (if (symbol? end)
                        start
                        (min end len))]
               [ateol?
                (and ateol?
                     (= end start)
                     (let-values ([(snip s-pos)
                                   (find-snip/pos start 'before)])
                       (and (has-flag? (snip->flags snip) NEWLINE) 
                            (not (has-flag? (snip->flags snip) INVISIBLE))
                            (= start (+ s-pos (snip->count snip))))))])
          (let-values ([(oldstart oldend oldateol?)
                        (if flash?
                            (values flashstartpos flashendpos flashposateol?)
                            (values startpos endpos posateol?))])
            (when (and (not setflash?)
                       flash?
                       flashautoreset?)
              (set! flash? #f)
              (when flash-timer
                (send flash-timer stop)
                (set! flash-timer #f)))
            (let* ([need-refresh? (not (and (= oldstart start)
                                            (= oldend end)
                                            (eq? oldateol? ateol?)))]
                   [changed-pos? need-refresh?])

              (if setflash?
                  (begin
                    (set! flashstartpos start)
                    (set! flashendpos end)
                    (set! flashposateol? ateol?))
                  (begin
                    (when ALLOW-X-STYLE-SELECTION?
                      (when (or (= end start)
                                (not (eq? editor-x-selection-allowed this))
                                (eq? 'local seltype))
                        (when (or (not (in-edit-sequence?))
                                  need-x-copy?)
                          (set! need-x-copy? #f)
                          (copy-out-x-selection))))
                    
                    (check-merge-snips startpos)
                    (check-merge-snips endpos)
    
                    (set! caret-style #f)
                    (set! startpos start)
                    (set! endpos end)
                    (set! posateol? ateol?)))

              (let-values ([(need-refresh? need-full-refresh?)
                            (let ([refresh? (and ALLOW-X-STYLE-SELECTION?
                                                 (not setflash?)
                                                 editor-x-selection-mode?
                                                 (or (and (not (eq? 'local seltype))
                                                          (not (= start end ))
                                                          (not (eq? editor-x-selection-owner this))
                                                          (eq? (own-x-selection #t #f seltype) 'x))
                                                     (and (or (= start end)
                                                              (not (eq? editor-x-selection-allowed this))
                                                              (eq? 'local seltype))
                                                          (eq? editor-x-selection-owner this)
                                                          (own-x-selection #f #f #f))))])
                              (values (or refresh? need-refresh?)
                                      refresh?))])
                (when setflash?
                  (set! flash? #t))

                (let ([need-refresh?
                       (or
                        (and scroll?
                             (let-values ([(scroll-start scroll-end bias)
                                           (cond
                                            [(eq? bias 'start-only)
                                             (values start start 'none)]
                                            [(eq? bias 'end-only)
                                             (values end end 'none)]
                                            [else
                                             (values start end bias)])])
                               (let ([was-blinked? caret-blinked?])
                                 (set! caret-blinked? #f)
                                 (if (scroll-to-position/refresh scroll-start posateol? #t scroll-end bias)
                                     #t
                                     (begin
                                       (set! caret-blinked? was-blinked?)
                                       #f)))))
                        need-refresh?)])

                  (when need-refresh?
                    (set! caret-blinked? #f)
                    (if (or (start . >= . oldend)
                            (end . <= . oldstart)
                            need-full-refresh?)
                        (begin
                          ;; no overlap:
                          (need-refresh oldstart oldend)
                          (need-refresh start end))
                        (begin
                          (when (start . < . oldstart)
                            (need-refresh start oldstart))
                          (when (oldstart . < . start)
                            (need-refresh oldstart start))
                          (when (end . < . oldend)
                            (need-refresh end oldend))
                          (when (oldend . < . end)
                            (need-refresh oldend end)))))))

              (when (and changed-pos? (not setflash?))
                (after-set-position))))))))

  (define/private (scroll-to-position/refresh start
                                              [ateol? #f]
                                              [refresh? #t]
                                              [end 'same]
                                              [bias 'none])
    (and 
     (not flow-locked?)
     (let ([end (if (eq? end 'same) start (max start end))])
       (cond
        [(in-edit-sequence?)
         (when s-admin
           (set! delayedscrollbox? #f)
           (set! delayedscroll start)
           (set! delayedscrollend end)
           (set! delayedscrollateol? ateol?)
           (set! delayedscrollbias bias))
         #f]
        [(not (check-recalc #t #f))
         #f]
        [else
         (set! delayedscroll -1)
  
         (let-boxes ([topx 0.0] [topy 0.0]
                     [botx 0.0] [boty 0.0])
             (begin
               (position-location start topx topy #t ateol? #t)
               (position-location end botx boty #f ateol? #t))
           (let-values ([(topx botx)
                         (if (botx . < . topx)
                             ;; when the end position is to the left of the start position 
                             (values 0 (+ total-width padding-t padding-b))
                             (values topx botx))])
             (scroll-editor-to topx topy (- botx topx) (- boty topy) refresh? bias)))]))))

  (def/public (scroll-to-position [exact-nonnegative-integer? start]
                                  [any? [ateol? #f]]
                                  [(make-alts exact-nonnegative-integer? (make-literal 'same)) [end 'same]]
                                  [(symbol-in start end none) [bias 'none]])
    (scroll-to-position/refresh start ateol? #t end bias))

  (define/private (get-visible-X-range start end all? find)
    (when (check-recalc #t #f)
      (let-boxes ([x 0.0] [y 0.0] [w 0.0] [h 0.0])
          (if all?
              (send s-admin get-max-view x y w h)
              (send s-admin get-view x y w h))
        (begin
          (when start
            (set-box! start (find x y)))
          (when end
            (set-box! end (find (+ x w) (+ y h))))))))

  (def/public (get-visible-position-range [maybe-box? start] [maybe-box? end] [any? [all? #t]])
    (get-visible-X-range start end all? (lambda (x y) (find-position x y))))

  (def/public (get-visible-line-range [maybe-box? start] [maybe-box? end] [any? [all? #t]])
    (get-visible-X-range start end all? (lambda (x y) (find-line y))))

  ;; ----------------------------------------

  (def/public (extend-position [exact-nonnegative-integer? dest])
    (cond
      [extend-streak?
       (values extendstartpos extendendpos)]
      [anchor-streak?
       (set! extend-streak? #t)
       (values extendstartpos extendendpos)]
      [else
       (set! extend-streak? #t)
       (set! extendstartpos startpos)
       (set! extendendpos endpos)])

    (define-values (start end bias)
      (cond
        [(dest . < . extendstartpos)
         (values dest extendendpos 'start)]
        [(dest . > . extendendpos)
         (values extendstartpos dest 'end)]
        [else
         (values extendstartpos extendendpos 'none)]))
    (do-set-position #f bias start end #f #t 'default #t))
  
  (def/public (move-position [(make-alts symbol? char?) code]
                             [any? [extend-selection? #f]]
                             [(symbol-in simple word page line) [kind 'simple]])
    (unless (or flow-locked?
                (not (check-recalc (max-width . > . 0.0) #f #t)))

      (let-values ([(anchor?) anchor-streak?]
                   [(vcursor?) vcursor-streak?]
                   [(extendstart extendend)
                    (if (or extend-streak? anchor-streak?)
                        (values extendstartpos extendendpos)
                        (values startpos endpos))]
                   [(kas?) keep-anchor-streak?])

        (set! keep-anchor-streak? anchor-streak?)
        
        (end-streaks '(delayed))

        (let* ([extend? (or anchor? extend-selection?)]
               ;; rightshrink: motion to right shrinks the selected region
               [rightshrink? (and extend? (startpos . < . extendstart))]
               [leftshrink? (and extend? (endpos . > . extendend))])
          (let-values ([(code kind)
                        (cond
                         [(eq? 'prior code) (values 'up 'page)]
                         [(eq? 'next code) (values 'down 'page)]
                         [else (values code kind)])])
            (cond
             [(eq? 'home code)
              (if leftshrink?
                  (set-position-bias-scroll 'start-only extendstart extendend)
                  (set-position-bias-scroll 'start-only 0 (if extend? extendend 0)))]
             [(eq? 'end code)
              (if rightshrink?
                  (set-position-bias-scroll 'end-only extendstart extendend)
                  (set-position-bias-scroll 'end-only (if extend? extendstart len) len))]
             [(eq? 'left code)
              (if (and (not (eq? 'line kind))
                       (not (eq? 'word kind))
                       (not extend?)
                       (not (= endpos startpos)))
                  (set-position startpos)
                  (begin
                    ;; pick a starting place
                    (let ([start 
                           (let ([start (if leftshrink?
                                            endpos
                                            startpos)])
                             (cond
                              [(eq? 'word kind)
                               (let-boxes ([start start])
                                   (find-wordbreak start #f 'caret)
                                 start)]
                              [(eq? 'line kind)
                               (line-start-position (position-line start posateol?))]
                              [else (max 0 (sub1 start))]))])
                      (let-values ([(start end)
                                    (if extend?
                                        (if leftshrink?
                                            (let ([start (max start extendend)]) ;; collapse to original
                                              (values startpos start))
                                            (values start endpos))
                                        (values start start))])
                        (set-position-bias-scroll 'start-only start end)))))]
             [(eq? 'right code)
              (if (and (not (eq? 'line kind))
                       (not (eq? 'word kind))
                       (not extend?)
                       (not (= endpos startpos)))
                  (set-position endpos endpos #t)
                  (begin
                    ;; pick a starting place
                    (let ([end
                           (let ([end (if rightshrink?
                                          startpos
                                          endpos)])
                             (cond
                              [(eq? 'word kind)
                               (let-boxes ([end end])
                                   (find-wordbreak #f end 'caret)
                                 end)]
                              [(eq? 'line kind)
                               (line-end-position (position-line end posateol?))]
                              [else (add1 end)]))])
                      (let-values ([(start end)
                                    (if extend?
                                        (if rightshrink?
                                            (let ([end (min end extendstart)]) ;; collapse to original
                                              (values end endpos))
                                            (values startpos end))
                                        (values end end))])
                        (set-position-bias-scroll 'end-only start end #t)))))]
             [(or (eq? 'up code) (eq? 'down code))
              (let ([special-scroll? (eq? 'page kind)]) ;; used when paging
                (let-values ([(start end ateol? special-scroll?
                                     scroll-left scroll-top scroll-width scroll-height
                                     bias)
                              (if (eq? 'up code)
                                  (let ([start (if leftshrink?
                                                   endpos
                                                   startpos)])
                                    (let-boxes ([vcl vcursorloc])
                                        (when (not vcursor?)
                                          (position-location start vcl #f #t posateol? #t))
                                      (set! vcursorloc vcl)
                                      (let ([cline (position-line start posateol?)])
                                        (let-values ([(i scroll-left scroll-top scroll-width scroll-height)
                                                      (if (eq? 'page kind)
                                                          ;; the current top line should become the next-to bottom line.
                                                          ;; the caret should go to line above current top line, but
                                                          ;; watch out for:
                                                          ;;   - especially tall lines
                                                          ;;   - already at top
                                                          (let-boxes ([scroll-left 0.0] [vy 0.0]
                                                                      [scroll-width 0.0] [scroll-height 0.0])
                                                              (when s-admin
                                                                (send s-admin get-view scroll-left vy scroll-width scroll-height))
                                                            ;; top line should be completely visible as bottom line after
                                                            ;;   scrolling
                                                            (let* ([top (find-scroll-line vy)]
                                                                   [ty (scroll-line-location (+ top 1))]
                                                                   [newtop (find-scroll-line (- ty scroll-height))]
                                                                   [y (scroll-line-location newtop)]
                                                                   [newtop (if (y . < . (- ty scroll-height))
                                                                               (add1 newtop)
                                                                               newtop)]
                                                                   [y (scroll-line-location newtop)]
                                                                   ;; y is the new top location
                                                                   [y (if (y . >= . vy)
                                                                          ;; no or backward progess
                                                                          (scroll-line-location (max 0 (sub1 top)))
                                                                          y)])
                                                              (let ([i (if (= vy y)
                                                                           ;; must be at the top:
                                                                           (find-line y)
                                                                           (let ([i (find-line (+ y scroll-height))])
                                                                             (if ((line-location (max 0 (- i 1))) . > . y)
                                                                                 (sub1 i)
                                                                                 i)))])
                                                                (values i scroll-left y scroll-width scroll-height))))
                                                          (values (- cline 1) 0.0 0.0 0.0 0.0))])
                                          (let-boxes ([start 0] [ateol? #f])
                                              (if (i . >= . 0)
                                                  (set-box! start (find-position-in-line i vcursorloc ateol?))
                                                  (begin (set-box! start 0) (set-box! ateol? #f)))
                                            (let-values ([(start end special-scroll?)
                                                          (if extend?
                                                              (if leftshrink?
                                                                  (if (start . < . extendend)
                                                                      (if (and (not (eq? 'page kind))
                                                                               (start . < . extendstart))
                                                                          ;; inversion!
                                                                          (values start extendend special-scroll?)
                                                                          ;; Collapse to original
                                                                          (values startpos extendend #f))
                                                                      (values startpos start special-scroll?))
                                                                  (values start endpos special-scroll?))
                                                              (values start start special-scroll?))])
                                              (values start end ateol? special-scroll?
                                                      scroll-left scroll-top scroll-width scroll-height
                                                      (if leftshrink? 'end-only 'start-only))))))))
                                  ;; (eq? code 'down)
                                  (let ([end (if rightshrink?
                                                 startpos
                                                 endpos)])
                                    (let-boxes ([vcl vcursorloc])
                                        (when (not vcursor?)
                                          (position-location end vcl #f #t posateol? #t))
                                      (set! vcursorloc vcl)
                                      (let ([cline (position-line end posateol?)])
                                        (let-values ([(i scroll-left scroll-top scroll-width scroll-height)
                                                      (if (eq? 'page kind)
                                                          (let-boxes ([scroll-left 0.0] [vy 0.0]
                                                                      [scroll-width 0.0] [scroll-height 0.0])
                                                              (when s-admin
                                                                (send s-admin get-view scroll-left vy scroll-width scroll-height))
                                                            ;; last fully-visible line is the new top line 
                                                            (let* ([newtop (find-scroll-line (+ vy scroll-height))]
                                                                   [y (scroll-line-location (+ newtop 1))]
                                                                   [newtop (if (y . > . (+ vy scroll-height))
                                                                               (sub1 newtop)
                                                                               newtop)]
                                                                   [y (scroll-line-location newtop)])
                                                              ;; y is the new top location
                                                              (let-values ([(newtop y)
                                                                            (if (y . <= . vy)
                                                                                ;; no or backwards movement; scroll back one
                                                                                (let ([newtop (+ (find-scroll-line vy) 1)])
                                                                                  (values newtop (scroll-line-location newtop)))
                                                                                (values newtop y))])
                                                                ;; compute top line, for caret
                                                                (let* ([i (find-line y)]
                                                                       [i (if ((line-location i #t) . < . y)
                                                                              (add1 i)
                                                                              i)])
                                                                  ;; Now, suppose we're scrolling down while extending the
                                                                  ;; selection. We want to be able to see that we're
                                                                  ;; selecting. So try moving the line `i' down one more, if
                                                                  ;; there's room:
                                                                  (let ([i (if ((line-location (+ i 1) #f) . < . (+ y scroll-height))
                                                                               (add1 i)
                                                                               i)])
                                                                    (values i scroll-left (- y 1) scroll-width scroll-height))))))
                                                          (values (+ cline 1) 0.0 0.0 0.0 0.0))])
                                          (let-values ([(end ateol?)
                                                        (if (i . <= . (sub1 num-valid-lines))
                                                            (let-boxes ([ateol? #f] [end 0])
                                                                (set-box! end (find-position-in-line i vcursorloc ateol?))
                                                              (values end ateol?))
                                                            (values len #f))])
                                            (let-values ([(start end special-scroll?)
                                                          (if extend?
                                                              (if rightshrink?
                                                                  (if (end . > . extendstart)
                                                                      (if (and (not (eq? 'page kind))
                                                                               (end . > . extendend))
                                                                          ;; inversion!
                                                                          (values extendstart end special-scroll?)
                                                                          ;; collapse to original
                                                                          (values extendstart endpos #f))
                                                                      (values end endpos special-scroll?))
                                                                  (values startpos end special-scroll?))
                                                              (values end end special-scroll?))])
                                              (values start end ateol? special-scroll?
                                                      scroll-left scroll-top scroll-width scroll-height
                                                      (if rightshrink? 'start-only 'end-only)))))))))])
                  (when special-scroll?
                    (begin-edit-sequence))
                  
                  ;; scroll only if !special-scroll
                  (set-position-bias-scroll bias start end ateol? (not special-scroll?))

                  (when special-scroll?
                    ;; special scrolling intructions:
                    (do-scroll-to #f scroll-left scroll-top scroll-width scroll-height #f 'none)

                    (end-edit-sequence))

                  (set! vcursor-streak? #t)))])

            (set! keep-anchor-streak? kas?)
            (when extend?
              (set! extend-streak? #t))

            (when (or extend-streak? anchor-streak?)
              (set! extendendpos extendend)
              (set! extendstartpos extendstart)))))))

  (def/public (set-anchor [any? on?])
    (let ([wason? anchor-streak?])
      (set! anchor-streak? (and on? #t))
      (when (and on? (not wason?))
        (set! extendendpos endpos)
        (set! extendstartpos startpos))))

  (def/public (get-extend-start-position) (if (or extend-streak? anchor-streak?) extendstartpos startpos))
  (def/public (get-extend-end-position) (if (or extend-streak? anchor-streak?) extendendpos endpos))
  
  (def/public (get-anchor)
    anchor-streak?)

  ;; ----------------------------------------

  (define/private (do-insert isnip str snipsl start end scroll-ok?)
    (assert (consistent-snip-lines 'do-insert))
    (unless (or write-locked?
                s-user-locked?
                (start . < . 0))
      (let ([start (min start len)]
            [str (and str (positive? (string-length str)) str)])
        ;; turn off pending style, if it doesn't apply
        (when caret-style
          (when (or (not (equal? end start)) (not (= startpos start)))
            (set! caret-style #f)))
        (let ([deleted? (and (not (eq? end 'same))
                             (start . < . end)
                             (begin
                               (when ALLOW-X-STYLE-SELECTION?
                                 (when (not (in-edit-sequence?))
                                   (set! need-x-copy? #t)))
                               (when (or isnip str snipsl)
                                 (begin-edit-sequence))
                               (delete start end scroll-ok?)
                               (when ALLOW-X-STYLE-SELECTION?
                                 (when (not (in-edit-sequence?))
                                   (set! need-x-copy? #f)))
                               #t))])
          (when (or isnip str snipsl)
            (set! write-locked? #t)
            (let ([success-finish
                   (lambda (addlen inserted-line?)
                     (set! initial-style-needed? #f)
                     (set! revision-count (add1 revision-count))

                     (adjust-clickbacks start start addlen #f)

                     (unless s-modified?
                       (add-undo-rec (make-object unmodify-record% delayed-streak?)))
                     (unless (positive? s-noundomode)
                       (add-undo-rec
                        (make-object insert-record%
                                     start addlen
                                     (or deleted? typing-streak? delayed-streak?
                                         insert-force-streak?
                                         (not s-modified?))
                                     startpos endpos)))
                     (when (in-edit-sequence?)
                       (set! delayed-streak? #t))

                     (let ([scroll? (= start startpos)])

                       (when (startpos . >= . start)
                         (set! startpos (+ startpos addlen)))
                       (when (endpos . >= . start)
                         (set! endpos (+ endpos addlen)))
                       (unless refresh-unset?
                         (when (refresh-start . >= . start)
                           (set! refresh-start (+ refresh-start addlen)))
                         (when (refresh-end . >= . start)
                           (set! refresh-end (+ refresh-end addlen))))

                       (set! extra-line? (has-flag? (snip->flags last-snip) NEWLINE))

                       (set! write-locked? #f)
                       (set! flow-locked? #f)

                       (when scroll?
                         (set! caret-blinked? #f))

                       (when (and scroll? scroll-ok?)
                         (set! delay-refresh (add1 delay-refresh))
                         (parameterize ([in-delayed-refresh #f])
                           (scroll-to-position/refresh startpos))
                         (set! delay-refresh (sub1 delay-refresh)))

                       (set! changed? #t)

                       (set! caret-style #f)

                       (if inserted-line?
                           (begin
                             (set! graphic-maybe-invalid? #t)
                             (need-refresh start))
                           (refresh-by-line-demand))

                       (when deleted?
                         (end-edit-sequence))

                       (unless s-modified?
                         (set-modified #t))

                       (assert (consistent-snip-lines 'pre-after-insert))
                       
                       (after-insert start addlen)))]
                  [fail-finish
                   (lambda ()
                     (set! write-locked? #f)
                     (set! flow-locked? #f)
                     (when deleted?
                       (end-edit-sequence)))])
              (cond
                [(or isnip snipsl) 
                 (insert-snips (if isnip (list isnip) snipsl) start success-finish fail-finish)]
                [else (insert-string str start success-finish fail-finish)])))))
      (assert (consistent-snip-lines 'post-do-insert))))
  
  (define/private (insert-snips snipsl start success-finish fail-finish)
    (let ([addlen (for/fold ([addlen 0])
                      ([isnip (in-list snipsl)]
                       #:when addlen)
                    (let ([c (snip->count isnip)])
                      (and (positive? c)
                           (not (send isnip is-owned?))
                           (+ addlen c))))])

      (if (or (not addlen)
              (zero? addlen)
              (not (can-insert? start addlen)))
          (fail-finish)
          (begin
            (on-insert start addlen)

            (set! flow-locked? #t)

            ;; make sure on-insert didn't do something bad to the snips:
            (if (not (for/and ([isnip (in-list snipsl)])
                       (and (positive? (snip->count isnip))
                            (not (send isnip is-owned?)))))

                (fail-finish)

                (let loop ([did-one? #f]
                           [before-snip #f]
                           [inserted-line? #f]
                           [snipsl snipsl])

                  (if (null? snipsl)
                      (success-finish addlen inserted-line?)
                      (let ([isnip (car snipsl)])
                        (when (and (has-flag? (snip->flags isnip) NEWLINE)
                                   (not (has-flag? (snip->flags isnip) HARD-NEWLINE)))
                          (set-snip-flags! isnip (remove-flag (snip->flags isnip) NEWLINE)))

                        (assert (consistent-snip-lines 'inner-insert))
                                

                        (let-values ([(before-snip inserted-new-line?)
                                      (if (and (zero? len) (not did-one?))
                                          
                                          ;; special case: ignore the empty snip
                                          (begin
                                            (set! snips isnip)
                                            (set! last-snip isnip)
                                            (let ([line-root (create-mline)])
                                              (set-box! line-root-box line-root)
                                              (set-snip-line! isnip line-root)
                                              (set-mline-snip! line-root isnip)
                                              (set-mline-last-snip! line-root isnip)
                                              (when (max-width . > . 0)
                                                (mline-mark-check-flow line-root)))
                                            (values before-snip #f))

                                          (let* ([gsnip (if (not did-one?)
                                                            (begin
                                                              (make-snipset start start)
                                                              (do-find-snip start 'after-or-none))
                                                            before-snip)]
                                                 [before-snip (or before-snip gsnip)]
                                                 [inserted-new-line?
                                                  (if (not gsnip)
                                                      (begin
                                                        (append-snip isnip)
                                                        (let ([gsnip (mline-last-snip last-line)])
                                                          (if (and gsnip (has-flag? (snip->flags gsnip) HARD-NEWLINE))
                                                              (let ([line (mline-insert last-line line-root-box #f)])
                                                                (set-snip-line! isnip line)
                                                                (set-mline-snip! line isnip)
                                                                (set-mline-last-snip! line isnip)
                                                                (set! num-valid-lines (add1 num-valid-lines))
                                                                #t)
                                                              (begin
                                                                ;; The former last snip might still have a NEWLINE
                                                                ;;  flag due to line-flowing
                                                                (when (has-flag? (snip->flags gsnip) NEWLINE)
                                                                  (set-snip-flags! gsnip (remove-flag (snip->flags gsnip) NEWLINE)))
                                                                (set-snip-line! isnip last-line)
                                                                (when (not (mline-snip last-line))
                                                                  (set-mline-snip! last-line isnip))
                                                                (set-mline-last-snip! last-line isnip)
                                                                ;; maybe added extra ghost line:
                                                                (has-flag? (snip->flags isnip) HARD-NEWLINE)))))
                                                      (begin
                                                        (insert-snip gsnip isnip)
                                                        (if (has-flag? (snip->flags isnip) HARD-NEWLINE)
                                                            (let* ([gline (snip->line gsnip)]
                                                                   [line (mline-insert gline line-root-box #t)])
                                                              (set-snip-line! isnip line)
                                                              (set! num-valid-lines (add1 num-valid-lines))
                                                              (if (eq? gsnip (mline-snip gline))
                                                                  (set-mline-snip! line isnip)
                                                                  (set-mline-snip! line (mline-snip gline)))
                                                              (set-mline-last-snip! line isnip)
                                                              (set-mline-snip! gline gsnip)
                                                              
                                                              (let loop ([c-snip (mline-snip line)])
                                                                (unless (eq? c-snip isnip)
                                                                  (set-snip-line! c-snip line)
                                                                  (loop (snip->next c-snip))))
                                                              
                                                              (mline-calc-line-length gline)
                                                              (mline-mark-recalculate gline)
                                                              #t)
                                                            (let ([gline (snip->line gsnip)])
                                                              (set-snip-line! isnip gline)
                                                              (when (eq? (mline-snip gline) gsnip)
                                                                (set-mline-snip! gline isnip))
                                                              #f))))])
                                            
                                            (when (max-width . > . 0)
                                              (mline-mark-check-flow (snip->line isnip))
                                              (let ([prev (snip->prev isnip)])
                                                (when (and prev
                                                           (not (has-flag? (snip->flags isnip) NEWLINE)))
                                                  (mline-mark-check-flow (snip->line prev))))
                                              (let ([next (mline-next (snip->line isnip))])
                                                (when (and next
                                                           (has-flag? (snip->flags isnip) HARD-NEWLINE))
                                                  (mline-mark-check-flow next))))

                                            (values before-snip inserted-new-line?)))])
                          
                          (set-snip-style! isnip (send s-style-list convert (or (snip->style isnip)
                                                                                (send s-style-list basic-style))))
                          
                          (send isnip size-cache-invalid)

                          (mline-calc-line-length (snip->line isnip))
                          (mline-mark-recalculate (snip->line isnip))

                          (set! len (+ len (snip->count isnip)))
                          
                          (snip-set-admin isnip snip-admin)

                          (set! first-line (mline-first (unbox line-root-box)))
                          (set! last-line (mline-last (unbox line-root-box)))

                          (assert (consistent-snip-lines 'inner-insert2))

                          (loop #t
                                before-snip
                                (or inserted-line? inserted-new-line?)
                                (cdr snipsl)))))))))))

  (define/private (insert-string str start success-finish fail-finish)
    (let ([addlen (string-length str)])
      (if (not (can-insert? start addlen))
          (fail-finish)
          (begin
            (on-insert start addlen)      
    
            (set! flow-locked? #t)

            (let-values ([(snip s-pos inserted-line?)
                          (if (zero? len)
                              
                              (let* ([style (if (and sticky-styles?
                                                     (not initial-style-needed?))
                                                (snip->style snips)
                                                (get-default-style))]
                                     [snip (insert-text-snip start style)])
                                (set! caret-style #f)
                                (set-mline-snip! (unbox line-root-box) snip)
                                (set-mline-last-snip! (unbox line-root-box) snip)
                                (values snip 0 #f))

                              (let-values ([(gsnip s-pos)
                                            (if (positive? start)
                                                (find-snip/pos start 'before)
                                                (values #f 0))])
                                (let-values ([(snip s-pos)
                                              (if (or (not gsnip)
                                                      (and caret-style (not (eq? caret-style (snip->style gsnip))))
                                                      (not (has-flag? (snip->flags gsnip) IS-TEXT))
                                                      ((+ (snip->count gsnip) addlen) . > . MAX-COUNT-FOR-SNIP)
                                                      (and (not sticky-styles?)
                                                           (not (eq? (snip->style gsnip) (get-default-style)))))
                                                  
                                                  (let ([style (or caret-style
                                                                   (if sticky-styles?
                                                                       (if gsnip
                                                                           (snip->style gsnip)
                                                                           (snip->style snips))
                                                                       (get-default-style)))])
                                                    (let ([snip (insert-text-snip start style)])
                                                      (set! caret-style #f)
                                                      (values snip start)))
                                                  
                                                  (let ([snip gsnip])
                                                    (if (has-flag? (snip->flags snip) CAN-APPEND)
                                                        (values snip s-pos)
                                                        (let ([style (if sticky-styles?
                                                                         (snip->style snip)
                                                                         (get-default-style))])
                                                          (values (insert-text-snip start style)
                                                                  start)))))])

                                  (if (and gsnip
                                           (has-flag? (snip->flags gsnip) HARD-NEWLINE)
                                           (eq? (snip->next gsnip) snip))
                                      ;; preceding snip was a newline, so the new slip belongs on the next line:
                                      (let* ([oldline (snip->line gsnip)]
                                             [inserted-new-line?
                                              (if (mline-next oldline)
                                                  #f
                                                  (begin
                                                    (mline-insert oldline line-root-box #f)
                                                    (set! num-valid-lines (add1 num-valid-lines))
                                                    (set-mline-last-snip! (mline-next oldline) snip)
                                                    #t))])
                                        (let ([newline (mline-next oldline)])
                                          (set-snip-line! snip newline)
                                          
                                          (set-mline-last-snip! oldline gsnip)
                                          (set-mline-snip! newline snip)
                                          
                                          (mline-calc-line-length oldline)
                                          (mline-mark-recalculate oldline)
                                          (values snip s-pos inserted-new-line?)))
                                      
                                      (values snip s-pos #f)))))])

              (let ([s (- start s-pos)])
                (set-snip-flags! snip (add-flag (snip->flags snip) CAN-SPLIT))
                (send snip insert str addlen s)
                (when (has-flag? (snip->flags snip) CAN-SPLIT)
                  (set-snip-flags! snip (remove-flag (snip->flags snip) CAN-SPLIT)))

                (mline-calc-line-length (snip->line snip))
                (mline-mark-recalculate (snip->line snip))

                (when (max-width . > . 0)
                  (mline-mark-check-flow (snip->line snip))
                  (let ([prev (mline-prev (snip->line snip))])
                    (when (and prev
                               (not (has-flag? (snip->flags (mline-last-snip prev)) HARD-NEWLINE)))
                      (mline-mark-check-flow prev))))

                ;; The text is inserted, but all into one big snip. If the
                ;;  inserted text contains any newlines or tabs, we need to split
                ;;  it up to use tab snips or the HARD-NEWLINE flag:
                (let loop ([snip-start-pos start]
                           [str (string-snip-buffer snip)]
                           [sp (+ s (string-snip-dtext snip))]
                           [i 0]
                           [cnt 0]
                           [inserted-line? inserted-line?])
                  (if (= i addlen)
                      (begin
                        (set! first-line (mline-first (unbox line-root-box)))
                        (set! last-line (mline-last (unbox line-root-box)))
                        (set! len (+ len addlen))
                        (assert (= (last-position) (+ (mline-get-position last-line)
                                                      (mline-len last-line))))
                        (success-finish addlen inserted-line?))
                      (begin
                        (when (equal? (string-ref str sp) #\return)
                          (string-set! str sp #\newline))
                        (let ([c (string-ref str sp)])
                          (cond
                           [(or (equal? c #\newline) (equal? c #\tab))
                            (let ([newline? (equal? c #\newline)])
                              (make-snipset (+ i start) (+ i start 1))
                              (let ([snip (do-find-snip (+ i start) 'after)])
                                (if newline?

                                    ;; forced return - split the snip
                                    (begin
                                      (set-snip-flags! snip
                                                       (remove-flag
                                                        (add-flag (add-flag (add-flag (snip->flags snip)
                                                                                      NEWLINE)
                                                                            HARD-NEWLINE)
                                                                  INVISIBLE)
                                                        CAN-APPEND))
                                      (if (not (eq? snip (mline-last-snip (snip->line snip))))
                                          (let* ([old-line (snip->line snip)]
                                                 [line (mline-insert old-line line-root-box #t)])
                                            (set-snip-line! snip line)
                                            (set! num-valid-lines (add1 num-valid-lines))
                                            (set-mline-last-snip! line snip)
                                            (set-mline-snip! line (mline-snip old-line))

                                            ;; retarget snips moved to new line:
                                            (let loop ([c-snip (mline-snip old-line)])
                                              (unless (eq? c-snip snip)
                                                (set-snip-line! c-snip line)
                                                (loop (snip->next c-snip))))
                                            
                                            (set-mline-snip! old-line (snip->next snip))

                                            (mline-calc-line-length old-line)
                                            (mline-mark-recalculate old-line)
                                            (when (max-width . > . 0)
                                              (mline-mark-check-flow old-line))

                                            (mline-calc-line-length line)
                                            (mline-mark-recalculate line)
                                            (when (max-width . > . 0)
                                              (mline-mark-check-flow line)))

                                          ;; carriage-return inserted at the end of a auto-wrapped line;
                                          ;; line lengths stay the same, but next line now starts
                                          ;; a paragraph
                                          (let ([next (mline-next (snip->line snip))])
                                            (when next
                                              (when (zero? (mline-starts-paragraph next))
                                                (mline-set-starts-paragraph next #t))))))

                                    ;; convert a tab to a tab-snip%
                                    (let ([tabsnip (let ([ts (on-new-tab-snip)])
                                                     (if (or (send ts is-owned?)
                                                             (positive? (snip->count ts)))
                                                         ;; uh-oh
                                                         (new tab-snip%)
                                                         ts))])
                                      (set-snip-style! tabsnip (snip->style snip))
                                      (let* ([rsnip (snip-set-admin tabsnip snip-admin)]
                                             [tabsnip (if (not (eq? rsnip tabsnip))
                                                          ;; uh-oh
                                                          (let ([tabsnip (new tab-snip%)])
                                                            (set-snip-style! tabsnip (snip->style snip))
                                                            (send tabsnip set-admin snip-admin)
                                                            tabsnip)
                                                          tabsnip)])
                                        
                                        (set-snip-flags! tabsnip 
                                                         (add-flag (snip->flags tabsnip) CAN-SPLIT))
                                        (send tabsnip insert "\t" 1 0)
                                        (when (has-flag? (snip->flags tabsnip) CAN-SPLIT)
                                          (set-snip-flags! tabsnip 
                                                           (remove-flag (snip->flags tabsnip) CAN-SPLIT)))
                                        (when (has-flag? (snip->flags snip) NEWLINE)
                                          (set-snip-flags! tabsnip (add-flag (snip->flags tabsnip) NEWLINE)))

                                        (splice-snip tabsnip (snip->prev snip) (snip->next snip))
                                        (set-snip-line! tabsnip (snip->line snip))
                                        (when (eq? (mline-snip (snip->line snip)) snip)
                                          (set-mline-snip! (snip->line tabsnip) tabsnip))
                                        (when (eq? (mline-last-snip (snip->line snip)) snip)
                                          (set-mline-last-snip! (snip->line tabsnip) tabsnip))))))

                              (let ([snip (do-find-snip (+ i start 1) 'after)])
                                (let ([i (add1 i)])
                                  (loop (+ i start)
                                        (if (= i addlen) #f (string-snip-buffer snip))
                                        (if (= i addlen) #f (string-snip-dtext snip))
                                        i
                                        0
                                        (or inserted-line? newline?)))))]

                           [(cnt . > . MAX-COUNT-FOR-SNIP)
                            ;; divide up snip, because it's too large:
                            (make-snipset (+ i start) (+ i start))
                            (let ([snip (do-find-snip (+ i start) 'after)])
                              (loop (+ i start)
                                    (string-snip-buffer snip)
                                    (add1 (string-snip-dtext snip))
                                    (add1 i)
                                    1
                                    inserted-line?))]
                           
                           [else
                            (loop start str (+ sp 1) (+ i 1) (+ cnt 1) inserted-line?)])))))))))))

  (define/private (check-len str len)
    (unless (len . <= . (string-length str))
      (raise-mismatch-error (method-name 'text% 'insert)
                            (format "length ~e too large for given string: "
                                    len)
                            str)))

  (define/override (insert . args)
    (case-args
     args
     [([string? str])
      (do-insert #f str #f startpos endpos #t)]
     [([string? str] 
       [exact-nonnegative-integer? start] 
       [(make-alts exact-nonnegative-integer? (symbol-in same)) [end 'same]]
       [any? [scroll-ok? #t]])
      (do-insert #f str #f start end scroll-ok?)]
     [([exact-nonnegative-integer? len] 
       [string? str])
      (check-len str len)
      (do-insert #f (substring str 0 len) #f startpos endpos #t)]
     [([exact-nonnegative-integer? len] 
       [string? str] 
       [exact-nonnegative-integer? start] 
       [(make-alts exact-nonnegative-integer? (symbol-in same)) [end 'same]]
       [any? [scroll-ok? #t]])
      (check-len str len)
      (do-insert #f (substring str 0 len) #f start end scroll-ok?)]
     [([snip% snip])
      (do-insert snip #f #f startpos endpos #t)]
     [([snip% snip]
       [exact-nonnegative-integer? [start startpos]]
       [(make-alts exact-nonnegative-integer? (symbol-in same)) [end 'same]]
       [any? [scroll-ok? #t]])
      (do-insert snip #f #f start end scroll-ok?)]
     [([char? ch])
      (do-insert-char ch startpos endpos)]
     [([char? ch] 
       [exact-nonnegative-integer? start] 
       [(make-alts exact-nonnegative-integer? (symbol-in same)) [end 'same]])
      (do-insert-char ch start end)]
     (method-name 'text% 'insert)))

  (define/public (do-insert-snips snips pos)
    (do-insert #f #f snips pos pos #t))

  (define/private (do-insert-char ch start end)
    (let ([streak? typing-streak?]
          [ifs? insert-force-streak?])
      (end-streaks '(delayed))
      (set! insert-force-streak? streak?)
      (do-insert #f (string ch) #f start end #t)
      (set! insert-force-streak? ifs?)
      (set! typing-streak? #t)))

  (define/private (do-delete start end with-undo? [scroll-ok? #t])
    (assert (consistent-snip-lines 'do-delete))
    (unless (or write-locked? s-user-locked?)
      (let-values ([(start end set-caret-style?)
                    (if (eq? end 'back)
                        (if (zero? start)
                            (values 0 0 #f)
                            (values (sub1 start) start #t))
                        (values start end (and (= start startpos)
                                               (= end endpos))))])
        (unless (or (start . >= . end)
                    (start . < . 0)
                    (start . >= . len))
          (let ([end (min end len)])
            (when ALLOW-X-STYLE-SELECTION?
              (when (and (start . <= . startpos) (end . >= . endpos))
                (when (or (not (in-edit-sequence?))
                          need-x-copy?)
                  (set! need-x-copy? #f)
                  (copy-out-x-selection))))

            (set! write-locked? #t)

            (if (not (can-delete? start (- end start)))
                (set! write-locked? #f)
                (begin
                  (on-delete start (- end start))

                  (set! flow-locked? #t)

                  (make-snipset start end)
                  (set! revision-count (add1 revision-count))

                  (let* ([start-snip (do-find-snip start 'before-or-none)]
                         [end-snip (do-find-snip end 'before)]
                         [with-undo? (and with-undo?
                                          (zero? s-noundomode))]
                         [rec (if with-undo?
                                  (begin
                                    (when (not s-modified?)
                                      (add-undo-rec (make-object unmodify-record% delayed-streak?)))
                                    (make-object delete-record%
                                                 start  end
                                                 (or deletion-streak? delayed-streak?
                                                     delete-force-streak? (not s-modified?))
                                                 startpos endpos))
                                  #f)])

                    (when (and set-caret-style? sticky-styles?)
                      (set! caret-style (if start-snip
                                            (snip->style (snip->next start-snip))
                                            (snip->style snips))))

                    (let-values ([(deleted-line? update-cursor?)
                                  (let loop ([snip end-snip]
                                             [deleted-line? #f]
                                             [update-cursor? #f])
                                    (if (eq? snip start-snip)
                                        (values deleted-line? update-cursor?)
                                        (let ([update-cursor?
                                               (or (and (eq? snip s-caret-snip)
                                                        (let ([rl? read-locked?])
                                                          (set! read-locked? #t)
                                                          (send s-caret-snip own-caret #f)
                                                          (set! read-locked? rl?)
                                                          (set! s-caret-snip #f)
                                                          #t))
                                                   update-cursor?)])
                                          
                                          (when with-undo?
                                            (send rec insert-snip snip))
                                          
                                          (let* ([prev (snip->prev snip)]
                                                 [deleted-another-line?
                                                  (let ([line (snip->line snip)])
                                                    (cond
                                                     [(eq? (mline-snip line) snip)
                                                      (if (eq? (mline-last-snip line) snip)
                                                          (begin
                                                            (mline-delete line line-root-box)
                                                            (set! num-valid-lines (sub1 num-valid-lines))
                                                            #t)
                                                          (begin
                                                            (set-mline-snip! line (snip->next snip))
                                                            #f))]
                                                     [(eq? (mline-last-snip line) snip)
                                                      (if (mline-next line)
                                                          (begin
                                                            (set-mline-last-snip! line (mline-last-snip (mline-next line)))
                                                            (mline-delete (mline-next line) line-root-box)
                                                            (set! num-valid-lines (sub1 num-valid-lines))
                                                            #t)
                                                          (begin
                                                            (set-mline-last-snip! line prev)
                                                            ;; maybe deleted extra ghost line:
                                                            extra-line?))]
                                                     [else 
                                                      #f]))])
                                            (delete-snip snip)
                                            (loop prev 
                                                  (or deleted-line?
                                                      deleted-another-line?)
                                                  update-cursor?)))))])

                      (when (zero? snip-count)
                        (make-only-snip)
                        (when caret-style
                          (set-snip-style! snips caret-style)
                          (set! caret-style #f)))

                      (set! first-line (mline-first (unbox line-root-box)))
                      (set! last-line (mline-last (unbox line-root-box)))

                      (let-values ([(line moved-to-next?)
                                    (if start-snip
                                        (if (has-flag? (snip->flags start-snip) NEWLINE)
                                            (if (mline-next (snip->line start-snip))
                                                (values (mline-next (snip->line start-snip))
                                                        #t)
                                                (begin
                                                  (mline-mark-check-flow (snip->line start-snip))
                                                  (values #f #f)))
                                            (values (snip->line start-snip) #f))
                                        (values first-line #f))])
                        
                        (when line
                          ;; fix line references from possibly moved snips:
                          (let ([next (snip->next (mline-last-snip line))])
                            (let loop ([snip (mline-snip line)])
                              (unless (eq? snip next)
                                (set-snip-line! snip line)
                                (loop (snip->next snip)))))

                          (mline-calc-line-length line)
                          (mline-mark-recalculate line)

                          (when (max-width . >= . 0)
                            (mline-mark-check-flow line)
                            (let ([next (mline-next line)])
                              (when next (mline-mark-check-flow next)))
                            (let ([prev (mline-prev line)])
                              (when (and prev
                                         (has-flag? (snip->flags (mline-last-snip prev)) HARD-NEWLINE))
                                (mline-mark-check-flow prev)
                                (when (and moved-to-next?
                                           deleted-line?
                                           (mline-prev prev)
                                           (not (has-flag? (snip->flags (mline-last-snip (mline-prev prev))) 
                                                           HARD-NEWLINE)))
                                  ;; maybe the deleted object was in the middle of a long word,
                                  ;; and maybe now the long word can be folded into the previous
                                  ;; line
                                  (mline-mark-check-flow (mline-prev prev)))))))

                        (adjust-clickbacks start end (- start end) rec)

                        (when with-undo?
                          (add-undo-rec rec)
                          (when (in-edit-sequence?)
                            (set! delayed-streak? #t)))

                        (let ([dellen (- end start)])
                          (set! len (- len dellen))

                          (check-merge-snips start)

                          (set! flow-locked? #f)
                          (set! write-locked? #f)

                          (cond
                           [(and (startpos . >= . start) (startpos . <= . end))
                            (set! caret-blinked? #f)
                            (set! startpos start)]
                           [(startpos . > . end)
                            (set! caret-blinked? #f)
                            (set! startpos (- startpos dellen))])

                          (cond
                           [(and (endpos . >= . start) (endpos . <= . end))
                            (set! endpos start)]
                           [(endpos . > . end)
                            (set! endpos (- endpos dellen))])

                          (unless refresh-unset?
                            (cond
                             [(and (refresh-start . >= . start) (refresh-start . <= . end))
                              (set! refresh-start start)]
                             [(refresh-start . >= . end)
                              (set! refresh-start (- refresh-start dellen))])
                            (cond
                             [(and (refresh-end . >= . start) (refresh-end . <= . end))
                              (set! refresh-end start)]
                             [(refresh-end . >= . end)
                              (set! refresh-end (- refresh-end dellen))]))

                          (set! extra-line? (has-flag? (snip->flags last-snip) NEWLINE))

                          (when (and scroll-ok? (= start startpos))
                            (set! delay-refresh (add1 delay-refresh))
                            (parameterize ([in-delayed-refresh #f])
                              (scroll-to-position/refresh startpos))
                            (set! delay-refresh (sub1 delay-refresh)))

                          (set! changed? #t)

                          (unless set-caret-style?
                            (set! caret-style #f))

                          (when (= len start)
                            ;; force recheck extra line state:
                            (set! graphic-maybe-invalid? #t)
                            (set! graphic-maybe-invalid-force? #t))

                          (if deleted-line?
                              (begin
                                (set! graphic-maybe-invalid? #t)
                                (need-refresh start))
                              (refresh-by-line-demand))

                          (unless s-modified?
                            (set-modified #t))

                          (after-delete start dellen)

                          (when update-cursor?
                            (when s-admin
                              (send s-admin update-cursor))))))))))))
      (assert (consistent-snip-lines 'post-do-delete))))

  (define/public (delete . args)
    (case-args
     args
     [()
      (let ([streak? (= endpos startpos)]
            [dstreak? deletion-streak?]
            [dfs? delete-force-streak?])
        (end-streaks '(delayed))
        (set! delete-force-streak? dstreak?)
        
        (delete startpos (if (= startpos endpos) 'back endpos))

        (set! delete-force-streak? dfs?)
        (set! deletion-streak? streak?))]
     [([(make-alts exact-nonnegative-integer? (symbol-in start)) start]
       [(make-alts exact-nonnegative-integer? (symbol-in back)) [end 'back]]
       [any? [scroll-ok? #t]])
      (do-delete (if (symbol? start) startpos start) end #t scroll-ok?)]
     (method-name 'text% 'delete)))

  (def/public (erase)
    (do-delete 0 len #t #t))
  
  (def/override (clear)
    (delete startpos endpos #t))
  
  ;; ----------------------------------------

  (def/override (cut [any? [extend? #f]] [exact-integer? [time 0]]
                     [(make-alts exact-nonnegative-integer? (symbol-in start)) [start 'start]]
                     [(make-alts exact-nonnegative-integer? (symbol-in end)) [end 'end]])
    (let* ([start (if (symbol? start)
                      startpos
                      start)]
           [end (if (symbol? end)
                    endpos
                    end)]
           [end (min end len)])
      (unless (start . >= . end)
        (copy extend? time start end)
        (delete start end))))

  (def/public (do-copy [exact-nonnegative-integer? startp] 
                       [exact-nonnegative-integer? endp] 
                       [exact-integer? time] 
                       [bool? extend?])
    (let ([startp (max startp 0)]
          [endp (min endp len)])
      (unless (endp . <= . startp)

        (make-snipset startp endp)
  
        (let ([sl (or (and extend? copy-style-list)
                      s-style-list)])
          (set-common-copy-region-data! (get-region-data startp endp))

          (let ([start (do-find-snip startp 'after)]
                [end (do-find-snip endp 'after-or-none)]
                [wl? write-locked?]
                [fl? flow-locked?])

            (set! write-locked? #t)
            (set! flow-locked? #t)

            (let loop ([snip start])
              (unless (eq? snip end)
                (let ([asnip (send snip copy)])
                  (snip-set-admin asnip #f)
                  (set-snip-style! asnip (send sl convert (snip->style asnip)))
                  (cons-common-copy-buffer! asnip)
                  (cons-common-copy-buffer2! (get-snip-data snip)))
                (loop (snip->next snip))))

            (set! write-locked? wl?)
            (set! flow-locked? fl?)

            (install-copy-buffer time sl))))))

  (def/override (copy [any? [extend? #f]] [exact-integer? [time 0]]
                      [(make-alts exact-nonnegative-integer? (symbol-in start)) [start 'start]]
                      [(make-alts exact-nonnegative-integer? (symbol-in end)) [end 'end]])
    (let* ([start (if (symbol? start)
                      startpos
                      start)]
           [end (if (symbol? end)
                    endpos
                    end)]
           [end (min end len)])
      (unless (start . >= . end)
        (begin-copy-buffer)
        (unless extend?
          (free-old-copies))
        (do-copy start end time extend?)
        (end-copy-buffer))))

  (define/private (do-generic-paste cb start time)
    (set! read-insert start)
    (set! read-insert-start start)
    (let ([orig-len len])
      (do-buffer-paste cb time #f)
      (let ([delta (- len orig-len)])
        (set! prev-paste-start start)
        (set! prev-paste-end (+ start delta)))))

  (define/public (do-paste start time)
    (do-generic-paste the-clipboard start time))

  (define/public (do-paste-x-selection start time)
    (do-generic-paste the-x-selection-clipboard start time))

  (define/private (generic-paste x-sel? time start end)
    (let* ([end (if (symbol? end)
                    (if (symbol? start)
                        endpos
                        start)
                    end)]
           [start (if (eq? start 'start)
                      startpos
                      (if (symbol? start)
                          endpos
                          start))]
           [end (min end len)])
      (unless (start . > . end)

        (begin-edit-sequence)
        (when (start . < . end)
          (delete start end))

        (if x-sel?
            (do-paste-x-selection start time)
            (do-paste start time))

        (let ([save-prev-paste prev-paste-start])
          (end-edit-sequence)
          (set! prev-paste-start save-prev-paste)))))

  (def/override (paste [exact-integer? [time 0]]
                       [(make-alts exact-nonnegative-integer? (symbol-in start end)) [start 'start]]
                       [(make-alts exact-nonnegative-integer? (symbol-in same)) [end 'same]])
    (generic-paste #f time start end))

  (def/override (paste-x-selection [exact-integer? [time 0]]
                                   [(make-alts exact-nonnegative-integer? (symbol-in start end)) [start 'start]]
                                   [(make-alts exact-nonnegative-integer? (symbol-in same)) [end 'same]])
    (generic-paste #t time start end))
  
  (define/override (insert-paste-snip snip data)
    (let ([addpos (snip->count snip)])
      (insert snip read-insert)
      (when data
        (let ([snip (do-find-snip read-insert 'after)])
          (set-snip-data snip data)))
      (set! read-insert (+ read-insert addpos))))

  (define/public (paste-region-data data)
    (set-region-data read-insert-start read-insert data))

  (define/override (insert-paste-string str)
    (let* ([str (if (eq? 'windows (system-type))
                    (regexp-replace* #rx"\r\n" str "\n")
                    str)]
           ;; change non-breaking space to space:
           [str (regexp-replace* #rx"\xA0" str " ")])

      (insert str read-insert)
      (set! read-insert (+ read-insert (string-length str)))))

  (def/public (paste-next)
    (unless (prev-paste-start . < . 0)
      (let ([start prev-paste-start]
            [end prev-paste-end])

        (copy-ring-next)
        (begin-edit-sequence)
        (delete start end)
        (set! read-insert start)
        (set! read-insert-start start)

        (let ([orig-len len])
          (do-buffer-paste the-clipboard 0 #t)

          (end-edit-sequence)
          
          (let ([delta (- len orig-len)])
            
            (set! prev-paste-start start)
            (set! prev-paste-end (+ start delta)))))))

  (define/private (do-kill time start end)
    (let ([streak? kill-streak?])

      (begin-edit-sequence)
      (let-values ([(start end)
                    (if (symbol? start)
                        (let ([newend (paragraph-end-position (position-paragraph endpos posateol?))])
                          (if (= newend startpos)
                              (set-position startpos (+ startpos 1) #f #t 'local)
                              (begin
                                (set-position startpos newend #f #t 'local)
                                
                                (let ([text (get-text startpos endpos)])
                                  (let loop ([i (- endpos startpos)])
                                    (if (zero? i)
                                        ;; line has all spaces: move one more
                                        (set-position startpos (+ endpos 1) #f #t 'local)
                                        (let ([i (sub1 i)])
                                          (when (char-whitespace? (string-ref text i))
                                            (loop i))))))))
                          (values startpos endpos))
                        (values start end))])

        (cut streak? time start end)
        (end-edit-sequence)

        (set! kill-streak? #t))))

  (define/override (kill . args)
    (case-args
     args
     [([exact-integer? [time 0]])
      (do-kill 0 'start 'end)]
     [([exact-integer? time]
       [exact-nonnegative-integer? start]
       [exact-nonnegative-integer? end])
      (do-kill time start end)]
     (method-name 'text% 'kill)))

  (def/override (select-all)
    (set-position 0 len))

  (define/override (really-can-edit? op)
    (cond
     [read-locked? #f]
     [(and (not (eq? 'copy op))
           (or flow-locked? write-locked?))
      #f]
     [else
      (case op
        [(clear cut copy)
         (not (= endpos startpos))]
        [(kill)
         (not (= len endpos))]
        [(select-all)
         (positive? len)]
        [else #t])]))

  ;; ----------------------------------------

  (def/public (split-snip [exact-nonnegative-integer? pos])
    (unless (or flow-locked?
                (pos . <= . 0)
                (pos . >= . len))
      (let ([wl? write-locked?])

        (set! write-locked? #t)
        (set! flow-locked? #t)
        (make-snipset pos pos)
        (set! write-locked? wl?)
        (set! flow-locked? #f))))

  (def/public (get-revision-number)
    revision-count)

  (def/override (get-flattened-text)
    (get-text 0 'eof #t #f))

  (def/public (get-text [exact-nonnegative-integer? [start 0]]
                        [(make-alts exact-nonnegative-integer? (symbol-in eof)) [end 'eof]]
                        [any? [flat? #f]]
                        [any? [force-cr? #f]])
    (if read-locked? 
        ""
        (let* ([end (if (eq? end 'eof)
                        len
                        end)]
               [start (min start len)]
               [end (max end start)]
               [end (min end len)]
               [count (- end start)])
          (if (zero? count)
              ""
              (let ([wl? write-locked?]
                    [fl? flow-locked?]
                    [p (open-output-string)])
                (set! write-locked? #t)
                (set! flow-locked? #t)

                (let-values ([(snip s-pos) (find-snip/pos start 'after)])
                  (let loop ([snip snip]
                             [offset (- start s-pos)]
                             [count count])
                    (let ([num (min (- (snip->count snip) offset)
                                    count)])
                      (if (not flat?)
                          (display (send-generic snip snip%-get-text offset num #f) p)
                          (begin
                            (display (send-generic snip snip%-get-text offset num #t) p)
                            (when (and force-cr?
                                       (has-flag? (snip->flags snip) NEWLINE)
                                       (not (has-flag? (snip->flags snip) HARD-NEWLINE)))
                              (display "\n" p))))
                      (let ([count (- count num)])
                        (if (zero? count)
                            (begin
                              (set! write-locked? wl?)
                              (set! flow-locked? fl?)
                              (get-output-string p))
                            (loop (snip->next snip)
                                  0
                                  count)))))))))))
  
  (def/public (get-character [exact-nonnegative-integer? start])
    (if read-locked?
        #\nul
        (let-values ([(snip s-pos) (find-snip/pos (max 0 (min start len)) 'after)])
          (let ([delta (- start s-pos)])
            (if (delta . >= . (snip->count snip))
                #\nul
                (let ([buffer (make-string 1)])
                  (send snip get-text! buffer delta 1 0)
                  (string-ref buffer 0)))))))

  ;; ----------------------------------------

  (def/public (set-clickback [exact-nonnegative-integer? start]
                             [exact-nonnegative-integer? end]
                             [procedure? f]
                             [(make-or-false style-delta%) [c-delta #f]]
                             [any? [call-on-down? #f]])
    (let ([delta (make-object style-delta%)])
      (when c-delta
        (send delta copy c-delta))
      
      (let ([cb (make-clickback start
                                end
                                f
                                call-on-down?
                                delta
                                #f
                                null)])
        (set! clickbacks (cons cb clickbacks)))))

  (define/public (add-back-clickback cb)
    (set! clickbacks (cons cb clickbacks)))

  (def/public (remove-clickback [exact-nonnegative-integer? start]
                                [exact-nonnegative-integer? end])
    (set! clickbacks
          (filter (lambda (cb)
                    (not (and (= start (clickback-start cb))
                              (= end (clickback-end cb)))))
                  clickbacks)))

  (def/public (call-clickback [exact-nonnegative-integer? start]
                              [exact-nonnegative-integer? end])
    (for-each (lambda (cb)
                (when (and ((clickback-start cb) . <= . start)
                           ((clickback-end cb) . >= . end))
                  ((clickback-f cb)  this (clickback-start cb)  (clickback-end cb))))
              clickbacks))

  
  (define/private (adjust-clickbacks start end d rec)
    (when (pair? clickbacks)
      (set! clickbacks
            (filter (lambda (c)
                      (if (and ((clickback-start c) . >= . start) 
                               ((clickback-end c) . <= . end))
                          (begin
                            (when rec
                              (send rec add-clickback c))
                            #f)
                          #t))
                    clickbacks))
      (for-each (lambda (c)
                  (cond
                   [((clickback-start c) . >= . end)
                    (set-clickback-start! c (+ (clickback-start c) d))
                    (set-clickback-end! c (+ (clickback-end c) d))]
                   [(and ((clickback-start c) . <= . start)
                         ((clickback-end c) . >= . end))
                    (when (or (d . < . 0) ((clickback-end c) . > . end))
                      (set-clickback-end! c (+ (clickback-end c) d)))]
                   [(and ((clickback-start c) . > . start) 
                         ((clickback-end c) . > . end))
                    (set-clickback-start! c start)
                    (set-clickback-end! c (+ (clickback-end c) d))]))
                clickbacks)
      (set! clickbacks
            (filter (lambda (c)
                      (if (= (clickback-start c) (clickback-end c))
                          (when rec
                            (send rec add-clickback c)
                            #f)
                          #t))
                    clickbacks))))

  (define/private (find-clickback start y)
    (ormap (lambda (c)
             (and ((clickback-start c) . <= . start)
                  ((clickback-end c) . > . start)
                  ;; we're in the right horizontal region, but maybe the mouse
                  ;; is above or below the clickback
                  (let ([start (do-find-snip (clickback-start c) 'after)]
                        [end (do-find-snip (clickback-end c) 'before)])
                    (and start
                         end
                         (let-boxes ([top 0.0]
                                     [bottom 0.0])
                             (begin
                               (get-snip-location start #f top #f)
                               (get-snip-location start #f bottom #t))
                           (let loop ([start start]
                                      [top top]
                                      [bottom bottom])
                             (if (eq? end start)
                                 (and (y . >= . top)
                                      (y . <= . bottom)
                                      c)
                                 (let ([start (snip->next start)])
                                   (let-boxes ([ntop 0.0]
                                               [nbottom 0.0])
                                       (begin
                                         (get-snip-location start #f ntop #f)
                                         (get-snip-location start #f nbottom #t))
                                     (loop start
                                           (min ntop top)
                                           (max nbottom bottom)))))))))))
           clickbacks))

  (define/private (set-clickback-hilited c on?)
    (when (not (eq? (and on? #t)
                    (clickback-hilited? c)))
      (cond
       [on? 
        (s-start-intercept)
        
        (begin-edit-sequence)
        (flash-on (clickback-start c) (clickback-end c) #f #f 0)
        (do-change-style (clickback-start c) (clickback-end c) #f (clickback-delta c) #f)
        (end-edit-sequence)

        (set-clickback-unhilite! c (s-end-intercept))]
       [else
        (perform-undo-list (clickback-unhilite c))
        (set-clickback-unhilite! c null)
        (flash-off)])
      (set-clickback-hilited?! (and on? #t))))

  ;; ----------------------------------------

  (def/public (flash-on [exact-nonnegative-integer? start]
                        [exact-nonnegative-integer? end]
                        [any? [ateol? #f]]
                        [any? [scroll? #t]]
                        [exact-nonnegative-integer? [timeout 500]])
    (do-set-position #t 'none start end ateol? scroll? 'default #f)
    (when (timeout . > . 0)
      (set! flashautoreset? #t)
      (when flash-timer
        (send flash-timer stop))
      (set! flash-timer (new flash-timer% [editor this]))
      (send flash-timer start timeout))
    (set! flashscroll? scroll?))

  (def/public (flash-off)
    (when flash?
      (set! flashautoreset? #t)
      (set! flashdirectoff? #t)
      (do-set-position #f 'none startpos endpos posateol? flashscroll? 'default #f)))

  ;; ----------------------------------------

  (def/public (set-wordbreak-func [procedure? f])
    (set! word-break f))

  (def/public (find-wordbreak [(make-or-false (make-box exact-nonnegative-integer?)) start] 
                              [(make-or-false (make-box exact-nonnegative-integer?)) end] 
                              [(symbol-in caret line selection user1 user2) reason])
    (unless read-locked?
      (let ([oldstart (if start (unbox start) 0)]
            [oldend (if end (unbox end) 0)])
        (word-break this start end reason)

        (when (and start ((unbox start) . > . oldstart))
          (set-box! start oldstart))
        (when (and end ((unbox end) . < . oldend))
          (set-box! end oldend)))))

  (def/public (get-wordbreak-map)
    word-break-map)

  (def/public (set-wordbreak-map [(make-or-false editor-wordbreak-map%) map])
    (set! word-break-map map))

  ;; ----------------------------------------

  (def/public (set-line-spacing [nonnegative-real? s])
    (unless (or flow-locked?
                (= line-spacing s))
      (set! line-spacing s)
      (size-cache-invalid)
      (set! changed? #t)
      (need-refresh -1 -1)))

  (def/public (get-line-spacing) line-spacing)

  (def/public (get-padding)
    (values padding-l padding-t padding-r padding-b))
  (def/public (set-padding [nonnegative-real? l]
                           [nonnegative-real? t]
                           [nonnegative-real? r]
                           [nonnegative-real? b])
    (unless (and (= l padding-l)
                 (= t padding-t)
                 (= r padding-r)
                 (= b padding-b))
      (set! padding-l (exact->inexact l))
      (set! padding-t (exact->inexact t))
      (set! padding-r (exact->inexact r))
      (set! padding-b (exact->inexact b))
      (unless (= 0.0 max-width)
        (set! max-line-width (max (- max-width padding-t padding-r)
                                  ZERO-LINE-WIDTH)))
      (set! flow-invalid? #t)
      (set! graphic-maybe-invalid? #t)
      (set! changed? #t)
      (need-refresh -1 -1)))

  (def/override (get-max-width)
    (if (max-width . <= . 0)
        'none
        (+ max-width wrap-bitmap-width)))

  (def/override (get-min-width)
    (if (min-width . <= . 0)
        'none
        min-width))

  (def/override (set-max-width [(make-alts nonnegative-real? (symbol-in none)) w])
    (unless flow-locked?
      (let* ([w (if (eq? w 'none) 0.0 w)]
             [w (if (and (positive? wrap-bitmap-width) (w . > . 0))
                    (let ([w (- w wrap-bitmap-width)])
                      (if (w . <= . 0.0)
                          (+ CURSOR-WIDTH 1)
                          w))
                    w)])
        (unless (or (= max-width w) 
                    (and (w . <= . 0) (max-width . <= . 0))
                    (not (can-set-size-constraint?)))
          (on-set-size-constraint)
          
          (let ([w (if (and (w . > . 0)
                            (w . < . (+ CURSOR-WIDTH 1)))
                       (+ CURSOR-WIDTH 1)
                       w)])
            (set! max-width w)
            (set! max-line-width (if (= w 0.0)
                                     0.0
                                     (max (- w padding-t padding-r)
                                          ZERO-LINE-WIDTH)))
            (set! flow-invalid? #t)
            (set! graphic-maybe-invalid? #t)
            (set! changed? #t)
            (need-refresh -1 -1)

            (after-set-size-constraint))))))

  (define/private (set-m-x v current setter)
    (let ([v (if (eq? v 'none) 0.0 v)])
      (unless (or flow-locked?
                  (= current v)
                  (and (v . <= . 0) (current . <= . 0))
                  (not (can-set-size-constraint?)))
        (on-set-size-constraint)

        (set! graphic-maybe-invalid? #t)
        (set! graphic-maybe-invalid-force? #t)
        (setter v)
        (set! changed? #t)
        (need-refresh -1 -1)

        (after-set-size-constraint))))

  (def/override (set-min-width [(make-alts nonnegative-real? (symbol-in none)) w])
    (set-m-x w min-width (lambda (w) (set! min-width w))))

  (def/override (set-min-height [(make-alts nonnegative-real? (symbol-in none)) h])
    (set-m-x h min-height (lambda (h) (set! min-height h))))

  (def/override (set-max-height [(make-alts nonnegative-real? (symbol-in none)) h])
    (set-m-x h max-height (lambda (h) (set! max-height h))))

  (def/override (get-min-height)
    (if (min-height . <= . 0)
        'none
        min-height))

  (def/override (get-max-height)
    (if (max-height . <= . 0)
        'none
        max-height))

  ;; ----------------------------------------

  (def/override (insert-port [input-port? f] 
                             [(symbol-in guess same copy standard text text-force-cr) [format 'guess]]
                             [any? [replace-styles? #t]])
    (if (or write-locked? s-user-locked?)
        (if (not (detect-wxme-file (method-name 'text% 'insert-file) f #t))
            'text
            'standard)
        (do-insert-file (method-name 'text% 'insert-file) f format replace-styles?)))

  (define/private (do-insert-file who f fmt clear-styles?)
    (let ([fmt
           (cond
            [(or (eq? 'guess fmt) (eq? 'same fmt) (eq? 'copy fmt))
             (if (not (detect-wxme-file who f #t))
                 'text
                 'standard)]
            [else fmt])])

      (let ([fileerr?
             (cond
              [(eq? 'standard fmt)
               (if (not (detect-wxme-file who f #f))
                   (error who "not a WXME file")
                   (let* ([b (make-object editor-stream-in-file-base% f)]
                          [mf (make-object editor-stream-in% b)])
                     (or (and (not (read-editor-version mf b #f #t))
                              'read-editor-version-failed)
                         (and (not (read-editor-global-header mf))
                              'read-editor-global-head-failed)
                         (and (not (send mf ok?))
                              'mf-not-ok)
                         (and (not (read-from-file mf clear-styles?))
                              'read-from-file-failed)
                         (and (not (read-editor-global-footer mf))
                              'read-editor-gobal-footer-failed)
                         (begin
                           ;; if STD-STYLE wasn't loaded, re-create it:
                           (send s-style-list new-named-style "Standard" (send s-style-list basic-style))
                           (and (not (send mf ok?))
                                'mf-not-okay-after-adding-standard-style)))))]
              [(or (eq? fmt 'text) (eq? fmt 'text-force-cr))
               (let ([s (make-string 1024)])
                 (let loop ([saved-cr? #f])
                   (let ([len (read-string! s f)])
                     (unless (eof-object? len)
                       (let* ([s1 (if (= len (string-length s))
                                      s
                                      (substring s 0 len))]
                              [s2 (if (equal? #\return (string-ref s1 (sub1 len)))
                                      (substring s1 0 (sub1 len))
                                      s1)])
                         (insert (regexp-replace* #rx"\r\n" 
                                                  (if saved-cr? (string-append "\r" s2) s2)
                                                  "\n"))
                         (loop (not (eq? s1 s2))))))))
               #f])])
        
        (when fileerr?
          (error who "error loading the file~a" (if (boolean? fileerr?)
                                                    ""
                                                    (format " (~a)" fileerr?))))

        fmt)))

  (def/override (save-port [output-port? f]
                           [(symbol-in guess same copy standard text text-force-cr) [format 'same]]
                           [any? [show-errors? #t]])
    (when read-locked?
      (error (method-name 'text% 'save-file) "editor locked for reading"))

    (let ([format 
           (cond
            [(or (eq? 'same format) (eq? 'guess format) (eq? 'copy format))
             file-format]
            [else format])])

      (let ([fileerr?
             (cond
              [(or (eq? 'text format) (eq? 'text-force-cr format))
               (display (get-text 0 'eof #t (eq? format 'text-force-cr)) f)
               #f]
              [else
               (let* ([b (make-object editor-stream-out-file-base% f)]
                      [mf (make-object editor-stream-out% b)])
                 (not (and (write-editor-version mf b)
                           (write-editor-global-header mf)
                           (send mf ok?)
                           (write-to-file mf)
                           (write-editor-global-footer mf)
                           (send mf ok?))))])])
        (when fileerr?
          (error (method-name 'text% 'save-port) "error writing editor content"))
        #t)))
  
  (define/private (do-read-from-file f start overwritestyle?)
    (if write-locked?
        #f
        (let ([start (if (symbol? start)
                         startpos
                         start)])
          (set! read-insert start)
          (let ([result (read-snips-from-file f overwritestyle?)])

            (when (zero? len)
              ;; we probably destructively changed the style list; reset the dummy snip
              (set-snip-style! snips (or (get-default-style)
                                         (send s-style-list basic-style))))

            result))))

  (define/override (read-from-file . args)
    (case-args
     args
     [([editor-stream-in% f] [exact-nonnegative-integer? start] [any? [overwritestyle? #f]])
      (do-read-from-file f start overwritestyle?)]
     [([editor-stream-in% f] [any? [overwritestyle? #f]])
      (do-read-from-file f 'start overwritestyle?)]
     (method-name 'text% 'read-from-file)))

  (define/override (do-read-insert snip)
    (if (list? snip)
        (let ([oldlen len])
          (do-insert #f #f snip startpos startpos #t)
          (set! read-insert (+ read-insert (- len oldlen)))
          #t)
        (let ([addpos (snip->count snip)])
          (do-insert snip #f #f startpos startpos #t)
          (set! read-insert (+ addpos read-insert))
          #t)))

  (def/override (write-to-file [editor-stream-out% f]
                               [exact-nonnegative-integer? [start 0]]
                               [(make-alts exact-nonnegative-integer? (symbol-in eof)) [end 'eof]])
    (if read-locked?
        #f
        (let ([end (max (if (eq? end 'eof)
                            len
                            end)
                        start)])
          (let ([start-snip (if (zero? len) #f (do-find-snip start 'after))]
                [end-snip (if (zero? len) #f (do-find-snip end 'after-or-none))])
            (and (do-write-headers-footers f #t)
                 (write-snips-to-file f s-style-list #f start-snip end-snip #f this)
                 (do-write-headers-footers f #f))))))

  (def/public (get-file-format) file-format)
  (def/public (set-file-format [(symbol-in standard text text-force-cr) format])
    (set! file-format format))

  (def/override (set-filename [(make-or-false path-string?) name][any? [temp? #f]])
    (set! s-filename (if (string? name)
                         (string->path name)
                         name))
    (set! s-temp-filename? temp?)
    (let ([wl? write-locked?]
          [fl? flow-locked?])
      (set! write-locked? #t)
      (set! flow-locked? #t)

      (let loop ([snip snips])
        (when snip
          (when (has-flag? (snip->flags snip) USES-BUFFER-PATH)
            (send snip set-admin snip-admin))
          (loop (snip->next snip))))
      
      (set! write-locked? wl?)
      (set! flow-locked? fl?)))

  ;; ----------------------------------------

  (def/public (get-region-data [exact-nonnegative-integer? start]
                               [exact-nonnegative-integer? end])
    #f)

  (def/public (set-region-data [exact-nonnegative-integer? start]
                               [exact-nonnegative-integer? end]
                               [editor-data% d])
    (void))

  ;; ----------------------------------------

  (def/public (get-tabs [maybe-box? [count #f]] 
                        [maybe-box? [space #f]] 
                        [maybe-box? [in-units #f]])
    (when count
      (set-box! count (vector-length tabs)))
    (when space
      (set-box! space (if (symbol? tab-space)
                          #f
                          tab-space)))
    (when in-units
      (set-box! in-units tab-space-in-units?))

    (vector->list tabs))

  (def/public (set-tabs [(make-list real?) newtabs]
                        [(make-alts real? (symbol-in tab-width)) [tab-width 20]]
                        [any? [in-units? #t]])
    (unless flow-locked?
      (set! tabs (list->vector newtabs))

      (if (and (number? tab-width) (tab-width . >= . 1))
          (set! tab-space (exact->inexact tab-width))
          (set! tab-space TAB-WIDTH))

      (set! tab-space-in-units? in-units?)
      
      (size-cache-invalid)
      (set! changed? #t)
      (need-refresh -1 -1)))

  ;; ----------------------------------------

  (define/private (do-find-position-in-line internal? i x ateol?-box onit?-box how-close-box)
    (when onit?-box
      (set-box! onit?-box #f))
    (when ateol?-box
      (set-box! ateol?-box #f))
    (when how-close-box
      (set-box! how-close-box 100.0))

    (cond
     [(and (not internal?) (not (check-recalc #t #f)))
      0]
     [(i . < . 0) 0]
     [(i . >= . num-valid-lines) len]
     [else
      (let* ([line (mline-find-line (unbox line-root-box) i)]
             [x (- x padding-l (mline-get-left-location line max-line-width))])
        (if (x . <= . 0)
            (find-first-visible-position line)
            (let ([p (mline-get-position line)])
              (let-values ([(snip s-pos p)
                            (if (x . >= . (mline-w line))
                                ;; snip == the last one
                                (let ([snip (mline-last-snip line)])
                                  (values snip
                                          (+ p (- (mline-len line) (snip->count snip)))
                                          (+ p (mline-len line))))
                                (begin
                                  (when onit?-box
                                    (set-box! onit?-box #t))

                                  (let ([dc (send s-admin get-dc)]
                                        [X 0]
                                        [wl? write-locked?]
                                        [fl? flow-locked?])
                                    (set! write-locked? #t)
                                    (set! flow-locked? #t)

                                    ;; linear search for snip
                                    (let ([topy (mline-get-location line)])
                                      (let loop ([snip (mline-snip line)]
                                                 [X X]
                                                 [x x]
                                                 [p p])
                                        (let-boxes ([w 0.0])
                                            (when dc (send snip get-extent dc X topy w #f #f #f #f #f))
                                          (if (and (x . > . w) (snip->next snip) dc)
                                              (loop (snip->next snip)
                                                    (+ X w)
                                                    (- x w)
                                                    (+ p (snip->count snip)))
                                              ;; found the right snip
                                              (let ([s-pos p]
                                                    [p (+ p (do-find-position-in-snip dc X topy snip x how-close-box))])
                                                (set! write-locked? wl?)
                                                (set! flow-locked? fl?)
                                                (values snip s-pos p)))))))))])

                ;; back up over invisibles
                (let ([atsnipend? (- (- p s-pos) (snip->count snip))])
                  (let-boxes ([p p]
                              [snip snip])
                      (when atsnipend?
                        (find-last-visible-position line p snip))
                    (when (and ateol?-box 
                               atsnipend?
                               snip 
                               (eq? snip (mline-last-snip line)))
                      (set-box! ateol?-box #t))
                    p))))))]))

  (define/private (find-first-visible-position line [snip #f])
    (if read-locked?
        0
        (let* ([snip (or snip (mline-snip line))]
               [startp (mline-get-position line)]
               [p startp]
               [next-snip (snip->next (mline-last-snip line))])
          (let loop ([snip snip]
                     [p p])
            (cond
             [(eq? snip next-snip)
              ;; if everything is invisible, then presumably the CR is forced,
              ;; so go to the beginning of the line anyway
              startp]
             [(has-flag? (snip->flags snip) INVISIBLE)
              (loop (snip->next snip) (+ p (snip->count snip)))]
             [else p])))))

  (define/private (find-last-visible-position line p-box [snip-box #f])
    (unless read-locked?
      (let ([snip (or (if snip-box
                          (unbox snip-box)
                          #f)
                      (mline-last-snip line))]
            [p (unbox p-box)])
        (let loop ([p p]
                   [snip snip])
          (let ([p (if (has-flag? (snip->flags snip) INVISIBLE)
                       (- p (snip->count snip))
                       p)])
            (if (eq? snip (mline-snip line))
                (begin
                  (set-box! p-box p)
                  (when snip-box
                    (set-box! snip-box snip)))
                (loop p (snip->prev snip))))))))

  (def/public (find-position-in-line [exact-nonnegative-integer? i]
                                     [real? x]
                                     [maybe-box? [ateol? #f]]
                                     [maybe-box? [onit? #f]]
                                     [maybe-box? [how-close #f]])
    (do-find-position-in-line #f i x ateol? onit? how-close))

  (define/private (do-find-position-in-snip dc X Y snip x how-close)
    (cond
     [read-locked? 0]
     [(x . < . 0)
      (when how-close
        (set-box! how-close -100.0))
      0]
     [else
      (let ([wl? write-locked?]
            [fl? flow-locked?])
        (set! write-locked? #t)
        (set! flow-locked? #t)

        (let ([c (snip->count snip)])
          (if ((send snip partial-offset dc X Y c) . <= . x)
              (begin
                (when how-close
                  (set-box! how-close 100.0))
                (set! write-locked? wl?)
                (set! flow-locked? fl?)
                c)
            
              ;; binary search for position within snip:
              (let loop ([range c]
                         [i (quotient c 2)]
                         [offset 0])
                (let ([dl (send snip partial-offset dc X Y (+ offset i))])
                  (if (dl . > . x)
                      (loop i (quotient i 2) offset)
                      (let ([dr (send snip partial-offset dc X Y (+ offset i 1))])
                        (if (dr . <= . x)
                            (let ([range (- range i)])
                              (loop range (quotient range 2) (+ offset i)))
                            (begin
                              (when how-close
                                (set-box! how-close
                                          (if ((- dr x) . < . (- x dl))
                                              (- dr x)
                                              (- dl x))))
                              (set! write-locked? wl?)
                              (set! flow-locked? fl?)
                              (+ i offset))))))))))]))

  (def/public (find-line [real? y] [maybe-box? [onit? #f]])
    (when onit?
      (set-box! onit? #f))
    
    (let ([y (- y padding-t)])
      (cond
       [(not (check-recalc #t #f)) 0]
       [(y . <= . 0) 0]
       [(or (y . >= . total-height) (and extra-line? (y . >= . (- total-height extra-line-h))))
        (- num-valid-lines (if extra-line? 0 1))]
       [else
        (when onit?
          (set-box! onit? #t))
        (mline-get-line (mline-find-location (unbox line-root-box) y))])))

  (def/public (find-position [real? x] [real? y]
                             [maybe-box? [ateol? #f]]
                             [maybe-box? [onit? #f]]
                             [maybe-box? [how-close #f]])
    (if read-locked?
        0
        (begin
          (when ateol?
            (set-box! ateol? #f))

          (let* ([online (box #f)]
                 [i (find-line y online)])
            (if (and (i . >= . (- num-valid-lines 1))
                     (not (unbox online))
                     (y . > . 0))
                (begin
                  (when onit?
                    (set-box! onit? #f))
                  (when how-close
                    (set-box! how-close 100.0))
                  len)
                (let ([p (find-position-in-line i x ateol? onit? how-close)])
                  (when onit?
                    (set-box! onit? (and (unbox online) (unbox onit?))))
                  p))))))

  (def/public (position-line [exact-nonnegative-integer? start]
                             [any? [eol? #f]])
    (cond
     [(not (check-recalc (max-width . > . 0) #f #t)) 0]
     [(start . <= . 0) 0]
     [(start . >= . len)
      (if (and extra-line? (not eol?))
          num-valid-lines
          (- num-valid-lines 1))]
     [else
      (let* ([line (mline-find-position (unbox line-root-box) start)]
             [line (if (and eol? (= (mline-get-position line) start))
                       (mline-prev line)
                       line)])
        (mline-get-line line))]))

  
  (def/public-final (get-snip-position-and-location [snip% thesnip] [maybe-box? pos] 
                                                    [maybe-box? [x #f]] [maybe-box? [y #f]])
    (cond
     [(not (check-recalc (or x y) #f))
      #f]
     [(or (not (snip->line thesnip))
          (not (eq? (mline-get-root (snip->line thesnip)) (unbox line-root-box))))
      #f]
     [(or pos x y)
      (let* ([line (snip->line thesnip)]
             [p (mline-get-position line)])
        (let loop ([snip (mline-snip line)]
                   [p p])
          (if (eq? snip thesnip)
              (begin
                (when pos
                  (set-box! pos p))
                (when (or x y)
                  (position-location p x y))
                #t)
              (loop (snip->next snip)
                    (+ p (snip->count snip))))))]
     [else #t]))

  (def/override (get-snip-location [snip% thesnip] [maybe-box? [x #f]] [maybe-box? [y #f]] [any? [bottom-right? #f]])
    (let ([x (or x (and bottom-right? (box 0.0)))]
          [y (or y (and bottom-right? (box 0.0)))])
      (if (get-snip-position-and-location thesnip #f x y)
          (if bottom-right?
              (let ([wl? write-locked?]
                    [fl? flow-locked?])
                (set! write-locked? #t)
                (set! flow-locked? #t)
      
                (let ([dc (send s-admin get-dc)])
                  (let-boxes ([w 0.0]
                              [h 0.0])
                      (when dc
                        (send thesnip get-extent dc (unbox x) (unbox y) w h #f #f #f #f))

                    (set! write-locked? wl?)
                    (set! flow-locked? fl?)

                    (set-box! x (+ (unbox x) w))
                    (set-box! y (+ (unbox y) h))
                    #t)))
              #t)
          #f)))

  (def/public (get-snip-position [snip% thesnip])
    (let-boxes ([pos 0])
        (unless (get-snip-position-and-location thesnip pos)
          (set-box! pos #f))
      pos))
  
  (def/public (position-locations [exact-nonnegative-integer? start]
                                  [maybe-box? [tx #f]]
                                  [maybe-box? [ty #f]]
                                  [maybe-box? [bx #f]]
                                  [maybe-box? [by #f]]
                                  [any? [eol? #f]]
                                  [any? [whole-line? #f]])
    (when (check-recalc #t #f)

      ;; handle boundary cases first:
      (let ([line
             (cond
              [(start . <= . 0)
               (if whole-line?
                   (begin
                     (when (or tx bx)
                       (let ([xl (+ (mline-get-left-location first-line max-line-width)
                                    padding-l)])
                         (when tx (set-box! tx xl))
                         (when bx (set-box! bx xl))))
                     (when (or ty by)
                       (let ([yl (+ (mline-get-location first-line)
                                    padding-t)])
                         (when ty (set-box! ty yl))
                         (when by (set-box! by (+ yl (mline-h first-line))))))
                     #f)
                   first-line)]
              [(start . >= . len)
               (if (and extra-line? (not eol?))
                   (begin
                     (when ty (set-box! ty (+ (- total-height extra-line-h) padding-t)))
                     (when by (set-box! by (+ total-height padding-t)))
                     (when tx (set-box! tx padding-l))
                     (when bx (set-box! bx padding-l))
                     #f)
                   (if (or whole-line? (zero? len))
                       (begin
                         (when (or tx bx)
                           (let ([xl (+ (mline-get-right-location last-line max-line-width)
                                        padding-l)])
                             (when tx (set-box! tx xl))
                             (when bx (set-box! bx xl))))
                         (when (or ty by)
                           (let ([yl (+ (mline-get-location last-line)
                                        padding-t)])
                             (when ty (set-box! ty yl))
                             (when by (set-box! by (+ yl (mline-h last-line))))))
                         #f)
                       last-line))]
              [else
               (let ([line (mline-find-line (unbox line-root-box) (position-line start eol?))])
                 (if whole-line?
                     (begin
                       (when (or by ty)
                         (let ([yl (+ (mline-get-location line) padding-t)])
                           (when ty (set-box! ty yl))
                           (when by (set-box! by (+ yl (mline-h line))))))
                       (if (not (or tx bx))
                           #f
                           line))
                     line))])])
        (when line
          (let ([wl? write-locked?]
                [fl? flow-locked?])
            (set! write-locked? #t)
            (set! flow-locked? #t)

            (let ([horiz (+ (mline-get-left-location line max-line-width) padding-l)]
                  [topy (+ (mline-get-location line) padding-t)]
                  [start (- start (mline-get-position line))])
              (let-values ([(snip horiz start dc)
                            (cond
                             [(zero? start) (values (mline-snip line) horiz start #f)]
                             [(start . >= . (mline-len line))
                              (values (mline-last-snip line) (+ horiz (- (mline-w line) (mline-last-w line)))
                                      start #f)]
                             [else
                              ;; linear search for snip
                              (let loop ([snip (mline-snip line)]
                                         [start start]
                                         [horiz horiz]
                                         [dc #f])
                                (if (or (start . > . (snip->count snip))
                                        (and (or whole-line? (positive? start))
                                             (= start (snip->count snip))))
                                    (let* ([start (- start (snip->count snip))]
                                           [dc (or dc (send s-admin get-dc))])
                                      (let-boxes ([v 1.0])
                                          (when dc
                                            (send snip get-extent dc horiz topy v #f #f #f #f #f))
                                        (loop (snip->next snip) start (+ horiz v) dc)))
                                    ;; found snip
                                    (values snip horiz start dc)))])])
                (let ([dc
                       (if (or tx bx)
                           (let ([dc (or dc
                                         (and (positive? start)
                                              (send s-admin get-dc)))])
                             (let ([xv (+ horiz
                                          (if (and dc (positive? start))
                                              (send snip partial-offset dc horiz topy start)
                                              0))])
                               (when tx (set-box! tx xv))
                               (when bx (set-box! bx xv)))
                             dc)
                           dc)])
                  (when (and (not whole-line?)
                             (or ty by))
                    (let ([dc (or dc (send s-admin get-dc))])
                      (let-boxes ([h 0.0]
                                  [descent 0.0]
                                  [space 0.0])
                          (when dc
                            (send snip get-extent dc horiz topy #f h descent space #f #F))
                        (let ([align (send (snip->style snip) get-alignment)])
                          (cond
                           [(eq? 'bottom align)
                            (let ([yl (+ topy (mline-bottombase line) descent)])
                              (when ty (set-box! ty (- yl h)))
                              (when by (set-box! by yl)))]
                           [(eq? 'top align)
                            (let ([yl (- (+ topy (mline-topbase line)) space)])
                              (when ty (set-box! ty yl))
                              (when by (set-box! by (+ yl h))))]
                           [else
                            (let* ([h (/ (- h descent space) 2)]
                                   [yl (+ topy (/ (+ (mline-topbase line) (mline-bottombase line)) 2))])
                              (when ty (set-box! ty (- yl h space)))
                              (when by (set-box! by (+ yl h descent))))])))))

                  (set! write-locked? wl?)
                  (set! flow-locked? fl?)))))))))

  (def/public (position-location [exact-nonnegative-integer? start]
                                 [maybe-box? [x #f]]
                                 [maybe-box? [y #f]]
                                 [any? [top? #t]]
                                 [any? [eol? #f]]
                                 [any? [whole-line? #f]])
    (position-locations start
                        (if top? x #f) (if top? y #f)
                        (if top? #f x) (if top? #f y)
                        eol? whole-line?))

  (def/public (line-location [exact-nonnegative-integer? i]
                             [any? [top? #t]])
    (cond
     [(not (check-recalc #t #f)) 0.0]
     [(i . < . 0) padding-t]
     [(i . > . num-valid-lines) (+ padding-t total-height)]
     [(= num-valid-lines i)
      (+ padding-t
         (if extra-line?
             (- total-height extra-line-h)
             total-height))]
     [else
      (let* ([line (mline-find-line (unbox line-root-box) i)]
             [y (mline-get-location line)])
        (+ padding-t
           (if top?
               y
               (+ y (mline-h line)))))]))

  (define/private (do-line-position start? i visible-only?)
    (cond
     [(not (check-recalc (max-width . > . 0) #f #t))
      0]
     [(and (i . >= . num-valid-lines) extra-line?)
      len]
     [else (let* ([i (max 0 (min i (sub1 num-valid-lines)))]
                  [line (mline-find-line (unbox line-root-box) i)])
             (if start?
                 (if visible-only?
                     (find-first-visible-position line)
                     (mline-get-position line))
                 (let ([p (+ (mline-get-position line) (mline-len line))])
                   (if visible-only?
                       (let-boxes ([p p])
                           (find-last-visible-position line p)
                         p)
                       p))))]))

  (def/public (line-start-position [exact-nonnegative-integer? i]
                                   [any? [visible-only? #t]])
    (do-line-position #t i visible-only?))

  (def/public (line-end-position [exact-nonnegative-integer? i]
                                 [any? [visible-only? #t]])
    (do-line-position #f i visible-only?))


  (def/public (line-length [exact-nonnegative-integer? i])
    (cond
     [(not (check-recalc (max-width . > . 0) #f #t))
      0]
     [(i . < . 0) 0]
     [(i . >= . num-valid-lines) 0]
     [else (let ([line (mline-find-line (unbox line-root-box) i)])
             (mline-len line))]))

  (def/public (position-paragraph [exact-nonnegative-integer? i]
                                  [any? [at-eol? #f]])
    (cond
     [(not (check-recalc #f #f #t)) 0]
     [else (let ([delta (if (and (i . >= . len) extra-line?)
                            1
                            0)]
                 [i (max 0 (min i len))])
             (let ([line (mline-find-position (unbox line-root-box) i)])
               (+ (mline-get-paragraph line) delta)))]))

  (def/public (paragraph-start-position [exact-nonnegative-integer? i]
                                        [any? [visible-only? #t]])
    (if (not (check-recalc #f #f #t))
        0
        (if (i . > . (+ (last-paragraph) (if extra-line? -1 0)))
            len
            (let* ([i (max 0 i)]
                   [l (mline-find-paragraph (unbox line-root-box) i)]
                   [l (if (not l)
                          (if extra-line?
                              len
                              (let loop ([l last-line])
                                (if (and (mline-prev l)
                                         (not (mline-starts-paragraph l)))
                                    (loop (mline-prev l))
                                    l)))
                          l)])
              (if visible-only?
                  (find-first-visible-position l)
                  (mline-get-position l))))))

  (def/public (paragraph-end-position [exact-nonnegative-integer? i]
                                      [any? [visible-only? #t]])
    (if (not (check-recalc #f #f #t))
        0
        (if (i . > . (+ (last-paragraph) (if extra-line? -1 0)))
            len
            (let* ([i (max 0 i)]
                   [l (mline-find-paragraph (unbox line-root-box) i)]
                   [l (if l
                          (let loop ([l l])
                            (if (and (mline-next l)
                                     (zero? (mline-starts-paragraph (mline-next l))))
                                (loop (mline-next l))
                                l))
                          (if extra-line?
                              len
                              last-line))])
              (if (mline? l)
                  (let ([p (+ (mline-get-position l) (mline-len l))])
                    (if visible-only?
                        (let-boxes ([p p])
                            (find-last-visible-position l p)
                          p)
                        p))
                  l)))))

  (def/public (line-paragraph [exact-nonnegative-integer? i])
    (cond
     [(not (check-recalc (max-width . > . 0) #f #t))
      0]
     [(i . < . 0) 0]
     [(i . >= . num-valid-lines)
      (+ (mline-get-paragraph last-line) (if extra-line? 1 0))]
     [else
      (let ([l (mline-find-line (unbox line-root-box) i)])
        (mline-get-paragraph l))]))

  (def/public (paragraph-start-line [exact-nonnegative-integer? i])
    (if (not (check-recalc (max-width . > . 0) #f #t))
        0
        (let* ([i (max i 0)]
               [l (mline-find-paragraph (unbox line-root-box) i)])
          (if (not l)
              (last-line)
              (mline-get-line l)))))

  (def/public (paragraph-end-line [exact-nonnegative-integer? i])
    (if (not (check-recalc (max-width . > . 0) #f #t))
        0
        (let* ([i (max i 0)]
               [l (mline-find-paragraph (unbox line-root-box) i)])
          (mline-get-line
           (if l
               (let loop ([l l])
                 (if (and (mline-next l)
                          (not (mline-starts-paragraph (mline-next l))))
                     (loop (mline-next l))
                     l))
               last-line)))))

  (def/public (last-position) len)
  
  (public [/last-line last-line])
  (define (/last-line)
    (if (not (check-recalc (max-width . > . 0) #f #t))
        0
        (- num-valid-lines (if extra-line? 0 1))))

  (def/public (last-paragraph)
    (if (not (check-recalc #f #f #t))
        0
        (+ (mline-get-paragraph last-line) (if extra-line? 1 0))))

  ;; ----------------------------------------

  (def/override (get-extent [maybe-box? w] [maybe-box? h])
    (check-recalc #t #f)
    (when w (set-box! w (+ total-width padding-l padding-r)))
    (when h (set-box! h (+ total-height padding-t padding-b))))

  (def/override (get-descent)
    (check-recalc #t #f)
    (+ final-descent padding-b))
  
  (def/override (get-space)
    (check-recalc #t #f)
    (+ initial-space padding-t))
  
  (def/public (get-top-line-base)
    (check-recalc #t #f)
    (+ initial-line-base padding-t))
  
  (def/override (scroll-line-location [exact-nonnegative-integer? scroll])
    (if read-locked?
        0.0
        (begin
          (check-recalc #t #f)
          (+ padding-t
             (let ([total (+ (mline-get-scroll last-line) (mline-numscrolls last-line))])
               (cond
                [(= total scroll)
                 (if extra-line?
                     (- total-height extra-line-h)
                     (+ total-height padding-b))]
                [(scroll . > . total)
                 (+ total-height padding-b)]
                [else
                 (let* ([line (mline-find-scroll (unbox line-root-box) scroll)]
                        [p (mline-get-scroll line)]
                        [y (mline-get-location line)])
                   (if (p . < . scroll)
                       (+ y (mline-scroll-offset line (- scroll p)))
                       y))]))))))

  (def/override (num-scroll-lines)
    (if read-locked?
        0
        (begin
          (check-recalc (max-width . > . 0) #f #t)
          (+ (mline-get-scroll last-line)
             (mline-numscrolls last-line)
             (if extra-line? 1 0)))))

  (def/override (find-scroll-line [real? p])
    (if read-locked?
        0
        (begin
          (check-recalc #t #f)
          (if (and extra-line?
                   (p . >= . (- total-height extra-line-h padding-t)))
              (- (num-scroll-lines) 1)
              (let* ([line (mline-find-location (unbox line-root-box) (- p padding-t))]
                     [s (mline-get-scroll line)])
                (if ((mline-numscrolls line) . > . 1)
                    (let ([y (+ (mline-get-location line) padding-t)])
                      (+ s (mline-find-extra-scroll line (- p y))))
                    s))))))

  ;; ----------------------------------------

  (def/public (find-string [string? str]
                           [(symbol-in forward backward) [direction 'forward]]
                           [(make-alts exact-nonnegative-integer? (symbol-in start)) [start 'start]]
                           [(make-alts exact-nonnegative-integer? (symbol-in eof)) [end 'eof]]
                           [any? [bos? #t]]
                           [any? [case-sens? #t]])
    (if (not (check-recalc #f #f))
        #f
        (do-find-string-all str direction start end #t bos? case-sens?)))

  (def/public (find-string-all [string? str]
                               [(symbol-in forward backward) [direction 'forward]]
                               [(make-alts exact-nonnegative-integer? (symbol-in start)) [start 'start]]
                               [(make-alts exact-nonnegative-integer? (symbol-in eof)) [end 'eof]]
                               [any? [bos? #t]]
                               [any? [case-sens? #t]])
    (if (not (check-recalc #f #f))
        null
        (reverse (do-find-string-all str direction start end #f bos? case-sens?))))

  (def/public (find-newline [(symbol-in forward backward) [direction 'forward]]
                            [(make-alts exact-nonnegative-integer? (symbol-in start)) [start 'start]]
                            [(make-alts exact-nonnegative-integer? (symbol-in eof)) [end 'eof]])
    (let* ([para (position-paragraph (if (symbol? start)
                                         startpos
                                         start)
                                     (eq? direction 'backward))]
           [pos (if (eq? direction 'backward)
                    (paragraph-start-position para)
                    (if (para . >= . (last-paragraph))
                        len
                        (paragraph-start-position (add1 para))))]
           [end (if (symbol? end) len end)])
      (if (eq? direction 'forward)
          (if (pos . > . end)
              #f
              pos)
          (if (pos . < . end)
              #f
              pos))))

  (define/private (do-find-string-all str direction
                                      start end
                                      just-one?
                                      bos?
                                      case-sens?)

    (let ([start (min (if (symbol? start)
                          startpos
                          start)
                      len)]
          [end (min (if (symbol? end)
                        (if (eq? direction 'forward)
                            len
                            0)
                        end)
                    len)])
      (let ([total-count
             (if (eq? direction 'backward)
                 (- start end)
                 (- end start))])
        (if (or (negative? total-count)
                (string=? str ""))
            (if just-one? #f null)
            
            (let ([slen (string-length str)]
                  [str (if case-sens?
                           str
                           (string-foldcase str))])
              (let-values ([(snip s-pos) (find-snip/pos start (if (eq? direction 'forward) 'after 'before))])
                
                (if (not snip)
                    (if just-one? #f null)

                    ;; Knuth-Bendix

                    (let-values ([(offset shorten sbase beyond sgoal direction)
                                  (if (eq? direction 'forward)
                                      (values (- start s-pos) 0 0 -1 slen 1)
                                      (values 0 (- (+ s-pos (snip->count snip)) start) (- slen 1) slen -1 -1))]
                                 [(smap) (make-vector slen 0)])

                      ;; initialize smap:
                      (vector-set! smap sbase beyond)
                      (let loop ([s beyond]
                                 [i (+ sbase direction)])
                        (unless (= i sgoal)
                          (let iloop ([s s])
                            (if (and (not (= beyond s))
                                     (not (char=? (string-ref str (+ s direction)) (string-ref str i))))
                                (iloop (vector-ref smap s))
                                (let ([s (if (char=? (string-ref str (+ s direction))
                                                     (string-ref str i))
                                             (+ s direction)
                                             s)])
                                  (vector-set! smap i s)
                                  (loop s (+ i direction)))))))
                      (define text "")
                      (let a-loop ([s beyond]
                                   [s-pos s-pos]
                                   [snip snip]
                                   [total-count total-count]
                                   [offset offset]
                                   [shorten shorten]
                                   [results null])
                        (if (and snip (positive? total-count))
                            (let*-values ([(need) (- (snip->count snip) shorten offset)]
                                          [(need offset)
                                           (if (need . > . total-count)
                                               (if (direction . < . 0)
                                                   (values total-count (+ offset (- need total-count)))
                                                   (values total-count offset))
                                               (values need offset))]
                                          [(total-count) (- total-count need)])
                              
                              (let b-loop ([checked 0]
                                           [need need]
                                           [results results])
                                (let* ([thistime (min need 255)]
                                       [need (- need thistime)]
                                       [thisoffset (+ offset (if (direction . < . 0) need checked))]
                                       [wl? write-locked?]
                                       [fl? flow-locked?])
                                  (when (< (string-length text) (send snip get-count))
                                    (set! text (make-string (send snip get-count))))
                                  (set! write-locked? #t)
                                  (set! flow-locked? #t)
                                  (send snip get-text! text thisoffset thistime 0)
                                    (set! write-locked? wl?)
                                    (set! flow-locked? fl?)
                                    
                                    (let c-loop ([i (if (direction . > . 0) 0 (- thistime 1))]
                                                 [n thistime]
                                                 [s s]
                                                 [results results])
                                      (if (zero? n)
                                          (if (positive? need)
                                              
                                              (b-loop (add1 checked)
                                                      need
                                                      results)

                                              (let* ([s-pos (if (direction . > . 0)
                                                                (+ s-pos (snip->count snip))
                                                                s-pos)]
                                                     [snip (if (direction . > . 0)
                                                               (snip->next snip)
                                                               (snip->prev snip))]
                                                     [s-pos (if (and snip (direction . < . 0))
                                                                (- s-pos (snip->count snip))
                                                                s-pos)])
                                                (a-loop s
                                                        s-pos
                                                        snip
                                                        total-count
                                                        0
                                                        0
                                                        results)))

                                          (let* ([n (sub1 n)]
                                                 [c (string-ref text i)]
                                                 [c (if case-sens? c (char-foldcase c))]
                                                 [s (let loop ([s s])
                                                      (if (and (not (= beyond s))
                                                               (not (char=? (string-ref str (+ s direction)) c)))
                                                          (loop (vector-ref smap s))
                                                          s))])
                                            (if (char=? (string-ref str (+ s direction)) c)
                                                (let ([s (+ s direction)])
                                                  (if (= (+ s direction) sgoal)
                                                      (let* ([p (+ s-pos i thisoffset)]
                                                             [p (if bos?
                                                                    (if (direction . < . 0)
                                                                        (+ p slen)
                                                                        (- p (- slen 1)))
                                                                    (if (direction . > . 0)
                                                                        (add1 p)
                                                                        p))])
                                                        (if just-one?
                                                            p ;; <------ single result returned here
                                                            (c-loop (+ i direction)
                                                                    n
                                                                    beyond
                                                                    (cons p results))))
                                                      (c-loop (+ i direction)
                                                              n
                                                              s
                                                              results)))
                                                (c-loop (+ i direction)
                                                        n
                                                        s
                                                        results))))))))
                            (if just-one?
                                #f
                                results)))))))))))

  ;; ----------------------------------------
  
  (define/private (do-change-style start end new-style delta restore-sel? counts-as-mod?)
    (assert (consistent-snip-lines 'do-change-style))
    (unless (or write-locked?
                s-user-locked?
                (and new-style
                     (not (send s-style-list style-to-index new-style))))
      (let* ([start (max 0 (min len start))]
             [end (min end len)])
        (unless (start . > . end)
          (let ([new-style (if (and (not new-style) (not delta))
                               (or (get-default-style)
                                   (send s-style-list basic-style))
                               new-style)])
            (cond
             [(and (= start startpos) (= end endpos) (= end start) (positive? len))
              (when sticky-styles?
                (set! caret-style 
                      (cond
                       [new-style new-style]
                       [caret-style (send s-style-list find-or-create-style caret-style delta)]
                       [else (let ([gsnip (do-find-snip start 'before)])
                               (send s-style-list find-or-create-style (snip->style gsnip) delta))])))]
             [else
              (set! write-locked? #t)

              (if (not (can-change-style? start (- end start)))
                  (set! write-locked? #f)
                  
                  (begin
                    (on-change-style start (- end start))
                    
                    (set! flow-locked? #t)

                    (make-snipset start end)
                    
                    (let-values ([(start-snip end-snip)
                                  (if (zero? len)
                                      (begin
                                        (set! initial-style-needed? #f)
                                        (values snips #f))
                                      (values (do-find-snip start 'after) (do-find-snip end 'after-or-none)))]
                                 [(rec)
                                  (and (zero? s-noundomode)
                                       (make-object style-change-record% start end 
                                                    (or delayed-streak? (not s-modified?))
                                                    startpos endpos restore-sel?))])
                      (let loop ([something? #f]
                                 [extra-check-pos #f]
                                 [prev-style #f]
                                 [prev-style-pos start]
                                 [p start]
                                 [gsnip start-snip])
                        (if (not (eq? gsnip end-snip))
                            ;; Change a snip style:
                            (let* ([style (snip->style gsnip)]
                                   [style2 (or new-style
                                               (send s-style-list find-or-create-style style delta))])
                              (if (not (eq? style style2))
                                  (begin
                                    (set-snip-style! gsnip style2)
                                    (let-values ([(prev-style prev-style-pos)
                                                  (if (and rec (not (eq? prev-style style)))
                                                      (begin
                                                        (when prev-style
                                                          (send rec add-style-change prev-style-pos p prev-style))
                                                        (values style p))
                                                      (values prev-style prev-style-pos))])
                                      (send gsnip size-cache-invalid)
                                      (mline-mark-recalculate (snip->line gsnip))
                                      (when (max-width . > . 0)
                                        (mline-mark-check-flow (snip->line gsnip)))
                                      (loop #t
                                            p
                                            prev-style
                                            prev-style-pos
                                            (+ p (snip->count gsnip))
                                            (snip->next gsnip))))
                                  (let ([prev-style
                                         (if (and rec prev-style)
                                             (begin
                                               (send rec add-style-change prev-style-pos p prev-style)
                                               #f)
                                             prev-style)])
                                    (loop something?
                                          extra-check-pos
                                          prev-style
                                          prev-style-pos
                                          (+ p (snip->count gsnip))
                                          (snip->next gsnip)))))
                            ;; All snips changed
                            (begin
                              (when (and rec prev-style)
                                (send rec add-style-change prev-style-pos p prev-style))

                              (if something?
                                  ;; Something changed, so recalc and refresh:
                                  (let ([line (snip->line start-snip)])
                                    (when (and (mline-prev line)
                                               (not (has-flag? (snip->flags (mline-snip (mline-prev line))) HARD-NEWLINE)))
                                      (mline-mark-check-flow (mline-prev line)))
                                    (when (not s-modified?)
                                      (add-undo-rec (make-object unmodify-record% delayed-streak?)))
                                    (when rec
                                      (add-undo-rec rec))
                                    (when (in-edit-sequence?)
                                      (set! delayed-streak? #t))
                                    
                                    (check-merge-snips start)
                                    (when extra-check-pos
                                      (check-merge-snips extra-check-pos))
                                    (when (not (= end extra-check-pos))
                                      (check-merge-snips end))
                                    
                                    (when (and (not s-modified?) counts-as-mod?)
                                      (set-modified #t))

                                    (set! write-locked? #f)
                                    (set! flow-locked? #f)
                                  
                                    (refresh-by-line-demand))
                                  ;; Nothing changed after all:
                                  (begin
                                    (set! write-locked? #f)
                                    (set! flow-locked? #f)

                                    (check-merge-snips start)
                                    (check-merge-snips end)))

                              (after-change-style start (- end start))))))))]))))
      (assert (consistent-snip-lines 'post-do-change-style))))

  (def/public (change-style [(make-or-false (make-alts style<%> style-delta%)) st]
                            [(make-alts exact-nonnegative-integer? (symbol-in start)) [start 'start]]
                            [(make-alts exact-nonnegative-integer? (symbol-in end)) [end 'end]]
                            [any? [counts-as-mod? #t]])
    (do-change-style (if (symbol? start) startpos start)
                     (if (symbol? end) (if (symbol? start) endpos len) end)
                     (and (st . is-a? . style<%>) st)
                     (and (st . is-a? . style-delta%) st)
                     1
                     counts-as-mod?))

  (def/override (set-style-list [style-list% new-list])
    (unless write-locked?
      (let ([delta (new style-delta%)]
            [count (send s-style-list number)])
        (when (positive? count)
          (let ([smap (make-vector count #f)])
            (vector-set! smap 0 (send new-list index-to-style 0))
            (for ([index (in-range 1 count)])
              (let* ([style (send s-style-list index-to-style index)]
                     [name (send style get-name)])
                (vector-set!
                 smap
                 index
                 (cond
                  [(and name (send new-list find-named-style name))
                   => (lambda (new-style) new-style)]
                  [else
                   (let ([new-style
                          (let* ([base-style (send style get-base-style)]
                                 [base-index (send s-style-list style-to-index base-style)])
                            (if (send style is-join?)
                                (let* ([ss (send style get-shift-style)]
                                       [shift-index (send s-style-list style-to-index ss)])
                                  (send new-list find-or-create-join-style 
                                        (vector-ref smap base-index)
                                        (vector-ref smap shift-index)))
                                (begin
                                  (send style get-delta delta)
                                  (send new-list find-or-create-style 
                                        (vector-ref smap base-index)
                                        delta))))])
                     (if name
                         (send new-list new-named-style name new-style)
                         new-style))]))))
            (let loop ([snip snips])
              (when snip
                (let* ([index (send s-style-list style-to-index (snip->style snip))]
                       [index (if (not index)
                                  ;; bad! snip had style not from this buffer's style list
                                  0
                                  index)])
                  (set-snip-style! snip (vector-ref smap index)))
                (loop (snip->next snip))))))
          
        (super set-style-list new-list)
        
        (size-cache-invalid)
        (set! changed? #t)
        (need-refresh -1 -1))))

  (def/override (style-has-changed [(make-or-false style<%>) style])
    (unless read-locked?
      (if (not style)
          ;; our cue to repaint
          (begin
            (set! changed? #t)
            (need-refresh -1 -1))
          ;; notify snips:
          (let ([wl? write-locked?]
                [fl? flow-locked?])
            (set! write-locked? #t)
            (set! flow-locked? #t)

            (let loop ([snip snips])
              (when snip
                (when (eq? style (snip->style snip))
                  (send snip size-cache-invalid)
                  (let ([line (snip->line snip)])
                    (mline-mark-recalculate line)
                    (when (max-width . >= . 0)
                      (mline-mark-check-flow line)
                      (when (and (mline-prev line)
                                 (not (has-flag? (snip->flags (mline-last-snip (mline-prev line)))
                                                 HARD-NEWLINE)))
                        (mline-mark-check-flow (mline-prev line))))))
                (loop (snip->next snip))))
            (set! write-locked? wl?)
            (set! flow-locked? fl?)))))

  ;; ----------------------------------------

  (define/private (do-scroll-to snip localx localy w h refresh? [bias 'none])
    (cond
     [flow-locked? #f]
     [(in-edit-sequence?)
      (when s-admin
        (set! delayedscroll -1)
        (set! delayedscrollbox? #t)
        (set! delayedscrollsnip snip)
        (set! delayedscroll-x localx)
        (set! delayedscroll-y localy)
        (set! delayedscroll-w w)
        (set! delayedscroll-h h)
        (set! delayedscrollbias bias))
      #f]
     [else
      (let-boxes ([x 0.0]
                  [y 0.0]
                  [ok? #t])
          (when snip
            (set-box! ok? (get-snip-position-and-location snip #f x y)))
        (cond
         [(not ok?) #f]
         [(scroll-editor-to (+ x localx) (+ y localy) w h refresh? bias)
          (unless refresh?
            (set! refresh-all? #t))
          #t]
         [else #f]))]))

  (define/override (scroll-editor-to localx localy w h refresh? bias)
    (super scroll-editor-to 
           (- localx padding-l)
           (- localy padding-t)
           (+ w padding-l padding-r)
           (+ h padding-t padding-b)
           refresh? bias))

  (def/public (scroll-to [snip% snip] [real? localx] [real? localy]
                         [nonnegative-real? w] [nonnegative-real? h]
                         [any? refresh?]
                         [(symbol-in start end none) [bias 'none]])
    (do-scroll-to snip localx localy w h refresh? bias))

  (def/override (resized [snip% snip] [any? redraw-now?])
    (when (get-snip-position-and-location snip #f #f #f)

      (let ([line (snip->line snip)])
        (mline-mark-recalculate line)
        (when (max-width . >= . 0)
          (mline-mark-check-flow line)
          ;; maybe something can now move to the previous line
          (when (and (mline-prev line)
                     (not (has-flag? (snip->flags (mline-last-snip (mline-prev line)))
                                     HARD-NEWLINE)))
            (mline-mark-check-flow (mline-prev line)))))

      (set! graphic-maybe-invalid? #t)

      (let ([redraw-now? (and redraw-now?
                              (not flow-locked?))])

        (set! changed? #t)

        (unless redraw-now? (set! delay-refresh (add1 delay-refresh)))
        (parameterize ([in-delayed-refresh #f])
          (refresh-by-line-demand))
        (unless redraw-now? (set! delay-refresh (sub1 delay-refresh))))))

  (def/override (recounted [snip% snip] [any? redraw-now?])
    (if write-locked?
        #f
        (begin
          (set! revision-count (add1 revision-count))
          (resized snip redraw-now?)
          #t)))

  (def/override (set-caret-owner [(make-or-false snip%) snip] 
                                 [(symbol-in immediate display global) [dist 'immediate]])
    (when (do-set-caret-owner snip dist)
      (need-refresh startpos endpos) ;; (need-caret-refresh); <- doesn't work; local caret ownership weirdness
      (on-focus (not snip))))

  (def/override (release-snip [snip% snip])
    (let ([pos (get-snip-position snip)])
      (and pos
           (begin
             (do-delete pos (+ pos (snip->count snip)) #f #f)
             (when (and (not (snip->admin snip))
                        (has-flag? (snip->flags snip) OWNED))
               (set-snip-flags! snip (remove-flag (snip->flags snip) OWNED)))
             #t))))
  
  (define/public (refresh-box L T w h)
    ;; This method can be called while updating is locked out,
    ;; possibly because another thread is in an edit sequence.
    (call-with-semaphore
     refresh-box-lock
     (lambda ()
       (let ([B (if (eq? h 'display-end) h (+ T h))]
             [R (if (eq? w 'display-end) w (+ L w))])
         (if refresh-box-unset?
             (begin
               (set! refresh-l L)
               (set! refresh-r R)
               (set! refresh-t T)
               (set! refresh-b B)
               (set! refresh-box-unset? #f))
             (begin
               (when (L . < . refresh-l)
                 (set! refresh-l L))
               (unless (eq? refresh-r 'display-end)
                 (when (or (eq? R 'display-end)
                           (R . > . refresh-r))
                   (set! refresh-r R)))
               (when (T . < . refresh-t)
                 (set! refresh-t T))
               (unless (eq? refresh-b 'display-end)
                 (when (or (eq? B 'display-end)
                           (B . > . refresh-b))
                   (set! refresh-b B)))))
         
         (set! draw-cached-in-bitmap? #f)))))

  (def/override (needs-update [snip% snip]
                              [real? localx] [real? localy]
                              [nonnegative-real? w] [nonnegative-real? h])
    (let-boxes ([x 0.0]
                [y 0.0]
                [ok? #t])
        (set-box! ok? (get-snip-location snip x y))
      (when ok?
        (refresh-box (+ x localx) (+ y localy) w h)
        (unless (in-edit-sequence?)
          (redraw)))))

  (def/override (invalidate-bitmap-cache [real? [x 0.0]] 
                                         [real? [y 0.0]]
                                         [(make-alts nonnegative-real? (symbol-in end display-end)) [w 'end]]
                                         [(make-alts nonnegative-real? (symbol-in end display-end)) [h 'end]])
    (let ([w (if (eq? w 'end) (- (+ total-width padding-l padding-r) x) w)]
          [h (if (eq? h 'end) (- (+ total-height padding-t padding-b) y) h)])

      (refresh-box x y w h)
      (unless (in-edit-sequence?)
        (redraw))))

  (def/public (hide-caret [any? hide?])
    (unless (eq? hilite-on? (not hide?))
      (set! hilite-on? (not hide?))
      (when (or s-own-caret? (not (= endpos startpos)))
        (need-caret-refresh))))

  (def/public (caret-hidden?) (not hilite-on?))

  (def/public (get-between-threshold) between-threshold)

  (def/public (set-between-threshold [nonnegative-real? t])
    (set! between-threshold (min t 99.0)))

  ;; ----------------------------------------

  (define/private (make-only-snip)
    (set! snips (new string-snip%))
    (set-snip-style! snips (or (get-default-style)
                               (send s-style-list basic-style)))
    (set-snip-count! snips 0)
    (send snips set-s-admin snip-admin)

    (let ([line (create-mline)])
      (set-snip-line! snips line)
      (set-box! line-root-box line)
      (set! first-line line)
      (set! last-line line)
      (mline-set-starts-paragraph line #t)

      (set-mline-snip! line snips)
      (set-mline-last-snip! line snips)

      (set! last-snip snips)
      (set! snip-count 1)

      (set! num-valid-lines 1)))

  (define/private (splice-snip snip prev next)
    (if prev
        (set-snip-next! prev snip)
        (set! snips snip))
    (set-snip-prev! snip prev)
    (set-snip-next! snip next)
    (if next
        (set-snip-prev! next snip)
        (set! last-snip snip)))

  (define/private (insert-snip before snip)
    (if (and (eq? snips last-snip) (zero? (snip->count snips)))
        (append-snip snip)
        (begin
          (splice-snip snip (snip->prev before) before)
          (set! snip-count (add1 snip-count)))))
  
  (define/private (append-snip snip)
    (if (and (eq? snips last-snip) (zero? (snip->count snips))) 
        ;; get rid of empty snip
        (begin
          (set! snips snip)
          (set! last-snip snip))
        (begin
          (splice-snip snip last-snip #f)
          (set! snip-count (add1 snip-count)))))
  
  (define/private (delete-snip snip)
    (when (eq? snip prev-mouse-snip)
      (set! prev-mouse-snip #f))
    (cond
     [(snip->next snip)
      (splice-snip (snip->next snip) (snip->prev snip) (snip->next (snip->next snip)))]
     [(snip->prev snip)
      (splice-snip (snip->prev snip) (snip->prev (snip->prev snip)) (snip->next snip))]
     [else
      (set! last-snip #f)
      (set! snips #f)])
    (set! snip-count (sub1 snip-count))
    (set-snip-flags! snip (add-flag (snip->flags snip) CAN-DISOWN))
    (snip-set-admin snip #f)
    (set-snip-line! snip #f)
    (set-snip-prev! snip #f)
    (set-snip-next! snip #f)
    (set-snip-flags! snip (remove-flag (snip->flags snip) CAN-DISOWN)))
  
  (define/private (snip-set-admin snip a)
    (let ([orig-count (snip->count snip)]
          [line (snip->line snip)]
          [orig-admin (snip->admin snip)]
          [wl? write-locked?]
          [fl? flow-locked?])

      (set! read-locked? #t)
      (set! write-locked? #t)
      (set! flow-locked? #t)

      (send snip set-admin a)
      
      (set! read-locked? #f) 
      (set! write-locked? wl?) 
      (set! flow-locked? fl?)

      (let ([snip
             (if (not (eq? (snip->admin snip) a))
                 ;; something went wrong
                 (cond
                  [(and (not a) (eq? (snip->admin snip) orig-admin))
                   ;; force admin to NULL
                   (send snip set-s-admin #f)
                   snip]
                  [a
                   ;; snip didn't accept membership into this editor; give up on it
                   (let ([naya (new snip%)])
                     (set-snip-count! naya orig-count)
                     (splice-snip naya (snip->prev snip) (snip->next snip))
                     (set-snip-line! naya line)
                     
                     (when line
                       (when (eq? (mline-snip line) snip)
                         (set-mline-snip! line naya))
                       (when (eq? (mline-last-snip line) snip)
                         (set-mline-last-snip! line naya)))
                     
                     (send snip set-s-admin #f)
                     
                     (send naya set-admin a)
                     (set! snip naya)
                     naya)]
                  [else snip])
                 snip)])

        ;; force count to be consistent:
        (when (and a (not (= (snip->count snip) orig-count)))
          (set-snip-count! snip orig-count))

        snip)))

  (define/private (snip-split snip pos a-ptr b-ptr)
    (let ([c (snip->count snip)]
          [nl? (has-flag? (snip->flags snip) NEWLINE)]
          [hnl? (has-flag? (snip->flags snip) HARD-NEWLINE)]
          [orig snip])

      (set-snip-flags! snip (add-flag (snip->flags snip) CAN-SPLIT))

      (delete-snip snip)

      (set-snip-flags! orig (remove-flag (snip->flags orig) OWNED))

      (set! revision-count (add1 revision-count))

      (let ([wl? write-locked?]
            [fl? flow-locked?])

        (set! read-locked? #t)
        (set! write-locked? #t)
        (set! flow-locked? #t)      

        (send snip split pos a-ptr b-ptr)

        (set! read-locked? #f)
        (set! write-locked? wl?)
        (set! flow-locked? fl?))

      (let* ([a (or (unbox a-ptr)
                    (new snip%))]
             [a (if (send a is-owned?)
                    (new snip%)
                    a)]
             [b (or (unbox b-ptr)
                    (new snip%))]
             [b (if (send b is-owned?)
                    (new snip%)
                    b)])

        (set-box! a-ptr a)
        (set-box! b-ptr b)

        (set-snip-flags! a (remove-flag (snip->flags a) CAN-SPLIT))
        (set-snip-flags! b (remove-flag (snip->flags b) CAN-SPLIT))
        (set-snip-flags! orig (remove-flag (snip->flags orig) CAN-SPLIT))

        ;; make sure that count is right
        (set-snip-count! a pos)
        (set-snip-count! b (- c pos))

        ;; make sure that NEWLINE & HARD-NEWLINE is consistent:
        (when nl?
          (set-snip-flags! b (add-flag (snip->flags b) NEWLINE)))
        (when hnl?
          (set-snip-flags! b (add-flag (snip->flags b) HARD-NEWLINE)))
        (set-snip-flags! a (remove-flag (remove-flag (snip->flags b) NEWLINE)
                                        HARD-NEWLINE)))))

    (define/private (split-one pos s-pos snip extra)
      (let ([line (snip->line snip)]
            [prev (snip->prev snip)]
            [next (snip->next snip)]
            [style (snip->style snip)])
        (let ([at-start? (eq? (mline-snip line) snip)]
              [at-end? (eq? (mline-last-snip line) snip)]
              [orig snip])
          (let-boxes ([ins-snip #f]
                      [snip #f])
              (snip-split orig (- pos s-pos) ins-snip snip)

            (set-snip-style! snip style)
            (set-snip-style! ins-snip style)
      
            (set-snip-line! snip line)
            (set-snip-line! ins-snip line)

            (when at-start?
              (set-mline-snip! line ins-snip))
            (when at-end?
              (set-mline-last-snip! line snip))
      
            (splice-snip snip prev next)
            (set! snip-count (add1 snip-count))
            (insert-snip snip ins-snip)
            (when extra
              (extra snip))
                  
            (snip-set-admin snip snip-admin)
            (snip-set-admin ins-snip snip-admin)
            
            (after-split-snip (- pos s-pos))))))

  (define/private (make-snipset start end) 
    ;; BEWARE: `len' may not be up-to-date
    (when (positive? start)
      (let-values ([(snip s-pos) (find-snip/pos start 'after-or-none)])
        (when snip
          (unless (= s-pos start)
            (split-one start s-pos snip #f)))))
    (when (positive? end)
      (let-values ([(snip s-pos) (find-snip/pos end 'before)])
        (unless (= (+ s-pos (snip->count snip)) end)
          (split-one end s-pos snip #f)))))

  (define/private (insert-text-snip start style)
    (let* ([snip (on-new-string-snip)]
           [snip (if (or (send snip is-owned?)
                         (positive? (snip->count snip)))
                     ;; uh-oh; resort to string-snip%
                     (new string-snip%)
                     snip)]
           [style (or style
                      (get-default-style)
                      (send s-style-list basic-style))])
      (set-snip-style! snip style)
      (let ([snip (let ([rsnip (snip-set-admin snip snip-admin)])
                    (if (not (eq? snip rsnip))
                        ;; uh-oh; resort to string-snip%:
                        (let ([snip (new string-snip%)])
                          (set-snip-style! snip style)
                          (send snip set-s-admin snip-admin))
                        snip))])
        (set-snip-count! snip 0)
        
        (let-values ([(gsnip s-pos) (find-snip/pos start 'before-or-none)])
          (if (and gsnip
                   (= (+ (snip->count gsnip) s-pos) start)
                   (has-flag? (snip->flags gsnip) NEWLINE)
                   (not (has-flag? (snip->flags gsnip) HARD-NEWLINE)))
              (begin
                ;; we want the snip on the same line as the preceding snip:
                (if (snip->next gsnip)
                    (insert-snip (snip->next gsnip) snip)
                    (append-snip snip))
                (set-snip-flags! gsnip (remove-flag (snip->flags gsnip) NEWLINE))
                (set-snip-flags! snip (add-flag (snip->flags snip) NEWLINE))
                (set-snip-line! snip (snip->line gsnip))
                (set-mline-last-snip! (snip->line snip) snip)
                snip)
              (let-values ([(gsnip s-pos) (find-snip/pos start 'after-or-none)])
                (cond
                 [(not gsnip)
                  (append-snip snip)
                  (set-snip-line! snip last-line)
                  (when (eq? (mline-last-snip last-line) last-snip)
                    (set! last-snip snip))
                  (set-mline-last-snip! last-line snip)
                  snip]
                 [(= s-pos start)
                  (insert-snip gsnip snip)
                  (set-snip-line! snip (snip->line gsnip))
                  (when (eq? (mline-snip (snip->line snip)) gsnip)
                    (set-mline-snip! (snip->line snip) snip))
                  snip]
                 [else
                  (split-one start s-pos gsnip
                             (lambda (gsnip)
                               (set-snip-line! snip (snip->line gsnip))
                               (insert-snip gsnip snip)))
                  snip])))))))

  (define/private (check-merge-snips start)
    (when (let loop ([did-something? #f])
            (let-values ([(snip1 s-pos1) (find-snip/pos start 'before)]
                         [(snip2 s-pos2) (find-snip/pos start 'after)])
              (if (eq? snip1 snip2)
                  did-something?
                  (if (not (and (snip->snipclass snip1)
                                (eq? (snip->snipclass snip1) (snip->snipclass snip2))
                                (eq? (snip->style snip1) (snip->style snip2))))
                      did-something?
                      (if (not (and
                                (not (has-flag? (snip->flags snip1) NEWLINE))
                                (has-flag? (snip->flags snip1) CAN-APPEND)
                                (has-flag? (snip->flags snip2) CAN-APPEND)
                                ((+ (snip->count snip1) (snip->count snip2)) . < . MAX-COUNT-FOR-SNIP)
                                (eq? (snip->line snip1) (snip->line snip2))))
                          did-something?
                          (cond
                           [(zero? (snip->count snip1))
                            (when (eq? (mline-snip (snip->line snip1)) snip1)
                              (set-mline-snip! (snip->line snip1) snip2))
                            (delete-snip snip1)
                            (set-snip-flags! snip1 (remove-flag (snip->flags snip1) OWNED))
                            (loop #t)]
                           [(zero? (snip->count snip2))
                            (when (eq? (mline-last-snip (snip->line snip2)) snip2)
                              (set-mline-last-snip! (snip->line snip2) snip1)
                              (mline-mark-recalculate (snip->line snip1)) ; need last-w updated
                              (set! graphic-maybe-invalid? #t))
                            (delete-snip snip2)
                            (set-snip-flags! snip2 (remove-flag (snip->flags snip2) OWNED))
                            (loop #t)]
                           [else
                            (let ([c (+ (snip->count snip1) (snip->count snip2))]
                                  [prev (snip->prev snip1)]
                                  [next (snip->next snip2)]
                                  [line (snip->line snip1)])
                              (let ([at-start? (eq? (mline-snip line) snip1)]
                                    [at-end? (eq? (mline-last-snip line) snip2)]
                                    [wl? write-locked?]
                                    [fl? flow-locked?])
                                (set! read-locked? #t)
                                (set! write-locked? #t)
                                (set! flow-locked? #t)

                                (set-snip-flags! snip2 (add-flag (snip->flags snip2) CAN-SPLIT))
                                (let ([naya (send snip2 merge-with snip1)])
                                  (set! read-locked? #f)
                                  (set! write-locked? wl?)
                                  (set! flow-locked? fl?)
                                  
                                  (if naya
                                      (begin
                                        ;; claim snip1 & snip2 unowned for naya test:
                                        (set-snip-flags! snip1 (remove-flag (remove-flag (snip->flags snip1) CAN-SPLIT)
                                                                            OWNED))
                                        (set-snip-flags! snip2 (remove-flag (remove-flag (snip->flags snip2) CAN-SPLIT)
                                                                            OWNED))
                                        
                                        (let ([naya (if (send naya is-owned?)
                                                        ;; uh-oh; make dummy
                                                        (new snip%)
                                                        naya)])
                                          (set-snip-flags! naya (remove-flag (snip->flags naya) CAN-SPLIT))
                                          (set-snip-flags! snip1 (add-flag (snip->flags snip1) OWNED))
                                          (set-snip-flags! snip2 (add-flag (snip->flags snip2) OWNED))

                                          (delete-snip snip1)
                                          (set-snip-flags! snip1 (remove-flag (snip->flags snip1) OWNED))
                                          (delete-snip snip2)
                                          (set-snip-flags! snip2 (remove-flag (snip->flags snip2) OWNED))
                                          
                                          (splice-snip naya prev next)
                                          (set! snip-count (add1 snip-count))
                                          
                                          ;; make sure that count is right:
                                          (set-snip-count! naya c)

                                          (set! revision-count (add1 revision-count))

                                          (let ([naya (snip-set-admin naya snip-admin)])
                                            
                                            (set-snip-line! naya line)
                                            (when at-start?
                                              (set-mline-snip! line naya))
                                            (when at-end?
                                              (set-mline-last-snip! line naya)
                                              (mline-mark-recalculate line) ;; need last-w updated
                                              (set! graphic-maybe-invalid? #t))
                                            #t)))
                                      (begin
                                        (set-snip-flags! snip2 (remove-flag (snip->flags snip2) CAN-SPLIT))
                                        #t)))))]))))))
      (after-merge-snips start)))

  ;; ----------------------------------------

  (def/public (on-new-string-snip)
    (new string-snip%))

  (def/public (on-new-tab-snip)
    (new tab-snip%))
  
  ;; ----------------------------------------

  (def/override (find-first-snip)
    (if (zero? len)
        #f
        snips))
  
  (define/private (do-find-snip p direction)
    ;; BEWARE: `len' may not be up-to-date
    (let-values ([(snip pos) (find-snip/pos p direction)])
      snip))

  (def/public (find-snip [exact-nonnegative-integer? p]
                         [(symbol-in before-or-none before after after-or-none) direction]
                         [maybe-box? [s-pos #f]])
    ;; BEWARE: `len' may not be up-to-date
    (let-values ([(snip pos) (find-snip/pos p direction)])
      (when s-pos (set-box! s-pos pos))
      snip))

  (define/private (find-snip/pos p direction)
    ;; BEWARE: `len' may not be up-to-date
    (cond
     [(and (eq? direction 'before-or-none) (zero? p))
      (values #f 0)]
     [else
      (let* ([line (mline-find-position (unbox line-root-box) p)]
             [pos (mline-get-position line)]
             [p (- p pos)])
        (if (and (eq? direction 'after-or-none) 
                 (not (mline-next line))
                 (p . >= . (mline-len line)))
            ;; past the end:
            (values #f 0)
            ;; within the line:
            (let-values ([(snip pos p)
                          (let ([snip (mline-snip line)])
                            (if (and (zero? p) (snip->prev snip))
                                ;; back up one:
                                (let ([snip (snip->prev snip)])
                                  (values snip
                                          (- pos (snip->count snip))
                                          (+ p (snip->count snip))))
                                (values snip pos p)))])

              (let loop ([snip snip]
                         [pos pos]
                         [p p])
                (if snip
                    (let ([p (- p (snip->count snip))])
                      (cond
                       [(or (and (eq? direction 'on)
                                 (zero? p))
                            (and (or (eq? direction 'before)
                                     (eq? direction 'before-or-none))
                                 (p . <= . 0))
                            (and (or (eq? direction 'after)
                                     (eq? direction 'after-or-none))
                                 (p . < . 0)))
                        (values snip pos)]
                       [(and (eq? direction 'on)
                             (p . < . 0))
                        (values #f 0)]
                       [else
                        (loop (snip->next snip) (+ pos (snip->count snip)) p)]))
                    (if (not (eq? direction 'after-or-none))
                        (values last-snip (- pos (snip->count last-snip)))
                        (values #f 0)))))))]))

  (def/public (find-next-non-string-snip [(make-or-false snip%) snip])
    (if (or (and snip
                 (not (eq? (snip->admin snip) snip-admin)))
            (zero? len))
        #f
        (let loop ([snip (if snip
                             (snip->next snip)
                             snips)])
          (if (and snip (snip . is-a? . string-snip%))
              (loop (snip->next snip))
              snip))))

  ;; ----------------------------------------

  (define/override (setting-admin admin) (void))

  (define/override (init-new-admin)
    (when (and (not (in-edit-sequence?))
               (or (not s-admin) (not (send s-admin refresh-delayed?))))
      (redraw)))

  (define/private (end-streaks exceptions)
    (when (and s-keymap 
               (not (memq 'key-sequence exceptions))
               (not streaks-pushed?))
      (send s-keymap break-sequence))
    (when (and flash? flashautoreset? (not flashdirectoff?))
      (flash-off))

    (set! typing-streak? #f)
    (set! deletion-streak? #f)
    (when (not (memq 'cursor exceptions))
      (set! vcursor-streak? #f)
      (set! extend-streak? #f))
    
    (when (and anchor-streak? (not keep-anchor-streak?))
      (set-anchor #f))

    (when (not (memq 'delayed exceptions))
      (set! delayed-streak? #f))

    (set! kill-streak? #f)
    
    (set! prev-paste-start -1))

  (define/private (push-streaks)
    (set! streaks-pushed? #t)
    (set! save-typing-streak? typing-streak?)
    (set! save-deletion-streak? deletion-streak?)
    (set! save-delayed-streak? delayed-streak?)
    (set! save-vcursor-streak? vcursor-streak?)
    (set! save-kill-streak? kill-streak?)
    (set! save-anchor-streak? anchor-streak?)
    (set! save-extend-streak? extend-streak?)
    (set! save-prev-paste-start prev-paste-start)
    (set! save-prev-paste-end prev-paste-end))

  (define/private (pop-streaks)
    (when streaks-pushed?
      (set! streaks-pushed? #f)
      (set! typing-streak? save-typing-streak?)
      (set! deletion-streak? save-deletion-streak?)
      (set! delayed-streak? save-delayed-streak?)
      (set! vcursor-streak? save-vcursor-streak?)
      (set! kill-streak? save-kill-streak?)
      (set! anchor-streak? save-anchor-streak?)
      (set! extend-streak? save-extend-streak?)
      (set! prev-paste-start save-prev-paste-start)
      (set! prev-paste-end save-prev-paste-end)))

  ;; ----------------------------------------

  (define/private (check-recalc [need-graphic? #t] [need-write? #t] [no-display-ok? #f])
    (and (not read-locked?)
         (not (and write-locked? need-write?))
         (if (not need-graphic?)
             #t
             (if (not s-admin)
                 no-display-ok?
                 (if (not graphic-maybe-invalid?)
                     #t
                     (if flow-locked?
                         #f
                         (let ([dc (send s-admin get-dc)])
                           (if (not dc)
                               no-display-ok?
                               (recalc-lines dc need-graphic?)))))))))

  (define/public (check-flow maxw dc Y startp start)
    ;; this method is called with write-locked and flow-locked already #t
    (let ([p startp]
          [checking-underflow? #f] ;; start by ensuring no overflow
          [checking-underflow-at-next? #f]
          [no-change-if-end-of-snip? #t] ;; because an immediate overflow can't be helped
          [no-change-if-start-of-snip? #f]
          [the-first-snip? #t]
          [first-underflow? #f]
          [deleted-a-newline? #f]
          [had-newline? #f])

      (define (done snip)
        (cond
         [(and (not snip)
               (has-flag? (snip->flags last-snip) NEWLINE)
               (not (has-flag? (snip->flags last-snip) HARD-NEWLINE)))
          (begin
            (set-snip-flags! last-snip (remove-flag (snip->flags last-snip) NEWLINE))
            (set! refresh-all? #t)
            #t)]
         [(or (not checking-underflow?) no-change-if-end-of-snip?)
          deleted-a-newline?]
         [else
          (set! refresh-all? #t)
          #t]))

      (let loop ([snip start]
                 [p p]
                 [_total-width padding-l])
        (if (and snip (not (has-flag? (snip->flags snip) HARD-NEWLINE)))
            (begin
              (when (not checking-underflow?)
                (set! checking-underflow? checking-underflow-at-next?)
                (when checking-underflow?
                  (set! first-underflow? #t)))
              (set! no-change-if-start-of-snip? no-change-if-end-of-snip?)
              
              (if (has-flag? (snip->flags snip) NEWLINE)
                  (begin
                    (set! no-change-if-end-of-snip? (not checking-underflow?))
                    (set-snip-flags! snip (remove-flag (snip->flags snip) NEWLINE))
                    (set! checking-underflow-at-next? #t)
                    (set! had-newline? #t)
                    (set! deleted-a-newline? #t)
                    ;; note: if the newline is restored, then
                    ;; we leave the loop
                    )
                  (begin
                    (set! no-change-if-end-of-snip? #f)
                    (set! checking-underflow-at-next? #f)
                    (set! had-newline? #f)))
              
              (let-boxes ([w 0.0])
                  (send snip get-extent dc _total-width Y w #f #f #f #f #f)
                (let ([_total-width (+ _total-width w)])
                  (if (_total-width . > . maxw)
                      (let ([_total-width (- _total-width w)])
                        ;; get best breaking position:
                        ;; (0.1 is hopefully a positive value smaller than any character)
                        (let ([origc (do-find-position-in-snip dc _total-width Y snip (- maxw _total-width 0.1) #f)])
                          ;; get legal breaking position before optimal:
                          (let-boxes ([b (+ p origc 1)])
                              (find-wordbreak b #f 'line)
                            (let ([c (min (- b p) origc)])
                              (let ([p
                                     (if (c . <= . 0)
                                         (cond
                                          [(and (b . <= . startp) checking-underflow? (positive? origc))
                                           ;; the word was currently force-broken; shift some part to here
                                           (+ p origc)]
                                          [(or (and checking-underflow?
                                                    first-underflow?
                                                    (or (b . <= . startp) (c . >= . 0)))
                                               (and (not the-first-snip?)
                                                    (or (zero? c)
                                                        (and (zero? origc)
                                                             (c . < . 0)
                                                             (b . <= . startp)))))
                                           ;; can't fit this snip in the line
                                           (when (snip->prev snip)
                                             (set-snip-flags! (snip->prev snip) (add-flag (snip->flags (snip->prev snip)) NEWLINE)))
                                           (when (and had-newline? (snip->next snip))
                                             (set-snip-flags! snip (add-flag (snip->flags snip) NEWLINE)))
                                           (if (and no-change-if-start-of-snip?
                                                    (or (not had-newline?)
                                                        (snip->next snip)))
                                               #f
                                               (begin
                                                (set! refresh-all? #t)
                                                #t))]
                                          [(and (c . < . 0) (b . > . startp))
                                           ;; overflow, but previous wordbreak was before this snip
                                           (when had-newline?
                                             (set-snip-flags! snip (add-flag (snip->flags snip) NEWLINE)))
                                           b]
                                          [else
                                           ;; overflow: we have to break the word anyway
                                           (if (zero? origc)
                                               (if (and (= (snip->count snip) 1)
                                                        (snip->next snip)
                                                        (has-flag? (snip->flags (snip->next snip)) NEWLINE))
                                                   ;; don't insert a break before a real newline
                                                   (done snip)
                                                   (+ p 1))
                                               (+ p origc))])
                                         (+ p c))])
                                (if (not (number? p))
                                    p ;; the result
                                    (begin
                                      (make-snipset p p)
                                      (let ([snip (find-snip p 'before)])
                                        (when (snip->next snip)
                                          (set-snip-flags! snip (add-flag (snip->flags snip) NEWLINE)))
                                        (set! refresh-all? #t)
                                        #t))))))))
                      (begin
                        (set! the-first-snip? #f)
                        (set! first-underflow? #f)
                        (loop (snip->next snip)
                              (+ p (snip->count snip))
                              _total-width))))))
            (done snip)))))

  (define/private (recalc-lines dc [calc-graphics? #t])
    (when calc-graphics?
      (when snip-cache-invalid?
        (let loop ([snip snips])
          (when snip
            (send snip size-cache-invalid)
            (loop (snip->next snip)))))

      (let ([old-max-width max-width])
        (when (and flow-invalid?
                   (max-width . <= . 0))
          (set! max-width A-VERY-BIG-NUMBER))

        (when (or graphics-invalid?
                  flow-invalid?
                  snip-cache-invalid?)
          ;; set all lines invalid
          (let loop ([line first-line])
            (when line
              (mline-mark-recalculate line)
              (when flow-invalid?
                (mline-mark-check-flow line))
              (loop (mline-next line)))))
        
        (let ([-changed?
               (if (max-width . > . 0)
                   (let ([wl? write-locked?]
                         [fl? flow-locked?])
                     ;; if any flow is updated, snip sizing methods will be called
                     (set! write-locked? #t)
                     (set! flow-locked? #t)
                     
                     (let ([w (- max-width padding-l padding-t CURSOR-WIDTH)])
                       (let loop ([-changed? #f])
                         (if (begin0
                              (mline-update-flow (unbox line-root-box) line-root-box this w dc
                                                 (lambda (del-line)
                                                   (when (eq? del-line first-line)
                                                     (set! first-line (mline-first (unbox line-root-box))))
                                                   (when (eq? del-line last-line)
                                                     (set! last-line (mline-last (unbox line-root-box)))))
                                                 (lambda (ins-line)
                                                   (when (not (mline-prev ins-line))
                                                     (set! first-line ins-line))
                                                   (when (not (mline-next ins-line))
                                                     (set! last-line ins-line))))
                              (assert (consistent-snip-lines 'post-update-flow)))

                             (loop #t)
                             
                             (begin
                               (set! flow-locked? fl?)
                               (set! write-locked? wl?)
                               -changed?)))))
                   #f)])

          (when (not (= max-width old-max-width))
            (set! max-width old-max-width))

          (when -changed?
            (set! refresh-all? #t)
            (set! first-line (mline-first (unbox line-root-box)))
            (set! last-line (mline-last (unbox line-root-box)))
            (set! num-valid-lines (mline-number (unbox line-root-box))))

          (let*-values ([(snip-sizes-changed? this-changed?)
                         (mline-update-graphics (unbox line-root-box) this dc
                                                padding-l padding-t
                                                max-line-width)]
                        [(-changed?) (or this-changed? -changed?)])

            (if (and (not -changed?)
                     (not graphic-maybe-invalid-force?))
                (begin
                  (set! graphic-maybe-invalid? #f)
                  (void))
                (begin
                  (set! graphic-maybe-invalid? #f)
                  (set! graphic-maybe-invalid-force? #f)

                  (let* ([Y (+ (mline-get-location last-line) (mline-h last-line))]
                         [Y (if (has-flag? (snip->flags last-snip) NEWLINE)
                                (begin
                                  (set! extra-line? #t)
                                  (set! extra-line-h (+ (mline-last-h last-line) line-spacing))
                                  (+ Y extra-line-h))
                                (begin
                                  (set! extra-line? #f)
                                  (set! extra-line-h 0)
                                  Y))]
                         [X (+ (mline-max-width (unbox line-root-box)) CURSOR-WIDTH)]
                         [X (if (min-width . > . 0.0)
                                (max X min-width)
                                X)]
                         [Y (if (min-height . > . 0.0)
                                (max Y min-height)
                                Y)]
                         [Y (if (max-height . > . 0.0)
                                (min Y max-height)
                                Y)])
                    (let ([descent (- (mline-h last-line) (mline-bottombase last-line))]
                          [space (mline-topbase first-line)]
                          [line-base (mline-bottombase first-line)])
                      (let ([resized?
                             (if (or (not (= total-height Y))
                                     (not (= total-width X))
                                     (not (= final-descent descent))
                                     (not (= initial-space space))
                                     (not (= line-base initial-line-base))
                                     (not (equal? reported-padding
                                                  (vector padding-l padding-t padding-r padding-b))))
                                 (begin
                                   (set! total-height Y)
                                   (set! total-width X)
                                   (set! final-descent descent)
                                   (set! initial-space space)
                                   (set! initial-line-base line-base)
                                   (set! reported-padding
                                         (vector padding-l padding-t padding-r padding-b))
                                   #t)
                                 #f)])

                        (set! graphics-invalid? #f)
                        (set! flow-invalid? #f)
                        (set! snip-cache-invalid? #f)

                        (set! draw-cached-in-bitmap? #f)

                        (when (and resized? s-admin)
                          (send s-admin resized #f))
                        (when (or resized? snip-sizes-changed?)
                          (on-reflow))))))))))))

  (def/public (on-reflow) (void))

  (def/public (set-autowrap-bitmap [(make-or-false bitmap%) bm])
    (if flow-locked?
        #f
        (let ([old auto-wrap-bitmap]
              [old-width wrap-bitmap-width])

          (set! auto-wrap-bitmap bm)
          (if auto-wrap-bitmap
              (set! wrap-bitmap-width (send auto-wrap-bitmap get-width))
              (set! wrap-bitmap-width 0))

          (when (max-width . > . 0)
            (set-max-width (+ max-width old-width)))

          old)))

  ;; ----------------------------------------

  ;; notifies the administrator that we need to be redrawn
  (define/private (redraw)

    (unless (or flow-locked? (not s-admin))
      (let-values ([(continue? notify?)
                    (if (send s-admin refresh-delayed?)
                        ;; does the admin know the refresh box already?
                        (if (and (not (= delayedscroll -1))
                                 (not delayedscrollbox?)
                                 (or refresh-all? refresh-unset?))
                            ;; yes...
                            (if (and (not refresh-all?) refresh-box-unset?)
                                ;; nothing to do
                                (values #f #f)
                                (values #t #t))
                            (values #t #t))
                        (values #t #f))])
        (when continue?
          
          (when notify?
            (let-boxes ([x 0.0] [y 0.0] [w 0.0] [h 0.0])
                (send s-admin get-max-view x y w h)
              (let ([top y]
                    [bottom (+ y h)]
                    [left x]
                    [right (+ x w)])
                (let-values ([(left right top bottom)
                              (call-with-semaphore
                               refresh-box-lock
                               (lambda ()
                                 (begin0
                                  (if refresh-all?
                                      (values left right top bottom)
                                      (values
                                       (max refresh-l left)
                                       (if (eq? refresh-r 'display-end)
                                           right
                                           (min refresh-r right))
                                       (max refresh-t top)
                                       (if (eq? refresh-b 'display-end)
                                           bottom
                                           (min refresh-b bottom))))
                                  (set! refresh-unset? #t)
                                  (set! refresh-box-unset? #t)
                                  (set! refresh-all? #f))))])
                  (let ([height (- bottom top)]
                        [width (- right left)])
                    (when (and (width . > . 0) (height . > . 0))
                      (send s-admin needs-update left top width height)))))))

          (let-boxes ([dc #f]
                      [x 0.0]
                      [y 0.0])
              (set-box! dc (send s-admin get-dc x y))
            (if (not dc)
                (begin
                  (set! delayedscroll -1)
                  (set! delayedscrollbox? #f))

                (let ([origx x]
                      [origy y])

                  (recalc-lines dc) 

                  (cond
                   [(not (= delayedscroll -1))
                    (when (scroll-to-position/refresh delayedscroll delayedscrollateol? #f
                                                      delayedscrollend delayedscrollbias)
                      (set! refresh-all? #t))]
                   [delayedscrollbox?
                    (set! delayedscrollbox? #f)
                    (when (do-scroll-to delayedscrollsnip delayedscroll-x delayedscroll-y
                                        delayedscroll-w delayedscroll-h #f delayedscrollbias)
                      (set! refresh-all? #t))])
                  (let-boxes ([x 0.0]
                              [y 0.0])
                      (send s-admin get-dc x y)
                    (when (or (not (= origx x)) (not (= origy y)))
                      (set! refresh-all? #t)))

                  (let-boxes ([x 0.0] [y 0.0] [w 0.0] [h 0.0])
                      (send s-admin get-max-view x y w h)
                    (let ([top y]
                          [bottom (+ y h)]
                          [left x]
                          [right (+ x w)])

                      ;; figure out the minimal refresh area; the refresh area may be
                      ;; determined by character position ranges, box coordinates, or
                      ;; both; if neither is specified, we have to assume that everything
                      ;; needs to be refreshed
                      (let-values ([(left top right bottom needs-update?)
                                    (call-with-semaphore
                                     refresh-box-lock
                                     (lambda ()
                                       (begin0
                                        (if (and (not refresh-all?)
                                                 (or (not refresh-unset?) (not refresh-box-unset?)))
                                            (if (not refresh-unset?)
                                                (let ([top (if (refresh-start . > . -1)
                                                               (let-boxes ([fy 0.0])
                                                                   (position-location refresh-start #f fy #t #t #t)
                                                                 (max top fy))
                                                               top)]
                                                      [bottom (if (refresh-end . > . -1)
                                                                  (let-boxes ([fy 0.0])
                                                                      (position-location refresh-end #f fy #f #f #t)
                                                                    (min bottom fy))
                                                                  bottom)])
                                                  (values left (if (not refresh-box-unset?)
                                                                   (min refresh-t top)
                                                                   top)
                                                          right (if (not refresh-box-unset?)
                                                                    (if (eq? refresh-b 'display-end)
                                                                        bottom
                                                                        (max bottom refresh-b))
                                                                    bottom)
                                                          #t))
                                                (values (max refresh-l left)
                                                        (max top refresh-t)
                                                        (if (eq? refresh-r 'display-end)
                                                            right
                                                            (min right refresh-r))
                                                        (if (eq? refresh-b 'display-end)
                                                            bottom
                                                            (min bottom refresh-b))
                                                        #t))
                                            (values left top right bottom refresh-all?))
                                        (set! refresh-unset? #t)
                                        (set! refresh-box-unset? #t)
                                        (set! refresh-all? #f))))])

                        (let ([height (- bottom top)]
                              [width (- right left)])

                          (when changed? 
                            (set! changed? #f)
                            (let ([wl? write-locked?]
                                  [fl? flow-locked?])
                              
                              (set! write-locked? #t)
                              (set! flow-locked? #t)
                              (on-change)
                              (set! write-locked? wl?)
                              (set! flow-locked? fl?)))

                          (when (and needs-update?
                                     (width . > . 0)
                                     (height . > . 0))
                            (send s-admin needs-update left top width height)))))))))))))

  (define/private (too-busy-to-refresh?)
    (or graphic-maybe-invalid?
        flow-locked?
        (in-edit-sequence?)))

  ;; called by the administrator to trigger a redraw
  (def/override (refresh [real? left] [real? top] [nonnegative-real? width] [nonnegative-real? height]
                         [caret-status? show-caret]
                         [(make-or-false color%) bg-color])
    (cond
     [(or (width . <= . 0) (height . <= . 0)) (void)]
     [(too-busy-to-refresh?)
      ;; this refresh command was not requested by us and we're busy
      ;; (probably in the middle of a begin-/end-edit-sequnce);
      ;; add the given region to our own invalid-region tracking, and
      ;; we'll get back to it when we're done with whatever
      (refresh-box left top width height)
      ;; Double-check that we didn't finish being busy while
      ;; setting the box:
      (unless (too-busy-to-refresh?) (redraw))]
     [(not s-admin)
      (void)]
     [else
      (let-boxes ([x 0.0]
                  [y 0.0]
                  [dc #f])
          (set-box! dc (send s-admin get-dc x y))
        (when dc
          (begin-sequence-lock)

          (let ([show-caret
                 (if (and caret-blinked?
                          (not (pair? show-caret))
                          (not (eq? show-caret 'no-caret))
                          (not s-caret-snip))
                     ;; maintain caret-blinked invariant
                     'no-caret
                     show-caret)])

            (when (send s-offscreen ready-offscreen width height)
              (set! draw-cached-in-bitmap? #f))

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
                             (dc . is-a? . printer-dc%))]
                    [show-xsel?
                     (and ALLOW-X-STYLE-SELECTION?
                          (or (and (not (eq? 'show-caret show-caret)) 
                                   (not (pair? show-caret)))
                              s-caret-snip)
                          (eq? this editor-x-selection-owner)
                          (not flash?)
                          (not (= endpos startpos)))])

                (if (and bg-color
                         (not (send s-offscreen is-in-use?))
                         (send s-offscreen get-bitmap)
                         (send (send s-offscreen get-bitmap) ok?)
                         (send (send s-offscreen get-dc) ok?)
                         (not ps?))
                    ;; draw to offscreen
                    (let ([red (send bg-color red)]
                          [green (send bg-color green)]
                          [blue (send bg-color blue)])
                      (send s-offscreen set-in-use #t)
                      
                      (when (or
                             (not draw-cached-in-bitmap?)
                             (not (eq? offscreen-key (send s-offscreen get-last-used)))
                             (not (= last-draw-t top))
                             (not (= last-draw-b bottom))
                             (not (= last-draw-l left))
                             (not (= last-draw-r right))
                             (not (eq? show-caret last-draw-caret))
                             (not (eq? show-xsel? last-draw-x-sel?))
                             (not (= last-draw-red red))
                             (not (= last-draw-green green))
                             (not (= last-draw-blue blue)))
                        
                        (do-redraw (send s-offscreen get-dc) top bottom left right 
                                   (- top) (- left) show-caret show-xsel? bg-color)
                        
                        (set! last-draw-l left)
                        (set! last-draw-t top)
                        (set! last-draw-r right)
                        (set! last-draw-b bottom)
                        (set! last-draw-caret show-caret)
                        (set! last-draw-x-sel? show-xsel?)
                        (set! last-draw-red red)
                        (set! last-draw-green green)
                        (set! last-draw-blue blue)
                        (set! draw-cached-in-bitmap? #t))

                      (send dc draw-bitmap-section 
                            (send (send s-offscreen get-dc) get-bitmap) 
                            (- left x) (- top y) 
                            0 0 width height 'solid)
                      
                      (send s-offscreen set-last-used offscreen-key)
                      (send s-offscreen set-in-use #f))
                    
                    ;; draw to given DC:
                    (let ([pen (send dc get-pen)]
                          [brush (send dc get-brush)]
                          [font (send dc get-font)]
                          [fg (make-object color% (send dc get-text-foreground))]
                          [bg (make-object color% (send dc get-text-background))]
                          [bgmode (send dc get-text-mode)]
                          [rgn (send dc get-clipping-region)])

                      (send dc set-clipping-rect (- left x) (- top y) width height)

                      (send dc suspend-flush)

                      (dynamic-wind
                          void
                          (lambda ()
                            (do-redraw dc top bottom left right (- y) (- x) show-caret show-xsel? bg-color))
                          (lambda ()
                            (send dc set-clipping-region rgn)
                            
                            (send dc set-brush brush)
                            (send dc set-pen pen)
                            (send dc set-font font)
                            (send dc set-text-foreground fg)
                            (send dc set-text-background bg)
                            (send dc set-text-mode bgmode)

                            (send dc resume-flush))))))))

          (end-sequence-lock)))]))
  
  ;; performs the actual drawing operations
  (define/private (do-redraw dc starty endy leftx rightx dy dx show-caret show-xsel? bg-color)
    (let ([wl? write-locked?])

      (set! flow-locked? #t)
      (set! write-locked? #t)

      (let-values ([(-startpos -endpos pos-at-eol?)
                    (if flash?
                        (values flashstartpos flashendpos flashposateol?)
                        (values startpos endpos posateol?))])

        (send dc set-text-mode 'solid)

        (let ([line (mline-find-location (unbox line-root-box) (- starty padding-t))])

          (when (and bg-color
                     (not (pair? show-caret)))
            (let ([lsave-pen (send dc get-pen)]
                  [lsave-brush (send dc get-brush)])
              (let ([wb (if (and (= 255 (send bg-color red))
                                 (= 255 (send bg-color green))
                                 (= 255 (send bg-color blue)))
                            clear-brush
                            (send the-brush-list find-or-create-brush bg-color 'solid))])
                (send dc set-brush wb)
                (send dc set-pen outline-pen)

                (send dc draw-rectangle
                      (+ leftx dx) (+ starty dy)
                      (- rightx leftx) (- endy starty))

                (send dc set-brush lsave-brush)
                (send dc set-pen lsave-pen))))

          (let* ([call-on-paint
                  (lambda (pre?)
                    (on-paint pre? dc leftx starty rightx endy dx dy
                              (if (or (pair? show-caret)
                                      (not s-caret-snip))
                                  show-caret
                                  'no-caret)))]
                 [paint-done
                  (lambda ()
                    (call-on-paint #f)
                    (set! write-locked? wl?)
                    (set! flow-locked? #f))]

                 [local-caret-pen
                  (if bg-color
                      (let ([r (send bg-color red)]
                            [g (send bg-color green)]
                            [b (send bg-color blue)])
                        (if (and (= r 255) (= g 255) (= b 255))
                            caret-pen
                            (make-object pen% (make-object color% 
                                                           (- 255 r)
                                                           (- 255 g)
                                                           (- 255 b))
                                         (send caret-pen get-width) 
                                         'solid)))
                      caret-pen)])

            (call-on-paint #t)
            
            (when line
              (let ([tleftx (+ leftx dx)]
                    [tstarty (+ starty dy)]
                    [trightx (+ rightx dx)]
                    [tendy (+ endy dy)])
                (let lloop ([line line]
                            [old-style #f]
                            [ycounter (+ (mline-get-location line) padding-t)]
                            [pcounter (mline-get-position line)]
                            [prevwasfirst 0.0])
                  (cond
                   [(not line)
                    (send (send s-style-list basic-style) switch-to dc old-style)
                    (when (and (eq? 'show-caret show-caret) 
                               (not s-caret-snip)
                               extra-line?
                               (not pos-at-eol?)
                               (= len -startpos)
                               (= -endpos -startpos)
                               hilite-on?)
                      (let ([y ycounter]
                            [save-pen (send dc get-pen)])
                        (send dc set-pen local-caret-pen)
                        (send dc draw-line 
                              (+ dx padding-l) (+ y dy)
                              (+ dx padding-l) (sub1 (+ y extra-line-h dy)))
                        (send dc set-pen save-pen)))
                    (paint-done)]
                   [(ycounter . >= . endy)
                    (paint-done)]
                   [line
                    (let ([first (mline-snip line)]
                          [last (snip->next (mline-last-snip line))]
                          [bottombase (+ ycounter (mline-bottombase line))]
                          [topbase (+ ycounter (mline-topbase line))])
                      (define (process-snips draw? maybe-hilite? old-style)
                        (let sloop ([snip first]
                                    [p pcounter]
                                    [x (+ (mline-get-left-location line max-line-width) padding-l)]
                                    [hilite-some? #f]
                                    [hsxs 0.0]
                                    [hsxe 0.0]
                                    [hsys 0.0]
                                    [hsye 0.0]
                                    [old-style old-style])
                          (if (eq? snip last)
                              (values hilite-some? hsxs hsxe hsys hsye old-style)
                              (begin
                                (send (snip->style snip) switch-to dc old-style)
                                (let ([old-style (snip->style snip)])
                                  (let-boxes ([w 0.0] [h 0.0] [descent 0.0] [space 0.0])
                                      (send snip get-extent dc x ycounter w h descent space #f #f)
                                    (let* ([align (send (snip->style snip) get-alignment)]
                                           [down
                                            (cond
                                             [(eq? 'bottom align)
                                              (+ (- bottombase h) descent)]
                                             [(eq? 'top align)
                                              (- topbase space)]
                                             [else
                                              (- (/ (+ topbase bottombase) 2) 
                                                 (/ (- h descent space) 2)
                                                 space)])])

                                      (when draw?
                                        (when (and (x . <= . rightx)
                                                   ((+ x w) . >= . leftx))
                                          (send snip draw dc (+ x dx) (+ down dy)
                                                tleftx tstarty trightx tendy
                                                dx dy
                                                (if (pair? show-caret)
                                                    (cons p (+ p (snip->count snip)))
                                                    (if (eq? snip s-caret-snip)
                                                        show-caret
                                                        (if (and maybe-hilite?
                                                                 (-endpos . > . p)
                                                                 (-startpos . < . (+ p (snip->count snip))))
                                                            (cons (max 0 (- -startpos p))
                                                                  (min (snip->count snip) (- -endpos p)))
                                                            'no-caret))))))

                                      ;; the rules for hiliting are surprisingly complicated:
                                      (let ([hilite?
                                             (and
                                              hilite-on?
                                              (or show-xsel?
                                                  (and (not s-caret-snip)
                                                       (or (eq? 'show-caret show-caret)
                                                           (and (show-caret . showcaret>= . s-inactive-caret-threshold)
                                                                (not (= -endpos -startpos))))))
                                              (if pos-at-eol?
                                                  (= -startpos (+ p (snip->count snip)))
                                                  (or (and (-startpos . < . (+ p (snip->count snip)))
                                                           (-endpos . >= . p)
                                                           (or (= -endpos -startpos) (-endpos . > . p)))
                                                      (and (= (+ p (snip->count snip)) len)
                                                           (= len -startpos))))
                                              (or (not (has-flag? (snip->flags snip) NEWLINE))
                                                  ;; end of line:
                                                  (or (not (= -startpos (+ p (snip->count snip))))
                                                      (and (= -endpos -startpos) pos-at-eol?)
                                                      (and (not (= -endpos -startpos)) 
                                                           (-startpos . < . (+ p (snip->count snip))))))
                                              (or (not (eq? snip first))
                                                  ;; beginning of line:
                                                  (or (not (= p -endpos))
                                                      (and (= -endpos -startpos) (not pos-at-eol?))
                                                      (and (not (= -endpos -startpos))
                                                           (-endpos . > . p)))))])
                                        
                                        (if hilite?
                                            (let*-values ([(bottom) (+ down h)]
                                                          [(hxs) (if (-startpos . <= . p)
                                                                     (if (-startpos . < . p)
                                                                         0
                                                                         x)
                                                                     (+ x (send snip partial-offset dc x ycounter
                                                                                (- -startpos p))))]
                                                          [(hxe bottom) (if (-endpos . >= . (+ p (snip->count snip)))
                                                                            (if (has-flag? (snip->flags snip) NEWLINE)
                                                                                (if (= -startpos -endpos)
                                                                                    (values hxs bottom)
                                                                                    (values rightx
                                                                                            (+ ycounter (mline-h line))))
                                                                                (values (+ x w) bottom))
                                                                            (values (+ x (send snip partial-offset dc x ycounter
                                                                                               (- -endpos p)))
                                                                                    bottom))])
                                              
                                              (let-values ([(hsxs hsxe hsys hsye)
                                                            (if (not hilite-some?)
                                                                (values hxs hxe down bottom)
                                                                (values hsxs hxe (min down hsys) (max hsye bottom)))])
                                                (sloop (snip->next snip)
                                                       (+ p (snip->count snip))
                                                       (+ x w)
                                                       #t hsxs hsxe hsys hsye
                                                       old-style)))
                                            (sloop (snip->next snip)
                                                   (+ p (snip->count snip))
                                                   (+ x w)
                                                   hilite-some? hsxs hsxe hsys hsye
                                                   old-style))))))))))
                      (let*-values ([(draw-first?)
                                     (or (and (or (not (showcaret>= show-caret 'show-caret))
						  (and s-caret-snip (not (pair? show-caret)))
						  (not hilite-on?))
					      (not show-xsel?))
                                         (= -startpos -endpos)
                                         (-endpos . < . pcounter)
                                         (-startpos . > . (+ pcounter (mline-len line))))]
                                    [(hilite-some? hsxs hsxe hsys hsye old-style)
                                     (process-snips draw-first? #f old-style)])
                        (let ([prevwasfirst
                               (if hilite-some?
                                   (if (not (= hsxs hsxe))
                                       (if (and (hsxs . <= . rightx) (hsxe . >= . leftx))
                                           (let ([save-pen (send dc get-pen)]
                                                 [hxsx (max hsxs leftx)]
                                                 [hsxe (min hsxe rightx)])
                                             (begin0
                                              (if (and (not show-xsel?)
                                                       (not (showcaret>= show-caret 'show-caret)))
                                                  (if show-outline-for-inactive?
                                                      (let ([first-hilite? (-startpos . >= . pcounter)]
                                                            [last-hilite? (-endpos . <= . (+ pcounter (mline-len line)))])
                                                        (send dc set-pen outline-inactive-pen)
                                                        (let ([prevwasfirst
                                                               (cond
                                                                [first-hilite?
                                                                 (send dc draw-line (+ hsxs dx) (+ hsys dy) (+ hsxe (sub1 dx)) (+ hsys dy))
                                                                 hsxs]
                                                                [(positive? prevwasfirst)
                                                                 (send dc draw-line dx (+ hsys dy) (+ prevwasfirst dx) (+ hsys dy))
                                                                 0.0]
                                                                [else 0.0])])
                                                          (send dc draw-line (+ hsxs dx) (+ hsys dy) (+ hsxs dx) (+ hsye (sub1 dy)))
                                                          (send dc draw-line (+ hsxe (sub1 dx)) (+ hsys dy) 
                                                                (+ hsxe (sub1 dx)) (+ hsye (sub1 dy)))
                                                          (when last-hilite?
                                                            (send dc draw-line (+ hsxs dx) (+ hsye dy) (+ hsxe (sub1 dx)) (+ hsye dy)))
                                                          (when (not first-hilite?)
                                                            (send dc draw-line (+ hsxe dx) (+ hsys dy) (+ rightx dx) (+ hsys dy)))
                                                          prevwasfirst))
                                                      prevwasfirst)
                                                  (let ([save-brush (send dc get-brush)])
                                                    (send dc set-pen outline-pen)
                                                    (send dc set-brush outline-brush)
                                                    
                                                    (send dc draw-rectangle (+ hsxs dx) (+ hsys dy) 
                                                          (max 0.0 (- hsxe hsxs)) (max 0.0 (- hsye hsys)))
                                                    (when ALLOW-X-STYLE-SELECTION?
                                                      (when show-xsel?
                                                        (send dc set-brush outline-nonowner-brush)
                                                        (send dc draw-rectangle (+ hsxs dx) (+ hsys dy) 
                                                              (max 0.0 (- hsxe hsxs)) (max 0.0 (- hsye hsys)))))
                                                    (send dc set-brush save-brush)
                                                    prevwasfirst))
                                              (send dc set-pen save-pen)))
                                           prevwasfirst)
                                       (begin
                                         (when (eq? 'show-caret show-caret)
                                           (when (and (hsxs . <= . rightx) (hsxs . >= . leftx))
                                             (let ([save-pen (send dc get-pen)])
                                               (send dc set-pen local-caret-pen)
                                               (send dc draw-line (+ hsxs dx) (+ hsys dy) 
                                                     (+ hsxs dx) 
                                                     (+ hsye (sub1 dy)))
                                               (send dc set-pen save-pen))))
                                         prevwasfirst))
                                   prevwasfirst)])
                          
                          (when (and (positive? wrap-bitmap-width)
                                     (not (has-flag? (snip->flags (mline-last-snip line)) HARD-NEWLINE))
                                     last
                                     (rightx . >= . max-width)
                                     (send auto-wrap-bitmap ok?))
                            (let ([h (min (->long (send auto-wrap-bitmap get-height))
                                          (mline-bottombase line))]
                                  [osfg (send old-style get-foreground)])
                              (send dc draw-bitmap-section
                                    auto-wrap-bitmap 
                                    (sub1 (+ max-width dx)) (+ (- bottombase h) dy)
                                    0 0 wrap-bitmap-width h
                                    'solid osfg)))

                          (let ([old-style
                                 (if draw-first?
                                     old-style
                                     (let-values ([(_hilite-some? _hsxs _hsxe _hsys _hsye old-style)
                                                   (process-snips #t #t old-style)])
                                       old-style))])
                            (lloop (mline-next line)
                                   old-style
                                   (+ ycounter (mline-h line))
                                   (+ pcounter (mline-len line))
                                   prevwasfirst)))))])))))))))

  ;; ----------------------------------------

  ;; used internally to delay refreshes:
  (define/private (need-refresh start [end -1])
    (if refresh-unset?
        (begin
          (set! refresh-start start)
          (set! refresh-end end)
          (set! refresh-unset? #f))
        (begin
          (set! refresh-start (min start refresh-start))
          (cond
           [(= end -1)
            (set! refresh-end -1)]
           [(= refresh-end -1)
            (void)]
           [else (set! refresh-end (max end refresh-end))])))

    (set! draw-cached-in-bitmap? #f)
    
    (continue-refresh))

  (define/private (refresh-by-line-demand)
    (set! graphic-maybe-invalid? #t)
    (continue-refresh))

  (define/private (continue-refresh)
    (if (and (not (in-edit-sequence?))
             (not (super is-printing?))
             (or (not s-admin) (not (send s-admin refresh-delayed?))))
        (redraw)
        (begin
          (when (and (not (in-edit-sequence?))
                     (or (= delayedscroll -1) 
                         delayedscrollbox?))
            (if (and (not (super is-printing?)) s-admin)
                ;; although the administrator says to delay,
                ;; we can't just drop scroll requests
                (redraw)
                (begin
                  (set! delayedscroll -1)
                  (set! delayedscrollbox? #f))))
          (when (and s-admin (zero? (send s-admin get-s-standard)))
            (send s-admin resized #f)))))

  (define/private (need-caret-refresh)
    (need-refresh startpos endpos))

  ;; ----------------------------------------

  (define/override (own-x-selection on? update? force?)
    (and (do-own-x-selection on? force?)
         (begin
           (when update?
             (need-caret-refresh))
           #t)))

  ;; ----------------------------------------

  (def/public (set-paragraph-margins [exact-nonnegative-integer? i]
                                     [nonnegative-real? first-left]
                                     [nonnegative-real? left]
                                     [nonnegative-real? right])
    (let ([l (mline-find-paragraph (unbox line-root-box) i)])
      (when l
        (let ([p (mline-clone-paragraph (mline-paragraph l))])
          (set-mline-paragraph! l p)

          (set-paragraph-left-margin-first! p first-left)
          (set-paragraph-left-margin! p left)
          (set-paragraph-right-margin! p right)

          (if (max-width . > . 0)
              (begin
                (mline-mark-check-flow l)
                (let loop ([l (mline-next l)])
                  (when (and l
                             (zero? (mline-starts-paragraph l)))
                    (mline-mark-check-flow l)
                    (loop (mline-next l)))))
              (need-refresh (paragraph-start-position i) (paragraph-end-position i)))

          (refresh-by-line-demand)))))
  
  (def/public (set-paragraph-alignment [exact-nonnegative-integer? i] [(symbol-in left center right) align])
    (let ([l (mline-find-paragraph (unbox line-root-box) i)])
      (when l
        (let ([p (mline-clone-paragraph (mline-paragraph l))])
          (set-mline-paragraph! l p)
          
          (set-paragraph-alignment! p align)

          (need-refresh (paragraph-start-position i) (paragraph-end-position i))

          (refresh-by-line-demand)))))

  ;; ----------------------------------------

  (def/override (is-printing?) (super is-printing?))

  (define/override (do-begin-print dc fit?)
    (if flow-locked?
        #f
        (begin
          (check-recalc)
          (size-cache-invalid)

          (let ([save-info (if fit?
                               (cons (get-max-width)
                                     (set-autowrap-bitmap #f))
                               #f)])
            (when fit?
              (let-values ([(w h) (send dc get-size)])
                (let-boxes ([hm 0]
                            [vm 0])
                    (send (current-ps-setup) get-editor-margin hm vm)
                  (set-max-width (- w (* 2 hm))))))

            (recalc-lines dc #t)

            (let ([wl? write-locked?]
                  [fl? flow-locked?])
              (set! write-locked? #t)
              (set! flow-locked? #t)
              (on-change)
              (set! write-locked? wl?)
              (set! flow-locked? fl?))

            save-info))))

  (define/override (do-end-print dc data)
    (unless flow-locked?
      (size-cache-invalid)

      (when data
        (set-max-width (car data))
        (set-autowrap-bitmap (cdr data)))

      (let ([wl? write-locked?]
            [fl? flow-locked?])
        (set! write-locked? #t)
        (set! flow-locked? #t)
        (on-change)
        (set! write-locked? wl?)
        (set! flow-locked? fl?))))

  (define/private (new-page-line? line)
    (let ([len (mline-len line)])
      (and (<= 1 len 2)
           (let* ([pos (mline-get-position line)]
                  [s (get-text pos (+ pos len))])
             (or (equal? s "\f")
                 (equal? s "\f\n"))))))

  (define/private (has/print-page dc page print?)
    (if flow-locked?
        #f
        (begin
          (recalc-lines dc #t)
          (let-values ([(W H) (send dc get-size)])
            (let-boxes ([W W]
                        [H H]
                        [hm 0]
                        [vm 0])
              (begin
                (when (or (zero? (unbox W)) (zero? (unbox H)))
                  (get-default-print-size W H))
                (when (not (zero? page))
                  (send (current-ps-setup) get-editor-margin hm vm)))
              (let ([H (- H (* 2 vm))]
                    [W (- W (* 2 hm))])

                ;; H is the total page height;
                ;; line is the line that we haven't finished printing;
                ;; y is the starting location to print for this page;
                ;; h is the height that we're hoping to fit into the page
                ;; i is the line number
                (let ploop ([this-page 1]
                            [line first-line]
                            [y 0.0]
                            [next-h 0.0]
                            [i 0])
                  (and 
                   line
                   (let ([h next-h]
                         [next-h 0.0])
                     (let loop ([h h]
                                [i i]
                                [line line]
                                [can-continue? #t]
                                [unline 0.0])
                       (cond
                         [(or (zero? h)
                              (and (i . < . num-valid-lines)
                                   (or (zero? page)
                                       ((mline-h line) . < . (- H h)))
                                   can-continue?))
                          (let ([lh (mline-h line)]
                                [new-page? (new-page-line? line)])
                            (loop (+ h lh)
                                  (add1 i)
                                  (mline-next line)
                                  (not new-page?)
                                  (if new-page? lh unline)))]
                         [else
                          (let-values ([(h i line)
                                        (cond
                                          [(and (not (zero? page))
                                                (h . < . H)
                                                (i . < . num-valid-lines)
                                                ((mline-h line) . > . H))
                                           ;; we'll have to break it up anyway; start now?
                                           (let* ([pos (find-scroll-line (+ y H))]
                                                  [py (scroll-line-location pos)])
                                             (if (py . > . (+ y h))
                                                 ;; yes, at least one line will fit
                                                 (values (+ h (mline-h line))
                                                         (add1 i)
                                                         (mline-next line))
                                                 (values h i line)))]
                                          [else
                                           (values h i line)])])
                            (let-values ([(next-h h)
                                          (if (and (not (zero? page))
                                                   (h . > . H))
                                              ;; only happens if we have something that's too big to fit on a page;
                                              ;; look for internal scroll positions
                                              (let* ([pos (find-scroll-line (+ y H))]
                                                     [py (scroll-line-location pos)])
                                                (if (py . > . y)
                                                    (let ([new-h (- py y)])
                                                      (values (- h new-h)
                                                              new-h))
                                                    (values next-h h)))
                                              (values next-h h))])
                              (or (if print?
                                      (begin
                                        (when (or (page . <= . 0)
                                                  (= this-page page))
                                          (begin
                                            (when (page . <= . 0)
                                              (send dc start-page))
                                            (do-redraw dc 
                                                       (+ y (if (zero? i) 0 1))
                                                       (+ y (- h 1 unline))
                                                       0 W (+ (- y) vm) hm
                                                       'no-caret #f #f)
                                            (when (page . <= . 0)
                                              (send dc end-page))))
                                        #f)
                                      (= this-page page))
                                  (ploop (add1 this-page)
                                         line
                                         (+ y h)
                                         next-h 
                                         i))))])))))))))))

  (define/override (do-has-print-page? dc page)
    (has/print-page dc page #f))

  (def/override (print-to-dc [dc<%> dc] [exact-integer? [page -1]])
    (has/print-page dc page #t)
    (void)))

(set-text%! text%)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/top (add-text-keymap-functions [keymap% tab])
  (let ([add (lambda (n f)
               (send tab add-function n
                     (lambda (e evt)
                       (if (e . is-a? . text%)
                           (begin (f e evt) #t)
                           #f))))])
    (add "forward-character" (lambda (t evt) (send t move-position 'right)))
    (add "backward-character" (lambda (t evt) (send t move-position 'left)))
    (add "previous-line" (lambda (t evt) (send t move-position 'up)))
    (add "next-line" (lambda (t evt) (send t move-position 'down)))
    (add "previous-page" (lambda (t evt) (send t move-position 'up #f 'page)))
    (add "next-page" (lambda (t evt) (send t move-position 'down #f 'page)))
    (add "forward-word" (lambda (t evt) (send t move-position 'right #f 'word)))
    (add "backward-word" (lambda (t evt) (send t move-position 'left #f 'word)))

    (add "forward-select" (lambda (t evt) (send t move-position 'right #t)))
    (add "backward-select" (lambda (t evt) (send t move-position 'left #t)))
    (add "select-down" (lambda (t evt) (send t move-position 'down #t)))
    (add "select-up" (lambda (t evt) (send t move-position 'up #t)))
    (add "select-page-up" (lambda (t evt) (send t move-position 'up #t 'page)))
    (add "select-page-down" (lambda (t evt) (send t move-position 'down #t 'page)))
    (add "forward-select-word" (lambda (t evt) (send t move-position 'right #t 'word)))
    (add "backward-select-word" (lambda (t evt) (send t move-position 'left #t 'word)))

    (add "beginning-of-file" (lambda (t evt) (send t move-position 'home)))
    (add "end-of-file" (lambda (t evt) (send t move-position 'end)))
    (add "beginning-of-line" (lambda (t evt) (send t move-position 'left #f 'line)))
    (add "end-of-line" (lambda (t evt) (send t move-position 'right #f 'line)))

    (add "select-to-beginning-of-file" (lambda (t evt) (send t move-position 'home #t)))
    (add "select-to-end-of-file" (lambda (t evt) (send t move-position 'end #t)))
    (add "select-to-beginning-of-line" (lambda (t evt) (send t move-position 'left #t 'line)))
    (add "select-to-end-of-line" (lambda (t evt) (send t move-position 'right #t 'line)))

    (add "delete-previous-character" (lambda (t evt) (send t delete)))
    (add "delete-next-character" (lambda (t evt)
                                   (let-boxes ([s 0]
                                               [e 0])
                                       (send t get-position s e)
                                     (if (not (= s e))
                                         (send t delete)
                                         (send t delete s (+ s 1))))))
    
    (add "clear-buffer" (lambda (t evt) (send t erase)))
    (add "delete-next-word" (lambda (t evt)
                              (send t begin-edit-sequence)
                              (send t move-position 'right #t 'word)
                              (send t delete)
                              (send t end-edit-sequence)))
    (add "delete-previous-word" (lambda (t evt)
                                  (send t begin-edit-sequence)
                                  (send t move-position 'left #t 'word)
                                  (send t delete)
                                  (send t end-edit-sequence)))
    (add "delete-line" (lambda (t evt)
                         (send t begin-edit-sequence)
                         (send t move-position 'left #f 'line)
                         (send t move-position 'right #t 'line)
                         (send t delete)
                         (send t end-edit-sequence)))

    (add "paste-next" (lambda (t evt) (send t paste-next)))

    (add-editor-keymap-functions tab)))
