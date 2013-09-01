#lang scheme/unit
  (require mzlib/class
           "sig.rkt"
           "../preferences.rkt"
           mred/mred-sig)
  
  (import mred^
          [prefix frame: framework:frame^]
          [prefix text: framework:text^]
          [prefix color-prefs: framework:color-prefs^])
  
  (export (rename framework:canvas^
                  (-color% color%)))
  
  (define basic<%> (interface ((class->interface editor-canvas%))))
  (define basic-mixin
    (mixin ((class->interface editor-canvas%)) (basic<%>)
      (super-new)))
  
  (define color<%> (interface (basic<%>)))
  
  (define color-mixin
    (mixin (basic<%>) (color<%>)
      (define callback (λ (v) (set-canvas-background v)))
      (super-new)
      (inherit set-canvas-background)
      (set-canvas-background (color-prefs:lookup-in-color-scheme 'framework:basic-canvas-background))
      (color-prefs:register-color-scheme-entry-change-callback
       'framework:basic-canvas-background callback #t)))
  
  (define delegate<%> (interface (basic<%>)))
  
  (define delegate-mixin
    (mixin (basic<%>) (delegate<%>)
      (inherit get-top-level-window)
      (define/override (on-superwindow-show shown?)
        (define delegatee (send (get-top-level-window) get-delegatee))
        (when delegatee
          (send delegatee set-start/end-para #f #f))
        (super on-superwindow-show shown?))
      (super-new)))
  
  (define info<%> (interface (basic<%>)))
  ;; (basic<%> -> (class (is-a? (send this get-top-level-window) frame:info<%>)))
  (define info-mixin 
    (mixin (basic<%>) (info<%>)
      (inherit has-focus? get-top-level-window)
      (define/override (on-focus on?)
        (super on-focus on?)
        (send (get-top-level-window) set-info-canvas (and on? this))
        (when on?
          (send (get-top-level-window) update-info)))
      (define/override (set-editor m [redraw? #t])
        (super set-editor m redraw?)
        (let ([tlw (get-top-level-window)])
          (when (eq? this (send tlw get-info-canvas))
            (send tlw update-info))))
      
      (super-new)
      
      (unless (is-a? (get-top-level-window) frame:info<%>)
        (error 'canvas:text-info-mixin
               "expected to be placed into a frame or dialog implementing frame:info<%>, got: ~e" 
               (get-top-level-window)))
      
      (when (has-focus?)
        (send (get-top-level-window) update-info))))
  
  (define wide-snip<%> (interface (basic<%>)
                         recalc-snips
                         add-wide-snip
                         add-tall-snip))
  
  (define wide-snip-mixin
    (mixin (basic<%>) (wide-snip<%>)
      (inherit get-editor)
      (define/private ((update-snip-size width?) s)
        (let* ([width (box 0)]
               [height (box 0)]
               [leftm (box 0)]
               [rightm (box 0)]
               [topm (box 0)]
               [bottomm (box 0)]
               [left-edge-box (box 0)]
               [top-edge-box (box 0)]
               [snip-media (send s get-editor)]
               [edit (get-editor)]        
               [get-width
                (let ([bl (box 0)]
                      [br (box 0)])
                  (λ (s)
                    (send edit get-snip-location s bl #f #f)
                    (send edit get-snip-location s br #f #t)
                    (- (unbox br) (unbox bl))))]
               [calc-after-width
                (λ (s)
                  (+ 4 ;; this is compensate for an autowrapping bug
                     (let loop ([s s])
                       (cond
                         [(not s) 0]
                         [(member 'hard-newline (send s get-flags)) (get-width s)]
                         [(member 'newline (send s get-flags)) (get-width s)]
                         [else
                          (+ (get-width s)
                             2 ;; for the caret
                             (loop (send s next)))]))))])
          (when edit
            (send edit
                  run-after-edit-sequence
                  (λ ()
                    (let ([admin (send edit get-admin)])
                      (send admin get-view #f #f width height)
                      (send s get-margin leftm topm rightm bottomm)
                      
                      
                      ;; when the width is to be maximized and there is a
                      ;; newline just behind the snip, we know that the left
                      ;; edge is zero. Special case for efficiency in the 
                      ;; console printer
                      (let ([fallback
                             (λ ()
                               (send edit get-snip-location s left-edge-box top-edge-box))])
                        (cond
                          [(not width?) (fallback)]
                          [(let ([prev (send s previous)])
                             (and prev
                                  (member 'hard-newline (send prev get-flags))))
                           (set-box! left-edge-box 0)]
                          [else (fallback)]))
                      
                      (if width?
                          (let* ([after-width (calc-after-width (send s next))]
                                 [snip-width (max 0 (- (unbox width)
                                                       (unbox left-edge-box)
                                                       (unbox leftm)
                                                       (unbox rightm)
                                                       after-width
                                                       ;; this two is the space that 
                                                       ;; the caret needs at the right of
                                                       ;; a buffer.
                                                       2))])
                            (send* s 
                              (set-min-width snip-width)
                              (set-max-width snip-width))
                            (when snip-media
                              (send snip-media set-max-width
                                    (if (send snip-media auto-wrap)
                                        snip-width
                                        0))))
                          (let ([snip-height (max 0 (- (unbox height)
                                                       (unbox top-edge-box)
                                                       (unbox topm)
                                                       (unbox bottomm)))])
                            (send* s 
                              (set-min-height snip-height)
                              (set-max-height snip-height))))))))))
      (define/public (recalc-snips)
        (let ([editor (get-editor)])
          (when editor
            (unless (is-a? editor text:wide-snip<%>)
              (error 'recalc-snips "expected a text:wide-snip<%> editor, instead ~e" editor))
            (when (eq? (send editor get-canvas) this)
              (for-each (update-snip-size #t) (send editor get-wide-snips))
              (for-each (update-snip-size #f) (send editor get-tall-snips))))))
      (define/public (add-wide-snip snip)
        (let ([editor (get-editor)])
          (unless (is-a? editor text:wide-snip<%>)
            (error 'add-wide-snip "expected to have a text:wide-snip<%> editor, instead ~e" editor))
          (send editor add-wide-snip snip))
        ((update-snip-size #t) snip))
      (define/public (add-tall-snip snip)
        (let ([editor (get-editor)])
          (unless (is-a? editor text:wide-snip<%>)
            (error 'add-wide-snip "expected to have a text:wide-snip<%> editor, instead ~e" editor))
          (send editor add-tall-snip snip))
        ((update-snip-size #f) snip))
      (define/override (on-size width height)
        (recalc-snips)
        (super on-size width height))
      (super-new)))
  
  (define basic% (basic-mixin editor-canvas%))
  (define -color% (color-mixin basic%))
  (define info% (info-mixin basic%))
  (define delegate% (delegate-mixin basic%))
  (define wide-snip% (wide-snip-mixin basic%))
