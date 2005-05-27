#| Rewrite these to use snip-wrapper% automatically |#

(module button-snip mzscheme
  
  (require
   (lib "mred.ss" "mred")
   (lib "class.ss")
   (lib "etc.ss")
   "snip-wrapper.ss")
  
  (provide
   text-button-snip%
   button-snip%
   toggle-button-snip%
   embedded-button%
   embedded-text-button%
   embedded-toggle-button%)
  
  (define embedded-button%
    (class snip-wrapper%
      (init images callback)
      (super-new
       (snip (new button-snip%
                  (images images)
                  (callback callback))))))
  
  (define embedded-text-button%
    (class snip-wrapper%
      (init label callback)
      (super-new
       (snip (new text-button-snip%
                  (label label)
                  (callback callback))))))
  
  (define embedded-toggle-button%
    (class snip-wrapper%
      (init images-off images-on turn-off turn-on (state 'on))
      (field [btn (new toggle-button-snip%
                       (images-off images-off)
                       (images-on images-on)
                       (turn-off turn-off)
                       (turn-on turn-on)
                       (state state))])
      ;; Provide the original toggle-button-snip% interface
      (define/public (set-state x) (send btn set-state x))
      (super-new (snip btn))))
  
  ;; a snip of a button that can be pushed to invoke a given callback
  (define button-snip%
    (class image-snip%
      (inherit load-file)
      (init images)
      (init-field callback)
      (field
       [got-click? false]
       [inside? false]
       [image (car images)]
       [depressed (cdr images)])
      
      ;; (string? . -> . void?)
      ;; set the image to be displayed on the button when it is not clicked
      (define/public (set-images i)
        (set! image (car i))
        (set! depressed (cdr i))
        (load-file image))
      
      ;; Should I be calling super-on-event?
      (define/override (on-event dc x y editorx editory event)
        (case (send event get-event-type)
          [(left-down)
           (set! got-click? true)
           (set! inside? true)
           (load-file depressed)]
          [(left-up)
           (load-file image)
           (when (and got-click? inside?)
             (callback this event))
           (set! got-click? false)
           (set! inside? false)]
          [(enter)
           (set! inside? true)
           (when got-click?
             (load-file depressed))]
          [(leave)
           (set! inside? false)
           (when got-click?
             (load-file image))]
          [else (void)]))
      
      (super-new)
      (load-file image)))
  
  ;; a textual button of the same type
  (define text-button-snip%
    (class string-snip%
      (init label)
      (init-field callback)
      (field
       [got-click? false]
       [inside? false])

      (define/override (on-event dc x y editorx editory event)
        (case (send event get-event-type)
          [(left-down)
           (set! got-click? true)
           (set! inside? true)]
          [(left-up)
           (when (and got-click? inside?)
             (callback this event))
           (set! got-click? false)
           (set! inside? false)]
          [(enter)
           (set! inside? true)]
          [(leave)
           (set! inside? false)]
          [else (void)]))
      
      (super-make-object label)
      (inherit set-style)
      (set-style control-style)))
  
  ;; a toggle button that displays different images
  (define toggle-button-snip%
    (class button-snip%
      (inherit set-images)
      (init-field images-off images-on turn-off turn-on (state 'on))
      
      ;; Emulates clicking the button to a certain state
      (define/public (turn astate)
        (case astate
          [(off) (set-state 'off)
                 (turn-on this #f)]
          [(on) (set-state 'on)
                (turn-off this #f)]))
      
      (define/public (set-state value)
        (case value
          [(off) (set-images images-off)
                 (set! state 'off)]
          [(on) (set-images images-on)
                (set! state 'on)]))
      
      (super-new
       (images (case state
                 [(on) images-on]
                 [(off) images-off]))
       (callback
        (lambda (b e)
          ;; NOTE: I lose the event right here, but turn can't require it.
          ;; Since it's public.
          (case state
            [(on) (turn 'off)]
            [(off) (turn 'on)]))))))
  
  ;;;;;;;;;;
  ;; tests
  #|
  (require
   (lib "locked-pasteboard.ss" "mrlib" "private" "aligned-pasteboard")
   (lib "click-forwarding-editor.ss" "mrlib"))
  
  (define (test)
    (define f (new frame% (label "test") (width 200) (height 200)))
    (define e (new (locked-pasteboard-mixin
                    (click-forwarding-editor-mixin pasteboard%))))
    (define c (new editor-canvas% (editor e) (parent f)))
    (define b (new button-snip%
                   (images (cons (build-path (collection-path "icons") "turn-up.gif")
                                 (build-path (collection-path "icons") "turn-up-click.gif")))
                   (callback
                    (lambda (b e)
                      (message-box "Test" "Horray!")))))
    (send e insert b)
    (send f show #t))
  
  (define (test2)
    (define f (new frame% (label "test") (width 200) (height 200)))
    (define e (new (locked-pasteboard-mixin
                    (click-forwarding-editor-mixin pasteboard%))))
    (define c (new editor-canvas% (editor e) (parent f)))
    (define t (new text%))
    (define es (new editor-snip% (editor t)))
    (define b (new toggle-button-snip%
                   (images-on (cons (build-path (collection-path "icons") "turn-up.gif")
                                  (build-path (collection-path "icons") "turn-up-click.gif")))
                   (images-off (cons (build-path (collection-path "icons") "turn-down.gif")
                                  (build-path (collection-path "icons") "turn-down-click.gif")))
                   (turn-on
                    (lambda (b e)
                      (send* t (erase) (insert "Up"))))
                   (turn-off
                    (lambda (b e)
                      (send* t (erase) (insert "Down"))))))
    (send e insert es 50 0)
    (send e insert b)
    (send f show #t))
  |#
)
