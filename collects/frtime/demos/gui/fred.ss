(module fred (lib "frtime.ss" "frtime")
  (require "mixin-macros.ss"
           ;"r-label.ss"
           (lib "class.ss")
           (lib "string.ss")
           (all-except (lib "mred.ss" "mred") send-event)
           (lib "framework.ss" "framework"))
  
  (define-syntax add-signal-controls
    (syntax-rules ()
      [(_ src (field-name0 update-call0 default-val0) clause ...)
       ((behavior->callbacks field-name0 update-call0)
        default-val0
        (add-signal-controls src clause ...))]
      [(_ src)
       src]))
  

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Helpers
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; adding assumed methods
  (define (add-void-set-value super-class)
    (class super-class
      (define/public (set-value v) (void))
      (super-new)))
  
  (define (add-focus-now super-class)
    (class super-class
      (super-new)
      (inherit focus)
      (define/public (focus-now _) (focus))))
  
  (define (callback->pub-meth super-class)
    (class super-class
      (define/public (callback-method w e) (void))
      (super-new (callback (lambda (w e) (callback-method w e))))))
  
  
  ;; *-event-processor init-argument values
  (define event-is-val
    (lambda (es)
      (map-e car es)))
  
  ; (send x get-mouse-events) returns a split procedure over the event-type
  (define split-mouse-events/type
    (lambda (evt-src)
      (split (map-e cadr evt-src) (lambda (evt) (send evt get-event-type)))))
  
  ; (send x get-key-events) returns a split procedure over the key code
  (define split-key-events/type
    (lambda (evt-src)
      (split (map-e cadr evt-src) (lambda (evt) (send evt get-key-code)))))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; make state available as eventstreams
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (add-mouse-access super-class)
    ((callbacks->args-evts mouse-events ; Name of event stream
                           on-subwindow-event ; proc overriding
                           (window evt) ; arguments for on-subwindow-event. Caused by super being a macro
                           )
     super-class))
  
  
  (define (add-focus-access super-class)
    ((callbacks->args-evts focus-events on-focus (is-focused?))
     super-class))
  
  (define (add-keypress-split super-class)
    ((callbacks->args-evts key-events on-subwindow-char (w e))
     super-class))
  
  
  
  #|
  (define (add-value-b super-class default)
    (class super-class
      (super-new)
      (inherit get-value-e)
      (define/public (get-value-b) (hold (get-value-e) default))))
  |#
  
  (define (add-callback-access val-ext default-val super-class)
    (class ((callbacks->args-evts set-value-events
                                  set-value
                                  (v))
            ((callbacks->args-evts callback-events
                                   callback-method
                                   (w e))
             (callback->pub-meth super-class)))
      (super-new (set-value-events-event-processor event-is-val)
                 (callback-events-event-processor (lambda (es)
                                                    (map-e (lambda (e) (apply val-ext e)) es))))
      (inherit get-set-value-events get-callback-events)
      (define value-e (merge-e (get-set-value-events)
                               (get-callback-events)))
      (define value-b (hold value-e (val-ext this #f)))
      (define/public (get-value-e) value-e)
      (define/public (get-value-b) value-b)
      ))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; using events to drive object interaction
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (add-callback-access/loop val-ext default-val super-class)
    ((events->callbacks value-set set-value)
     (add-callback-access val-ext default-val super-class)))
  
  
  (define (add-focus-on-event super-class)
    ((events->callbacks focus-when focus-now)
     (add-focus-now super-class)))
  
  
  
    ;; Special case widgets
  (define (in-string itm)
    (if (undefined? itm)
        ""
        (if (string? itm)
            itm
            (expr->string itm))))
  
  
  
  (define ft-frame%
    (class (add-mouse-access (add-keypress-split (add-signal-controls frame% (label set-label ""))))
      ; Members, initialized
      (init-field (its-width 800) (its-height 600) (label-text "") (x-loc 0) (y-loc 0))
      #|
      ;(make-prog-control label-text set-label)
      
      ; Private members, internal
      (define width-e (event-receiver))
      (define width-b (hold width-e its-width))
      (define height-e (event-receiver))
      (define height-b (hold height-e its-height))
      
      (define mouse-x-e (event-receiver))
      (define mouse-x-b (hold mouse-x-e 0))
      (define mouse-y-e (event-receiver))
      (define mouse-y-b (hold mouse-y-e 0))
      
      ; Overridden methods
      (override on-size on-subwindow-event)
      
      ; Overrides on-size from frame% to update width-e and height-e
      (define (on-size new-width new-height)
        (begin
          (send-event width-e new-width)
          (send-event height-e new-height)
          
          (super on-size new-width new-height)))
      
      (define (on-subwindow-event a-window event)
        (begin
          (case (send event get-event-type)
            [(enter motion)
             (send-event mouse-x-e (+ (send a-window get-x) (send event get-x)))
             (send-event mouse-y-e (+ (send a-window get-y) (send event get-y)))])
          (super on-subwindow-event a-window event)))
      
      ; Public Members    
      (public get-width-b get-height-b get-mouse-x get-mouse-y)
      
      ; Returns a behavior of the width of the frame
      (define (get-width-b) width-b)
      
      ; Returns a behavior of the height of the frame
      (define (get-height-b) height-b)
      
      (define (get-mouse-x) mouse-x-b)
      (define (get-mouse-y) mouse-y-b)
      |#
      (super-new (label (in-string (value-now label-text))) 
                 (width its-width) 
                 (height its-height)
                 (x x-loc)
                 (y y-loc)
                 #;(style '(float metal)))))
  
  

  
  (define ft-message% 
    (add-mouse-access
     (add-focus-access
      (add-signal-controls message% (label set-label "") (enabled enable #t)))))
  
  #;(define ft-autoresize-label% 
    (add-mouse-access
     (add-focus-access
      (add-signal-controls autoresize-label% (text set-label-text "") (enabled enable #t)))))
  
  
  (define specialized-gauge%
    (class gauge%
      (init value)
      
      (super-new)
      
      (inherit set-value)
      #;(set-value value)))
  
  (define ft-gauge%
    (add-mouse-access
     (add-focus-access
      (add-signal-controls specialized-gauge% 
                           (label set-label "") 
                           (enabled enable #t)
                           (value set-value 0)
                           (range set-range 1)))))
  
  (define ft-menu-item%
    (add-callback-access
     list
     '()
     (add-void-set-value
      menu-item%)))
  
  
  
  (define (send-for-value w e)
    (send w get-value))
  
  (define (send-for-selection w e)
    (send w get-selection))
  
  
  ;; Standard mixin combinations
  (define (standard-lift widget value-method value-default)
    (add-mouse-access
     (add-focus-access
      (add-callback-access
       value-method
       value-default
       (add-signal-controls (add-void-set-value widget) (label set-label "") (enabled enable #t))))))
  
  (define (standard-lift/loop widget value-method value-default)
    (add-mouse-access
     (add-focus-access
      (add-callback-access/loop
       value-method
       value-default
       (add-signal-controls widget (label set-label "") (enabled enable #t))))))
  
  
  
  (define ft-button%
    (standard-lift button% (lambda (w e) e) undefined))  
  
  (define ft-check-box%
    (standard-lift/loop check-box% send-for-value #f))
  
  (define ft-radio-box%
    (standard-lift radio-box% send-for-selection 0))
  
  (define ft-choice%
    (standard-lift choice% send-for-selection 0))
  
  (define ft-slider%
    (standard-lift/loop slider% send-for-value 0))
  
  (define ft-list-box% 
    (standard-lift list-box% send-for-selection 0))
  
  (define ft-text-field%
    (add-keypress-split
     (add-focus-on-event
      (standard-lift/loop text-field% send-for-value ""))))
  
 
  
  
  
  
  (provide ft-frame%
           ft-message%
           ;ft-autoresize-label%
           ft-gauge%
           ft-button%
           ft-check-box%
           ft-radio-box%
           ft-choice%
           ft-slider%
           ft-list-box%
           ft-text-field%
           ft-menu-item%
           menu%
           menu-bar%
           finder:get-file
           finder:put-file
           split-mouse-events/type
           split-key-events/type
           (all-from (lib "class.ss"))
           (all-from "mixin-macros.ss"))) 



