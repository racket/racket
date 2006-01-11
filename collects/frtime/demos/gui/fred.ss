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
  
  (define (add-callback-access val-ext super-class)
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
  
  (define (add-callback-access/loop val-ext super-class)
    ((events->callbacks value-set set-value)
     (add-callback-access val-ext super-class)))
  
  
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
    (class ((callbacks->args-evts resize-events on-size (w h))
            (add-mouse-access 
             (add-keypress-split 
              (add-signal-controls frame% 
                                   (label set-label "")))))
      (super-new)
      ))
  
  

  
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
     (add-void-set-value
      menu-item%)))
  
  
  
  (define (send-for-value w e)
    (send w get-value))
  
  (define (send-for-selection w e)
    (send w get-selection))
  
  
  ;; Standard mixin combinations
  (define (standard-lift widget value-method)
    (add-mouse-access
     (add-focus-access
      (add-callback-access
       value-method
       (add-signal-controls (add-void-set-value widget) (label set-label "") (enabled enable #t))))))
  
  (define (standard-lift/loop widget value-method)
    (add-mouse-access
     (add-focus-access
      (add-callback-access/loop
       value-method
       (add-signal-controls widget (label set-label "") (enabled enable #t))))))
  
  
  
  (define ft-button%
    (standard-lift button% (lambda (w e) e)))  
  
  (define ft-check-box%
    (standard-lift/loop check-box% send-for-value))
  
  (define ft-radio-box%
    (standard-lift radio-box% send-for-selection))
  
  (define ft-choice%
    (standard-lift choice% send-for-selection))
  
  (define ft-slider%
    (standard-lift/loop slider% send-for-value))
  
  (define ft-list-box% 
    (standard-lift list-box% send-for-selection))
  
  (define ft-text-field%
    (add-keypress-split
     (add-focus-on-event
      (standard-lift/loop text-field% send-for-value))))
  
 
  
  
  
  
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



