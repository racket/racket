(module fred frtime
  (require "mixin-macros.rkt"
           "aux-mixin-macros.rkt"
           racket/class
           racket/string
           texpict/mrpict
           mred
           framework)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Helpers
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; adding assumed methods
  (define (add-void-set-value super-class)
    (class super-class
      (define/public (set-value v) (void))
      (super-new)))
  
  (define (callback->pub-meth super-class)
    (class super-class
      (define/public (callback-method w e) (void))
      (super-new (callback (lambda (w e) (callback-method w e))))))
  
  (define (add-shown super-class)
    (class super-class
      (init (shown #f))
      (define shown-val shown)
      (super-new)
      (inherit show)
      (show shown-val)))
      
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
  
  
  (define (send-for-value w e)
    (send w get-value))
  
  (define (send-for-selection w e)
    (send w get-selection))
  
  (define (send-for-selections w e)
    (send w get-selections))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; make state available as eventstreams
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (add-mouse-access super-class)
    ((callbacks->args-evts mouse-events ; Name of event stream
                          on-subwindow-event ; proc overriding
                          )
     split-mouse-events/type
     super-class))
  
  
  (define (add-focus-access super-class)
    ((callbacks->args-evts focus-events on-focus)
     event-is-val
     super-class))
  
  (define (add-keypress-split super-class)
    ((callbacks->args-evts key-events on-subwindow-char)
     split-key-events/type
     super-class))
  
  
  (define (add-size-access super-class)
    ((callbacks->args-evts size-events on-size)
     (lambda (x) x)
     (class super-class
       (super-new)
       (define/public (get-size-as-list)
         (list (send this get-width)
               (send this get-height))))))
  

  
  (define (add-size-b super-class)
    ((mixin-hold size-b get-size-as-list get-size-events)
     (add-size-access super-class)))
  

  
  (define (add-position-access super-class)
    ((callbacks->args-evts position-events on-move)
     (lambda (x) x)
     (class super-class
       (super-new)
       (define/public (get-position-as-list)
         (list (send this get-x)
               (send this get-y))))))
  
  
  (define (add-position-b super-class)
    ((mixin-hold position-b 
                 get-position-as-list 
                 get-position-events)
     (add-position-access super-class)))
  
  
  
  (define (monitor-set-value super-class)
    ((callbacks->args-evts set-value-events set-value)
     event-is-val
     super-class))
  
  (define (monitor-callback-method super-class)
    ((callbacks->args-evts callback-events callback-method)
     (lambda (x) x)
     super-class))
  
  
  (define (add-callback-access val-ext super-class)
    ((mixin-merge-e
      value-e
      get-set-value-events 
      get-callback-events)
     (class (monitor-set-value
             (monitor-callback-method
              (callback->pub-meth super-class)))
       (super-new (callback-events-event-processor 
                   (lambda (es) (map-e (lambda (e) (apply val-ext e)) es)))))))
  
  (define (add-callback-access/selection val-ext super-class)
    ((mixin-merge-e
      selection-e
      get-set-value-events 
      get-callback-events)
     (class (monitor-set-value
             (monitor-callback-method
              (callback->pub-meth super-class)))
       (super-new (callback-events-event-processor 
                   (lambda (es) (map-e (lambda (e) (apply val-ext e)) es)))))))

  (define (add-callback-access/selections val-ext super-class)
    ((mixin-merge-e
      selections-e
      get-set-value-events 
      get-callback-events)
     (class (monitor-set-value
             (monitor-callback-method
              (callback->pub-meth super-class)))
       (super-new (callback-events-event-processor 
                   (lambda (es) (map-e (lambda (e) (apply val-ext e)) es)))))))
  
  (define add-value-b (mixin-hold value-b get-value get-value-e))
                       
  (define add-selection-b (mixin-hold selection-b get-selection get-selection-e))
  
  (define add-selections-b (mixin-hold selections-b get-selections get-selections-e))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; using events to drive object interaction
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (add-callback-access/loop val-ext super-class)
    ((events->callbacks value-set set-value)
     (add-callback-access val-ext super-class)))
  
  
  (define (add-focus-on-event super-class)
    (class ((events->callbacks focus-when carries-args-for focus)
            super-class)
      (init (focus-when (event-receiver)))
      (define focus-map (map-e (lambda (_) '()) focus-when))
      (super-new (focus-when focus-map))))
  
  
  
 

  (define (control-stretchability default widget)
    (add-signal-controls
     widget
     (stretchable-width stretchable-width default)
     (stretchable-height stretchable-width default)))
  
    ;; Standard mixin combinations
  (define (standard-lift widget)
    (add-size-b
     (add-position-b
      (add-keypress-split
       (add-focus-on-event
        (add-mouse-access
         (add-focus-access
          (add-signal-controls 
           widget
           (label set-label "") 
           (enabled enable #t)
           (min-width min-width 0)
           (min-height min-height 0)
           ))))))))
  
  (define (standard-container-lift widget)
    (control-stretchability
     #t
     (standard-lift widget)))
                                 
  
  (define (standard-input-lift accessor val-ext)
    (lambda (super-class)
      (add-value-b
       (accessor val-ext super-class))))
  
  (define (selection-input-lift accessor val-ext)
    (lambda (super-class)
      (add-selection-b
       (accessor val-ext super-class))))
  
  (define (selections-input-lift accessor val-ext)
    (lambda (super-class)
      (add-selections-b
       (accessor val-ext super-class))))
  
  (define ft-frame%
    ((behavior->callbacks shown show)
     #f
     (add-shown
      (standard-container-lift frame%))))
      
  (define ft-message% 
    (standard-lift message%))
  
  (define ft-button%
    (add-callback-access (lambda (w e) e) (add-void-set-value (standard-lift button%))))
  
  (define ft-check-box%
    ((standard-input-lift add-callback-access/loop send-for-value)
     (standard-lift check-box%)))
  
  (define ft-slider%
    ((standard-input-lift add-callback-access/loop send-for-value)
     (standard-lift slider%))) ;ideally the default should be the minimum value
  
  (define ft-text-field%
    ((standard-input-lift add-callback-access/loop send-for-value)
     (standard-lift text-field%)))

  (define ft-radio-box%
    ((selection-input-lift add-callback-access/selection send-for-selection)
     (add-void-set-value (standard-lift radio-box%))))
  
  (define ft-choice%
    ((selection-input-lift add-callback-access/selection send-for-selection)
     (add-void-set-value (standard-lift choice%))))
  
  (define ft-list-box%
    (class ((selections-input-lift add-callback-access/selections send-for-selections)
            (add-void-set-value (standard-lift list-box%)))
      (super-new)
      (define/public (get-selection-b)
        (let ([selections-b (send this get-selections-b)])
          (if (null? selections-b)
              #f
              (car selections-b))))))
  
  (define ft-canvas%
    (class (standard-lift canvas%)
      (inherit get-dc refresh get-width get-height)
      (init-field pict)
      (define bitmap #f)
      (define bitmap-dc #f)
      (super-new [paint-callback (lambda (canvas dc)
                                   (unless (and bitmap
                                                (= (send bitmap get-width) (get-width))
                                                (= (send bitmap get-height) (get-height)))
                                     (set! bitmap (make-object bitmap% (get-width) (get-height)))
                                     (set! bitmap-dc (new bitmap-dc% [bitmap bitmap])))
                                   (unless (undefined? (value-now pict))
                                     (send bitmap-dc clear)
                                     (draw-pict (value-now pict) bitmap-dc 0 0)
                                     (send dc draw-bitmap bitmap 0 0)))])
      (for-each-e! (changes pict) (lambda (_) (refresh)))))
  
  ;; Special case widgets

  
  (define specialized-gauge%
    (add-signal-controls
     (class gauge%
       (init value)
       (super-new)
       (send this set-value value))
     (value set-value 0)
     (range set-range 1)))
  
  
  (define ft-gauge%
    (standard-lift specialized-gauge%))
  
  
  (define ft-menu-item%
    (add-callback-access
     list
     (add-void-set-value
      menu-item%)))  
  
  
  (provide (all-defined-out)
           (all-from-out racket/class)
           (all-from-out "mixin-macros.rkt")
           (all-from-out "aux-mixin-macros.rkt")))
