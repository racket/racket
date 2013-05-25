(module const mzscheme
  (require mzlib/class
           mzlib/file
           racket/snip/private/prefs
           (prefix wx: "kernel.rkt"))
  (provide (protect (all-defined)))

  ;; default spacing between items.
  (define const-default-spacing 0)
  
  ;; default margins:
  (define const-default-x-margin 2)
  (define const-default-y-margin 2)

  ;; default spacing around edge of panel
  (define const-default-border 0)
  
  ;; the maximum hard-min-width of a gauge
  (define const-max-gauge-length 150)
  
  ;; maximum reasonable minimum width/height
  (define max-min 10000)

  (define side-combo-width (case (system-type)
			     [(windows) 20]
			     [(macosx) 18]
			     [else 16]))
  
  ;; message-box, etc.:
  (define box-width 300)
  
  (define err (current-error-port))
  
  ;; indicates init arg not supplied
  (define no-val (gensym))
  (define (no-val->#f v) (if (eq? v no-val) #f v))

  (define ibeam (make-object wx:cursor% 'ibeam))
  (define arrow-cursor (make-object wx:cursor% 'arrow))

  (define default-x-prefix (if (eq? 'unix (system-type))
			       (let ([v (get-preference*
                                         '|GRacket:defaultMenuPrefix| 
                                         (lambda () 'ctl))])
				 (if (memq v '(meta ctl alt ctl-m))
				     v
				     'ctl))
			       'ctl))

  (define (menu-shortcut-in-label?)
    (case (system-type)
      [(unix) (not (memq default-x-prefix '(alt meta)))]
      [else (wx:shortcut-visible-in-label? #t)]))

  (define bg-color (wx:get-panel-background))

  (define (scale-color c f)
    (make-object wx:color% 
		 (min 255 (floor (* f (send c red)))) 
		 (min 255 (floor (* f (send c green)))) 
		 (min 255 (floor (* f (send c blue))))))

  (define black-color (make-object wx:color% 0 0 0))
  (define disabled-color (make-object wx:color% 150 150 150))

  (define trans-pen (send wx:the-pen-list find-or-create-pen "white" 0 'transparent))
  (define light-pen (send wx:the-pen-list find-or-create-pen (scale-color bg-color #e1.35) 0 'solid))
  (define border-pen (send wx:the-pen-list find-or-create-pen (scale-color bg-color #e0.85) 0 'solid)) 
  (define dark-pen (send wx:the-pen-list find-or-create-pen (scale-color bg-color #e0.6) 0 'solid)) 
  (define dark-brush (send wx:the-brush-list find-or-create-brush (scale-color bg-color #e0.8) 'solid))
  
  (define wx-tab-group<%> (interface ()))
  (define wx-group-box<%> (interface ()))
  (define wx-text-editor-canvas<%> (interface ()))
  (define wx-basic-panel<%> (interface ()))
  (define internal-editor<%> (interface ()))
  (define internal-menu<%> (interface ())))
