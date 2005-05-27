
(module splash mzscheme
  (require (lib "class.ss")
           (lib "file.ss")
           (lib "mred.ss" "mred"))
  
  (provide get-splash-bitmap
           set-splash-bitmap
           get-splash-canvas
           get-splash-eventspace
           start-splash 
           shutdown-splash
           close-splash
           add-splash-icon
           set-splash-char-observer
           set-splash-paint-callback
           get-splash-paint-callback
           set-splash-event-callback)
  
  (define splash-filename #f)
  (define splash-bitmap #f)
  (define splash-eventspace (make-eventspace))
  
  (define (get-splash-bitmap) splash-bitmap)
  (define (set-splash-bitmap bm) 
    (set! splash-bitmap bm)
    (send splash-canvas on-paint))
  (define (get-splash-canvas) splash-canvas)
  (define (get-splash-eventspace) splash-eventspace)

  (define (set-splash-paint-callback pc) (set! splash-paint-callback pc))
  (define (get-splash-paint-callback) splash-paint-callback)
  (define (set-splash-event-callback ec) (set! splash-event-callback ec))
  
  (define (splash-paint-callback dc) 
    (if splash-bitmap
        (send dc draw-bitmap splash-bitmap 0 0)
        (send dc clear))
    (for-each (λ (icon)
                (send dc draw-bitmap
                      (icon-bm icon)
                      (icon-x icon)
                      (icon-y icon)
                      'solid
                      (make-object color% "black")
                      (send (icon-bm icon) get-loaded-mask)))
              icons))
  (define (splash-event-callback evt) (void))
  
  (define char-observer void)
  (define (set-splash-char-observer proc)
    (set! char-observer proc))
  
  (define-struct icon (bm x y))
  (define icons null)
  (define (add-splash-icon bm x y)
    (set! icons (cons (make-icon bm x y) icons))
    (send splash-canvas on-paint))
  
  (define (start-splash _splash-filename _splash-title width-default)
    (set! splash-title _splash-title)
    (set! splash-filename _splash-filename)
    (set! splash-max-width (max 1 (splash-get-preference (get-splash-width-preference-name) width-default)))
    (send gauge set-range splash-max-width)
    (send splash-frame set-label splash-title)
    (let/ec k
      (define (no-splash)
        (set! splash-bitmap #f)
        (set! splash-canvas #f)
        (set! splash-eventspace #f)
        (k (void)))
      
      (unless splash-filename
        (no-splash))
      (unless (file-exists? splash-filename)
        (fprintf (current-error-port) "WARNING: bitmap path ~s not found~n" splash-filename)
        (no-splash))
      
      (set! splash-bitmap (make-object bitmap% splash-filename))
      (unless (send splash-bitmap ok?)
        (fprintf (current-error-port) "WARNING: bad bitmap ~s~n" splash-filename)
        (no-splash))
      
      (send splash-canvas min-width (send splash-bitmap get-width))
      (send splash-canvas min-height (send splash-bitmap get-height))
      (send splash-frame center 'both)
      (send splash-frame show #t)
      (flush-display) (yield) (sleep)
      (flush-display) (yield) (sleep)))
  
  (define splash-title "no title")
  
  (define splash-current-width 0)
  
  (define (get-splash-width-preference-name) 
    (string->symbol (format "plt:~a-splash-max-width" splash-title)))
  (define splash-max-width 1)
  
  (define (close-splash)
    (unless (= splash-max-width splash-current-width)
      (splash-set-preference (get-splash-width-preference-name) (max 1 splash-current-width)))
    (set! quit-on-close? #f)
    (when splash-frame
      (send splash-frame show #f)))
  
  (define (shutdown-splash)
    (set! splash-load-handler (λ (old-load f expected) (old-load f expected))))
   
  (define funny?
    (let ([date (seconds->date (current-seconds))])
      (and (with-handlers ([exn:fail:filesystem? (λ (x) #f)])
	     (collection-path "icons")
	     #t)
	     (= (date-day date) 25)
	     (= (date-month date) 12))))
  
   (define (splash-load-handler old-load f expected)
     (let ([finalf (splitup-path f)])
       (set! splash-current-width (+ splash-current-width 1))
       (when (<= splash-current-width splash-max-width)
         (send gauge set-value splash-current-width))
       (old-load f expected)))
  
  (let-values ([(make-compilation-manager-load/use-compiled-handler
                 manager-trace-handler)
                (if (or (getenv "PLTDRCM")
                        (getenv "PLTDRDEBUG"))
                    (parameterize ([current-namespace (make-namespace)])
                      (values
                       (dynamic-require '(lib "cm.ss") 'make-compilation-manager-load/use-compiled-handler)
                       (dynamic-require '(lib "cm.ss") 'manager-trace-handler)))
                    (values #f #f))])
    
    (current-load
     (let ([old-load (current-load)])
       (λ (f expected)
         (splash-load-handler old-load f expected))))
    
    (when (and make-compilation-manager-load/use-compiled-handler
               manager-trace-handler)
      (printf "PLTDRCM/PLTDRDEBUG: reinstalling CM load handler after setting splash load handler\n")
      (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler))
      (when (or (equal? (getenv "PLTDRCM") "trace")
                (equal? (getenv "PLTDRDEBUG") "trace"))
        (printf "PLTDRCM/PLTDRDEBUG: reinstalling CM trace handler after setting splash load handler\n")
        (manager-trace-handler
         (λ (x) (display "2: ") (display x) (newline))))))
  
  (define funny-gauge%
    (class canvas% 
      (inherit get-dc min-width min-height stretchable-width stretchable-height)
      (field
       [funny-value 0]
       [funny-bitmap
	(make-object bitmap% (build-path (collection-path "icons") "touch.bmp"))]
       [max-value 1])

      [define/public set-range (λ (r) (set! max-value r))]
      [define/public set-value
        (λ (new-value)
          (let* ([before-x
                  (floor (* (send funny-bitmap get-width) (/ funny-value max-value)))]
                 [after-x
                  (ceiling (* (send funny-bitmap get-width) (/ new-value max-value)))]
                 [width (- after-x before-x)])
            (send (get-dc) draw-line
                  (+ before-x 2) 0
                  (+ width 2) 0)
            (send (get-dc) draw-line
                  (+ before-x 2) (+ (send funny-bitmap get-height) 4)
                  (+ width 2) (+ (send funny-bitmap get-height) 4))
            (send (get-dc) draw-bitmap-section funny-bitmap
                  (+ 2 before-x) 2
                  before-x 0
                  width (send funny-bitmap get-height)))
          (set! funny-value new-value))]

      [define/override (on-paint)
        (let ([dc (get-dc)])
          (send dc clear)
          (send dc draw-rectangle 0 0
                (+ (send funny-bitmap get-width) 4)
                (+ (send funny-bitmap get-height) 4))
          (send dc draw-bitmap-section funny-bitmap
                2 2 0 0
                (* (send funny-bitmap get-width) (/ funny-value max-value))
                (send funny-bitmap get-height)))]

      (super-instantiate ())
      (min-width (+ (send funny-bitmap get-width) 4))
      (min-height (+ (send funny-bitmap get-height) 4))
      (stretchable-width #f)
      (stretchable-height #f)))
  
  (define (splash-get-preference name default)
    (get-preference
     name
     (λ ()
       default)))
  (define (splash-set-preference name value)
    (put-preferences (list name) (list value)))
  
  (define (splitup-path f)
    (let*-values ([(absf) (if (relative-path? f)
                              (build-path (current-directory) f)
                              f)]
                  [(base name _1) (split-path absf)])
      
      (if base
          (let-values ([(base2 name2 _2) (split-path base)])
            (if base2
                (let-values ([(base3 name3 _2) (split-path base2)])
                  (build-path name3 name2 name))
                (build-path name2 name)))
          name)))
  
  (define quit-on-close? #t)
  
  (define splash-frame%
    (class frame%
      (define/augment (on-close)
        (when quit-on-close?
          (exit)))
      (super-new)))
  
  (define splash-canvas%
    (class canvas%
      (inherit get-dc)
      (define/override (on-char evt) (char-observer evt))
      (define/override (on-paint) (splash-paint-callback (get-dc)))
      (define/override (on-event evt) (splash-event-callback evt))
      (super-new)))
  
  (define splash-frame
    (parameterize ([current-eventspace splash-eventspace])
      (instantiate splash-frame% ()
        (label splash-title)
        (style '(no-resize-border)))))
  (send splash-frame set-alignment 'center 'center)
  
  (define panel (make-object vertical-pane% splash-frame))
  (define splash-canvas (make-object splash-canvas% panel))
  (define h-panel (make-object horizontal-pane% panel))
  (define gauge
    (if funny?
        (make-object funny-gauge% h-panel)
        (make-object gauge% #f splash-max-width h-panel '(horizontal))))
  (send panel stretchable-width #f)
  (send panel stretchable-height #f)
  (send h-panel set-alignment 'center 'top)
  (send splash-canvas focus)
  (send splash-canvas stretchable-width #f)
  (send splash-canvas stretchable-height #f))
