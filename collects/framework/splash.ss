#lang scheme/base

(require scheme/class
         scheme/file
         scheme/gui/base)

(provide get-splash-bitmap
         set-splash-bitmap
         get-splash-canvas
         get-splash-eventspace
         get-splash-paint-callback
         set-splash-paint-callback
         start-splash 
         shutdown-splash
         close-splash
         add-splash-icon
         set-splash-progress-bar?
         set-splash-char-observer
         set-splash-event-callback
         get-splash-event-callback
         set-refresh-splash-on-gauge-change?!
         get-splash-width
         get-splash-height
         refresh-splash)

(define splash-bitmap #f)
(define splash-cache-bitmap #f)
(define splash-cache-dc (make-object bitmap-dc%))
(define splash-eventspace (make-eventspace))

(define (get-splash-bitmap) splash-bitmap)
(define (set-splash-bitmap bm) 
  (set! splash-bitmap bm)
  (send splash-canvas on-paint))
(define (get-splash-canvas) splash-canvas)
(define (get-splash-eventspace) splash-eventspace)

(define (get-splash-paint-callback) splash-paint-callback)
(define (set-splash-paint-callback sp) 
  (set! splash-paint-callback sp)
  (refresh-splash))

(define (get-splash-width) (send splash-canvas get-width))
(define (get-splash-height) (send splash-canvas get-height))

(define (set-splash-event-callback cb) (set! splash-event-callback cb))
(define (get-splash-event-callback cb) splash-event-callback)

(define (refresh-splash-on-gauge-change? start range) #f)
(define (set-refresh-splash-on-gauge-change?! f) (set! refresh-splash-on-gauge-change? f))

(define (refresh-splash)
  
  (define (recompute-bitmap/refresh)
    (send splash-cache-dc set-bitmap splash-cache-bitmap)
    (call-splash-paint-callback splash-cache-dc)
    (send splash-cache-dc set-bitmap #f)
    (send splash-canvas on-paint))
  
  (cond
    [(not (is-a? splash-cache-bitmap bitmap%)) (void)]
    [(eq? (current-thread) (eventspace-handler-thread splash-eventspace))
     (recompute-bitmap/refresh)]
    [else
     (parameterize ([current-eventspace splash-eventspace])
       (queue-callback
        recompute-bitmap/refresh))]))
        
(define (call-splash-paint-callback dc)
  (cond
    [(equal? 1 (procedure-arity splash-paint-callback))
     (splash-paint-callback dc)]
    [else 
     (splash-paint-callback dc 
                            (send gauge get-value) 
                            (send gauge get-range)
                            (get-splash-width)
                            (get-splash-height))])
  (for-each (λ (icon)
              (send dc draw-bitmap
                    (icon-bm icon)
                    (icon-x icon)
                    (icon-y icon)
                    'solid
                    (make-object color% "black")
                    (send (icon-bm icon) get-loaded-mask)))
            icons))

(define (set-splash-progress-bar? b?) 
  (send gauge-panel change-children
        (λ (l) (if b? (list gauge) '()))))

(define (splash-paint-callback dc)
  (if splash-bitmap
      (send dc draw-bitmap splash-bitmap 0 0)
      (send dc clear)))

(define (splash-event-callback evt) (void))

(define char-observer void)
(define (set-splash-char-observer proc)
  (set! char-observer proc))

(define-struct icon (bm x y))
(define icons null)
(define (add-splash-icon bm x y)
  (set! icons (cons (make-icon bm x y) icons))
  (refresh-splash))

(define (start-splash splash-draw-spec _splash-title width-default)
  (set! splash-title _splash-title)
  (set! splash-max-width (max 1 (splash-get-preference (get-splash-width-preference-name) width-default)))
  (send gauge set-range splash-max-width)
  (send splash-frame set-label splash-title)
  (let/ec k
    (define (no-splash)
      (set! splash-bitmap #f)
      (set! splash-canvas #f)
      (set! splash-eventspace #f)
      (k (void)))
  
    (cond
      [(or (path? splash-draw-spec)
           (string? splash-draw-spec))
       (unless (file-exists? splash-draw-spec)
         (fprintf (current-error-port) "WARNING: bitmap path ~s not found~n" splash-draw-spec)
         (no-splash))
       
       (set! splash-bitmap (make-object bitmap% splash-draw-spec))
       (unless (send splash-bitmap ok?)
         (fprintf (current-error-port) "WARNING: bad bitmap ~s~n" splash-draw-spec)
         (no-splash))
       
       (send splash-canvas min-width (send splash-bitmap get-width))
       (send splash-canvas min-height (send splash-bitmap get-height))
       (set! splash-cache-bitmap (make-object bitmap% 
                                   (send splash-bitmap get-width)
                                   (send splash-bitmap get-height)))]
      [(and (vector? splash-draw-spec)
            (procedure? (vector-ref splash-draw-spec 0))
            (number? (vector-ref splash-draw-spec 1))
            (number? (vector-ref splash-draw-spec 2)))
       (set! splash-paint-callback (vector-ref splash-draw-spec 0))
       (send splash-canvas min-width (vector-ref splash-draw-spec 1))
       (send splash-canvas min-height (vector-ref splash-draw-spec 2))
       (set! splash-cache-bitmap (make-object bitmap% 
                                   (vector-ref splash-draw-spec 1)
                                   (vector-ref splash-draw-spec 2)))]
      [(not splash-draw-spec)
       (no-splash)]
      [else
       (fprintf (current-error-port)
                "WARNING: unknown splash spec: ~s" splash-draw-spec)
       (no-splash)])
    
    (refresh-splash)
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
  (set! splash-current-width (+ splash-current-width 1))
  (when (<= splash-current-width splash-max-width)
    (send gauge set-value splash-current-width)
    (when (or (not (member gauge (send gauge-panel get-children)))
              ;; when the gauge is not visible, we'll redraw the canvas
              (refresh-splash-on-gauge-change? splash-current-width splash-max-width))
      (refresh-splash)))
  (old-load f expected))

(let-values ([(make-compilation-manager-load/use-compiled-handler
               manager-trace-handler)
              (if (or (getenv "PLTDRCM")
                      (getenv "PLTDRDEBUG"))
                  (parameterize ([current-namespace (make-base-namespace)])
                    (printf "hello?\n")
                    (values
                     (dynamic-require 'compiler/cm 'make-compilation-manager-load/use-compiled-handler)
                     (dynamic-require 'compiler/cm 'manager-trace-handler)))
                  (values #f #f))])
  
  (current-load
   (let ([old-load (current-load)])
     (λ (f expected)
       (splash-load-handler old-load f expected))))
  (printf ">> ~s\n" (list  make-compilation-manager-load/use-compiled-handler
                           manager-trace-handler))
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
    
    (define/public (get-range) max-value)
    (define/public (get-value) funny-value) 
    
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

(define quit-on-close? #t)

(define splash-frame%
  (class frame%
    (define/augment (on-close)
      (when quit-on-close?
        (exit)))
    (super-new)))

(define splash-canvas%
  (class canvas%
    (inherit get-client-size get-dc)
    (define/override (on-char evt) (char-observer evt))
    (define/override (on-paint) (when splash-cache-bitmap (send (get-dc) draw-bitmap splash-cache-bitmap 0 0)))
    (define/override (on-event evt) (splash-event-callback evt))
    (super-new)))

(define splash-frame
  (parameterize ([current-eventspace splash-eventspace])
    (instantiate splash-frame% ()
      (label splash-title)
      (style '(no-resize-border)))))
(send splash-frame set-alignment 'center 'center)

(define panel (make-object vertical-pane% splash-frame))
(define splash-canvas (new splash-canvas% [parent panel] [style '(no-autoclear)]))
(define gauge-panel (make-object horizontal-pane% panel))
(define gauge
  (if funny?
      (make-object funny-gauge% gauge-panel)
      (make-object gauge% #f splash-max-width gauge-panel '(horizontal))))
(send panel stretchable-width #f)
(send panel stretchable-height #f)
(send gauge-panel set-alignment 'center 'top)
(send splash-canvas focus)
(send splash-canvas stretchable-width #f)
(send splash-canvas stretchable-height #f)
