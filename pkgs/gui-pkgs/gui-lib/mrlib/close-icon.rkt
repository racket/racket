#lang scheme/base
(require scheme/gui/base
         scheme/class
         scheme/runtime-path
         (for-syntax scheme/base))
(provide close-icon%)

(define-runtime-path icon-path '(lib "close.png" "icons"))
  
(define icon 'icon-not-yet-init)

(define (init-icon)
  (when (symbol? icon)
    (set! icon (read-bitmap icon-path #:try-@2x? #t))))

(define close-icon%
  (class canvas%
    (inherit get-dc min-width min-height stretchable-width stretchable-height
             get-client-size refresh)
    (init-field [callback void]
                [bg-color #f])
    (init [horizontal-pad 4]
          [vertical-pad 4])
    (init-icon)
    
    (define mouse-in? #f)
    (define mouse-down? #f)
    
    (define/override (on-event evt)
      (cond
        [(send evt leaving?)
         (set! mouse-in? #f)
         (refresh)]
        [(send evt entering?)
         (set! mouse-in? #t)
         (refresh)]
        [(send evt button-down?)
         (set! mouse-down? #t)
         (refresh)]
        [(send evt button-up?)
         (set! mouse-down? #f)
         (refresh)
         (when mouse-in? 
           (callback))]
        [(send evt moving?)
         (let ([new-mouse-in?
                (and (<= 0
                         (send evt get-x)
                         (send icon get-width))
                     (<= 0
                         (send evt get-y)
                         (send icon get-height)))])
           (unless (equal? new-mouse-in? mouse-in?)
             (set! mouse-in? new-mouse-in?)
             (refresh)))]))
    
    (define/override (on-paint)
      (let ([dc (get-dc)])
        (let-values ([(cw ch) (get-client-size)])
          (when bg-color
            (send dc set-brush bg-color 'solid)
            (send dc set-pen bg-color 1 'transparent)
            (let-values ([(w h) (get-client-size)])
              (send dc draw-rectangle 0 0 w h)))
          (send dc set-alpha (cond
                              [(and mouse-in?
                                    mouse-down?)
                               0.5]
                              [(and mouse-in?
                                    (not mouse-down?))
                               0.75]
                              [else
                               1]))
          (send dc draw-bitmap icon 
                (- (/ cw 2) (/ (send icon get-width) 2))
                (- (/ ch 2) (/ (send icon get-height) 2))
                'solid
                (send the-color-database find-color "black")))))

    (define/override (on-superwindow-show on?)
      (unless on?
        (set! mouse-in? #f)
        (set! mouse-down? #f)))
    
    (super-new [style '(transparent no-focus)])
    (min-width (+ horizontal-pad horizontal-pad (send icon get-width)))
    (min-height (+ vertical-pad vertical-pad (send icon get-height)))
    (stretchable-width #f)
    (stretchable-height #f)))

#;
(begin
  (define f (new frame% [label "test"]))
  (define c (new close-icon% [parent f] [callback (λ () (printf "hi\n"))]))
  (define gb (new grow-box-spacer-pane% [parent f]))
  (send f show #t))
