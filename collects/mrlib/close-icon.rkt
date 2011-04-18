#lang scheme/base
(require scheme/gui/base
         scheme/class
         scheme/runtime-path
         (for-syntax scheme/base))
(provide close-icon%)

(define-runtime-path icon-path '(lib "close.png" "icons"))
  
(define icon 'icon-not-yet-init)
(define mask1 'mask-not-yet-init)
(define mask2 'mask-not-yet-init)
(define mask3 'mask-not-yet-init)

(define (init-masks)
  (define (for-each/b bytes r g b)
    (let loop ([i 0])
      (when (< i (bytes-length bytes))
        (bytes-set! bytes (+ i 1) (r (bytes-ref bytes (+ i 1))))
        (bytes-set! bytes (+ i 2) (g (bytes-ref bytes (+ i 2))))
        (bytes-set! bytes (+ i 3) (b (bytes-ref bytes (+ i 3))))
        (loop (+ i 4)))))
  
  (define stupid-internal-define-syntax1
    (set! icon (make-object bitmap% icon-path 'png/mask)))
  (define stupid-internal-define-syntax2
    (set! mask1 (send icon get-loaded-mask)))
  
  (define bytes (make-bytes (* (send icon get-width) (send icon get-width) 4)))
  (define bdc (make-object bitmap-dc% mask1))
  
  (set! mask2 (make-object bitmap% (send mask1 get-width) (send mask1 get-height)))
  (set! mask3 (make-object bitmap% (send mask1 get-width) (send mask1 get-height)))
  
  (send bdc get-argb-pixels 0 0 (send mask1 get-width) (send mask1 get-height) bytes)
  (send bdc set-bitmap mask2)
  (for-each/b bytes 
              (λ (x) (- 255 (floor (* (- 255 x) 2/3))))
              values
              values)
  (send bdc set-argb-pixels 0 0 (send mask1 get-width) (send mask1 get-height) bytes)
  
  (send bdc set-bitmap mask1)
  (send bdc get-argb-pixels 0 0 (send mask1 get-width) (send mask1 get-height) bytes)
  (send bdc set-bitmap mask3)
  (for-each/b bytes 
              (λ (x) (- 255 (floor (* (- 255 x) 1/4))))
              values
              values)
  (send bdc set-argb-pixels 0 0 (send mask1 get-width) (send mask1 get-height) bytes)
  
  (send bdc set-bitmap #f))

(define close-icon%
  (class canvas%
    (inherit get-dc min-width min-height stretchable-width stretchable-height
             get-client-size refresh)
    (init-field [callback void]
                [bg-color #f])
    (init [horizontal-pad 4]
          [vertical-pad 4])
    (init-masks)
    
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
                (and (<= (send evt get-x)
                         (send icon get-width))
                     (<= (send evt get-y)
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
          (send dc draw-bitmap icon 
                (- (/ cw 2) (/ (send icon get-width) 2))
                (- (/ ch 2) (/ (send icon get-height) 2))
                'solid
                (send the-color-database find-color "black")
                (cond
                  [(and mouse-in?
                        mouse-down?)
                   mask3]
                  [(and mouse-in?
                        (not mouse-down?))
                   mask2]
                  [else
                   mask1])))))
    
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
