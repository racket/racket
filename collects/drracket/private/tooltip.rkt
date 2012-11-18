#lang racket/base
(require racket/gui/base
         racket/class)

(provide tooltip-frame%)

(define tooltip-frame%
  (class frame%
    (inherit reflow-container move get-width get-height is-shown?)
    
    (init-field [frame-to-track #f])
    (define timer
      (and frame-to-track
           (new timer%
                [notify-callback
                 (λ ()
                   (unless (send frame-to-track is-shown?)
                     (show #f)
                     (send timer stop)))])))
                
    
    (define/override (on-subwindow-event r evt)
      (and (is-shown?)
           (begin (show #f)
                  #t)))
    (define/public (set-tooltip ls) 
      (send yellow-message set-lab ls))
    
    (define/override (show on?)
      (when timer
        (cond
          [on? (send timer start 200 #f)]
          [else (send timer stop)]))
      (super show on?))
    
    (define/public (show-over x y w h #:prefer-upper-left? [prefer-upper-left? #f])
      (reflow-container)
      (define mw (get-width))
      (define mh (get-height))
      (define (upper-left must?) 
        (define the-x (- x mw))
        (define the-y (- y mh))
        (if must?
            (move the-x the-y)
            (try-moving-to the-x the-y mw mh)))
      (define (lower-right must?) 
        (define the-x (+ x w))
        (define the-y (+ y h))
        (if must?
            (move the-x the-y)
            (try-moving-to the-x the-y mw mh)))
      (if prefer-upper-left?
          (or (upper-left #t) (lower-right #f) (upper-left #t))
          (or (lower-right #t) (upper-left #f) (lower-right #t)))
      (show #t))
    
    (define/private (try-moving-to x y w h)
      (and (for/or ([m (in-range 0 (get-display-count))])
             (define-values (mx my) (get-display-left-top-inset #:monitor m))
             (define-values (mw mh) (get-display-size #:monitor m))
             (and (<= (- mx) x (+ x w) (+ (- mx) mw))
                  (<= (- my) y (+ y h) (+ (- my) mh))))
           (begin (move x y)
                  #t)))

    (super-new [style '(no-resize-border no-caption float)]
               [label ""]
               [stretchable-width #f]
               [stretchable-height #f])
    (define yellow-message (new yellow-message% [parent this]))))

(define yellow-message%
  (class canvas%
    (inherit get-dc refresh get-client-size
             min-width min-height
             get-parent)
    (define labels '(""))
    (define/public (set-lab _ls) 
      (unless (equal? labels _ls)
        (set! labels _ls)
        (update-size)
        (refresh)))
    (define/private (update-size)
      (define dc (get-dc))
      (send dc set-font small-control-font)
      (define-values (w h) 
        (for/fold ([w 0] [h 0])
                  ([lab (in-list labels)])
          (define-values (this-w this-h _1 _2) (send dc get-text-extent lab))
          (values (max this-w w)
                  (max this-h h))))
      (send (get-parent) begin-container-sequence)
      (min-width (+ 5 (inexact->exact (ceiling w))))
      (min-height (+ 5 (* (length labels) (inexact->exact (ceiling h)))))
      (send (get-parent) end-container-sequence)
      (send (get-parent) reflow-container))
    (define/override (on-paint)
      (define dc (get-dc))
      (send dc set-font small-control-font)
      (define-values (w h) (get-client-size))
      (define-values (tw th _1 _2) (send dc get-text-extent (car labels)))
      (send dc set-pen "black" 1 'transparent)
      (send dc set-brush "LemonChiffon" 'solid)
      (send dc set-pen "black" 1 'solid)
      (send dc draw-rectangle 0 0 w h)
      (for ([label (in-list labels)]
            [i (in-naturals)])
        (send dc draw-text label 2 (+ 2 (* i th)))))
    (super-new [stretchable-width #f] [stretchable-height #f])))
