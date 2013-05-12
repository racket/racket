#lang racket/gui
(require framework 
         pict
         "display.rkt" 
         "constants.rkt" 
         "pict-canvas.rkt") 
(provide label 
         mt-label 
         bold-label 
         mt-bold-label 
         section-header
         (struct-out event-target) 
         make-listener-table 
         add-receiver 
         post-event)

(define bold-system-font
  (send the-font-list find-or-create-font
        (send normal-control-font get-point-size)
        (send normal-control-font get-family)
        (send normal-control-font get-style)
        'bold))

(define (label p str)
  (new message% [parent p]
       [label str]
       [stretchable-width #t]))

(define (mt-label p) 
  (label p ""))

(define (bold-label p str) 
  (new message% [parent p] 
       [label str] 
       [font bold-system-font]
       [stretchable-width #t])) 

(define (mt-bold-label p) 
  (bold-label p ""))

(define (section-header par name orientation) 
  (let* ([text-pict (colorize (text name) (header-forecolor))]
         [text-container (pin-over (colorize (rectangle (+ 10 (pict-width text-pict)) 
                                                        (+ 10 (pict-height text-pict))) 
                                             (header-backcolor))
                                   5 
                                   5 
                                   text-pict)]
         [c (case orientation 
             [(horizontal) 
              (let ([canv (new pict-canvas% 
                               [parent par] 
                               [redraw-on-resize #t] 
                               [pict-builder (λ (vregion)
                                               (lc-superimpose (colorize (filled-rectangle (viewable-region-width vregion) 
                                                                                           HEADER-HEIGHT) 
                                                                         (header-backcolor))
                                                               text-container))] 
                               [min-height HEADER-HEIGHT] 
                               [stretchable-width #t] 
                               [stretchable-height #f])]) 
                canv)]
             [(vertical) 
              (let ([canv (new pict-canvas% 
                               [parent par] 
                               [redraw-on-resize #t] 
                               [pict-builder (λ (vregion)
                                               (rotate (lc-superimpose (colorize (filled-rectangle (viewable-region-height vregion) 
                                                                                                 HEADER-HEIGHT) 
                                                                               (header-backcolor))
                                                                     text-container)
                                                     -1.57079633))] 
                               [min-width HEADER-HEIGHT] 
                               [stretchable-width #f] 
                               [stretchable-height #t])]) 
                canv)])]) 
    c))

;Events
;receiver : any
;handler : (any -> void)
(struct event-target (receiver handler) #:transparent)

(define (make-listener-table) (make-hash))

(define (add-receiver table evt-name object handler)
  (hash-update! table
                evt-name
                (λ (old)
                  (cons (event-target object handler) old))
                (list (event-target object handler))))

(define (post-event table name sender arg)
    (let ([targets (hash-ref table name)])
      (for ([target (in-list targets)])
        (let ([receiver (event-target-receiver target)]
              [handler (event-target-handler target)])
          (unless (eq? receiver sender)
            (handler arg))))))
