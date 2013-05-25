#lang racket/base 
(provide get-event-color 
         get-event-forecolor
         get-event-opacity
         header-forecolor 
         header-backcolor 
         timeline-event-baseline-color 
         event-connection-line-color 
         event-target-future-line-color
         timeline-tick-color 
         timeline-tick-bold-color 
         timeline-tick-label-backcolor 
         timeline-tick-label-forecolor
         timeline-baseline-color 
         timeline-frame-color 
         timeline-frame-bg-color
         timeline-event-strokecolor
         hover-tickline-color 
         create-graph-node-backcolor
         create-graph-node-strokecolor 
         create-graph-node-forecolor 
         create-graph-edge-color 
         create-graph-block-node-forecolor 
         create-graph-sync-node-forecolor
         get-time-string
         (struct-out viewable-region) 
         viewable-region-x-extent 
         viewable-region-y-extent
         in-viewable-region 
         in-viewable-region-horiz 
         in-viewable-region-vert?
         scale-viewable-region
         between)

(struct viewable-region (x y width height) #:transparent)

;;viewable-region-x-extent : viewable-region -> uint
(define (viewable-region-x-extent vregion) 
  (+ (viewable-region-x vregion) (viewable-region-width vregion)))

;;viewable-region-y-extent : viewable-region -> uint
(define (viewable-region-y-extent vregion) 
  (+ (viewable-region-y vregion) (viewable-region-height vregion)))

(define (scale-viewable-region vreg factor)
  (define (scale n) (* n factor))
  (struct-copy viewable-region vreg
               [width (scale (viewable-region-width vreg))]
               [height (scale (viewable-region-height vreg))]))
               

;;between : uint uint uint -> bool
(define (between x start end) 
  (and (>= x start) (<= x end)))

;;in-viewable-region : viewable-region uint -> bool
(define (in-viewable-region-horiz vregion x) 
  (between x (viewable-region-x vregion) (viewable-region-x-extent vregion)))

;;in-viewable-region-vert : viewable-region uint -> bool
(define (in-viewable-region-vert? vregion y) 
  (between y (viewable-region-y vregion) (viewable-region-y-extent vregion)))

;;in-viewable-region : viewable-region segment -> bool
(define (in-viewable-region vregion x y w h)
  (define-values (start-x start-y end-x end-y) 
    (values (viewable-region-x vregion) 
            (viewable-region-y vregion) 
            (viewable-region-x-extent vregion) 
            (viewable-region-y-extent vregion))) 
  (define-values (x-end y-end) 
    (values (+ x w) 
            (+ y h))) 
  (and (or (between x start-x end-x) 
           (between x-end start-x end-x)
           (between start-x x x-end)
           (between end-y y y-end))
       (or (between y start-y end-y) 
           (between y-end start-y end-y)
           (between start-y y y-end)
           (between end-y y y-end))))

;;get-event-color : symbol -> string
(define (get-event-color type) 
  (case type 
    [(create) "blue"] 
    [(start-work start-0-work touch-resume) "green"] 
    [(block touch) "red"] 
    [(sync) "orange"] 
    [(touch-pause) "blue"] 
    [(result abort suspend) "white"] 
    [(complete end-work) "white"] 
    [(gc) "maroon"]
    [else "black"])) 

;;get-event-opacity : symbol -> real [0 .. 1]
(define (get-event-opacity type) 
  (case type 
    [(gc) 0.15] 
    [else 1]))

;;get-event-forecolor : symbol -> string
(define (get-event-forecolor type) 
  (case type 
    [(block) "white"] 
    [else "black"]))

(define (header-forecolor) "white") 
(define (header-backcolor) "slategray")
(define (timeline-event-baseline-color) "gray")
(define (event-connection-line-color) "orchid")
(define (event-target-future-line-color) "blue")
(define (creation-line-color) "green") 
(define (touch-line-color) "red")
(define (timeline-tick-color) "gray")
(define (timeline-tick-bold-color) "darkgray")
(define (timeline-tick-label-backcolor) "darkgray") 
(define (timeline-tick-label-forecolor) "white")
(define (timeline-baseline-color) "darkgray")
(define (timeline-frame-color) "gray")
(define (timeline-frame-bg-color) "white")
(define (timeline-event-strokecolor) "darkgray")
(define (hover-tickline-color) "darkgray")
(define (create-graph-node-forecolor) "white")
(define (create-graph-node-backcolor) "steelblue")
(define (create-graph-node-strokecolor) "darkgray")
(define (create-graph-edge-color) "black")
(define (create-graph-block-node-forecolor) "white")
(define (create-graph-sync-node-forecolor) "white")

(define (get-time-string time) 
  (if (or (= 0.0 time) (> time 0.1))
      (format "~a ms" time)
      (format "~a μs" (* 1000 time))))
   
   
   
   
   
