#lang racket/base
(require racket/list 
         racket/class 
         racket/draw
         slideshow/pict
         data/interval-map
         "visualizer-data.rkt" 
         "graph-drawing.rkt" 
         "drawing-helpers.rkt"
         "display.rkt" 
         "constants.rkt")

(provide seg-in-vregion 
         calc-segments 
         calc-ticks
         calc-row-mid-y 
         find-seg-for-coords 
         segment-edge
         segs-equal-or-later
         build-timeline-pict
         build-timeline-bmp-from-log
         build-timeline-pict-from-log 
         build-timeline-overlay 
         build-timeline-with-overlay 
         build-timeline-bmp-with-overlay 
         build-creategraph-pict 
         graph-overlay-pict
         (struct-out segment) 
         (struct-out frame-info) 
         (struct-out timeline-tick) 
         find-node-for-coords
         find-fid-for-coords
         first-seg-for-fid)

;Represents a dot or square on the timeline
(struct segment (event
                 x 
                 y 
                 width 
                 height 
                 color 
                 p 
                 prev-future-seg
                 next-future-seg 
                 prev-proc-seg
                 next-proc-seg  
                 prev-targ-future-seg
                 next-targ-future-seg) #:transparent #:mutable)


;General information about the timeline image
(struct frame-info (adjusted-width 
                    adjusted-height
                    row-height 
                    modifier 
                    timeline-ticks
                    process-line-coords) #:transparent)

;Represents a vertical line depicting a specific time in the execution history
(struct timeline-tick (x 
                       abs-time
                       rel-time) #:transparent) 

;;viewable-region-from-frame : frame-info -> viewable-region
(define (viewable-region-from-frame finfo) 
  (viewable-region 0 
                   0 
                   (frame-info-adjusted-width finfo) 
                   (frame-info-adjusted-height finfo)))

;;seg-in-vregion : viewable-region segment -> bool
(define (seg-in-vregion vregion)
  (λ (seg) 
    (in-viewable-region vregion 
                        (segment-x seg) 
                        (segment-y seg) 
                        (segment-width seg) 
                        (segment-height seg))))

;;calc-seg-x : event process-timeline trace uint float -> uint
(define (calc-seg-x evt tr modifier) 
  (floor (* (relative-time tr (event-start-time evt)) 
            modifier)))

;;calc-seg-width : float event -> uint
(define (calc-seg-width modifier evt)
  (case (event-type evt) 
    [(start-work start-0-work) (max MIN-SEG-WIDTH (* modifier (- (event-end-time evt) 
                                                                 (event-start-time evt))))] 
    [else MIN-SEG-WIDTH]))

;Finds the segment for given x and y mouse coordinates
;;find-seg-for-coords : uint uint interval-map -> segment
(define (find-seg-for-coords x y index) 
  (let ([xmap (interval-map-ref index y #f)]) 
    (if xmap 
        (interval-map-ref xmap x #f) 
        #f)))

;;find-fid-for-coords : uint uint (listof drawable-node) -> drawable-node
(define (find-node-for-coords x y nodes) 
  (define node-l (filter (λ (n)
                               (define n-x (drawable-node-x n))
                               (define n-y (drawable-node-y n))
                               (define n-w (drawable-node-width n))
                               (and (n-x . < . x)
                                    (n-y . < . y)
                                    (x . < . (+ n-x n-w))
                                    (y . < . (+ n-y n-w))))
                          (remove-duplicates (flatten nodes))))
  (cond 
    [(empty? node-l) 
     #f] 
    [(= 1 (length node-l)) 
     (car node-l)] 
    [else 
     (error 'find-node-for-coords "Multiple nodes found for coords: ~s ~s, ~s" x y node-l)]))

;;find-fid-for-coords : x y ??(listof (listof nodes)) by depth?? viewable-region -> fid
(define (find-fid-for-coords x y nodes vregion)
  (define n (find-node-for-coords x y nodes)) 
  (if n 
      (event-user-data (node-data (drawable-node-node n))) 
      #f))

;;first-seg-for-fid : future-id (listof segments) -> segment
(define (first-seg-for-fid fid segs)
  (first
   (sort
    (filter (λ (s) (define seg-fid (event-future-id (segment-event s)))
              (and seg-fid fid (= fid seg-fid))) segs)
    < #:key (λ (s) (event-start-time (segment-event s))))))

;;calc-adjusted-width : uint trace -> uint
(define (calc-adjusted-width w tr) 
  (define baseModifier (/ w (- (trace-end-time tr) (trace-start-time tr)))) 
  (define max-x-extent (for*/fold ([x 0]) ([tl (in-list (trace-proc-timelines tr))] 
                                           [evt (in-list (process-timeline-events tl))]) 
                         (max (+ (calc-seg-x evt tr baseModifier) 
                                 (calc-seg-width baseModifier evt))  
                              x))) 
  (- (- w (- max-x-extent w)) MIN-SEG-WIDTH))

;;calc-row-mid-y : uint uint -> uint
(define (calc-row-mid-y proc-index row-height) 
  (floor (- (+ (* proc-index 
                  row-height) 
               (/ row-height 2)) 
            2)))

;Gets the center of a circle with (xleft, ytop) as the top-left coordinate.
;;calc-center : uint uint uint -> (values uint uint)
(define (calc-center xleft ytop diameter) 
  (let ([rad (floor (/ diameter 2))]) 
    (values (+ xleft rad) 
            (+ ytop rad))))

;;segs-equal-or-after : float (listof segment) -> (listof segment)
(define (segs-equal-or-later real-time segs)
  (let loop ([sgs segs]) 
    (cond 
      [(null? sgs) '()] 
      [(>= (event-start-time (segment-event (car sgs))) real-time) sgs] 
      [else (loop (cdr sgs))])))  

;;segment-edge : segment -> uint
(define (segment-edge seg) 
  (define evt (segment-event seg)) 
  (if (event-has-duration? evt) 
      (segment-x seg)
      (+ (segment-x seg) (segment-width seg))))

;;calc-ticks : (listof segment) float trace -> (listof timeline-tick)
(define (calc-ticks segs timeToPixMod tr)
  (define trace-start (inexact->exact (trace-start-time tr)))
  (define segs-len (length segs)) 
  (define-values (lt lx tks) 
    (for/fold ([last-time trace-start] 
               [last-x 0]
               [ticks '()]) ([i (in-range 0 (floor (/ (- (trace-end-time tr) 
                                                         trace-start) 
                                                      DEFAULT-TIME-INTERVAL)))]) 
      (define tick-time (+ last-time DEFAULT-TIME-INTERVAL)) 
      (define tick-rel-time (* (add1 i) DEFAULT-TIME-INTERVAL)) 
      (define want-x (+ last-x (* DEFAULT-TIME-INTERVAL timeToPixMod)))
      (define next-seg (findf (λ (s) (> (event-start-time (segment-event s)) tick-time)) segs))
      (define most-recent-seg (list-ref segs (sub1 (event-index (segment-event next-seg)))))
      (define most-recent-evt (segment-event most-recent-seg))
      (define most-recent-time (inexact->exact (event-start-time most-recent-evt)))
      (define next-evt (segment-event next-seg))
      (define next-evt-time (inexact->exact (event-start-time next-evt)))
      (define most-recent-edge (segment-edge most-recent-seg)) 
      (define next-edge (segment-x next-seg))
      (cond 
        [(= most-recent-time tick-time) 
         (values tick-time 
                 (segment-x most-recent-seg) 
                 (cons (timeline-tick (segment-x most-recent-seg) tick-time tick-rel-time) ticks))]
        [(= (segment-x next-seg) (add1 (+ (segment-x most-recent-seg) (segment-width most-recent-seg))))
         (values tick-time 
                 (+ (segment-x most-recent-seg) (segment-width most-recent-seg)) 
                 (cons (timeline-tick (+ (segment-x most-recent-seg) 
                                         (segment-width most-recent-seg))
                                      tick-time
                                      tick-rel-time) 
                       ticks))]
        [else 
         (define start-x (max most-recent-edge last-x))
         (define start-time (max most-recent-time last-time))
         (define size-mod (/ (- next-edge start-x) (- next-evt-time start-time)))
         (define x-offset (ceiling (* (- tick-time start-time) size-mod)))
         (define tick-x (round (+ start-x x-offset)))
         (values tick-time 
                 tick-x 
                 (cons (timeline-tick tick-x tick-time tick-rel-time) ticks))]))) 
  tks)

;;calc-process-timespan-lines : trace (listof segment) -> (listof (uint . uint))
(define (calc-process-timespan-lines trace segs) 
  (for/list ([tl (in-list (trace-proc-timelines trace))]) 
    (let ([segs (filter (λ (s) (= (process-timeline-proc-id tl) 
                                  (event-proc-id (segment-event s)))) 
                        segs)]) 
      (cons (segment-x (car segs)) 
            (segment-x (last segs))))))

;;get-first-future-seg : seg -> seg
(define (get-first-future-seg seg) 
  (let loop ([cur seg]) 
    (if (segment-prev-future-seg cur) 
        (loop (segment-prev-future-seg cur)) 
        cur)))

;;get-first-future-seg-in-region : viewable-region segment -> segment
(define (get-first-future-seg-in-region vregion seg) 
  (let loop ([cur seg]) 
    (let ([prev (segment-prev-future-seg cur)]) 
      (if (not prev) 
          cur 
          (if (not ((seg-in-vregion vregion) prev)) 
              cur 
              (loop prev))))))
  
;;adjust-work-segs! : (listof segment) -> void
(define (adjust-work-segs! segs) 
  (for ([seg (in-list segs)]) 
    (case (event-type (segment-event seg)) 
      [(start-work start-0-work) 
       (set-segment-width! seg (max MIN-SEG-WIDTH 
                                    (- (segment-x (segment-next-proc-seg seg)) (segment-x seg))))] 
      [else 
       void])))

;;connect-segments! : (listof segment) -> void
(define (connect-segments! segs) 
  (for ([s (in-list segs)]) 
    (let ([evt (segment-event s)])
      (set-segment-prev-proc-seg! s (if (event-prev-proc-event evt) 
                                        (event-segment (event-prev-proc-event evt)) 
                                        #f)) 
      (set-segment-next-proc-seg! s (if (event-next-proc-event evt) 
                                        (event-segment (event-next-proc-event evt))
                                        #f)) 
      (set-segment-prev-future-seg! s (if (event-prev-future-event evt) 
                                          (event-segment (event-prev-future-event evt)) 
                                          #f))
      (set-segment-next-future-seg! s (if (event-next-future-event evt) 
                                          (event-segment (event-next-future-event evt)) 
                                          #f)) 
      (set-segment-prev-targ-future-seg! s (if (event-prev-targ-future-event evt) 
                                               (event-segment (event-prev-targ-future-event evt)) 
                                               #f)) 
      (set-segment-next-targ-future-seg! s (if (event-next-targ-future-event evt) 
                                               (event-segment (event-next-targ-future-event evt)) 
                                               #f)))))

(struct acc (delta last-right-edge) #:transparent)

;;build-seg-layout : flonum (listof event) trace -> (values (listof segment) uint uint)
(define (build-seg-layout timeToPixModifier events tr)
  (define last-right-edges (build-vector (length (trace-proc-timelines tr)) (λ (n) 0)))
  (define-values (sgs d x-extent)
    (for/fold ([segs '()]
               [delta 0]
               [largest-x 0]) ([evt (in-list events)])
      (define last-right-edge (vector-ref last-right-edges (event-proc-index evt)))
      (define wanted-offset (+ delta (* DEFAULT-TIMELINE-WIDTH
                                        (inexact->exact 
                                         (/ (- (event-start-time evt) (trace-start-time tr))
                                            (- (trace-end-time tr) (trace-start-time tr)))))))
      (define-values (offset new-delta) 
        (if (last-right-edge . <= . wanted-offset) 
            (values wanted-offset delta)
            (values last-right-edge (+ delta (- last-right-edge wanted-offset)))))
      (define radius (/ MIN-SEG-WIDTH 2))
      (define segw MIN-SEG-WIDTH)
      #;(define segw (case (event-type evt) 
                     [(start-work start-0-work) 
                      (max MIN-SEG-WIDTH 
                           (inexact->exact 
                            (round (* timeToPixModifier 
                                      (- (event-end-time evt) (event-start-time evt))))))] 
                     [else MIN-SEG-WIDTH]))
      (define seg (segment evt 
                           (round offset) 
                           (- (calc-row-mid-y (event-proc-index evt) TIMELINE-ROW-HEIGHT) radius)
                           segw
                           MIN-SEG-WIDTH 
                           (get-event-color (event-type evt)) 
                           #f 
                           #f 
                           #f 
                           #f 
                           #f 
                           #f 
                           #f)) 
      (set-event-segment! evt seg)
      (vector-set! last-right-edges (event-proc-index evt) (+ offset segw))
      (values (cons seg segs)
              new-delta
              (max largest-x last-right-edge))))
  (values sgs x-extent))

;;calc-segments : trace uint uint -> (values frame-info (listof segment))
(define (calc-segments tr) 
  (define evts (trace-all-events tr))
  (define timeToPixModifier (/ DEFAULT-TIMELINE-WIDTH (- (trace-end-time tr) (trace-start-time tr))))
  (define-values (segments x) 
    (build-seg-layout timeToPixModifier evts tr))
  (define ordered-segs (reverse segments))
  (connect-segments! ordered-segs)
  (adjust-work-segs! ordered-segs)
  (define ticks (calc-ticks ordered-segs timeToPixModifier tr))
  (values (frame-info (+ MIN-SEG-WIDTH (round x)) 
                      (* TIMELINE-ROW-HEIGHT (length (trace-proc-timelines tr)))
                      TIMELINE-ROW-HEIGHT 
                      timeToPixModifier 
                      ticks
                      (calc-process-timespan-lines tr ordered-segs)) 
          ordered-segs))

;;pict-for-segment : segment -> pict
(define (pict-for-segment seg) 
  (when (not (segment-p seg))
    (set-segment-p! seg (if (event-has-duration? (segment-event seg)) 
                            (rect-pict (segment-color seg)
                                       (timeline-event-strokecolor) 
                                       (segment-width seg) 
                                       MIN-SEG-WIDTH 
                                       #:stroke-width .5) 
                            (circle-pict (segment-color seg) 
                                         (timeline-event-strokecolor) 
                                         MIN-SEG-WIDTH  
                                         #:stroke-width .5)))) 
  (segment-p seg))

;;draw-ruler-on : pict viewable-region frameinfo -> pict
(define (draw-ruler-on base vregion frameinfo)
  (let loop ([pct base] 
             [ticks (filter (λ (t) (in-viewable-region-horiz vregion (timeline-tick-x t)))
                            (frame-info-timeline-ticks frameinfo))]
             [next-label-x (viewable-region-x-extent vregion)] 
             [next-tick-x (viewable-region-x-extent vregion)])
    (cond 
      [(null? ticks) pct] 
      [(< (- next-tick-x (timeline-tick-x (car ticks))) TIMELINE-MIN-TICK-PADDING) 
       (loop pct 
             (cdr ticks) 
             next-label-x 
             next-tick-x)]
      [else (let* ([LABEL-PAD 2] 
                   [VERT-PAD 3]
                   [cur-tick (car ticks)] 
                   [cur-x (timeline-tick-x cur-tick)]
                   [tick-desc (format "~a ms" (real->decimal-string 
                                               (timeline-tick-rel-time cur-tick) 1))]
                   [t (text-block-pict tick-desc 
                                       #:backcolor (timeline-tick-label-backcolor) 
                                       #:forecolor (timeline-tick-label-forecolor)
                                       #:padding 3)] 
                   [text-width (pict-width t)] 
                   [show-label? (<= (+ cur-x LABEL-PAD text-width) next-label-x)]
                   [pinnedline (pin-over pct 
                                         (- cur-x (viewable-region-x vregion))
                                         0 
                                         (linestyle 'dot 
                                                    (colorize (vline 1 
                                                                     (frame-info-adjusted-height frameinfo)) 
                                                              (if show-label? 
                                                                  (timeline-tick-bold-color)
                                                                  (timeline-tick-color)))))])
              (if show-label? 
                  (loop (pin-over pinnedline 
                                  (- cur-x (viewable-region-x vregion)) 
                                  VERT-PAD 
                                  t) 
                        (cdr ticks) 
                        cur-x 
                        cur-x)
                  (loop pinnedline
                        (cdr ticks) 
                        next-label-x 
                        cur-x)))])))

;;draw-row-lines-on : pict viewable-region trace frameinfo -> pict
(define (draw-row-lines-on base vregion tr finfo opacity) 
  (pin-over base 
            0 
            0
            (for/fold ([pct base]) ([tl (in-list (trace-proc-timelines tr))] 
                                    [i (in-naturals)])
              (let* ([line-coords (list-ref (frame-info-process-line-coords finfo) 
                                           (process-timeline-proc-index tl))]
                     [line-start (car line-coords)] 
                     [line-end (cdr line-coords)]
                     [vregion-start (viewable-region-x vregion)] 
                     [vregion-end (viewable-region-x-extent vregion)]
                     [start-x (cond 
                                [(< line-start vregion-start) 0] 
                                [(between line-start vregion-start vregion-end) 
                                 (- line-start vregion-start)] 
                                [else vregion-end])]
                     [end-x (cond 
                              [(< line-end vregion-start) 0] 
                              [(between line-end vregion-start vregion-end) 
                               (- line-end vregion-start)] 
                              [else vregion-end])]
                     [proc-name (if (zero? i) 
                                    "Thread 0 (Runtime Thread)" 
                                    (format "Thread ~a" (process-timeline-proc-id tl)))]
                     [proc-title (text-block-pict proc-name 
                                                  #:backcolor (header-backcolor) 
                                                  #:forecolor (header-forecolor) 
                                                  #:padding HEADER-PADDING
                                                  #:opacity opacity 
                                                  #:width (viewable-region-width vregion))]) 
                (draw-stack-onto pct 
                                 (at 0 
                                     (- (* (add1 i) (frame-info-row-height finfo)) (viewable-region-y vregion)) 
                                     (colorize (hline (viewable-region-width vregion) 1) (timeline-baseline-color))) 
                                 (at 0  
                                     (+ (+ (- (* i (frame-info-row-height finfo)) (viewable-region-y vregion)) 
                                           (- (frame-info-row-height finfo) (pict-height proc-title))) 
                                        1) 
                                     proc-title) 
                                 (at start-x 
                                     (- (calc-row-mid-y (process-timeline-proc-index tl) (frame-info-row-height finfo))
                                        (viewable-region-y vregion))
                                     (colorize (hline (- end-x start-x) 1) 
                                               (timeline-event-baseline-color))))))))

;Magnifies a segment's pict (dot or square) to make 
;it stand out when hovered over with the mouse pointer.
;;make-stand-out-pict : segment -> pict
(define (make-stand-out-pict seg) 
  (case (event-type (segment-event seg)) 
    [(start-work start-0-work) (scale (segment-p seg) 1 2)] 
    [else (scale (segment-p seg) 2)]))

;;frame-bg : viewable-region frame-info trace -> pict
(define (frame-bg vregion finfo tr)
  (draw-frame-bg-onto (colorize (filled-rectangle (viewable-region-width vregion) 
                                                  (frame-info-adjusted-height finfo)) 
                                (timeline-frame-bg-color))
                      vregion                                
                      finfo 
                      tr 
                      TIMELINE-HEADER-OPACITY))

;;draw-frame-bg-onto : pict viewable-region frameinfo trace -> pict
(define (draw-frame-bg-onto base vregion finfo tr opacity) 
  (let ([with-ruler (draw-ruler-on base vregion finfo)])
    (draw-row-lines-on with-ruler vregion tr finfo opacity)))

;;draw-connection : viewable-region segment segment pict string [uint bool symbol] -> pict
(define (draw-connection vregion 
                         start 
                         end 
                         base-pct 
                         color 
                         #:width [width 1] 
                         #:with-arrow [with-arrow #f] 
                         #:style [style 'solid]) 
  (let*-values ([(midx midy) (calc-center (- (segment-x start) (viewable-region-x vregion)) 
                                          (- (segment-y start) (viewable-region-y vregion)) 
                                          MIN-SEG-WIDTH)] 
                [(nextx nexty) (calc-center (- (segment-x end) (viewable-region-x vregion)) 
                                            (- (segment-y end) (viewable-region-y vregion)) 
                                            MIN-SEG-WIDTH)] 
                [(dx dy) (values (- nextx midx) (- nexty midy))]) 
    (if (and (zero? dy) 
             (or (not (eq? (segment-next-proc-seg start) end)) 
                 (< dx CONNECTION-LINE-HAT-THRESHOLD))) 
        (let* ([dxa (/ dx 2)]  
               [dya (- HAT-HEIGHT CONNECTION-LINE-HAT-THRESHOLD)] 
               [breakx (+ midx dxa)] 
               [breaky (+ midy dya)])
          (draw-line-onto (draw-line-onto base-pct 
                                          midx 
                                          midy 
                                          breakx 
                                          breaky 
                                          color 
                                          #:width width
                                          #:style style) 
                          breakx 
                          breaky 
                          nextx 
                          nexty 
                          color 
                          #:width width 
                          #:with-arrow with-arrow 
                          #:style style))
        (draw-line-onto base-pct 
                        midx 
                        midy 
                        nextx 
                        nexty 
                        color 
                        #:width width 
                        #:with-arrow with-arrow 
                        #:style style))))
        
;;draw-arrows : pict viewable-region segment -> pict
(define (draw-arrows base-pct vregion seg) 
  (let ([fst (get-first-future-seg-in-region vregion seg)]) 
    (let loop ([pct base-pct] 
               [cur-seg fst])
        (if (not cur-seg) 
            pct 
            (let ([next (segment-next-future-seg cur-seg)])
              (let* ([next-targ (segment-next-targ-future-seg cur-seg)] 
                     [prev-targ (segment-prev-targ-future-seg cur-seg)]              
                     [ftl-arrows (if (not next) 
                                     pct 
                                     (draw-connection vregion 
                                                      cur-seg 
                                                      next 
                                                      pct 
                                                      (event-connection-line-color) 
                                                      #:width 2))] 
                     [prev-targ-arr (if (not prev-targ) 
                                        ftl-arrows 
                                        (draw-connection vregion 
                                                         prev-targ 
                                                         cur-seg 
                                                         ftl-arrows 
                                                         (event-target-future-line-color) 
                                                         #:with-arrow #t 
                                                         #:style 'dot))] 
                     [next-targ-arr (if (not next-targ) 
                                        prev-targ-arr 
                                        (draw-connection vregion 
                                                         cur-seg 
                                                         next-targ 
                                                         prev-targ-arr 
                                                         (event-target-future-line-color) 
                                                         #:with-arrow #t 
                                                         #:style 'dot))])
                (loop next-targ-arr 
                      next)))))))  

;;timeline-bmp-from-log : (listof indexed-fevent) (or uint bool) (or uint bool) -> bitmap%
(define (build-timeline-bmp-from-log logs 
                                     #:max-width [max-width #f] 
                                     #:max-height [max-height #f]) 
  (define vregion (if (or (not max-width) (not max-height)) 
                      #f 
                      (viewable-region 0 
                                       0 
                                       max-width 
                                       max-height)))
  
  (define p (build-timeline-pict-from-log logs vregion)) 
  (pict->bitmap p))

(define (truncate-bmp bmp width height)
  (define w (min width (send bmp get-width))) 
  (define h (min height (send bmp get-height)))
  (let ([buf (make-bytes (* width height 4))]) 
    (send bmp 
          get-argb-pixels 
          0 
          0 
          w 
          h 
          buf) 
    (let ([new-b (make-bitmap w h)]) 
      (send new-b 
            set-argb-pixels 
            0 
            0 
            w 
            h 
            buf) 
      new-b)))

;;build-timeline-bmp-with-overlay : (listof indexed-fevent) uint [uint] [uint] -> bitmap%
(define (build-timeline-bmp-with-overlay logs 
                                         event-index 
                                         #:max-width [max-width #f] 
                                         #:max-height [max-height #f]) 
  (define p (build-timeline-with-overlay logs event-index)) 
  (define-values (w h) 
    (values (if max-width (min max-width (pict-width p)) (pict-width p)) 
            (if max-height (min max-height (pict-height p)) (pict-height p))))
  (truncate-bmp (pict->bitmap p) w h))
  

;;build-timeline-pict-from-trace : trace viewable-region -> pict
(define (build-timeline-pict-from-trace tr vregion) 
  (define-values (finfo segments) (calc-segments tr))
  (build-timeline-pict vregion 
                       tr 
                       finfo 
                       segments))

;;build-timeline-pict : (or viewable-region #f) trace frame-info (listof segment) -> pict
(define (build-timeline-pict vregion tr finfo segments) 
  (define vr (if (not vregion) 
                 (viewable-region 0 
                                  0 
                                  (frame-info-adjusted-width finfo) 
                                  (frame-info-adjusted-height finfo)) 
                 vregion))
  (for/fold ([pct (frame-bg vr finfo tr)]) 
    ([seg (in-list (filter (seg-in-vregion vr) segments))])                 
    (pin-over pct
              (- (segment-x seg) (viewable-region-x vr)) 
              (- (segment-y seg) (viewable-region-y vr)) 
              (pict-for-segment seg))))

;;build-timeline-pict-from-log : (listof indexed-fevent) viewable-region -> pict
(define (build-timeline-pict-from-log logs vregion)
  (build-timeline-pict-from-trace (build-trace logs) vregion)) 

;;build-timeline-with-overlay : (listof indexed-fevent) uint -> pict
(define (build-timeline-with-overlay logs event-index)
  (define tr (build-trace logs)) 
  (define-values (finfo segments) (calc-segments tr)) 
  (define vregion (viewable-region-from-frame finfo))
  (define timeline-p (build-timeline-pict vregion
                                          tr 
                                          finfo 
                                          segments))
  (define overlay (build-timeline-overlay vregion
                                          #f 
                                          (list-ref segments event-index) 
                                          finfo 
                                          tr)) 
  (pin-over timeline-p 
            0 
            0 
            overlay))

;Draws the pict that is layered on top of the exec. timeline canvas 
;to highlight a specific future's event sequence
;;build-timeline-overlay : uint uint (or segment #f) (or segment #f) frame-info trace -> pict
(define (build-timeline-overlay vregion tacked hovered finfo tr)
  (define-values (width height) (values (viewable-region-width vregion) 
                                        (viewable-region-height vregion)))
  (define base (blank (viewable-region-width vregion) 
                      (viewable-region-height vregion))) 
  (define-values (seg-with-arrows showing-tacked) 
    (if tacked (values tacked #t) (values hovered #f)))
  (if (and seg-with-arrows (segment-p seg-with-arrows)) 
      (let* ([bg base] 
             [dots (let loop ([p bg] [cur-seg (get-first-future-seg-in-region vregion seg-with-arrows)]) 
                     (if (or (not cur-seg) (not ((seg-in-vregion vregion) cur-seg))) 
                         p 
                         (loop (pin-over p 
                                         (- (segment-x cur-seg) (viewable-region-x vregion)) 
                                         (- (segment-y cur-seg) (viewable-region-y vregion)) 
                                         (pict-for-segment cur-seg)) 
                               (segment-next-future-seg cur-seg))))]
             [aseg-rel-x (- (segment-x seg-with-arrows) (viewable-region-x vregion))] 
             [aseg-rel-y (- (segment-y seg-with-arrows) (viewable-region-y vregion))]
             [line (pin-over dots
                             (- (+ aseg-rel-x 
                                   (/ (segment-width seg-with-arrows) 2)) 
                                2)
                             0
                             (colorize (vline 1 height) (hover-tickline-color)))]
             [bigger (make-stand-out-pict seg-with-arrows)]
             [width-dif (/ (- (pict-width bigger) (pict-width (segment-p seg-with-arrows))) 2)]
             [height-dif (/ (- (pict-height bigger) (pict-height (segment-p seg-with-arrows))) 2)]
             [magnified (pin-over line 
                                  (- aseg-rel-x width-dif)
                                  (- aseg-rel-y height-dif) 
                                  bigger)] 
             [hover-magnified (if (and showing-tacked 
                                       hovered 
                                       (segment-p hovered)
                                       (not (eq? hovered tacked)))
                                  (let* ([hmag (make-stand-out-pict hovered)] 
                                         [hwidth-dif (/ (- (pict-width hmag)
                                                           (pict-width (segment-p hovered)))
                                                        2)] 
                                         [hheight-dif (/ (- (pict-height hmag)
                                                            (pict-height (segment-p hovered)))
                                                         2)])
                                    (pin-over magnified 
                                              (- (- (segment-x hovered) (viewable-region-x vregion)) hwidth-dif) 
                                              (- (- (segment-y hovered) (viewable-region-y vregion)) hheight-dif)
                                              hmag)) 
                                  magnified)] 
             [arrows (draw-arrows hover-magnified vregion seg-with-arrows)])
        arrows)
      base))

;Draw a line from one node on the creation graph to another
;;line-from : drawable-node drawable-node pict viewable-region -> pict
(define (line-from start end pct vregion) 
  (let* ([par-center (drawable-node-center start)] 
         [child-center (drawable-node-center end)] 
         [minx (viewable-region-x vregion)] 
         [miny (viewable-region-y vregion)])
    (draw-line-onto pct 
                    (- (point-x par-center) minx)
                    (- (point-y par-center) miny)
                    (- (point-x child-center) minx)  
                    (- (point-y child-center) miny) 
                    (create-graph-edge-color) 
                    #:width 1
                    #:style 'dot)))

;Draws a circle for a node on the creation graph
;;node-pict : drawable-node -> pict
(define (node-pict dnode) 
  (let* ([ndata (node-data (drawable-node-node dnode))]
         [ntext (if (equal? ndata 'runtime-thread) 
                    "RTT" 
                    (format "~a" (event-user-data ndata)))]) 
    (cc-superimpose (circle-pict (create-graph-node-backcolor) 
                                 (create-graph-node-strokecolor) 
                                 (drawable-node-width dnode)) 
                    (colorize (text ntext) (create-graph-node-forecolor)))))

;Cache the creation graph pict after first drawing
(define cg-pict #f)

;;draw-creategraph-pict : viewable-region tree-layout -> pict
(define (draw-creategraph-pict vregion layout) 
  (define rt-root (first (graph-layout-nodes layout)))
  (define width (inexact->exact (floor (graph-layout-width layout))))
  (define height (inexact->exact (floor (graph-layout-height layout))))
  (define base (blank (viewable-region-width vregion) (viewable-region-height vregion)))
  (define viewable-nodes (filter (λ (n) (in-viewable-region vregion 
                                                            (drawable-node-x n) 
                                                            (drawable-node-y n) 
                                                            (drawable-node-width n) 
                                                            (drawable-node-width n))) 
                                 (graph-layout-nodes layout)))
  (let ([arrow-pct (for/fold ([pct base]) ([node (in-list (graph-layout-nodes layout))])
                     (for/fold ([p pct]) ([child (in-list (drawable-node-children node))]) 
                       (line-from node child p vregion)))])
    (for/fold ([pct arrow-pct]) ([node (in-list viewable-nodes)]) 
      (pin-over pct 
                (- (drawable-node-x node) (viewable-region-x vregion)) 
                (- (drawable-node-y node) (viewable-region-y vregion)) 
                (node-pict node)))))

;Draws the pict displayed in the creation graph canvas
;;creategraph-p : viewable-region tree-layout uint -> pict
(define (build-creategraph-pict vregion layout zoom-level)
  (define factor (zoom-level->factor zoom-level))
  (set! cg-pict (draw-creategraph-pict (scale-viewable-region vregion (/ 1 factor)) layout)) 
  (scale cg-pict factor))

(define (zoom-level->factor zoom-level)
   (+ 1.0 (* (- zoom-level CREATE-GRAPH-DEFAULT-ZOOM) 
                       CREATE-GRAPH-ZOOM-FACTOR)))

;;graph-overlay-pict : drawable-node trace graph-layout -> pict
(define (graph-overlay-pict hover-node tr layout vregion) 
  (when hover-node 
    (unless (equal? (node-data (drawable-node-node hover-node)) 'runtime-thread)
      (define fid (event-user-data (node-data (drawable-node-node hover-node))))
      (define ri (hash-ref (trace-future-rtcalls tr) fid (λ () #f)))
      (when ri
        (define block-ops (sort (hash-keys (rtcall-info-block-hash ri)) 
                                > 
                                #:key (λ (p) 
                                        (hash-ref (rtcall-info-block-hash ri) p))))
        (define sync-ops (sort (hash-keys (rtcall-info-sync-hash ri))
                               > 
                               #:key (λ (op) 
                                       (hash-ref (rtcall-info-sync-hash ri) op))))
        (define-values (node-origin-x node-origin-y) 
          (values (- (drawable-node-x hover-node) (viewable-region-x vregion)) 
                  (- (drawable-node-y hover-node) (viewable-region-y vregion))))
        (define-values (center-x center-y) 
          (values (+ node-origin-x (/ (drawable-node-width hover-node) 2)) 
                  (+ node-origin-y (/ (drawable-node-width hover-node) 2))))
        (define x (+ center-x CREATE-GRAPH-NODE-DIAMETER))
        (define-values (pct yacc) 
          (for/fold ([p (pin-over (blank (viewable-region-width vregion) (viewable-region-height vregion)) 
                                  node-origin-x 
                                  node-origin-y 
                                  (node-pict hover-node))] 
                     [yacc node-origin-y])
            ([rtcall (in-list (append (map (λ (op) (cons 'block op)) block-ops) 
                                      (map (λ (op) (cons 'sync op)) sync-ops)))]) 
            (define evt-type (car rtcall)) 
            (define prim (cdr rtcall))
            (define the-hash (if (equal? evt-type 'block) (rtcall-info-block-hash ri) (rtcall-info-sync-hash ri)))
            (define txtp (text-pict (format "~a (~a)" 
                                            (symbol->string prim) 
                                            (hash-ref the-hash prim)) 
                                    #:color (get-event-forecolor evt-type))) 
            (define txtbg (rect-pict (get-event-color evt-type)
                                     (create-graph-edge-color) 
                                     (+ (pict-width txtp) (* TOOLTIP-MARGIN 2)) 
                                     (+ (pict-height txtp) (* TOOLTIP-MARGIN 2)) 
                                     #:stroke-width .5))
            (values 
             (pin-over (draw-line-onto p 
                                       center-x
                                       center-y
                                       x 
                                       yacc 
                                       (create-graph-edge-color))
                       x 
                       yacc 
                       (pin-over txtbg 
                                 TOOLTIP-MARGIN 
                                 TOOLTIP-MARGIN 
                                 txtp)) 
             (+ yacc (pict-height txtbg) CREATE-GRAPH-PADDING))))
        pct))))
  