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

(provide timeline-pict
         timeline-pict-for-trace-data
         timeline-overlay
         seg-in-vregion
         calc-segments
         calc-ticks
         calc-row-mid-y
         find-seg-for-coords
         segment-edge
         segs-equal-or-later
         creation-tree-pict
         draw-creategraph-pict
         zoom-level->factor
         graph-overlay-pict
         (struct-out segment)
         (struct-out frame-info)
         (struct-out timeline-tick)
         find-node-for-coords
         find-fid-for-coords
         first-seg-for-fid
         print-seg)

;Represents a dot or square on the timeline
(struct segment (event
                 x
                 y
                 width
                 height
                 color
                 opacity
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
                       rel-time
                       show-label?) #:transparent)

;;viewable-region-from-frame : frame-info -> viewable-region
(define (viewable-region-from-frame finfo)
  (viewable-region 0
                   0
                   (frame-info-adjusted-width finfo)
                   (frame-info-adjusted-height finfo)))

;;print-seg : segment -> void
(define (print-seg seg)
  (printf "(segment type:~a x:~a y:~a width:~a height:~a color:~a\n"
          (event-type (segment-event seg))
          (segment-x seg)
          (segment-y seg)
          (segment-width seg)
          (segment-height seg)
          (segment-color seg)))

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

;;calc-row-mid-y : uint uint trace -> uint
(define (calc-row-mid-y proc-index row-height tr)
  (case proc-index 
    [(gc) 0]
    [else
     (define PADDING 2)
     (floor (- (+ (* proc-index row-height)
                  (/ row-height 2))
               PADDING))]))

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

(define (find-most-recent-and-next segs time)
  (let loop ([ss segs])
    (cond
      [(empty? (cddr ss))
       (values (first ss) (second ss) ss)]
      [(> (event-start-time (segment-event (second ss))) time)
       (values (first ss) (second ss) ss)]
      [else
       (loop (cdr ss))])))

;;timeline-tick-label-pict : real -> pict
(define (timeline-tick-label-pict rel-time)
  (text-block-pict (format "~a ms" (real->decimal-string rel-time))
                   #:backcolor (timeline-tick-label-backcolor)
                   #:forecolor (timeline-tick-label-forecolor)
                   #:padding 3))

;;calc-ticks : (listof segment) float trace -> (listof timeline-tick)
(define (calc-ticks segs timeToPixMod tr)
  (define LABEL-PAD 3)
  (define trace-start (inexact->exact (trace-start-time tr)))
  (define segs-len (length segs))
  (define-values (lt lx tks _ __)
    (for/fold ([last-time trace-start]
               [last-x 0]
               [ticks '()]
               [last-label-x-extent 0]
               [remain-segs segs]) ([i (in-range 0 (floor (/ (- (trace-end-time tr)
                                                         trace-start)
                                                      DEFAULT-TIME-INTERVAL)))])
      (define tick-rel-time (* (add1 i) DEFAULT-TIME-INTERVAL))
      (define tick-time (+ trace-start tick-rel-time))
      (define want-x (+ last-x (* DEFAULT-TIME-INTERVAL timeToPixMod)))
      (define-values (most-recent-seg next-seg r-segs)
        (find-most-recent-and-next remain-segs tick-time))
      (define most-recent-evt (segment-event most-recent-seg))
      (define most-recent-time (inexact->exact (event-start-time most-recent-evt)))
      (define next-evt (segment-event next-seg))
      (define next-evt-time (inexact->exact (event-start-time next-evt)))
      (define most-recent-edge (segment-edge most-recent-seg))
      (define next-edge (segment-x next-seg))
      (define tick-x
        (cond
          [(= most-recent-time tick-time) (segment-x most-recent-seg)]
          [(= (segment-x next-seg) (add1 (+ (segment-x most-recent-seg) (segment-width most-recent-seg))))
           (+ (segment-x most-recent-seg) (segment-width most-recent-seg))]
          [else
           (define start-x (max most-recent-edge last-x))
           (define start-time (max most-recent-time last-time))
           (define size-mod (/ (- next-edge start-x) (- next-evt-time start-time)))
           (define x-offset (ceiling (* (- tick-time start-time) size-mod)))
           (round (+ start-x x-offset))]))
      (define show-tick? ((- tick-x last-x) . >= . TIMELINE-MIN-TICK-PADDING))
      (define show-label?
        (if (not show-tick?)
            #f
            (>= tick-x (+ last-label-x-extent LABEL-PAD))))
      (define new-label-x-extent
        (if show-label?
            (+ tick-x (pict-width (timeline-tick-label-pict tick-rel-time)))
            last-label-x-extent))
      (if show-tick?
          (values tick-time
                  tick-x
                  (cons (timeline-tick tick-x tick-time tick-rel-time show-label?) ticks)
                  new-label-x-extent
                  r-segs)
          (values tick-time
                  last-x
                  ticks
                  new-label-x-extent
                  r-segs))))
  tks)

;;calc-process-timespan-lines : trace (listof segment) uint -> (listof (uint . uint))
(define (calc-process-timespan-lines trace segs max-x)
  (for/list ([tl (in-list (trace-proc-timelines trace))])
    (define sgs (filter (λ (s) (equal? (process-timeline-proc-id tl)
                                        (event-proc-id (segment-event s))))
                         segs))
    (cond
      [(empty? sgs) (cons 0 max-x)]
      [else
       (cons (segment-x (car sgs))
             (segment-x (last sgs)))])))

;;get-first-future-seg : seg -> seg
(define (get-first-future-seg seg)
  (let loop ([cur seg])
    (if (segment-prev-future-seg cur)
        (loop (segment-prev-future-seg cur))
        cur)))

;;get-first-future-seg-in-region : viewable-region segment -> segment
(define (get-first-future-seg-in-region vregion seg)
  (define prev-seg (get-seg-previous-to-vregion vregion seg))
    (if ((seg-in-vregion vregion) prev-seg)
        prev-seg
        (segment-next-future-seg prev-seg)))

;; get-seg-previous-to-vregion : viewable-region segment -> segment
(define (get-seg-previous-to-vregion vregion seg)
  (let loop ([cur seg])
    (define prev (segment-prev-future-seg cur))
    (cond
      [(or (not prev) (not ((seg-in-vregion vregion) cur))) cur]
      [else (loop prev)])))

;;Set pixel widths of segments with variable widths, e.g.
;;work and GC events
;;adjust-variable-width-segs! : (listof segment) uint -> void
(define (adjust-variable-width-segs! segs max-x)
  (cond
    [(empty? segs) void]
    [else
     (define cur (car segs))
     (case (event-type (segment-event cur))
       [(start-work start-0-work)
        (define next-seg (segment-next-proc-seg cur))
        ;Because we are truncating logs after they reach a certain size,
        ;next-seg could be #f (where before it was safe to assume that a work segment
        ;was always followed by another segment).
        (define x-end (if next-seg
                          (segment-x next-seg)
                          max-x))
        (set-segment-width! cur (max MIN-SEG-WIDTH
                                     (- x-end (segment-x cur))))
        (adjust-variable-width-segs! (cdr segs) max-x)]
       [(gc)
        (cond
          [(empty? (cdr segs)) void]
          [else
           (set-segment-width! cur (max MIN-SEG-WIDTH
                                        (- (segment-x (car (cdr segs))) (segment-x cur))))
           (adjust-variable-width-segs! (cdr segs) max-x)])]
       [else (adjust-variable-width-segs! (cdr segs) max-x)])]))

;;connect-segments! : (listof segment) -> void
(define (connect-segments! segs)
  (for ([s (in-list segs)])
    (define evt (segment-event s))
    (define (segment-of-evt getter)
      (define x (getter evt))
      (and x (event-segment x)))
    (set-segment-prev-proc-seg! s (segment-of-evt event-prev-proc-event))
    (set-segment-next-proc-seg! s (segment-of-evt event-next-proc-event))
    (set-segment-prev-future-seg! s (segment-of-evt event-prev-future-event))
    (set-segment-next-future-seg! s (segment-of-evt event-next-future-event))
    (set-segment-prev-targ-future-seg! s (segment-of-evt event-prev-targ-future-event))
    (set-segment-next-targ-future-seg! s (segment-of-evt event-next-targ-future-event))))

;;build-seg-layout : flonum (listof event) trace -> (values (listof segment) uint uint)
(define (build-seg-layout timeToPixModifier events tr max-y)
  (define num-tls (length (trace-proc-timelines tr)))
  (define last-right-edges (build-vector num-tls (λ (n) 0)))
  (define-values (sgs d x-extent)
    (for/fold ([segs '()]
               [delta 0]
               [largest-x 0]) ([evt (in-list events)])
      (define is-gc-evt? (equal? (event-type evt) 'gc))
      (define last-right-edge (if is-gc-evt?
                                  largest-x
                                  (vector-ref last-right-edges (event-proc-index evt))))
      (define wanted-offset (+ delta (* DEFAULT-TIMELINE-WIDTH
                                        (inexact->exact
                                         (/ (- (event-start-time evt) (trace-start-time tr))
                                            (- (trace-end-time tr) (trace-start-time tr)))))))
      (define-values (offset new-delta)
        (if (last-right-edge . <= . wanted-offset)
            (values wanted-offset delta)
            (values last-right-edge (+ delta (- last-right-edge wanted-offset)))))
      (define radius (if is-gc-evt? 0 (/ MIN-SEG-WIDTH 2)))
      (define segw MIN-SEG-WIDTH)
      (define segh (cond
                     [is-gc-evt? max-y]
                     [else MIN-SEG-WIDTH]))
      (define seg (segment evt
                           (round offset)
                           (- (calc-row-mid-y (event-proc-index evt) TIMELINE-ROW-HEIGHT tr) radius)
                           segw
                           segh
                           (get-event-color (event-type evt))
                           (get-event-opacity (event-type evt))
                           #f
                           #f
                           #f
                           #f
                           #f
                           #f
                           #f))
      (set-event-segment! evt seg)
      (when (not is-gc-evt?)
        (vector-set! last-right-edges (event-proc-index evt) (+ offset segw)))
      (values (cons seg segs)
              new-delta
              (max largest-x (+ offset segw) #;last-right-edge))))
  (values sgs x-extent))

;;calc-segments : trace uint uint -> (values frame-info (listof segment))
(define (calc-segments tr)
  (define evts (trace-all-events tr))
  (define timeToPixModifier (/ DEFAULT-TIMELINE-WIDTH (- (trace-end-time tr) (trace-start-time tr))))
  (define max-y (* TIMELINE-ROW-HEIGHT (length (trace-proc-timelines tr))))
  (define-values (segments x)
    (build-seg-layout timeToPixModifier evts tr max-y))
  (define max-x (+ MIN-SEG-WIDTH (round x)))
  (define ordered-segs (reverse segments))
  (connect-segments! ordered-segs)
  (adjust-variable-width-segs! ordered-segs max-x)
  (define ticks (calc-ticks ordered-segs timeToPixModifier tr))
  (values (frame-info max-x
                      max-y
                      TIMELINE-ROW-HEIGHT
                      timeToPixModifier
                      ticks
                      (calc-process-timespan-lines tr ordered-segs max-x))
          ordered-segs))

;;pict-for-segment : segment -> pict
(define (pict-for-segment seg)
  (unless (segment-p seg)
    (define p (if (event-has-duration? (segment-event seg))
                  (rect-pict (segment-color seg)
                             (timeline-event-strokecolor)
                             (segment-width seg)
                             (segment-height seg)
                             #:stroke-width .5)
                  (circle-pict (segment-color seg)
                               (timeline-event-strokecolor)
                               MIN-SEG-WIDTH
                               #:stroke-width .5)))
    (set-segment-p! seg (if (< (segment-opacity seg) 1)
                            (cellophane p (segment-opacity seg))
                            p)))
  (segment-p seg))

;;draw-ruler-on : pict viewable-region frameinfo -> pict
(define (draw-ruler-on base vregion frameinfo)
  (for/fold ([pct base]) ([tick (in-list (filter (λ (t) (in-viewable-region-horiz vregion (timeline-tick-x t)))
                                                 (frame-info-timeline-ticks frameinfo)))])
    (define cur-x (timeline-tick-x tick))
    (define pinnedline
      (pin-over pct
                (- cur-x (viewable-region-x vregion))
                0
                (linestyle 'dot
                           (colorize (vline 1
                                            (viewable-region-height vregion))
                                     (timeline-tick-color)))))
    (if (timeline-tick-show-label? tick)
        (pin-over pinnedline
                  (- cur-x (viewable-region-x vregion))
                  3
                  (timeline-tick-label-pict (timeline-tick-rel-time tick)))
        pinnedline)))

;;draw-row-lines-on : pict viewable-region trace frameinfo -> pict
(define (draw-row-lines-on base vregion tr finfo opacity)
  (define num-tls (length (trace-proc-timelines tr)))
  (pin-over base
            0
            0
            (for/fold ([pct base]) ([tl (in-list (filter (λ (tline)
                                                           (define midy (calc-row-mid-y (process-timeline-proc-index tline)
                                                                                        (frame-info-row-height finfo)
                                                                                        tr))
                                                           (define topy (- midy (frame-info-row-height finfo)))
                                                           (define boty (+ midy (frame-info-row-height finfo)))
                                                           (or (in-viewable-region-vert? vregion topy)
                                                               (in-viewable-region-vert? vregion boty)))
                                                         (trace-proc-timelines tr)))])
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
                     [index (process-timeline-proc-index tl)]
                     [proc-name (if (zero? index)
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
                                     (- (* index (frame-info-row-height finfo)) (viewable-region-y vregion))
                                     (colorize (hline (viewable-region-width vregion) 1) (timeline-baseline-color)))
                                 (at 0
                                     (+ (+ (- (* index (frame-info-row-height finfo)) (viewable-region-y vregion))
                                           (- (frame-info-row-height finfo) (pict-height proc-title)))
                                        1)
                                     proc-title)
                                 (at start-x
                                     (- (calc-row-mid-y index
                                                        (frame-info-row-height finfo)
                                                        tr)
                                        (viewable-region-y vregion))
                                     (colorize (hline (- end-x start-x) 1)
                                               (timeline-event-baseline-color))))))))

;Magnifies a segment's pict (dot or square) to make
;it stand out when hovered over with the mouse pointer.
;;make-stand-out-pict : segment -> pict
(define (make-stand-out-pict seg)
  (case (event-type (segment-event seg))
    [(start-work start-0-work) (scale (pict-for-segment seg) 1 2)]
    [(gc) (cellophane (pict-for-segment seg) 1)]
    [else (scale (pict-for-segment seg) 2)]))

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

;;timeline-pict : (listof indexed-future-event) [viewable-region] [integer] -> pict
(define (timeline-pict logs
                       #:x [x #f]
                       #:y [y #f]
                       #:width [width #f]
                       #:height [height #f]
                       #:selected-event-index [selected-event-index #f])
  (define tr (build-trace logs))
  (define-values (finfo segments) (calc-segments tr))
  (define vregion (if x
                      (viewable-region x y width height)
                      (viewable-region 0 0 (frame-info-adjusted-width finfo) (frame-info-adjusted-height finfo))))
  (timeline-pict-for-trace-data vregion
                                tr
                                finfo
                                segments
                                #:selected-event-index selected-event-index))

;;timeline-pict : (or viewable-region #f) trace frame-info (listof segment) -> pict
(define (timeline-pict-for-trace-data vregion
                                      tr
                                      finfo
                                      segments
                                      #:selected-event-index [selected-event-index #f])
  (define vr (if (not vregion)
                 (viewable-region 0
                                  0
                                  (frame-info-adjusted-width finfo)
                                  (frame-info-adjusted-height finfo))
                 vregion))
  (define tp (for/fold ([pct (frame-bg vr finfo tr)])
               ([seg (in-list (filter (seg-in-vregion vr) segments))])
               (pin-over pct
                         (- (segment-x seg) (viewable-region-x vr))
                         (- (segment-y seg) (viewable-region-y vr))
                         (pict-for-segment seg))))
  (cond
    [selected-event-index
     (define overlay (timeline-overlay vregion
                                       #f
                                       (list-ref segments selected-event-index)
                                       finfo
                                       tr))
     (pin-over tp
               0
               0
               overlay)]
    [else tp]))

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
  (let ([fst (get-seg-previous-to-vregion vregion seg)])
    (let loop ([pct base-pct]
               [cur-seg fst]
               [event-scope #f])
        (if (not cur-seg)
            pct
            (let ([next (segment-next-future-seg cur-seg)])
              (define cur-evt (segment-event cur-seg))
              ;(define next-evt (segment-event next))
              (define in-rtcall-scope 
                (cond 
                  [(and (symbol? event-scope) (or (runtime-block-event? cur-evt)
                                                  (runtime-sync-event? cur-evt))) #f]
                  [(worker-sync-event? cur-evt) 'sync] 
                  [(worker-block-event? cur-evt) 'block] 
                  [else event-scope]))
              (let* ([next-targ (segment-next-targ-future-seg cur-seg)]
                     [prev-targ (segment-prev-targ-future-seg cur-seg)]
                     [ftl-arrows (if (not next)
                                     pct
                                     (draw-connection vregion
                                                      cur-seg
                                                      next
                                                      pct
                                                      (case in-rtcall-scope 
                                                        [(block) (get-event-color 'block)]
                                                        [(sync) (get-event-color 'sync)]
                                                        [else (event-connection-line-color)])
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
                (if (and next
                         ((seg-in-vregion vregion) next))
                    (loop next-targ-arr next in-rtcall-scope)
                    next-targ-arr)))))))

;Draws the pict that is layered on top of the exec. timeline canvas
;to highlight a specific future's event sequence
;;timeline-overlay : uint uint (or segment #f) (or segment #f) frame-info trace -> pict
(define (timeline-overlay vregion tacked hovered finfo tr)
  (define-values (width height) (values (viewable-region-width vregion)
                                        (viewable-region-height vregion)))
  (define base (blank (viewable-region-width vregion)
                      (viewable-region-height vregion)))
  (define-values (picked-seg showing-tacked)
    (if tacked (values tacked #t) (values hovered #f)))
  (cond
    [picked-seg
     (define bg base)
     (define aseg-rel-x (- (segment-x picked-seg) (viewable-region-x vregion)))
     (define aseg-rel-y (- (segment-y picked-seg) (viewable-region-y vregion)))
     (define emphasized (make-stand-out-pict picked-seg))
     (case (event-type (segment-event picked-seg))
       [(gc)
        (pin-over bg aseg-rel-x aseg-rel-y emphasized)]
       [else
        (let* ([line (pin-over bg
                               (- (+ aseg-rel-x
                                     (/ (segment-width picked-seg) 2))
                                  2)
                               0
                               (colorize (vline 1 height) (hover-tickline-color)))]
               [width-dif (/ (- (pict-width emphasized) (segment-width picked-seg)) 2)]
               [height-dif (/ (- (pict-height emphasized) (segment-height picked-seg)) 2)]
               [magnified (pin-over line
                                    (- aseg-rel-x width-dif)
                                    (- aseg-rel-y height-dif)
                                    emphasized)]
               [hover-magnified (if (and showing-tacked
                                         hovered
                                         (not (eq? hovered tacked)))
                                    (let* ([hmag (make-stand-out-pict hovered)]
                                           [hwidth-dif (/ (- (pict-width hmag)
                                                             (pict-width (pict-for-segment hovered)))
                                                          2)]
                                           [hheight-dif (/ (- (pict-height hmag)
                                                              (pict-height (pict-for-segment hovered)))
                                                           2)])
                                      (pin-over magnified
                                                (- (- (segment-x hovered) (viewable-region-x vregion)) hwidth-dif)
                                                (- (- (segment-y hovered) (viewable-region-y vregion)) hheight-dif)
                                                hmag))
                                    magnified)]
               [arrows (draw-arrows hover-magnified vregion picked-seg)])
          arrows)])]
    [else base]))

;Draw a line from one node on the creation graph to another
;;line-from : drawable-node drawable-node pict viewable-region -> pict
(define (line-from start end pct minx miny)
  (define par-center (drawable-node-center start))
  (define child-center (drawable-node-center end))
  (draw-line-onto pct
                  (- (point-x par-center) minx)
                  (- (point-y par-center) miny)
                  (- (point-x child-center) minx)
                  (- (point-y child-center) miny)
                  (create-graph-edge-color)
                  #:width 1
                  #:style 'dot))

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

;;creation-tree-pict : (listof indexed-future-event) [enni] [enni] [enni] [enni] [enni] [enni] [enni] -> pict
(define (creation-tree-pict events
                             #:x [x #f]
                             #:y [y #f]
                             #:width [width #f]
                             #:height [height #f]
                             #:node-width [node-width #f]
                             #:padding [padding #f]
                             #:zoom [zoom CREATE-GRAPH-MIN-ZOOM])
  (define tr (build-trace events))
  (define node-diam (if node-width
                        node-width
                        CREATE-GRAPH-NODE-DIAMETER))
  (define graph-padding (if padding
                            padding
                            CREATE-GRAPH-PADDING))
  (define layout (draw-tree (trace-creation-tree tr)
                            #:node-width node-diam
                            #:padding graph-padding
                            #:zoom zoom))
  (define vregion (if x
                      (viewable-region x y width height)
                      #f))
  (draw-creategraph-pict vregion layout))


;;draw-creategraph-pict : (or/c viewable-region #f) tree-layout -> pict
;; if vregion is #f, return a pict that includes the entire tree
(define (draw-creategraph-pict vregion layout)
  (define rt-root (first (graph-layout-nodes layout)))
  (define width (inexact->exact (floor (graph-layout-width layout))))
  (define height (inexact->exact (floor (graph-layout-height layout))))
  (define base (if vregion
                   (blank (viewable-region-width vregion) (viewable-region-height vregion))
                   (blank)))
  (define minx (if vregion (viewable-region-x vregion) 0))
  (define miny (if vregion (viewable-region-y vregion) 0))
  (define viewable-nodes (if vregion
                             (filter (λ (n) (in-viewable-region vregion
                                                                (drawable-node-x n)
                                                                (drawable-node-y n)
                                                                (drawable-node-width n)
                                                                (drawable-node-width n)))
                                     (graph-layout-nodes layout))
                             (graph-layout-nodes layout)))
  (define with-arrows
    (let ([arrow-pct (for/fold ([pct base]) ([node (in-list (graph-layout-nodes layout))])
                       (for/fold ([p pct]) ([child (in-list (drawable-node-children node))])
                         (line-from node child p minx miny)))])
      (for/fold ([pct arrow-pct]) ([node (in-list viewable-nodes)])
        (pin-over pct
                  (- (drawable-node-x node) minx)
                  (- (drawable-node-y node) miny)
                  (node-pict node)))))
  (if vregion
      with-arrows
      (panorama with-arrows)))

(define (zoom-level->factor zoom-level)
   (+ 1.0 (* (- zoom-level CREATE-GRAPH-DEFAULT-ZOOM)
                       CREATE-GRAPH-ZOOM-FACTOR)))

;;graph-overlay-pict : drawable-node trace graph-layout -> pict
(define (graph-overlay-pict hover-node tr layout vregion scale-factor)
  (define (root-sym-or-first-evt n) (node-data (drawable-node-node n)))
  (cond
    [(or (not hover-node) (equal? (root-sym-or-first-evt hover-node) 'runtime-thread))
      #f]
    [else
     (define fid (event-user-data (root-sym-or-first-evt hover-node)))
     (define ri (hash-ref (trace-future-rtcalls tr) fid #f))
     (cond
       [(not ri) #f]
       [else
        (define block-ops (sort (hash-keys (rtcall-info-block-hash ri))
                                >
                                #:key (λ (p)
                                        (hash-ref (rtcall-info-block-hash ri) p))))
        (define sync-ops (sort (hash-keys (rtcall-info-sync-hash ri))
                               >
                               #:key (λ (op)
                                       (hash-ref (rtcall-info-sync-hash ri) op))))
        (define-values (node-origin-x node-origin-y)
          (values (* (- (drawable-node-x hover-node) (viewable-region-x vregion)) scale-factor)
                  (* (- (drawable-node-y hover-node) (viewable-region-y vregion)) scale-factor)))
        (define-values (center-x center-y)
          (values (+ node-origin-x (/ (* (drawable-node-width hover-node) scale-factor) 2))
                  (+ node-origin-y (/ (* (drawable-node-width hover-node) scale-factor) 2))))
        (define x (+ center-x CREATE-GRAPH-NODE-DIAMETER))
        (define-values (pct yacc)
          (for/fold ([p (pin-over (blank (viewable-region-width vregion) (viewable-region-height vregion))
                                  node-origin-x
                                  node-origin-y
                                  (scale (node-pict hover-node) scale-factor))]
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
        pct])]))
