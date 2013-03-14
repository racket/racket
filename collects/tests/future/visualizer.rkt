#lang racket/base 
(require rackunit 
         racket/list 
         racket/vector
         future-visualizer/private/visualizer-drawing
         future-visualizer/private/visualizer-data 
         future-visualizer/private/display 
         future-visualizer/private/graph-drawing)  

(define (compile-trace-data logs) 
  (define tr (build-trace logs)) 
  (define-values (finfo segs) (calc-segments tr)) 
  (values tr 
          finfo 
          segs 
          (frame-info-timeline-ticks finfo)))

(define (check-in-bounds? segs finfo) 
  (for ([s (in-list segs)]) 
    (check-false (negative? (segment-y s)))
    (check-true (< (segment-y s) (frame-info-adjusted-height finfo)))))

;Display tests
(let ([vr (viewable-region 3 3 500 500)])
  (for ([i (in-range 4 503)])
    (check-true (in-viewable-region-horiz vr i)
                (format "~a should be in ~a" i vr)))
  (for ([i (in-range 0 2)])
    (check-false (in-viewable-region-horiz vr i)
                 (format "~a should not be in ~a" i vr))
    (for ([i (in-range 504 1000)])
      (check-false (in-viewable-region-horiz vr i)
                   (format "~a should not be in ~a" i vr)))))

(let ([vr (viewable-region 0 0 732 685)])
  (check-true (in-viewable-region-horiz vr 10))
  (check-true (in-viewable-region-horiz vr 63.0))
  (check-true (in-viewable-region-horiz vr 116.0))
  (check-true (in-viewable-region-horiz vr 169.0))
  (check-true (in-viewable-region-horiz vr 222)))

(let ([vr (viewable-region 0 0 732 685)] 
      [ticks (list (timeline-tick 222.0 #f 0.4999999999999982 #f) 
                   (timeline-tick 169.0 #f 0.3999999999999986 #f) 
                   (timeline-tick 116.0 #f 0.29999999999999893 #f) 
                   (timeline-tick 63.0 #f 0.1999999999999993 #f) 
                   (timeline-tick 10 #f 0.09999999999999964 #f))]) 
  (define in-vr (filter (λ (t) 
                          (in-viewable-region-horiz vr (timeline-tick-x t))) 
                        ticks)) 
  (check-equal? (length in-vr) 5))

;Trace compilation tests
(let* ([future-log (list (indexed-future-event 0 (future-event #f 0 'create 0 #f 0)) 
                         (indexed-future-event 1 (future-event 0 1 'start-work 1 #f #f)) 
                         (indexed-future-event 2 (future-event 0 1 'end-work 2 #f #f)) 
                         (indexed-future-event 3 (future-event 0 0 'complete 3 #f #f)))] 
       [organized (organize-output future-log 0 3)]) 
  (check-equal? (vector-length organized) 2) 
  (let ([proc0log (vector-ref organized 0)] 
        [proc1log (vector-ref organized 1)]) 
    (check-equal? (vector-length proc0log) 2) 
    (check-equal? (vector-length proc1log) 2)))

(let* ([future-log (list (indexed-future-event 0 (future-event #f 0 'create 0 #f 0)) 
                         (indexed-future-event 1 (future-event 0 1 'start-work 1 #f #f)) 
                         (indexed-future-event 2 (future-event 0 1 'end-work 2 #f #f)) 
                         (indexed-future-event 3 (future-event 0 0 'complete 3 #f #f)))] 
       [trace (build-trace future-log)] 
       [evts (trace-all-events trace)])
  (check-equal? (length evts) 4) 
  (check-equal? (length (filter (λ (e) (event-next-future-event e)) evts)) 2) 
  (check-equal? (length (filter (λ (e) (event-next-targ-future-event e)) evts)) 1) 
  (check-equal? (length (filter (λ (e) (event-prev-targ-future-event e)) evts)) 1))

(let* ([future-log (list (indexed-future-event 0 (future-event 0 0 'create 0 #f 0)) 
                         (indexed-future-event 1 (future-event 1 0 'create 1 #f 1)) 
                         (indexed-future-event 2 (future-event 0 1 'start-work 2 #f #f)) 
                         (indexed-future-event 3 (future-event 1 2 'start-work 2 #f #f))
                         (indexed-future-event 4 (future-event 0 1 'end-work 4 #f #f)) 
                         (indexed-future-event 5 (future-event 0 0 'complete 5 #f #f)) 
                         (indexed-future-event 6 (future-event 1 2 'end-work 5 #f #f)) 
                         (indexed-future-event 7 (future-event 1 0 'complete 7 #f #f)))] 
       [organized (organize-output future-log 0 7)]) 
  (check-equal? (vector-length organized) 3) 
  (let ([proc0log (vector-ref organized 0)] 
        [proc1log (vector-ref organized 1)] 
        [proc2log (vector-ref organized 2)]) 
    (check-equal? (vector-length proc0log) 4) 
    (check-equal? (vector-length proc1log) 2)
    (check-equal? (vector-length proc2log) 2) 
    (for ([msg (in-vector (vector-map indexed-future-event-fevent proc0log))])  
      (check-equal? (future-event-process-id msg) 0)) 
    (for ([msg (in-vector (vector-map indexed-future-event-fevent proc1log))]) 
      (check-equal? (future-event-process-id msg) 1)) 
    (for ([msg (in-vector (vector-map indexed-future-event-fevent proc2log))]) 
      (check-equal? (future-event-process-id msg) 2))))

;Drawing calculation tests
(let* ([future-log (list (indexed-future-event 0 (future-event #f 0 'create 0 #f 0)) 
                         (indexed-future-event 1 (future-event 0 1 'start-work 1 #f #f)) 
                         (indexed-future-event 2 (future-event 0 1 'end-work 2 #f #f)) 
                         (indexed-future-event 3 (future-event 0 0 'complete 3 #f #f)))] 
       [trace (build-trace future-log)]) 
  (let-values ([(finfo segments) (calc-segments trace)])
    (check-in-bounds? segments finfo)
    (check-equal? (length segments) 4) 
    (check-equal? (length (filter (λ (s) (segment-next-future-seg s)) segments)) 2) 
    (check-equal? (length (filter (λ (s) (segment-next-targ-future-seg s)) segments)) 1) 
    (check-equal? (length (filter (λ (s) (segment-prev-targ-future-seg s)) segments)) 1)))

;Future=42
(let* ([future-log (list (indexed-future-event 0 (future-event #f 0 'create 0.05 #f 42)) 
                         (indexed-future-event 1 (future-event 42 1 'start-work 0.07 #f #f)) 
                         (indexed-future-event 2 (future-event 42 1 'end-work 0.3 #f #f)) 
                         (indexed-future-event 3 (future-event 42 0 'complete 1.2 #f #f)))] 
       [tr (build-trace future-log)])
  (define-values (finfo segs) (calc-segments tr))
  (check-in-bounds? segs finfo)
  (define ticks (frame-info-timeline-ticks finfo))
  (check-equal? (length ticks) 11))

(define (sanity-check-ticks ticks)
  (define ticks-in-ascending-time-order (reverse ticks))
  (let loop ([cur (car ticks-in-ascending-time-order)] 
             [rest (cdr ticks-in-ascending-time-order)]) 
    (unless (null? rest) 
      (define next (car rest))
      (check-true (>= (timeline-tick-x next) (timeline-tick-x cur)) 
                  (format "Tick at time ~a [x:~a] had x-coord less than previous tick: ~a [x:~a]" 
                          (exact->inexact (timeline-tick-rel-time next)) 
                          (timeline-tick-x next) 
                          (exact->inexact (timeline-tick-rel-time cur)) 
                          (timeline-tick-x cur)))
      (loop next 
            (cdr rest)))))

;;do-seg-check : trace segment timeline-tick (a a -> bool) string -> void
(define (do-seg-check tr seg tick op adjective) 
  (define evt (segment-event seg))
  (check-true (op (segment-x seg) (timeline-tick-x tick)) 
              (format "Event at time ~a [x:~a] (~a) should be ~a tick at time ~a [x:~a]"
                      (relative-time tr (event-start-time evt))
                      (segment-x seg)
                      (event-type evt)
                      adjective
                      (exact->inexact (timeline-tick-rel-time tick)) 
                      (timeline-tick-x tick))))

;;check-seg-layout : trace (listof segment) (listof timeline-tick) -> void
(define (check-seg-layout tr segs ticks) 
  (for ([seg (in-list segs)])
    (define evt-rel-time (relative-time tr (event-start-time (segment-event seg))))
      (for ([tick (in-list ticks)]) 
        (define ttime (timeline-tick-rel-time tick))
          (cond 
            [(< evt-rel-time ttime) 
             (do-seg-check tr seg tick <= "before")] 
            [(= evt-rel-time ttime) 
             (do-seg-check tr seg tick = "equal to")]
            [(> evt-rel-time ttime) 
             (do-seg-check tr seg tick >= "after")]))))
             
(let* ([future-log (list (indexed-future-event 0 (future-event #f 0 'create 0.05 #f 42)) 
                         (indexed-future-event 1 (future-event 42 1 'start-work 0.09 #f #f)) 
                         (indexed-future-event 2 (future-event 42 1 'suspend 1.1 #f #f)) 
                         (indexed-future-event 3 (future-event 42 1 'resume 1.101 #f #f)) 
                         (indexed-future-event 4 (future-event 42 1 'suspend 1.102 #f #f)) 
                         (indexed-future-event 5 (future-event 42 1 'resume 1.103 #f #f)) 
                         (indexed-future-event 6 (future-event 42 1 'start-work 1.104 #f #f)) 
                         (indexed-future-event 7 (future-event 42 1 'complete 1.41 #f #f)) 
                         (indexed-future-event 8 (future-event 42 1 'end-work 1.42 #f #f)) 
                         (indexed-future-event 9 (future-event 42 0 'result 1.43 #f #f)))] 
       [tr (build-trace future-log)]) 
  (define-values (finfo segs) (calc-segments tr))
  (check-in-bounds? segs finfo)
  (define ticks (frame-info-timeline-ticks finfo))
  (check-seg-layout tr segs ticks))

(let* ([future-log (list (indexed-future-event 0 (future-event #f 0 'create 0 #f 0)) 
                         (indexed-future-event 1 (future-event 0 1 'start-work 1 #f #f)) 
                         (indexed-future-event 2 (future-event 0 1 'end-work 2 #f #f)) 
                         (indexed-future-event 3 (future-event 0 0 'complete 3 #f #f)))] 
       [trace (build-trace future-log)]) 
  (check-equal? (trace-start-time trace) 0) 
  (check-equal? (trace-end-time trace) 3) 
  (check-equal? (length (trace-proc-timelines trace)) 2) 
  (check-equal? (trace-real-time trace) 3) 
  (check-equal? (trace-num-futures trace) 1) 
  (check-equal? (trace-num-blocks trace) 0) 
  (check-equal? (trace-num-syncs trace) 0) 
  (let ([proc0tl (list-ref (trace-proc-timelines trace) 0)] 
        [proc1tl (list-ref (trace-proc-timelines trace) 1)]) 
    (check-equal? (process-timeline-start-time proc0tl) 0) 
    (check-equal? (process-timeline-end-time proc0tl) 3) 
    (check-equal? (process-timeline-start-time proc1tl) 1) 
    (check-equal? (process-timeline-end-time proc1tl) 2) 
    (let ([proc0segs (process-timeline-events proc0tl)] 
          [proc1segs (process-timeline-events proc1tl)]) 
      (check-equal? (length proc0segs) 2) 
      (check-equal? (length proc1segs) 2)
      (check-equal? (event-timeline-position (list-ref proc0segs 0)) 'start) 
      (check-equal? (event-timeline-position (list-ref proc0segs 1)) 'end)))) 

;Viewable region tests 
(define (make-seg-at x y w h) 
  (segment #f x y w h #f #f #f #f #f #f #f #f #f))

;;make-segs-with-times : (listof (or float (float . float))) -> (listof segment)
(define (make-segs-with-times . times) 
  (for/list ([t (in-list times)] [i (in-naturals)]) 
    (if (pair? t) 
        (make-seg-with-time i (car t) #:end-time (cdr t)) 
        (make-seg-with-time i t))))

;;make-seg-with-time : fixnum float [float] -> segment
(define (make-seg-with-time index real-start-time #:end-time [real-end-time real-start-time]) 
  (segment (event index
                  real-start-time 
                  real-end-time 
                  0 0 0 0 0 0 0 0 0 0 0 0 0 #f #f)  
           0 0 0 0 #f #f #f #f #f #f #f #f #f))
                   

(let ([vregion (viewable-region 20 30 100 100)] 
      [seg1 (make-seg-at 0 5 10 10)] 
      [seg2 (make-seg-at 20 30 5 5)] 
      [seg3 (make-seg-at 150 35 5 5)]) 
  (check-false ((seg-in-vregion vregion) seg1)) 
  (check-true ((seg-in-vregion vregion) seg2)) 
  (check-false ((seg-in-vregion vregion) seg3)))

;segs-equal-or-later 
(let ([segs (make-segs-with-times 0.1 
                                  0.3 
                                  1.2 
                                  (cons 1.4 1.9) 
                                  2.4 
                                  2.8 
                                  3.1)]) 
  (check-equal? (length (segs-equal-or-later 0.1 segs)) 7) 
  (check-equal? (length (segs-equal-or-later 0.3 segs)) 6) 
  (check-equal? (length (segs-equal-or-later 1.2 segs)) 5) 
  (check-equal? (length (segs-equal-or-later 1.4 segs)) 4) 
  (check-equal? (length (segs-equal-or-later 2.4 segs)) 3) 
  (check-equal? (length (segs-equal-or-later 2.8 segs)) 2) 
  (check-equal? (length (segs-equal-or-later 3.1 segs)) 1) 
  (check-equal? (length (segs-equal-or-later 4.0 segs)) 0))

;Tick drawing  
(let ([l (list (indexed-future-event 0 (future-event #f 0 'create 10.0 #f 0))
               (indexed-future-event 1 (future-event 0 0 'start-work 11.0 #f #f)) 
               (indexed-future-event 2 (future-event 0 0 'end-work 20.0 #f #f)))])
  (define-values (tr finfo segs ticks) (compile-trace-data l)) 
  ;Number of ticks can vary, but cannot exceed (total trace time / tick interval)
  (check-true (<= (length ticks) 100))
  (check-equal? (length (calc-ticks segs 1000 tr)) 99))

(let ([l (list (indexed-future-event 0 '#s(future-event #f 0 create 1334778395768.733 #f 3))
               (indexed-future-event 1 '#s(future-event 3 2 start-work 1334778395768.771 #f #f))
               (indexed-future-event 2 '#s(future-event 3 2 complete 1334778395864.648 #f #f))
               (indexed-future-event 3 '#s(future-event 3 2 end-work 1334778395864.652 #f #f)))]) 
  (define-values (tr finfo segs ticks) (compile-trace-data l))
  (define last-evt (indexed-future-event-fevent (list-ref l 3))) 
  (define first-evt (indexed-future-event-fevent (list-ref l 0))) 
  (define total-time (- (future-event-time last-evt) (future-event-time first-evt))) 
  (check-true (<= (length ticks) (inexact->exact (floor (* 10 total-time))))))

(define mand-first 
  (list (indexed-future-event 0 '#s(future-event #f 0 create 1334779294212.415 #f 1))
        (indexed-future-event 1 '#s(future-event 1 1 start-work 1334779294212.495 #f #f))
        (indexed-future-event 2 '#s(future-event 1 1 sync 1334779294212.501 #f #f))
        (indexed-future-event 3 (future-event 1 0 'sync 1334779294221.128 'allocate_memory #f))
        (indexed-future-event 4 '#s(future-event 1 0 result 1334779294221.138 #f #f))
        (indexed-future-event 5 '#s(future-event 1 1 result 1334779294221.15 #f #f)))) 
(let-values ([(tr finfo segs ticks) (compile-trace-data mand-first)]) 
  (check-seg-layout tr segs ticks))

(define single-block-log 
  (list
   (indexed-future-event 0 '#s(future-event #f 0 create 1339469018856.55 #f 1))
   (indexed-future-event 1 '#s(future-event 1 1 start-work 1339469018856.617 #f 0))
   (indexed-future-event 2 '#s(future-event 1 1 block 1339469018856.621 #f 0))
   (indexed-future-event 3 '#s(future-event 1 1 suspend 1339469018856.891 #f 0))
   (indexed-future-event 4 '#s(future-event 1 1 end-work 1339469018856.891 #f 0))
   (indexed-future-event 5 '#s(future-event 1 0 block 1339469019057.609 printf 0))
   (indexed-future-event 6 '#s(future-event 1 0 result 1339469019057.783 #f 0))
   (indexed-future-event 7 '#s(future-event 1 2 start-work 1339469019057.796 #f 0))
   (indexed-future-event 8 '#s(future-event 1 2 complete 1339469019057.799 #f 0))
   (indexed-future-event 9 '#s(future-event 1 2 end-work 1339469019057.801 #f 0)))) 
(let ([tr (build-trace single-block-log)]) 
  (check-equal? (length (hash-keys (trace-block-counts tr))) 1) 
  (check-equal? (length (hash-keys (trace-sync-counts tr))) 0) 
  (check-equal? (length (hash-keys (trace-future-rtcalls tr))) 1))

(define gci (gc-info #f 0 0 0 0 0 0 0 4.0 6.0))
(check-true (gc-event? gci)) 
(check-true (gc-event? (indexed-future-event 0 gci)))

(define gc-log1 
  (list 
   (indexed-future-event 0 (future-event #f 0 'create 10.0 #f 1)) 
   (indexed-future-event 1 (gc-info #f 0 0 0 0 0 0 0 4.0 6.0)) 
   (indexed-future-event 2 (future-event 1 1 'start-work 11.0 #f 0)) 
   (indexed-future-event 3 (future-event 1 1 'complete 14.0 #f 0)) 
   (indexed-future-event 4 (future-event 1 1 'end-work 15.0 #f 0)))) 
(let ([tr (build-trace gc-log1)]) 
  (check-true (not (findf gc-event? (trace-all-events tr)))) 
  (check-equal? (trace-num-gcs tr) 0)
  (check-equal? (process-timeline-proc-id (trace-gc-timeline tr)) 'gc) 
  (check-equal? (length (process-timeline-events (trace-gc-timeline tr))) 0))

(define gc-log2 
  (list 
   (indexed-future-event 0 (future-event #f 0 'create 10.0 #f 1)) 
   (indexed-future-event 1 (gc-info #f 0 0 0 0 0 0 0 14.0 19.0)) 
   (indexed-future-event 2 (future-event 1 1 'start-work 11.0 #f 0)) 
   (indexed-future-event 3 (future-event 1 1 'complete 20.0 #f 0)) 
   (indexed-future-event 4 (future-event 1 1 'end-work 21.0 #f 0)))) 
(let ([tr (build-trace gc-log2)]) 
  (check-equal? (length (filter gc-event? (trace-all-events tr))) 1) 
  (check-equal? (process-timeline-proc-id (trace-gc-timeline tr)) 'gc)
  (check-equal? (length (process-timeline-events (trace-gc-timeline tr))) 1)
  (check-equal? (trace-num-gcs tr) 1))

(define gc-log3 
  (list 
   (indexed-future-event 0 (future-event #f 0 'create 10.0 #f 1)) 
   (indexed-future-event 1 (future-event 1 1 'start-work 11.0 #f 0))
   (indexed-future-event 2 (gc-info #f 0 0 0 0 0 0 0 14.0 15.0)) 
   (indexed-future-event 3 (gc-info #f 0 0 0 0 0 0 0 15.0 19.5)) 
   (indexed-future-event 4 (future-event 1 1 'complete 20.0 #f 0)) 
   (indexed-future-event 5 (future-event 1 1 'end-work 21.0 #f 0)))) 
(let-values ([(tr finfo segs ticks) (compile-trace-data gc-log3)])
  (check-in-bounds? segs finfo)
  (check-equal? (length (filter gc-event? (trace-all-events tr))) 2) 
  (check-equal? (trace-num-gcs tr) 2) 
  (check-equal? (length (trace-proc-timelines tr)) 2)
  (check-equal? (process-timeline-proc-id (trace-gc-timeline tr)) 'gc)
  (check-equal? (length (process-timeline-events (trace-gc-timeline tr))) 2)
  (let ([gc-segs (filter (λ (s) (gc-event? (segment-event s))) segs)]) 
    (check-equal? (length gc-segs) 2) 
    (for ([gs (in-list gc-segs)]) 
      (check-true (= (segment-height gs) (frame-info-adjusted-height finfo))) 
      (check-true (> (segment-width gs) 10))
      (check-true (= (segment-y gs) 0)))))

(check-true (work-event? (future-event #f 0 'start-work 1.0 #f 0))) 
(check-true (work-event? (future-event #f 0 'start-0-work 2.0 #f 0))) 
(check-false (work-event? (future-event #f 0 'end-work 1.0 #f 0)))

;Graph drawing tests 
(let* ([nodea (drawable-node (node 'a '()) 5 5 10 0 0 '() 10)]
       [center (drawable-node-center nodea)]) 
  (check-equal? (point-x center) 10.0) 
  (check-equal? (point-y center) 10.0))


(define test-padding 5)
(define test-width 10)

(define (tree root-data . children) 
  (node root-data children))

(define (get-node data layout) 
  (first (filter (λ (dn) (equal? (node-data (drawable-node-node dn)) data)) (graph-layout-nodes layout))))

#|
   a
   |
   b
|#
(define tree0 (tree 'a (tree 'b)))
(let* ([layout (draw-tree tree0 #:node-width test-width #:padding test-padding)]
       [dnode-a (get-node 'a layout)]
       [dnode-b (get-node 'b layout)])
  (check-equal? (graph-layout-width layout) (+ (* test-padding 2) test-width))
  (check-equal? (graph-layout-height layout) (+ (* test-padding 3) (* test-width 2)))
  (check-equal? (drawable-node-x dnode-a) test-padding) 
  (check-equal? (drawable-node-y dnode-a) test-padding) 
  (check-equal? (drawable-node-x dnode-b) test-padding) 
  (check-equal? (drawable-node-y dnode-b) (+ test-padding test-width test-padding)))
(let ([atree (build-attr-tree tree0 0)]) 
  (check-equal? (attributed-node-num-leaves atree) 1))

#|
      a
     / \
    b   c
|# 
(define tree1 (tree 'a 
                    (tree 'b) 
                    (tree 'c)))
(define layout (draw-tree tree1 #:node-width test-width #:padding test-padding)) 
(for ([dnode (in-list (graph-layout-nodes layout))]) 
  (check-equal? (drawable-node-width dnode) test-width))
(define dnode-a (get-node 'a layout)) 
(define dnode-b (get-node 'b layout)) 
(define dnode-c (get-node 'c layout))

(define slot-one-pos (+ test-padding test-width test-padding))
(define square-sz (+ (* test-padding 3) (* test-width 2)))
(check-equal? (graph-layout-width layout) square-sz) 
(check-equal? (graph-layout-height layout) square-sz)
(check-equal? (drawable-node-x dnode-b) test-padding) 
(check-equal? (drawable-node-y dnode-b) slot-one-pos)
(check-equal? (drawable-node-x dnode-c) slot-one-pos)
(check-equal? (drawable-node-y dnode-c) slot-one-pos) 
(check-equal? (drawable-node-x dnode-a) (/ 25 2)) 
(check-equal? (drawable-node-y dnode-a) test-padding) 
(check-equal? (length (drawable-node-children dnode-a)) 2)
(let ([atree (build-attr-tree tree1 0)]) 
  (check-equal? (attributed-node-num-leaves atree) 2))

#|
         a
       /  \
      b    d
      |   / \
      c   e f
            |
            g
|#
(define tree2 (tree 'a 
                    (tree 'b 
                          (tree 'c)) 
                    (tree 'd 
                          (tree 'e) 
                          (tree 'f 
                                (tree 'g)))))
(let* ([layout (draw-tree tree2 #:node-width test-width #:padding test-padding)] 
       [nodes (graph-layout-nodes layout)] 
       [dnode-a (get-node 'a layout)] 
       [dnode-b (get-node 'b layout)] 
       [dnode-c (get-node 'c layout)] 
       [dnode-d (get-node 'd layout)] 
       [dnode-e (get-node 'e layout)] 
       [dnode-f (get-node 'f layout)] 
       [dnode-g (get-node 'g layout)])
  (check-equal? (node-data (drawable-node-node dnode-a)) 'a) 
  (check-equal? (node-data (drawable-node-node dnode-b)) 'b) 
  (check-equal? (node-data (drawable-node-node dnode-c)) 'c) 
  (check-equal? (node-data (drawable-node-node dnode-d)) 'd) 
  (check-equal? (node-data (drawable-node-node dnode-e)) 'e) 
  (check-equal? (node-data (drawable-node-node dnode-f)) 'f) 
  (check-equal? (node-data (drawable-node-node dnode-g)) 'g) 
  (check-equal? (graph-layout-width layout) 50) 
  (check-equal? (graph-layout-height layout) 65) 
  (check-equal? (drawable-node-x dnode-a) (/ 65 4))
  (check-equal? (drawable-node-y dnode-a) test-padding)
  (check-equal? (drawable-node-x dnode-b) test-padding) 
  (check-equal? (drawable-node-y dnode-b) (+ (* 2 test-padding) test-width)) 
  (check-equal? (drawable-node-x dnode-c) test-padding) 
  (check-equal? (drawable-node-y dnode-c) (+ (drawable-node-y dnode-b) test-width test-padding)) 
  (check-equal? (drawable-node-x dnode-e) (+ (* 2 test-padding) test-width)) 
  (check-equal? (drawable-node-y dnode-e) (+ (drawable-node-y dnode-d) test-width test-padding))
  (check-equal? (drawable-node-x dnode-f) (+ (drawable-node-x dnode-e) test-width test-padding))
  (check-equal? (drawable-node-y dnode-f) (drawable-node-y dnode-e))
  (check-equal? (drawable-node-x dnode-g) (drawable-node-x dnode-f))
  (check-equal? (drawable-node-y dnode-g) (+ (drawable-node-y dnode-f) test-width test-padding)))
(let ([atree (build-attr-tree tree2 0)]) 
  (check-equal? (attributed-node-num-leaves atree) 3))

#|
        a
       /|\
      b c e
        |
        d
|#
(define tree3 (tree 'a 
                    (tree 'b) 
                    (tree 'c 
                          (tree 'd)) 
                    (tree 'e)))
(let* ([layout (draw-tree tree3 #:node-width test-width #:padding test-padding)] 
       [nodes (graph-layout-nodes layout)] 
       [dnode-a (get-node 'a layout)] 
       [dnode-b (get-node 'b layout)] 
       [dnode-c (get-node 'c layout)] 
       [dnode-d (get-node 'd layout)] 
       [dnode-e (get-node 'e layout)]) 
  (check-equal? (graph-layout-width layout) 50) 
  (check-equal? (graph-layout-height layout) 50) 
  (check-equal? (drawable-node-x dnode-a) 20) 
  (check-equal? (drawable-node-y dnode-a) 5)
  (check-equal? (drawable-node-x dnode-b) test-padding) 
  (check-equal? (drawable-node-y dnode-b) (+ (* 2 test-padding) test-width)) 
  (check-equal? (drawable-node-x dnode-c) (+ (* 2 test-padding) test-width)) 
  (check-equal? (drawable-node-y dnode-c) (drawable-node-y dnode-b)) 
  (check-equal? (drawable-node-x dnode-e) (+ (* 3 test-padding) (* 2 test-width))) 
  (check-equal? (drawable-node-y dnode-e) (drawable-node-y dnode-c)) 
  (check-equal? (drawable-node-x dnode-d) (drawable-node-x dnode-c)) 
  (check-equal? (drawable-node-y dnode-d) (+ (drawable-node-y dnode-c) test-padding test-width)))
(let ([atree (build-attr-tree tree3 0)]) 
  (check-equal? (attributed-node-num-leaves atree) 3))

#| 
        a
     / | | \
    b  c f  g 
      / \
     d   e
|#
(define tree4 (tree 'a 
                    (tree 'b) 
                    (tree 'c 
                          (tree 'd) 
                          (tree 'e)) 
                    (tree 'f) 
                    (tree 'g)))
(let* ([layout (draw-tree tree4 #:node-width test-width #:padding test-padding)] 
       [nodes (graph-layout-nodes layout)] 
       [dnode-a (get-node 'a layout)] 
       [dnode-b (get-node 'b layout)] 
       [dnode-c (get-node 'c layout)] 
       [dnode-d (get-node 'd layout)] 
       [dnode-e (get-node 'e layout)] 
       [dnode-f (get-node 'f layout)] 
       [dnode-g (get-node 'g layout)]) 
  (check-equal? (graph-layout-width layout) 80) 
  (check-equal? (graph-layout-height layout) 50) 
  (check-equal? (drawable-node-x dnode-b) test-padding) 
  (check-equal? (drawable-node-y dnode-b) (+ (drawable-node-y dnode-a) test-width test-padding)) 
  (check-equal? (drawable-node-y dnode-c) (drawable-node-y dnode-b)) 
  (check-equal? (drawable-node-x dnode-d) (+ (drawable-node-x dnode-b) test-width test-padding)) 
  (check-equal? (drawable-node-y dnode-d) (+ (drawable-node-y dnode-c) test-width test-padding)) 
  (check-equal? (drawable-node-x dnode-e) (+ (drawable-node-x dnode-d) test-width test-padding)) 
  (check-equal? (drawable-node-y dnode-e) (drawable-node-y dnode-d)) 
  (check-equal? (drawable-node-x dnode-f) (+ (drawable-node-x dnode-e) test-width test-padding)) 
  (check-equal? (drawable-node-y dnode-f) (drawable-node-y dnode-c)) 
  (check-equal? (drawable-node-x dnode-g) (+ (drawable-node-x dnode-f) test-width test-padding)))
(let ([atree (build-attr-tree tree4 0)]) 
  (check-equal? (attributed-node-num-leaves atree) 5))

#| 
Layered-tree-draw example from Di Battista 
         a
        /   \
       b      g
       |     / \
       c    h   k
       |   / \
       d  i   j
      / \
     e   f 
|#
(define tree5 (tree 'a 
                    (tree 'b 
                          (tree 'c 
                                (tree 'd 
                                      (tree 'e) 
                                      (tree 'f)))) 
                    (tree 'g 
                          (tree 'h 
                                (tree 'i) 
                                (tree 'j)) 
                          (tree 'k))))
(let* ([layout (draw-tree tree5 #:node-width test-width #:padding test-padding)] 
       [nodes (graph-layout-nodes layout)] 
       [dnode-a (get-node 'a layout)] 
       [dnode-b (get-node 'b layout)] 
       [dnode-c (get-node 'c layout)] 
       [dnode-d (get-node 'd layout)] 
       [dnode-e (get-node 'e layout)] 
       [dnode-f (get-node 'f layout)] 
       [dnode-g (get-node 'g layout)] 
       [dnode-h (get-node 'h layout)] 
       [dnode-i (get-node 'i layout)] 
       [dnode-j (get-node 'j layout)] 
       [dnode-k (get-node 'k layout)]) 
  (check-equal? (graph-layout-width layout) 80) 
  (check-equal? (graph-layout-height layout) 80) 
  (check-equal? (drawable-node-x dnode-e) test-padding) 
  (check-equal? (drawable-node-y dnode-e) 65) 
  (check-equal? (drawable-node-x dnode-f) (+ (drawable-node-x dnode-e) test-width test-padding)) 
  (check-equal? (drawable-node-x dnode-i) (+ (drawable-node-x dnode-f) test-width test-padding)) 
  (check-equal? (drawable-node-x dnode-j) (+ (drawable-node-x dnode-i) test-width test-padding)) 
  (check-equal? (drawable-node-x dnode-k) (+ (drawable-node-x dnode-j) test-width test-padding)))
(let ([atree (build-attr-tree tree5 0)]) 
  (check-equal? (attributed-node-num-leaves atree) 5))



  

