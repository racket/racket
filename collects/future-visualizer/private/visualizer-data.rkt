#lang racket/base
(require racket/bool 
         racket/list 
         racket/contract
         racket/future
         racket/set 
         "constants.rkt"
         "graph-drawing.rkt" 
         (only-in '#%futures 
                  reset-future-logs-for-tracing! 
                  mark-future-trace-end!)) 

(provide start-future-tracing! 
         stop-future-tracing!
         (struct-out future-event)
         (struct-out indexed-future-event)
         (struct-out trace) 
         (struct-out process-timeline) 
         (struct-out future-timeline)
         (struct-out event)
         (struct-out rtcall-info)
         timeline-events
         organize-output 
         build-trace 
         missing-data?
         event-has-duration? 
         op-name 
         touch-event?
         allocation-event? 
         jitcompile-event? 
         synchronization-event? 
         runtime-synchronization-event?
         final-event? 
         relative-time)

(define-struct future-event (future-id process-id what time prim-name user-data) 
  #:prefab)

;Contains an index and a future-event, 
;so we can preserve the order in which future-events 
;were logged. 
;Many future-events can be logged at what appears to be the same 
;time, apparently because the time values don't have great enough precision 
;to separate events which temporally occur close together.
(struct indexed-future-event (index fevent) #:transparent)

;The whole trace, with a start/end time and list of process timelines
(struct trace (start-time ;Absolute start time (in process milliseconds)
               end-time ;Absolute end time
               proc-timelines ;(listof process-timeline)
               future-timelines ;Hash of (future id --o--> (listof event))
               all-events ;(listof event)
               real-time ;Total amount of time for the trace (in ms)
               num-futures ;Number of futures created
               num-blocks ;Number of barricades hit
               num-syncs ;Number of 'atomic' ops done
               blocked-futures ;Number of futures which encountered a barricade at some point
               avg-syncs-per-future 
               block-counts ;prim name --o--> number of blocks 
               sync-counts ;op name --o--> number of syncs 
               future-rtcalls ;fid --o--> rtcall-info
               creation-tree))

(struct rtcall-info (fid 
                    block-hash ; prim name --o--> number of blocks
                    sync-hash) ; op name --o--> number of syncs 
  #:transparent)

;(struct process-timeline timeline (proc-index))
(struct process-timeline (proc-id 
                          proc-index
                          start-time 
                          end-time 
                          events))

;(struct future-timeline timeline ())
(struct future-timeline (future-id 
                         start-time 
                         end-time 
                         events))

;A block of time (e.g. a process executing a portion of a future thunk).
(struct event (index
               start-time 
               end-time 
               proc-id 
               proc-index ;The id of the process in which this event occurred
               future-id 
               user-data
               type 
               prim-name
               timeline-position ;The event's position among all events occurring in its process (sorted by time)
               [prev-proc-event #:mutable] 
               [next-proc-event #:mutable]
               [prev-future-event #:mutable]
               [next-future-event #:mutable]
               [next-targ-future-event #:mutable] 
               [prev-targ-future-event #:mutable]
               [segment #:mutable]) #:transparent)

;;event-has-duration? : event -> bool
(define (event-has-duration? evt) 
  (case (event-type evt) 
    [(start-work start-0-work) #t] 
    [else #f]))

(define (missing-data? log) 
  (if (findf (λ (e) (equal? (future-event-what (indexed-future-event-fevent e)) 'missing)) log) 
      #t 
      #f))

;;event-op-name : (or event indexed-future-event future-event) -> symbol
(define (op-name evt) 
  (cond 
    [(event? evt) (event-prim-name evt)] 
    [(indexed-future-event? evt) (future-event-prim-name (indexed-future-event-fevent evt))] 
    [(future-event? evt) (future-event-prim-name evt)]))

;;event-what : (or event indexed-future-event future-event) -> symbol
(define (what evt) 
  (cond 
    [(event? evt) (event-type evt)] 
    [(indexed-future-event? evt) (future-event-what (indexed-future-event-fevent evt))] 
    [(future-event? evt) (future-event-what evt)]))

;;process-id : (or event indexed-future-event future-event) -> exact-nonnegative-integer
(define (process-id evt) 
  (cond 
    [(event? evt) (event-proc-id evt)] 
    [(indexed-future-event? evt) (future-event-process-id (indexed-future-event-fevent evt))] 
    [(future-event? evt) (future-event-process-id evt)]))

;;touch-event? : (or event indexed-future-event future-event) -> symbol
(define (touch-event? evt) 
  (equal? (what evt) 'touch))

;;allocation-event? : (or event indexed-future-event future-event) -> bool
(define (allocation-event? evt) 
  (equal? (op-name evt) '|[allocate memory]|))

;;jitcompile-event : (or event indexed-future-event future-event) -> bool
(define (jitcompile-event? evt) 
  (equal? (op-name evt) '|[jit_on_demand]|))

;;synchronization-event? : (or event indexed-future-event future-event) -> bool
(define (synchronization-event? evt) 
  (case (what evt) 
    [(block sync) #t] 
    [else #f]))

;;runtime-thread-evt? : (or event indexed-future-event future-event) -> bool
(define (runtime-thread-evt? evt) 
  (= (process-id evt) RT-THREAD-ID))

;;runtime-synchronization-event? : (or event indexed-future-event future-event) -> bool
(define (runtime-synchronization-event? evt) 
  (and (synchronization-event? evt) (= (process-id evt) RT-THREAD-ID)))

;;runtime-block-evt? : (or event indexed-future-event future-event) -> bool
(define (runtime-block-evt? evt) 
  (and (runtime-thread-evt? evt) (equal? (what evt) 'block))) 

;;runtime-sync-evt? : (or event indexed-future-event future-event) -> bool
(define (runtime-sync-evt? evt) 
  (and (runtime-thread-evt? evt) (equal? (what evt) 'sync)))

;;final-event? : event -> bool
(define (final-event? evt) 
  (case (event-timeline-position evt) 
    [(end singleton) #t] 
    [else #f]))

;;get-relative-start-time : trace float -> float
(define (relative-time trace abs-time) 
  (- abs-time (trace-start-time trace)))

;Log message receiver 
(define recv #f) 

;;start-future-tracing! -> void
(define (start-future-tracing!)
  (reset-future-logs-for-tracing!)
  (when (not recv) 
    (set! recv (make-log-receiver (current-logger) 'debug))))

;;stop-future-tracing! -> void
(define (stop-future-tracing!) 
  (mark-future-trace-end!))

;;timeline-events/private : -> void
(define (timeline-events/private) 
  (let ([info (sync/timeout 0 recv)]) 
    (if info 
        (let ([v (vector-ref info 2)]) 
          (if (future-event? v) 
              (case (future-event-what v) 
                [(stop-trace) '()] 
                [else (cons v (timeline-events/private))]) 
              (timeline-events/private))) 
        (timeline-events/private))))
          
;Gets log events for an execution timeline
;;timeline-events : (listof indexed-future-event)
(define (timeline-events)
  (cond 
    [(not (futures-enabled?)) '()]
    [else 
     (define sorted (sort (timeline-events/private)
                          #:key future-event-time 
                          <)) 
     (for/list ([fe (in-list sorted)] 
                [i (in-naturals)]) 
       (indexed-future-event i fe))]))

;Produces a vector of vectors, where each inner vector contains 
;all the log output messages for a specific process
;;organize-output : (listof indexed-future-event) -> (vectorof (vectorof future-event))
(define (organize-output raw-log-output)
  (define unique-proc-ids (for/set ([ie (in-list raw-log-output)]) 
                                     (future-event-process-id (indexed-future-event-fevent ie))))
  (for/vector ([procid (in-list (sort (set->list unique-proc-ids) <))])
    (for/vector ([e (in-list raw-log-output)] 
                 #:when (eq? procid (future-event-process-id (indexed-future-event-fevent e)))) 
      e)))
  
;;build-trace : (listof indexed-future-event) -> trace
(define (build-trace log-output) 
  (when (empty? log-output) 
    (error 'build-trace "Empty timeline in log-output"))
  (define data (organize-output log-output))
  (define start-time (future-event-time (indexed-future-event-fevent (car log-output)))) 
  (define end-time (future-event-time (indexed-future-event-fevent (last log-output))))
  (define-values (unique-fids nblocks nsyncs) 
    (for/fold ([unique-fids (set)]
               [nblocks 0] 
               [nsyncs 0]) ([ie (in-list log-output)]) 
      (let* ([evt (indexed-future-event-fevent ie)] 
             [fid (future-event-future-id evt)]
             [is-future-thread? (not (= (future-event-process-id evt) RT-THREAD-ID))])
        (values 
         (if fid 
             (set-add unique-fids fid) 
             unique-fids)
         (if (and is-future-thread? 
                  (case (future-event-what evt) 
                    [(block touch) #t] 
                    [else #f]))
             (add1 nblocks) 
             nblocks) 
         (if (and is-future-thread? (symbol=? (future-event-what evt) 'sync)) 
             (add1 nsyncs) 
             nsyncs)))))
  (define tls (for/list ([proc-log-vec (in-vector data)]  
                         [i (in-naturals)]) 
                (let* ([fst-ie (vector-ref proc-log-vec 0)]
                       [fst-log-msg (indexed-future-event-fevent fst-ie)])
                  (process-timeline (future-event-process-id fst-log-msg) 
                                    i
                                    (future-event-time fst-log-msg) 
                                    (future-event-time (indexed-future-event-fevent 
                                                        (vector-ref proc-log-vec 
                                                                    (sub1 (vector-length proc-log-vec))))) 
                                    (for/list ([ie (in-vector proc-log-vec)]
                                               [j (in-naturals)])
                                      (let* ([evt (indexed-future-event-fevent ie)]
                                             [start (future-event-time evt)] 
                                             [pos (cond 
                                                   [(zero? j) (if (= j (sub1 (vector-length proc-log-vec))) 
                                                                  'singleton 
                                                                  'start)]
                                                   [(= j (sub1 (vector-length proc-log-vec))) 'end] 
                                                   [else 'interior])])
                                        (event (indexed-future-event-index ie) 
                                               start 
                                               (if (or (equal? pos 'end) (equal? pos 'singleton))
                                                   start 
                                                   (future-event-time (indexed-future-event-fevent 
                                                                       (vector-ref proc-log-vec (add1 j)))))
                                               (future-event-process-id evt) 
                                               i
                                               (future-event-future-id evt) 
                                               (future-event-user-data evt)
                                               (future-event-what evt) 
                                               (future-event-prim-name evt)
                                               pos 
                                               #f 
                                               #f 
                                               #f 
                                               #f 
                                               #f 
                                               #f
                                               #f)))))))
  (define all-evts (sort (flatten (for/list ([tl (in-list tls)]) (process-timeline-events tl))) 
                         #:key event-index 
                         <))
  (define ftls (let ([h (make-hash)]) 
                 (for ([evt (in-list all-evts)]) 
                   (let* ([fid (event-future-id evt)]
                          [existing (hash-ref h fid '())]) 
                     (hash-set! h fid (cons evt existing))))
                 h))
  (for ([fid (in-list (hash-keys ftls))]) 
    (hash-set! ftls fid (reverse (hash-ref ftls fid)))) 
  (define-values (block-hash sync-hash rtcalls-per-future-hash) (build-rtcall-hashes all-evts))
  (define tr (trace start-time 
                    end-time 
                    tls 
                    ftls
                    all-evts
                    (- end-time start-time) ;real time 
                    (set-count unique-fids) ;num-futures
                    nblocks ;num-blocks
                    nsyncs ;num-syncs
                    0 
                    0 
                    block-hash
                    sync-hash 
                    rtcalls-per-future-hash ;hash of fid -> rtcall-info
                    (build-creation-graph ftls)))
  (connect-event-chains! tr) 
  (connect-target-fid-events! tr)
  tr)

;;build-rtcall-hash : (listof event) -> (values (blocking_prim -o-> count) (sync_prim -o-> count) (fid -o-> rtcall-info)
(define (build-rtcall-hashes evts) 
  (define block-hash (make-hash)) 
  (define sync-hash (make-hash)) 
  (define rt-hash (make-hash)) 
  (for ([evt (in-list (filter runtime-synchronization-event? evts))])
    (define isblock (runtime-block-evt? evt))
    (define ophash (if isblock block-hash sync-hash))
    (hash-update! ophash 
                  (event-prim-name evt) 
                  (λ (old) (+ old 1)) 
                  0) 
    (hash-update! rt-hash 
                  (event-future-id evt) 
                  (λ (old) 
                    (let ([h (if isblock 
                                 (rtcall-info-block-hash old) 
                                 (rtcall-info-sync-hash old))]) 
                      (hash-update! h 
                                    (event-prim-name evt) 
                                    (λ (o) (+ o 1)) 
                                    0)) 
                    old)  
                  (rtcall-info (event-future-id evt) (make-hash) (make-hash))))
  (values block-hash sync-hash rt-hash))
                                
;;connect-event-chains! : trace -> void
(define (connect-event-chains! trace) 
  (for ([tl (in-list (trace-proc-timelines trace))]) 
    (let loop ([evts (process-timeline-events tl)])
      (if (or (empty? evts) (empty? (cdr evts))) 
          void 
          (begin 
            (set-event-prev-proc-event! (first (cdr evts)) (car evts))
            (set-event-next-proc-event! (car evts) (first (cdr evts)))
            (loop (cdr evts)))))) 
  (for ([fid (in-list (hash-keys (trace-future-timelines trace)))]) 
    (let ([events (hash-ref (trace-future-timelines trace) fid)])
      (let loop ([evts events]) 
        (if (or (empty? evts) (empty? (cdr evts))) 
            void 
            (begin 
              (set-event-prev-future-event! (first (cdr evts)) (car evts))
              (set-event-next-future-event! (car evts) (first (cdr evts))) 
              (loop (cdr evts))))))))

;;connect-target-fid-events! : trace -> void
(define (connect-target-fid-events! trace)
  (let loop ([rest (trace-all-events trace)])
    (unless (empty? rest)
      (let ([cur-evt (car rest)])
        (when (and (or (equal? (event-type cur-evt) 'create) 
                       (equal? (event-type cur-evt) 'touch)) 
                   (>= (event-user-data cur-evt) 0)) 
          (let ([targ-evt (findf (λ (e) (and (event-future-id e) 
                                             (= (event-future-id e) 
                                                (event-user-data cur-evt)))) 
                                 (cdr rest))]) 
            (when targ-evt
              (set-event-next-targ-future-event! cur-evt targ-evt) 
              (set-event-prev-targ-future-event! targ-evt cur-evt)))) 
        (loop (cdr rest))))))
      
;;creation-event : event -> bool
(define (creation-event? evt) 
  (equal? (event-type evt) 'create))

;;buid-creation-graph/private : (uint -o-> (listof future-event)) -> (listof node)
(define (build-creation-graph/private future-timelines evt) 
  (let* ([fid (event-user-data evt)] 
         [ftimeline (hash-ref future-timelines fid #f)]) 
    (if ftimeline 
        (let ([fevents (filter creation-event? (hash-ref future-timelines fid #f))])
          (for/list ([cevt (in-list fevents)]) 
            (node cevt 
                  (build-creation-graph/private future-timelines cevt)))) 
        (begin 
          (eprintf "WARNING: Could not find timeline for future ~a.  Creation tree may be truncated.\n" fid) 
          '()))))

;;build-creation-graph : (uint -o-> (listof future-event)) -> node
(define (build-creation-graph future-timelines) 
  (define roots (filter creation-event? 
                        (hash-ref future-timelines #f))) 
  (define root-nodes (for/list ([root (in-list roots)]) 
                       (node root 
                             (build-creation-graph/private future-timelines root)))) 
  (node 'runtime-thread 
        root-nodes))