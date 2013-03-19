#lang racket/base
(require (only-in racket/list flatten) 
         (only-in racket/future futures-enabled?)
         racket/set 
         (only-in racket/vector vector-drop)
         "constants.rkt"
         "graph-drawing.rkt" 
         "display.rkt"
         (only-in '#%futures 
                  reset-future-logs-for-tracing! 
                  mark-future-trace-end!)) 

(provide start-future-tracing! 
         stop-future-tracing!
         (struct-out future-event)
         (struct-out gc-info)
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
         runtime-block-event?
         worker-block-event?
         runtime-sync-event? 
         worker-sync-event?
         gc-event?
         work-event?
         final-event? 
         relative-time 
         event-or-gc-time
         proc-id-or-gc<?)

(define-struct future-event (future-id process-id what time prim-name user-data) 
  #:prefab)

(define-struct gc-info (major? 
                        pre-used 
                        pre-admin 
                        code-page-total 
                        post-used 
                        post-admin 
                        start-time 
                        end-time 
                        start-real-time 
                        end-real-time) #:prefab)

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
               gc-timeline ;process-timeline where proc-id == 'gc, and each event is a GC
               all-events ;(listof event)
               real-time ;Total amount of time for the trace (in ms)
               num-futures ;Number of futures created
               num-blocks ;Number of barricades hit
               num-syncs ;Number of 'atomic' ops done 
               num-gcs ;Number of GC's that occurred during the trace
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
               [user-data #:mutable]
               type 
               [prim-name #:mutable]
               timeline-position ;The event's position among all events occurring in its process (sorted by time)
               [prev-proc-event #:mutable] 
               [next-proc-event #:mutable]
               [prev-future-event #:mutable]
               [next-future-event #:mutable]
               [next-targ-future-event #:mutable] 
               [prev-targ-future-event #:mutable]
               ;If the event is a block on a future thread, pointer to the corresponding 
               ;event indicating block handled on runtime thread
               [block-handled-event #:mutable] 
               [segment #:mutable]) #:transparent)

;;event-has-duration? : event -> bool
(define (event-has-duration? evt) 
  (case (event-type evt) 
    [(start-work start-0-work gc) #t] 
    [else #f]))

(define (missing-data? log) 
  (if (findf (λ (e) (equal? (future-event-what (indexed-future-event-fevent e)) 'missing)) log) 
      #t 
      #f))

;;event-op-name : (or event indexed-future-event future-event) -> symbol
(define (op-name evt) 
  (cond 
    [(event? evt) (event-prim-name evt)] 
    [(indexed-future-event? evt) (op-name (indexed-future-event-fevent evt))] 
    [(future-event? evt) (future-event-prim-name evt)]
    [(gc-info? evt) 'gc]))

;;event-what : (or event indexed-future-event future-event) -> symbol
(define (what evt) 
  (cond 
    [(event? evt) (event-type evt)] 
    [(indexed-future-event? evt) (what (indexed-future-event-fevent evt))] 
    [(future-event? evt) (future-event-what evt)] 
    [(gc-info? evt) 'gc]))

;;process-id : (or event indexed-future-event future-event) -> exact-nonnegative-integer
(define (process-id evt) 
  (cond 
    [(event? evt) (event-proc-id evt)] 
    [(indexed-future-event? evt) (process-id (indexed-future-event-fevent evt))] 
    [(future-event? evt) (future-event-process-id evt)]
    [(gc-info? evt) RT-THREAD-ID]))

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

;;work-event : (or event indexed-future-event future-event) -> bool
(define (work-event? evt) 
  (case (what evt) 
    [(start-work start-0-work) #t] 
    [else #f]))

;;runtime-thread-evt? : (or event indexed-future-event future-event) -> bool
(define (runtime-thread-evt? evt) 
  (define pid (process-id evt))
  (and (number? pid) (= (process-id evt) RT-THREAD-ID)))

;;runtime-synchronization-event? : (or event indexed-future-event future-event) -> bool
(define (runtime-synchronization-event? evt) 
  (and (synchronization-event? evt) (= (process-id evt) RT-THREAD-ID)))

;;runtime-block-event? : (or event indexed-future-event future-event) -> bool
(define (runtime-block-event? evt) 
  (and (runtime-thread-evt? evt) (equal? (what evt) 'block))) 

;;worker-block-event? : (or event indexed-future-event future-event) -> bool
(define (worker-block-event? evt) 
  (and (not (runtime-thread-evt? evt)) (equal? (what evt) 'block)))

;;runtime-sync-evt? : (or event indexed-future-event future-event) -> bool
(define (runtime-sync-event? evt) 
  (and (runtime-thread-evt? evt) (equal? (what evt) 'sync)))

;;worker-sync-evt? : (or event indexed-future-event future-event) -> bool
(define (worker-sync-event? evt) 
  (and (not (runtime-sync-event? evt)) (equal? (what evt) 'sync)))

;;gc-event? : (or event indexed-future-event future-event) -> bool
(define (gc-event? evt) 
  (equal? (what evt) 'gc))

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

;;event-or-gc-time : (or future-event gc-info indexed-future-event) -> float
(define (event-or-gc-time evt) 
  (cond 
    [(future-event? evt) (future-event-time evt)] 
    [(gc-info? evt) (gc-info-start-real-time evt)]
    [else (event-or-gc-time (indexed-future-event-fevent evt))]))

;;process-id-or-gc : (or future-event gc-info) -> (or nonnegative-integer 'gc)
(define (process-id-or-gc evt) 
  (if (future-event? evt) 
      (future-event-process-id evt) 
      'gc))

;;timeline-events/private : -> void
(define (timeline-events/private) 
  (let ([info (sync/timeout 0 recv)]) 
    (if info 
        (let ([v (vector-ref info 2)]) 
          (cond 
            [(future-event? v) 
             (case (future-event-what v) 
               [(stop-trace) '()] 
               [else (cons v (timeline-events/private))])] 
            [(gc-info? v) (cons v (timeline-events/private))] 
            [else (timeline-events/private)])) 
        (timeline-events/private))))
          
;Gets log events for an execution timeline
;;timeline-events : (listof indexed-future-event)
(define (timeline-events)
  (cond 
    [(not (futures-enabled?)) '()]
    [else 
     (define sorted (sort (timeline-events/private)
                          #:key event-or-gc-time 
                          <)) 
     (for/list ([evt (in-list sorted)] 
                [i (in-naturals)]) 
       (indexed-future-event i evt))]))

;;proc-id-or-gc<? : (or number symbol) (or number symbol) -> bool
(define (proc-id-or-gc<? a b) 
  (cond 
    [(equal? b 'gc) #f]
    [(equal? a 'gc) #t]  
    [else (< a b)]))

;Produces a vector of vectors, where each inner vector contains 
;all the log output messages for a specific process
;;organize-output : (listof indexed-future-event) real real -> (vectorof (vectorof future-event))
(define (organize-output raw-log-output start-time end-time)
  (define unique-proc-ids (for/set ([ie (in-list (filter (λ (e) 
                                                           (between (event-or-gc-time (indexed-future-event-fevent e)) 
                                                                    start-time 
                                                                    end-time))
                                                         raw-log-output))]) 
                                   (process-id-or-gc (indexed-future-event-fevent ie))))
  (for/vector ([procid (in-list (sort (set->list unique-proc-ids) proc-id-or-gc<?))])
    (for/vector ([e (in-list raw-log-output)] 
                 #:when (equal? procid (process-id-or-gc (indexed-future-event-fevent e)))) 
      e)))

;;Grab the first and last future events in the trace. 
;;first-and-last-fevents : (listof (or future-event gc-info)) -> (values future-event future-event)
(define (first-and-last-fevents log)  
  (let loop ([fst #f] 
             [last #f] 
             [remaining-log log])
    (cond 
      [(null? remaining-log) (values fst last)] 
      [else 
       (define f (indexed-future-event-fevent (car remaining-log)))
       (define rest (cdr remaining-log))
       (cond 
         [fst (if (future-event? f) 
                  (loop fst f rest) 
                  (loop fst last rest))] 
         [else (if (future-event? f) 
                   (loop f last rest) 
                   (loop fst last rest))])])))

;;event-pos-description : uint uint -> (or 'singleton 'start 'end 'interior)
(define (event-pos-description index timeline-len) 
  (cond 
    [(zero? index) (if (= index (sub1 timeline-len)) 
                       'singleton 
                       'start)] 
    [(= index (sub1 timeline-len)) 'end] 
    [else 'interior]))

;;build-timelines : (vectorof (vectorof future-event)) -> (listof process-timeline)
(define (build-timelines data) 
  (for/list ([proc-log-vec (in-vector data)]  
             [i (in-naturals)]) 
    (define timeline-len (vector-length proc-log-vec))
    (let* ([fst-ie (vector-ref proc-log-vec 0)]
           [fst-log-msg (indexed-future-event-fevent fst-ie)])
      (process-timeline (process-id-or-gc fst-log-msg) 
                        i
                        (event-or-gc-time fst-log-msg) 
                        (event-or-gc-time (indexed-future-event-fevent 
                                           (vector-ref proc-log-vec 
                                                       (sub1 timeline-len)))) 
                        (for/list ([ie (in-vector proc-log-vec)]
                                   [j (in-naturals)])
                          (define evt (indexed-future-event-fevent ie))
                          (define pos (event-pos-description j timeline-len))
                          (define start (event-or-gc-time evt))
                          (define end (if (or (equal? pos 'end) (equal? pos 'singleton)) 
                                          start 
                                          (future-event-time (indexed-future-event-fevent 
                                                              (vector-ref proc-log-vec (add1 j))))))
                          (event (indexed-future-event-index ie) 
                                 start 
                                 end
                                 (process-id-or-gc evt) 
                                 i
                                 (future-event-future-id evt) 
                                 (future-event-user-data evt)
                                 (future-event-what evt) 
                                 (future-event-prim-name evt)
                                 pos 
                                 #f #f #f #f #f #f #f #f))))))
    
;;build-trace : (listof indexed-future-event) -> trace
(define (build-trace log-output) 
  (when (null? log-output) 
    (error 'build-trace "Empty timeline in log-output"))
  (define-values (fst last) (first-and-last-fevents log-output))
  (when (and (not fst) (not last)) ;If the log has no future events (only GC's) no timeline
    (error 'build-trace "Empty timeline in log-output"))
  (define start-time (future-event-time fst)) 
  (define end-time (future-event-time last))
  (define data (organize-output log-output start-time end-time))
  (define-values (unique-fids nblocks nsyncs gcs) 
    (for/fold ([unique-fids (set)]
               [nblocks 0] 
               [nsyncs 0]
               [gc-evts '()]) ([ie (in-list log-output)])
      (define evt (indexed-future-event-fevent ie)) 
      (cond 
        [(gc-info? evt) 
         (cond 
           [(between (event-or-gc-time evt) start-time end-time) 
            (values unique-fids nblocks nsyncs (cons ie gc-evts))] 
           [else (values unique-fids nblocks nsyncs gc-evts)])]
        [else 
         (define fid (future-event-future-id evt)) 
         (define is-future-thread? (not (= (future-event-process-id evt) RT-THREAD-ID)))
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
          (if (and is-future-thread? (equal? (future-event-what evt) 'sync)) 
              (add1 nsyncs) 
              nsyncs) 
          gc-evts)])))
  (define ngcs (length gcs))
  ;If we have any GC events, the 0th element of 'data' contains them; 
  ;don't build a timeline for it in the usual manner
  (define tls (build-timelines (if (zero? ngcs) data (vector-drop data 1))))
  (define gc-timeline (process-timeline 'gc 
                                        'gc
                                        start-time 
                                        end-time 
                                        (for/list ([gcie (in-list gcs)]
                                                   [i (in-naturals)]) 
                                          (define gc (indexed-future-event-fevent gcie))
                                          (event (indexed-future-event-index gcie) 
                                                 (event-or-gc-time gc) 
                                                 (gc-info-end-real-time gc) 
                                                 'gc
                                                 'gc
                                                 #f 
                                                 (if (gc-info-major? gc) 'major 'minor)
                                                 'gc 
                                                 #f 
                                                 (event-pos-description i ngcs) 
                                                 #f #f #f #f #f #f #f #f))))
  (define all-evts (sort (append (flatten (for/list ([tl (in-list tls)]) (process-timeline-events tl)))
                                 (process-timeline-events gc-timeline))
                         #:key event-index
                         <))
  (define non-gc-evts (filter (λ (e) (not (gc-event? e))) all-evts))
  (define future-tl-hash (let ([h (make-hash)]) 
                           (for ([evt (in-list non-gc-evts)]) 
                             (let* ([fid (event-future-id evt)]
                                    [existing (hash-ref h fid '())]) 
                               (hash-set! h fid (cons evt existing))))
                           h))
  (for ([fid (in-list (hash-keys future-tl-hash))]) 
    (hash-set! future-tl-hash fid (reverse (hash-ref future-tl-hash fid)))) 
  (define-values (block-hash sync-hash rtcalls-per-future-hash) (build-rtcall-hashes all-evts))
  (define tr (trace start-time 
                    end-time 
                    tls 
                    future-tl-hash 
                    gc-timeline
                    all-evts
                    (- end-time start-time) ;real time 
                    (set-count unique-fids) ;num-futures
                    nblocks                 ;num-blocks
                    nsyncs                  ;num-syncs 
                    ngcs                    ;num-gcs
                    0 
                    0 
                    block-hash
                    sync-hash 
                    rtcalls-per-future-hash ;hash of fid -> rtcall-info
                    (build-creation-graph future-tl-hash)))
  (connect-event-chains! tr) 
  (connect-target-fid-events! tr)
  tr)

;;build-rtcall-hash : (listof event) -> (values (blocking_prim -o-> count) (sync_prim -o-> count) (fid -o-> rtcall-info)
(define (build-rtcall-hashes evts) 
  (define block-hash (make-hash)) 
  (define sync-hash (make-hash)) 
  (define rt-hash (make-hash)) 
  (for ([evt (in-list (filter runtime-synchronization-event? evts))])
    (define isblock (runtime-block-event? evt))
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
      (cond 
        [(or (null? evts) (null? (cdr evts))) void]
        [else
         (set-event-prev-proc-event! (car (cdr evts)) (car evts))
         (set-event-next-proc-event! (car evts) (car (cdr evts)))
         (loop (cdr evts))])))
  (for ([fid (in-list (hash-keys (trace-future-timelines trace)))]) 
    (let loop ([evts (hash-ref (trace-future-timelines trace) fid)]
               [last-fthread-block #f])
      (cond 
        [(or (null? evts) (null? (cdr evts))) void]
        [else
         (define curevt (car evts)) 
         (define nextevt (car (cdr evts)))
         (set-event-prev-future-event! nextevt curevt)
         (set-event-next-future-event! curevt nextevt)
         (cond 
           [(and last-fthread-block (or (runtime-sync-event? curevt) (runtime-block-event? curevt)))
            (set-event-block-handled-event! last-fthread-block curevt)
            (set-event-prim-name! last-fthread-block (event-prim-name curevt))
            (set-event-user-data! last-fthread-block (event-user-data curevt))
            (loop (cdr evts) #f)] 
           [(or (worker-block-event? curevt) (worker-sync-event? curevt)) 
            (loop (cdr evts) curevt)] 
           [else 
            (loop (cdr evts) last-fthread-block)])]))))

;;connect-target-fid-events! : trace -> void
(define (connect-target-fid-events! trace)
  (let loop ([rest (trace-all-events trace)])
    (unless (null? rest)
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
