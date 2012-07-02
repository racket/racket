#lang racket/base
(require racket/bool 
         racket/list 
         racket/contract
         racket/future 
         racket/set 
         "constants.rkt"
         "graph-drawing.rkt" 
         (only-in '#%futures init-visualizer-tracking!))

(provide (contract-out [start-performance-tracking! (-> void?)])
         (struct-out future-event)
         (struct-out indexed-fevent)
         (struct-out trace) 
         (struct-out process-timeline) 
         (struct-out future-timeline)
         (struct-out event)
         (struct-out rtcall-info)
         raw-log-output
         organize-output 
         build-trace 
         event-has-duration? 
         touch-event?
         allocation-event? 
         jitcompile-event? 
         final-event? 
         relative-time)

;Log message receiver
(define recv #f) 

;;start-performance-tracking! -> void
(define (start-performance-tracking!) 
  (when (not recv) 
    (init-visualizer-tracking!)
    (set! recv (make-log-receiver (current-logger) 'debug))))

(define-struct future-event (future-id process-id what time prim-name user-data) 
  #:prefab)

;Contains an index and a future-event, 
;so we can preserve the order in which future-events 
;were logged. 
;Many future-events can be logged at what appears to be the same 
;time, apparently because the time values don't have great enough precision 
;to separate events which temporally occur close together.
(struct indexed-fevent (index fevent) #:transparent)

;The whole trace, with a start/end time and list of process timelines
(struct trace (start-time 
               end-time
               proc-timelines 
               future-timelines
               all-events
               real-time ;TODO: What is this
               num-futures ;TODO: (length future-timelines)
               num-blocks
               num-syncs 
               blocked-futures 
               avg-syncs-per-future 
               block-counts ;prim name --o--> number of blocks 
               sync-counts ;op name --o--> number of syncs 
               future-rtcalls ;fid --o--> rtcall-info
               creation-tree))

(struct rtcall-info (fid 
                    block-hash ; prim name --o--> number of blocks
                    sync-hash) ; op name --o--> number of syncs 
  #:transparent)

;The timeline of events for a specific process 
(struct timeline (id 
                  start 
                  end 
                  events))
;(struct process-timeline timeline (proc-index))
(struct process-timeline (proc-id 
                          proc-index ;Why do we need this
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
               proc-index ;TODO: why here?
               future-id 
               user-data
               type 
               prim-name
               timeline-position ;TODO: what is this
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

(define (touch-event? evt) 
  (equal? (event-prim-name evt) 'touch))

;;allocation-event? : event -> bool
(define (allocation-event? evt) 
  (equal? (event-prim-name evt) '|[allocate memory|))

(define (jitcompile-event? evt) 
  (equal? (event-prim-name evt) '|[jit_on_demand]|))

;;final-event? : event -> bool
(define (final-event? evt) 
  (case (event-timeline-position evt) 
    [(end singleton) #t] 
    [else #f]))

(define (get-log-events) 
  (let ([info (sync/timeout 0 recv)]) 
    (if info 
        (let ([v (vector-ref info 2)]) 
          (cons v (get-log-events)))
        '())))

;;get-relative-start-time : trace float -> float
(define (relative-time trace abs-time) 
  (- abs-time (trace-start-time trace)))

;Gets log output as a straight list, ordered according to when the 
;message was logged
;;raw-log-output : uint -> (listof indexed-fevent)
(define (raw-log-output index)
  (let ([info (sync/timeout 0 recv)]) 
    (if info 
        (let ([v (vector-ref info 2)]) 
          (if (future-event? v) 
              (cons (indexed-fevent index v) (raw-log-output (add1 index))) 
              (raw-log-output index))) 
        '())))

(define (print-blocks raw-output) 
  (for ([fe (in-list raw-output)]) 
    (when (equal? (future-event-what fe) 'block) 
      (printf "~a\n" (future-event-prim-name fe)))))

;Produces a vector of vectors, where each inner vector contains 
;all the log output messages for a specific process
;;organize-output : (listof indexed-fevent) -> (vectorof (vectorof future-event))
(define (organize-output raw-log-output)  
  ;TODO: Try using for/set here, does calling code depend on ordering
  #;(define unique-proc-ids (for/set ([ie (in-list raw-log-output)]) 
                                     (future-event-process-id (indexed-fevent-fevent ie))))
  (let ([unique-proc-ids (sort (for/fold ([ids '()]) ([ie (in-list raw-log-output)]) 
                                 (let* ([evt (indexed-fevent-fevent ie)]
                                        [procid (future-event-process-id evt)])
                                   (if (member procid ids)
                                       ids 
                                       (cons procid ids)))) 
                               <)]) 
    (for/vector ([procid (in-list unique-proc-ids)])
      (for/vector ([e (in-list raw-log-output)] 
                   #:when (eq? procid (future-event-process-id (indexed-fevent-fevent e)))) 
        e))))
  
;;build-trace : (listof indexed-fevent) -> trace
(define (build-trace log-output) 
  (define data (organize-output log-output))
  (define-values (start-time end-time unique-fids nblocks nsyncs) 
    (for/fold ([start-time #f] 
               [end-time #f] 
               [unique-fids (set)]
               [nblocks 0] 
               [nsyncs 0]) ([ie (in-list log-output)]) 
      (let* ([evt (indexed-fevent-fevent ie)] 
             [fid (future-event-future-id evt)]
             [is-future-thread? (not (= (future-event-process-id evt) RT-THREAD-ID))])
        (values 
         (if start-time 
             (min start-time (future-event-time evt)) 
             (future-event-time evt)) 
         (if end-time 
             (max end-time (future-event-time evt))
             (future-event-time evt))
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
                       [fst-log-msg (indexed-fevent-fevent fst-ie)])
                  (process-timeline (future-event-process-id fst-log-msg) 
                                    i
                                    (future-event-time fst-log-msg) 
                                    (future-event-time (indexed-fevent-fevent 
                                                        (vector-ref proc-log-vec 
                                                                    (sub1 (vector-length proc-log-vec))))) 
                                    (for/list ([ie (in-vector proc-log-vec)]
                                               [j (in-naturals)])
                                      (let* ([evt (indexed-fevent-fevent ie)]
                                             [start (future-event-time evt)] 
                                             [pos (cond 
                                                   [(zero? j) (if (= j (sub1 (vector-length proc-log-vec))) 
                                                                  'singleton 
                                                                  'start)]
                                                   [(= j (sub1 (vector-length proc-log-vec))) 'end] 
                                                   [else 'interior])])
                                        (event (indexed-fevent-index ie) 
                                               start 
                                               (if (or (equal? pos 'end) (equal? pos 'singleton))
                                                   start 
                                                   (future-event-time (indexed-fevent-fevent 
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
  (define all-evts (sort (flatten (for/list ([tl (in-list tls)]) 
                                    (process-timeline-events tl))) 
                         (λ (a b) 
                           (< (event-index a) (event-index b)))))
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
  (for ([evt (in-list (filter (λ (e) (and (= (event-proc-id e) RT-THREAD-ID) 
                                              (or (equal? (event-type e) 'block) 
                                                  (equal? (event-type e) 'sync)))) 
                                  evts))])
    (define isblock (case (event-type evt) 
                      [(block) #t] 
                      [else #f]))
    (define ophash (if isblock block-hash sync-hash))
    (hash-update! ophash 
                  (event-prim-name evt) 
                  (λ (old) (add1 old)) 
                  1) 
    (hash-update! rt-hash 
                  (event-future-id evt) 
                  (λ (old) 
                    (let ([h (if isblock 
                                 (rtcall-info-block-hash old) 
                                 (rtcall-info-sync-hash old))]) 
                      (hash-update! h 
                                    (event-prim-name evt) 
                                    (λ (o) (add1 o)) 
                                    (λ () 1))) 
                    old) 
                  (λ ()
                    (let* ([ri (rtcall-info (event-future-id evt) (make-hash) (make-hash))] 
                           [h (if isblock 
                                 (rtcall-info-block-hash ri) 
                                 (rtcall-info-sync-hash ri))])
                      (hash-update! h 
                                    (event-prim-name evt) 
                                    (λ (o) (add1 o)) 
                                    (λ () 1)) 
                      ri))))
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
         [fevents (filter creation-event? (hash-ref future-timelines fid))]) 
    (for/list ([cevt (in-list fevents)]) 
      (node cevt 
            (build-creation-graph/private future-timelines cevt)))))

;;build-creation-graph : (uint -o-> (listof future-event)) -> node
(define (build-creation-graph future-timelines) 
  (define roots (filter creation-event? 
                        (hash-ref future-timelines #f))) 
  (define root-nodes (for/list ([root (in-list roots)]) 
                       (node root 
                             (build-creation-graph/private future-timelines root)))) 
  (node 'runtime-thread 
        root-nodes))