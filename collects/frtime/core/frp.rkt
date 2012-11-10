#lang racket/base

(require racket/function
         racket/list
         racket/match
         racket/contract
         "contract.rkt"
         "erl.rkt"
         "heap.rkt")

;;;;;;;;;;;;;
;; Globals ;;
;;;;;;;;;;;;;

;; the current logical time step
(define logical-time (box 0))
(define (current-logical-time)
  (unbox logical-time))

(define frtime-inspector (make-inspector))
(print-struct #t)

(define snap? (make-parameter #f))

(define named-dependents (make-hash))

(define (compose-continuation-mark-sets2 s1 s2)
  s2)

(define (my-ccm)
  (current-continuation-marks)
  ; XXX What is this?
  #;(continuation-mark-set->list (current-continuation-marks) 'drscheme-debug-continuation-mark-key))

;;;;;;;;;;;;;;;;
;; Structures ;;
;;;;;;;;;;;;;;;;

; also models events, where 'value' is all the events that
; haven't yet occurred (more specifically, an event-cons cell whose
; tail is *undefined*)
(define-struct signal ([value #:mutable] 
                       [dependents #:mutable]
                       [stale? #:mutable]
                       [thunk #:mutable]
                       [depth #:mutable]
                       [continuation-marks #:mutable]
                       parameterization
                       [producers #:mutable])
  #:inspector frtime-inspector
  #:property prop:procedure
  (lambda (fn . args)
    (unregister #f fn) ; clear out stale dependencies from previous apps
    (let* (; revisit error-reporting for switched behaviors
           [ccm (my-ccm)]
           [app-fun (lambda (cur-fn)
                      (let ([res (apply cur-fn args)])
                        (when (signal? res)
                          (set-signal-continuation-marks! res ccm))
                        res))])
      (super-lift app-fun fn))))  

; XXX Remove
#;(define (signal-custodian sig)
  (call-with-parameterization
   (signal-parameterization sig)
   current-cust))

(define-struct ft-cust (signal constructed-sigs children) #:mutable)

(define make-non-scheduled identity)
(define (non-scheduled? x) #f)
(define (non-scheduled-signal x)
  (error 'non-scheduled-signal "should never be called"))

(define current-cust
  (make-parameter #f))

(define-struct multiple (values) 
  #:inspector frtime-inspector)

(define-struct event-set (time events))
(define (make-events-now events)
  (make-event-set (current-logical-time) events))

(define-struct (signal:unchanged signal) () 
  #:inspector frtime-inspector)
(define-struct (signal:compound signal:unchanged) (content copy) 
  #:inspector frtime-inspector)
(define-struct (signal:switching signal:unchanged) (current trigger) 
  #:inspector frtime-inspector)
(define-struct (signal:event signal) () 
  #:inspector frtime-inspector)

; an external event; contains a list of pairs
; (recip val), where val is passed to recip's thunk
(define-struct external-event (recip-val-pairs))

; update the given signal at the given time
(define-struct alarm (time signal))  

(define extra-cont-marks (make-parameter #f))

(define (effective-continuation-marks)
  (if (extra-cont-marks)
      (begin
        ; XXX Remove
        #;(thread (lambda () (raise (make-exn:fail
                                     "extra marks present!" (extra-cont-marks)))))
        (compose-continuation-mark-sets2
         (my-ccm)
         (extra-cont-marks)
         ))
      (my-ccm)))

;; Simple Structure Combinators

(define (event-receiver)
  (event-producer2
   (lambda (emit)
     (lambda the-args
       (if (cons? the-args)
           (emit (first the-args))
           (make-events-now empty))))))

(define (event-producer2 proc . deps)
  (let* ([result (apply proc->signal (lambda args (make-events-now empty)) deps)]
         [proc/emit (proc
                     (lambda (val)
                       (let ([old-value (signal-value result)])
                         (make-events-now
                          (if (and (event-set? old-value)
                                   (= (current-logical-time) (event-set-time old-value)))
                              (append (event-set-events old-value) (list val))
                              (list val))))))])
    (set-signal-thunk! result proc/emit)
    result))

(define (build-signal ctor thunk producers)
  (let ([ccm (effective-continuation-marks)])
    (do-in-manager
     (let* ([cust (current-cust)]
            [cust-sig (and cust (ft-cust-signal cust))]
            [sig (ctor
                  undefined empty #t thunk
                  (add1 (apply max 0 (cons (safe-signal-depth cust-sig) (map safe-signal-depth producers))))
                  ccm
                  (parameterize ([uncaught-exception-handler
                                  (lambda (exn) (exn-handler exn))]
                                 [extra-cont-marks ccm])
                    (current-parameterization))
                  (if cust-sig (append producers (list cust-sig)) producers))])
       (when (cons? producers)
         (register sig producers))
       (when cust-sig
         (register (make-non-scheduled sig) cust-sig))
       (when cust
         (set-ft-cust-constructed-sigs! cust (cons (make-weak-box sig) (ft-cust-constructed-sigs cust))))
       (iq-enqueue sig)
       sig))))

(define (proc->signal:switching thunk current-box trigger . producers)
  (let ([ccm (effective-continuation-marks)])
    (do-in-manager
     (let* ([cust (current-cust)]
            [cust-sig (and cust (ft-cust-signal cust))]
            [sig (make-signal:switching
                  undefined empty #t thunk
                  (add1 (apply max 0 (cons (safe-signal-depth cust-sig) (map safe-signal-depth producers))))
                  ccm
                  (parameterize ([uncaught-exception-handler
                                  (lambda (exn) (exn-handler exn))]
                                 [extra-cont-marks ccm])
                    (current-parameterization))
                  (if cust-sig (cons cust-sig producers) producers)
                  current-box
                  trigger)])
       (when (cons? producers)
         (register sig producers))
       (when cust-sig
         (register (make-non-scheduled sig) cust-sig))
       (when cust
         (set-ft-cust-constructed-sigs!
          cust (cons (make-weak-box sig) (ft-cust-constructed-sigs cust))))
       (iq-enqueue sig)
       sig))))

(define (proc->signal thunk . producers)
  (build-signal make-signal thunk producers))

; XXX Remove
#;(define ht (make-hash))
#;(define (proc->signal/dont-gc-unless other-val thunk . producers)
  (let ([result (build-signal make-signal thunk producers)])
    (hash-set! ht other-val result)
    result))

(define (proc->signal:unchanged thunk . producers)
  (build-signal make-signal:unchanged thunk producers))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple Signal Tools ;;
;;;;;;;;;;;;;;;;;;;;;;;;;  

(define (send-event rcvr val)
  (! man (make-external-event (list (list rcvr val)))))

; XXX move man check into contract
(define (send-synchronous-event rcvr val)
  (when (man?)
    (error 'send-synchronous-event "already in frtime engine (would deadlock)"))
  (! man (make-external-event (list (list rcvr val))))
  (do-in-manager '()))

; XXX move man check into contract
(define (send-synchronous-events rcvr-val-pairs)
  (when (man?)
    (error 'send-synchronous-events "already in frtime engine (would deadlock)"))
  (! man (make-external-event rcvr-val-pairs))
  (do-in-manager '()))

; set-cell! : cell[a] a -> void
(define (set-cell! ref beh)
  (if (man?)
      (iq-enqueue (list ((signal-thunk ref) #t) beh))
      (! man (make-external-event (list (list ((signal-thunk ref) #t) beh))))))

(define-values (undefined undefined?)
  (let ()
    (define-struct undefined ()
      #:inspector frtime-inspector
      #:property prop:procedure (lambda (fn . args) fn))
    (values (make-undefined) undefined?)))

(define (behavior? v)
  (and (signal? v) (not (event-set? (signal-value v)))))

(define (undef b)
  (match b
    [(and (? signal?)
          (app signal-value value))
     (set-signal-stale?! b #f)
     (when (not (undefined? value))
       (set-signal-value! b undefined)
       (propagate b))]
    [_ (void)]))

(define (multiple->values v)
  (if (multiple? v)
      (apply values (multiple-values v))
      v))

(define (values->multiple proc)
  (call-with-values
   proc
   (case-lambda
     [(v) v]
     [vals (make-multiple vals)])))

; value-now : signal[a] -> a
(define (value-now val)
  ;(multiple->values
  (cond
    [(signal:compound? val) ((signal:compound-copy val))]
    [(signal:switching? val) (value-now (unbox (signal:switching-current val)))]
    [(signal? val) (signal-value val)]
    [else val]));)

(define (value-now/no-copy val)
  ;(multiple->values
  (cond
    [(signal:switching? val) (value-now/no-copy (unbox (signal:switching-current val)))]
    [(signal? val) (signal-value val)]
    [else val]));)

;; given a list, will return a list of their value-nows that will agree
(define (value-now/sync . sigs)
  (do-in-manager-after
   (apply values (map value-now sigs))))

(define (kill-signal sig)
  (for-each
   (lambda (prod)
     (unregister sig prod))
   (signal-producers sig))
  (set-signal-thunk! sig (lambda _ 'really-dead))
  (set-signal-value! sig 'dead)
  (set-signal-dependents! sig empty)
  (set-signal-producers! sig empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dataflow Graph Maintenance ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (safe-signal-depth v)
  (cond
    [(signal? v) (signal-depth v)]
    [(non-scheduled? v) (signal-depth (non-scheduled-signal v))]
    [else 0]))

(define fix-depths
  (lambda (inf sup [mem empty])
    (if (memq sup mem)
        (send-event exceptions (list (make-exn:fail "tight cycle in dataflow graph" (signal-continuation-marks sup))
                                     sup))
        (when (<= (safe-signal-depth inf)
                  (safe-signal-depth sup))
          (set-signal-depth! inf (add1 (safe-signal-depth sup)))
          (for-each
           (lambda (dep) (fix-depths dep inf (cons sup mem)))
           (foldl (lambda (wb acc)
                    (match (weak-box-value wb)
                      [(and sig (? signal?)) (cons sig acc)]
                      [(and (? non-scheduled?) (app non-scheduled-signal sig)) (cons sig acc)]
                      [_ acc]))
                  empty (signal-dependents inf)))))))

(define-values (iq-enqueue iq-dequeue iq-empty? iq-resort)
  (let* ([depth
          (lambda (msg)
            (let ([msg (if (weak-box? msg) (weak-box-value msg) msg)])
              (cond
                [(cons? msg) (signal-depth (first msg))]
                [(signal? msg) (signal-depth msg)]
                [else 0])))]
         [heap (make-heap
                (lambda (b1 b2) (< (depth b1) (depth b2)))
                eq?)])
    (values
     (lambda (b) (heap-insert heap b))
     (lambda () (let ([v (heap-pop heap)])
                  (if (weak-box? v)
                      (weak-box-value v)
                      v)))
     (lambda () (heap-empty? heap))
     (lambda () (let loop ([elts empty])
                  (if (heap-empty? heap)
                      (let loop ([elts elts])
                        (when (cons? elts)
                          (heap-insert heap (first elts))
                          (loop (rest elts))))
                      (loop (cons (heap-pop heap) elts))))))))

(define-values (alarms-enqueue alarms-dequeue-beh alarms-peak-ms alarms-empty?)
  (let ([heap (make-heap (lambda (a b) (< (first a) (first b))) eq?)])
    (values (lambda (ms beh) (heap-insert heap (list ms (make-weak-box beh))))
            (lambda () (match (heap-pop heap) [(list _ beh) (weak-box-value beh)]))
            (lambda () (match (heap-peak heap) [(list ms _) ms]))
            (lambda () (heap-empty? heap)))))

(define (schedule-alarm ms beh)
  (if (eq? (self) man)
      (alarms-enqueue ms beh)
      (! man (make-alarm ms beh))))

;;;;;;;;;;;;;;;;;;;;;
;; Manager Helpers ;;
;;;;;;;;;;;;;;;;;;;;;

(define man?
  (lambda ([v (self)])
    (eq? v man)))

(define-syntax do-in-manager
  (syntax-rules ()
    [(_ expr ...)
     (if (man?)
         (begin expr ...)
         (begin
           (! man (list 'run-thunk (self)
                        (let ([params (current-parameterization)])
                          (lambda ()
                            (call-with-parameterization
                             params
                             (lambda () expr ...))))))
           (receive [(list-rest 'vals vs) (apply values vs)]
                    [(list 'exn e) (raise e)])))]))

(define-syntax do-in-manager-after
  (syntax-rules ()
    [(_ expr ...)
     (if (man?)
         (begin expr ...)
         (begin
           (! man (list 'run-thunk/stabilized (self)
                        (let ([params (current-parameterization)])
                          (lambda ()
                            (call-with-parameterization
                             params
                             (lambda () expr ...))))))
           (receive [(list-rest 'vals  vs) (apply values vs)]
                    [(list 'exn e) (raise e)])))]))

(define (register inf sup)
  (do-in-manager
   (match sup
     [(and (? signal?)
           (app signal-dependents dependents))
      (set-signal-dependents!
       sup
       (cons (make-weak-box inf) dependents))
      (fix-depths inf sup)]
     [(? list?) (for-each (lambda (sup1) (register inf sup1)) sup)]
     [_ (void)])
   inf))

(define (unregister inf sup)
  (do-in-manager
   (match sup
     [(and (? signal?)
           (app signal-dependents dependents))
      (set-signal-dependents!
       sup
       (filter (lambda (a)
                 (let ([v (weak-box-value a)])
                   (not (or (eq? v inf)
                            (eq? v #f)))))
               dependents))]
     [_ (void)])))

(define (cust-killall! cust)
  (let loop ([sigs (ft-cust-constructed-sigs cust)])
    (when (cons? sigs)
      (cond
        [(weak-box-value (first sigs)) => kill-signal]
        [else (void)])
      (loop (rest sigs))))
  (for-each cust-killall! (ft-cust-children cust)))

(define (super-lift fun bhvr)
  (if (behavior? bhvr)
      (parameterize ([extra-cont-marks
                      (effective-continuation-marks)])
        (do-in-manager
         (let* ([cust (make-ft-cust (void) empty empty)]
                [_ (cond
                     [(current-cust)
                      => (lambda (c) (set-ft-cust-children! c (cons cust (ft-cust-children c))))]
                     [else (void)])]
                [pfun (lambda (b)
                        (parameterize ([current-cust cust])
                          (fun b)))]
                [current (box undefined)])
           (letrec ([custodian-signal
                     (proc->signal:unchanged
                      (lambda ()
                        (cust-killall! cust)
                        (set-ft-cust-constructed-sigs! cust empty)
                        (set-ft-cust-children! cust empty)
                        (unregister rtn (unbox current))
                        (set-box! current (pfun (value-now/no-copy bhvr)))
                        (register rtn (unbox current))
                        ;; keep rtn's producers up-to-date
                        (set-signal-producers! rtn (cons (unbox current)
                                                         (cdr (signal-producers rtn))))
                        (iq-resort)
                        'custodian)
                      bhvr)]
                    [rtn (proc->signal:switching
                          (lambda () custodian-signal (value-now/no-copy (unbox current)))
                          current custodian-signal undefined bhvr custodian-signal)])
             (set-ft-cust-signal! cust custodian-signal)
             rtn))))
      (fun bhvr)))

(define (propagate b)
  (let ([empty-boxes 0]
        [dependents (signal-dependents b)]
        [depth (signal-depth b)])
    (for-each
     (lambda (wb)
       (match (weak-box-value wb)
         [(and dep (? signal?) (app signal-stale? #f))
          (set-signal-stale?! dep #t)
          ; If I'm crossing a "back" edge (one potentially causing a cycle),
          ; then I send a message.  Otherwise, I add to the internal
          ; priority queue.
          (if (< depth (signal-depth dep))
              (iq-enqueue wb)
              (! man dep))]
         [_
          (set! empty-boxes (add1 empty-boxes))]))
     dependents)
    (when (> empty-boxes 9)
      (set-signal-dependents!
       b
       (filter weak-box-value dependents)))))

(define (update0 b)
  (match b
    [(and (? signal?)
          (app signal-value value)
          (app signal-thunk thunk)
          (app signal-parameterization params))
     (set-signal-stale?! b #f)
     (let ([new-value (call-with-parameterization
                       params
                       thunk)])
       (when (or (signal:unchanged? b)
                 (and (not (eq? value new-value))
                      (or (not (event-set? new-value)) (cons? (event-set-events new-value))
                          (not (event-set? value)))))
         (begin
           (set-signal-value! b new-value)
           (propagate b))))]
    [_ (void)]))

(define (update1 b a)
  (match b
    [(and (? signal?)
          (app signal-value value)
          (app signal-thunk thunk))
     (set-signal-stale?! b #f)
     (let ([new-value (thunk a)])
       (when (not (equal? value new-value))
         (set-signal-value! b new-value)
         (propagate b)))]
    [_ (void)]))

(define (signal-count)
  (! man `(stat ,(self)))
  (receive [n n]))

(define (hash-table-size ht)
  (let ([x 0])
    (hash-for-each ht (lambda (k v) 
                        (when k (set! x (add1 x)))))
    x))

(define exn-handler (lambda (exn) (raise exn)))

;;;;;;;;;;;;;
;; Manager ;;
;;;;;;;;;;;;;

;; the manager of all signals
(define man
  (spawn/name
   'frtime-heart
   (let* ([named-providers (make-hash)] 
          [cur-beh #f]
          [signal-cache (make-weak-hash)]
          [last-known-signal-count 50]
          [notifications empty]
          
          ;; added for run-thunk/stablized
          [thunks-to-run empty]
          [do-and-queue (lambda (pid thnk)
                          (with-handlers
                              ([exn:fail? (lambda (exn)
                                            (set! notifications
                                                  (cons (list pid 'exn exn)
                                                        notifications)))])
                            (set! notifications
                                  (cons (list* pid 'vals (call-with-values thnk list))
                                        notifications))))])
     (let outer ()
       (with-handlers ([exn:fail?
                        (lambda (exn)
                          (when cur-beh
                            (set! exn (make-exn:fail
                                       (exn-message exn)
                                       (compose-continuation-mark-sets2
                                        (signal-continuation-marks
                                         cur-beh)
                                        (exn-continuation-marks exn))));)
                            (iq-enqueue (list exceptions (list exn cur-beh)))
                            (when (behavior? cur-beh)
                              (undef cur-beh)))
                          (outer))])
         (set! exn-handler (uncaught-exception-handler))
         (let inner ()
           
           ;; process external messages until there is an internal update
           ;; or an expired alarm
           (let loop ()
             (receive [after (cond
                               [(not (iq-empty?)) 0]
                               [(not (alarms-empty?)) (- (alarms-peak-ms)
                                                         (current-inexact-milliseconds))]
                               [else #f])
                             (void)]
                      [(? signal? b)
                       (iq-enqueue b)
                       (loop)]
                      [(struct external-event (recip-val-pairs))
                       (for-each iq-enqueue recip-val-pairs)
                       (loop)]
                      [(struct alarm (ms beh))
                       (schedule-alarm ms beh)
                       (loop)]
                      [(list 'run-thunk rtn-pid thunk)
                       (begin
                         (do-and-queue rtn-pid thunk)
                         (loop))]
                      
                      ;; !Experimental!
                      ;; queues thunks to be evaluated after this round of computation,
                      ;; but before the next round
                      
                      [(list 'run-thunk/stabilized rtn-pid thunk)
                       (begin
                         (set! thunks-to-run (cons (list rtn-pid thunk) thunks-to-run))
                         (loop))]
                      
                      [(list 'stat rtn-pid)
                       (! rtn-pid (hash-table-size signal-cache))]
                      
                      [(list 'remote-reg tid sym)
                       (let ([f+l (hash-ref named-providers sym)])
                         (when (not (member tid (mcdr f+l)))
                           (set-mcdr! f+l (cons tid (mcdr f+l)))))
                       (loop)]
                      [(list 'remote-evt sym val)
                       (iq-enqueue
                        (list (hash-ref named-dependents sym (lambda () dummy)) val))
                       (loop)]
                      [msg
                       (eprintf "frtime engine: msg not understood: ~a\n" msg)
                       (loop)]))
           
           ;; enqueue expired timers for execution
           (let loop ()
             (unless (or (alarms-empty?)
                         (< (current-inexact-milliseconds)
                            (alarms-peak-ms)))
               (let ([beh (alarms-dequeue-beh)])
                 (when (and beh (not (signal-stale? beh)))
                   (set-signal-stale?! beh #t)
                   (iq-enqueue beh)))
               (loop)))
           
           ;; process internal updates
           (let loop ()
             (unless (iq-empty?)
               (match (iq-dequeue)
                 [(list b val)
                  (set! cur-beh b)
                  (update1 b val)
                  (set! cur-beh #f)]
                 [b
                  (set! cur-beh b)
                  (update0 b)
                  (hash-ref signal-cache b (lambda () (hash-set! signal-cache b #t)))
                  (set! cur-beh #f)])
               (loop)))
           
           ;; do the run-thunk/stabalized; use existing notification mechanism
           (for-each (lambda (pair)
                       (do-and-queue (first pair) (second pair)))
                     thunks-to-run)
           
           
           (for-each (lambda (lst)
                       (! (first lst) (rest lst)))
                     notifications)
           
           (set! notifications empty)
           (set! thunks-to-run empty)
           
           (set-box! logical-time (add1 (unbox logical-time)))
           
           (inner)))))))

(define exceptions (event-receiver)) 

(define dummy (proc->signal void)) 

(provide do-in-manager
         do-in-manager-after)

(define thunk/c
  (unconstrained-domain-> any/c)) ; XXX Not really thunk
(define producers/c
  (listof any/c)) ; XXX bad
(define switching-current/c
  (box/c any/c)) ; XXX
(define switching-trigger/c
  any/c)

(provide/contract*
 ; Event Sets
 [make-events-now ((listof any/c) . -> . event-set?)] ; XXX Ugly contract
 [event-set? (any/c . -> . boolean?)]
 [event-set-time (event-set? . -> . number?)]
 [event-set-events (event-set? . -> . (listof any/c))] ; XXX Ugly contract
 ; Undefined
 [undefined undefined?]
 [undefined? (any/c . -> . boolean?)]
 ; Signals
 [proc->signal ((thunk/c) () #:rest producers/c . ->* . signal?)]
 [signal? (any/c . -> . boolean?)]
 [signal-value (signal? . -> . any/c)]
 [set-signal-value! (signal? any/c . -> . void)]
 [signal-depth (signal? . -> . exact-nonnegative-integer?)]
 [set-signal-depth! (signal? exact-nonnegative-integer? . -> . void)]
 [set-signal-producers! (signal? producers/c . -> . void)]
 [signal-thunk (signal? . -> . thunk/c)]
 [set-signal-thunk! (signal? thunk/c . -> . void)]
 [signal-count (-> exact-nonnegative-integer?)]
 ; Signal : Compound
 [signal:compound? (any/c . -> . boolean?)]
 [signal:compound-content (signal:compound? . -> . cons?)] ; XXX Ugly contract on codomain
 ; Signal : Switching
 [proc->signal:switching ((thunk/c switching-current/c switching-trigger/c) () #:rest producers/c . ->* . signal:switching?)]
 [signal:switching? (any/c . -> . boolean?)]
 [signal:switching-current (signal:switching? . -> . switching-current/c)]
 [signal:switching-trigger (signal:switching? . -> . switching-trigger/c)]
 ; Input queue
 [iq-enqueue (any/c . -> . void)] ; XXX Not sure what any/c should be
 [iq-resort (-> void)]
 ; Events
 [send-event (signal? any/c . -> . void)]
 [send-synchronous-event (signal? any/c . -> . void)]
 [send-synchronous-events ((listof (cons/c signal? (listof any/c))) . -> . void)]
 [event-receiver (-> signal?)]
 [event-producer2 ((thunk/c) () #:rest producers/c . ->* . signal?)]
 ; Other
 [register (signal? any/c . -> . void)] ; XXX Ugly contract
 [unregister (signal? any/c . -> . void)] ; XXX Ugly contract
 [current-logical-time (-> exact-nonnegative-integer?)]
 [snap? (parameter/c boolean?)]
 [super-lift ((any/c . -> . any/c) any/c . -> . any/c)] ; XXX Ugly contract
 [behavior? (any/c . -> . boolean?)]
 [value-now (any/c . -> . any/c)] ; XXX Should this return (not/c signal?) and why not take signal?
 [value-now/sync (() () #:rest (listof any/c) . ->* . any)] ; XXX See above + not matching number of values returned with number of signals
 [value-now/no-copy (any/c . -> . any/c)] ; XXX Should this return (not/c signal?) and why not take signal?
 [safe-signal-depth (any/c . -> . exact-nonnegative-integer?)] ; XXX Ugly contract
 [schedule-alarm (number? signal? . -> . void)]
 [set-cell! (signal? any/c . -> . void)] ; XXX What is any/c?
 [exceptions signal?])
