
(module frp-core mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           (lib "match.ss")
           "erl.ss"
           "heap.ss")
  
 
  
  
  
  ;;;;;;;;;;;;;
  ;; Globals ;;
  ;;;;;;;;;;;;;
  
  (define frtime-inspector (make-inspector))
  (print-struct #t)
  
  (define snap? (make-parameter #f))
  
  (define named-dependents (make-hash-table))
  
  (define frtime-version "0.3b -- Tue Nov 9 13:39:45 2004")
  
  (define (compose-continuation-mark-sets2 s1 s2)
    (append s1 s2))
  
  
  (define (my-ccm)
    (continuation-mark-set->list (current-continuation-marks) 'drscheme-debug-continuation-mark-key))
  
  ;;;;;;;;;;;;;;;;
  ;; Structures ;;
  ;;;;;;;;;;;;;;;;
  
  ; also models events, where 'value' is all the events that
  ; haven't yet occurred (more specifically, an event-cons cell whose
  ; tail is *undefined*)
  (define-values (struct:signal
                  make-signal
                  signal?
                  signal-value
                  signal-dependents
                  signal-stale?
                  signal-thunk
                  signal-depth
                  signal-continuation-marks
                  signal-parameterization
                  signal-producers
                  set-signal-value!
                  set-signal-dependents!
                  set-signal-stale?!
                  set-signal-thunk!
                  set-signal-depth!
                  set-signal-continuation-marks!
                  set-signal-parameterization!
                  set-signal-producers!)
    (let*-values ([(field-name-symbols)
                   (list 'value 'dependents 'stale? 'thunk
                         'depth 'continuation-marks 'parameterization 'producers)]
                  [(desc make-signal signal? acc mut)
                   (make-struct-type
                    'signal #f (length field-name-symbols) 0 #f null frtime-inspector
                    (lambda (fn . args)
                      (unregister #f fn) ; clear out stale dependencies from previous apps
                      (let* (; revisit error-reporting for switched behaviors
                             [ccm (my-ccm)]
                             [app-fun (lambda (cur-fn)
                                        (let ([res (apply cur-fn args)])
                                          (when (signal? res)
                                            (set-signal-continuation-marks! res ccm))
                                          res))])
                        (super-lift app-fun fn))))])
      (apply values
             desc
             make-signal
             signal?
             (append
              (build-list (length field-name-symbols)
                          (lambda (i) (make-struct-field-accessor acc i (list-ref field-name-symbols i))))
              (build-list (length field-name-symbols)
                          (lambda (i) (make-struct-field-mutator mut i (list-ref field-name-symbols i))))))))
  
  (define-syntax signal
    (let ([field-name-symbols (list 'value 'dependents 'stale? 'thunk
                                    'depth 'continuation-marks 'parameterization
                                    'producers)]
          [cert (syntax-local-certifier #t)])
      (list-immutable
       (cert #'struct:signal)
       (cert #'make-signal)
       (cert #'signal?)
       (apply list-immutable
              (map
               (lambda (fd)
                 (cert (datum->syntax-object
                        #'here
                        (string->symbol (format "signal-~a" fd)))))
               (reverse field-name-symbols)))
       (apply list-immutable
              (map
               (lambda (fd)
                 (cert (datum->syntax-object
                        #'here
                        (string->symbol (format "set-signal-~a!" fd)))))
               (reverse field-name-symbols)))
       #t)))
  
  (define (signal-custodian sig)
    (call-with-parameterization
     (signal-parameterization sig)
     current-cust))
  
  (define-struct ft-cust (signal constructed-sigs children))
  ;(define-struct non-scheduled (signal))
  (define make-non-scheduled identity)
  (define (non-scheduled? x) #f)
  (define (non-scheduled-signal x)
    (error 'non-scheduled-signal "should never be called"))
  
  (define current-cust
    (make-parameter #f))
  
  (define-struct multiple (values) frtime-inspector)
  
  (define-struct event-cons (head tail))
  (define econs make-event-cons)
  (define efirst event-cons-head)
  (define erest event-cons-tail)
  (define econs? event-cons?)
  (define set-efirst! set-event-cons-head!)
  (define set-erest! set-event-cons-tail!)
  
  (define-struct (signal:unchanged signal) () frtime-inspector)
  (define-struct (signal:compound signal:unchanged) (content copy) frtime-inspector)
  (define-struct (signal:switching signal:unchanged) (current trigger) frtime-inspector)
  (define-struct (signal:event signal) () frtime-inspector)
  
  ; an external event; contains a list of pairs
  ; (recip val), where val is passed to recip's thunk
  (define-struct external-event (recip-val-pairs))
  
  ; update the given signal at the given time
  (define-struct alarm (time signal))  
  
  (define extra-cont-marks (make-parameter #f))

  (define (effective-continuation-marks)
    (if (extra-cont-marks)
        (begin
          #;(thread (lambda () (raise (make-exn:fail
                                     "extra marks present!" (extra-cont-marks)))))
          (compose-continuation-mark-sets2
           (extra-cont-marks)
           (my-ccm)))
        (my-ccm)))
  
  ;; Simple Structure Combinators
  
  (define (event-receiver)
    (event-producer2
     (lambda (emit)
       (lambda the-args
         (when (cons? the-args)
           (emit (first the-args)))))))
  
  (define (event-producer2 proc . deps)
    (let* ([out (econs undefined undefined)]
           [proc/emit (proc
                       (lambda (val)
                         (set-erest! out (econs val undefined))
                         (set! out (erest out))
                         val))])
      (apply proc->signal (lambda the-args (apply proc/emit the-args) out) deps)))
  
  (define (build-signal ctor thunk producers)
    (let ([ccm (effective-continuation-marks)])
      ;(printf "*")
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
         ;(printf "~a custodians~n" (length custs))
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
         ;(printf "~a custodians~n" (length custs))
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
     
  (define (proc->signal:unchanged thunk . producers)
    (build-signal make-signal:unchanged thunk producers))
  
  ;; mutate! : compound num -> (any -> ())
  (define (procs->signal:compound ctor mutate! . args)
    (let ([ccm (effective-continuation-marks)])
      (do-in-manager
       (let* ([cust (current-cust)]
              [cust-sig (and cust (ft-cust-signal cust))]
              [value (apply ctor (map value-now/no-copy args))]
              #;[mutators
                 (foldl
                  (lambda (arg idx acc)
                    (if (signal? arg) ; behavior?
                        (cons (proc->signal
                               (let ([m (mutate! value idx)])
                                 (lambda ()
                                   (let ([v (value-now/no-copy arg)])
                                     (m v)
                                     'struct-mutator)))
                               arg) acc)
                        acc))
                  empty args (build-list (length args) identity))]
                [sig (make-signal:compound
                      undefined
                      empty
                      #f
                      (lambda () ;mutators
                        (let loop ([i 0] [args args] [val value])
                          (if (cons? args)
                              (let ([fd (value-now/no-copy (car args))])
                                ((mutate! value i) fd)
                                (loop (add1 i) (cdr args)
                                      (if (undefined? fd)
                                          undefined
                                          val)))
                              val)))
                      (add1 (apply max 0 (cons (safe-signal-depth cust-sig) (map safe-signal-depth args))))
                      ccm
                      (parameterize ([uncaught-exception-handler
                                      (lambda (exn) (exn-handler exn))]
                                     [extra-cont-marks ccm])
                        (current-parameterization))
                      (if cust-sig (cons cust-sig args) args)
                      (apply ctor args)
                      (lambda () (apply ctor (map value-now args))))])
         ;(printf "mutators = ~a~n" mutators)
         (when (cons? args)
           (register sig args))
         (when cust-sig
           (register (make-non-scheduled sig) cust-sig))
         (when cust
           (set-ft-cust-constructed-sigs! cust (cons (make-weak-box sig) (ft-cust-constructed-sigs cust))))
         (iq-enqueue sig)
         ;(printf "~n*made a compound [~a]*~n~n" (value-now/no-copy sig))
         sig))))
  
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Simple Signal Tools ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  (define (send-event rcvr val)
    (! man (make-external-event (list (list rcvr val)))))
  
  (define (send-synchronous-event rcvr val)
    (when (man?)
      (error 'send-synchronous-event "already in frtime engine (would deadlock)"))
    (! man (make-external-event (list (list rcvr val))))
    (do-in-manager ()))
  
  (define (send-synchronous-events rcvr-val-pairs)
    (when (man?)
      (error 'send-synchronous-events "already in frtime engine (would deadlock)"))
    (unless (ormap list? rcvr-val-pairs) (error "not list"))
    (unless (ormap signal? (map first rcvr-val-pairs)) (error "not signals"))
    (! man (make-external-event rcvr-val-pairs))
    (do-in-manager ()))


  ; set-cell! : cell[a] a -> void
  (define (set-cell! ref beh)
    (! man (make-external-event (list (list ((signal-thunk ref) #t) beh)))))
  
  
  (define-values (undefined undefined?)
    (let-values ([(desc make-undefined undefined? acc mut)
                  (make-struct-type
                   'undefined #f 0 0 #f null frtime-inspector
                   (lambda (fn . args) fn))])
      (values (make-undefined) undefined?)))
  
  
  (define (behavior? v)
    (and (signal? v) (not (event-cons? (signal-value v)))))
  
  (define (undef b)
    (match b
      [(and (? signal?)
            (= signal-value value))
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
  
  #;(define-syntax value-now/sync
      (syntax-rules ()
        [(_ beh ...)
         (begin
           (! man (list 'run-thunk/stabalized (self) (lambda () (list (value-now beh) ...))))
           (receive [('val v) v]
                    [('exn e) (raise e)]))]))
  
  
  
  (define (extract k evs)
    (if (cons? evs)
        (let ([ev (first evs)])
          (if (or (eq? ev undefined) (undefined? (erest ev)))
              (extract k (rest evs))
              (begin
                (let ([val (efirst (erest ev))])
                  (set-first! evs (erest ev))
                  (k val)))))))
  
  
  (define (kill-signal sig)
    ;(printf "killing~n")
    (for-each
     (lambda (prod)
       (unregister sig prod))
     (signal-producers sig))
    (set-signal-thunk! sig (lambda _ 'really-dead))
    (set-signal-value! sig 'dead)
    (set-signal-dependents! sig empty)
    (set-signal-producers! sig empty)
    #;(for-each
     (lambda (c)
       (set-ft-cust-constructed-sigs!
        c
        (filter (lambda (wbox)
                   (cond
                     [(weak-box-value wbox) => (lambda (v) (not (eq? sig v)))]
                     [else (begin #;(printf "empty weak box~n") #f)]))
                (ft-cust-constructed-sigs c))))
     (signal-custodians sig)))
  
  
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Dataflow Graph Maintenance ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  (define (fix-streams streams args)
    (if (empty? streams)
        empty
        (cons
         (if (undefined? (first streams))
             (let ([stream (signal-value (first args))])
               stream
               #;(if (undefined? stream)
                     stream
                     (if (equal? stream (econs undefined undefined))
                         stream
                         (econs undefined stream))))
             (first streams))
         (fix-streams (rest streams) (rest args)))))
  
  (define (event-forwarder sym evt f+l)
    (let ([proc (lambda (emit)
                  (lambda (the-event)
                    (for-each (lambda (tid) 
                                (! tid (list 'remote-evt sym the-event))) (rest f+l))))]
          
          [args (list evt)])
      (let* ([out (econs undefined undefined)]
             [proc/emit (proc
                         (lambda (val)
                           (set-erest! out (econs val undefined))
                           (set! out (erest out))
                           val))]
             [streams (map signal-value args)]
             [thunk (lambda ()
                      (when (ormap undefined? streams)
                        ;(fprintf (current-error-port) "had an undefined stream~n")
                        (set! streams (fix-streams streams args)))
                      (let loop ()
                        (extract (lambda (the-event) (proc/emit the-event) (loop))
                                 streams))
                      (set! streams (map signal-value args))
                      out)])
        (apply proc->signal thunk args))))
  
  
  
  (define (safe-signal-depth v)
    (cond
      [(signal? v) (signal-depth v)]
      [(non-scheduled? v) (signal-depth (non-scheduled-signal v))]
      [0]))
  
  
  ; *** will have to change significantly to support depth-guided recomputation ***
  ; Basically, I'll have to check that I'm not introducing a cycle.
  ; If there is no cycle, then I simply ensure that inf's depth is at least one more than
  ; sup's.  If this requires an increase to inf's depth, then I need to propagate the
  ; new depth to inf's dependents.  Since there are no cycles, this step is guaranteed to
  ; terminate.  When checking for cycles, I should of course stop when I detect a pre-existing
  ; cycle.
  ; If there is a cycle, then 'inf' has (and retains) a lower depth than 'sup' (?), which
  ; indicates the cycle.  Importantly, 'propagate' uses the external message queue whenever
  ; a dependency crosses an inversion of depth.
  (define (fix-depths inf sup)
    (let help ([inf inf] [sup sup] [mem empty])
      (if (memq sup mem)
          (send-event exceptions (list (make-exn:fail "tight cycle in dataflow graph" (signal-continuation-marks sup))
                                       sup))
          (when (<= (safe-signal-depth inf)
                    (safe-signal-depth sup))
            (set-signal-depth! inf (add1 (safe-signal-depth sup)))
            (for-each
             (lambda (dep) (help dep inf (cons sup mem)))
             (foldl (lambda (wb acc)
                      (match (weak-box-value wb)
                        [(and sig (? signal?)) (cons sig acc)]
                        [(and (? non-scheduled?) (= non-scheduled-signal sig)) (cons sig acc)]
                        [_ acc]))
                    empty (signal-dependents inf)))))))
  
  
  (define-values (iq-enqueue iq-dequeue iq-empty? iq-resort)
    (let* ([depth
            (lambda (msg)
              (if (signal? msg) 
                  (signal-depth msg)
                  (signal-depth (first msg))))]
           [heap (make-heap
                  (lambda (b1 b2) (< (depth b1) (depth b2)))
                  eq?)])
      (values
       (lambda (b) (heap-insert heap b))
       (lambda () (heap-pop heap))
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
              (lambda () (match (heap-pop heap) [(_ beh) (weak-box-value beh)]))
              (lambda () (match (heap-peak heap) [(ms _) ms]))
              (lambda () (heap-empty? heap)))))
  
  (define (schedule-alarm ms beh)
    (when (> ms 1073741824)
      (set! ms (- ms 2147483647)))
    (if (eq? (self) man)
        (alarms-enqueue ms beh)
        (! man (make-alarm ms beh))))
  
  
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;
  ;; Manager Helpers ;;
  ;;;;;;;;;;;;;;;;;;;;;
  
  (define man?
    (opt-lambda ([v (self)])
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
             (receive [('vals . vs) (apply values vs)]
                      [('exn e) (raise e)])))]))
  
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
             (receive [('vals .  vs) (apply values vs)]
                      [('exn e) (raise e)])))]))
  
  (define (register inf sup)
    (do-in-manager
     (match sup
       [(and (? signal?)
             (= signal-dependents dependents))
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
             (= signal-dependents dependents))
        (set-signal-dependents!
         sup
         (filter (lambda (a)
                   (let ([v (weak-box-value a)])
                     (nor (eq? v inf)
                          (eq? v #f))))
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
                  [_ (when (current-cust) (set-ft-cust-children! (current-cust) (cons cust (ft-cust-children (current-cust)))))]
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
                          #;(for-each kill-signal
                                    (filter identity
                                            (map weak-box-value (ft-cust-constructed-sigs cust))))
                          (unregister rtn (unbox current))
                          (set-box! current (pfun (value-now/no-copy bhvr)))
                          (register rtn (unbox current))
                          ;; keep rtn's producers up-to-date
                          (set-car! (signal-producers rtn) (unbox current))
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
           [(and dep (? signal?) (= signal-stale? #f))
            (set-signal-stale?! dep #t)
            ; If I'm crossing a "back" edge (one potentially causing a cycle),
            ; then I send a message.  Otherwise, I add to the internal
            ; priority queue.
            (if (< depth (signal-depth dep))
                (iq-enqueue dep)
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
            (= signal-value value)
            (= signal-thunk thunk)
            (= signal-parameterization params))
       (set-signal-stale?! b #f)
       (let ([new-value (call-with-parameterization
                         params
                         thunk)])
         (if (or (signal:unchanged? b)
                 (not (or (boolean? new-value)
                          (symbol? new-value)
                          (number? new-value)
                          (string? new-value)))
                 (not (eq? value new-value)))
           (begin
             #;(if (signal? new-value)
                 (raise (make-exn:fail
                         "signal from update thunk!!!"
                         (signal-continuation-marks b))))
             #;(printf "~n[~a]: ~a --> ~a~n" (cond
                                         [(signal:switching? b) 'signal:switching]
                                         [(signal:compound? b) 'signal:compound]
                                         [(signal:unchanged? b) 'signal:unchanged]
                                         [else 'signal])
                     value new-value)
             (set-signal-value! b new-value)
             (propagate b))
           #;(parameterize ([print-struct #f])
             (printf "~a ... ~a (~a)~n" value new-value b))))]
      [_ (void)]))
  
  (define (update1 b a)
    (match b
      [(and (? signal?)
            (= signal-value value)
            (= signal-thunk thunk))
       (set-signal-stale?! b #f)
       (let ([new-value (thunk a)])
         (when (not (equal? value new-value))
           (set-signal-value! b new-value)
           (propagate b)))]
      [_ (void)]))
  
  (define (signal-count)
    (! man `(stat ,(self)))
    (receive [n n]))

  (define exn-handler (lambda (exn) (raise exn)))
  
  ;;;;;;;;;;;;;
  ;; Manager ;;
  ;;;;;;;;;;;;;
  
  ;; the manager of all signals
  (define man
    (spawn/name
     'frtime-heart
     (let* ([named-providers (make-hash-table)] 
            [cur-beh #f]
            [signal-cache (make-hash-table 'weak)]
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
                            (when (and cur-beh
                                       #;(not (undefined? (signal-value cur-beh))))
                              ;(when (empty? (continuation-mark-set->list
                               ;              (exn-continuation-marks exn) 'frtime))
                                (set! exn (make-exn:fail
                                           (exn-message exn)
                                           (compose-continuation-mark-sets2
                                            (signal-continuation-marks
                                             cur-beh)
                                            (exn-continuation-marks exn))));)
                              ;(raise exn)
                              (iq-enqueue (list exceptions (list exn cur-beh)))
                              (when (behavior? cur-beh)
                                (undef cur-beh)
                                #;(kill-signal cur-beh)))
                            (outer))])
           ;; (set! exn-handler (current-exception-handler)) <-- FIXME!
           (let inner ()
             
             ;; process external messages until there is an internal update
             ;; or an expired alarm
             (let loop ()
               (receive [after (cond
                                 [(not (iq-empty?)) 0]
                                 [(not (alarms-empty?)) (- (alarms-peak-ms)
                                                           (current-milliseconds))]
                                 [else #f])
                               (void)]
                        [(? signal? b)
                         (iq-enqueue b)
                         (loop)]
                        [($ external-event recip-val-pairs)
                         (for-each iq-enqueue recip-val-pairs)
                         (loop)]
                        [($ alarm ms beh)
                         (schedule-alarm ms beh)
                         (loop)]
                        [('run-thunk rtn-pid thunk)
                         (begin
                           (do-and-queue rtn-pid thunk)
                           (loop))]
                        
                        
                        ;; !Experimental!
                        ;; queues thunks to be evaluated after this round of computation,
                        ;; but before the next round
                        
                        [('run-thunk/stabilized rtn-pid thunk)
                         (begin
                           (set! thunks-to-run (cons (list rtn-pid thunk) thunks-to-run))
                           (loop))]
                        
                        
                        [('stat rtn-pid)
                         (let ([x 0])
                           (hash-table-for-each signal-cache (lambda (k v) 
                                                               (if k (set! x (add1 x)))))
                           (! rtn-pid x))]
                        
                        [('bind sym evt)
                         (let ([forwarder+listeners (cons #f empty)])
                           (set-car! forwarder+listeners
                                     (event-forwarder sym evt forwarder+listeners))
                           (hash-table-put! named-providers sym forwarder+listeners))
                         (loop)]
                        [('remote-reg tid sym)
                         (let ([f+l (hash-table-get named-providers sym)])
                           (when (not (member tid (rest f+l)))
                             (set-rest! f+l (cons tid (rest f+l)))))
                         (loop)]
                        [('remote-evt sym val)
                         (iq-enqueue
                          (list (hash-table-get named-dependents sym (lambda () dummy)) val))
                         (loop)]
                        [msg
                         (fprintf (current-error-port)
                                  "frtime engine: msg not understood: ~a~n"
                                  msg)
                         (loop)]))
             
             ;; enqueue expired timers for execution
             (let loop ()
               (unless (or (alarms-empty?)
                           (< (current-milliseconds)
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
                   [(b val)
                    (set! cur-beh b)
                    (update1 b val)
                    (set! cur-beh #f)]
                   [b
                    (set! cur-beh b)
                    (update0 b)
                    (hash-table-get signal-cache b (lambda () (hash-table-put! signal-cache b #t)))
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
             
             (inner)))))))
  
  (define exceptions
    (event-receiver)) 
  
  (define dummy (proc->signal void)) 
  
  (provide (all-defined)))