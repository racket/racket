#lang racket/base
(require (for-syntax racket/base
                     (only-in racket/list first second last-pair empty empty?))
         (only-in racket/list first second cons? empty empty? rest last-pair)
         (only-in racket/function identity)
         data/queue
         (only-in frtime/core/frp super-lift undefined undefined? behavior? do-in-manager-after do-in-manager proc->signal set-signal-thunk! register unregister 
                  signal? signal-depth signal:switching? signal-value value-now signal:compound? signal:compound-content signal:switching-current signal:switching-trigger 
                  set-cell! snap? iq-enqueue value-now/no-copy event-receiver event-set? proc->signal:switching set-signal-producers! set-signal-depth! safe-signal-depth 
                  make-events-now iq-resort event-set-events current-logical-time event-set-time event-producer2 schedule-alarm value-now/sync set-signal-value! signal-thunk
                  send-event exceptions send-synchronous-event send-synchronous-events signal-count))

(define nothing (void));(string->uninterned-symbol "nothing"))

(define (nothing? v) (eq? v nothing))

(define-syntax define-reactive
  (syntax-rules ()
    [(_ name expr)
     (define name
       (let ([val (parameterize ([snap? #f])
                    expr)])
         (lambda () (deep-value-now val empty))))]))

(define (deep-value-now obj table)
  (cond
    [(assq obj table) => second]
    [(behavior? obj)
     (deep-value-now (signal-value obj) (cons (list obj (signal-value obj)) table))]
    [(cons? obj)
     (let* ([result (cons #f #f)]
            [new-table (cons (list obj result) table)]
            [car-val (deep-value-now (car obj) new-table)]
            [cdr-val (deep-value-now (cdr obj) new-table)])
       (if (and (eq? car-val (car obj))
                (eq? cdr-val (cdr obj)))
           obj
           (cons car-val cdr-val)))]
    ; won't work in the presence of super structs or immutable fields
    [(struct? obj)
     (let*-values ([(info skipped) (struct-info obj)]
                   [(name init-k auto-k acc mut! immut sup skipped?) (struct-type-info info)]
                   [(ctor) (struct-type-make-constructor info)]
                   [(indices) (build-list init-k identity)]
                   [(result) (apply ctor (build-list init-k (lambda (i) #f)))]
                   [(new-table) (cons (list obj result) table)]
                   [(elts) (build-list init-k (lambda (i)
                                                (deep-value-now (acc obj i) new-table)))])
       (if (andmap (lambda (i e) (eq? (acc obj i) e)) indices elts)
           obj
           (begin
             (for-each (lambda (i e) (mut! result i e)) indices elts)
             result)))]
    [(vector? obj)
     (let* ([len (vector-length obj)]
            [indices (build-list len identity)]
            [result (build-vector len (lambda (_) #f))]
            [new-table (cons (list obj result) table)]
            [elts (build-list len (lambda (i)
                                    (deep-value-now (vector-ref obj i) new-table)))])
       (if (andmap (lambda (i e) (eq? (vector-ref obj i) e)) indices elts)
           obj
           (begin
             (for-each (lambda (i e) (vector-set! result i e)) indices elts)
             result)))]
    [else obj]))

#;(define deep-value-now
    (case-lambda
      [(obj) (deep-value-now obj empty)]
      [(obj table)
       (cond
         [(assq obj table) => second]
         [(behavior? obj)
          (deep-value-now (signal-value obj) (cons (list obj (signal-value obj)) table))]
         [(event? obj)
          (signal-value obj)]
         [(cons? obj)
          (let* ([result (cons #f #f)]
                 [new-table (cons (list obj result) table)]
                 [car-val (deep-value-now (car obj) new-table)]
                 [cdr-val (deep-value-now (cdr obj) new-table)])
            (cons car-val cdr-val))]
         [(struct? obj)
          (let*-values ([(info skipped) (struct-info obj)]
                        [(name init-k auto-k acc mut immut sup skipped?) (struct-type-info info)]
                        [(ctor) (struct-type-make-constructor info)])
            (apply ctor (build-list (+ auto-k init-k)
                                    (lambda (i) (deep-value-now (acc obj i) table)))))]
         [(vector? obj)
          (build-vector
           (vector-length obj)
           (lambda (i)
             (deep-value-now (vector-ref obj i) table)))]
         [else obj])]))

(define (lift strict? fn . args)
  (if (snap?) ;; maybe fix later to handle undefined-strictness
      (apply fn (map value-now args))
      (with-continuation-mark
          'frtime 'lift-active
        (cond
          [(ormap signal? args)
           (apply
            proc->signal
            (apply (if strict? create-strict-thunk create-thunk) fn args)
            args)]
          [(and strict? (ormap undefined? args)) undefined]
          [else (apply fn args)]))))

(define (lift-strict . args)
  (apply lift #t args))

; new-cell : behavior[a] -> behavior[a] (cell)
(define new-cell
  (lambda ([init undefined])
    (switch (event-receiver) init)))

(define (b1 . until . b2)
  (proc->signal
   (lambda () (if (undefined? (value-now b2))
                  (value-now b1)
                  (value-now b2)))
   ; deps
   b1 b2))

(define-syntax (event-loop-help stx)
  (syntax-case stx ()
    [(_ ([name expr] ...)
        [e => body] ...)
     (with-syntax ([args #'(name ...)])
       #'(accum-e
          (merge-e
           (e . ==> . (lambda (v)
                        (lambda (state)
                          (apply
                           (lambda args (body v))
                           state)))) ...)
          (list expr ...)))]))

(define-syntax (event-loop stx)
  
  (define (add-arrow clause)
    (syntax-case clause (=>)
      [(e => body) #'(e => body)]
      [(e body) #'(e => (lambda (_) body))]))
  
  (syntax-case stx ()
    [(_ ([name expr] ...)
        clause ...)
     (with-syntax ([(new-clause ...)
                    (map add-arrow (syntax->list #'(clause ...)))])
       #'(event-loop-help
          ([name expr] ...)
          new-clause ...)
       )]))

(define undefined?/lifted (lambda (arg) (lift #f undefined? arg)))

(define (event? v)
  (and (signal? v)
       (if (undefined? (signal-value v))
           undefined
           (event-set? (signal-value v)))))

; switch : event[behavior] behavior -> behavior
(define switch
  (lambda (e [init undefined])
    (let* ([init (box init)]
           [e-b (hold e (unbox init) #t)]
           [ret (proc->signal:switching
                 (case-lambda [() (value-now (unbox init))]
                              [(msg) e])
                 init e-b e-b (unbox init))])
      (set-signal-thunk!
       ret
       (case-lambda
         [()
          (when (not (eq? (unbox init) (signal-value e-b)))
            (unregister ret (unbox init))
            (set-box! init (value-now e-b))
            (register ret (unbox init))
            (set-signal-producers! ret (list e-b (unbox init)))
            (set-signal-depth! ret (max (signal-depth ret)
                                        (add1 (safe-signal-depth (unbox init)))))
            (iq-resort))
          (value-now/no-copy (unbox init))]
         [(msg) e]))
      ret)))

; event ... -> event
(define (merge-e . args)
  (apply lift #t (lambda args
                   (make-events-now
                    (apply append
                           (map event-set-events
                                (filter (lambda (es) (= (current-logical-time) (event-set-time es)))
                                        args)))))
         args))

(define (once-e e)
  (map-e second (filter-e (lambda (p) (= 1 (first p)))
                          (collect-e e (list 0) (lambda (e p) (list (add1 (first p)) e))))))

; behavior[a] -> event[a]
(define (changes b)
  (lift #f (let ([first-time #t])
             (lambda (bh)
               (begin0
                 (make-events-now
                  (if first-time
                      empty
                      (list (deep-value-now bh empty))))
                 (set! first-time #f))))
        b))

(define never-e
  (changes #f))

; when-e : behavior[bool] -> event
(define (when-e b)
  (let* ([last (value-now b)])
    (lift #t (lambda (bh)
               (make-events-now
                (let ([current bh])
                  (begin0
                    (if (and (not last) current)
                        (list current)
                        empty)
                    (set! last current)))))
          b)))

; while-e : behavior[bool] behavior[number] -> event
(define (while-e b interval)
  (letrec ([ret (event-producer2
            (lambda (emit)
              (lambda the-args
                (cond
                  [(value-now b) =>
                                 (lambda (v)
                                   (emit v)
                                   (schedule-alarm (+ (value-now interval) (current-inexact-milliseconds)) ret))])))
            b)])
    ret))

; ==> : event[a] (a -> b) -> event[b]
(define (e . ==> . f)
  (lift #t (lambda (es)
             (make-events-now
              (if (= (current-logical-time) (event-set-time es))
                  (map f (event-set-events es))
                  empty)))
        e))

; -=> : event[a] b -> event[b]
(define-syntax -=>
  (syntax-rules ()
    [(_ e k-e) (==> e (lambda (_) k-e))]))

; =#> : event[a] (a -> bool) -> event[a]
(define (e . =#> . p)
  (lift #t (lambda (es)
             (make-events-now
              (if (= (current-logical-time) (event-set-time es))
                  (filter (value-now p) (map value-now (event-set-events es)))
                  empty)))
        e))  

; =#=> : event[a] (a -> b U nothing) -> event[b]
(define (e . =#=> . f)
  (lift #t (lambda (es)
             (make-events-now
              (if (= (current-logical-time) (event-set-time es))
                  (filter (compose not nothing?) (map f (event-set-events es)))
                  empty)))
        e))

(define (map-e f e)
  (==> e f))
(define (filter-e p e)
  (=#> e p))
(define (filter-map-e f e)
  (=#=> e f))

(define (scan trans acc lst)
  (if (cons? lst)
      (let ([new-acc (trans (first lst) acc)])
        (cons new-acc (scan trans new-acc (rest lst))))
      empty))

; event[a] b (a b -> b) -> event[b]
(define (collect-e e init trans)
  (lift #t (lambda (es)
             (make-events-now
              (cond
                [(= (current-logical-time) (event-set-time es))
                 (let ([all-events (scan trans init (event-set-events es))])
                   (when (cons? all-events)
                     (set! init (first (last-pair all-events))))
                   all-events)]
                [else empty])))
        e))

; event[(a -> a)] a -> event[a]
(define (accum-e e init)
  (lift #t (lambda (es)
             (make-events-now
              (cond
                [(= (current-logical-time) (event-set-time es))
                 (let ([all-events (scan (lambda (t a) (t a)) init (event-set-events es))])
                   (when (cons? all-events)
                     (set! init (first (last-pair all-events))))
                   all-events)]
                [else empty])))
        e))

; event[a] b (a b -> b) -> behavior[b]
(define (collect-b ev init trans)
  (hold (collect-e ev init trans) init))

; event[(a -> a)] a -> behavior[a]
(define (accum-b ev init)
  (hold (accum-e ev init) init))

; hold : a event[a] -> behavior[a]
(define hold 
  (lambda (e [init undefined] [allow-behaviors? #f])
    (let ([val init]
          [warn-about-behaviors? #t])
      (lift #t (lambda (es) (let ([events (event-set-events es)])
                              (when (and (= (current-logical-time) (event-set-time es))
                                         (cons? events))
                                (set! val (first (last-pair (event-set-events es)))))
                              (when (and (behavior? val) (not allow-behaviors?))
                                (set! val (value-now val))
                                (when warn-about-behaviors?
                                  (thread
                                   (lambda ()
                                     (error "hold: input event had a behavior; snapshotting to prevent nested behavior")))
                                  (set! warn-about-behaviors? #f)))
                              val))
            e))))

(define-syntax snapshot/sync
  (syntax-rules ()
    [(_ (id ...) expr ...)
     (let-values ([(id ...) (value-now/sync id ...)])
       expr ...)]))

(define (synchronize)
  (snapshot/sync () (void)))

(define-syntax snapshot
  (syntax-rules ()
    [(_ (id ...) expr ...)
     (let ([id (value-now id)] ...)
       expr ...)]))

(define-syntax snapshot-all
  (syntax-rules ()
    [(_ expr ...)
     (parameterize ([snap? #t])
       expr ...)]))

(define (snapshot-e e . bs)
  (apply lift #t (lambda (es . bs)
                   (make-events-now
                    (cond
                      [(= (current-logical-time) (event-set-time es))
                       (map (lambda (the-event) (cons the-event (map value-now bs)))
                            (event-set-events es))]
                      [else empty])))
         e bs))

(define (snapshot/apply fn . args)
  (apply fn (map value-now args)))

;; Deprecated
(define-syntax frp:send
  (syntax-rules ()
    [(_ obj meth arg ...)
     (if (snap?)
         (send obj meth (value-now arg) ...)
         (send obj meth arg ...))]))  

(define (make-time-b ms)
  (let ([ret (proc->signal void)])
    (set-signal-thunk! ret
                       (lambda ()
                         (let ([t (current-inexact-milliseconds)])
                           (schedule-alarm (+ (value-now ms) t) ret)
                           t)))
    (set-signal-value! ret ((signal-thunk ret)))
    ret))

(define seconds
  (let ([ret (proc->signal void)])
    (set-signal-thunk! ret
                       (lambda ()
                         (let ([s (current-seconds)]
                               [t (current-inexact-milliseconds)])
                           (schedule-alarm (* 1000 (add1 (floor (/ t 1000)))) ret)
                           s)))
    (set-signal-value! ret ((signal-thunk ret)))
    ret))

;; signal[a] num -> signal[a]
;;
;; Returns a signal whose value at (approximately) time (+ t |delay-millis|) is a (deep) snapshot
;; of the value of |sig| at time t, for all times t from now on. For earlier times, the value of the
;; returned signal is undefined.
;;
;; Assumptions: (current-inexact-milliseconds) is monotonically non-decreasing; |delay-millis| is
;; positive and finite.
(define (delay-by sig delay-millis)
  ;; Implementation strategy:
  ;;
  ;; Maintain a queue of pairs (snapshot . timestamp) of the observed signal going back in
  ;; time for at least |delay-millis|. Start with (undefined . -inf.0) and (current-value . now), so
  ;; there should always be at least one item (value . timestamp) in the queue such that
  ;; (>= now (+ timestamp delay-millis)).
  ;;
  ;; |consumer| runs whenever |sig| changes and adds an item with the observed value and current
  ;;     time to the queue; schedules |producer| to run at |delay-millis| in the future, by which
  ;;     time it should be ready to take on that observed value.
  ;;
  ;; |producer| has no dependencies recorded in the dataflow graph and only runs when scheduled
  ;;     by the consumer. (This is what allows delay-by to break cycles.) It traverses the queue
  ;;     looking for the latest observation (value . timestamp) such that
  ;;     (>= now (+ timestamp delay-millis)), and takes on the observed value. |producer| is the
  ;;     value returned by this procedure, so it stays alive as long as anything cares about its
  ;;     value.
  (let* ([queue (make-queue)]
         
         ;; finish : (a . num) a -> a
         ;; Puts |queue-item| back on the front of the queue and returns |val|, updating the
         ;; occurrence timestamp if |val| represents an event set.
         ;; TODO(gcooper): We could avoid this if data/queue supported a "peek" operation.
         [finish! (lambda (queue-item val)
                    (enqueue-front! queue queue-item)
                    (if (event-set? val)
                        (make-events-now (event-set-events val))
                        val))]
         [now-millis (current-inexact-milliseconds)]
          
         [_ (begin
              ;; Add initial observations to the queue.
              (enqueue! queue (cons undefined -inf.0))
              (enqueue! queue (cons (deep-value-now sig empty) now-millis)))]
         
         ;; |consumer|'s thunk needs |producer| to be in scope so it can schedule it, and
         ;; |producer|'s thunk needs |consumer| to be in scope so it can keep it alive. To set up
         ;; this cycle, we first create |consumer| with a dummy thunk (void), then define
         ;; |producer|, and finally update |consumer|'s thunk to what we want it to be.
         [consumer (proc->signal void sig)]
         [producer (proc->signal
                    (lambda ()
                      (let ([now-millis (current-inexact-milliseconds)])
                        ;; There's no way to "peek" at the next item in the queue, so we have to
                        ;; dequeue it, check whether we're ready for it, and if not, stick it back
                        ;; on the front...
                        (let loop ([front (dequeue! queue)])
                          ;; |val| is our current candidate value; we'll use it if there's no later
                          ;; observation that's at least |delay-millis| old.
                          (let* ([val (car front)])
                            (if (queue-empty? queue)
                                ;; There are no later observations to consider, so use the current
                                ;; one.
                                (finish! front val)
                                ;; Look at the next item in the queue to see if we're ready for it.
                                ;; If so, recur. Otherwise, put it back on the front of the queue
                                ;; and use the previous value.
                                (let* ([next (dequeue! queue)]
                                       [timestamp-millis (cdr next)])
                                  ;; Kludge: since there's nothing that would otherwise keep
                                  ;; |consumer| alive, we retain a reference to it here, and we
                                  ;; trick the runtime into not optimizing it away by calling a
                                  ;; predicate and using the result in a conditional expression. If
                                  ;; the compiler ever gets smart enough to determine that the
                                  ;; outcome is provably always true, and therefore that it can
                                  ;; optimize away this code, we'll have to come up with a
                                  ;; different strategy (e.g., adding a special field to the signal
                                  ;; structure).
                                  (if (and (signal? consumer)
                                           (< now-millis (+ timestamp-millis delay-millis)))
                                      ;; We're not ready for the next value yet, so push it back
                                      ;; and proceed with the previous value.
                                      (begin
                                        (enqueue-front! queue next)
                                        (finish! front val))
                                      (loop next)))))))))])
    (begin
      (set-signal-thunk!
       consumer
       (lambda ()
         (let* ([now-millis (current-inexact-milliseconds)]
                [new-value (deep-value-now sig empty)])
           ;; Record the current observation and schedule |producer| to run when it's time to take
           ;; on this value.
           (enqueue! queue (cons new-value now-millis))
           (schedule-alarm (+ now-millis delay-millis) producer))))
      
      ;; Make sure producer is scheduled to run as soon as there's a value ready for it.
      (schedule-alarm (+ now-millis delay-millis) producer)
      producer)))

;; signal[a] -> signal[a]
;; Delays |sig| by the smallest possible amount of time.
(define (inf-delay sig)
  (delay-by sig 0))

; XXX fix to take arbitrary monotonically increasing number
; (instead of milliseconds)
; integral : signal[num] signal[num] -> signal[num]
(define integral
  (lambda (b [ms-b 20])
    (letrec ([accum 0]
             [last-time (current-inexact-milliseconds)]
             [last-val (value-now b)]
             [last-alarm 0]
             [consumer (proc->signal void b ms-b)]
             [producer (proc->signal (lambda () (and (signal? consumer) accum)))])
      (set-signal-thunk!
       consumer
       (lambda ()
         (let ([now (current-inexact-milliseconds)])
           (if (> now (+ last-time 20))
               (begin
                 (when (not (number? last-val))
                   (set! last-val 0))
                 (set! accum (+ accum
                                (* last-val
                                   (- now last-time))))
                 (set! last-time now)
                 (set! last-val (value-now b))
                 (when (value-now ms-b)
                   (schedule-alarm (+ last-time (value-now ms-b))
                                   consumer)))
               (when (or (>= now last-alarm)
                         (and (< now 0)
                              (>= last-alarm 0)))
                 (set! last-alarm (+ now 20))
                 (schedule-alarm last-alarm consumer)))
           (schedule-alarm now producer))))
      ((signal-thunk consumer))
      producer)))

; XXX fix for accuracy
; derivative : signal[num] -> signal[num]
(define (derivative b)
  (let* ([last-value (value-now b)]
         [last-time (current-inexact-milliseconds)]
         [thunk (lambda ()
                  (let* ([new-value (value-now b)]
                         [new-time (current-inexact-milliseconds)]
                         [result (if (or (= new-value last-value)
                                         (= new-time last-time)
                                         (> new-time
                                            (+ 500 last-time))
                                         (not (number? last-value))
                                         (not (number? new-value)))
                                     0
                                     (/ (- new-value last-value)
                                        (- new-time last-time)))])
                    (set! last-value new-value)
                    (set! last-time new-time)
                    result))])
    (proc->signal thunk b)))

(define create-strict-thunk
  (case-lambda
    [(fn) fn]
    [(fn arg1) (lambda ()
                 (let ([a1 (value-now/no-copy arg1)])
                   (if (undefined? a1)
                       undefined
                       (fn a1))))]
    [(fn arg1 arg2) (lambda ()
                      (let ([a1 (value-now/no-copy arg1)]
                            [a2 (value-now/no-copy arg2)])
                        (if (or (undefined? a1)
                                (undefined? a2))
                            undefined
                            (fn a1 a2))))]
    [(fn arg1 arg2 arg3) (lambda ()
                           (let ([a1 (value-now/no-copy arg1)]
                                 [a2 (value-now/no-copy arg2)]
                                 [a3 (value-now/no-copy arg3)])
                             (if (or (undefined? a1)
                                     (undefined? a2)
                                     (undefined? a3))
                                 undefined
                                 (fn a1 a2 a3))))]
    [(fn . args) (lambda ()
                   (let ([as (map value-now/no-copy args)])
                     (if (ormap undefined? as)
                         undefined
                         (apply fn as))))]))

(define create-thunk
  (case-lambda
    [(fn) fn]
    [(fn arg1) (lambda () (fn (value-now/no-copy arg1)))]
    [(fn arg1 arg2) (lambda () (fn (value-now/no-copy arg1) (value-now/no-copy arg2)))]
    [(fn arg1 arg2 arg3) (lambda () (fn (value-now/no-copy arg1)
                                        (value-now/no-copy arg2)
                                        (value-now/no-copy arg3)))]
    [(fn . args) (lambda () (apply fn (map value-now/no-copy args)))]))

#;
(define (general-event-processor proc . args)
  ; proc : (lambda (emit suspend first-evt) ...)
  (let* ([out (econs undefined undefined)]
         [esc #f]
         [emit (lambda (val)
                 (set-erest! out (econs val undefined))
                 (set! out (erest out))
                 val)]
         [streams (map signal-value args)])
    (letrec ([suspend (lambda ()
                        (call/cc
                         (lambda (k)
                           (set! proc-k k)
                           (esc (void)))))]
             [proc-k (lambda (evt) (proc emit suspend evt) (set! proc-k #f))])
      (let ([thunk (lambda ()
                     (when (ormap undefined? streams)
                       ;(eprintf "had an undefined stream\n")
                       (set! streams (fix-streams streams args)))
                     (let loop ([streams streams])
                       (extract (lambda (the-event strs)
                                  (when proc-k
                                    (call/cc
                                     (lambda (k)
                                       (set! esc k)
                                       (proc-k the-event)))) (loop strs))
                                streams))
                     (set! streams (map signal-value args))
                     out)])
        (apply proc->signal thunk args)))))

(define current-emit (make-parameter #f))
(define current-select (make-parameter #f))
(define (emit ev)
  (cond
    [(current-emit) => (lambda (f) (f ev))]
    [else (error 'emit "outside of general-event-processor")]))
(define (select-proc . clauses)
  (cond
    [(current-select) => (lambda (f) (apply f clauses))]
    [else (error 'select "outside of general-event-processor")]))

(define-syntax (select stx)
  (syntax-case stx ()
    [(select clause ...)
     (with-syntax ([((e k) ...)
                    (map (lambda (c)
                           (syntax-case c (=>)
                             [(e => k) #'(e k)]
                             [(e exp0 exp1 ...) #'(e (lambda (_) exp0 exp1 ...))]))
                         (syntax-e #'(clause ...)))])
       #'(select-proc (list e k) ...))]))

(define (flush . strs)
  (select-proc (map (lambda (str) (list str void)) strs)))

#;
(define (general-event-processor2 proc)
  (do-in-manager
   (let* ([out (econs undefined undefined)]
          [emit (lambda (val)
                  (set-erest! out (econs val undefined))
                  (set! out (erest out))
                  val)]
          [streams (make-weak-hash)]
          [extracted (make-weak-hash)]
          [top-esc #f]
          [rtn (proc->signal void)]
          [select (lambda e/k-list
                    (let/ec esc
                      (let loop ()
                        (for-each (lambda (e/k)
                                    (let* ([e (first e/k)]
                                           [x (hash-ref
                                               extracted e
                                               (lambda () empty))])
                                      (when (cons? x)
                                        (hash-set!
                                         extracted e (rest x))
                                        (esc ((second e/k) (first x))))))
                                  e/k-list)
                        (for-each (lambda (e/k)
                                    (let* ([e (first e/k)])
                                      (hash-ref
                                       streams e
                                       (lambda ()
                                         (register rtn e)
                                         (hash-set!
                                          streams e
                                          (signal-value e))))))
                                  e/k-list)
                        (let/cc k
                          (set! proc (lambda () (k (void))))
                          (top-esc (void)))
                        (loop))))])
     (let ([thunk (lambda ()
                    (hash-for-each
                     streams
                     (lambda (k v)
                       ;; inefficient! appends each new event individually
                       (let loop ([str v])
                         (when (and (econs? str)
                                    (not (undefined? (erest str))))
                           (hash-set!
                            extracted k
                            (append (hash-ref extracted k (lambda () empty))
                                    (list (efirst (erest str)))))
                           (loop (erest str))))
                       (hash-set! streams k (signal-value k))))
                    (let/cc k
                      (set! top-esc k)
                      (parameterize ([current-emit emit]
                                     [current-select select])
                        (proc)))
                    out)])
       (set-signal-thunk! rtn thunk)
       (iq-enqueue rtn)
       rtn))))

(define (make-mutable lst)
  (printf "make-mutable called on ~a\n" lst)
  lst
  #;(if (pair? lst)
        (mcons (first lst) (make-mutable (rest lst)))
        lst))

;; split : event[a] (a -> b) -> (b -> event[a])
(define (split ev fn)
  (let* ([ht (make-weak-hash)]
         [sig (for-each-e!
               ev
               (lambda (e)
                 (let/ec k
                   (send-event
                    (hash-ref ht (fn e) (lambda () (k (void))))
                    e)))
               ht)])
    (lambda (x)
      sig
      (hash-ref
       ht x (lambda ()
              (let ([rtn (event-receiver)])
                (hash-set! ht x rtn)
                rtn))))))

(define-syntax event-select
  (syntax-rules ()
    [(_ [ev k] ...)
     ()]))

(define fine-timer-granularity (new-cell 20))

(define milliseconds (make-time-b fine-timer-granularity))
(define time-b milliseconds)

;;;;;;;;;;;;;;;;;;;;;;
;; Command Lambda 

(define-syntax mk-command-lambda
  (syntax-rules ()
    [(_ (free ...) forms body ...)
     (if (ormap behavior? (list free ...))
         (procs->signal:compound
          (lambda x (lambda forms
                      (snapshot (free ...) body ...)))
          (lambda (a b) void)
          free ...)
         (lambda forms body ...))]))

(define-for-syntax code-insp (variable-reference->module-declaration-inspector
                              (#%variable-reference)))

(define-syntax (command-lambda stx)
  
  (define (arglist-bindings arglist-stx)
    (syntax-case arglist-stx ()
      [var
       (identifier? arglist-stx)
       (list arglist-stx)]
      [(var ...)
       (syntax->list arglist-stx)]
      [(var . others)
       (cons #'var (arglist-bindings #'others))]))
  
  
  (define (make-snapshot-unbound insp unbound-ids)
    (lambda (expr bound-ids)
      (let snapshot-unbound ([expr expr] [bound-ids bound-ids])
        (syntax-case (syntax-disarm expr code-insp) (#%datum
                                                     quote
                                                     #%top
                                                     let-values
                                                     letrec-values
                                                     lambda)
           [x (identifier? #'x) (if (or
                                     (syntax-property #'x 'protected)
                                     (ormap (lambda (id)
                                              (bound-identifier=? id #'x)) bound-ids))
                                    #'x
                                    (begin
                                      (hash-set! unbound-ids #'x #t)
                                      #'(#%app value-now x)))]
           [(#%datum . val) expr]
           [(quote . _) expr]
           [(#%top . var) (begin
                            (hash-set! unbound-ids #'var #t)
                            #`(#%app value-now #,expr))] ; FIX
           
           [(letrec-values (((variable ...) in-e) ...) body-e ...)
            (let ([new-bound-ids (append (syntax->list #'(variable ... ...)) bound-ids)])
              (with-syntax ([(new-in-e ...) (map (lambda (exp)
                                                   (snapshot-unbound exp new-bound-ids))
                                                 (syntax->list #'(in-e ...)))]
                            [(new-body-e ...) (map (lambda (exp)
                                                     (snapshot-unbound exp new-bound-ids))
                                                   (syntax->list #'(body-e ...)))])
                #'(letrec-values (((variable ...) new-in-e) ...) new-body-e ...)))]
           [(let-values (((variable ...) in-e) ...) body-e ...)
            (let ([new-bound-ids (append (syntax->list #'(variable ... ...)) bound-ids)])
              (with-syntax ([(new-in-e ...) (map (lambda (exp)
                                                   (snapshot-unbound exp bound-ids))
                                                 (syntax->list #'(in-e ...)))]
                            [(new-body-e ...) (map (lambda (exp)
                                                     (snapshot-unbound exp new-bound-ids))
                                                   (syntax->list #'(body-e ...)))])
                #'(let-values (((variable ...) new-in-e) ...) new-body-e ...)))]
           [(lambda forms body-e ...)
            (let ([new-bound-ids (append (arglist-bindings #'forms) bound-ids)])
              (with-syntax ([(new-body-e ...) (map (lambda (exp)
                                                     (snapshot-unbound exp new-bound-ids))
                                                   (syntax->list #'(body-e ...)))])
                #'(lambda forms new-body-e ...)))]
           [(tag exp ...)
            (with-syntax ([(new-exp ...) (map (lambda (exp)
                                                (snapshot-unbound exp bound-ids))
                                              (syntax->list #'(exp ...)))])
              #'(tag new-exp ...))]
           [x (begin
                (eprintf "snapshot-unbound: fell through on ~a\n" #'x)
                '())]))))
  
  (syntax-case stx ()
    [(src-command-lambda (id ...) expr ...)
     (let ([c-insp (current-code-inspector)])
       (parameterize ([current-code-inspector (make-inspector)])
         (syntax-case (local-expand #'(lambda (id ...) expr ...) 'expression '()) (lambda)
           [(lambda (id ...) expr ...)
            (let ([unbound-ids (make-hash)])
              (with-syntax ([(new-expr ...) (map (lambda (exp)
                                                   ((make-snapshot-unbound c-insp unbound-ids)
                                                    exp
                                                    (syntax->list #'(id ...))))
                                                 (syntax->list #'(expr ...)))]
                            [(free-var ...) (hash-map unbound-ids
                                                      (lambda (k v) k))])
                (begin
                  ;(printf "~a\n" unbound-ids)
                  #'(if (ormap behavior? (list free-var ...))
                        (procs->signal:compound (lambda _ 
                                                  (lambda (id ...)
                                                    new-expr ...))
                                                (lambda (a b) void)
                                                free-var ...)
                        (lambda (id ...) expr ...)))))])))]))


(define for-each-e!
  (let ([ht (make-weak-hash)])
    (lambda (ev proc [ref 'dummy])
      (hash-set! ht ref (cons (ev . ==> . proc) (hash-ref ht ref (lambda () empty)))))))

(define raise-exceptions (new-cell #t))

(define exception-raiser
  (exceptions . ==> . (lambda (p) (when (value-now raise-exceptions)
                                    (thread
                                     (lambda () (raise (car p))))))))

(provide raise-exceptions
         deep-value-now
         nothing
         nothing?
         ;general-event-processor
         ;general-event-processor2
         emit
         select
         switch
         merge-e
         once-e
         changes
         never-e
         when-e
         while-e
         ==>
         -=>
         =#>
         =#=>
         map-e
         filter-e
         filter-map-e
         collect-e
         accum-e
         collect-b
         accum-b
         hold
         for-each-e!
         snapshot/sync
         synchronize
         snapshot
         snapshot-e
         snapshot/apply
         milliseconds
         fine-timer-granularity
         seconds
         delay-by
         inf-delay
         integral
         derivative
         new-cell
         lift
         lift-strict
         event?
         command-lambda
         mk-command-lambda
         until
         event-loop
         split
         define-reactive
         
         ;; from core/frp
         event-receiver
         send-event
         send-synchronous-event
         send-synchronous-events
         set-cell!
         undefined
         (rename-out [undefined?/lifted undefined?])
         (rename-out [undefined? frp:undefined?])
         behavior?
         value-now
         value-now/no-copy
         value-now/sync
         signal-count
         signal?)


