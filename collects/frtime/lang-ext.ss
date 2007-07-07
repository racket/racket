(module lang-ext mzscheme
  (require (lib "frp-core.ss" "frtime")
           (lib "etc.ss")
           (lib "list.ss"))

  (require-for-syntax (lib "list.ss"))
  
  (define nothing (void));(string->uninterned-symbol "nothing"))
  
  (define (nothing? v) (eq? v nothing))
  
  (define deep-value-now
    (case-lambda
      [(obj) (deep-value-now obj empty)]
      [(obj table)
       (cond
         [(assq obj table) => second]
         [(behavior? obj)
          (deep-value-now (signal-value obj) (cons (list obj (signal-value obj)) table))]
         [(cons? obj)
          (let* ([result (cons #f #f)]
                 [new-table (cons (list obj result) table)]
                 [car-val (deep-value-now (car obj) new-table)]
                 [cdr-val (deep-value-now (cdr obj) new-table)])
            (set-car! result car-val)
            (set-cdr! result cdr-val)
            result)]
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
      
  
  
  ; new-cell : behavior[a] -> behavior[a] (cell)
  (define new-cell
    (opt-lambda ([init undefined])
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
  
  
  (define undefined?/lifted (lambda (arg) (lift false undefined? arg)))
  
  (define (event? v)
    (and (signal? v)
         (if (undefined? (signal-value v))
             undefined
             (event-cons? (signal-value v)))))
  
  
  (define-syntax (event-producer stx)
    (syntax-case stx ()
      [(src-event-producer expr dep ...)
       (with-syntax ([emit (datum->syntax-object (syntax src-event-producer) 'emit)]
                     [the-args (datum->syntax-object
                                (syntax src-event-producer) 'the-args)])
         (syntax (let* ([out (econs undefined undefined)]
                        [emit (lambda (val)
                                (set-erest! out (econs val undefined))
                                (set! out (erest out)))])
                   (proc->signal (lambda the-args expr out) dep ...))))]))
    
  ; switch : event[behavior] behavior -> behavior
  (define switch
    (opt-lambda (e [init undefined])
      (let* ([init (box init)]
             [e-b (hold e (unbox init))]
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
    (apply event-processor
           (lambda (emit)
             (lambda (the-event)
               (emit the-event)))
           args))
  
  (define (once-e e)
    (let ([b #t])
      (rec ret (event-processor
                (lambda (emit)
                  (lambda (the-event)
                    (when b
                      (set! b false)
                      (unregister ret e)
                      (emit the-event))))
                e))))
  
  ; behavior[a] -> event[a]
  (define (changes b)
    (event-producer2
     (lambda (emit)
       (lambda the-args
         (emit (deep-value-now b))))
     b))
  
  (define never-e
    (changes #f))
    
  
  ; when-e : behavior[bool] -> event
  (define (when-e b)
    (let* ([last (value-now b)])
      (event-producer2
       (lambda (emit)
         (lambda the-args
           (let ([current (value-now b)])
             (when (and (not last) current)
               (emit current))
             (set! last current))))
       b)))
  
  ; while-e : behavior[bool] behavior[number] -> event
  (define (while-e b interval)
    (rec ret (event-producer2
              (lambda (emit)
                (lambda the-args
                  (cond
                    [(value-now b) =>
                                   (lambda (v)
                                     (emit v)
                                     (schedule-alarm (+ (value-now interval) (current-milliseconds)) ret))])))
              b)))
  
  ; ==> : event[a] (a -> b) -> event[b]
  (define (e . ==> . f)
    (event-processor
     (lambda (emit)
       (lambda (the-event)
         (emit ((value-now f) the-event))))
     e))
  
  
  
  #|
  (define (e . =>! . f)
    (event-processor
     ((value-now f) the-event)
     (list e)))
  |#
  
  ; -=> : event[a] b -> event[b]
  (define-syntax -=>
    (syntax-rules ()
      [(_ e k-e) (==> e (lambda (_) k-e))]))
  
  ; =#> : event[a] (a -> bool) -> event[a]
  (define (e . =#> . p)
    (event-processor
     (lambda (emit)
       (lambda (the-event)
         (when (value-now (p the-event))
           (emit the-event))))
     e))
  
  
  
  ; =#=> : event[a] (a -> b U nothing) -> event[b]
  (define (e . =#=> . f)
    (event-processor
     (lambda (emit)
       (lambda (the-event)
         (let ([x (f the-event)])
           (unless (or (nothing? x) (undefined? x))
             (emit x)))))
     e))
  
  (define (map-e f e)
    (==> e f))
  (define (filter-e p e)
    (=#> e p))
  (define (filter-map-e f e)
    (=#=> e f))
  
  ; event[a] b (a b -> b) -> event[b]
  (define (collect-e e init trans)
    (event-processor
     (lambda (emit)
       (lambda (the-event)
         (let ([ret (trans the-event init)])
           (set! init ret)
           (emit ret))))
     e))
  
  ; event[(a -> a)] a -> event[a]
  (define (accum-e e init)
    (event-processor
     (lambda (emit)
       (lambda (the-event)
         (let ([ret (the-event init)])
           (set! init ret)
           (emit ret))))
     e))
  
  ; event[a] b (a b -> b) -> behavior[b]
  (define (collect-b ev init trans)
    (hold (collect-e ev init trans) init))
  
  ; event[(a -> a)] a -> behavior[a]
  (define (accum-b ev init)
    (hold (accum-e ev init) init))
  
  ; hold : a event[a] -> behavior[a]
  (define hold 
    (opt-lambda (e [init undefined])
      (let ([val init])
        (let* ([updator (event-processor
                         (lambda (emit)
                           (lambda (the-event)
                             (set! val the-event)
                             (emit the-event)))
                         e)]
               [rtn (proc->signal (lambda () updator val) updator)])
          rtn))))
  
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
    (event-processor
     (lambda (emit)
       (lambda (the-event)
         (emit (cons the-event (map value-now bs)))))
     e))
  
  (define (snapshot/apply fn . args)
    (apply fn (map value-now args)))
  
  

  ;; Deprecated
  (define-syntax frp:send
    (syntax-rules ()
      [(_ obj meth arg ...)
       (if (snap?)
           (send obj meth (value-now arg) ...)
           (send obj meth arg ...))]))
  
  ;; Depricated
  (define (magic dtime thunk)
    (let* ([last-time (current-milliseconds)]
           [ret (let ([myself #f])
                  (event-producer
                   (let ([now (current-milliseconds)])
                     (snapshot (dtime)
                               (when (cons? the-args)
                                 (set! myself (first the-args)))
                               (when (and dtime (>= now (+ last-time dtime)))
                                 (emit (thunk))
                                 (set! last-time now))
                               (when dtime
                                 (schedule-alarm (+ last-time dtime) myself))))
                   dtime))])
      (send-event ret ret)
      ret))
  
  
  ;; Depricated
  (define (make-time-b ms)
    (let ([ret (proc->signal void)])
      (set-signal-thunk! ret
                         (lambda ()
                           (let ([t (current-milliseconds)])
                             (schedule-alarm (+ ms t) ret)
                             t)))
      (set-signal-value! ret ((signal-thunk ret)))
      ret))
  
  
  
  (define milliseconds (make-time-b 20))
  (define time-b milliseconds)
  
  (define seconds
    (let ([ret (proc->signal void)])
      (set-signal-thunk! ret
                         (lambda ()
                           (let ([s (current-seconds)]
                                 [t (current-milliseconds)])
                             (schedule-alarm (* 1000 (add1 (floor (/ t 1000)))) ret)
                             s)))
      (set-signal-value! ret ((signal-thunk ret)))
      ret))
  
  ; general efficiency fix for delay
  ; signal[a] signal[num] -> signal[a]
  (define (delay-by beh ms-b)
    (letrec ([last (cons (cons (if (zero? (value-now ms-b))
                                   (value-now/no-copy beh)
                                   undefined)
                               (current-milliseconds))
                         empty)]
             [head last]
             [dummy 0]
             [producer (proc->signal
                        (lambda ()
                          (let* ([now (current-milliseconds)]
                                 [ms (value-now ms-b)])
                            (let loop ()
                              (if (or (empty? (rest head))
                                      (< now (+ ms (cdadr head))))
                                  (caar head)
                                  (begin
                                    (set! dummy consumer) ;; just to prevent GC
                                    (set! head (rest head))
                                    (loop)))))))]
             [consumer (proc->signal
                        (lambda ()
                          (let* ([now (current-milliseconds)]
                                 [new (deep-value-now beh)]
                                 [ms (value-now ms-b)])
                            (when (not (equal? new (caar last)))
                              (set-rest! last (cons (cons new now)
                                                    empty))
                              (set! last (rest last))
                              (schedule-alarm (+ now ms) producer))))
                        beh ms-b)])
      producer))
  
  (define (inf-delay beh)
    (delay-by beh 0))
  
  ; fix to take arbitrary monotonically increasing number
  ; (instead of milliseconds)
  ; integral : signal[num] signal[num] -> signal[num]
  (define integral
    (opt-lambda (b [ms-b 20])
      (letrec ([accum 0]
               [last-time (current-milliseconds)]
               [last-val (value-now b)]
               [last-alarm 0]
               [dummy 0]
               [producer (proc->signal (lambda ()
                                         (set! dummy consumer) ;; just to prevent GC
                                         accum))]
               [consumer (proc->signal void b ms-b)])
        (set-signal-thunk!
         consumer
         (lambda ()
           (let ([now (current-milliseconds)])
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
  
  ; fix for accuracy
  ; derivative : signal[num] -> signal[num]
  (define (derivative b)
    (let* ([last-value (value-now b)]
           [last-time (current-milliseconds)]
           [thunk (lambda ()
                    (let* ([new-value (value-now b)]
                           [new-time (current-milliseconds)]
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
  
  
  (define (lift strict? fn . args)
    (if (snap?) ;; maybe fix later to handle undefined-strictness
        (apply fn (map value-now/no-copy args))
        (with-continuation-mark
            'frtime 'lift-active
          (if (ormap behavior? args)
              (begin
                #;(when (ormap signal:compound? args)
                  (printf "attempting to lift ~a over a signal:compound in ~a!~n" fn (map value-now args)))
                (apply
                 proc->signal
                 (apply (if strict? create-strict-thunk create-thunk) fn args)
                 args))
              (if (and strict? (ormap undefined? args))
                  undefined
                  (apply fn args))))))
  
  (define (lift-strict . args)
    (apply lift #t args))
  
  
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
                         ;(fprintf (current-error-port) "had an undefined stream~n")
                         (set! streams (fix-streams streams args)))
                       (let loop ()
                         (extract (lambda (the-event)
                                    (when proc-k
                                      (call/cc
                                       (lambda (k)
                                         (set! esc k)
                                         (proc-k the-event)))) (loop))
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
  
  (define (general-event-processor2 proc)
    (do-in-manager
     (let* ([out (econs undefined undefined)]
            [emit (lambda (val)
                    (set-erest! out (econs val undefined))
                    (set! out (erest out))
                    val)]
            [streams (make-hash-table 'weak)]
            [extracted (make-hash-table 'weak)]
            [top-esc #f]
            [rtn (proc->signal void)]
            [select (lambda e/k-list
                      (let/ec esc
                        (let loop ()
                          (for-each (lambda (e/k)
                                      (let* ([e (first e/k)]
                                             [x (hash-table-get
                                                 extracted e
                                                 (lambda () empty))])
                                        (when (cons? x)
                                          (hash-table-put!
                                           extracted e (rest x))
                                          (esc ((second e/k) (first x))))))
                                    e/k-list)
                          (for-each (lambda (e/k)
                                      (let* ([e (first e/k)])
                                        (hash-table-get
                                         streams e
                                         (lambda ()
                                           (register rtn e)
                                           (hash-table-put!
                                            streams e
                                            (signal-value e))))))
                                    e/k-list)
                          (let/cc k
                            (set! proc (lambda () (k (void))))
                            (top-esc (void)))
                          (loop))))])
       (let ([thunk (lambda ()
                      (hash-table-for-each
                       streams
                       (lambda (k v)
                         ;; inefficient! appends each new event individually
                         (let loop ([str v])
                           (when (and (econs? str)
                                      (not (undefined? (erest str))))
                             (hash-table-put!
                              extracted k
                              (append (hash-table-get extracted k (lambda () empty))
                                      (list (efirst (erest str)))))
                             (loop (erest str))))
                         (hash-table-put! streams k (signal-value k))))
                      (let/cc k
                        (set! top-esc k)
                        (parameterize ([current-emit emit]
                                       [current-select select])
                          (proc)))
                      out)])
         (set-signal-thunk! rtn thunk)
         (iq-enqueue rtn)
         rtn))))
  
  (define (event-processor proc . args)
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
      (apply proc->signal thunk args)))
  
  ;; split : event[a] (a -> b) -> (b -> event[a])
  (define (split ev fn)
    (let* ([ht (make-hash-table 'weak)]
           [sig (map-e (lambda (e)
                         (let/ec k
                           (send-event
                            (hash-table-get ht (fn e) (lambda () (k (void))))
                            e)))
                       ev)])                             
      (lambda (x)
        sig
        (hash-table-get
         ht x (lambda ()
                (let ([rtn (event-receiver)])
                  (hash-table-put! ht x rtn)
                  rtn))))))
  
  (define-syntax event-select
    (syntax-rules ()
      [(_ [ev k] ...)
       ()]))
  
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
          (syntax-recertify
           (syntax-case expr (#%datum
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
                                        (hash-table-put! unbound-ids #'x #t)
                                        #'(#%app value-now x)))]
             [(#%datum . val) expr]
             [(quote . _) expr]
             [(#%top . var) (begin
                              (hash-table-put! unbound-ids #'var #t)
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
                  (fprintf (current-error-port) "snapshot-unbound: fell through on ~a~n" #'x)
                  ())])
           expr insp #f))))
    
    (syntax-case stx ()
      [(src-command-lambda (id ...) expr ...)
       (let ([c-insp (current-code-inspector)])
         (parameterize ([current-code-inspector (make-inspector)])
           (syntax-case (local-expand #'(lambda (id ...) expr ...) 'expression ()) (lambda)
             [(lambda (id ...) expr ...)
              (let ([unbound-ids (make-hash-table)])
                (with-syntax ([(new-expr ...) (map (lambda (exp)
                                                     ((make-snapshot-unbound c-insp unbound-ids)
                                                      exp
                                                      (syntax->list #'(id ...))))
                                                   (syntax->list #'(expr ...)))]
                              [(free-var ...) (hash-table-map unbound-ids
                                                              (lambda (k v) k))])
                  (begin
                    ;(printf "~a~n" unbound-ids)
                    #'(if (ormap behavior? (list free-var ...))
                          (procs->signal:compound (lambda _ 
                                                    (lambda (id ...)
                                                      new-expr ...))
                                                  (lambda (a b) void)
                                                  free-var ...)
                          (lambda (id ...) expr ...)))))])))]))
  
  
  (define for-each-e!
    (let ([ht (make-hash-table 'weak)])
      (opt-lambda (ev proc [ref 'dummy])
        (hash-table-put! ht ref (cons (ev . ==> . proc) (hash-table-get ht ref (lambda () empty)))))))
  
  (define raise-exceptions (new-cell #t))
  
  (define exception-raiser
    (exceptions . ==> . (lambda (p) (when (value-now raise-exceptions)
                                      (thread
                                       (lambda () (raise (car p))))))))
      
         

  
  (provide raise-exceptions
           nothing
           nothing?
           general-event-processor
           general-event-processor2
           emit
           select
           event-processor
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
           magic
           milliseconds
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
           
           ;; from frp-core
           event-receiver
           send-event
           send-synchronous-event
           send-synchronous-events
           set-cell!
           undefined
           (rename undefined?/lifted undefined?)
           (rename undefined? frp:undefined?)
           behavior?
           value-now
           value-now/no-copy
           value-now/sync
           frtime-version
           signal-count
           signal?
           
           )
  )


