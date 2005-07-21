; Ideas:
;  tag impure and imperative signals (pure vs. stateful vs. effectful)
;  use weak boxes in internal queue
;  have manager initialize signals
;  
;
; To do:
; QUESTION: should cond change to skip undefined conditions?
; make switchable events
; split delay into consumer and producer
; deal with multiple values (?)
; handle structs, vectors (done?)
; localized exception-handling mechanism
; precise depths even when switching, proper treatment of cycles
; revisit cons, vector [, structs]
;
; generalize and improve notion of time
;  (e.g., combine seconds & milliseconds,
;  give user general "timer : number -> signal (event?)"
;  'loose' timers that don't exactly measure real time
;   (e.g., during garbage-collection)
; completely restructure:
;  eliminate alarms, give processes timeouts
;  make special constructor signals (?)
;   (have tried and achieved unencouraging results)
;  separate signal and event structures (?)
;    - could make event a substructure of signal,
;      but this could be problematic for letrec
;    - better option seems to be explicit tag
; partial-order based evaluation:
;  - add a 'depth' field to signal structure (DONE)
;  - make 'register' responsible for maintaining consistency
;  - 'switch' can result in cycle, in which case consistent
;    depths cannot be assigned
;  - should perhaps tag delay, integral nodes
; selective evaluation (?)
; consider adding placeholders again, this time as part
;   of the FRP system
;   (probably not necessary, since signals can serve
;    in this role)
; consider whether any other syntax should be translated
;   (e.g. 'begin')
; consider 'strict structure'
;
; Done:
; mutual dependencies between signals (sort of ...)
; fix delay bug
; allow delay to take a time signal (sort of ...)
; events:
;   use signal structs where value is tail of stream
;   interface with other libraries (e.g. graphics), other threads
;   hold : event * val -> signal
;   changes : signal -> event
;   map-e : event[a] * (a -> b) -> event[b]
;   merge-e : event[a1] * event[a2] * ... -> event[a1 U a2 U ...]
;   filter-e : event[a] * (a -> bool) -> event[a]
;   accum : event[a] * b * (a -> b -> b) -> event[b]
; modify graphics library to send messages for events
; signal manager's priority queue should
;   - maintain weak boxes
;   - check stale flag before enqueuing for update
; delete dead weak references
; fix letrec-b macro to use switch
; - allow fn signals
; eliminate letrec-b, make appropriate letrec macro
;  ('undefined' value) (probably)
; rewrite lift
; - provide specialized lift for 0-3 (?) arguments
; fix subtle concurrency issue between
;  signal creation outside manager thread
;  and activities of manager, particularly
;  involving registration/deregistration
;  (solution: send reg/unreg requests to manager if necessary)
; make separate library for graphics
;
; need to get rid of #%app macro, explicitly lift procedures, usually
; bottom-strictly
;
; remove #%app, lambda, and define macros; lift all
;   primitives, redefine higher-order procedures
;   macro to automate definition of lifted primitives
;   make signals directly applicable
; flip arguments in event-handling combinators (done)
;


;; Fix all predicates for compound signals to return constant as per frp:pair?
;; Consider re-implementing switch strategy
(module frp mzscheme
  
  (require (lib "list.ss")
           (lib "etc.ss")
           (lib "class.ss")
           ;(all-except (lib "mred.ss" "mred") send-event)
           (lib "string.ss")
           "erl.ss"
           (lib "match.ss")
           "heap.ss")
  
  (require-for-syntax (lib "list.ss") (lib "etc.ss") (lib "struct.ss" "frtime"))
  
  (define frtime-version "0.3b -- Tue Nov 9 13:39:45 2004")
  (define frtime-inspector (make-inspector))
  (print-struct #t)
  
  (define snap? (make-parameter #f))

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
                  signal-custodians
                  signal-producers
                  set-signal-value!
                  set-signal-dependents!
                  set-signal-stale?!
                  set-signal-thunk!
                  set-signal-depth!
                  set-signal-continuation-marks!
                  set-signal-custodians!
                  set-signal-producers!)
    (let*-values ([(field-name-symbols)
                   (list 'value 'dependents 'stale? 'thunk
                         'depth 'continuation-marks 'guards 'producers)]
                  [(desc make-signal signal? acc mut)
                   (make-struct-type
                    'signal #f (length field-name-symbols) 0 #f null frtime-inspector
                    (lambda (fn . args)
                      (unregister #f fn) ; clear out stale dependencies from previous apps
                      (let* (; revisit error-reporting for switched behaviors
                             [ccm (current-continuation-marks)]
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
                                    'depth 'continuation-marks 'guards 'producers)])
      (list-immutable
       ((syntax-local-certifier) #'struct:signal)
       ((syntax-local-certifier) #'make-signal)
       ((syntax-local-certifier) #'signal?)
       (apply list-immutable
              (map
               (lambda (fd)
                 ((syntax-local-certifier) (datum->syntax-object
                                            #'here
                                            (string->symbol (format "signal-~a" fd)))))
               (reverse field-name-symbols)))
       (apply list-immutable
              (map
               (lambda (fd)
                 ((syntax-local-certifier) (datum->syntax-object
                                            #'here
                                            (string->symbol (format "set-signal-~a!" fd)))))
               (reverse field-name-symbols)))
       #t)))
  
  (define-struct ft-cust (signal constructed-sigs))
  (define-struct non-scheduled (signal))
  
  (define current-custs
    (make-parameter empty))
  
  (define-struct multiple (values))
  
  (define-struct event-cons (head tail))
  (define econs make-event-cons)
  (define efirst event-cons-head)
  (define erest event-cons-tail)
  (define econs? event-cons?)
  (define set-efirst! set-event-cons-head!)
  (define set-erest! set-event-cons-tail!)
  
  (define-struct (signal:compound signal) (content copy))
  
  ;;;;;;;;;;;;;;;;;;;;;
  ;; Ported Structs  ;;
  ;;;;;;;;;;;;;;;;;;;;;
  
  (define (frp:eq? itm1 itm2)
    (lift #t eq? itm1 itm2))
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; CONS
  
  
  (define (frp:cons f r)
    (if (or (behavior? f) (behavior? r))
        (procs->signal:compound
         cons
         (lambda (p i)
           (if (zero? i)
               (lambda (v) (set-car! p v))
               (lambda (v) (set-cdr! p v))))
         f r)
        (cons f r)))
  
  (define (make-accessor acc)
    (lambda (v)
      (cond
        [(signal:compound? v) (acc (signal:compound-content v))]
        [(signal? v) (lift #t acc v)]
        [else (acc v)])))
  
  (define frp:car
    (make-accessor car))
  
  (define frp:cdr
    (make-accessor cdr))
 
  (define frp:pair? (lambda (arg) (if (signal:compound? arg)
                                      (pair? (signal:compound-content arg))
                                      (lift true pair? arg))))
  
  (define (frp:empty? x)
    (lift true empty? x))
  
  (define (frp:append lst0 lst1)
    (frp:if (frp:empty? lst0)
            lst1
            (frp:cons (frp:car lst0)
                      (frp:append (frp:cdr lst0) lst1))))
  
  (define frp:list
    (lambda elts
      (frp:if (frp:empty? elts)
              '()
              (frp:cons (frp:car elts)
                        (apply frp:list (frp:cdr elts))))))
  
  (define frp:list*
    (lambda elts
      (frp:if (frp:empty? elts)
              '()
              (frp:if (frp:empty? (frp:cdr elts))
                      (frp:car elts)
                      (frp:cons (frp:car elts)
                                (apply frp:list* (frp:cdr elts)))))))
  
  (define (frp:list? itm)
    (if (signal:compound? itm)
        (let ([ctnt (signal:compound-content itm)])
          ;        (let ([ctnt (value-now itm)])
          (if (cons? ctnt)
              (frp:list? (cdr ctnt))
              #f))
        (if (signal? itm)
            (frp:if (lift true cons? itm)
                    (frp:list? (frp:cdr itm))
                    (frp:null? itm))
            (or (null? itm)
                (and (cons? itm) (frp:list? (cdr itm)))))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Vector
  
  
  (define (frp:vector . args)
    (if (ormap behavior? args)
        (apply procs->signal:compound
               vector
               (lambda (vec idx)
                 (lambda (x)
                   (vector-set! vec idx x)))
               args)        
        (apply vector args)))
  
  (define (frp:vector-ref v i)
    (cond
      [(signal:compound? v) (vector-ref (signal:compound-content v) i)]
      [(signal? v) (lift #t vector-ref v i)]
      [else (vector-ref v i)]))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; make-struct-type + define-struct Macros
  
  
  (define (frp:make-struct-type name-symbol super-struct-type init-field-k auto-field-k . args)
    (let-values ([(desc ctor pred acc mut)
                  (apply make-struct-type name-symbol super-struct-type init-field-k auto-field-k
                         args)])
      (values
       desc
       (lambda fields
         (if (ormap behavior? fields)
             (apply procs->signal:compound
                    ctor
                    (lambda (strct idx)
                      (lambda (val)
                        (mut strct idx val)))
                    fields)
             (apply ctor fields)))
       (lambda (v) (lift #t pred v)) ; FIX
       acc
       mut)))
  
  (define (frp:make-struct-field-accessor acc i sym)
    (make-accessor (make-struct-field-accessor acc i sym)))
  
  ; FORBIDS MUTATION
  (define (frp:make-struct-field-mutator acc i sym)
    (lambda (s)
      (error "MUTATION NOT ALLOWED IN FrTime STRUCTURES")))
  
  (define-syntax (frp:define-struct stx)
    (syntax-case stx ()
      [(_ (s t) (field ...) insp)
       (let ([field-names (syntax->list #'(field ...))]
             [super-for-gen (if (syntax-e #'t)
                                (string->symbol
                                 (format "struct:~a" (syntax-e #'t)))
                                #f)]
             [super-for-exp (if (syntax-e #'t)
                                #'t
                                #t)])
         #`(begin
             (define-values #,(build-struct-names #'s field-names #f #f stx)
               (parameterize ([current-inspector insp])
                 #,(build-struct-generation #'s field-names #f #f super-for-gen)))
             (define-syntax s
               #,(build-struct-expand-info #'s field-names #f #f super-for-exp
                                           empty empty))))]
      [(_ (s t) (field ...))
       #'(frp:define-struct (s t) (field ...) (current-inspector))]
      [(_ s (field ...) insp)
       #'(frp:define-struct (s #f) (field ...) insp)]
      [(_ s (field ...))
       #'(frp:define-struct (s #f) (field ...) (current-inspector))]))
  
  (define (event? v)
    (and (signal? v)
         (if (undefined? (signal-value v))
             undefined
             (event-cons? (signal-value v)))))
  
  (define (event-receiver? v)
    (and (event? v)
         (procedure-arity-includes? (signal-thunk v) 1)))
  
  (define (behavior? v)
    (and (signal? v) (not (event-cons? (signal-value v)))))
  
  (define (safe-signal-depth v)
    (cond
      [(signal? v) (signal-depth v)]
      [(non-scheduled? v) (signal-depth (non-scheduled-signal v))]
      [0]))
  
  (define-syntax do-in-manager
    (syntax-rules ()
      [(_ expr ...)
       (if (man?)
           (begin expr ...)
           (begin
             (! man (list 'run-thunk (self) (let ([custs (current-custs)])
                                              (lambda ()
                                                (parameterize ([current-custs custs])
                                                  expr ...)))))
             (receive [('val v) v]
                      [('exn e) (raise e)])))]))
  
  ;; mutate! : compound num -> (any -> ())
  (define (procs->signal:compound ctor mutate! . args)
    (do-in-manager
     (let* ([custs (current-custs)]
            [cust-sigs (map ft-cust-signal custs)]
            [value (apply ctor (map value-now/no-copy args))]
            [mutators
             (foldl
              (lambda (arg idx acc)
                (if (signal? arg) ; behavior?
                    (cons (proc->signal
                           (let ([m (mutate! value idx)])
                             (lambda ()
                               (let ([v (value-now/no-copy arg)])
                                 (m v)
                                 v)))
                           arg) acc)
                    acc))
              empty args (build-list (length args) identity))]
            [sig (make-signal:compound
                  value
                  empty
                  #f
                  (lambda () mutators value)
                  (add1 (apply max 0 (append (map safe-signal-depth args)
                                             (map (lambda (s) (+ 0 (safe-signal-depth s)))
                                                  cust-sigs))))
                  (current-continuation-marks)
                  custs
                  args
                  (apply ctor args)
                  (lambda () (apply ctor (map value-now args))))])
       ;(printf "mutators = ~a~n" mutators)
       (when (cons? args)
         (register sig args))
       (when (cons? cust-sigs)
         (register (make-non-scheduled sig) cust-sigs))
       (for-each (lambda (g) (set-ft-cust-constructed-sigs!
                              g (cons sig (ft-cust-constructed-sigs g))))
                 custs)
       sig)))
  
  (define (proc->signal thunk . producers)
    (do-in-manager
     (let* ([custs (current-custs)]
            [cust-sigs (map ft-cust-signal custs)]
            [sig (make-signal
                  undefined empty #t thunk
                  (add1 (apply max 0 (append (map safe-signal-depth producers)
                                             (map safe-signal-depth cust-sigs))))
                  (current-continuation-marks)
                  (current-custs)
                  producers)])
       ;(printf "~a custodians~n" (length custs))
       (when (cons? producers)
         (register sig producers))
       (when (cons? cust-sigs)
         (register (make-non-scheduled sig) cust-sigs))
       (for-each (lambda (g) (set-ft-cust-constructed-sigs!
                              g (cons sig (ft-cust-constructed-sigs g))))
                 custs)
       (iq-enqueue sig)
       sig)))
  
  (define errortrace-key 'drscheme-debug-continuation-mark-key)
  
  (define (install-errortrace-key key)
    (set! key key))
  
  ; messages for signal manager; we now ensure that
  ; only the manager manipulates the dependency graph
  (define-struct reg (inf sup ret))
  (define-struct unreg (inf sup))
  
  ; an external event; contains a list of pairs
  ; (recip val), where val is passed to recip's thunk
  (define-struct external-event (recip-val-pairs))
  
  ; update the given signal at the given time
  (define-struct alarm (time signal))
  
  (define (kill-signal sig)
    ;(printf "killing~n")
    (for-each
     (lambda (prod)
       (unregister sig prod))
     (signal-producers sig))
    (set-signal-thunk! sig void)
    (set-signal-value! sig 'dead)
    (set-signal-dependents! sig empty)
    (set-signal-producers! sig empty)
    (for-each
     (lambda (c)
       (set-ft-cust-constructed-sigs!
        c
        (remq sig (ft-cust-constructed-sigs c))))
     (signal-custodians sig)))
  
  (define (super-lift fun bhvr)
    (do-in-manager
     (let* ([cust (make-ft-cust (void) empty)]
            [custs (cons cust (current-custs))]
            [pfun (lambda (b)
                    (parameterize ([current-custs custs])
                      (fun b)))]
            [current undefined])
       (letrec ([custodian-signal
                 (proc->signal
                  (lambda ()
                    (for-each kill-signal (ft-cust-constructed-sigs cust))
                    (unregister rtn current)
                    (set! current (pfun (value-now bhvr)))
                    (register rtn current)
                    (set-car! (signal-producers rtn) current)
                    (iq-resort))
                  bhvr)]
                [rtn (proc->signal
                      (lambda () custodian-signal (value-now current))
                      current bhvr custodian-signal)])
         (set-ft-cust-signal! cust custodian-signal)
         rtn))))
  
  (define (frp:if-helper test then-thunk else-thunk undef-thunk)
    (let ([if-fun (lambda (b)
                    (cond
                      [(undefined? b) (undef-thunk)]
                      [b (then-thunk)]
                      [else (else-thunk)]))])
      (super-lift if-fun test)))
  
  (define-syntax frp:if
    (syntax-rules ()
      [(_ test-exp then-exp)
       (frp:if test-exp then-exp (void))]
      [(_ test-exp then-exp else-exp)
       (frp:if test-exp then-exp else-exp undefined)]
      [(_ test-exp then-exp else-exp undef-exp)
       (let ([v test-exp])
         (cond
           [(behavior? v) (frp:if-helper
                           v
                           (lambda () then-exp)
                           (lambda () else-exp)
                           (lambda () undef-exp))]
           [(undefined? v) undef-exp]
           [v then-exp]
           [else else-exp]))]))
    
  ; value-now : signal[a] -> a
  (define (value-now val)
    (cond
      [(signal:compound? val) ((signal:compound-copy val))]
      [(signal? val) (signal-value val)]
      [else val]))
  
  (define (value-now/no-copy val)
    (cond
      [(signal? val) (signal-value val)]
      [else val]))
  
  ; no multiple value support
  #;(define (value-now/copy val)
      (if (signal? val)
          (let ([v1 (signal-value val)])
            (if (vector? v1)
                (build-vector (vector-length v1) (lambda (i) (vector-ref v1 i)))
                v1))
          val))
  
  
  ;   (define value-now/copy
  ;     (frp:lambda (val)
  ;       (match val
  ;         [($ signal value _ _ _) (cond
  ;                                     [(cons? value)
  ;                                      (cons (first value) (rest value))]
  ;                                     [(posn? value)
  ;                                      (make-posn (posn-x value) (posn-y value))]
  ;                                     [else value])]
  ;         [_ val])))
  
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
  
  (define-values (undefined undefined?)
    (let-values ([(desc make-undefined undefined? acc mut)
                  (make-struct-type
                   'undefined #f 0 0 #f null frtime-inspector
                   (lambda (fn . args) fn))])
      (values (make-undefined) undefined?)))
  
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
              (apply
               proc->signal
               (apply (if strict? create-strict-thunk create-thunk) fn args)
               args)
              (if (and strict? (ormap undefined? args))
                  undefined
                  (apply fn args))))))
  
  (define (extract k evs)
    (if (cons? evs)
        (let ([ev (first evs)])
          (if (or (eq? ev undefined) (undefined? (erest ev)))
              (extract k (rest evs))
              (begin
                (let ([val (efirst (erest ev))])
                  (set-first! evs (erest ev))
                  (k val)))))))
  
  ; until : behavior behavior -> behavior
  (define (b1 . until . b2)
    (proc->signal
     (lambda () (if (undefined? (value-now b2))
                    (value-now b1)
                    (value-now b2)))
     ; deps
     b1 b2))
  
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
  
  (define (event-producer2 proc . deps)
    (let* ([out (econs undefined undefined)]
           [proc/emit (proc
                       (lambda (val)
                         (set-erest! out (econs val undefined))
                         (set! out (erest out))
                         val))])
      (apply proc->signal (lambda the-args (apply proc/emit the-args) out) deps)))
  
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
      (let ([e-b (hold e init)])
        (rec ret
          (proc->signal
           (case-lambda
             [()
              (when (not (eq? init (signal-value e-b)))
                (unregister ret init)
                (set! init (value-now e-b))
                (register ret init)
                (set-signal-producers! ret (list e-b init))
                (set-signal-depth! ret (max (signal-depth ret)
                                            (add1 (safe-signal-depth init))))
                (iq-resort))
              (value-now init)]
             [(msg) e])
           e-b init)))))
  
  ; event ... -> event
  (define (merge-e . args)
    (apply event-processor
           (lambda (emit)
             (lambda (the-event)
               (emit the-event)))
           args))
  
  (define (once-e e)
    (let ([b true])
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
         (emit (value-now b))))
     b))
  
  (define (event-forwarder sym evt f+l)
    (event-processor
     (lambda (emit)
       (lambda (the-event)
         (for-each (lambda (tid) (! tid (list 'remote-evt sym the-event))) (rest f+l))))
     evt))
  
  ; event-receiver : () -> event
  (define (event-receiver)
    (event-producer2
     (lambda (emit)
       (lambda the-args
         (when (cons? the-args)
           (emit (first the-args)))))))
  
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
  
  (define nothing (void));(string->uninterned-symbol "nothing"))
  
  (define (nothing? v) (eq? v nothing))
  
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
  
  ; event[a] signal[b]* -> event[(list a b*)]
  (define (snapshot-e e . bs)
    (event-processor
     (lambda (emit)
       (lambda (the-event)
         (emit (cons the-event (map value-now bs)))))
     e))
  
  (define (snapshot/apply fn . args)
    (apply (value-now/no-copy fn) (map value-now/no-copy args)))
  
  ; (a b* -> c) event[a] signal[b]* -> event[c]
  (define (snapshot-map-e fn ev . bs)
    (event-processor
     (lambda (emit)
       (lambda (the-event)
         (emit (apply fn the-event (map value-now bs)))))
     ev))
  
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
  
  (define update
    (case-lambda
      [(b) (update0 b)]
      [(b a) (update1 b a)]))
  
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
  
  ; *** will have to change ... ***
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
            (= signal-custodians custs))
       (set-signal-stale?! b #f)
       (let ([new-value (parameterize ([current-custs custs])
                          (thunk))])
         (when (or (signal:compound? b) (not (equal? value new-value)))
           (set-signal-value! b new-value)
           (propagate b)))]
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
  
  (define (undef b)
    (match b
      [(and (? signal?)
            (= signal-value value))
       (set-signal-stale?! b #f)
       (when (not (undefined? value))
         (set-signal-value! b undefined)
         (propagate b))]
      [_ (void)]))
  
  (define named-dependents (make-hash-table))
  
  (define (bind sym evt)
    (! man (list 'bind sym evt))
    evt)
  
  (define (remote-reg tid sym)
    (hash-table-get named-dependents sym
                    (lambda ()
                      (let ([ret (event-receiver)])
                        (hash-table-put! named-dependents sym ret)
                        (! tid (list 'remote-reg man sym))
                        ret))))
  
  (define-values (alarms-enqueue alarms-dequeue-beh alarms-peak-ms alarms-empty?)
    (let ([heap (make-heap (lambda (a b) (< (first a) (first b))) eq?)])
      (values (lambda (ms beh) (heap-insert heap (list ms (make-weak-box beh))))
              (lambda () (match (heap-pop heap) [(_ beh) (weak-box-value beh)]))
              (lambda () (match (heap-peak heap) [(ms _) ms]))
              (lambda () (heap-empty? heap)))))
  
  
  ;; the manager of all signals and event streams
  (define man
    (spawn/name
     'frtime-heart
     (let ([named-providers (make-hash-table)]
           [cur-beh #f]
           [notifications empty])
       (let outer ()
         (with-handlers ([exn:fail?
                          (lambda (exn)
                            (when (and cur-beh
                                       #;(not (undefined? (signal-value cur-beh))))
                              (when (empty? (continuation-mark-set->list
                                             (exn-continuation-marks exn) 'frtime))
                                  (set! exn (make-exn:fail (exn-message exn)
                                                           (signal-continuation-marks
                                                            cur-beh))))
                              ;(raise exn)
                              (iq-enqueue (list exceptions (list exn cur-beh)))
                              (when (behavior? cur-beh)
                                (undef cur-beh)
                                #;(kill-signal cur-beh)))
                            (outer))])
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
                         (with-handlers
                             ([exn:fail? (lambda (exn)
                                           (set! notifications
                                                 (cons (list rtn-pid 'exn exn)
                                                       notifications)))])
                           (set! notifications (cons (list rtn-pid 'val (thunk))
                                                     notifications)))
                         (loop)]
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
                    (set! cur-beh #f)])
                 (loop)))
             
             (for-each (lambda (lst)
                         (! (first lst) (rest lst)))
                       notifications)
             (set! notifications empty)
             
             (inner)))))))
  
  (define man?
    (opt-lambda ([v (self)])
      (eq? v man)))
  
  (define exceptions
    (event-receiver))
  
  (define notifier
    (event-producer2
     (lambda (emit)
       (lambda the-args
         (when (cons? the-args)
           (let ([arg (first the-args)])
             (! (first arg) ((second arg)))))))))
  (set-signal-depth! notifier +inf.0)

  (define dummy
    (proc->signal void))
  
  (define (silly)
    (letrec ([res (proc->signal
                   (let ([x 0]
                         [init (current-milliseconds)])
                     (lambda ()
                       (if (< x 400000)
                           (begin
                             (set! x (+ x 1)))
                           (begin
                             (printf "time = ~a~n" (- (current-milliseconds) init))
                             (set-signal-dependents! res empty)))
                       x)))])
      (set-signal-dependents! res (cons (make-weak-box res) empty))
      (! man res)
      res))
  
  (define (simple-b fn)
    (let ([ret (proc->signal void)])
      (set-signal-thunk! ret (fn ret))
      (set-signal-value! ret ((signal-thunk ret)))
      ret))
  
  (define (send-event rcvr val)
    (! man (make-external-event (list (list rcvr val)))))
  
  (define (send-synchronous-event rcvr val)
    (when (man?)
      (error 'send-synchronous-event "already in frtime engine (would deadlock)"))
    (! man (make-external-event (list (list rcvr val) (list notifier (list (self) (lambda () man))))))
    (receive [(? man?) (void)]))
  
  (define (send-synchronous-events rcvr-val-pairs)
    (when (man?)
      (error 'send-synchronous-events "already in frtime engine (would deadlock)"))
    (unless (ormap list? rcvr-val-pairs) (error "not list"))
    (unless (ormap signal? (map first rcvr-val-pairs)) (error "not signals"))
    (! man (make-external-event (cons (list notifier (list (self) (lambda () man))) rcvr-val-pairs)))
    (receive [(? man?) (void)]))
  
  (define (sync/read . signals)
    (if (man?)
        (apply values (map value-now signals))
        (begin
          (! man (make-external-event (list (list notifier (list (self) (lambda () (map value-now signals)))))))
          (receive [x (apply values x)]))))
  
  (define (schedule-alarm ms beh)
    (when (> ms 1073741824)
      (set! ms (- ms 2147483647)))
    (if (eq? (self) man)
        (alarms-enqueue ms beh)
        (! man (make-alarm ms beh))))
  
  (define-syntax snapshot/sync
    (syntax-rules ()
      [(_ (id ...) expr ...)
       (let-values ([(id ...) (sync/read id ...)])
         expr ...)]))
  
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
  
  (define-syntax frp:send
    (syntax-rules ()
      [(_ obj meth arg ...)
       (if (snap?)
           (send obj meth (value-now arg) ...)
           (send obj meth arg ...))]))
  
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
  
  (define (make-time-b ms)
    (let ([ret (proc->signal void)])
      (set-signal-thunk! ret
                         (lambda ()
                           (let ([t (current-milliseconds)])
                             (schedule-alarm (+ ms t) ret)
                             t)))
      (set-signal-value! ret ((signal-thunk ret)))
      ret))
  
  (define never-e
    (changes #f))
  
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
             [producer (proc->signal
                        (lambda ()
                          (let* ([now (current-milliseconds)]
                                 [ms (value-now ms-b)])
                            (let loop ()
                              (if (or (empty? (rest head))
                                      (< now (+ ms (cdadr head))))
                                  (caar head)
                                  (begin
                                    consumer ;; just to prevent GC
                                    (set! head (rest head))
                                    (loop)))))))]
             [consumer (proc->signal
                        (lambda ()
                          (let* ([now (current-milliseconds)]
                                 [new (value-now beh)]
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
               [producer (proc->signal (lambda ()
                                         consumer ;; just to prevent GC
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
  
  ; new-cell : behavior[a] -> behavior[a] (cell)
  (define new-cell
    (opt-lambda ([init undefined])
      (switch (event-receiver) init)))
  
  ; set-cell! : cell[a] a -> void
  (define (set-cell! ref beh)
    (! man (make-external-event (list (list ((signal-thunk ref) #t) beh)))))
  
  (define (synchronize)
    (when (man?)
      (error 'synchronize "already in frtime engine (would deadlock)"))
    (! man (make-external-event (list (list notifier (list (self) (lambda () man))))))
    (receive [(? man?) (void)]))
  
  (define (curried-apply fn)
    (lambda (lis) (apply fn lis)))
  
  (define-syntax frp:app
    (syntax-rules ()
      [(_ fn arg ...) (lift fn arg ...)]))
  
  (define-syntax frp:letrec
    (syntax-rules ()
      [(_ ([id val] ...) expr ...)
       (let ([id (new-cell)] ...)
         (let ([tmp val])
           (if (signal? tmp)
               (set-cell! id tmp)
               (set! id tmp)))
         ...
         expr ...)]))
  
  (define-syntax frp:match
    (syntax-rules ()
      [(_ expr clause ...) (lift #t (match-lambda clause ...) expr)]))
  
  (define (geometric)
    (- (log (/ (random 2147483647) 2147483647.0))))
  
  (define (make-geometric mean)
    (simple-b (lambda (ret)
                (let ([cur 0])
                  (lambda ()
                    (! man (make-alarm (+ (current-milliseconds)
                                          (inexact->exact (ceiling (* mean (geometric)))))
                                       ret))
                    (set! cur (- 1 cur))
                    cur)))))
  
  (define (make-constant ms)
    (simple-b (lambda (ret)
                (let ([cur 0])
                  (lambda ()
                    (! man (make-alarm (+ (current-milliseconds) ms)
                                       ret))
                    (set! cur (- 1 cur))
                    cur)))))
  
  
  
  (define raise-exceptions (new-cell #t))
  (define exception-raiser
    (exceptions . ==> . (lambda (p) (when (value-now raise-exceptions)
                                      (thread
                                       (lambda () (raise (car p))))))))
  
  
  (define (find pred lst)
    (cond
      [(empty? lst) #f]
      [(pred (first lst)) (first lst)]
      [else (find pred (rest lst))]))
  
  (define-syntax (frp:provide stx)
    (syntax-case stx ()
      [(_ . clauses)
       (foldl
        (lambda (c prev)
          (syntax-case prev ()
            [(begin clause ...)
             (syntax-case c (lifted lifted:nonstrict)
               [(lifted . ids)
                (with-syntax ([(fun-name ...) (syntax ids)]
                              [(tmp-name ...)
                               (map (lambda (id)
                                      (datum->syntax-object stx (syntax-object->datum id)))
                                    (generate-temporaries (syntax ids)))])
                  (syntax
                   (begin
                     clause ...
                     (define (tmp-name . args)
                       (apply lift true fun-name args))
                     ...
                     (provide (rename tmp-name fun-name) ...))))]
               [(lifted:nonstrict . ids)
                (with-syntax ([(fun-name ...) (syntax ids)]
                              [(tmp-name ...)
                               (map (lambda (id)
                                      (datum->syntax-object stx (syntax-object->datum id)))
                                    (generate-temporaries (syntax ids)))])
                  (syntax
                   (begin
                     clause ...
                     (define (tmp-name . args)
                       (apply lift false fun-name args))
                     ...
                     (provide (rename tmp-name fun-name) ...))))]
               [provide-spec
                (syntax (begin clause ... (provide provide-spec)))])]))
        (syntax (begin))
        (syntax->list (syntax clauses)))]))  
  
  (define (ensure-no-signal-args val name)
    (if (procedure? val)
        (lambda args
          (cond
            [(find signal? args)
             =>
             (lambda (v)
               (raise-type-error name "not time-varying"
                                 (if (event? v)
                                     (format "#<event (last: ~a)>" (efirst (signal-value v)))
                                     (format "#<behavior: ~a>" (signal-value v)))))]
            [else (apply val args)]))))
  
  (define-syntax (frp:require stx)
    (define (generate-temporaries/loc st ids)
      (map (lambda (id)
             (datum->syntax-object stx (syntax-object->datum id)))
           (generate-temporaries ids)))
    (syntax-case stx ()
      [(_ . clauses)
       (foldl
        (lambda (c prev)
          (syntax-case prev ()
            [(begin clause ...)
             (syntax-case c (lifted lifted:nonstrict as-is:unchecked as-is frlibs)
               [(lifted:nonstrict module . ids)
                (with-syntax ([(fun-name ...) #'ids]
                              [(tmp-name ...) (generate-temporaries/loc stx #'ids)])
                  #'(begin
                      clause ...
                      (require (rename module tmp-name fun-name) ...)
                      (define (fun-name . args)
                        (apply lift false tmp-name args))
                      ...))]
               [(lifted module . ids)
                (with-syntax ([(fun-name ...) (syntax ids)]
                              [(tmp-name ...) (generate-temporaries/loc stx #'ids)])
                  #'(begin
                      clause ...
                      (require (rename module tmp-name fun-name) ...)
                      (define (fun-name . args)
                        (apply lift true tmp-name args))
                      ...))]
               [(as-is:unchecked module id ...)
                (syntax (begin clause ... (require (rename module id id) ...)))]
               [(as-is module . ids)
                (with-syntax ([(fun-name ...) (syntax ids)]
                              [(tmp-name ...) (generate-temporaries/loc stx #'ids)])
                  #'(begin
                      clause ...
                      (require (rename module tmp-name fun-name) ...)
                      (define fun-name (ensure-no-signal-args tmp-name 'fun-name))
                      ...))]
               [(frlibs str ...)
                #'(begin
                    clause ...
                    (require (lib str "frtime") ...))]
               [require-spec
                #'(begin clause ... (require require-spec))])]))
        #'(begin)
        (syntax->list #'clauses))]))
  
  (define undefined?/lifted (lambda (arg) (lift false undefined? arg)))
  (define frp:null? (lambda (arg) (lift true null? arg)))

  ;(define frp:cons (lambda (a d) (lift false cons a d)))
  ;(define frp:car (lambda (arg) (lift true car arg)))
  ;(define frp:cdr (lambda (arg) (lift true cdr arg)))
  
  (provide module
           #%app
           #%top
           #%datum
           #%plain-module-begin
           #%module-begin
           null
           (rename frp:if if)
           (rename frp:require require)
           (rename frp:provide provide)
           (rename frp:letrec letrec)
           (rename frp:match match)
           (rename frp:cons cons)
           (rename frp:pair? pair?)
           (rename frp:null? null?)
           (rename frp:car car)
           (rename frp:cdr cdr)
           (rename frp:make-struct-type make-struct-type)
           (rename frp:make-struct-field-accessor make-struct-field-accessor)
           (rename frp:vector vector)
           (rename frp:vector-ref vector-ref)
           (rename undefined?/lifted undefined?)
           
           (rename undefined? frp:undefined?)
         ;  (rename frp:eq? eq?)
           
           ;added for quasiquote
           (rename frp:empty? empty?)
           (rename frp:list list)
           (rename frp:list* list*)
           (rename frp:list? list?)
           (rename frp:append append)
           
           (rename frp:define-struct define-struct)
           ;           (rename frp:quasiquote quasiquote)
           ;           (rename frp:qq-append qq-append)
           ;          (rename frp:unquote-splicing unquote-splicing)
           (all-defined-except
            frp:if
            frp:require
            frp:provide
            frp:letrec
            frp:match
            frp:cons
            frp:pair?
            frp:null?
            frp:car
            frp:cdr
            frp:make-struct-type
            frp:make-struct-field-accessor
            frp:vector
            frp:vector-ref
            ;            frp:quasiquote
            ;            frp:qq-append
            ;           frp:unquote-splicing
            undefined?
            undefined?/lifted
            frp:define-struct
            ;            reconstruct
            
            ;added for quasiquote
            frp:list
            frp:list*
            frp:list?
            frp:append
            )))
