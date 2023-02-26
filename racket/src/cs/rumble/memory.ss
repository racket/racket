
(define collect-request (box #f))
(define request-incremental? #f)
(define disable-incremental? #f)

(define (set-collect-handler!)
  (collect-request-handler (lambda ()
                             ;; Handler is called in an unspecified thread with
                             ;; all other threads paused (at some point where interrupts
                             ;; were enabled) and with interrupts disabled
                             (let ([r (unbox collect-request)])
                               (set-box! collect-request #f)
                               (collect/report r)))))

;; Notification can be called in any Chez Scheme thread
(define (set-garbage-collect-notify! proc)
  (set! garbage-collect-notify proc))

(define garbage-collect-notify
  (lambda (gen pre-allocated pre-allocated+overhead pre-time re-cpu-time
               post-allocated post-allocated+overhead
               proper-post-time proper-post-cpu-time
               post-time post-cpu-time)
    (void)))

;; #f or a procedure that accepts a CPSed `compute-size-increments` to be
;; called in any Chez Scheme thread (with all other threads paused)
;; after each major GC; this procedure must not do anything that might
;; use "control.ss":
(define reachable-size-increments-callback #f)

(define (set-reachable-size-increments-callback! proc)
  (set! reachable-size-increments-callback proc))

(define allocating-places 1)
(define (collect-trip-for-allocating-places! delta)
  (with-global-lock
   (set! allocating-places (+ allocating-places delta))
   (collect-trip-bytes (* allocating-places (* 8 1024 1024)))))

;; Replicate the counting that `(collect)` would do
;; so that we can report a generation to the notification
;; callback
(define gc-counter 1)
(define log-collect-generation-radix 2)
(define collect-generation-radix-mask (sub1 (bitwise-arithmetic-shift 1 log-collect-generation-radix)))

;; Some allocation patterns create a lot of overhead (i.e., wasted
;; pages in the allocator), so we need to detect that and force a GC.
;; Other patterns don't have as much overhead, so triggering only
;; on total size with overhead can increase peak memory use too much.
;; Trigger a GC when either the non-overhead or with-overhead counts
;; grows enough.
(define GC-TRIGGER-FACTOR (* 8 1024)) ; this times the square root of memory use is added to memory use
(define trigger-major-gc-allocated (* 32 1024 1024))
(define trigger-major-gc-allocated+overhead (* 64 1024 1024))
(define non-full-gc-counter 0)

;; Called in any thread with all other threads paused. The Racket
;; thread scheduler may be in atomic mode. In fact, the engine
;; and control layer may be in uninterrupted mode, so don't
;; do anything that might use "control.ss" (especially in logging).
(define (collect/report g)
  ;; If you get "$collect-rendezvous: cannot return to the collect-request-handler", then
  ;; probably something here is trying to raise an exception. To get more information try
  ;; uncommenting the exception-handler line below, and then move its last close parenthesis
  ;; to the end of the enclosing function.
  #;(guard (x [else (if (condition? x) (display-condition x) (#%write x)) (#%newline) (#%flush-output-port)]))
  (let ([this-counter (if g (bitwise-arithmetic-shift-left 1 (* log-collect-generation-radix g)) gc-counter)]
        [pre-allocated (bytes-allocated)]
        [pre-allocated+overhead (current-memory-bytes)]
        [pre-time (current-inexact-milliseconds)]
        [pre-cpu-time (cpu-time)])
    (if (> (add1 this-counter) (bitwise-arithmetic-shift-left 1 (* log-collect-generation-radix (sub1 (collect-maximum-generation)))))
        (set! gc-counter 1)
        (set! gc-counter (add1 this-counter)))
    (let* ([req-gen (cond
                      [(and (not g)
                            (or (>= pre-allocated trigger-major-gc-allocated)
                                (>= pre-allocated+overhead trigger-major-gc-allocated+overhead)
                                (>= non-full-gc-counter 10000)))
                       ;; Force a major collection if memory use has doubled
                       (collect-maximum-generation)]
                      [else
                       ;; Find the minor generation implied by the counter
                       (let loop ([c this-counter] [gen 0])
                         (cond
                           [(zero? (bitwise-and c collect-generation-radix-mask))
                            (loop (bitwise-arithmetic-shift-right c log-collect-generation-radix) (add1 gen))]
                           [else gen]))])]
           [gen (cond
                  [(and (= req-gen (collect-maximum-generation))
                        (not (in-original-host-thread?))
                        (fxpositive? (hashtable-size collect-callbacks)))
                   ;; Defer a major collection to the main thread
                   (async-callback-queue-major-gc!)
                   0]
                  [else req-gen])])
      (run-collect-callbacks car)
      (let ([maybe-finish-accounting
             (cond
               [(and reachable-size-increments-callback
                     (fx= gen (collect-maximum-generation)))
                ;; Collect with a fused `collect-size-increments`
                (reachable-size-increments-callback
                 (lambda (roots domains k)
                   (cond
                     [(null? roots)
                      ;; Plain old collection, after all:
                      (collect gen 1 gen)
                      #f]
                     [else
                      (let ([domains (weaken-accounting-domains domains)])
                        ;; Accounting collection:
                        (let ([counts (collect gen 1 gen (weaken-accounting-roots roots))])
                          (lambda ()
                            (call-with-accounting-domains k counts domains))))])))]
               [(and request-incremental?
                     (fx= gen (sub1 (collect-maximum-generation))))
                ;; "Incremental" mode by not promoting to the maximum generation
                (collect gen 1 gen)
                #f]
               [(fx= gen 0)
                ;; Plain old minor collection:
                (collect 0 1 1)
                #f]
               [(fx= gen (collect-maximum-generation))
                ;; Plain old major collection:
                (collect gen 1 gen)
                #f]
               [else
                ;; Plain old collection that does not necessairy promote to `gen`+1:
                (collect gen 1 (fx+ gen 1))
                #f])])
        (when (fx= gen (collect-maximum-generation))
          (set! request-incremental? #f))
        (let ([post-allocated (bytes-allocated)]
              [post-allocated+overhead (current-memory-bytes)]
              [post-time (current-inexact-milliseconds)]
              [post-cpu-time (cpu-time)])
          (when (= gen (collect-maximum-generation))
            ;; Trigger a major GC when memory use is a certain factor of current use.
            ;;
            ;; The factor is based on the square root of current memory use, which is intended
            ;;  to make Racket a good citizen in its environment and maximize cooperation among
            ;;  different processes.[1] We do not attempt to measure allocation and collection
            ;;  rates, as in the paper describing this rule. Instead, we simplify by assuming
            ;;  that that ratio of rates is relatively constant.
            ;;   [1] Kirisame, Shenoy, and Panchekha (2022) https://arxiv.org/abs/2204.10455
            ;;
            ;; A factor on `post-allocated+overhead` seems to be too long a wait, because
            ;;  that value may include underused pages that have locked objects.
            ;;  Using just `post-allocated` is too small, because it may force an
            ;;  immediate major GC too soon. So, watch both.
            (let ([scale (lambda (n)
                           (+ n (inexact->exact (floor (* (sqrt (max 0 n)) GC-TRIGGER-FACTOR)))))])
              (set! trigger-major-gc-allocated (scale (- post-allocated (bytes-finalized))))
              (set! trigger-major-gc-allocated+overhead (scale post-allocated+overhead))))
          (update-eq-hash-code-table-size!)
          (update-struct-procs-table-sizes!)
          (poll-foreign-guardian)
          (when maybe-finish-accounting
            (maybe-finish-accounting))
          (run-collect-callbacks cdr)
          (garbage-collect-notify gen
                                  pre-allocated pre-allocated+overhead pre-time pre-cpu-time
                                  post-allocated post-allocated+overhead post-time post-cpu-time
                                  (current-inexact-milliseconds) (cpu-time)))
        (when (and (= req-gen (collect-maximum-generation))
                   (currently-in-engine?))
          ;; This `set-timer` doesn't necessarily penalize the right thread,
          ;; but it's likely to penalize a thread that is allocating quickly:
          (set-timer 1))
        (cond
          [(= gen (collect-maximum-generation))
           (set! non-full-gc-counter 0)]
          [else
           (set! non-full-gc-counter (add1 non-full-gc-counter))])
        (void)))))

(define collect-garbage
  (case-lambda
   [() (collect-garbage 'major)]
   [(request)
    (cond
     [(eq? request 'incremental)
      (unless disable-incremental?
        (set! request-incremental? #t))]
     [else
      (let ([req (case request
                   [(minor) 0]
                   [(major) (collect-maximum-generation)]
                   [else
                    (raise-argument-error 'collect-garbage
                                          "(or/c 'major 'minor 'incremental)"
                                          request)])])
        (let loop ()
          (let ([current-req (unbox collect-request)])
            (unless (#%box-cas! collect-request current-req (max req (or current-req 0)))
              (loop))))
        (collect-rendezvous))])]))

(define current-memory-use
  (case-lambda
   [() (bytes-allocated)]
   [(mode)
    (cond
     [(not mode) (bytes-allocated)]
     [(eq? mode 'cumulative) (with-interrupts-disabled
                              (+ (bytes-deallocated) (bytes-allocated)))]
     ;; must be a custodian; hook is reposnsible for complaining if not
     [else (custodian-memory-use mode (bytes-allocated))])]))

(define custodian-memory-use (lambda (mode all) all))
(define (set-custodian-memory-use-proc! proc) (set! custodian-memory-use proc))

(define immediate-allocation-check (lambda (n) (void)))
(define (set-immediate-allocation-check-proc! proc) (set! immediate-allocation-check proc))

(define (guard-large-allocation who what len size)
  (when (exact-nonnegative-integer? len)
    (let ([n (* len size)])
      (unless (fixnum? n)
        (raise (|#%app|
                exn:fail:out-of-memory
                (#%format "out of memory making ~a\n  length: ~a"
                          what len)
                (current-continuation-marks))))
      (immediate-allocation-check n)
      ;; Watch out for radiply growing memory use that isn't captured
      ;; fast enough by regularly scheduled event checking because it's
      ;; allocated in large chunks
      (when (>= (bytes-allocated) trigger-major-gc-allocated)
        (set-timer 1)))))

(define (set-incremental-collection-enabled! on?)
  (set! disable-incremental? (not on?)))

;; ----------------------------------------

(define (weaken-accounting-roots roots)
  (let loop ([roots roots])
    (cond
      [(null? roots) '()]
      [else
       (let ([root (car roots)]
             [rest (loop (cdr roots))])
         (cond
           [(thread? root) (cons root rest)]
           [else
            (weak-cons root rest)]))])))

;; We want the elements of `domains` to be available for
;; finalization, so refer to all of them weakly
(define (weaken-accounting-domains domains)
  (let loop ([domains domains])
    (if (null? domains)
        '()
        (weak-cons (car domains) (loop (cdr domains))))))

;; Filter any domain (and associated count) that went to `#!bwp` during collection
(define (call-with-accounting-domains k counts domains)
  (let loop ([counts counts] [domains domains] [r-counts '()] [r-domains '()])
    (cond
      [(null? counts) (k (reverse r-counts) (reverse r-domains))]
      [(eq? #!bwp (car domains)) (loop (cdr counts) (cdr domains) r-counts r-domains)]
      [else (loop (cdr counts) (cdr domains)
                  (cons (car counts) r-counts) (cons (car domains) r-domains))])))

;; ----------------------------------------

(define prev-stats-objects #f)

(define/who (dump-memory-stats . args)
  (let-values ([(backtrace-predicate flags max-path-length every-n)
                (parse-dump-memory-stats-arguments who args)])
    (enable-object-counts #t)
    (enable-object-backreferences (and backtrace-predicate #t))
    (collect-garbage)
    (let* ([counts (object-counts)]
           [backreferences (object-backreferences)]
           [extract (lambda (static? cxr)
                      (lambda (c) (if (or static? (not (eq? (car c) 'static)))
                                      (cxr c)
                                      0)))]
           [get-count (lambda (static?) (lambda (e) (apply + (map (extract static? cadr) (cdr e)))))]
           [get-bytes (lambda (static?) (lambda (e) (apply + (map (extract static? cddr) (cdr e)))))]
           [pad (lambda (s n)
                  (string-append (#%make-string (max 0 (- n (string-length s))) #\space) s))]
           [pad-right (lambda (s n)
                        (string-append s (#%make-string (max 0 (- n (string-length s))) #\space)))]
           [commas (lambda (n)
                     (let* ([l (string->list (number->string n))]
                            [len (length l)])
                       (list->string
                        (cons #\space
                              (let loop ([l l] [len len])
                                (cond
                                 [(<= len 3) l]
                                 [else
                                  (let ([m (modulo len 3)])
                                    (case m
                                      [(0) (list* (car l)
                                                  (cadr l)
                                                  (caddr l)
                                                  #\,
                                                  (loop (cdddr l) (- len 3)))]
                                      [(2) (list* (car l)
                                                  (cadr l)
                                                  #\,
                                                  (loop (cddr l) (- len 2)))]
                                      [else  (list* (car l)
                                                    #\,
                                                    (loop (cdr l) (- len 1)))]))]))))))]
           [count-width 11]
           [size-width 13]
           [trim-type (lambda (s)
                        (let ([len (string-length s)])
                          (cond
                           [(and (> len 14)
                                 (string=? (substring s 2 14) "record type "))
                            (string-append "#<" (substring s 14 len))]
                           [else s])))]
           [layout (lambda args
                     (let loop ([args args] [actual-col 0] [want-col 0])
                       (cond
                        [(null? args) "\n"]
                        [(< actual-col want-col)
                         (string-append (#%make-string (- want-col actual-col) #\space)
                                        (loop args want-col want-col))]
                        [(integer? (car args))
                         (loop (cons (pad (commas (car args))
                                          (- (+ want-col (sub1 (cadr args)))
                                             actual-col))
                                     (cdr args))
                               actual-col
                               want-col)]
                        [else
                         (string-append (car args)
                                        (loop (cddr args)
                                              (+ actual-col (string-length (car args)))
                                              (+ want-col (cadr args))))])))]
           [layout-line (lambda (label c1 s1 c2 s2)
                          (layout " " 1
                                  (trim-type label) 22
                                  c1 count-width
                                  s1 size-width
                                  " | " 3
                                  c2 count-width
                                  s2 size-width))]
           [use-prev? (#%memq 'new flags)]
           [skip-counts? (#%memq 'only flags)])
      (enable-object-counts #f)
      (enable-object-backreferences #f)
      (#%fprintf (current-error-port) "Begin Dump\n")
      (#%fprintf (current-error-port) "Current memory use: ~a\n" (bytes-allocated))
      (when (#%memq 'help flags)
        (let ([lines (lambda strs
                       (for-each (lambda (str)
                                   (#%fprintf (current-error-port) str)
                                   (#%newline (current-error-port)))
                                 strs))])
          (lines "Begin Help"
                 "  (dump-memory-stats <spec> <modifier> ...)"
                 "    where <spec> shows paths to objects:"
                 "      <spec> = <symbol>"
                 "             | <predicate-procedure>"
                 "             | (make-weak-box <val>)"
                 "             | (list 'struct <symbol>)"
                 "    and <modifier> controls that output:"
                 "       <modifier> = 'new        ; only trace new since last dump"
                 "                  | 'max-path <exact-nonnegative-integer>"
                 "                  | 'every <exact-positive-integer> ; show a subset"
                 "                  | 'only       ; skip table of object counts"
                 "End Help")))
      (unless skip-counts?
        (#%fprintf (current-error-port) "Begin RacketCS\n")
        (for-each (lambda (e)
                    (chez:display (layout-line (chez:format "~a" (car e))
                                               ((get-count #f) e) ((get-bytes #f) e)
                                               ((get-count #t) e) ((get-bytes #t) e))
				  (current-error-port)))
                  (list-sort (lambda (a b) (< ((get-bytes #f) a) ((get-bytes #f) b))) counts))
        (#%fprintf (current-error-port) (layout-line "total"
                                                     (apply + (map (get-count #f) counts))
                                                     (apply + (map (get-bytes #f) counts))
                                                     (apply + (map (get-count #t) counts))
                                                     (apply + (map (get-bytes #t) counts))))
        (#%fprintf (current-error-port) "End RacketCS\n"))
      (when backtrace-predicate
        (when (and use-prev? (not prev-stats-objects))
          (set! prev-stats-objects (make-weak-eq-hashtable)))
        (let ([backreference-ht (make-eq-hashtable)])
          (for-each (lambda (l)
                      (for-each (lambda (p)
                                  (eq-hashtable-set! backreference-ht (car p) (cdr p)))
                                l))
                    backreferences)
          (#%fprintf (current-error-port) "Begin Traces\n")
          (let ([prev-trace (box '())]
                [count-n 0])
            (for-each (lambda (l)
                        (for-each (lambda (p)
                                    (when (backtrace-predicate (car p))
                                      (set! count-n (add1 count-n))
                                      (unless (or (< count-n every-n)
                                                  (and use-prev?
                                                       (eq-hashtable-ref prev-stats-objects (car p) #f)))
                                        (set! count-n 0)
                                        (when use-prev?
                                          (eq-hashtable-set! prev-stats-objects (car p) #t))
                                        (unless (eqv? 0 max-path-length)
                                          (#%printf "*== ~a" (object->backreference-string (car p)))
                                          (let loop ([prev (car p)] [o (cdr p)] [accum '()] [len (sub1 (or max-path-length +inf.0))])
                                            (cond
                                             [(zero? len) (void)]
                                             [(not o) (set-box! prev-trace (reverse accum))]
                                             [(and (not (null? o))
                                                   (#%memq o (unbox prev-trace)))
                                              => (lambda (l)
                                                   (#%printf " <- DITTO\n")
                                                   (set-box! prev-trace (append (reverse accum) l)))]
                                             [else
                                              (#%printf " <- ~a" (object->backreference-string
                                                                  (cond
                                                                   [(and (pair? o)
                                                                         (eq? prev (car o)))
                                                                    (cons 'PREV (cdr o))]
                                                                   [(and (pair? o)
                                                                         (eq? prev (cdr o)))
                                                                    (cons (car o) 'PREV)]
                                                                   [else o])))
                                              (loop o (eq-hashtable-ref backreference-ht o #f) (cons o accum) (sub1 len))]))))))
                                  l))
                      backreferences))
          (#%fprintf (current-error-port) "End Traces\n")))
      (#%fprintf (current-error-port) "End Dump\n"))))

(define (parse-dump-memory-stats-arguments who args)
  (cond
   [(null? args)
    (values #f  ; predicate
            '() ; flags
            #f  ; max-path-length
            1)] ; every-n
   [else
    (let ([predicate
           (let ([arg (car args)])
             (case arg
               [(help) #f]
               [(struct) #f]
               [(code) #%$code?]
               [(procedure) #%procedure?]
               [(ephemeron) ephemeron-pair?]
               [(bignum) bignum?]
               [(vector) #%vector?]
               [(box) #%box?]
               [(stencil-vector) stencil-vector?]
               [(keyword) keyword?]
               [(string) string?]
               [(symbol) symbol?]
               [(weakpair) weak-pair?]
               [(<ffi-lib>) ffi-lib?]
               [(<will-executor>) will-executor?]
               [(metacontinuation-frame) metacontinuation-frame?]
               [else
                (cond
                 [(and (#%procedure? arg)
                       (procedure-arity-includes? arg 1))
                  arg]
                 [(symbol? arg) (make-struct-name-predicate arg)]
                 [(and (#%list? arg)
                       (fx= 2 (length arg))
                       (eq? (car arg) 'struct)
                       (symbol? (cadr arg)))
                  (make-struct-name-predicate (cadr arg))]
                 [(weak-box? arg)
                  (let ([v (weak-cons (weak-box-value arg) #f)])
                    (lambda (o) (eq? o (car v))))]
                 [else
                  (raise-arguments-error who "unrecognized predicate;\n try 'help for more information"
                                         "given" arg)])]))])
      (let loop ([args (cdr args)] [flags (if (eq? 'help (car args)) '(help) '())] [max-path-length #f] [every-n 1])
        (cond
         [(null? args)
          (values predicate flags max-path-length every-n)]
         [(eq? (car args) 'new)
          (loop (cdr args) (cons 'new flags) max-path-length every-n)]
         [(eq? (car args) 'only)
          (loop (cdr args) (cons 'only flags) max-path-length every-n)]
         [(eq? (car args) 'help)
          (loop (cdr args) (cons 'help flags) max-path-length every-n)]
         [(eq? (car args) 'max-path)
          (when (null? (cdr args))
            (raise-arguments-error who "missing argument for 'max-path"))
          (let ([max-path-length (cadr args)])
            (unless (exact-nonnegative-integer? max-path-length)
              (raise-arguments-error who "bad 'max-path value" "given" max-path-length))
            (loop (cddr args) flags max-path-length every-n))]
         [(eq? (car args) 'every)
          (when (null? (cdr args))
            (raise-arguments-error who "missing argument for 'every"))
          (let ([every-n (cadr args)])
            (unless (exact-positive-integer? every-n)
              (raise-arguments-error who "bad 'every value" "given" every-n))
            (loop (cddr args) flags max-path-length every-n))]
         [else
          (raise-arguments-error who "unrecognized argument;\n try 'help for more information" "given" (car args))])))]))

(define (make-struct-name-predicate name)
  (lambda (o)
    (and (#%record? o)
         (let ([rtd (#%record-rtd o)])
           (eq? name (#%record-type-name rtd))))))

(define (object->backreference-string o)
  (parameterize ([print-level 3])
    (let ([s (call-with-string-output-port
              (lambda (dest)
                (pretty-print o dest)))])
      (if (> (string-length s) 256)
          (let ([s (substring s 0 256)])
            (string-set! s 252 #\.)
            (string-set! s 253 #\.)
            (string-set! s 254 #\.)
            (string-set! s 255 #\newline)
            s)
          s))))

;; ----------------------------------------

(define-record-type (phantom-bytes create-phantom-bytes phantom-bytes?)
  (fields pbv)
  (sealed #t))

(define/who (make-phantom-bytes k)
  (check who exact-nonnegative-integer? k)
  (guard-large-allocation who "byte string" k 1)
  (create-phantom-bytes (make-phantom-bytevector k)))

(define/who (set-phantom-bytes! phantom-bstr k)
  (check who phantom-bytes? phantom-bstr)
  (check who exact-nonnegative-integer? k)
  (when (> k (phantom-bytevector-length (phantom-bytes-pbv phantom-bstr)))
    (guard-large-allocation who "byte string" k 1))
  (set-phantom-bytevector-length! (phantom-bytes-pbv phantom-bstr) k))

;; ----------------------------------------

;; Weak table of (cons <pre> <post>) keys, currently supported
;; only in the original host thread of the original place
(define collect-callbacks (make-weak-eq-hashtable))

(define (unsafe-add-collect-callbacks pre post)
  (when (in-original-host-thread?)
    (let ([p (cons pre post)])
      (with-interrupts-disabled
       (hashtable-set! collect-callbacks p #t))
      p)))

(define (unsafe-remove-collect-callbacks p)
  (when (in-original-host-thread?)
    (with-interrupts-disabled
     (hashtable-delete! collect-callbacks p))))

;; Called during collection in a thread with all others stopped; currently
;; we run callbacks only if the main thread gets to perform the GC, which
;; is often enough to be useful for flashing a GC icon
(define (run-collect-callbacks sel)
  (when (in-original-host-thread?)
    (let loop ([l (vector->list (hashtable-keys collect-callbacks))])
      (unless (null? l)
        (let ([v (sel (car l))])
          (let loop ([i 0] [save #f])
            (unless (fx= i (#%vector-length v))
              (loop (fx+ i 1)
                    (run-one-collect-callback (#%vector-ref v i) save sel))))
          (loop (cdr l)))))))

(define-syntax (osapi-foreign-procedure stx)
  (syntax-case stx ()
    [(_ s ...)
     (case (machine-type)
       [(i3nt ti3nt) #'(foreign-procedure __stdcall s ...)]
       [else #'(foreign-procedure s ...)])]))

;; This is an inconvenient callback interface, certainly, but it
;; accommodates a limitation of the traditional Racket implementation
(define (run-one-collect-callback v save sel)
  (let ([protocol (#%vector-ref v 0)]
        [proc (cpointer-address (#%vector-ref v 1))]
        [ptr (lambda (i)
               (cpointer*-address (#%vector-ref v (fx+ 2 i))))]
        [val (lambda (i)
               (#%vector-ref v (fx+ 2 i)))])
    (case protocol
      [(int->void)
       ((foreign-procedure proc (int) void) (val 0))
       save]
      [(ptr_ptr_ptr_int->void)
       ((foreign-procedure proc (void* void* void* int) void) (ptr 0) (ptr 1) (ptr 2) (val 3))
       save]
      [(ptr_ptr->save)
       ((foreign-procedure proc (void* void*) void*) (ptr 0) (ptr 1))]
      [(save!_ptr->void)
       (and save (not (eqv? save 0))
            ((foreign-procedure proc (void* void*) void*) save (ptr 0))
            save)]
      [(ptr_ptr_ptr->void)
       ((foreign-procedure proc (void* void* void*) void) (ptr 0) (ptr 1) (ptr 2))
       save]
      [(ptr_ptr_float->void)
       ((foreign-procedure proc (void* void* float) void) (ptr 0) (ptr 1) (val 2))
       save]
      [(ptr_ptr_double->void)
       ((foreign-procedure proc (void* void* double) void) (ptr 0) (ptr 1) (val 2))
       save]
      [(float_float_float_float->void)
       ((foreign-procedure proc (float float float float) void) (val 0) (val 1) (val 2) (val 3))
       save]
      [(ptr_ptr_ptr_int_int_int_int_int_int_int_int_int->void)
       ((foreign-procedure proc (void* void* void* int int int int int int int int int) void)
        (ptr 0) (ptr 1) (ptr 2)
        (val 3) (val 4) (val 5) (val 6)
        (val 7) (val 8) (val 9) (val 10) (val 11))
       save]
      [(osapi_ptr_ptr->void)
       ((osapi-foreign-procedure proc (void* void*) void) (ptr 0) (ptr 1))
       save]
      [(osapi_ptr_int->void)
       ((osapi-foreign-procedure proc (void* int) void) (ptr 0) (val 1))
       save]
      [(osapi_ptr_int_int_int_int_ptr_int_int_long->void)
       ((osapi-foreign-procedure proc (void* int int int int void* int int long) void)
        (ptr 0) (val 1) (val 2) (val 3) (val 4)
        (ptr 5) (val 6) (val 7) (val 8))
       save]
      [else
       (eprintf "unrecognized collect-callback protocol: ~s\n" protocol)
       save])))
