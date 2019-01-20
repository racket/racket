
(define collect-request (box #f))

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
               post-allocated post-allocated+overhead post-time post-cpu-time)
    (void)))

;; #f or a procedure that accepts `compute-size-increments` to be
;; called in any Chez Scheme thread (with all other threads paused)
;; after each major GC; this procedure must not do anything that might
;; use "control.ss":
(define reachable-size-increments-callback #f)

(define (set-reachable-size-increments-callback! proc)
  (set! reachable-size-increments-callback proc))

;; Replicate the counting that `(collect)` would do
;; so that we can report a generation to the notification
;; callback
(define gc-counter 1)
(define log-collect-generation-radix 2)
(define collect-generation-radix-mask (sub1 (bitwise-arithmetic-shift 1 log-collect-generation-radix)))
(define allocated-after-major (* 32 1024 1024))

;; Called in any thread with all other threads paused. The Racket
;; thread scheduler may be in atomic mode. In fact, the engine
;; and control layer may be in uninterrupted mode, so don't
;; do anything that might use "control.ss" (especially in logging).
(define (collect/report g)
  (let ([this-counter (if g (bitwise-arithmetic-shift-left 1 (* log-collect-generation-radix g)) gc-counter)]
        [pre-allocated (bytes-allocated)]
        [pre-allocated+overhead (current-memory-bytes)]
        [pre-time (real-time)] 
        [pre-cpu-time (cpu-time)])
    (if (> (add1 this-counter) (bitwise-arithmetic-shift-left 1 (* log-collect-generation-radix (sub1 (collect-maximum-generation)))))
        (set! gc-counter 1)
        (set! gc-counter (add1 this-counter)))
    (let ([gen (cond
                [(and (not g)
                      (>= pre-allocated (* 2 allocated-after-major)))
                 ;; Force a major collection if memory use has doubled
                 (collect-maximum-generation)]
                [else
                 ;; Find the minor generation implied by the counter
                 (let loop ([c this-counter] [gen 0])
                   (cond
                    [(zero? (bitwise-and c collect-generation-radix-mask))
                     (loop (bitwise-arithmetic-shift-right c log-collect-generation-radix) (add1 gen))]
                    [else gen]))])])
      (run-collect-callbacks car)
      (collect gen)
      (let ([post-allocated (bytes-allocated)])
        (when (= gen (collect-maximum-generation))
          (set! allocated-after-major post-allocated))
        (garbage-collect-notify gen
                                pre-allocated pre-allocated+overhead pre-time pre-cpu-time
                                post-allocated  (current-memory-bytes) (real-time) (cpu-time)))
      (poll-foreign-guardian)
      (run-collect-callbacks cdr)
      (when (and reachable-size-increments-callback
                 (fx= gen (collect-maximum-generation)))
        (reachable-size-increments-callback compute-size-increments)))))

(define collect-garbage
  (case-lambda
   [() (collect-garbage 'major)]
   [(request)
    (cond
     [(eq? request 'incremental)
      (void)]
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
     [(eq? mode 'cumulative) (sstats-bytes (statistics))]
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
      (immediate-allocation-check n))))

(define prev-stats-objects #f)

(define (dump-memory-stats . args)
  (let-values ([(backtrace-predicate use-prev? max-path-length) (parse-dump-memory-stats-arguments args)])
    (enable-object-counts #t)
    (enable-object-backreferences (and backtrace-predicate #t))
    (collect (collect-maximum-generation))
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
                                  s2 size-width))])
      (enable-object-counts #f)
      (enable-object-backreferences #f)
      (chez:fprintf (current-error-port) "Begin Dump\n")
      (chez:fprintf (current-error-port) "Current memory use: ~a\n" (bytes-allocated))
      (unless (#%memq 'only args)
        (chez:fprintf (current-error-port) "Begin RacketCS\n")
        (for-each (lambda (e)
                    (chez:fprintf (current-error-port)
                                  (layout-line (chez:format "~a" (car e))
                                               ((get-count #f) e) ((get-bytes #f) e)
                                               ((get-count #t) e) ((get-bytes #t) e))))
                  (list-sort (lambda (a b) (< ((get-bytes #f) a) ((get-bytes #f) b))) counts))
        (chez:fprintf (current-error-port) (layout-line "total"
                                                        (apply + (map (get-count #f) counts))
                                                        (apply + (map (get-bytes #f) counts))
                                                        (apply + (map (get-count #t) counts))
                                                        (apply + (map (get-bytes #t) counts))))
        (chez:fprintf (current-error-port) "End RacketCS\n"))
      (when backtrace-predicate
        (when (and use-prev? (not prev-stats-objects))
          (set! prev-stats-objects (make-weak-eq-hashtable)))
        (let ([backreference-ht (make-eq-hashtable)])
          (for-each (lambda (l)
                      (for-each (lambda (p)
                                  (hashtable-set! backreference-ht (car p) (cdr p)))
                                l))
                    backreferences)
          (chez:fprintf (current-error-port) "Begin Traces\n")
          (let ([prev-trace (box '())])
            (for-each (lambda (l)
                        (for-each (lambda (p)
                                    (when (backtrace-predicate (car p))
                                      (unless (and use-prev?
                                                   (hashtable-ref prev-stats-objects (car p) #f))
                                        (when use-prev?
                                          (hashtable-set! prev-stats-objects (car p) #t))
                                        (unless (eqv? 0 max-path-length)
                                          (chez:printf "*== ~a" (object->backreference-string (car p)))
                                          (let loop ([prev (car p)] [o (cdr p)] [accum '()] [len (sub1 (or max-path-length +inf.0))])
                                            (cond
                                             [(zero? len) (void)]
                                             [(not o) (set-box! prev-trace (reverse accum))]
                                             [(chez:memq o (unbox prev-trace))
                                              => (lambda (l)
                                                   (chez:printf " <- DITTO\n")
                                                   (set-box! prev-trace (append (reverse accum) l)))]
                                             [else
                                              (chez:printf " <- ~a" (object->backreference-string
                                                                     (cond
                                                                      [(and (pair? o)
                                                                            (eq? prev (car o)))
                                                                       (cons 'PREV (cdr o))]
                                                                      [(and (pair? o)
                                                                            (eq? prev (cdr o)))
                                                                       (cons (car o) 'PREV)]
                                                                      [else o])))
                                              (loop o (hashtable-ref backreference-ht o #f) (cons o accum) (sub1 len))]))))))
                                  l))
                      backreferences))
          (chez:fprintf (current-error-port) "End Traces\n")))
      (chez:fprintf (current-error-port) "End Dump\n"))))

(define (parse-dump-memory-stats-arguments args)
  (values
   ;; backtrace predicate:
   (cond
    [(null? args) #f]
    [(eq? (car args) 'struct) #f]
    [(and (list? (car args))
          (= 2 (length (car args)))
          (eq? (caar args) 'struct)
          (symbol? (cadar args)))
     (let ([struct-name (cadar args)])
       (lambda (o)
         (and (#%$record? o)
              (eq? (record-type-name (#%$record-type-descriptor o)) struct-name))))]
    [(weak-box? (car args))
     (let ([v (weak-box-value (car args))])
       (lambda (o) (eq? o v)))]
    [(eq? 'code (car args))
     #%$code?]
    [(eq? 'procedure (car args))
     #%procedure?]
    [(eq? 'ephemeron (car args))
     ephemeron-pair?]
    [(symbol? (car args))
     #f
     ;; This is disaterously slow, so don't try it:
     #;
     (let ([type (car args)])
       (lambda (o)
         (eq? ((inspect/object o) 'type) type)))]
    [else #f])
   ;; 'new mode for backtrace?
   (and (pair? args)
        (pair? (cdr args))
        (eq? 'new (cadr args)))
   ;; max path length
   (and (pair? args)
        (pair? (cdr args))
        (or (and (exact-nonnegative-integer? (cadr args))
                 (cadr args))
            (and (pair? (cddr args))
                 (exact-nonnegative-integer? (caddr args))
                  (caddr args))))))

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
  (fields pbv))

(define/who (make-phantom-bytes k)
  (check who exact-nonnegative-integer? k)
  (let ([ph (create-phantom-bytes (make-phantom-bytevector k))])
    (when (>= (bytes-allocated) (* 2 allocated-after-major))
      (collect-garbage))
    ph))

(define/who (set-phantom-bytes! phantom-bstr k)
  (check who phantom-bytes? phantom-bstr)
  (check who exact-nonnegative-integer? k)
  (set-phantom-bytevector-length! (phantom-bytes-pbv phantom-bstr) k))

;; ----------------------------------------

;; List of (cons <pre> <post>), currently suported
;; only in the original host thread of the original place
(define collect-callbacks '())

(define (unsafe-add-collect-callbacks pre post)
  (when (in-original-host-thread?)
    (let ([p (cons pre post)])
      (with-interrupts-disabled
       (set! collect-callbacks (cons p collect-callbacks)))
      p)))

(define (unsafe-remove-collect-callbacks p)
  (when (in-original-host-thread?)
    (with-interrupts-disabled
     (set! collect-callbacks (#%remq p collect-callbacks)))))

(define (run-collect-callbacks sel)
  (when (in-original-host-thread?)
    (let loop ([l collect-callbacks])
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
;; accomodates a limitatuon of the traditional Racket implementation
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
        (ptr 0) (ptr 2) (ptr 2)
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
