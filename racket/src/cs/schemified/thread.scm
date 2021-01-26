(export (rename (|#%thread-instance| |#%thread-instance|)
                (create-alarm-evt alarm-evt)
                (the-always-evt always-evt)
                (1/break-enabled break-enabled)
                (1/break-enabled-key break-enabled-key)
                (1/break-thread break-thread)
                (call-in-main-thread call-in-main-thread)
                (1/call-in-nested-thread call-in-nested-thread)
                (1/call-with-semaphore call-with-semaphore)
                (1/call-with-semaphore/enable-break
                 call-with-semaphore/enable-break)
                (channel-get channel-get)
                (channel-put channel-put)
                (1/channel-put-evt channel-put-evt)
                (1/channel-put-evt? channel-put-evt?)
                (1/channel? channel?)
                (1/chaperone-channel chaperone-channel)
                (1/chaperone-evt chaperone-evt)
                (1/check-for-break check-for-break)
                (1/choice-evt choice-evt)
                (1/continuation-marks continuation-marks)
                (1/current-custodian current-custodian)
                (1/current-evt-pseudo-random-generator
                 current-evt-pseudo-random-generator)
                (1/current-future current-future)
                (current-future-prompt current-future-prompt)
                (1/current-plumber current-plumber)
                (1/current-process-milliseconds current-process-milliseconds)
                (1/current-thread current-thread)
                (1/current-thread-group current-thread-group)
                (1/current-thread-initial-stack-size
                 current-thread-initial-stack-size)
                (1/custodian-box-value custodian-box-value)
                (1/custodian-box? custodian-box?)
                (1/custodian-limit-memory custodian-limit-memory)
                (1/custodian-managed-list custodian-managed-list)
                (1/custodian-memory-accounting-available?
                 custodian-memory-accounting-available?)
                (1/custodian-require-memory custodian-require-memory)
                (1/custodian-shut-down? custodian-shut-down?)
                (1/custodian-shutdown-all custodian-shutdown-all)
                (custodian-shutdown-root-at-exit
                 custodian-shutdown-root-at-exit)
                (1/custodian? custodian?)
                (1/dynamic-place dynamic-place)
                (1/evt? evt?)
                (1/exit exit)
                (1/exit-handler exit-handler)
                (1/fsemaphore-count fsemaphore-count)
                (1/fsemaphore-post fsemaphore-post)
                (1/fsemaphore-try-wait? fsemaphore-try-wait?)
                (1/fsemaphore-wait fsemaphore-wait)
                (1/fsemaphore? fsemaphore?)
                (1/future future)
                (future-block future-block)
                (future-sync future-sync)
                (1/future? future?)
                (1/futures-enabled? futures-enabled?)
                (guard-evt guard-evt)
                (1/handle-evt handle-evt)
                (1/handle-evt? handle-evt?)
                (1/impersonate-channel impersonate-channel)
                (install-future-logging-procs! install-future-logging-procs!)
                (install-place-logging-procs! install-place-logging-procs!)
                (1/kill-thread kill-thread)
                (1/make-channel make-channel)
                (1/make-custodian make-custodian)
                (1/make-custodian-box make-custodian-box)
                (1/make-fsemaphore make-fsemaphore)
                (1/make-late-will-executor make-late-will-executor)
                (1/make-plumber make-plumber)
                (1/make-semaphore make-semaphore)
                (1/make-thread-group make-thread-group)
                (1/make-will-executor make-will-executor)
                (1/mark-future-trace-end! mark-future-trace-end!)
                (1/nack-guard-evt nack-guard-evt)
                (the-never-evt never-evt)
                (1/place-break place-break)
                (1/place-channel place-channel)
                (1/place-channel-get place-channel-get)
                (1/place-channel-put place-channel-put)
                (1/place-channel? place-channel?)
                (1/place-dead-evt place-dead-evt)
                (1/place-kill place-kill)
                (1/place-message-allowed? place-message-allowed?)
                (1/place-pumper-threads place-pumper-threads)
                (1/place-wait place-wait)
                (1/place? place?)
                (1/plumber-add-flush! plumber-add-flush!)
                (1/plumber-flush-all plumber-flush-all)
                (1/plumber-flush-handle-remove! plumber-flush-handle-remove!)
                (1/plumber-flush-handle? plumber-flush-handle?)
                (1/plumber? plumber?)
                (1/poll-guard-evt poll-guard-evt)
                (1/prop:evt prop:evt)
                (prop:place-message prop:place-message)
                (1/replace-evt replace-evt)
                (1/reset-future-logs-for-tracing!
                 reset-future-logs-for-tracing!)
                (1/semaphore-peek-evt semaphore-peek-evt)
                (1/semaphore-peek-evt? semaphore-peek-evt?)
                (1/semaphore-post semaphore-post)
                (semaphore-post-all semaphore-post-all)
                (1/semaphore-try-wait? semaphore-try-wait?)
                (1/semaphore-wait semaphore-wait)
                (1/semaphore-wait/enable-break semaphore-wait/enable-break)
                (1/semaphore? semaphore?)
                (set-make-place-ports+fds! set-make-place-ports+fds!)
                (set-processor-count! set-processor-count!)
                (set-schedule-quantum! set-schedule-quantum!)
                (1/sleep sleep)
                (1/sync sync)
                (1/sync/enable-break sync/enable-break)
                (1/sync/timeout sync/timeout)
                (1/sync/timeout/enable-break sync/timeout/enable-break)
                (get-system-idle-evt system-idle-evt)
                (make-thread thread)
                (get-thread-dead-evt thread-dead-evt)
                (thread-dead-evt? thread-dead-evt?)
                (1/thread-dead? thread-dead?)
                (1/thread-group? thread-group?)
                (1/thread-receive thread-receive)
                (1/thread-receive-evt thread-receive-evt)
                (1/thread-resume thread-resume)
                (1/thread-resume-evt thread-resume-evt)
                (1/thread-rewind-receive thread-rewind-receive)
                (1/thread-running? thread-running?)
                (1/thread-send thread-send)
                (1/thread-suspend thread-suspend)
                (1/thread-suspend-evt thread-suspend-evt)
                (1/thread-try-receive thread-try-receive)
                (1/thread-wait thread-wait)
                (1/thread/suspend-to-kill thread/suspend-to-kill)
                (1/thread? thread?)
                (1/touch touch)
                (1/unsafe-add-post-custodian-shutdown
                 unsafe-add-post-custodian-shutdown)
                (1/unsafe-call-in-os-thread unsafe-call-in-os-thread)
                (1/unsafe-custodian-register unsafe-custodian-register)
                (1/unsafe-custodian-unregister unsafe-custodian-unregister)
                (1/unsafe-end-atomic unsafe-end-atomic)
                (1/unsafe-end-breakable-atomic unsafe-end-breakable-atomic)
                (1/unsafe-in-atomic? unsafe-in-atomic?)
                (1/unsafe-make-custodian-at-root unsafe-make-custodian-at-root)
                (1/unsafe-make-os-semaphore unsafe-make-os-semaphore)
                (1/unsafe-os-semaphore-post unsafe-os-semaphore-post)
                (1/unsafe-os-semaphore-wait unsafe-os-semaphore-wait)
                (1/unsafe-os-thread-enabled? unsafe-os-thread-enabled?)
                (unsafe-semaphore-post unsafe-semaphore-post)
                (unsafe-semaphore-wait unsafe-semaphore-wait)
                (1/unsafe-set-on-atomic-timeout! unsafe-set-on-atomic-timeout!)
                (1/unsafe-start-atomic unsafe-start-atomic)
                (1/unsafe-start-breakable-atomic unsafe-start-breakable-atomic)
                (1/unsafe-thread-at-root unsafe-thread-at-root)
                (1/vector-set-performance-stats! vector-set-performance-stats!)
                (1/will-execute will-execute)
                (1/will-executor? will-executor?)
                (1/will-register will-register)
                (1/will-try-execute will-try-execute)
                (1/would-be-future would-be-future)
                (1/wrap-evt wrap-evt)))
(define hash2610 (hasheq))
(define hash2589 (hasheqv))
(define hash2725 (hash))
(define bad-list$1
  (|#%name|
   bad-list
   (lambda (who_0 orig-l_0)
     (begin (raise-mismatch-error who_0 "not a proper list: " orig-l_0)))))
(define memq
  (|#%name|
   memq
   (lambda (v_0 orig-l_0)
     (begin
       (letrec*
        ((loop_0
          (|#%name|
           loop
           (lambda (ls_0)
             (begin
               (if (null? ls_0)
                 #f
                 (if (not (pair? ls_0))
                   (begin-unsafe
                    (raise-mismatch-error
                     'memq
                     "not a proper list: "
                     orig-l_0))
                   (if (eq? v_0 (car ls_0)) ls_0 (loop_0 (cdr ls_0))))))))))
        (loop_0 orig-l_0))))))
(define memv
  (|#%name|
   memv
   (lambda (v_0 orig-l_0)
     (begin
       (letrec*
        ((loop_0
          (|#%name|
           loop
           (lambda (ls_0)
             (begin
               (if (null? ls_0)
                 #f
                 (if (not (pair? ls_0))
                   (begin-unsafe
                    (raise-mismatch-error
                     'memv
                     "not a proper list: "
                     orig-l_0))
                   (if (eqv? v_0 (car ls_0)) ls_0 (loop_0 (cdr ls_0))))))))))
        (loop_0 orig-l_0))))))
(define member
  (let ((default_0
         (|#%name|
          member
          (lambda (v_0 orig-l_0)
            (begin
              (letrec*
               ((loop_0
                 (|#%name|
                  loop
                  (lambda (ls_0)
                    (begin
                      (if (null? ls_0)
                        #f
                        (if (not (pair? ls_0))
                          (begin-unsafe
                           (raise-mismatch-error
                            'member
                            "not a proper list: "
                            orig-l_0))
                          (if (equal? v_0 (car ls_0))
                            ls_0
                            (loop_0 (cdr ls_0))))))))))
               (loop_0 orig-l_0)))))))
    (|#%name|
     member
     (case-lambda
      ((v_0 orig-l_0) (begin (default_0 v_0 orig-l_0)))
      ((v_0 orig-l_0 eq?_0)
       (begin
         (if (if (procedure? eq?_0) (procedure-arity-includes? eq?_0 2) #f)
           (void)
           (raise-argument-error
            'member
            "(procedure-arity-includes/c 2)"
            eq?_0))
         ((|#%name|
           member
           (lambda (v_1 orig-l_1)
             (begin
               (letrec*
                ((loop_0
                  (|#%name|
                   loop
                   (lambda (ls_0)
                     (begin
                       (if (null? ls_0)
                         #f
                         (if (not (pair? ls_0))
                           (begin-unsafe
                            (raise-mismatch-error
                             'member
                             "not a proper list: "
                             orig-l_1))
                           (if (|#%app| eq?_0 v_1 (car ls_0))
                             ls_0
                             (loop_0 (cdr ls_0))))))))))
                (loop_0 orig-l_1)))))
          v_0
          orig-l_0)))))))
(define select-handler/no-breaks
  (lambda (e_0 bpz_0 l_0)
    (with-continuation-mark*
     authentic
     break-enabled-key
     (make-thread-cell #f)
     (letrec*
      ((loop_0
        (|#%name|
         loop
         (lambda (l_1)
           (begin
             (if (null? l_1)
               (raise e_0)
               (if (|#%app| (caar l_1) e_0)
                 (begin0
                   (|#%app| (cdar l_1) e_0)
                   (with-continuation-mark*
                    push-authentic
                    break-enabled-key
                    bpz_0
                    (check-for-break)))
                 (loop_0 (cdr l_1)))))))))
      (loop_0 l_0)))))
(define false-thread-cell (make-thread-cell #f))
(define handler-prompt-key (make-continuation-prompt-tag 'handler-prompt-tag))
(define call-handled-body
  (lambda (bpz_0 handle-proc_0 body-thunk_0)
    (with-continuation-mark*
     authentic
     break-enabled-key
     false-thread-cell
     (call-with-continuation-prompt
      (lambda (bpz_1 body-thunk_1)
        (with-continuation-mark*
         authentic
         break-enabled-key
         bpz_1
         (with-continuation-mark*
          authentic
          exception-handler-key
          (lambda (e_0) (abort-current-continuation handler-prompt-key e_0))
          (|#%app| body-thunk_1))))
      handler-prompt-key
      handle-proc_0
      bpz_0
      body-thunk_0))))
(define-values
 (prop:keyword-impersonator keyword-impersonator? keyword-impersonator-ref)
 (make-struct-type-property 'keyword-impersonator))
(define keyword-procedure-impersonator-of
  (lambda (v_0)
    (if (keyword-impersonator? v_0)
      (|#%app| (keyword-impersonator-ref v_0) v_0)
      #f)))
(define-values
 (struct:keyword-procedure
  mk-kw-proc
  keyword-procedure?
  keyword-procedure-ref
  keyword-procedure-set!)
 (let ((app_0
        (list
         (cons prop:checked-procedure #t)
         (cons prop:impersonator-of keyword-procedure-impersonator-of))))
   (make-struct-type
    'keyword-procedure
    #f
    4
    0
    #f
    app_0
    (current-inspector)
    #f
    '(0 1 2 3))))
(define keyword-procedure-required
  (make-struct-field-accessor keyword-procedure-ref 2))
(define keyword-procedure-allowed
  (make-struct-field-accessor keyword-procedure-ref 3))
(define-values
 (prop:procedure-accessor procedure-accessor? procedure-accessor-ref)
 (make-struct-type-property
  'procedure
  (lambda (v_0 info-l_0)
    (if (exact-integer? v_0)
      (make-struct-field-accessor (list-ref info-l_0 3) v_0)
      #f))))
(define-values
 (new-prop:procedure new-procedure? new-procedure-ref)
 (make-struct-type-property
  'procedure
  #f
  (list (cons prop:procedure values) (cons prop:procedure-accessor values))
  #t))
(define procedure-keywords
  (lambda (p_0)
    (if (keyword-procedure? p_0)
      (let ((app_0 (keyword-procedure-required p_0)))
        (values app_0 (keyword-procedure-allowed p_0)))
      (if (procedure? p_0)
        (if (new-procedure? p_0)
          (let ((v_0 (new-procedure-ref p_0)))
            (if (procedure? v_0)
              (procedure-keywords v_0)
              (let ((a_0 (procedure-accessor-ref p_0)))
                (if a_0
                  (procedure-keywords (|#%app| a_0 p_0))
                  (values null null)))))
          (values null null))
        (raise-argument-error 'procedure-keywords "procedure?" p_0)))))
(define reverse$1
  (|#%name|
   reverse
   (lambda (l_0)
     (begin
       (begin
         (letrec*
          ((loop_0
            (|#%name|
             loop
             (lambda (a_0 l_1)
               (begin
                 (if (null? l_1)
                   a_0
                   (let ((app_0 (cons (car l_1) a_0)))
                     (loop_0 app_0 (cdr l_1)))))))))
          (loop_0 null l_0)))))))
(define-values
 (prop:stream stream-via-prop? stream-ref)
 (make-struct-type-property
  'stream
  (lambda (v_0 si_0)
    (begin
      (if (if (vector? v_0)
            (if (= 3 (vector-length v_0))
              (if (procedure? (vector-ref v_0 0))
                (if (procedure-arity-includes? (vector-ref v_0 0) 1)
                  (if (procedure? (vector-ref v_0 1))
                    (if (procedure-arity-includes? (vector-ref v_0 1) 1)
                      (if (procedure? (vector-ref v_0 2))
                        (procedure-arity-includes? (vector-ref v_0 2) 1)
                        #f)
                      #f)
                    #f)
                  #f)
                #f)
              #f)
            #f)
        (void)
        (raise-argument-error
         'guard-for-prop:stream
         (string-append
          "(vector/c (procedure-arity-includes/c 1)\n"
          "          (procedure-arity-includes/c 1)\n"
          "          (procedure-arity-includes/c 1))")
         v_0))
      (vector->immutable-vector v_0)))
  '()
  #t))
(define-values
 (prop:gen-sequence sequence-via-prop? sequence-ref)
 (make-struct-type-property
  'sequence
  (lambda (v_0 si_0)
    (begin
      (if (if (procedure? v_0) (procedure-arity-includes? v_0 1) #f)
        (void)
        (raise-argument-error
         'guard-for-prop:sequence
         "(procedure-arity-includes/c 1)"
         v_0))
      v_0))))
(define-values
 (struct:range make-range range? range-ref range-set!)
 (make-struct-type
  'stream
  #f
  3
  0
  #f
  (list
   (cons
    prop:stream
    (vector
     (lambda (v_0)
       (let ((cont?_0 (|#%app| range-ref v_0 2)))
         (if cont?_0 (not (|#%app| cont?_0 (|#%app| range-ref v_0 0))) #f)))
     (lambda (v_0) (|#%app| range-ref v_0 0))
     (lambda (v_0)
       (let ((app_0
              (let ((app_0 (|#%app| range-ref v_0 1)))
                (|#%app| app_0 (|#%app| range-ref v_0 0)))))
         (let ((app_1 (|#%app| range-ref v_0 1)))
           (make-range app_0 app_1 (|#%app| range-ref v_0 2)))))))
   (cons
    prop:gen-sequence
    (lambda (v_0)
      (let ((app_0 (|#%app| range-ref v_0 1)))
        (let ((app_1 (|#%app| range-ref v_0 0)))
          (values values #f app_0 app_1 (|#%app| range-ref v_0 2) #f #f))))))))
(define check-range
  (lambda (a_0 b_0 step_0)
    (begin
      (if (real? a_0) (void) (raise-argument-error 'in-range "real?" a_0))
      (if (real? b_0) (void) (raise-argument-error 'in-range "real?" b_0))
      (if (real? step_0)
        (void)
        (raise-argument-error 'in-range "real?" step_0)))))
(define-values
 (struct:list-stream
  make-list-stream
  list-stream?
  list-stream-ref
  list-stream-set!)
 (make-struct-type
  'stream
  #f
  1
  0
  #f
  (list
   (cons
    prop:stream
    (vector
     (lambda (v_0) (not (pair? (|#%app| list-stream-ref v_0 0))))
     (lambda (v_0) (car (|#%app| list-stream-ref v_0 0)))
     (lambda (v_0) (make-list-stream (cdr (|#%app| list-stream-ref v_0 0))))))
   (cons
    prop:gen-sequence
    (lambda (v_0)
      (values car cdr values (|#%app| list-stream-ref v_0 0) pair? #f #f))))))
(define check-list
  (lambda (l_0)
    (if (list? l_0) (void) (raise-argument-error 'in-list "list?" l_0))))
(define check-in-hash
  (lambda (ht_0)
    (if (hash? ht_0) (void) (raise-argument-error 'in-hash "hash?" ht_0))))
(define check-in-hash-keys
  (lambda (ht_0)
    (if (hash? ht_0)
      (void)
      (raise-argument-error 'in-hash-keys "hash?" ht_0))))
(define check-ranges
  (lambda (who_0 type-name_0 vec_0 start_0 stop_0 step_0 len_0)
    (begin
      (if (if (exact-nonnegative-integer? start_0)
            (let ((or-part_0 (< start_0 len_0)))
              (if or-part_0 or-part_0 (= len_0 start_0 stop_0)))
            #f)
        (void)
        (raise-range-error
         who_0
         type-name_0
         "starting "
         start_0
         vec_0
         0
         (sub1 len_0)))
      (if (if (exact-integer? stop_0)
            (if (<= -1 stop_0) (<= stop_0 len_0) #f)
            #f)
        (void)
        (raise-range-error
         who_0
         type-name_0
         "stopping "
         stop_0
         vec_0
         -1
         len_0))
      (if (if (exact-integer? step_0) (not (zero? step_0)) #f)
        (void)
        (raise-argument-error
         who_0
         "(and/c exact-integer? (not/c zero?))"
         step_0))
      (if (if (< start_0 stop_0) (< step_0 0) #f)
        (raise-arguments-error
         who_0
         "starting index less than stopping index, but given a negative step"
         "starting index"
         start_0
         "stopping index"
         stop_0
         "step"
         step_0)
        (void))
      (if (if (< stop_0 start_0) (> step_0 0) #f)
        (raise-arguments-error
         who_0
         "starting index more than stopping index, but given a positive step"
         "starting index"
         start_0
         "stopping index"
         stop_0
         "step"
         step_0)
        (void)))))
(define normalise-inputs
  (lambda (who_0
           type-name_0
           vector?_0
           unsafe-vector-length_0
           vec_0
           start_0
           stop_0
           step_0)
    (begin
      (if (|#%app| vector?_0 vec_0)
        (void)
        (raise-argument-error who_0 type-name_0 vec_0))
      (let ((len_0 (|#%app| unsafe-vector-length_0 vec_0)))
        (let ((stop*_0 (if stop_0 stop_0 len_0)))
          (begin
            (check-ranges who_0 type-name_0 vec_0 start_0 stop*_0 step_0 len_0)
            (values vec_0 start_0 stop*_0 step_0)))))))
(define unsafe-normalise-inputs
  (lambda (unsafe-vector-length_0 vec_0 start_0 stop_0 step_0)
    (values
     vec_0
     start_0
     (if stop_0 stop_0 (|#%app| unsafe-vector-length_0 vec_0))
     step_0)))
(define check-vector
  (lambda (v_0)
    (if (vector? v_0) (void) (raise-argument-error 'in-vector "vector" v_0))))
(define-values
 (struct:do-stream make-do-stream do-stream? do-stream-ref do-stream-set!)
 (make-struct-type
  'stream
  #f
  3
  0
  #f
  (list
   (cons
    prop:stream
    (vector
     (lambda (v_0) (|#%app| (|#%app| do-stream-ref v_0 0)))
     (lambda (v_0) (|#%app| (|#%app| do-stream-ref v_0 1)))
     (lambda (v_0) (|#%app| (|#%app| do-stream-ref v_0 2))))))))
(define empty-stream (make-do-stream (lambda () #t) void void))
(define map_1346
  (|#%name|
   map
   (case-lambda
    ((f_0 l_0)
     (begin
       (letrec*
        ((loop_0
          (|#%name|
           loop
           (lambda (l_1)
             (begin
               (if (null? l_1)
                 null
                 (let ((r_0 (cdr l_1)))
                   (let ((app_0 (|#%app| f_0 (car l_1))))
                     (cons app_0 (loop_0 r_0))))))))))
        (loop_0 l_0))))
    ((f_0 l1_0 l2_0)
     (letrec*
      ((loop_0
        (|#%name|
         loop
         (lambda (l1_1 l2_1)
           (begin
             (if (null? l1_1)
               null
               (let ((r1_0 (cdr l1_1)))
                 (let ((r2_0 (cdr l2_1)))
                   (let ((r1_1 r1_0))
                     (let ((app_0
                            (let ((app_0 (car l1_1)))
                              (|#%app| f_0 app_0 (car l2_1)))))
                       (cons app_0 (loop_0 r1_1 r2_0))))))))))))
      (loop_0 l1_0 l2_0)))
    ((f_0 l_0 . args_0) (gen-map f_0 (cons l_0 args_0))))))
(define for-each_2380
  (|#%name|
   for-each
   (case-lambda
    ((f_0 l_0)
     (begin
       (letrec*
        ((loop_0
          (|#%name|
           loop
           (lambda (l_1)
             (begin
               (if (null? l_1)
                 (void)
                 (let ((r_0 (cdr l_1)))
                   (begin (|#%app| f_0 (car l_1)) (loop_0 r_0)))))))))
        (loop_0 l_0))))
    ((f_0 l1_0 l2_0)
     (letrec*
      ((loop_0
        (|#%name|
         loop
         (lambda (l1_1 l2_1)
           (begin
             (if (null? l1_1)
               (void)
               (let ((r1_0 (cdr l1_1)))
                 (let ((r2_0 (cdr l2_1)))
                   (let ((r1_1 r1_0))
                     (begin
                       (let ((app_0 (car l1_1)))
                         (|#%app| f_0 app_0 (car l2_1)))
                       (loop_0 r1_1 r2_0)))))))))))
      (loop_0 l1_0 l2_0)))
    ((f_0 l_0 . args_0) (gen-for-each f_0 (cons l_0 args_0))))))
(define check-args
  (lambda (who_0 f_0 ls_0)
    (begin
      (if (procedure? f_0)
        (void)
        (raise-argument-error who_0 "procedure?" f_0))
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda (prev-len_0 ls_1 i_0)
            (begin
              (if (null? ls_1)
                (void)
                (let ((l_0 (car ls_1)))
                  (begin
                    (if (list? l_0)
                      (void)
                      (raise-argument-error who_0 "list?" l_0))
                    (let ((len_0 (length l_0)))
                      (begin
                        (if (if prev-len_0 (not (= len_0 prev-len_0)) #f)
                          (raise-arguments-error
                           who_0
                           "all lists must have same size"
                           "first list length"
                           prev-len_0
                           "other list length"
                           len_0
                           "procedure"
                           f_0)
                          (void))
                        (let ((app_0 (cdr ls_1)))
                          (loop_0 len_0 app_0 (add1 i_0)))))))))))))
       (loop_0 #f ls_0 1))
      (if (procedure-arity-includes? f_0 (length ls_0))
        (void)
        (call-with-values
         (lambda () (procedure-keywords f_0))
         (case-lambda
          ((required-keywords_0 optional-keywords_0)
           (let ((app_0
                  (if (pair? required-keywords_0)
                    (string-append
                     "argument mismatch;\n"
                     " the given procedure expects keyword arguments")
                    (string-append
                     "argument mismatch;\n"
                     " the given procedure's expected number of arguments does not match"
                     " the given number of lists"))))
             (let ((app_1
                    (unquoted-printing-string
                     (let ((or-part_0
                            (let ((n_0 (object-name f_0)))
                              (if (symbol? n_0) (symbol->string n_0) #f))))
                       (if or-part_0 or-part_0 "#<procedure>")))))
               (apply
                raise-arguments-error
                who_0
                app_0
                "given procedure"
                app_1
                (let ((app_2
                       (let ((a_0 (procedure-arity f_0)))
                         (if (pair? required-keywords_0)
                           null
                           (if (integer? a_0)
                             (list "expected" a_0)
                             (if (arity-at-least? a_0)
                               (list
                                "expected"
                                (unquoted-printing-string
                                 (string-append
                                  "at least "
                                  (number->string
                                   (arity-at-least-value a_0)))))
                               null))))))
                  (let ((app_3
                         (if (pair? required-keywords_0)
                           null
                           (list "given" (length ls_0)))))
                    (let ((app_4
                           (if (pair? required-keywords_0)
                             (list
                              "required keywords"
                              (unquoted-printing-string
                               (apply
                                string-append
                                (cdr
                                 (letrec*
                                  ((loop_0
                                    (|#%name|
                                     loop
                                     (lambda (kws_0)
                                       (begin
                                         (if (null? kws_0)
                                           null
                                           (let ((app_4
                                                  (string-append
                                                   "#:"
                                                   (keyword->string
                                                    (car kws_0)))))
                                             (list*
                                              " "
                                              app_4
                                              (loop_0 (cdr kws_0))))))))))
                                  (loop_0 required-keywords_0))))))
                             null)))
                      (append
                       app_2
                       app_3
                       app_4
                       (let ((w_0
                              (let ((app_5 (error-print-width)))
                                (quotient app_5 (length ls_0)))))
                         (if (> w_0 10)
                           (list
                            "argument lists..."
                            (unquoted-printing-string
                             (apply
                              string-append
                              (letrec*
                               ((loop_0
                                 (|#%name|
                                  loop
                                  (lambda (ls_1)
                                    (begin
                                      (if (null? ls_1)
                                        null
                                        (let ((app_5
                                               (string-append
                                                "\n   "
                                                (let ((app_5
                                                       (error-value->string-handler)))
                                                  (|#%app|
                                                   app_5
                                                   (car ls_1)
                                                   w_0)))))
                                          (cons
                                           app_5
                                           (loop_0 (cdr ls_1))))))))))
                               (loop_0 ls_0)))))
                           null))))))))))
          (args (raise-binding-result-arity-error 2 args))))))))
(define gen-map
  (lambda (f_0 ls_0)
    (begin
      #t
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda (ls_1)
            (begin
              (if (null? (car ls_1))
                null
                (let ((next-ls_0 (map_1346 cdr ls_1)))
                  (let ((app_0 (apply f_0 (map_1346 car ls_1))))
                    (cons app_0 (loop_0 next-ls_0))))))))))
       (loop_0 ls_0)))))
(define gen-for-each
  (lambda (f_0 ls_0)
    (begin
      #t
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda (ls_1)
            (begin
              (if (null? (car ls_1))
                (void)
                (let ((next-ls_0 (map_1346 cdr ls_1)))
                  (begin
                    (apply f_0 (map_1346 car ls_1))
                    (loop_0 next-ls_0)))))))))
       (loop_0 ls_0)))))
(define -random
  (|#%name|
   random
   (case-lambda
    (() (begin (random)))
    ((x_0) (random x_0))
    ((x_0 y_0)
     (if (exact-integer? y_0)
       (begin
         (if (exact-integer? x_0)
           (void)
           (raise-argument-error 'random "exact-integer?" 0 x_0 y_0))
         (if (< x_0 y_0)
           (void)
           (raise-argument-error
            'random
            (string-append "(>/c " (number->string x_0) ")")
            1
            x_0
            y_0))
         (let ((d_0 (- y_0 x_0)))
           (begin
             (if (<= d_0 4294967087)
               (void)
               (raise-arguments-error
                'random
                "difference between arguments is greater than 4294967087"
                "min"
                x_0
                "max"
                y_0))
             (+ x_0 (random d_0)))))
       (if (pseudo-random-generator? y_0)
         (begin
           (if (if (exact-integer? x_0) (<= 1 x_0 4294967087) #f)
             (void)
             (raise-argument-error
              'random
              "(integer-in 1 4294967087)"
              0
              x_0
              y_0))
           (random x_0 y_0))
         (begin
           (if (exact-integer? x_0)
             (void)
             (raise-argument-error 'random "exact-integer?" 0 x_0 y_0))
           (raise-argument-error
            'random
            "(or/c exact-integer? pseudo-random-generator?)"
            1
            x_0
            y_0)))))
    ((min_0 max_0 prng_0)
     (begin
       (if (exact-integer? min_0)
         (void)
         (raise-argument-error 'random "exact-integer?" 0 min_0 max_0 prng_0))
       (if (exact-integer? max_0)
         (void)
         (raise-argument-error 'random "exact-integer?" 1 min_0 max_0 prng_0))
       (if (< min_0 max_0)
         (void)
         (raise-argument-error
          'random
          (string-append "(>/c " (number->string min_0) ")")
          1
          min_0
          max_0
          prng_0))
       (let ((d_0 (- max_0 min_0)))
         (begin
           (if (<= d_0 4294967087)
             (void)
             (raise-arguments-error
              'random
              "difference between first and second arguments is greater than 4294967087"
              "min"
              min_0
              "max"
              max_0
              "rand-gen"
              prng_0))
           (if (pseudo-random-generator? prng_0)
             (void)
             (raise-argument-error
              'random
              "pseudo-random-generator?"
              2
              min_0
              max_0
              prng_0))
           (+ min_0 (random d_0 prng_0)))))))))
(define hash-keys
  (lambda (h_0)
    (letrec*
     ((loop_0
       (|#%name|
        loop
        (lambda (pos_0)
          (begin
            (if pos_0
              (let ((app_0 (hash-iterate-key h_0 pos_0)))
                (cons app_0 (loop_0 (hash-iterate-next h_0 pos_0))))
              null))))))
     (loop_0 (hash-iterate-first h_0)))))
(define hash-empty?
  (lambda (table_0)
    (begin
      (if (hash? table_0)
        (void)
        (raise-argument-error 'hash-empty? "hash?" table_0))
      (zero? (hash-count table_0)))))
(define struct:queue
  (make-record-type-descriptor*
   'queue
   #f
   (|#%nongenerative-uid| queue)
   #f
   #f
   2
   3))
(define effect_2212
  (struct-type-install-properties!
   struct:queue
   '(queue)
   2
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '()
   #f
   'queue))
(define queue1.1
  (|#%name|
   queue
   (record-constructor
    (make-record-constructor-descriptor struct:queue #f #f))))
(define queue? (|#%name| queue? (record-predicate struct:queue)))
(define queue-start (|#%name| queue-start (record-accessor struct:queue 0)))
(define queue-end (|#%name| queue-end (record-accessor struct:queue 1)))
(define set-queue-start!
  (|#%name| set-queue-start! (record-mutator struct:queue 0)))
(define set-queue-end!
  (|#%name| set-queue-end! (record-mutator struct:queue 1)))
(define struct:node$2
  (make-record-type-descriptor*
   'node
   #f
   (|#%nongenerative-uid| node)
   #f
   #f
   3
   6))
(define effect_2496
  (struct-type-install-properties!
   struct:node$2
   '(node)
   3
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(0)
   #f
   'node))
(define node2.1
  (|#%name|
   node
   (record-constructor
    (make-record-constructor-descriptor struct:node$2 #f #f))))
(define node?$2 (|#%name| node? (record-predicate struct:node$2)))
(define node-elem (|#%name| node-elem (record-accessor struct:node$2 0)))
(define node-prev$1 (|#%name| node-prev (record-accessor struct:node$2 1)))
(define node-next$1 (|#%name| node-next (record-accessor struct:node$2 2)))
(define set-node-prev!$1
  (|#%name| set-node-prev! (record-mutator struct:node$2 1)))
(define set-node-next!$1
  (|#%name| set-node-next! (record-mutator struct:node$2 2)))
(define make-queue (lambda () (queue1.1 #f #f)))
(define queue-empty? (lambda (q_0) (not (queue-start q_0))))
(define queue-remove!
  (lambda (q_0)
    (let ((qs_0 (queue-start q_0)))
      (if (not qs_0)
        #f
        (let ((n_0 (node-next$1 qs_0)))
          (begin
            (set-queue-start! q_0 n_0)
            (if n_0 (set-node-prev!$1 n_0 #f) (set-queue-end! q_0 #f))
            (node-elem qs_0)))))))
(define queue-fremove!
  (lambda (q_0 pred_0)
    (letrec*
     ((loop_0
       (|#%name|
        loop
        (lambda (qs_0)
          (begin
            (if qs_0
              (let ((w_0 (node-elem qs_0)))
                (if (|#%app| pred_0 w_0)
                  (begin (queue-remove-node! q_0 qs_0) w_0)
                  (loop_0 (node-next$1 qs_0))))
              #f))))))
     (loop_0 (queue-start q_0)))))
(define queue-remove-all!
  (lambda (q_0 proc_0)
    (begin
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda (qs_0)
            (begin
              (if qs_0
                (begin
                  (|#%app| proc_0 (node-elem qs_0))
                  (loop_0 (node-next$1 qs_0)))
                (void)))))))
       (loop_0 (queue-start q_0)))
      (set-queue-start! q_0 #f)
      (set-queue-end! q_0 #f))))
(define queue-add!
  (lambda (q_0 w_0)
    (let ((e_0 (queue-end q_0)))
      (let ((n_0 (node2.1 w_0 e_0 #f)))
        (begin
          (if (not e_0) (set-queue-start! q_0 n_0) (set-node-next!$1 e_0 n_0))
          (set-queue-end! q_0 n_0)
          n_0)))))
(define queue-add-front!
  (lambda (q_0 w_0)
    (let ((e_0 (queue-start q_0)))
      (let ((n_0 (node2.1 w_0 #f e_0)))
        (begin
          (if (not e_0)
            (begin (set-queue-start! q_0 n_0) (set-queue-end! q_0 n_0))
            (begin (set-node-prev!$1 e_0 n_0) (set-queue-start! q_0 n_0)))
          n_0)))))
(define queue-remove-node!
  (lambda (q_0 n_0)
    (begin
      (if (node-prev$1 n_0)
        (let ((app_0 (node-prev$1 n_0)))
          (set-node-next!$1 app_0 (node-next$1 n_0)))
        (set-queue-start! q_0 (node-next$1 n_0)))
      (if (node-next$1 n_0)
        (let ((app_0 (node-next$1 n_0)))
          (set-node-prev!$1 app_0 (node-prev$1 n_0)))
        (set-queue-end! q_0 (node-prev$1 n_0))))))
(define internal-error
  (lambda (s_0)
    (raise
     (let ((app_0 (string-append "internal error: " s_0)))
       (|#%app| exn:fail app_0 (current-continuation-marks))))))
(define effect_3485
  (begin
    (void
     (if (primitive-table '|#%engine|)
       (void)
       (internal-error "engines not provided by host")))
    (void)))
(define effect_2501
  (begin
    (void
     (if (primitive-table '|#%pthread|)
       (void)
       (internal-error "pthreads not provided by host")))
    (void)))
(define 1/make-pthread-parameter make-pthread-parameter)
(define 1/unsafe-make-place-local unsafe-make-place-local)
(define unsafe-place-local-ref$1 unsafe-place-local-ref)
(define 1/unsafe-place-local-set! unsafe-place-local-set!)
(define 1/unsafe-root-continuation-prompt-tag
  unsafe-root-continuation-prompt-tag)
(define 1/break-enabled-key break-enabled-key)
(define 1/engine-block engine-block)
(define make-engine (hash-ref (primitive-table '|#%engine|) 'make-engine #f))
(define engine-timeout
  (hash-ref (primitive-table '|#%engine|) 'engine-timeout #f))
(define engine-return
  (hash-ref (primitive-table '|#%engine|) 'engine-return #f))
(define engine-roots (hash-ref (primitive-table '|#%engine|) 'engine-roots #f))
(define call-with-engine-completion
  (hash-ref (primitive-table '|#%engine|) 'call-with-engine-completion #f))
(define current-process-milliseconds$1
  (hash-ref (primitive-table '|#%engine|) 'current-process-milliseconds #f))
(define set-ctl-c-handler!
  (hash-ref (primitive-table '|#%engine|) 'set-ctl-c-handler! #f))
(define set-break-enabled-transition-hook!
  (hash-ref
   (primitive-table '|#%engine|)
   'set-break-enabled-transition-hook!
   #f))
(define host:continuation-marks
  (hash-ref (primitive-table '|#%engine|) 'continuation-marks #f))
(define host:poll-will-executors
  (hash-ref (primitive-table '|#%engine|) 'poll-will-executors #f))
(define host:make-will-executor
  (hash-ref (primitive-table '|#%engine|) 'make-will-executor #f))
(define host:make-late-will-executor
  (hash-ref (primitive-table '|#%engine|) 'make-late-will-executor #f))
(define host:will-executor?
  (hash-ref (primitive-table '|#%engine|) 'will-executor? #f))
(define host:will-register
  (hash-ref (primitive-table '|#%engine|) 'will-register #f))
(define host:will-try-execute
  (hash-ref (primitive-table '|#%engine|) 'will-try-execute #f))
(define set-reachable-size-increments-callback!
  (hash-ref
   (primitive-table '|#%engine|)
   'set-reachable-size-increments-callback!
   #f))
(define set-custodian-memory-use-proc!
  (hash-ref (primitive-table '|#%engine|) 'set-custodian-memory-use-proc! #f))
(define set-immediate-allocation-check-proc!
  (hash-ref
   (primitive-table '|#%engine|)
   'set-immediate-allocation-check-proc!
   #f))
(define exn:break/non-engine
  (hash-ref (primitive-table '|#%engine|) 'exn:break/non-engine #f))
(define exn:break:hang-up/non-engine
  (hash-ref (primitive-table '|#%engine|) 'exn:break:hang-up/non-engine #f))
(define exn:break:terminate/non-engine
  (hash-ref (primitive-table '|#%engine|) 'exn:break:terminate/non-engine #f))
(define host:poll-async-callbacks
  (hash-ref (primitive-table '|#%engine|) 'poll-async-callbacks #f))
(define host:disable-interrupts
  (hash-ref (primitive-table '|#%engine|) 'disable-interrupts #f))
(define host:enable-interrupts
  (hash-ref (primitive-table '|#%engine|) 'enable-interrupts #f))
(define host:sleep (hash-ref (primitive-table '|#%engine|) 'sleep #f))
(define host:get-wakeup-handle
  (hash-ref (primitive-table '|#%engine|) 'get-wakeup-handle #f))
(define host:wakeup (hash-ref (primitive-table '|#%engine|) 'wakeup #f))
(define host:fork-place
  (hash-ref (primitive-table '|#%engine|) 'fork-place #f))
(define host:start-place
  (hash-ref (primitive-table '|#%engine|) 'start-place #f))
(define host:exit (hash-ref (primitive-table '|#%engine|) 'exit #f))
(define host:current-place-roots
  (hash-ref (primitive-table '|#%engine|) 'current-place-roots #f))
(define host:get-initial-place
  (hash-ref (primitive-table '|#%engine|) 'get-initial-place #f))
(define host:call-with-current-continuation-roots
  (hash-ref
   (primitive-table '|#%engine|)
   'call-with-current-continuation-roots
   #f))
(define fork-pthread (hash-ref (primitive-table '|#%engine|) 'fork-pthread #f))
(define pthread? (hash-ref (primitive-table '|#%engine|) 'pthread? #f))
(define get-pthread-id
  (hash-ref (primitive-table '|#%engine|) 'get-thread-id #f))
(define host:make-condition
  (hash-ref (primitive-table '|#%engine|) 'make-condition #f))
(define host:condition-wait
  (hash-ref (primitive-table '|#%engine|) 'condition-wait #f))
(define host:condition-signal
  (hash-ref (primitive-table '|#%engine|) 'condition-signal #f))
(define host:condition-broadcast
  (hash-ref (primitive-table '|#%engine|) 'condition-broadcast #f))
(define host:make-mutex
  (hash-ref (primitive-table '|#%engine|) 'make-mutex #f))
(define host:mutex-acquire
  (hash-ref (primitive-table '|#%engine|) 'mutex-acquire #f))
(define host:mutex-release
  (hash-ref (primitive-table '|#%engine|) 'mutex-release #f))
(define threaded? (hash-ref (primitive-table '|#%engine|) 'threaded? #f))
(define host:call-as-asynchronous-callback
  (hash-ref (primitive-table '|#%engine|) 'call-as-asynchronous-callback #f))
(define host:post-as-asynchronous-callback
  (hash-ref (primitive-table '|#%engine|) 'post-as-asynchronous-callback #f))
(define continuation-current-primitive
  (hash-ref (primitive-table '|#%engine|) 'continuation-current-primitive #f))
(define host:prop:unsafe-authentic-override
  (hash-ref (primitive-table '|#%engine|) 'prop:unsafe-authentic-override #f))
(define struct:node$1
  (make-record-type-descriptor*
   'node
   #f
   (|#%nongenerative-uid| node)
   #f
   #f
   5
   0))
(define effect_1764
  (struct-type-install-properties!
   struct:node$1
   '(node)
   5
   0
   #f
   null
   #f
   #f
   '(0 1 2 3 4)
   #f
   'node))
(define node1.1$1
  (|#%name|
   node
   (record-constructor
    (make-record-constructor-descriptor struct:node$1 #f #f))))
(define node?$1_2258 (|#%name| node? (record-predicate struct:node$1)))
(define node?$1
  (|#%name|
   node?
   (lambda (v)
     (if (node?$1_2258 v)
       #t
       ($value
        (if (impersonator? v) (node?$1_2258 (impersonator-val v)) #f))))))
(define node-key_2296 (|#%name| node-key (record-accessor struct:node$1 0)))
(define node-key
  (|#%name|
   node-key
   (lambda (s)
     (if (node?$1_2258 s)
       (node-key_2296 s)
       ($value
        (impersonate-ref node-key_2296 struct:node$1 0 s 'node 'key))))))
(define node-val_2531 (|#%name| node-val (record-accessor struct:node$1 1)))
(define node-val
  (|#%name|
   node-val
   (lambda (s)
     (if (node?$1_2258 s)
       (node-val_2531 s)
       ($value
        (impersonate-ref node-val_2531 struct:node$1 1 s 'node 'val))))))
(define node-height_2814
  (|#%name| node-height (record-accessor struct:node$1 2)))
(define node-height
  (|#%name|
   node-height
   (lambda (s)
     (if (node?$1_2258 s)
       (node-height_2814 s)
       ($value
        (impersonate-ref node-height_2814 struct:node$1 2 s 'node 'height))))))
(define node-left_2332 (|#%name| node-left (record-accessor struct:node$1 3)))
(define node-left
  (|#%name|
   node-left
   (lambda (s)
     (if (node?$1_2258 s)
       (node-left_2332 s)
       ($value
        (impersonate-ref node-left_2332 struct:node$1 3 s 'node 'left))))))
(define node-right_2584
  (|#%name| node-right (record-accessor struct:node$1 4)))
(define node-right
  (|#%name|
   node-right
   (lambda (s)
     (if (node?$1_2258 s)
       (node-right_2584 s)
       ($value
        (impersonate-ref node-right_2584 struct:node$1 4 s 'node 'right))))))
(define empty-tree #f)
(define is-empty? (lambda (t_0) (not t_0)))
(define tree-height (lambda (t_0) (if (not t_0) 0 (node-height t_0))))
(define tree-balance
  (lambda (t_0)
    (let ((app_0 (tree-height (node-left t_0))))
      (- app_0 (tree-height (node-right t_0))))))
(define combine
  (lambda (key_0 val_0 left_0 right_0)
    (node1.1$1
     key_0
     val_0
     (add1
      (let ((app_0 (tree-height left_0))) (max app_0 (tree-height right_0))))
     left_0
     right_0)))
(define reverse-combine
  (lambda (key_0 val_0 right_0 left_0) (combine key_0 val_0 left_0 right_0)))
(define lookup
  (lambda (t_0 key_0 <?_0)
    (if (not t_0)
      #f
      (if (|#%app| <?_0 key_0 (node-key t_0))
        (lookup (node-left t_0) key_0 <?_0)
        (if (|#%app| <?_0 (node-key t_0) key_0)
          (lookup (node-right t_0) key_0 <?_0)
          (node-val t_0))))))
(define insert
  (lambda (t_0 key_0 val_0 <?_0)
    (if (not t_0)
      (combine key_0 val_0 #f #f)
      (if (|#%app| <?_0 key_0 (node-key t_0))
        (insert-to t_0 key_0 val_0 <?_0 node-left node-right combine)
        (if (|#%app| <?_0 (node-key t_0) key_0)
          (insert-to t_0 key_0 val_0 <?_0 node-right node-left reverse-combine)
          (if (node?$1 t_0)
            (let ((app_0 (node-key t_0)))
              (let ((app_1 (node-height t_0)))
                (let ((app_2 (node-left t_0)))
                  (node1.1$1 app_0 val_0 app_1 app_2 (node-right t_0)))))
            (raise-argument-error 'struct-copy "node?" t_0)))))))
(define insert-to
  (lambda (t_0 new-key_0 new-val_0 <?_0 node-to_0 node-other_0 comb_0)
    (let ((new-to_0 (insert (|#%app| node-to_0 t_0) new-key_0 new-val_0 <?_0)))
      (let ((new-other_0 (|#%app| node-other_0 t_0)))
        (let ((new-t_0
               (let ((app_0 (node-key t_0)))
                 (|#%app| comb_0 app_0 (node-val t_0) new-to_0 new-other_0))))
          (check-rotate new-t_0 node-to_0 node-other_0 comb_0))))))
(define check-rotate
  (lambda (new-t_0 node-to_0 node-other_0 comb_0)
    (let ((new-to_0 (|#%app| node-to_0 new-t_0)))
      (let ((new-other_0 (|#%app| node-other_0 new-t_0)))
        (let ((to-height_0 (tree-height new-to_0)))
          (let ((other-height_0 (tree-height new-other_0)))
            (if (= (- to-height_0 other-height_0) 2)
              (rotate new-t_0 node-to_0 node-other_0 comb_0)
              new-t_0)))))))
(define rotate
  (lambda (t_0 node-to_0 node-other_0 comb_0)
    (let ((to_0 (|#%app| node-to_0 t_0)))
      (let ((to-balance_0
             (let ((app_0 (tree-height (|#%app| node-to_0 to_0))))
               (- app_0 (tree-height (|#%app| node-other_0 to_0))))))
        (if (negative? to-balance_0)
          (double-rotate t_0 node-to_0 node-other_0 comb_0)
          (single-rotate t_0 node-to_0 node-other_0 comb_0))))))
(define double-rotate
  (lambda (t_0 node-to_0 node-other_0 comb_0)
    (let ((orange_0 (|#%app| node-to_0 t_0)))
      (let ((yellow_0 (|#%app| node-other_0 orange_0)))
        (let ((A_0 (|#%app| node-to_0 orange_0)))
          (let ((B_0 (|#%app| node-to_0 yellow_0)))
            (let ((C_0 (|#%app| node-other_0 yellow_0)))
              (let ((D_0 (|#%app| node-other_0 t_0)))
                (single-rotate
                 (let ((app_0 (node-key t_0)))
                   (let ((app_1 (node-val t_0)))
                     (|#%app|
                      comb_0
                      app_0
                      app_1
                      (let ((app_2 (node-key yellow_0)))
                        (let ((app_3 (node-val yellow_0)))
                          (|#%app|
                           comb_0
                           app_2
                           app_3
                           (let ((app_4 (node-key orange_0)))
                             (|#%app|
                              comb_0
                              app_4
                              (node-val orange_0)
                              A_0
                              B_0))
                           C_0)))
                      D_0)))
                 node-to_0
                 node-other_0
                 comb_0)))))))))
(define single-rotate
  (lambda (t_0 node-to_0 node-other_0 comb_0)
    (let ((yellow_0 (|#%app| node-to_0 t_0)))
      (let ((app_0 (node-key yellow_0)))
        (let ((app_1 (node-val yellow_0)))
          (let ((app_2 (|#%app| node-to_0 yellow_0)))
            (|#%app|
             comb_0
             app_0
             app_1
             app_2
             (let ((app_3 (node-key t_0)))
               (let ((app_4 (node-val t_0)))
                 (let ((app_5 (|#%app| node-other_0 yellow_0)))
                   (|#%app|
                    comb_0
                    app_3
                    app_4
                    app_5
                    (|#%app| node-other_0 t_0))))))))))))
(define delete
  (lambda (t_0 key_0 <?_0)
    (if (not t_0)
      #f
      (if (|#%app| <?_0 key_0 (node-key t_0))
        (delete-at t_0 key_0 <?_0 node-left node-right combine reverse-combine)
        (if (|#%app| <?_0 (node-key t_0) key_0)
          (delete-at
           t_0
           key_0
           <?_0
           node-right
           node-left
           reverse-combine
           combine)
          (if (not (node-left t_0))
            (node-right t_0)
            (if (not (node-right t_0))
              (node-left t_0)
              (call-with-values
               (lambda () (max-key+value (node-left t_0)))
               (case-lambda
                ((move-key_0 move-val_0)
                 (let ((new-left_0 (delete (node-left t_0) move-key_0 <?_0)))
                   (let ((new-t_0
                          (combine
                           move-key_0
                           move-val_0
                           new-left_0
                           (node-right t_0))))
                     (let ((balance_0 (tree-balance new-t_0)))
                       (if (= balance_0 -2)
                         (check-rotate
                          new-t_0
                          node-right
                          node-left
                          reverse-combine)
                         (check-rotate
                          new-t_0
                          node-left
                          node-right
                          combine))))))
                (args (raise-binding-result-arity-error 2 args)))))))))))
(define delete-at
  (lambda (t_0 key_0 <?_0 node-to_0 node-from_0 combine_0 reverse-combine_0)
    (let ((new-to_0 (delete (|#%app| node-to_0 t_0) key_0 <?_0)))
      (if (eq? new-to_0 (|#%app| node-to_0 t_0))
        t_0
        (check-rotate
         (let ((app_0 (node-key t_0)))
           (let ((app_1 (node-val t_0)))
             (|#%app|
              combine_0
              app_0
              app_1
              new-to_0
              (|#%app| node-from_0 t_0))))
         node-from_0
         node-to_0
         reverse-combine_0)))))
(define min-key+value
  (lambda (t_0)
    (if (not (node-left t_0))
      (let ((app_0 (node-key t_0))) (values app_0 (node-val t_0)))
      (min-key+value (node-left t_0)))))
(define max-key+value
  (lambda (t_0)
    (if (not (node-right t_0))
      (let ((app_0 (node-key t_0))) (values app_0 (node-val t_0)))
      (max-key+value (node-right t_0)))))
(define struct:sandman
  (make-record-type-descriptor*
   'sandman
   #f
   (structure-type-lookup-prefab-uid
    'sandman
    #f
    11
    0
    #f
    '(0 1 2 3 4 5 6 7 8 9 10))
   #f
   #f
   11
   2047))
(define effect_2467
  (struct-type-install-properties!
   struct:sandman
   '(sandman)
   11
   0
   #f
   null
   'prefab
   #f
   '(0 1 2 3 4 5 6 7 8 9 10)
   #f
   'sandman))
(define sandman1.1
  (|#%name|
   sandman
   (record-constructor
    (make-record-constructor-descriptor struct:sandman #f #f))))
(define sandman?_2599 (|#%name| sandman? (record-predicate struct:sandman)))
(define sandman?
  (|#%name|
   sandman?
   (lambda (v)
     (if (sandman?_2599 v)
       #t
       ($value
        (if (impersonator? v) (sandman?_2599 (impersonator-val v)) #f))))))
(define sandman-do-sleep_2487
  (|#%name| sandman-do-sleep (record-accessor struct:sandman 0)))
(define sandman-do-sleep
  (|#%name|
   sandman-do-sleep
   (lambda (s)
     (if (sandman?_2599 s)
       (sandman-do-sleep_2487 s)
       ($value
        (impersonate-ref
         sandman-do-sleep_2487
         struct:sandman
         0
         s
         'sandman
         'do-sleep))))))
(define sandman-do-poll_2411
  (|#%name| sandman-do-poll (record-accessor struct:sandman 1)))
(define sandman-do-poll
  (|#%name|
   sandman-do-poll
   (lambda (s)
     (if (sandman?_2599 s)
       (sandman-do-poll_2411 s)
       ($value
        (impersonate-ref
         sandman-do-poll_2411
         struct:sandman
         1
         s
         'sandman
         'do-poll))))))
(define sandman-do-get-wakeup_3028
  (|#%name| sandman-do-get-wakeup (record-accessor struct:sandman 2)))
(define sandman-do-get-wakeup
  (|#%name|
   sandman-do-get-wakeup
   (lambda (s)
     (if (sandman?_2599 s)
       (sandman-do-get-wakeup_3028 s)
       ($value
        (impersonate-ref
         sandman-do-get-wakeup_3028
         struct:sandman
         2
         s
         'sandman
         'do-get-wakeup))))))
(define sandman-do-wakeup_2562
  (|#%name| sandman-do-wakeup (record-accessor struct:sandman 3)))
(define sandman-do-wakeup
  (|#%name|
   sandman-do-wakeup
   (lambda (s)
     (if (sandman?_2599 s)
       (sandman-do-wakeup_2562 s)
       ($value
        (impersonate-ref
         sandman-do-wakeup_2562
         struct:sandman
         3
         s
         'sandman
         'do-wakeup))))))
(define sandman-do-any-sleepers?_2376
  (|#%name| sandman-do-any-sleepers? (record-accessor struct:sandman 4)))
(define sandman-do-any-sleepers?
  (|#%name|
   sandman-do-any-sleepers?
   (lambda (s)
     (if (sandman?_2599 s)
       (sandman-do-any-sleepers?_2376 s)
       ($value
        (impersonate-ref
         sandman-do-any-sleepers?_2376
         struct:sandman
         4
         s
         'sandman
         'do-any-sleepers?))))))
(define sandman-do-sleepers-external-events_2747
  (|#%name|
   sandman-do-sleepers-external-events
   (record-accessor struct:sandman 5)))
(define sandman-do-sleepers-external-events
  (|#%name|
   sandman-do-sleepers-external-events
   (lambda (s)
     (if (sandman?_2599 s)
       (sandman-do-sleepers-external-events_2747 s)
       ($value
        (impersonate-ref
         sandman-do-sleepers-external-events_2747
         struct:sandman
         5
         s
         'sandman
         'do-sleepers-external-events))))))
(define sandman-do-add-thread!_3210
  (|#%name| sandman-do-add-thread! (record-accessor struct:sandman 6)))
(define sandman-do-add-thread!
  (|#%name|
   sandman-do-add-thread!
   (lambda (s)
     (if (sandman?_2599 s)
       (sandman-do-add-thread!_3210 s)
       ($value
        (impersonate-ref
         sandman-do-add-thread!_3210
         struct:sandman
         6
         s
         'sandman
         'do-add-thread!))))))
(define sandman-do-remove-thread!_2183
  (|#%name| sandman-do-remove-thread! (record-accessor struct:sandman 7)))
(define sandman-do-remove-thread!
  (|#%name|
   sandman-do-remove-thread!
   (lambda (s)
     (if (sandman?_2599 s)
       (sandman-do-remove-thread!_2183 s)
       ($value
        (impersonate-ref
         sandman-do-remove-thread!_2183
         struct:sandman
         7
         s
         'sandman
         'do-remove-thread!))))))
(define sandman-do-merge-external-event-sets_2575
  (|#%name|
   sandman-do-merge-external-event-sets
   (record-accessor struct:sandman 8)))
(define sandman-do-merge-external-event-sets
  (|#%name|
   sandman-do-merge-external-event-sets
   (lambda (s)
     (if (sandman?_2599 s)
       (sandman-do-merge-external-event-sets_2575 s)
       ($value
        (impersonate-ref
         sandman-do-merge-external-event-sets_2575
         struct:sandman
         8
         s
         'sandman
         'do-merge-external-event-sets))))))
(define sandman-do-merge-timeout_2100
  (|#%name| sandman-do-merge-timeout (record-accessor struct:sandman 9)))
(define sandman-do-merge-timeout
  (|#%name|
   sandman-do-merge-timeout
   (lambda (s)
     (if (sandman?_2599 s)
       (sandman-do-merge-timeout_2100 s)
       ($value
        (impersonate-ref
         sandman-do-merge-timeout_2100
         struct:sandman
         9
         s
         'sandman
         'do-merge-timeout))))))
(define sandman-do-extract-timeout_2311
  (|#%name| sandman-do-extract-timeout (record-accessor struct:sandman 10)))
(define sandman-do-extract-timeout
  (|#%name|
   sandman-do-extract-timeout
   (lambda (s)
     (if (sandman?_2599 s)
       (sandman-do-extract-timeout_2311 s)
       ($value
        (impersonate-ref
         sandman-do-extract-timeout_2311
         struct:sandman
         10
         s
         'sandman
         'do-extract-timeout))))))
(define the-sandman #f)
(define current-sandman
  (case-lambda
   (() the-sandman)
   ((sm_0)
    (begin
      (if (sandman? sm_0)
        (void)
        (raise-argument-error 'current-sandman "sandman?" sm_0))
      (set! the-sandman sm_0)))))
(define sandman-merge-timeout
  (lambda (exts_0 timeout_0)
    (|#%app| (sandman-do-merge-timeout the-sandman) exts_0 timeout_0)))
(define sandman-merge-exts
  (lambda (a-exts_0 b-exts_0)
    (|#%app|
     (sandman-do-merge-external-event-sets the-sandman)
     a-exts_0
     b-exts_0)))
(define sandman-add-sleeping-thread!
  (lambda (th_0 exts_0)
    (|#%app| (sandman-do-add-thread! the-sandman) th_0 exts_0)))
(define sandman-remove-sleeping-thread!
  (lambda (th_0 h_0)
    (|#%app| (sandman-do-remove-thread! the-sandman) th_0 h_0)))
(define sandman-poll
  (lambda (thread-wakeup_0)
    (|#%app| (sandman-do-poll the-sandman) thread-wakeup_0)))
(define sandman-sleep
  (lambda (exts_0) (|#%app| (sandman-do-sleep the-sandman) exts_0)))
(define sandman-get-wakeup-handle
  (lambda () (|#%app| (sandman-do-get-wakeup the-sandman))))
(define sandman-wakeup
  (lambda (h_0) (|#%app| (sandman-do-wakeup the-sandman) h_0)))
(define sandman-any-sleepers?
  (lambda () (|#%app| (sandman-do-any-sleepers? the-sandman))))
(define sandman-sleepers-external-events
  (lambda () (|#%app| (sandman-do-sleepers-external-events the-sandman))))
(define cell.1$8 (unsafe-make-place-local '()))
(define cell.2$5 (unsafe-make-place-local '()))
(define cell.3$1 (unsafe-make-place-local #f))
(define min*
  (lambda (a-sleep-until_0 b-sleep-until_0)
    (if (if a-sleep-until_0 b-sleep-until_0 #f)
      (min a-sleep-until_0 b-sleep-until_0)
      (if a-sleep-until_0 a-sleep-until_0 b-sleep-until_0))))
(define the-default-sandman
  (sandman1.1
   (lambda (timeout-at_0)
     (|#%app|
      host:sleep
      (max
       0.0
       (/
        (let ((app_0 (if timeout-at_0 timeout-at_0 (|#%app| distant-future))))
          (- app_0 (current-inexact-milliseconds)))
        1000.0))))
   (lambda (wakeup_0)
     (if (let ((t_0 (unsafe-place-local-ref cell.3$1)))
           (begin-unsafe (not t_0)))
       (void)
       (call-with-values
        (lambda () (min-key+value (unsafe-place-local-ref cell.3$1)))
        (case-lambda
         ((timeout-at_0 threads_0)
          (if (<= timeout-at_0 (current-inexact-milliseconds))
            (if (null? threads_0)
              (void)
              (begin
                (begin
                  (letrec*
                   ((for-loop_0
                     (|#%name|
                      for-loop
                      (lambda (i_0)
                        (begin
                          (if i_0
                            (let ((t_0 (hash-iterate-key threads_0 i_0)))
                              (begin
                                (|#%app| wakeup_0 t_0)
                                (for-loop_0
                                 (hash-iterate-next threads_0 i_0))))
                            (values)))))))
                   (for-loop_0 (hash-iterate-first threads_0))))
                (void)))
            (void)))
         (args (raise-binding-result-arity-error 2 args))))))
   (lambda () (|#%app| host:get-wakeup-handle))
   (lambda (h_0) (|#%app| host:wakeup h_0))
   (lambda ()
     (not
      (let ((t_0 (unsafe-place-local-ref cell.3$1)))
        (begin-unsafe (not t_0)))))
   (lambda ()
     (if (not
          (let ((t_0 (unsafe-place-local-ref cell.3$1)))
            (begin-unsafe (not t_0))))
       (call-with-values
        (lambda () (min-key+value (unsafe-place-local-ref cell.3$1)))
        (case-lambda
         ((timeout-at_0 threads_0) timeout-at_0)
         (args (raise-binding-result-arity-error 2 args))))
       #f))
   (lambda (t_0 sleep-until_0)
     (begin
       (unsafe-place-local-set!
        cell.3$1
        (insert
         (unsafe-place-local-ref cell.3$1)
         sleep-until_0
         (hash-set
          (let ((or-part_0
                 (lookup (unsafe-place-local-ref cell.3$1) sleep-until_0 <)))
            (if or-part_0 or-part_0 hash2610))
          t_0
          #t)
         <))
       sleep-until_0))
   (lambda (t_0 sleep-until_0)
     (let ((threads_0
            (lookup (unsafe-place-local-ref cell.3$1) sleep-until_0 <)))
       (begin
         (if threads_0
           (void)
           (internal-error "thread not found among sleeping threads"))
         (let ((new-threads_0 (hash-remove threads_0 t_0)))
           (unsafe-place-local-set!
            cell.3$1
            (if (zero? (hash-count new-threads_0))
              (delete (unsafe-place-local-ref cell.3$1) sleep-until_0 <)
              (insert
               (unsafe-place-local-ref cell.3$1)
               sleep-until_0
               new-threads_0
               <)))))))
   (lambda (a-sleep-until_0 b-sleep-until_0)
     (min* a-sleep-until_0 b-sleep-until_0))
   (lambda (sleep-until_0 timeout-at_0)
     (if sleep-until_0 (min sleep-until_0 timeout-at_0) timeout-at_0))
   (lambda (sleep-until_0) sleep-until_0)))
(define effect_2775
  (begin (void (current-sandman the-default-sandman)) (void)))
(define distant-future
  (lambda () (+ (current-inexact-milliseconds) 31536000000.0)))
(define current-atomic (make-pthread-parameter 0))
(define current-thread/in-atomic (make-pthread-parameter #f))
(define current-future$1 (make-pthread-parameter #f))
(define start-atomic
  (lambda ()
    (begin (future-barrier) (current-atomic (fx+ (current-atomic) 1)))))
(define end-atomic
  (lambda ()
    (let ((n_0 (fx- (current-atomic) 1)))
      (if (fx= n_0 0)
        (if (eq? 0 (end-atomic-callback))
          (current-atomic n_0)
          (do-end-atomic-callback))
        (if (fx< n_0 0) (bad-end-atomic) (current-atomic n_0))))))
(define do-end-atomic-callback
  (lambda ()
    (let ((cbs_0 (end-atomic-callback)))
      (begin
        (end-atomic-callback 0)
        (current-atomic 0)
        (letrec*
         ((loop_0
           (|#%name|
            loop
            (lambda (cbs_1)
              (begin
                (if (eq? cbs_1 0)
                  (void)
                  (begin (|#%app| (car cbs_1)) (loop_0 (cdr cbs_1)))))))))
         (loop_0 cbs_0))))))
(define bad-end-atomic
  (lambda () (internal-error "not in atomic mode to end")))
(define start-atomic/no-interrupts
  (lambda () (begin (start-atomic) (|#%app| host:disable-interrupts))))
(define end-atomic/no-interrupts
  (lambda () (begin (|#%app| host:enable-interrupts) (end-atomic))))
(define in-atomic-mode? (lambda () (positive? (current-atomic))))
(define future-barrier
  (lambda () (if (current-future$1) (|#%app| future-block-for-atomic) (void))))
(define end-atomic-callback (make-pthread-parameter 0))
(define add-end-atomic-callback!
  (lambda (cb_0)
    (begin
      (|#%app| host:disable-interrupts)
      (let ((all-cbs_0 (end-atomic-callback)))
        (begin
          (letrec*
           ((loop_0
             (|#%name|
              loop
              (lambda (cbs_0)
                (begin
                  (if (eq? cbs_0 0)
                    (end-atomic-callback (cons cb_0 all-cbs_0))
                    (if (eq? (car cbs_0) cb_0)
                      (void)
                      (loop_0 (cdr cbs_0)))))))))
           (loop_0 all-cbs_0))
          (|#%app| host:enable-interrupts))))))
(define flush-end-atomic-callbacks! (lambda () (end-atomic-callback 0)))
(define future-block-for-atomic (lambda () (void)))
(define set-future-block!
  (lambda (block_0) (set! future-block-for-atomic block_0)))
(define-values
 (1/prop:evt primary-evt? primary-evt-ref)
 (make-struct-type-property
  'evt
  (lambda (v_0 info_0)
    (if (poller? v_0)
      v_0
      (if (1/evt? v_0)
        v_0
        (if (if (procedure? v_0) (procedure-arity-includes? v_0 1) #f)
          v_0
          (if (exact-nonnegative-integer? v_0)
            (let ((init-count_0 (cadr info_0)))
              (begin
                (if (< v_0 init-count_0)
                  (void)
                  (raise-arguments-error
                   'guard-for-prop:evt
                   "index for immutable field >= initialized-field count"
                   "index"
                   v_0
                   "initialized-field count"
                   init-count_0))
                (if (memv v_0 (list-ref info_0 5))
                  (void)
                  (raise-arguments-error
                   'guard-for-prop:evt
                   "field index not declared immutable"
                   "field index"
                   v_0))
                (selector-prop-evt-value1.1
                 (make-struct-field-accessor (list-ref info_0 3) v_0))))
            (raise-argument-error
             'guard-for-prop:evt
             "(or/c evt? (procedure-arity-includes/c 1) exact-nonnegative-integer?)"
             v_0))))))))
(define struct:selector-prop-evt-value
  (make-record-type-descriptor*
   'selector-prop-evt-value
   #f
   (|#%nongenerative-uid| selector-prop-evt-value)
   #f
   #f
   1
   0))
(define effect_3012
  (struct-type-install-properties!
   struct:selector-prop-evt-value
   '(selector-prop-evt-value)
   1
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(0)
   #f
   'selector-prop-evt-value))
(define selector-prop-evt-value1.1
  (|#%name|
   selector-prop-evt-value
   (record-constructor
    (make-record-constructor-descriptor
     struct:selector-prop-evt-value
     #f
     #f))))
(define selector-prop-evt-value?
  (|#%name|
   selector-prop-evt-value?
   (record-predicate struct:selector-prop-evt-value)))
(define selector-prop-evt-value-selector
  (|#%name|
   selector-prop-evt-value-selector
   (record-accessor struct:selector-prop-evt-value 0)))
(define-values
 (prop:secondary-evt secondary-evt? secondary-evt-ref)
 (make-struct-type-property 'secondary-evt))
(define 1/evt?
  (|#%name|
   evt?
   (lambda (v_0)
     (begin
       (let ((or-part_0 (primary-evt? v_0)))
         (if or-part_0 or-part_0 (secondary-evt? v_0)))))))
(define struct:poller
  (make-record-type-descriptor*
   'poller
   #f
   (|#%nongenerative-uid| poller)
   #f
   #f
   1
   0))
(define effect_2322
  (struct-type-install-properties!
   struct:poller
   '(poller)
   1
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(0)
   #f
   'poller))
(define poller2.1
  (|#%name|
   poller
   (record-constructor
    (make-record-constructor-descriptor struct:poller #f #f))))
(define poller? (|#%name| poller? (record-predicate struct:poller)))
(define poller-proc (|#%name| poller-proc (record-accessor struct:poller 0)))
(define struct:poll-ctx
  (make-record-type-descriptor*
   'poll-ctx
   #f
   (|#%nongenerative-uid| poll-ctx)
   #f
   #f
   4
   8))
(define effect_2873
  (struct-type-install-properties!
   struct:poll-ctx
   '(poll-ctx)
   4
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(0 1 2)
   #f
   'poll-ctx))
(define poll-ctx3.1
  (|#%name|
   poll-ctx
   (record-constructor
    (make-record-constructor-descriptor struct:poll-ctx #f #f))))
(define poll-ctx? (|#%name| poll-ctx? (record-predicate struct:poll-ctx)))
(define poll-ctx-poll?
  (|#%name| poll-ctx-poll? (record-accessor struct:poll-ctx 0)))
(define poll-ctx-select-proc
  (|#%name| poll-ctx-select-proc (record-accessor struct:poll-ctx 1)))
(define poll-ctx-sched-info
  (|#%name| poll-ctx-sched-info (record-accessor struct:poll-ctx 2)))
(define poll-ctx-incomplete?
  (|#%name| poll-ctx-incomplete? (record-accessor struct:poll-ctx 3)))
(define set-poll-ctx-incomplete?!
  (|#%name| set-poll-ctx-incomplete?! (record-mutator struct:poll-ctx 3)))
(define struct:never-evt
  (make-record-type-descriptor*
   'never-evt
   #f
   (|#%nongenerative-uid| never-evt)
   #f
   #f
   0
   0))
(define effect_2678
  (struct-type-install-properties!
   struct:never-evt
   '(never-evt)
   0
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1 (lambda (self_0 poll-ctx_0) (values #f self_0)))))
   (current-inspector)
   #f
   '()
   #f
   'never-evt))
(define never-evt4.1
  (|#%name|
   never-evt
   (record-constructor
    (make-record-constructor-descriptor struct:never-evt #f #f))))
(define never-evt?_1958
  (|#%name| never-evt? (record-predicate struct:never-evt)))
(define never-evt?
  (|#%name|
   never-evt?
   (lambda (v)
     (if (never-evt?_1958 v)
       #t
       ($value
        (if (impersonator? v) (never-evt?_1958 (impersonator-val v)) #f))))))
(define the-never-evt (never-evt4.1))
(define struct:always-evt
  (make-record-type-descriptor*
   'always-evt
   #f
   (|#%nongenerative-uid| always-evt)
   #f
   #f
   0
   0))
(define effect_2666
  (struct-type-install-properties!
   struct:always-evt
   '(always-evt)
   0
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1 (lambda (self_0 poll-ctx_0) (values (list self_0) #f)))))
   (current-inspector)
   #f
   '()
   #f
   'always-evt))
(define always-evt5.1
  (|#%name|
   always-evt
   (record-constructor
    (make-record-constructor-descriptor struct:always-evt #f #f))))
(define always-evt?_2466
  (|#%name| always-evt? (record-predicate struct:always-evt)))
(define always-evt?
  (|#%name|
   always-evt?
   (lambda (v)
     (if (always-evt?_2466 v)
       #t
       ($value
        (if (impersonator? v) (always-evt?_2466 (impersonator-val v)) #f))))))
(define the-always-evt (always-evt5.1))
(define struct:async-evt
  (make-record-type-descriptor*
   'async-evt
   #f
   (|#%nongenerative-uid| async-evt)
   #f
   #f
   0
   0))
(define effect_2516
  (struct-type-install-properties!
   struct:async-evt
   '(async-evt)
   0
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1 (lambda (self_0 poll-ctx_0) (values #f self_0)))))
   (current-inspector)
   #f
   '()
   #f
   'async-evt))
(define async-evt6.1
  (|#%name|
   async-evt
   (record-constructor
    (make-record-constructor-descriptor struct:async-evt #f #f))))
(define async-evt?_2619
  (|#%name| async-evt? (record-predicate struct:async-evt)))
(define async-evt?
  (|#%name|
   async-evt?
   (lambda (v)
     (if (async-evt?_2619 v)
       #t
       ($value
        (if (impersonator? v) (async-evt?_2619 (impersonator-val v)) #f))))))
(define the-async-evt (async-evt6.1))
(define struct:wrap-evt
  (make-record-type-descriptor* 'evt #f (|#%nongenerative-uid| evt) #f #f 2 0))
(define effect_2243
  (struct-type-install-properties!
   struct:wrap-evt
   '(evt)
   2
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1 (lambda (self_0 poll-ctx_0) (values #f self_0)))))
   (current-inspector)
   #f
   '(0 1)
   #f
   'wrap-evt))
(define wrap-evt7.1
  (|#%name|
   wrap-evt
   (record-constructor
    (make-record-constructor-descriptor struct:wrap-evt #f #f))))
(define wrap-evt?_2747 (|#%name| evt? (record-predicate struct:wrap-evt)))
(define wrap-evt?
  (|#%name|
   evt?
   (lambda (v)
     (if (wrap-evt?_2747 v)
       #t
       ($value
        (if (impersonator? v) (wrap-evt?_2747 (impersonator-val v)) #f))))))
(define wrap-evt-evt_2872
  (|#%name| evt-evt (record-accessor struct:wrap-evt 0)))
(define wrap-evt-evt
  (|#%name|
   evt-evt
   (lambda (s)
     (if (wrap-evt?_2747 s)
       (wrap-evt-evt_2872 s)
       ($value
        (impersonate-ref wrap-evt-evt_2872 struct:wrap-evt 0 s 'evt 'evt))))))
(define wrap-evt-wrap_3005
  (|#%name| evt-wrap (record-accessor struct:wrap-evt 1)))
(define wrap-evt-wrap
  (|#%name|
   evt-wrap
   (lambda (s)
     (if (wrap-evt?_2747 s)
       (wrap-evt-wrap_3005 s)
       ($value
        (impersonate-ref
         wrap-evt-wrap_3005
         struct:wrap-evt
         1
         s
         'evt
         'wrap))))))
(define struct:handle-evt
  (make-record-type-descriptor*
   'handle-evt
   struct:wrap-evt
   (|#%nongenerative-uid| handle-evt)
   #f
   #f
   0
   0))
(define effect_2575
  (struct-type-install-properties!
   struct:handle-evt
   '(handle-evt)
   0
   0
   struct:wrap-evt
   null
   (current-inspector)
   #f
   '()
   #f
   'handle-evt))
(define handle-evt8.1
  (|#%name|
   handle-evt
   (record-constructor
    (make-record-constructor-descriptor struct:handle-evt #f #f))))
(define handle-evt?$1_2894
  (|#%name| handle-evt? (record-predicate struct:handle-evt)))
(define handle-evt?$1
  (|#%name|
   handle-evt?
   (lambda (v)
     (if (handle-evt?$1_2894 v)
       #t
       ($value
        (if (impersonator? v)
          (handle-evt?$1_2894 (impersonator-val v))
          #f))))))
(define struct:control-state-evt
  (make-record-type-descriptor*
   'control-state-evt
   #f
   (|#%nongenerative-uid| control-state-evt)
   #f
   #f
   5
   0))
(define effect_2497
  (struct-type-install-properties!
   struct:control-state-evt
   '(control-state-evt)
   5
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1 (lambda (self_0 poll-ctx_0) (values #f self_0)))))
   (current-inspector)
   #f
   '(0 1 2 3 4)
   #f
   'control-state-evt))
(define control-state-evt9.1
  (|#%name|
   control-state-evt
   (record-constructor
    (make-record-constructor-descriptor struct:control-state-evt #f #f))))
(define control-state-evt?_2384
  (|#%name| control-state-evt? (record-predicate struct:control-state-evt)))
(define control-state-evt?
  (|#%name|
   control-state-evt?
   (lambda (v)
     (if (control-state-evt?_2384 v)
       #t
       ($value
        (if (impersonator? v)
          (control-state-evt?_2384 (impersonator-val v))
          #f))))))
(define control-state-evt-evt_2775
  (|#%name|
   control-state-evt-evt
   (record-accessor struct:control-state-evt 0)))
(define control-state-evt-evt
  (|#%name|
   control-state-evt-evt
   (lambda (s)
     (if (control-state-evt?_2384 s)
       (control-state-evt-evt_2775 s)
       ($value
        (impersonate-ref
         control-state-evt-evt_2775
         struct:control-state-evt
         0
         s
         'control-state-evt
         'evt))))))
(define control-state-evt-wrap-proc_2332
  (|#%name|
   control-state-evt-wrap-proc
   (record-accessor struct:control-state-evt 1)))
(define control-state-evt-wrap-proc
  (|#%name|
   control-state-evt-wrap-proc
   (lambda (s)
     (if (control-state-evt?_2384 s)
       (control-state-evt-wrap-proc_2332 s)
       ($value
        (impersonate-ref
         control-state-evt-wrap-proc_2332
         struct:control-state-evt
         1
         s
         'control-state-evt
         'wrap-proc))))))
(define control-state-evt-interrupt-proc_2271
  (|#%name|
   control-state-evt-interrupt-proc
   (record-accessor struct:control-state-evt 2)))
(define control-state-evt-interrupt-proc
  (|#%name|
   control-state-evt-interrupt-proc
   (lambda (s)
     (if (control-state-evt?_2384 s)
       (control-state-evt-interrupt-proc_2271 s)
       ($value
        (impersonate-ref
         control-state-evt-interrupt-proc_2271
         struct:control-state-evt
         2
         s
         'control-state-evt
         'interrupt-proc))))))
(define control-state-evt-abandon-proc_2780
  (|#%name|
   control-state-evt-abandon-proc
   (record-accessor struct:control-state-evt 3)))
(define control-state-evt-abandon-proc
  (|#%name|
   control-state-evt-abandon-proc
   (lambda (s)
     (if (control-state-evt?_2384 s)
       (control-state-evt-abandon-proc_2780 s)
       ($value
        (impersonate-ref
         control-state-evt-abandon-proc_2780
         struct:control-state-evt
         3
         s
         'control-state-evt
         'abandon-proc))))))
(define control-state-evt-retry-proc_2119
  (|#%name|
   control-state-evt-retry-proc
   (record-accessor struct:control-state-evt 4)))
(define control-state-evt-retry-proc
  (|#%name|
   control-state-evt-retry-proc
   (lambda (s)
     (if (control-state-evt?_2384 s)
       (control-state-evt-retry-proc_2119 s)
       ($value
        (impersonate-ref
         control-state-evt-retry-proc_2119
         struct:control-state-evt
         4
         s
         'control-state-evt
         'retry-proc))))))
(define struct:poll-guard-evt
  (make-record-type-descriptor* 'evt #f (|#%nongenerative-uid| evt) #f #f 1 0))
(define effect_2340
  (struct-type-install-properties!
   struct:poll-guard-evt
   '(evt)
   1
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1 (lambda (self_0 poll-ctx_0) (values #f self_0)))))
   (current-inspector)
   #f
   '(0)
   #f
   'poll-guard-evt))
(define poll-guard-evt10.1
  (|#%name|
   poll-guard-evt
   (record-constructor
    (make-record-constructor-descriptor struct:poll-guard-evt #f #f))))
(define poll-guard-evt?_2462
  (|#%name| evt? (record-predicate struct:poll-guard-evt)))
(define poll-guard-evt?
  (|#%name|
   evt?
   (lambda (v)
     (if (poll-guard-evt?_2462 v)
       #t
       ($value
        (if (impersonator? v)
          (poll-guard-evt?_2462 (impersonator-val v))
          #f))))))
(define poll-guard-evt-proc_2498
  (|#%name| evt-proc (record-accessor struct:poll-guard-evt 0)))
(define poll-guard-evt-proc
  (|#%name|
   evt-proc
   (lambda (s)
     (if (poll-guard-evt?_2462 s)
       (poll-guard-evt-proc_2498 s)
       ($value
        (impersonate-ref
         poll-guard-evt-proc_2498
         struct:poll-guard-evt
         0
         s
         'evt
         'proc))))))
(define struct:choice-evt
  (make-record-type-descriptor* 'evt #f (|#%nongenerative-uid| evt) #f #f 1 0))
(define effect_2203
  (struct-type-install-properties!
   struct:choice-evt
   '(evt)
   1
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1 (lambda (self_0 poll-ctx_0) (values #f self_0)))))
   (current-inspector)
   #f
   '(0)
   #f
   'choice-evt))
(define choice-evt11.1
  (|#%name|
   choice-evt
   (record-constructor
    (make-record-constructor-descriptor struct:choice-evt #f #f))))
(define choice-evt?_3069 (|#%name| evt? (record-predicate struct:choice-evt)))
(define choice-evt?
  (|#%name|
   evt?
   (lambda (v)
     (if (choice-evt?_3069 v)
       #t
       ($value
        (if (impersonator? v) (choice-evt?_3069 (impersonator-val v)) #f))))))
(define choice-evt-evts_2883
  (|#%name| evt-evts (record-accessor struct:choice-evt 0)))
(define choice-evt-evts
  (|#%name|
   evt-evts
   (lambda (s)
     (if (choice-evt?_3069 s)
       (choice-evt-evts_2883 s)
       ($value
        (impersonate-ref
         choice-evt-evts_2883
         struct:choice-evt
         0
         s
         'evt
         'evts))))))
(define-values
 (impersonator-prop:evt evt-impersonator? evt-impersonator-ref)
 (make-impersonator-property 'evt-impersonator))
(define evt-poll
  (lambda (evt_0 poll-ctx_0)
    (let ((v_0
           (if (|#%app| evt-impersonator? evt_0)
             (|#%app| evt-impersonator-ref evt_0)
             (if (primary-evt? evt_0)
               (primary-evt-ref evt_0)
               (secondary-evt-ref evt_0)))))
      (let ((v_1
             (if (selector-prop-evt-value? v_0)
               (|#%app| (selector-prop-evt-value-selector v_0) evt_0)
               v_0)))
        (if (procedure? v_1)
          (values
           #f
           (delayed-poll12.1
            (lambda ()
              (let ((v_2
                     (call-with-continuation-barrier
                      (lambda () (|#%app| v_1 evt_0)))))
                (if (1/evt? v_2)
                  v_2
                  (if (poller? v_2)
                    (poller-evt13.1 v_2)
                    (wrap-evt7.1 the-always-evt (lambda (v_3) evt_0))))))))
          (if (poller? v_1)
            (|#%app| (poller-proc v_1) evt_0 poll-ctx_0)
            (if (1/evt? v_1) (values #f v_1) (values #f the-never-evt))))))))
(define struct:delayed-poll
  (make-record-type-descriptor*
   'delayed-poll
   #f
   (|#%nongenerative-uid| delayed-poll)
   #f
   #f
   1
   0))
(define effect_2389
  (struct-type-install-properties!
   struct:delayed-poll
   '(delayed-poll)
   1
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(0)
   #f
   'delayed-poll))
(define delayed-poll12.1
  (|#%name|
   delayed-poll
   (record-constructor
    (make-record-constructor-descriptor struct:delayed-poll #f #f))))
(define delayed-poll?
  (|#%name| delayed-poll? (record-predicate struct:delayed-poll)))
(define delayed-poll-resume
  (|#%name| delayed-poll-resume (record-accessor struct:delayed-poll 0)))
(define struct:poller-evt
  (make-record-type-descriptor*
   'poller-evt
   #f
   (|#%nongenerative-uid| poller-evt)
   #f
   #f
   1
   0))
(define effect_2296
  (struct-type-install-properties!
   struct:poller-evt
   '(poller-evt)
   1
   0
   #f
   (list (cons 1/prop:evt 0))
   (current-inspector)
   #f
   '(0)
   #f
   'poller-evt))
(define poller-evt13.1
  (|#%name|
   poller-evt
   (record-constructor
    (make-record-constructor-descriptor struct:poller-evt #f #f))))
(define poller-evt?_2710
  (|#%name| poller-evt? (record-predicate struct:poller-evt)))
(define poller-evt?
  (|#%name|
   poller-evt?
   (lambda (v)
     (if (poller-evt?_2710 v)
       #t
       ($value
        (if (impersonator? v) (poller-evt?_2710 (impersonator-val v)) #f))))))
(define poller-evt-poller_2477
  (|#%name| poller-evt-poller (record-accessor struct:poller-evt 0)))
(define poller-evt-poller
  (|#%name|
   poller-evt-poller
   (lambda (s)
     (if (poller-evt?_2710 s)
       (poller-evt-poller_2477 s)
       ($value
        (impersonate-ref
         poller-evt-poller_2477
         struct:poller-evt
         0
         s
         'poller-evt
         'poller))))))
(define-values
 (prop:waiter waiter? waiter-ref)
 (make-struct-type-property 'waiter))
(define struct:waiter-methods
  (make-record-type-descriptor*
   'waiter-methods
   #f
   (|#%nongenerative-uid| waiter-methods)
   #f
   #f
   2
   0))
(define effect_3276
  (struct-type-install-properties!
   struct:waiter-methods
   '(waiter-methods)
   2
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(0 1)
   #f
   'waiter-methods))
(define waiter-methods1.1
  (|#%name|
   waiter-methods
   (record-constructor
    (make-record-constructor-descriptor struct:waiter-methods #f #f))))
(define waiter-methods?
  (|#%name| waiter-methods? (record-predicate struct:waiter-methods)))
(define waiter-methods-suspend
  (|#%name| waiter-methods-suspend (record-accessor struct:waiter-methods 0)))
(define waiter-methods-resume
  (|#%name| waiter-methods-resume (record-accessor struct:waiter-methods 1)))
(define make-waiter-methods.1
  (|#%name|
   make-waiter-methods
   (lambda (resume!3_0 suspend!2_0)
     (begin (waiter-methods1.1 suspend!2_0 resume!3_0)))))
(define waiter-resume!
  (lambda (w_0 s_0)
    (|#%app| (waiter-methods-resume (waiter-ref w_0)) w_0 s_0)))
(define waiter-suspend!
  (lambda (w_0 interrupt-cb_0)
    (|#%app| (waiter-methods-suspend (waiter-ref w_0)) w_0 interrupt-cb_0)))
(define struct:select-waiter
  (make-record-type-descriptor*
   'select-waiter
   #f
   (|#%nongenerative-uid| select-waiter)
   #f
   #f
   1
   0))
(define effect_2810
  (struct-type-install-properties!
   struct:select-waiter
   '(select-waiter)
   1
   0
   #f
   (list
    (cons
     prop:waiter
     (let ((temp10_0
            (lambda args_0
              (internal-error "should not suspend a select-waiter"))))
       (let ((temp11_0
              (lambda (w_0 s_0) (|#%app| (|#%app| select-waiter-proc w_0)))))
         (make-waiter-methods.1 temp11_0 temp10_0)))))
   (current-inspector)
   #f
   '(0)
   #f
   'select-waiter))
(define select-waiter7.1
  (|#%name|
   select-waiter
   (record-constructor
    (make-record-constructor-descriptor struct:select-waiter #f #f))))
(define select-waiter?_2648
  (|#%name| select-waiter? (record-predicate struct:select-waiter)))
(define select-waiter?
  (|#%name|
   select-waiter?
   (lambda (v)
     (if (select-waiter?_2648 v)
       #t
       ($value
        (if (impersonator? v)
          (select-waiter?_2648 (impersonator-val v))
          #f))))))
(define select-waiter-proc_2543
  (|#%name| select-waiter-proc (record-accessor struct:select-waiter 0)))
(define select-waiter-proc
  (|#%name|
   select-waiter-proc
   (lambda (s)
     (if (select-waiter?_2648 s)
       (select-waiter-proc_2543 s)
       ($value
        (impersonate-ref
         select-waiter-proc_2543
         struct:select-waiter
         0
         s
         'select-waiter
         'proc))))))
(define struct:custodian
  (make-record-type-descriptor*
   'custodian
   #f
   (|#%nongenerative-uid| custodian)
   #f
   #f
   13
   8188))
(define effect_2862
  (struct-type-install-properties!
   struct:custodian
   '(custodian)
   13
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(0 1)
   #f
   'custodian))
(define custodian1.1
  (|#%name|
   custodian
   (record-constructor
    (make-record-constructor-descriptor struct:custodian #f #f))))
(define 1/custodian? (|#%name| custodian? (record-predicate struct:custodian)))
(define custodian-children
  (|#%name| custodian-children (record-accessor struct:custodian 0)))
(define custodian-shut-down?-box
  (|#%name| custodian-shut-down?-box (record-accessor struct:custodian 1)))
(define custodian-shutdown-sema
  (|#%name| custodian-shutdown-sema (record-accessor struct:custodian 2)))
(define custodian-need-shutdown
  (|#%name| custodian-need-shutdown (record-accessor struct:custodian 3)))
(define custodian-parent-reference
  (|#%name| custodian-parent-reference (record-accessor struct:custodian 4)))
(define custodian-self-reference
  (|#%name| custodian-self-reference (record-accessor struct:custodian 5)))
(define custodian-place
  (|#%name| custodian-place (record-accessor struct:custodian 6)))
(define custodian-memory-use
  (|#%name| custodian-memory-use (record-accessor struct:custodian 7)))
(define custodian-gc-roots
  (|#%name| custodian-gc-roots (record-accessor struct:custodian 8)))
(define custodian-memory-limits
  (|#%name| custodian-memory-limits (record-accessor struct:custodian 9)))
(define custodian-immediate-limit
  (|#%name| custodian-immediate-limit (record-accessor struct:custodian 10)))
(define custodian-sync-futures?
  (|#%name| custodian-sync-futures? (record-accessor struct:custodian 11)))
(define custodian-post-shutdown
  (|#%name| custodian-post-shutdown (record-accessor struct:custodian 12)))
(define set-custodian-shutdown-sema!
  (|#%name| set-custodian-shutdown-sema! (record-mutator struct:custodian 2)))
(define set-custodian-need-shutdown!
  (|#%name| set-custodian-need-shutdown! (record-mutator struct:custodian 3)))
(define set-custodian-parent-reference!
  (|#%name|
   set-custodian-parent-reference!
   (record-mutator struct:custodian 4)))
(define set-custodian-self-reference!
  (|#%name| set-custodian-self-reference! (record-mutator struct:custodian 5)))
(define set-custodian-place!
  (|#%name| set-custodian-place! (record-mutator struct:custodian 6)))
(define set-custodian-memory-use!
  (|#%name| set-custodian-memory-use! (record-mutator struct:custodian 7)))
(define set-custodian-gc-roots!
  (|#%name| set-custodian-gc-roots! (record-mutator struct:custodian 8)))
(define set-custodian-memory-limits!
  (|#%name| set-custodian-memory-limits! (record-mutator struct:custodian 9)))
(define set-custodian-immediate-limit!
  (|#%name|
   set-custodian-immediate-limit!
   (record-mutator struct:custodian 10)))
(define set-custodian-sync-futures?!
  (|#%name| set-custodian-sync-futures?! (record-mutator struct:custodian 11)))
(define set-custodian-post-shutdown!
  (|#%name| set-custodian-post-shutdown! (record-mutator struct:custodian 12)))
(define create-custodian
  (lambda (parent_0)
    (let ((app_0 (make-weak-hasheq)))
      (custodian1.1 app_0 (box #f) #f #f #f #f #f 0 #f null #f #f null))))
(define 1/custodian-shut-down?
  (|#%name|
   custodian-shut-down?
   (lambda (c_0) (begin (unsafe-unbox* (custodian-shut-down?-box c_0))))))
(define set-custodian-shut-down!
  (lambda (c_0)
    (if (unsafe-box*-cas! (custodian-shut-down?-box c_0) #f #t)
      (void)
      (set-custodian-shut-down! c_0))))
(define custodian-shut-down?/other-pthread
  (lambda (c_0)
    (if (unsafe-box*-cas! (custodian-shut-down?-box c_0) #f #f)
      #f
      (if (unsafe-box*-cas! (custodian-shut-down?-box c_0) #t #t)
        #t
        (custodian-shut-down?/other-pthread c_0)))))
(define initial-place-root-custodian (create-custodian #f))
(define cell.1$6 (unsafe-make-place-local initial-place-root-custodian))
(define immutable-prefab-struct-key
  (lambda (v_0)
    (let ((k_0 (prefab-struct-key v_0)))
      (if k_0 (if (all-fields-immutable? k_0) k_0 #f) #f))))
(define all-fields-immutable?
  (lambda (k_0)
    (let ((or-part_0 (symbol? k_0)))
      (if or-part_0
        or-part_0
        (let ((or-part_1 (null? k_0)))
          (if or-part_1
            or-part_1
            (let ((rk_0 (cdr k_0)))
              (let ((rk_1
                     (if (if (pair? rk_0) (exact-integer? (car rk_0)) #f)
                       (cdr rk_0)
                       rk_0)))
                (let ((rk_2
                       (if (if (pair? rk_1) (pair? (car rk_1)) #f)
                         (if (zero? (caar rk_1))
                           (cdr rk_1)
                           (cons '#(1) (cdr rk_1)))
                         rk_1)))
                  (if (if (pair? rk_2) (vector? (car rk_2)) #f)
                    (if (zero? (vector-length (car rk_2)))
                      (all-fields-immutable? (cdr rk_2))
                      #f)
                    (all-fields-immutable? rk_2)))))))))))
(define-values
 (prop:place-message place-message? place-message-ref)
 (make-struct-type-property 'place-message))
(define struct:message-ized
  (make-record-type-descriptor*
   'message-ized
   #f
   (|#%nongenerative-uid| message-ized)
   #f
   #f
   1
   0))
(define effect_2533
  (struct-type-install-properties!
   struct:message-ized
   '(message-ized)
   1
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(0)
   #f
   'message-ized))
(define message-ized1.1
  (|#%name|
   message-ized
   (record-constructor
    (make-record-constructor-descriptor struct:message-ized #f #f))))
(define message-ized?
  (|#%name| message-ized? (record-predicate struct:message-ized)))
(define message-ized-unmessage
  (|#%name| message-ized-unmessage (record-accessor struct:message-ized 0)))
(define allowed?.1
  (|#%name|
   allowed?
   (lambda (direct?2_0 v4_0)
     (begin
       (letrec*
        ((loop_0
          (|#%name|
           loop
           (lambda (v_0 graph_0)
             (begin
               (let ((or-part_0 (number? v_0)))
                 (if or-part_0
                   or-part_0
                   (let ((or-part_1 (char? v_0)))
                     (if or-part_1
                       or-part_1
                       (let ((or-part_2 (boolean? v_0)))
                         (if or-part_2
                           or-part_2
                           (let ((or-part_3 (keyword? v_0)))
                             (if or-part_3
                               or-part_3
                               (let ((or-part_4 (void? v_0)))
                                 (if or-part_4
                                   or-part_4
                                   (let ((or-part_5 (symbol? v_0)))
                                     (if or-part_5
                                       or-part_5
                                       (let ((or-part_6
                                              (if (let ((or-part_6
                                                         (string? v_0)))
                                                    (if or-part_6
                                                      or-part_6
                                                      (bytes? v_0)))
                                                (let ((or-part_6
                                                       (not direct?2_0)))
                                                  (if or-part_6
                                                    or-part_6
                                                    (let ((or-part_7
                                                           (immutable? v_0)))
                                                      (if or-part_7
                                                        or-part_7
                                                        (if (bytes? v_0)
                                                          (place-shared? v_0)
                                                          #f)))))
                                                #f)))
                                         (if or-part_6
                                           or-part_6
                                           (let ((or-part_7 (null? v_0)))
                                             (if or-part_7
                                               or-part_7
                                               (let ((or-part_8
                                                      (if (pair? v_0)
                                                        (let ((or-part_8
                                                               (hash-ref
                                                                graph_0
                                                                v_0
                                                                #f)))
                                                          (if or-part_8
                                                            or-part_8
                                                            (let ((graph_1
                                                                   (hash-set
                                                                    graph_0
                                                                    v_0
                                                                    #t)))
                                                              (if (loop_0
                                                                   (car v_0)
                                                                   graph_1)
                                                                (loop_0
                                                                 (cdr v_0)
                                                                 graph_1)
                                                                #f))))
                                                        #f)))
                                                 (if or-part_8
                                                   or-part_8
                                                   (let ((or-part_9
                                                          (if (vector? v_0)
                                                            (if (let ((or-part_9
                                                                       (not
                                                                        direct?2_0)))
                                                                  (if or-part_9
                                                                    or-part_9
                                                                    (if (immutable?
                                                                         v_0)
                                                                      (not
                                                                       (impersonator?
                                                                        v_0))
                                                                      #f)))
                                                              (let ((or-part_9
                                                                     (hash-ref
                                                                      graph_0
                                                                      v_0
                                                                      #f)))
                                                                (if or-part_9
                                                                  or-part_9
                                                                  (let ((graph_1
                                                                         (hash-set
                                                                          graph_0
                                                                          v_0
                                                                          #t)))
                                                                    (call-with-values
                                                                     (lambda ()
                                                                       (begin
                                                                         (check-vector
                                                                          v_0)
                                                                         (values
                                                                          v_0
                                                                          (unsafe-vector-length
                                                                           v_0))))
                                                                     (case-lambda
                                                                      ((vec_0
                                                                        len_0)
                                                                       (begin
                                                                         #f
                                                                         (letrec*
                                                                          ((for-loop_0
                                                                            (|#%name|
                                                                             for-loop
                                                                             (lambda (result_0
                                                                                      pos_0)
                                                                               (begin
                                                                                 (if (unsafe-fx<
                                                                                      pos_0
                                                                                      len_0)
                                                                                   (let ((e_0
                                                                                          (unsafe-vector-ref
                                                                                           vec_0
                                                                                           pos_0)))
                                                                                     (let ((result_1
                                                                                            (let ((result_1
                                                                                                   (loop_0
                                                                                                    e_0
                                                                                                    graph_1)))
                                                                                              (values
                                                                                               result_1))))
                                                                                       (if (if (not
                                                                                                (let ((x_0
                                                                                                       (list
                                                                                                        e_0)))
                                                                                                  (not
                                                                                                   result_1)))
                                                                                             #t
                                                                                             #f)
                                                                                         (for-loop_0
                                                                                          result_1
                                                                                          (unsafe-fx+
                                                                                           1
                                                                                           pos_0))
                                                                                         result_1)))
                                                                                   result_0))))))
                                                                          (for-loop_0
                                                                           #t
                                                                           0))))
                                                                      (args
                                                                       (raise-binding-result-arity-error
                                                                        2
                                                                        args)))))))
                                                              #f)
                                                            #f)))
                                                     (if or-part_9
                                                       or-part_9
                                                       (let ((or-part_10
                                                              (if (immutable-prefab-struct-key
                                                                   v_0)
                                                                (let ((or-part_10
                                                                       (hash-ref
                                                                        graph_0
                                                                        v_0
                                                                        #f)))
                                                                  (if or-part_10
                                                                    or-part_10
                                                                    (let ((graph_1
                                                                           (hash-set
                                                                            graph_0
                                                                            v_0
                                                                            #t)))
                                                                      (call-with-values
                                                                       (lambda ()
                                                                         (let ((vec_0
                                                                                (struct->vector
                                                                                 v_0)))
                                                                           (begin
                                                                             (check-vector
                                                                              vec_0)
                                                                             (values
                                                                              vec_0
                                                                              (unsafe-vector-length
                                                                               vec_0)))))
                                                                       (case-lambda
                                                                        ((vec_0
                                                                          len_0)
                                                                         (begin
                                                                           #f
                                                                           (letrec*
                                                                            ((for-loop_0
                                                                              (|#%name|
                                                                               for-loop
                                                                               (lambda (result_0
                                                                                        pos_0)
                                                                                 (begin
                                                                                   (if (unsafe-fx<
                                                                                        pos_0
                                                                                        len_0)
                                                                                     (let ((e_0
                                                                                            (unsafe-vector-ref
                                                                                             vec_0
                                                                                             pos_0)))
                                                                                       (let ((result_1
                                                                                              (let ((result_1
                                                                                                     (loop_0
                                                                                                      e_0
                                                                                                      graph_1)))
                                                                                                (values
                                                                                                 result_1))))
                                                                                         (if (if (not
                                                                                                  (let ((x_0
                                                                                                         (list
                                                                                                          e_0)))
                                                                                                    (not
                                                                                                     result_1)))
                                                                                               #t
                                                                                               #f)
                                                                                           (for-loop_0
                                                                                            result_1
                                                                                            (unsafe-fx+
                                                                                             1
                                                                                             pos_0))
                                                                                           result_1)))
                                                                                     result_0))))))
                                                                            (for-loop_0
                                                                             #t
                                                                             0))))
                                                                        (args
                                                                         (raise-binding-result-arity-error
                                                                          2
                                                                          args)))))))
                                                                #f)))
                                                         (if or-part_10
                                                           or-part_10
                                                           (let ((or-part_11
                                                                  (if (hash?
                                                                       v_0)
                                                                    (if (let ((or-part_11
                                                                               (not
                                                                                direct?2_0)))
                                                                          (if or-part_11
                                                                            or-part_11
                                                                            (if (immutable?
                                                                                 v_0)
                                                                              (not
                                                                               (impersonator?
                                                                                v_0))
                                                                              #f)))
                                                                      (let ((or-part_11
                                                                             (hash-ref
                                                                              graph_0
                                                                              v_0
                                                                              #f)))
                                                                        (if or-part_11
                                                                          or-part_11
                                                                          (let ((graph_1
                                                                                 (hash-set
                                                                                  graph_0
                                                                                  v_0
                                                                                  #t)))
                                                                            (begin
                                                                              (letrec*
                                                                               ((for-loop_0
                                                                                 (|#%name|
                                                                                  for-loop
                                                                                  (lambda (result_0
                                                                                           i_0)
                                                                                    (begin
                                                                                      (if i_0
                                                                                        (call-with-values
                                                                                         (lambda ()
                                                                                           (hash-iterate-key+value
                                                                                            v_0
                                                                                            i_0))
                                                                                         (case-lambda
                                                                                          ((k_0
                                                                                            v_1)
                                                                                           (let ((result_1
                                                                                                  (let ((result_1
                                                                                                         (if (loop_0
                                                                                                              k_0
                                                                                                              graph_1)
                                                                                                           (loop_0
                                                                                                            v_1
                                                                                                            graph_1)
                                                                                                           #f)))
                                                                                                    (values
                                                                                                     result_1))))
                                                                                             (if (if (not
                                                                                                      (let ((x_0
                                                                                                             (list
                                                                                                              k_0
                                                                                                              v_1)))
                                                                                                        (not
                                                                                                         result_1)))
                                                                                                   #t
                                                                                                   #f)
                                                                                               (for-loop_0
                                                                                                result_1
                                                                                                (hash-iterate-next
                                                                                                 v_0
                                                                                                 i_0))
                                                                                               result_1)))
                                                                                          (args
                                                                                           (raise-binding-result-arity-error
                                                                                            2
                                                                                            args))))
                                                                                        result_0))))))
                                                                               (for-loop_0
                                                                                #t
                                                                                (hash-iterate-first
                                                                                 v_0)))))))
                                                                      #f)
                                                                    #f)))
                                                             (if or-part_11
                                                               or-part_11
                                                               (if (not
                                                                    direct?2_0)
                                                                 (let ((or-part_12
                                                                        (cpointer?
                                                                         v_0)))
                                                                   (if or-part_12
                                                                     or-part_12
                                                                     (let ((or-part_13
                                                                            (if (let ((or-part_13
                                                                                       (fxvector?
                                                                                        v_0)))
                                                                                  (if or-part_13
                                                                                    or-part_13
                                                                                    (let ((or-part_14
                                                                                           (flvector?
                                                                                            v_0)))
                                                                                      (if or-part_14
                                                                                        or-part_14
                                                                                        (bytes?
                                                                                         v_0)))))
                                                                              (place-shared?
                                                                               v_0)
                                                                              #f)))
                                                                       (if or-part_13
                                                                         or-part_13
                                                                         (if (place-message?
                                                                              v_0)
                                                                           (if (|#%app|
                                                                                (place-message-ref
                                                                                 v_0)
                                                                                v_0)
                                                                             #t
                                                                             #f)
                                                                           #f)))))
                                                                 #f))))))))))))))))))))))))))))))
        (loop_0 v4_0 hash2610))))))
(define place-message-allowed-direct? (lambda (v_0) (allowed?.1 #t v_0)))
(define 1/place-message-allowed?
  (|#%name| place-message-allowed? (lambda (v_0) (begin (allowed?.1 #f v_0)))))
(define message-ize
  (lambda (v_0 fail_0)
    (let ((graph_0 #f))
      (let ((used_0 #f))
        (let ((maybe-ph_0
               (|#%name|
                maybe-ph
                (lambda (ph_0 v_1 new-v_0)
                  (begin
                    (if (if used_0 (hash-ref used_0 ph_0 #f) #f)
                      (begin (placeholder-set! ph_0 new-v_0) ph_0)
                      (begin (hash-remove! graph_0 v_1) new-v_0)))))))
          (let ((new-v_0
                 (letrec*
                  ((loop_0
                    (|#%name|
                     loop
                     (lambda (v_1)
                       (begin
                         (if (let ((or-part_0 (number? v_1)))
                               (if or-part_0
                                 or-part_0
                                 (let ((or-part_1 (char? v_1)))
                                   (if or-part_1
                                     or-part_1
                                     (let ((or-part_2 (boolean? v_1)))
                                       (if or-part_2
                                         or-part_2
                                         (let ((or-part_3 (keyword? v_1)))
                                           (if or-part_3
                                             or-part_3
                                             (let ((or-part_4 (void? v_1)))
                                               (if or-part_4
                                                 or-part_4
                                                 (let ((or-part_5
                                                        (symbol? v_1)))
                                                   (if or-part_5
                                                     or-part_5
                                                     (null? v_1)))))))))))))
                           v_1
                           (if (string? v_1)
                             (string->immutable-string v_1)
                             (if (bytes? v_1)
                               (if (place-shared? v_1)
                                 v_1
                                 (bytes->immutable-bytes v_1))
                               (begin
                                 (if graph_0
                                   (void)
                                   (set! graph_0 (make-hasheq)))
                                 (let ((c2_0 (hash-ref graph_0 v_1 #f)))
                                   (if c2_0
                                     (begin
                                       (if used_0
                                         (void)
                                         (set! used_0 (make-hasheq)))
                                       (hash-set! used_0 c2_0 #t)
                                       c2_0)
                                     (if (pair? v_1)
                                       (let ((ph_0 (make-placeholder #f)))
                                         (begin
                                           (hash-set! graph_0 v_1 ph_0)
                                           (maybe-ph_0
                                            ph_0
                                            v_1
                                            (let ((app_0 (loop_0 (car v_1))))
                                              (cons
                                               app_0
                                               (loop_0 (cdr v_1)))))))
                                       (if (vector? v_1)
                                         (let ((ph_0 (make-placeholder #f)))
                                           (begin
                                             (hash-set! graph_0 v_1 ph_0)
                                             (maybe-ph_0
                                              ph_0
                                              v_1
                                              (let ((len_0
                                                     (vector-length v_1)))
                                                (begin
                                                  (if (exact-nonnegative-integer?
                                                       len_0)
                                                    (void)
                                                    (raise-argument-error
                                                     'for/vector
                                                     "exact-nonnegative-integer?"
                                                     len_0))
                                                  (let ((v_2
                                                         (make-vector
                                                          len_0
                                                          0)))
                                                    (begin
                                                      (if (zero? len_0)
                                                        (void)
                                                        (call-with-values
                                                         (lambda ()
                                                           (begin
                                                             (check-vector v_1)
                                                             (values
                                                              v_1
                                                              (unsafe-vector-length
                                                               v_1))))
                                                         (case-lambda
                                                          ((vec_0 len_1)
                                                           (begin
                                                             #f
                                                             (letrec*
                                                              ((for-loop_0
                                                                (|#%name|
                                                                 for-loop
                                                                 (lambda (i_0
                                                                          pos_0)
                                                                   (begin
                                                                     (if (unsafe-fx<
                                                                          pos_0
                                                                          len_1)
                                                                       (let ((e_0
                                                                              (unsafe-vector-ref
                                                                               vec_0
                                                                               pos_0)))
                                                                         (let ((i_1
                                                                                (let ((i_1
                                                                                       (begin
                                                                                         (unsafe-vector*-set!
                                                                                          v_2
                                                                                          i_0
                                                                                          (loop_0
                                                                                           e_0))
                                                                                         (unsafe-fx+
                                                                                          1
                                                                                          i_0))))
                                                                                  (values
                                                                                   i_1))))
                                                                           (if (if (not
                                                                                    (let ((x_0
                                                                                           (list
                                                                                            e_0)))
                                                                                      (unsafe-fx=
                                                                                       i_1
                                                                                       len_0)))
                                                                                 #t
                                                                                 #f)
                                                                             (for-loop_0
                                                                              i_1
                                                                              (unsafe-fx+
                                                                               1
                                                                               pos_0))
                                                                             i_1)))
                                                                       i_0))))))
                                                              (for-loop_0
                                                               0
                                                               0))))
                                                          (args
                                                           (raise-binding-result-arity-error
                                                            2
                                                            args)))))
                                                      v_2)))))))
                                         (let ((c1_0
                                                (immutable-prefab-struct-key
                                                 v_1)))
                                           (if c1_0
                                             (let ((ph_0
                                                    (make-placeholder #f)))
                                               (begin
                                                 (hash-set! graph_0 v_1 ph_0)
                                                 (maybe-ph_0
                                                  ph_0
                                                  v_1
                                                  (apply
                                                   make-prefab-struct
                                                   c1_0
                                                   (reverse$1
                                                    (call-with-values
                                                     (lambda ()
                                                       (unsafe-normalise-inputs
                                                        unsafe-vector-length
                                                        (struct->vector v_1)
                                                        1
                                                        #f
                                                        1))
                                                     (case-lambda
                                                      ((v*_0
                                                        start*_0
                                                        stop*_0
                                                        step*_0)
                                                       (begin
                                                         #t
                                                         (letrec*
                                                          ((for-loop_0
                                                            (|#%name|
                                                             for-loop
                                                             (lambda (fold-var_0
                                                                      idx_0)
                                                               (begin
                                                                 (if (unsafe-fx<
                                                                      idx_0
                                                                      stop*_0)
                                                                   (let ((e_0
                                                                          (unsafe-vector-ref
                                                                           v*_0
                                                                           idx_0)))
                                                                     (let ((fold-var_1
                                                                            (let ((fold-var_1
                                                                                   (cons
                                                                                    (loop_0
                                                                                     e_0)
                                                                                    fold-var_0)))
                                                                              (values
                                                                               fold-var_1))))
                                                                       (for-loop_0
                                                                        fold-var_1
                                                                        (unsafe-fx+
                                                                         idx_0
                                                                         1))))
                                                                   fold-var_0))))))
                                                          (for-loop_0
                                                           null
                                                           start*_0))))
                                                      (args
                                                       (raise-binding-result-arity-error
                                                        4
                                                        args)))))))))
                                             (if (hash? v_1)
                                               (let ((ph_0
                                                      (make-placeholder #f)))
                                                 (begin
                                                   (hash-set! graph_0 v_1 ph_0)
                                                   (maybe-ph_0
                                                    ph_0
                                                    v_1
                                                    (if (hash-eq? v_1)
                                                      (begin
                                                        (letrec*
                                                         ((for-loop_0
                                                           (|#%name|
                                                            for-loop
                                                            (lambda (table_0
                                                                     i_0)
                                                              (begin
                                                                (if i_0
                                                                  (call-with-values
                                                                   (lambda ()
                                                                     (hash-iterate-key+value
                                                                      v_1
                                                                      i_0))
                                                                   (case-lambda
                                                                    ((k_0 v_2)
                                                                     (let ((table_1
                                                                            (let ((table_1
                                                                                   (call-with-values
                                                                                    (lambda ()
                                                                                      (let ((app_0
                                                                                             (loop_0
                                                                                              k_0)))
                                                                                        (values
                                                                                         app_0
                                                                                         (loop_0
                                                                                          v_2))))
                                                                                    (case-lambda
                                                                                     ((key_0
                                                                                       val_0)
                                                                                      (hash-set
                                                                                       table_0
                                                                                       key_0
                                                                                       val_0))
                                                                                     (args
                                                                                      (raise-binding-result-arity-error
                                                                                       2
                                                                                       args))))))
                                                                              (values
                                                                               table_1))))
                                                                       (for-loop_0
                                                                        table_1
                                                                        (hash-iterate-next
                                                                         v_1
                                                                         i_0))))
                                                                    (args
                                                                     (raise-binding-result-arity-error
                                                                      2
                                                                      args))))
                                                                  table_0))))))
                                                         (for-loop_0
                                                          hash2610
                                                          (hash-iterate-first
                                                           v_1))))
                                                      (if (hash-eqv? v_1)
                                                        (begin
                                                          (letrec*
                                                           ((for-loop_0
                                                             (|#%name|
                                                              for-loop
                                                              (lambda (table_0
                                                                       i_0)
                                                                (begin
                                                                  (if i_0
                                                                    (call-with-values
                                                                     (lambda ()
                                                                       (hash-iterate-key+value
                                                                        v_1
                                                                        i_0))
                                                                     (case-lambda
                                                                      ((k_0
                                                                        v_2)
                                                                       (let ((table_1
                                                                              (let ((table_1
                                                                                     (call-with-values
                                                                                      (lambda ()
                                                                                        (let ((app_0
                                                                                               (loop_0
                                                                                                k_0)))
                                                                                          (values
                                                                                           app_0
                                                                                           (loop_0
                                                                                            v_2))))
                                                                                      (case-lambda
                                                                                       ((key_0
                                                                                         val_0)
                                                                                        (hash-set
                                                                                         table_0
                                                                                         key_0
                                                                                         val_0))
                                                                                       (args
                                                                                        (raise-binding-result-arity-error
                                                                                         2
                                                                                         args))))))
                                                                                (values
                                                                                 table_1))))
                                                                         (for-loop_0
                                                                          table_1
                                                                          (hash-iterate-next
                                                                           v_1
                                                                           i_0))))
                                                                      (args
                                                                       (raise-binding-result-arity-error
                                                                        2
                                                                        args))))
                                                                    table_0))))))
                                                           (for-loop_0
                                                            hash2589
                                                            (hash-iterate-first
                                                             v_1))))
                                                        (begin
                                                          (letrec*
                                                           ((for-loop_0
                                                             (|#%name|
                                                              for-loop
                                                              (lambda (table_0
                                                                       i_0)
                                                                (begin
                                                                  (if i_0
                                                                    (call-with-values
                                                                     (lambda ()
                                                                       (hash-iterate-key+value
                                                                        v_1
                                                                        i_0))
                                                                     (case-lambda
                                                                      ((k_0
                                                                        v_2)
                                                                       (let ((table_1
                                                                              (let ((table_1
                                                                                     (call-with-values
                                                                                      (lambda ()
                                                                                        (let ((app_0
                                                                                               (loop_0
                                                                                                k_0)))
                                                                                          (values
                                                                                           app_0
                                                                                           (loop_0
                                                                                            v_2))))
                                                                                      (case-lambda
                                                                                       ((key_0
                                                                                         val_0)
                                                                                        (hash-set
                                                                                         table_0
                                                                                         key_0
                                                                                         val_0))
                                                                                       (args
                                                                                        (raise-binding-result-arity-error
                                                                                         2
                                                                                         args))))))
                                                                                (values
                                                                                 table_1))))
                                                                         (for-loop_0
                                                                          table_1
                                                                          (hash-iterate-next
                                                                           v_1
                                                                           i_0))))
                                                                      (args
                                                                       (raise-binding-result-arity-error
                                                                        2
                                                                        args))))
                                                                    table_0))))))
                                                           (for-loop_0
                                                            hash2725
                                                            (hash-iterate-first
                                                             v_1)))))))))
                                               (if (cpointer? v_1)
                                                 (ptr-add v_1 0)
                                                 (if (if (let ((or-part_0
                                                                (fxvector?
                                                                 v_1)))
                                                           (if or-part_0
                                                             or-part_0
                                                             (flvector? v_1)))
                                                       (place-shared? v_1)
                                                       #f)
                                                   v_1
                                                   (if (place-message? v_1)
                                                     (let ((make-unmessager_0
                                                            (|#%app|
                                                             (place-message-ref
                                                              v_1)
                                                             v_1)))
                                                       (if make-unmessager_0
                                                         (message-ized1.1
                                                          (|#%app|
                                                           make-unmessager_0))
                                                         (|#%app| fail_0)))
                                                     (|#%app|
                                                      fail_0))))))))))))))))))))
                  (loop_0 v_0))))
            (message-ized1.1 new-v_0)))))))
(define un-message-ize
  (lambda (v_0)
    (if (message-ized? v_0)
      (make-reader-graph (do-un-message-ize (message-ized-unmessage v_0)))
      v_0)))
(define do-un-message-ize
  (lambda (v_0)
    (let ((graph_0 #f))
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda (v_1)
            (begin
              (if (placeholder? v_1)
                (let ((ph_0 (make-placeholder #f)))
                  (begin
                    (if graph_0 (void) (set! graph_0 (make-hasheq)))
                    (let ((c4_0 (hash-ref graph_0 v_1 #f)))
                      (if c4_0
                        c4_0
                        (begin
                          (hash-set! graph_0 v_1 ph_0)
                          (placeholder-set!
                           ph_0
                           (loop_0 (placeholder-get v_1)))
                          ph_0)))))
                (if (pair? v_1)
                  (let ((app_0 (loop_0 (car v_1))))
                    (cons app_0 (loop_0 (cdr v_1))))
                  (if (vector? v_1)
                    (vector->immutable-vector
                     (let ((len_0 (vector-length v_1)))
                       (begin
                         (if (exact-nonnegative-integer? len_0)
                           (void)
                           (raise-argument-error
                            'for/vector
                            "exact-nonnegative-integer?"
                            len_0))
                         (let ((v_2 (make-vector len_0 0)))
                           (begin
                             (if (zero? len_0)
                               (void)
                               (call-with-values
                                (lambda ()
                                  (begin
                                    (check-vector v_1)
                                    (values v_1 (unsafe-vector-length v_1))))
                                (case-lambda
                                 ((vec_0 len_1)
                                  (begin
                                    #f
                                    (letrec*
                                     ((for-loop_0
                                       (|#%name|
                                        for-loop
                                        (lambda (i_0 pos_0)
                                          (begin
                                            (if (unsafe-fx< pos_0 len_1)
                                              (let ((e_0
                                                     (unsafe-vector-ref
                                                      vec_0
                                                      pos_0)))
                                                (let ((i_1
                                                       (let ((i_1
                                                              (begin
                                                                (unsafe-vector*-set!
                                                                 v_2
                                                                 i_0
                                                                 (loop_0 e_0))
                                                                (unsafe-fx+
                                                                 1
                                                                 i_0))))
                                                         (values i_1))))
                                                  (if (if (not
                                                           (let ((x_0
                                                                  (list e_0)))
                                                             (unsafe-fx=
                                                              i_1
                                                              len_0)))
                                                        #t
                                                        #f)
                                                    (for-loop_0
                                                     i_1
                                                     (unsafe-fx+ 1 pos_0))
                                                    i_1)))
                                              i_0))))))
                                     (for-loop_0 0 0))))
                                 (args
                                  (raise-binding-result-arity-error 2 args)))))
                             v_2)))))
                    (let ((c3_0 (immutable-prefab-struct-key v_1)))
                      (if c3_0
                        (apply
                         make-prefab-struct
                         c3_0
                         (reverse$1
                          (call-with-values
                           (lambda ()
                             (unsafe-normalise-inputs
                              unsafe-vector-length
                              (struct->vector v_1)
                              1
                              #f
                              1))
                           (case-lambda
                            ((v*_0 start*_0 stop*_0 step*_0)
                             (begin
                               #t
                               (letrec*
                                ((for-loop_0
                                  (|#%name|
                                   for-loop
                                   (lambda (fold-var_0 idx_0)
                                     (begin
                                       (if (unsafe-fx< idx_0 stop*_0)
                                         (let ((e_0
                                                (unsafe-vector-ref
                                                 v*_0
                                                 idx_0)))
                                           (let ((fold-var_1
                                                  (let ((fold-var_1
                                                         (cons
                                                          (loop_0 e_0)
                                                          fold-var_0)))
                                                    (values fold-var_1))))
                                             (for-loop_0
                                              fold-var_1
                                              (unsafe-fx+ idx_0 1))))
                                         fold-var_0))))))
                                (for-loop_0 null start*_0))))
                            (args
                             (raise-binding-result-arity-error 4 args))))))
                        (if (hash? v_1)
                          (if (hash-eq? v_1)
                            (begin
                              (letrec*
                               ((for-loop_0
                                 (|#%name|
                                  for-loop
                                  (lambda (table_0 i_0)
                                    (begin
                                      (if i_0
                                        (call-with-values
                                         (lambda ()
                                           (hash-iterate-key+value v_1 i_0))
                                         (case-lambda
                                          ((k_0 v_2)
                                           (let ((table_1
                                                  (let ((table_1
                                                         (call-with-values
                                                          (lambda ()
                                                            (let ((app_0
                                                                   (loop_0
                                                                    k_0)))
                                                              (values
                                                               app_0
                                                               (loop_0 v_2))))
                                                          (case-lambda
                                                           ((key_0 val_0)
                                                            (hash-set
                                                             table_0
                                                             key_0
                                                             val_0))
                                                           (args
                                                            (raise-binding-result-arity-error
                                                             2
                                                             args))))))
                                                    (values table_1))))
                                             (for-loop_0
                                              table_1
                                              (hash-iterate-next v_1 i_0))))
                                          (args
                                           (raise-binding-result-arity-error
                                            2
                                            args))))
                                        table_0))))))
                               (for-loop_0
                                hash2610
                                (hash-iterate-first v_1))))
                            (if (hash-eqv? v_1)
                              (begin
                                (letrec*
                                 ((for-loop_0
                                   (|#%name|
                                    for-loop
                                    (lambda (table_0 i_0)
                                      (begin
                                        (if i_0
                                          (call-with-values
                                           (lambda ()
                                             (hash-iterate-key+value v_1 i_0))
                                           (case-lambda
                                            ((k_0 v_2)
                                             (let ((table_1
                                                    (let ((table_1
                                                           (call-with-values
                                                            (lambda ()
                                                              (let ((app_0
                                                                     (loop_0
                                                                      k_0)))
                                                                (values
                                                                 app_0
                                                                 (loop_0
                                                                  v_2))))
                                                            (case-lambda
                                                             ((key_0 val_0)
                                                              (hash-set
                                                               table_0
                                                               key_0
                                                               val_0))
                                                             (args
                                                              (raise-binding-result-arity-error
                                                               2
                                                               args))))))
                                                      (values table_1))))
                                               (for-loop_0
                                                table_1
                                                (hash-iterate-next v_1 i_0))))
                                            (args
                                             (raise-binding-result-arity-error
                                              2
                                              args))))
                                          table_0))))))
                                 (for-loop_0
                                  hash2589
                                  (hash-iterate-first v_1))))
                              (begin
                                (letrec*
                                 ((for-loop_0
                                   (|#%name|
                                    for-loop
                                    (lambda (table_0 i_0)
                                      (begin
                                        (if i_0
                                          (call-with-values
                                           (lambda ()
                                             (hash-iterate-key+value v_1 i_0))
                                           (case-lambda
                                            ((k_0 v_2)
                                             (let ((table_1
                                                    (let ((table_1
                                                           (call-with-values
                                                            (lambda ()
                                                              (let ((app_0
                                                                     (loop_0
                                                                      k_0)))
                                                                (values
                                                                 app_0
                                                                 (loop_0
                                                                  v_2))))
                                                            (case-lambda
                                                             ((key_0 val_0)
                                                              (hash-set
                                                               table_0
                                                               key_0
                                                               val_0))
                                                             (args
                                                              (raise-binding-result-arity-error
                                                               2
                                                               args))))))
                                                      (values table_1))))
                                               (for-loop_0
                                                table_1
                                                (hash-iterate-next v_1 i_0))))
                                            (args
                                             (raise-binding-result-arity-error
                                              2
                                              args))))
                                          table_0))))))
                                 (for-loop_0
                                  hash2725
                                  (hash-iterate-first v_1))))))
                          (if (if (cpointer? v_1)
                                (if v_1 (not (bytes? v_1)) #f)
                                #f)
                            (ptr-add v_1 0)
                            (if (message-ized? v_1)
                              (|#%app| (message-ized-unmessage v_1))
                              v_1)))))))))))))
       (loop_0 v_0)))))
(define struct:place
  (make-record-type-descriptor*
   'place
   #f
   (|#%nongenerative-uid| place)
   #f
   #f
   19
   491440))
(define effect_2252
  (struct-type-install-properties!
   struct:place
   '(place)
   19
   0
   #f
   (list
    (cons prop:authentic #t)
    (cons
     prop:place-message
     (lambda (self_0) (lambda () (lambda () (place-pch self_0)))))
    (cons 1/prop:evt 3)
    (cons host:prop:unsafe-authentic-override #t))
   (current-inspector)
   #f
   '(0 1 2 3 6 15)
   #f
   'place))
(define place1.1
  (|#%name|
   place
   (record-constructor
    (make-record-constructor-descriptor struct:place #f #f))))
(define 1/place? (|#%name| place? (record-predicate struct:place)))
(define place-parent (|#%name| place-parent (record-accessor struct:place 0)))
(define place-lock (|#%name| place-lock (record-accessor struct:place 1)))
(define place-activity-canary
  (|#%name| place-activity-canary (record-accessor struct:place 2)))
(define place-pch (|#%name| place-pch (record-accessor struct:place 3)))
(define place-result (|#%name| place-result (record-accessor struct:place 4)))
(define place-queued-result
  (|#%name| place-queued-result (record-accessor struct:place 5)))
(define place-custodian
  (|#%name| place-custodian (record-accessor struct:place 6)))
(define place-custodian-ref
  (|#%name| place-custodian-ref (record-accessor struct:place 7)))
(define place-host-thread
  (|#%name| place-host-thread (record-accessor struct:place 8)))
(define place-id (|#%name| place-id (record-accessor struct:place 9)))
(define place-host-roots
  (|#%name| place-host-roots (record-accessor struct:place 10)))
(define place-current-thread
  (|#%name| place-current-thread (record-accessor struct:place 11)))
(define place-post-shutdown
  (|#%name| place-post-shutdown (record-accessor struct:place 12)))
(define place-pumpers
  (|#%name| place-pumpers (record-accessor struct:place 13)))
(define place-pending-break
  (|#%name| place-pending-break (record-accessor struct:place 14)))
(define place-done-waiting
  (|#%name| place-done-waiting (record-accessor struct:place 15)))
(define place-wakeup-handle
  (|#%name| place-wakeup-handle (record-accessor struct:place 16)))
(define place-dequeue-semas
  (|#%name| place-dequeue-semas (record-accessor struct:place 17)))
(define place-future-scheduler
  (|#%name| place-future-scheduler (record-accessor struct:place 18)))
(define set-place-result!
  (|#%name| set-place-result! (record-mutator struct:place 4)))
(define set-place-queued-result!
  (|#%name| set-place-queued-result! (record-mutator struct:place 5)))
(define set-place-custodian-ref!
  (|#%name| set-place-custodian-ref! (record-mutator struct:place 7)))
(define set-place-host-thread!
  (|#%name| set-place-host-thread! (record-mutator struct:place 8)))
(define set-place-id! (|#%name| set-place-id! (record-mutator struct:place 9)))
(define set-place-host-roots!
  (|#%name| set-place-host-roots! (record-mutator struct:place 10)))
(define set-place-current-thread!
  (|#%name| set-place-current-thread! (record-mutator struct:place 11)))
(define set-place-post-shutdown!
  (|#%name| set-place-post-shutdown! (record-mutator struct:place 12)))
(define set-place-pumpers!
  (|#%name| set-place-pumpers! (record-mutator struct:place 13)))
(define set-place-pending-break!
  (|#%name| set-place-pending-break! (record-mutator struct:place 14)))
(define set-place-wakeup-handle!
  (|#%name| set-place-wakeup-handle! (record-mutator struct:place 16)))
(define set-place-dequeue-semas!
  (|#%name| set-place-dequeue-semas! (record-mutator struct:place 17)))
(define set-place-future-scheduler!
  (|#%name| set-place-future-scheduler! (record-mutator struct:place 18)))
(define make-place.1
  (|#%name|
   make-place
   (lambda (parent2_0 place-channel3_0 lock6_0 cust7_0)
     (begin
       (let ((app_0 (box #f)))
         (place1.1
          parent2_0
          lock6_0
          app_0
          place-channel3_0
          #f
          #f
          cust7_0
          #f
          #f
          0
          #f
          #f
          '()
          #f
          #f
          (make-hasheq)
          #f
          '()
          #f))))))
(define initial-place
  (let ((temp10_0 (|#%app| host:make-mutex)))
    (let ((root-custodian11_0 (unsafe-place-local-ref cell.1$6)))
      (let ((temp10_1 temp10_0))
        (make-place.1 #f #f temp10_1 root-custodian11_0)))))
(define cell.1$2 (unsafe-make-place-local initial-place))
(define effect_2788
  (begin
    (void (set-custodian-place! initial-place-root-custodian initial-place))
    (void)))
(define effect_2183
  (begin
    (void
     (set-place-host-thread! initial-place (|#%app| host:get-initial-place)))
    (void)))
(define pre-poll-callbacks null)
(define unsafe-add-pre-poll-callback!
  (lambda (proc_0) (set! pre-poll-callbacks (cons proc_0 pre-poll-callbacks))))
(define call-pre-poll-external-callbacks
  (lambda ()
    (if (eq? (unsafe-place-local-ref cell.1$2) initial-place)
      (if (null? pre-poll-callbacks)
        (void)
        (begin
          (|#%app| host:disable-interrupts)
          (let ((l_0 pre-poll-callbacks))
            (begin
              (set! pre-poll-callbacks null)
              (|#%app| host:enable-interrupts)
              (let ((lst_0 (reverse$1 l_0)))
                (begin
                  (letrec*
                   ((for-loop_0
                     (|#%name|
                      for-loop
                      (lambda (lst_1)
                        (begin
                          (if (pair? lst_1)
                            (let ((cb_0 (unsafe-car lst_1)))
                              (let ((rest_0 (unsafe-cdr lst_1)))
                                (begin (|#%app| cb_0) (for-loop_0 rest_0))))
                            (values)))))))
                   (for-loop_0 lst_0))))
              (void)))))
      (void))))
(define struct:semaphore
  (make-record-type-descriptor*
   'semaphore
   struct:queue
   (|#%nongenerative-uid| semaphore)
   #f
   #f
   1
   1))
(define effect_2858
  (struct-type-install-properties!
   struct:semaphore
   '(semaphore)
   1
   0
   struct:queue
   (list
    (cons prop:authentic #t)
    (cons
     1/prop:evt
     (poller2.1
      (lambda (s_0 poll-ctx_0)
        (semaphore-wait/poll.1 #f unsafe-undefined s_0 s_0 poll-ctx_0))))
    (cons host:prop:unsafe-authentic-override #t))
   (current-inspector)
   #f
   '()
   #f
   'semaphore))
(define semaphore1.1
  (|#%name|
   semaphore
   (record-constructor
    (make-record-constructor-descriptor struct:semaphore #f #f))))
(define 1/semaphore? (|#%name| semaphore? (record-predicate struct:semaphore)))
(define semaphore-count
  (|#%name| semaphore-count (record-accessor struct:semaphore 0)))
(define set-semaphore-count!
  (|#%name| set-semaphore-count! (record-mutator struct:semaphore 0)))
(define count-field-pos 2)
(define struct:semaphore-peek-evt
  (make-record-type-descriptor*
   'semaphore-peek-evt
   #f
   (|#%nongenerative-uid| semaphore-peek-evt)
   #f
   #f
   1
   0))
(define effect_2145
  (struct-type-install-properties!
   struct:semaphore-peek-evt
   '(semaphore-peek-evt)
   1
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1
      (lambda (sp_0 poll-ctx_0)
        (let ((temp18_0 (semaphore-peek-evt-sema sp_0)))
          (semaphore-wait/poll.1 #t sp_0 temp18_0 sp_0 poll-ctx_0))))))
   (current-inspector)
   #f
   '(0)
   #f
   'semaphore-peek-evt))
(define semaphore-peek-evt2.1
  (|#%name|
   semaphore-peek-evt
   (record-constructor
    (make-record-constructor-descriptor struct:semaphore-peek-evt #f #f))))
(define 1/semaphore-peek-evt?_2396
  (|#%name| semaphore-peek-evt? (record-predicate struct:semaphore-peek-evt)))
(define 1/semaphore-peek-evt?
  (|#%name|
   semaphore-peek-evt?
   (lambda (v)
     (if (1/semaphore-peek-evt?_2396 v)
       #t
       ($value
        (if (impersonator? v)
          (1/semaphore-peek-evt?_2396 (impersonator-val v))
          #f))))))
(define semaphore-peek-evt-sema_2184
  (|#%name|
   semaphore-peek-evt-sema
   (record-accessor struct:semaphore-peek-evt 0)))
(define semaphore-peek-evt-sema
  (|#%name|
   semaphore-peek-evt-sema
   (lambda (s)
     (if (1/semaphore-peek-evt?_2396 s)
       (semaphore-peek-evt-sema_2184 s)
       ($value
        (impersonate-ref
         semaphore-peek-evt-sema_2184
         struct:semaphore-peek-evt
         0
         s
         'semaphore-peek-evt
         'sema))))))
(define struct:semaphore-peek-select-waiter
  (make-record-type-descriptor*
   'semaphore-peek-select-waiter
   struct:select-waiter
   (|#%nongenerative-uid| semaphore-peek-select-waiter)
   #f
   #f
   0
   0))
(define effect_2532
  (struct-type-install-properties!
   struct:semaphore-peek-select-waiter
   '(semaphore-peek-select-waiter)
   0
   0
   struct:select-waiter
   null
   (current-inspector)
   #f
   '()
   #f
   'semaphore-peek-select-waiter))
(define semaphore-peek-select-waiter3.1
  (|#%name|
   semaphore-peek-select-waiter
   (record-constructor
    (make-record-constructor-descriptor
     struct:semaphore-peek-select-waiter
     #f
     #f))))
(define semaphore-peek-select-waiter?_2529
  (|#%name|
   semaphore-peek-select-waiter?
   (record-predicate struct:semaphore-peek-select-waiter)))
(define semaphore-peek-select-waiter?
  (|#%name|
   semaphore-peek-select-waiter?
   (lambda (v)
     (if (semaphore-peek-select-waiter?_2529 v)
       #t
       ($value
        (if (impersonator? v)
          (semaphore-peek-select-waiter?_2529 (impersonator-val v))
          #f))))))
(define 1/make-semaphore
  (let ((make-semaphore_0
         (|#%name|
          make-semaphore
          (lambda (init4_0)
            (begin
              (begin
                (if (exact-nonnegative-integer? init4_0)
                  (void)
                  (raise-argument-error
                   'make-semaphore
                   "exact-nonnegative-integer?"
                   init4_0))
                (if (fixnum? init4_0)
                  (void)
                  (raise
                   (let ((app_0
                          (string-append
                           "make-semaphore: starting value "
                           (number->string init4_0)
                           " is too large")))
                     (|#%app| exn:fail app_0 (current-continuation-marks)))))
                (semaphore1.1 #f #f init4_0)))))))
    (|#%name|
     make-semaphore
     (case-lambda
      (() (begin (make-semaphore_0 0)))
      ((init4_0) (make-semaphore_0 init4_0))))))
(define 1/semaphore-post
  (|#%name|
   semaphore-post
   (lambda (s_0)
     (begin
       (begin
         (if (1/semaphore? s_0)
           (void)
           (raise-argument-error 'semaphore-post "semaphore?" s_0))
         (unsafe-semaphore-post s_0))))))
(define unsafe-semaphore-post
  (lambda (s_0)
    (let ((c_0 (semaphore-count s_0)))
      (if (if (>= c_0 0)
            (if (not (current-future$1))
              (unsafe-struct*-cas! s_0 2 c_0 (add1 c_0))
              #f)
            #f)
        (void)
        (begin (start-atomic) (semaphore-post/atomic s_0) (end-atomic))))))
(define semaphore-post/atomic
  (lambda (s_0)
    (letrec*
     ((loop_0
       (|#%name|
        loop
        (lambda ()
          (begin
            (let ((w_0 (queue-remove! s_0)))
              (if (not w_0)
                (set-semaphore-count! s_0 (add1 (semaphore-count s_0)))
                (begin
                  (begin-unsafe
                   (|#%app| (waiter-methods-resume (waiter-ref w_0)) w_0 s_0))
                  (if (begin-unsafe (not (queue-start s_0)))
                    (set-semaphore-count! s_0 0)
                    (void))
                  (if (semaphore-peek-select-waiter? w_0)
                    (loop_0)
                    (void))))))))))
     (loop_0))))
(define semaphore-post-all/atomic
  (lambda (s_0)
    (begin
      (set-semaphore-count! s_0 +inf.0)
      (queue-remove-all!
       s_0
       (lambda (w_0)
         (begin-unsafe
          (|#%app| (waiter-methods-resume (waiter-ref w_0)) w_0 s_0)))))))
(define semaphore-post-all
  (lambda (s_0)
    (begin (start-atomic) (semaphore-post-all/atomic s_0) (end-atomic))))
(define semaphore-any-waiters?
  (lambda (s_0) (not (begin-unsafe (not (queue-start s_0))))))
(define 1/semaphore-try-wait?
  (|#%name|
   semaphore-try-wait?
   (lambda (s_0)
     (begin
       (begin
         (if (1/semaphore? s_0)
           (void)
           (raise-argument-error 'semaphore-try-wait? "semaphore?" s_0))
         (start-atomic)
         (begin0
           (begin
             (call-pre-poll-external-callbacks)
             (let ((c_0 (semaphore-count s_0)))
               (if (positive? c_0)
                 (begin (set-semaphore-count! s_0 (sub1 c_0)) #t)
                 #f)))
           (end-atomic)))))))
(define 1/semaphore-wait
  (|#%name|
   semaphore-wait
   (lambda (s_0)
     (begin
       (begin
         (if (1/semaphore? s_0)
           (void)
           (raise-argument-error 'semaphore-wait "semaphore?" s_0))
         (unsafe-semaphore-wait s_0))))))
(define unsafe-semaphore-wait
  (lambda (s_0)
    (let ((c_0 (semaphore-count s_0)))
      (if (if (positive? c_0)
            (if (not (current-future$1))
              (unsafe-struct*-cas! s_0 2 c_0 (sub1 c_0))
              #f)
            #f)
        (void)
        (|#%app|
         (begin
           (start-atomic)
           (begin0
             (let ((c_1 (semaphore-count s_0)))
               (if (positive? c_1)
                 (begin (set-semaphore-count! s_0 (sub1 c_1)) void)
                 (let ((w_0 (current-thread/in-atomic)))
                   (let ((n_0 (queue-add! s_0 w_0)))
                     (begin
                       (set-semaphore-count! s_0 -1)
                       (let ((interrupt-cb_0
                              (lambda ()
                                (begin
                                  (queue-remove-node! s_0 n_0)
                                  (if (begin-unsafe (not (queue-start s_0)))
                                    (set-semaphore-count! s_0 0)
                                    (void))
                                  (lambda () (unsafe-semaphore-wait s_0))))))
                         (begin-unsafe
                          (|#%app|
                           (waiter-methods-suspend (waiter-ref w_0))
                           w_0
                           interrupt-cb_0))))))))
             (end-atomic))))))))
(define semaphore-wait/poll.1
  (|#%name|
   semaphore-wait/poll
   (lambda (peek?5_0 result6_0 s9_0 self10_0 poll-ctx11_0)
     (begin
       (let ((result_0 (if (eq? result6_0 unsafe-undefined) s9_0 result6_0)))
         (let ((c_0 (semaphore-count s9_0)))
           (if (positive? c_0)
             (begin
               (if peek?5_0 (void) (set-semaphore-count! s9_0 (sub1 c_0)))
               (values (list result_0) #f))
             (if (poll-ctx-poll? poll-ctx11_0)
               (values #f self10_0)
               (let ((w_0
                      (if peek?5_0
                        (semaphore-peek-select-waiter3.1
                         (poll-ctx-select-proc poll-ctx11_0))
                        (select-waiter7.1
                         (poll-ctx-select-proc poll-ctx11_0)))))
                 (let ((n_0 (queue-add! s9_0 w_0)))
                   (begin
                     (set-semaphore-count! s9_0 -1)
                     (values
                      #f
                      (control-state-evt9.1
                       the-async-evt
                       (lambda (v_0) result_0)
                       (lambda ()
                         (begin
                           (queue-remove-node! s9_0 n_0)
                           (if (begin-unsafe (not (queue-start s9_0)))
                             (set-semaphore-count! s9_0 0)
                             (void))))
                       void
                       (lambda ()
                         (let ((c_1 (semaphore-count s9_0)))
                           (if (positive? c_1)
                             (begin
                               (if peek?5_0
                                 (void)
                                 (set-semaphore-count! s9_0 (sub1 c_1)))
                               (values result_0 #t))
                             (begin
                               (set! n_0 (queue-add! s9_0 w_0))
                               (set-semaphore-count! s9_0 -1)
                               (values #f #f))))))))))))))))))
(define semaphore-wait/atomic
  (lambda (s_0)
    (let ((c_0 (semaphore-count s_0)))
      (if (positive? c_0)
        (set-semaphore-count! s_0 (sub1 c_0))
        (internal-error
         "semaphore-wait/atomic: cannot decrement semaphore")))))
(define struct:node
  (make-record-type-descriptor*
   'node
   #f
   (|#%nongenerative-uid| node)
   #f
   #f
   2
   3))
(define effect_2309
  (struct-type-install-properties!
   struct:node
   '(node)
   2
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '()
   #f
   'node))
(define node1.1
  (|#%name|
   node
   (record-constructor
    (make-record-constructor-descriptor struct:node #f #f))))
(define node? (|#%name| node? (record-predicate struct:node)))
(define node-prev (|#%name| node-prev (record-accessor struct:node 0)))
(define node-next (|#%name| node-next (record-accessor struct:node 1)))
(define set-node-prev!
  (|#%name| set-node-prev! (record-mutator struct:node 0)))
(define set-node-next!
  (|#%name| set-node-next! (record-mutator struct:node 1)))
(define child-node (lambda (child_0) child_0))
(define node-child (lambda (n_0) n_0))
(define struct:thread-group
  (make-record-type-descriptor*
   'thread-group
   struct:node
   (|#%nongenerative-uid| thread-group)
   #f
   #f
   4
   14))
(define effect_2274
  (struct-type-install-properties!
   struct:thread-group
   '(thread-group)
   4
   0
   struct:node
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(0)
   #f
   'thread-group))
(define thread-group2.1
  (|#%name|
   thread-group
   (record-constructor
    (make-record-constructor-descriptor struct:thread-group #f #f))))
(define 1/thread-group?
  (|#%name| thread-group? (record-predicate struct:thread-group)))
(define thread-group-parent
  (|#%name| thread-group-parent (record-accessor struct:thread-group 0)))
(define thread-group-chain-start
  (|#%name| thread-group-chain-start (record-accessor struct:thread-group 1)))
(define thread-group-chain
  (|#%name| thread-group-chain (record-accessor struct:thread-group 2)))
(define thread-group-chain-end
  (|#%name| thread-group-chain-end (record-accessor struct:thread-group 3)))
(define set-thread-group-chain-start!
  (|#%name|
   set-thread-group-chain-start!
   (record-mutator struct:thread-group 1)))
(define set-thread-group-chain!
  (|#%name| set-thread-group-chain! (record-mutator struct:thread-group 2)))
(define set-thread-group-chain-end!
  (|#%name|
   set-thread-group-chain-end!
   (record-mutator struct:thread-group 3)))
(define not-added-key #f)
(define assert-not-added (lambda (n_0) (void)))
(define assert-added (lambda (n_0) (void)))
(define make-root-thread-group (lambda () (thread-group2.1 #f #f #f #f #f #f)))
(define cell.1 (unsafe-make-place-local (make-root-thread-group)))
(define cell.2 (unsafe-make-place-local 0))
(define 1/current-thread-group
  (make-parameter
   (unsafe-place-local-ref cell.1)
   (lambda (v_0)
     (begin
       (if (1/thread-group? v_0)
         (void)
         (raise-argument-error 'current-thread-group "thread-group?" v_0))
       v_0))
   'current-thread-group))
(define make-another-initial-thread-group
  (lambda () (unsafe-place-local-set! cell.1 (make-root-thread-group))))
(define 1/make-thread-group
  (let ((make-thread-group_0
         (|#%name|
          make-thread-group
          (lambda (parent3_0)
            (begin
              (let ((parent_0
                     (if (eq? parent3_0 unsafe-undefined)
                       (1/current-thread-group)
                       parent3_0)))
                (begin
                  (if (1/thread-group? parent_0)
                    (void)
                    (raise-argument-error
                     'make-thread-group
                     "thread-group?"
                     parent_0))
                  (let ((tg_0 (thread-group2.1 #f #f parent_0 #f #f #f)))
                    tg_0))))))))
    (|#%name|
     make-thread-group
     (case-lambda
      (() (begin (make-thread-group_0 unsafe-undefined)))
      ((parent3_0) (make-thread-group_0 parent3_0))))))
(define thread-group-next!
  (lambda (tg_0)
    (let ((n_0 (thread-group-chain tg_0)))
      (if (not n_0)
        (let ((n_1 (thread-group-chain-start tg_0)))
          (if (not n_1)
            #f
            (begin (set-thread-group-chain! tg_0 (node-next n_1)) n_1)))
        (begin
          (set-thread-group-chain! tg_0 (node-next n_0))
          (begin-unsafe n_0))))))
(define thread-group-add!
  (lambda (parent_0 child_0)
    (let ((t_0 (thread-group-chain-start parent_0)))
      (let ((was-empty?_0 (not t_0)))
        (let ((n_0 (begin-unsafe child_0)))
          (begin
            (let ((n_1 n_0)) (begin-unsafe (void)))
            (set-node-next! n_0 t_0)
            (set-node-prev! n_0 #f)
            (if t_0
              (set-node-prev! t_0 n_0)
              (set-thread-group-chain-end! parent_0 n_0))
            (set-thread-group-chain-start! parent_0 n_0)
            (if (1/thread-group? child_0)
              (void)
              (unsafe-place-local-set!
               cell.2
               (add1 (unsafe-place-local-ref cell.2))))
            (if was-empty?_0
              (let ((parent-parent_0 (thread-group-parent parent_0)))
                (if parent-parent_0
                  (thread-group-add! parent-parent_0 parent_0)
                  (void)))
              (void))))))))
(define thread-group-remove!
  (lambda (parent_0 child_0)
    (let ((n_0 (begin-unsafe child_0)))
      (begin
        (begin-unsafe (void))
        (if (node-next n_0)
          (let ((app_0 (node-next n_0)))
            (set-node-prev! app_0 (node-prev n_0)))
          (set-thread-group-chain-end! parent_0 (node-prev n_0)))
        (if (node-prev n_0)
          (let ((app_0 (node-prev n_0)))
            (set-node-next! app_0 (node-next n_0)))
          (set-thread-group-chain-start! parent_0 (node-next n_0)))
        (if (eq? n_0 (thread-group-chain parent_0))
          (set-thread-group-chain! parent_0 (node-next n_0))
          (void))
        (set-node-next! n_0 #f)
        (set-node-prev! n_0 #f)
        (if (1/thread-group? child_0)
          (void)
          (unsafe-place-local-set!
           cell.2
           (sub1 (unsafe-place-local-ref cell.2))))
        (if (not (thread-group-chain-end parent_0))
          (let ((parent-parent_0 (thread-group-parent parent_0)))
            (if parent-parent_0
              (thread-group-remove! parent-parent_0 parent_0)
              (void)))
          (void))))))
(define thread-group-all-threads
  (lambda (parent_0 accum_0)
    (if (not (1/thread-group? parent_0))
      (cons parent_0 accum_0)
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda (n_0 accum_1)
            (begin
              (if (not n_0)
                accum_1
                (let ((app_0 (node-next n_0)))
                  (loop_0
                   app_0
                   (thread-group-all-threads
                    (begin-unsafe n_0)
                    accum_1)))))))))
       (loop_0 (thread-group-chain-start parent_0) accum_0)))))
(define struct:schedule-info
  (make-record-type-descriptor*
   'schedule-info
   #f
   (|#%nongenerative-uid| schedule-info)
   #f
   #f
   2
   3))
(define effect_2483
  (struct-type-install-properties!
   struct:schedule-info
   '(schedule-info)
   2
   0
   #f
   null
   (current-inspector)
   #f
   '()
   #f
   'schedule-info))
(define schedule-info1.1
  (|#%name|
   schedule-info
   (record-constructor
    (make-record-constructor-descriptor struct:schedule-info #f #f))))
(define schedule-info?_2650
  (|#%name| schedule-info? (record-predicate struct:schedule-info)))
(define schedule-info?
  (|#%name|
   schedule-info?
   (lambda (v)
     (if (schedule-info?_2650 v)
       #t
       ($value
        (if (impersonator? v)
          (schedule-info?_2650 (impersonator-val v))
          #f))))))
(define schedule-info-did-work?_2393
  (|#%name| schedule-info-did-work? (record-accessor struct:schedule-info 0)))
(define schedule-info-did-work?
  (|#%name|
   schedule-info-did-work?
   (lambda (s)
     (if (schedule-info?_2650 s)
       (schedule-info-did-work?_2393 s)
       ($value
        (impersonate-ref
         schedule-info-did-work?_2393
         struct:schedule-info
         0
         s
         'schedule-info
         'did-work?))))))
(define schedule-info-exts_2787
  (|#%name| schedule-info-exts (record-accessor struct:schedule-info 1)))
(define schedule-info-exts
  (|#%name|
   schedule-info-exts
   (lambda (s)
     (if (schedule-info?_2650 s)
       (schedule-info-exts_2787 s)
       ($value
        (impersonate-ref
         schedule-info-exts_2787
         struct:schedule-info
         1
         s
         'schedule-info
         'exts))))))
(define set-schedule-info-did-work?!_2489
  (|#%name|
   set-schedule-info-did-work?!
   (record-mutator struct:schedule-info 0)))
(define set-schedule-info-did-work?!
  (|#%name|
   set-schedule-info-did-work?!
   (lambda (s v)
     (if (schedule-info?_2650 s)
       (set-schedule-info-did-work?!_2489 s v)
       ($value
        (impersonate-set!
         set-schedule-info-did-work?!_2489
         struct:schedule-info
         0
         0
         s
         v
         'schedule-info
         'did-work?))))))
(define set-schedule-info-exts!_2592
  (|#%name| set-schedule-info-exts! (record-mutator struct:schedule-info 1)))
(define set-schedule-info-exts!
  (|#%name|
   set-schedule-info-exts!
   (lambda (s v)
     (if (schedule-info?_2650 s)
       (set-schedule-info-exts!_2592 s v)
       ($value
        (impersonate-set!
         set-schedule-info-exts!_2592
         struct:schedule-info
         1
         1
         s
         v
         'schedule-info
         'exts))))))
(define make-schedule-info.1
  (|#%name|
   make-schedule-info
   (lambda (did-work?2_0) (begin (schedule-info1.1 did-work?2_0 #f)))))
(define schedule-info-current-exts
  (case-lambda
   ((sched-info_0) (schedule-info-exts sched-info_0))
   ((sched-info_0 exts_0) (set-schedule-info-exts! sched-info_0 exts_0))))
(define schedule-info-add-timeout-at!
  (lambda (sched-info_0 timeout-at_0)
    (let ((exts_0 (schedule-info-exts sched-info_0)))
      (set-schedule-info-exts!
       sched-info_0
       (begin-unsafe
        (|#%app|
         (sandman-do-merge-timeout the-sandman)
         exts_0
         timeout-at_0))))))
(define schedule-info-did-work!
  (lambda (sched-info_0) (set-schedule-info-did-work?! sched-info_0 #t)))
(define reference-sink
  (lambda (v_0) (ephemeron-value (make-ephemeron #f (void)) (void) v_0)))
(define struct:plumber
  (make-record-type-descriptor*
   'plumber
   #f
   (|#%nongenerative-uid| plumber)
   #f
   #f
   2
   0))
(define effect_2626
  (struct-type-install-properties!
   struct:plumber
   '(plumber)
   2
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(0 1)
   #f
   'plumber))
(define plumber1.1
  (|#%name|
   plumber
   (record-constructor
    (make-record-constructor-descriptor struct:plumber #f #f))))
(define 1/plumber? (|#%name| plumber? (record-predicate struct:plumber)))
(define plumber-callbacks
  (|#%name| plumber-callbacks (record-accessor struct:plumber 0)))
(define plumber-weak-callbacks
  (|#%name| plumber-weak-callbacks (record-accessor struct:plumber 1)))
(define 1/make-plumber
  (|#%name|
   make-plumber
   (lambda ()
     (begin
       (let ((app_0 (make-hasheq))) (plumber1.1 app_0 (make-weak-hasheq)))))))
(define 1/current-plumber
  (make-parameter
   (1/make-plumber)
   (lambda (v_0)
     (begin
       (if (1/plumber? v_0)
         (void)
         (raise-argument-error 'current-plumber "plumber?" v_0))
       v_0))
   'current-plumber))
(define struct:plumber-flush-handle
  (make-record-type-descriptor*
   'plumber-flush-handle
   #f
   (|#%nongenerative-uid| plumber-flush-handle)
   #f
   #f
   2
   0))
(define effect_2487
  (struct-type-install-properties!
   struct:plumber-flush-handle
   '(plumber-flush-handle)
   2
   0
   #f
   null
   (current-inspector)
   #f
   '(0 1)
   #f
   'plumber-flush-handle))
(define plumber-flush-handle2.1
  (|#%name|
   plumber-flush-handle
   (record-constructor
    (make-record-constructor-descriptor struct:plumber-flush-handle #f #f))))
(define 1/plumber-flush-handle?_2816
  (|#%name|
   plumber-flush-handle?
   (record-predicate struct:plumber-flush-handle)))
(define 1/plumber-flush-handle?
  (|#%name|
   plumber-flush-handle?
   (lambda (v)
     (if (1/plumber-flush-handle?_2816 v)
       #t
       ($value
        (if (impersonator? v)
          (1/plumber-flush-handle?_2816 (impersonator-val v))
          #f))))))
(define plumber-flush-handle-plumber_2929
  (|#%name|
   plumber-flush-handle-plumber
   (record-accessor struct:plumber-flush-handle 0)))
(define plumber-flush-handle-plumber
  (|#%name|
   plumber-flush-handle-plumber
   (lambda (s)
     (if (1/plumber-flush-handle?_2816 s)
       (plumber-flush-handle-plumber_2929 s)
       ($value
        (impersonate-ref
         plumber-flush-handle-plumber_2929
         struct:plumber-flush-handle
         0
         s
         'plumber-flush-handle
         'plumber))))))
(define plumber-flush-handle-proc_2551
  (|#%name|
   plumber-flush-handle-proc
   (record-accessor struct:plumber-flush-handle 1)))
(define plumber-flush-handle-proc
  (|#%name|
   plumber-flush-handle-proc
   (lambda (s)
     (if (1/plumber-flush-handle?_2816 s)
       (plumber-flush-handle-proc_2551 s)
       ($value
        (impersonate-ref
         plumber-flush-handle-proc_2551
         struct:plumber-flush-handle
         1
         s
         'plumber-flush-handle
         'proc))))))
(define 1/plumber-add-flush!
  (let ((plumber-add-flush!_0
         (|#%name|
          plumber-add-flush!
          (lambda (p4_0 proc5_0 weak?3_0)
            (begin
              (begin
                (if (1/plumber? p4_0)
                  (void)
                  (raise-argument-error 'plumber-add-flush! "plumber?" p4_0))
                (begin
                  (if (if (procedure? proc5_0)
                        (procedure-arity-includes? proc5_0 1)
                        #f)
                    (void)
                    (raise-argument-error
                     'plumber-add-flush!
                     "(procedure-arity-includes/c 1)"
                     proc5_0))
                  (let ((h_0 (plumber-flush-handle2.1 p4_0 proc5_0)))
                    (begin
                      (hash-set!
                       (if weak?3_0
                         (plumber-weak-callbacks p4_0)
                         (plumber-callbacks p4_0))
                       h_0
                       #t)
                      h_0)))))))))
    (|#%name|
     plumber-add-flush!
     (case-lambda
      ((p_0 proc_0) (begin (plumber-add-flush!_0 p_0 proc_0 #f)))
      ((p_0 proc_0 weak?3_0) (plumber-add-flush!_0 p_0 proc_0 weak?3_0))))))
(define 1/plumber-flush-all
  (|#%name|
   plumber-flush-all
   (lambda (p_0)
     (begin
       (begin
         (if (1/plumber? p_0)
           (void)
           (raise-argument-error 'plumber-flush-all "plumber?" p_0))
         (plumber-flush-all/wrap
          p_0
          (lambda (proc_0 h_0) (|#%app| proc_0 h_0))))))))
(define plumber-flush-all/wrap
  (lambda (p_0 app_0)
    (let ((hs_0
           (reverse$1
            (let ((lst_0
                   (list
                    (plumber-callbacks p_0)
                    (plumber-weak-callbacks p_0))))
              (begin
                (letrec*
                 ((for-loop_0
                   (|#%name|
                    for-loop
                    (lambda (fold-var_0 lst_1)
                      (begin
                        (if (pair? lst_1)
                          (let ((cbs_0 (unsafe-car lst_1)))
                            (let ((rest_0 (unsafe-cdr lst_1)))
                              (let ((fold-var_1
                                     (begin
                                       (letrec*
                                        ((for-loop_1
                                          (|#%name|
                                           for-loop
                                           (lambda (fold-var_1 i_0)
                                             (begin
                                               (if i_0
                                                 (let ((h_0
                                                        (hash-iterate-key
                                                         cbs_0
                                                         i_0)))
                                                   (let ((fold-var_2
                                                          (cons
                                                           h_0
                                                           fold-var_1)))
                                                     (let ((fold-var_3
                                                            (values
                                                             fold-var_2)))
                                                       (for-loop_1
                                                        fold-var_3
                                                        (hash-iterate-next
                                                         cbs_0
                                                         i_0)))))
                                                 fold-var_1))))))
                                        (for-loop_1
                                         fold-var_0
                                         (hash-iterate-first cbs_0))))))
                                (for-loop_0 fold-var_1 rest_0))))
                          fold-var_0))))))
                 (for-loop_0 null lst_0)))))))
      (begin
        (begin
          (letrec*
           ((for-loop_0
             (|#%name|
              for-loop
              (lambda (lst_0)
                (begin
                  (if (pair? lst_0)
                    (let ((h_0 (unsafe-car lst_0)))
                      (let ((rest_0 (unsafe-cdr lst_0)))
                        (begin
                          (|#%app| app_0 (plumber-flush-handle-proc h_0) h_0)
                          (for-loop_0 rest_0))))
                    (values)))))))
           (for-loop_0 hs_0)))
        (void)))))
(define 1/plumber-flush-handle-remove!
  (|#%name|
   plumber-flush-handle-remove!
   (lambda (h_0)
     (begin
       (begin
         (if (1/plumber-flush-handle? h_0)
           (void)
           (raise-argument-error
            'plumber-flush-handle-remove!
            "plumber-flush-handle?"
            h_0))
         (let ((p_0 (plumber-flush-handle-plumber h_0)))
           (begin
             (hash-remove! (plumber-callbacks p_0) h_0)
             (hash-remove! (plumber-weak-callbacks p_0) h_0))))))))
(define 1/exit-handler
  (make-parameter
   (let ((root-plumber_0 (1/current-plumber)))
     (lambda (v_0)
       (begin (1/plumber-flush-all root-plumber_0) (|#%app| force-exit v_0))))
   (lambda (p_0)
     (begin
       (if (if (procedure? p_0) (procedure-arity-includes? p_0 1) #f)
         (void)
         (raise-argument-error
          'exit-handler
          "(procedure-arity-includes/c 1)"
          p_0))
       p_0))
   'exit-handler))
(define force-exit
  (lambda (v_0)
    (if (byte? v_0) (|#%app| host:exit v_0) (|#%app| host:exit 0))))
(define 1/exit
  (let ((exit_0
         (|#%name|
          exit
          (lambda (v1_0)
            (begin (begin (|#%app| (1/exit-handler) v1_0) (void)))))))
    (|#%name|
     exit
     (case-lambda (() (begin (exit_0 #t))) ((v1_0) (exit_0 v1_0))))))
(define struct:custodian-box
  (make-record-type-descriptor*
   'custodian-box
   #f
   (|#%nongenerative-uid| custodian-box)
   #f
   #f
   2
   1))
(define effect_2348
  (struct-type-install-properties!
   struct:custodian-box
   '(custodian-box)
   2
   0
   #f
   (list
    (cons prop:authentic #t)
    (cons
     1/prop:evt
     (lambda (cb_0)
       (wrap-evt7.1 (custodian-box-sema cb_0) (lambda (v_0) cb_0)))))
   (current-inspector)
   #f
   '(1)
   #f
   'custodian-box))
(define custodian-box1.1
  (|#%name|
   custodian-box
   (record-constructor
    (make-record-constructor-descriptor struct:custodian-box #f #f))))
(define 1/custodian-box?
  (|#%name| custodian-box? (record-predicate struct:custodian-box)))
(define custodian-box-v
  (|#%name| custodian-box-v (record-accessor struct:custodian-box 0)))
(define custodian-box-sema
  (|#%name| custodian-box-sema (record-accessor struct:custodian-box 1)))
(define set-custodian-box-v!
  (|#%name| set-custodian-box-v! (record-mutator struct:custodian-box 0)))
(define struct:willed-callback
  (make-record-type-descriptor*
   'willed-callback
   #f
   (|#%nongenerative-uid| willed-callback)
   #f
   #f
   2
   0))
(define effect_2870
  (struct-type-install-properties!
   struct:willed-callback
   '(willed-callback)
   2
   0
   #f
   (list (cons prop:authentic #t) (cons new-prop:procedure 0))
   (current-inspector)
   #f
   '(0 1)
   #f
   'willed-callback))
(define willed-callback2.1
  (|#%name|
   willed-callback
   (record-constructor
    (make-record-constructor-descriptor struct:willed-callback #f #f))))
(define willed-callback?
  (|#%name| willed-callback? (record-predicate struct:willed-callback)))
(define willed-callback-proc
  (|#%name| willed-callback-proc (record-accessor struct:willed-callback 0)))
(define willed-callback-will
  (|#%name| willed-callback-will (record-accessor struct:willed-callback 1)))
(define struct:at-exit-callback
  (make-record-type-descriptor*
   'at-exit-callback
   struct:willed-callback
   (|#%nongenerative-uid| at-exit-callback)
   #f
   #f
   0
   0))
(define effect_2332
  (struct-type-install-properties!
   struct:at-exit-callback
   '(at-exit-callback)
   0
   0
   struct:willed-callback
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '()
   #f
   'at-exit-callback))
(define at-exit-callback3.1
  (|#%name|
   at-exit-callback
   (record-constructor
    (make-record-constructor-descriptor struct:at-exit-callback #f #f))))
(define at-exit-callback?
  (|#%name| at-exit-callback? (record-predicate struct:at-exit-callback)))
(define struct:custodian-reference
  (make-record-type-descriptor*
   'custodian-reference
   #f
   (|#%nongenerative-uid| custodian-reference)
   #f
   #f
   1
   1))
(define effect_2409
  (struct-type-install-properties!
   struct:custodian-reference
   '(custodian-reference)
   1
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '()
   #f
   'custodian-reference))
(define custodian-reference4.1
  (|#%name|
   custodian-reference
   (record-constructor
    (make-record-constructor-descriptor struct:custodian-reference #f #f))))
(define custodian-reference?
  (|#%name|
   custodian-reference?
   (record-predicate struct:custodian-reference)))
(define custodian-reference-weak-c
  (|#%name|
   custodian-reference-weak-c
   (record-accessor struct:custodian-reference 0)))
(define set-custodian-reference-weak-c!
  (|#%name|
   set-custodian-reference-weak-c!
   (record-mutator struct:custodian-reference 0)))
(define cell.1$7
  (unsafe-make-place-local (|#%app| host:make-late-will-executor void #f)))
(define 1/current-custodian
  (make-parameter
   (unsafe-place-local-ref cell.1$6)
   (lambda (v_0)
     (begin
       (if (1/custodian? v_0)
         (void)
         (raise-argument-error 'current-custodian "custodian?" v_0))
       v_0))
   'current-custodian))
(define set-root-custodian!
  (lambda (c_0)
    (begin
      (unsafe-place-local-set! cell.1$6 c_0)
      (1/current-custodian c_0)
      (unsafe-place-local-set!
       cell.1$7
       (|#%app| host:make-late-will-executor void #f)))))
(define 1/make-custodian
  (let ((make-custodian_0
         (|#%name|
          make-custodian
          (lambda (parent5_0)
            (begin
              (let ((parent_0
                     (if (eq? parent5_0 unsafe-undefined)
                       (1/current-custodian)
                       parent5_0)))
                (begin
                  (if (1/custodian? parent_0)
                    (void)
                    (raise-argument-error
                     'make-custodian
                     "custodian?"
                     parent_0))
                  (let ((c_0 (create-custodian parent_0)))
                    (begin
                      (set-custodian-place! c_0 (custodian-place parent_0))
                      (let ((children_0 (custodian-children c_0)))
                        (let ((cref_0
                               (let ((temp39_0
                                      (|#%name|
                                       temp39
                                       (lambda (c_1)
                                         (begin
                                           (begin
                                             (reference-sink children_0)
                                             (do-custodian-shutdown-all
                                              c_1)))))))
                                 (do-custodian-register.1
                                  #t
                                  #t
                                  #f
                                  #t
                                  parent_0
                                  c_0
                                  temp39_0))))
                          (begin
                            (set-custodian-parent-reference! c_0 cref_0)
                            (if cref_0
                              (void)
                              (begin-unsafe
                               (raise-arguments-error
                                'make-custodian
                                "the custodian has been shut down"
                                "custodian"
                                parent_0)))
                            (|#%app|
                             host:will-register
                             (unsafe-place-local-ref cell.1$7)
                             c_0
                             merge-custodian-into-parent)
                            c_0))))))))))))
    (|#%name|
     make-custodian
     (case-lambda
      (() (begin (make-custodian_0 unsafe-undefined)))
      ((parent5_0) (make-custodian_0 parent5_0))))))
(define 1/unsafe-make-custodian-at-root
  (|#%name|
   unsafe-make-custodian-at-root
   (lambda () (begin (1/make-custodian (unsafe-place-local-ref cell.1$6))))))
(define do-custodian-register.1
  (|#%name|
   do-custodian-register
   (lambda (at-exit?6_0
            gc-root?9_0
            late?8_0
            weak?7_0
            cust14_0
            obj15_0
            callback16_0)
     (begin
       (begin
         (start-atomic)
         (begin0
           (if (1/custodian-shut-down? cust14_0)
             #f
             (let ((we_0
                    (if (not weak?7_0)
                      (if late?8_0
                        (|#%app| host:make-late-will-executor void)
                        (|#%app| host:make-will-executor void))
                      #f)))
               (begin
                 (hash-set!
                  (custodian-children cust14_0)
                  obj15_0
                  (if weak?7_0
                    callback16_0
                    (if at-exit?6_0
                      (at-exit-callback3.1 callback16_0 we_0)
                      (willed-callback2.1 callback16_0 we_0))))
                 (if we_0
                   (|#%app| host:will-register we_0 obj15_0 void)
                   (void))
                 (if gc-root?9_0
                   (begin
                     (|#%app| host:disable-interrupts)
                     (if (custodian-gc-roots cust14_0)
                       (void)
                       (set-custodian-gc-roots! cust14_0 (make-weak-hasheq)))
                     (hash-set! (custodian-gc-roots cust14_0) obj15_0 #t)
                     (check-limit-custodian cust14_0)
                     (|#%app| host:enable-interrupts))
                   (void))
                 (let ((or-part_0 (custodian-self-reference cust14_0)))
                   (if or-part_0
                     or-part_0
                     (let ((cref_0
                            (custodian-reference4.1 (make-weak-box cust14_0))))
                       (begin
                         (set-custodian-self-reference! cust14_0 cref_0)
                         cref_0)))))))
           (end-atomic)))))))
(define 1/unsafe-custodian-register
  (let ((unsafe-custodian-register_0
         (|#%name|
          unsafe-custodian-register
          (lambda (cust19_0
                   obj20_0
                   callback21_0
                   at-exit?22_0
                   weak?23_0
                   late?18_0)
            (begin
              (do-custodian-register.1
               at-exit?22_0
               #f
               late?18_0
               weak?23_0
               cust19_0
               obj20_0
               callback21_0))))))
    (|#%name|
     unsafe-custodian-register
     (case-lambda
      ((cust_0 obj_0 callback_0 at-exit?_0 weak?_0)
       (begin
         (unsafe-custodian-register_0
          cust_0
          obj_0
          callback_0
          at-exit?_0
          weak?_0
          #f)))
      ((cust_0 obj_0 callback_0 at-exit?_0 weak?_0 late?18_0)
       (unsafe-custodian-register_0
        cust_0
        obj_0
        callback_0
        at-exit?_0
        weak?_0
        late?18_0))))))
(define custodian-register-thread
  (lambda (cust_0 obj_0 callback_0)
    (do-custodian-register.1 #f #t #f #t cust_0 obj_0 callback_0)))
(define custodian-register-place
  (lambda (cust_0 obj_0 callback_0)
    (do-custodian-register.1 #f #t #f #t cust_0 obj_0 callback_0)))
(define 1/unsafe-custodian-unregister
  (|#%name|
   unsafe-custodian-unregister
   (lambda (obj_0 cref_0)
     (begin
       (if cref_0
         (begin
           (start-atomic)
           (begin0
             (let ((c_0 (custodian-reference->custodian cref_0)))
               (if c_0
                 (begin
                   (if (1/custodian-shut-down? c_0)
                     (void)
                     (hash-remove! (custodian-children c_0) obj_0))
                   (begin
                     (|#%app| host:disable-interrupts)
                     (let ((gc-roots_0 (custodian-gc-roots c_0)))
                       (begin
                         (if gc-roots_0
                           (begin
                             (hash-remove! gc-roots_0 obj_0)
                             (check-limit-custodian c_0))
                           (void))
                         (|#%app| host:enable-interrupts)))))
                 (void)))
             (end-atomic))
           (void))
         (void))))))
(define merge-custodian-into-parent
  (lambda (c_0)
    (if (1/custodian-shut-down? c_0)
      (void)
      (let ((p-cref_0 (custodian-parent-reference c_0)))
        (let ((parent_0 (custodian-reference->custodian p-cref_0)))
          (let ((gc-roots_0 (custodian-gc-roots c_0)))
            (begin
              (1/unsafe-custodian-unregister c_0 p-cref_0)
              (begin
                (let ((ht_0 (custodian-children c_0)))
                  (begin
                    (letrec*
                     ((for-loop_0
                       (|#%name|
                        for-loop
                        (lambda (i_0)
                          (begin
                            (if i_0
                              (call-with-values
                               (lambda () (hash-iterate-key+value ht_0 i_0 #f))
                               (case-lambda
                                ((child_0 callback_0)
                                 (begin
                                   (if child_0
                                     (let ((gc-root?_0
                                            (if gc-roots_0
                                              (if (hash-ref
                                                   gc-roots_0
                                                   child_0
                                                   #f)
                                                #t
                                                #f)
                                              #f)))
                                       (if (willed-callback? callback_0)
                                         (let ((temp61_0
                                                (willed-callback-proc
                                                 callback_0)))
                                           (let ((temp62_0
                                                  (at-exit-callback?
                                                   callback_0)))
                                             (do-custodian-register.1
                                              temp62_0
                                              gc-root?_0
                                              #f
                                              #f
                                              parent_0
                                              child_0
                                              temp61_0)))
                                         (do-custodian-register.1
                                          #f
                                          gc-root?_0
                                          #f
                                          #f
                                          parent_0
                                          child_0
                                          callback_0)))
                                     (void))
                                   (for-loop_0 (hash-iterate-next ht_0 i_0))))
                                (args
                                 (raise-binding-result-arity-error 2 args))))
                              (values)))))))
                     (for-loop_0 (hash-iterate-first ht_0)))))
                (let ((self-ref_0 (custodian-self-reference c_0)))
                  (begin
                    (if self-ref_0
                      (set-custodian-reference-weak-c!
                       self-ref_0
                       (custodian-self-reference parent_0))
                      (void))
                    (hash-clear! (custodian-children c_0))
                    (set-custodian-post-shutdown!
                     parent_0
                     (let ((app_0 (custodian-post-shutdown c_0)))
                       (append app_0 (custodian-post-shutdown parent_0))))
                    (set-custodian-post-shutdown! c_0 null)
                    (if gc-roots_0 (hash-clear! gc-roots_0) (void))
                    (check-limit-custodian parent_0)))))))))))
(define poll-custodian-will-executor
  (lambda ()
    (let ((c1_0
           (|#%app| host:will-try-execute (unsafe-place-local-ref cell.1$7))))
      (if c1_0
        (begin
          (let ((app_0 (car c1_0))) (|#%app| app_0 (cdr c1_0)))
          (poll-custodian-will-executor))
        (void)))))
(define post-shutdown-action void)
(define set-post-shutdown-action!
  (lambda (proc_0) (set! post-shutdown-action proc_0)))
(define 1/custodian-shutdown-all
  (|#%name|
   custodian-shutdown-all
   (lambda (c_0)
     (begin
       (begin
         (if (1/custodian? c_0)
           (void)
           (raise-argument-error 'custodian-shutdown-all "custodian?" c_0))
         (start-atomic)
         (begin0 (do-custodian-shutdown-all c_0) (end-atomic))
         (|#%app| post-shutdown-action))))))
(define custodian-shutdown-root-at-exit
  (lambda ()
    (begin
      (start-atomic)
      (begin0
        (do-custodian-shutdown-all (unsafe-place-local-ref cell.1$6) #t)
        (end-atomic)))))
(define queued-shutdowns null)
(define queue-custodian-shutdown!
  (lambda (c_0)
    (if (custodian-need-shutdown c_0)
      (void)
      (begin
        (set-custodian-need-shutdown! c_0 'needed)
        (set! queued-shutdowns (cons c_0 queued-shutdowns))
        (|#%app| place-wakeup-initial)))))
(define check-queued-custodian-shutdown
  (lambda ()
    (begin
      (if (unsafe-place-local-ref cell.2$4)
        (begin (unsafe-place-local-set! cell.2$4 #f) (collect-garbage))
        (void))
      (if (null? queued-shutdowns)
        #f
        (begin
          (|#%app| host:disable-interrupts)
          (begin
            (|#%app| host:mutex-acquire memory-limit-lock)
            (let ((queued_0 queued-shutdowns))
              (begin
                (set! queued-shutdowns
                  (reverse$1
                   (begin
                     (letrec*
                      ((for-loop_0
                        (|#%name|
                         for-loop
                         (lambda (fold-var_0 lst_0)
                           (begin
                             (if (pair? lst_0)
                               (let ((c_0 (unsafe-car lst_0)))
                                 (let ((rest_0 (unsafe-cdr lst_0)))
                                   (let ((fold-var_1
                                          (if (custodian-this-place? c_0)
                                            fold-var_0
                                            (let ((fold-var_1
                                                   (cons
                                                    (begin
                                                      (if (eq?
                                                           (custodian-need-shutdown
                                                            c_0)
                                                           'needed)
                                                        (begin
                                                          (set-custodian-need-shutdown!
                                                           c_0
                                                           'neeed/sent-wakeup)
                                                          (let ((app_0
                                                                 place-wakeup))
                                                            (|#%app|
                                                             app_0
                                                             (custodian-place
                                                              c_0))))
                                                        (void))
                                                      c_0)
                                                    fold-var_0)))
                                              (values fold-var_1)))))
                                     (for-loop_0 fold-var_1 rest_0))))
                               fold-var_0))))))
                      (for-loop_0 null queued_0)))))
                (|#%app| host:mutex-release memory-limit-lock)
                (|#%app| host:enable-interrupts)
                (begin
                  (letrec*
                   ((for-loop_0
                     (|#%name|
                      for-loop
                      (lambda (lst_0)
                        (begin
                          (if (pair? lst_0)
                            (let ((c_0 (unsafe-car lst_0)))
                              (let ((rest_0 (unsafe-cdr lst_0)))
                                (call-with-values
                                 (lambda ()
                                   (if (custodian-this-place? c_0)
                                     (begin
                                       (do-custodian-shutdown-all c_0)
                                       (values))
                                     (values)))
                                 (case-lambda
                                  (() (for-loop_0 rest_0))
                                  (args
                                   (raise-binding-result-arity-error
                                    0
                                    args))))))
                            (values)))))))
                   (for-loop_0 queued_0)))
                (void)
                (unsafe-place-local-set! cell.2$4 #t)
                #t))))))))
(define cell.2$4 (unsafe-make-place-local #f))
(define place-ensure-wakeup! (lambda () #f))
(define place-wakeup-initial void)
(define place-wakeup void)
(define set-place-custodian-procs!
  (lambda (ensure-wakeup!_0 wakeup-initial_0 wakeup_0)
    (begin
      (set! place-ensure-wakeup! ensure-wakeup!_0)
      (set! place-wakeup-initial wakeup-initial_0)
      (set! place-wakeup wakeup_0))))
(define custodian-this-place?
  (lambda (c_0) (eq? (custodian-place c_0) (unsafe-place-local-ref cell.1$2))))
(define do-custodian-shutdown-all
  (let ((do-custodian-shutdown-all_0
         (|#%name|
          do-custodian-shutdown-all
          (lambda (c25_0 only-at-exit?24_0)
            (begin
              (if (1/custodian-shut-down? c25_0)
                (void)
                (begin
                  (set-custodian-shut-down! c25_0)
                  (begin
                    (if (custodian-sync-futures? c25_0)
                      (|#%app| futures-sync-for-custodian-shutdown)
                      (void))
                    (begin
                      (let ((ht_0 (custodian-children c25_0)))
                        (begin
                          (letrec*
                           ((for-loop_0
                             (|#%name|
                              for-loop
                              (lambda (i_0)
                                (begin
                                  (if i_0
                                    (call-with-values
                                     (lambda ()
                                       (hash-iterate-key+value ht_0 i_0 #f))
                                     (case-lambda
                                      ((child_0 callback_0)
                                       (begin
                                         (if (if child_0
                                               (let ((or-part_0
                                                      (not only-at-exit?24_0)))
                                                 (if or-part_0
                                                   or-part_0
                                                   (at-exit-callback?
                                                    callback_0)))
                                               #f)
                                           (if (procedure-arity-includes?
                                                callback_0
                                                2)
                                             (|#%app| callback_0 child_0 c25_0)
                                             (|#%app| callback_0 child_0))
                                           (void))
                                         (for-loop_0
                                          (hash-iterate-next ht_0 i_0))))
                                      (args
                                       (raise-binding-result-arity-error
                                        2
                                        args))))
                                    (values)))))))
                           (for-loop_0 (hash-iterate-first ht_0)))))
                      (begin
                        (hash-clear! (custodian-children c25_0))
                        (begin
                          (if (custodian-gc-roots c25_0)
                            (hash-clear! (custodian-gc-roots c25_0))
                            (void))
                          (begin
                            (let ((lst_0 (custodian-post-shutdown c25_0)))
                              (begin
                                (letrec*
                                 ((for-loop_0
                                   (|#%name|
                                    for-loop
                                    (lambda (lst_1)
                                      (begin
                                        (if (pair? lst_1)
                                          (let ((proc_0 (unsafe-car lst_1)))
                                            (let ((rest_0 (unsafe-cdr lst_1)))
                                              (begin
                                                (|#%app| proc_0)
                                                (for-loop_0 rest_0))))
                                          (values)))))))
                                 (for-loop_0 lst_0))))
                            (begin
                              (set-custodian-post-shutdown! c25_0 null)
                              (begin
                                (let ((sema_0 (custodian-shutdown-sema c25_0)))
                                  (if sema_0
                                    (semaphore-post-all sema_0)
                                    (void)))
                                (let ((p-cref_0
                                       (custodian-parent-reference c25_0)))
                                  (begin
                                    (if p-cref_0
                                      (1/unsafe-custodian-unregister
                                       c25_0
                                       p-cref_0)
                                      (void))
                                    (remove-limit-custodian! c25_0)
                                    (set-custodian-memory-limits!
                                     c25_0
                                     null)))))))))))))))))
    (case-lambda
     ((c_0) (do-custodian-shutdown-all_0 c_0 #f))
     ((c_0 only-at-exit?24_0)
      (do-custodian-shutdown-all_0 c_0 only-at-exit?24_0)))))
(define custodian-get-shutdown-sema
  (lambda (c_0)
    (begin
      (start-atomic)
      (begin0
        (let ((or-part_0 (custodian-shutdown-sema c_0)))
          (if or-part_0
            or-part_0
            (let ((sema_0 (1/make-semaphore)))
              (begin
                (set-custodian-shutdown-sema! c_0 sema_0)
                (if (1/custodian-shut-down? c_0)
                  (semaphore-post-all sema_0)
                  (void))
                sema_0))))
        (end-atomic)))))
(define 1/unsafe-add-post-custodian-shutdown
  (let ((unsafe-add-post-custodian-shutdown_0
         (|#%name|
          unsafe-add-post-custodian-shutdown
          (lambda (proc27_0 custodian26_0)
            (begin
              (begin
                (if (if (procedure? proc27_0)
                      (procedure-arity-includes? proc27_0 0)
                      #f)
                  (void)
                  (raise-argument-error
                   'unsafe-add-post-custodian-shutdown
                   "(procedure-arity-includes/c 0)"
                   proc27_0))
                (begin
                  (if (let ((or-part_0 (not custodian26_0)))
                        (if or-part_0 or-part_0 (1/custodian? custodian26_0)))
                    (void)
                    (raise-argument-error
                     'unsafe-add-post-custodian-shutdown
                     "(or/c custodian? #f)"
                     custodian26_0))
                  (let ((c_0
                         (if custodian26_0
                           custodian26_0
                           (place-custodian
                            (unsafe-place-local-ref cell.1$2)))))
                    (if (if (not
                             (place-parent (unsafe-place-local-ref cell.1$2)))
                          (eq?
                           c_0
                           (place-custodian (unsafe-place-local-ref cell.1$2)))
                          #f)
                      (void)
                      (begin
                        (start-atomic)
                        (begin0
                          (set-custodian-post-shutdown!
                           c_0
                           (cons proc27_0 (custodian-post-shutdown c_0)))
                          (end-atomic))))))))))))
    (|#%name|
     unsafe-add-post-custodian-shutdown
     (case-lambda
      ((proc_0) (begin (unsafe-add-post-custodian-shutdown_0 proc_0 #f)))
      ((proc_0 custodian26_0)
       (unsafe-add-post-custodian-shutdown_0 proc_0 custodian26_0))))))
(define custodian-subordinate?
  (lambda (c_0 super-c_0)
    (letrec*
     ((loop_0
       (|#%name|
        loop
        (lambda (p-cref_0)
          (begin
            (let ((p_0
                   (if p-cref_0 (custodian-reference->custodian p-cref_0) #f)))
              (if (eq? p_0 super-c_0)
                #t
                (if (not p_0)
                  #f
                  (loop_0 (custodian-parent-reference p_0))))))))))
     (loop_0 (custodian-parent-reference c_0)))))
(define custodian-manages-reference?
  (lambda (c_0 cref_0)
    (let ((ref-c_0 (custodian-reference->custodian cref_0)))
      (let ((or-part_0 (eq? c_0 ref-c_0)))
        (if or-part_0 or-part_0 (custodian-subordinate? ref-c_0 c_0))))))
(define custodian-reference->custodian
  (lambda (cref_0)
    (let ((c_0 (custodian-reference-weak-c cref_0)))
      (if (custodian-reference? c_0)
        (let ((next-c_0 (custodian-reference-weak-c c_0)))
          (if (custodian-reference? next-c_0)
            (begin
              (set-custodian-reference-weak-c! cref_0 next-c_0)
              (custodian-reference->custodian cref_0))
            (weak-box-value next-c_0)))
        (weak-box-value c_0)))))
(define 1/custodian-managed-list
  (|#%name|
   custodian-managed-list
   (lambda (c_0 super-c_0)
     (begin
       (begin
         (if (1/custodian? c_0)
           (void)
           (raise-argument-error 'custodian-managed-list "custodian?" c_0))
         (if (1/custodian? super-c_0)
           (void)
           (raise-argument-error
            'custodian-managed-list
            "custodian?"
            super-c_0))
         (if (custodian-subordinate? c_0 super-c_0)
           (void)
           (raise-arguments-error
            'custodian-managed-list
            "the second custodian does not manage the first custodian"
            "first custodian"
            c_0
            "second custodian"
            super-c_0))
         (reverse$1
          (let ((ht_0 (custodian-children c_0)))
            (begin
              (letrec*
               ((for-loop_0
                 (|#%name|
                  for-loop
                  (lambda (fold-var_0 i_0)
                    (begin
                      (if i_0
                        (let ((v_0 (hash-iterate-key ht_0 i_0)))
                          (let ((fold-var_1
                                 (if (not (1/custodian-box? v_0))
                                   (let ((fold-var_1 (cons v_0 fold-var_0)))
                                     (values fold-var_1))
                                   fold-var_0)))
                            (for-loop_0
                             fold-var_1
                             (hash-iterate-next ht_0 i_0))))
                        fold-var_0))))))
               (for-loop_0 null (hash-iterate-first ht_0)))))))))))
(define 1/custodian-memory-accounting-available?
  (|#%name| custodian-memory-accounting-available? (lambda () (begin #t))))
(define 1/custodian-require-memory
  (|#%name|
   custodian-require-memory
   (lambda (limit-cust_0 need-amt_0 stop-cust_0)
     (begin
       (begin
         (if (1/custodian? limit-cust_0)
           (void)
           (raise-argument-error
            'custodian-require-memory
            "custodian?"
            limit-cust_0))
         (if (exact-nonnegative-integer? need-amt_0)
           (void)
           (raise-argument-error
            'custodian-require-memory
            "exact-nonnegative-integer?"
            need-amt_0))
         (if (1/custodian? stop-cust_0)
           (void)
           (raise-argument-error
            'custodian-require-memory
            "custodian?"
            stop-cust_0))
         (raise
          (|#%app|
           exn:fail:unsupported
           "custodian-require-memory: unsupported"
           (current-continuation-marks))))))))
(define 1/custodian-limit-memory
  (let ((custodian-limit-memory_0
         (|#%name|
          custodian-limit-memory
          (lambda (limit-cust29_0 need-amt30_0 stop-cust28_0)
            (begin
              (let ((stop-cust_0
                     (if (eq? stop-cust28_0 unsafe-undefined)
                       limit-cust29_0
                       stop-cust28_0)))
                (begin
                  (if (1/custodian? limit-cust29_0)
                    (void)
                    (raise-argument-error
                     'custodian-limit-memory
                     "custodian?"
                     limit-cust29_0))
                  (if (exact-nonnegative-integer? need-amt30_0)
                    (void)
                    (raise-argument-error
                     'custodian-limit-memory
                     "exact-nonnegative-integer?"
                     need-amt30_0))
                  (if (1/custodian? stop-cust_0)
                    (void)
                    (raise-argument-error
                     'custodian-limit-memory
                     "custodian?"
                     stop-cust_0))
                  (|#%app| place-ensure-wakeup!)
                  (start-atomic/no-interrupts)
                  (begin0
                    (if (let ((or-part_0
                               (1/custodian-shut-down? limit-cust29_0)))
                          (if or-part_0
                            or-part_0
                            (1/custodian-shut-down? stop-cust_0)))
                      (void)
                      (begin
                        (set-custodian-memory-limits!
                         limit-cust29_0
                         (let ((app_0
                                (cons
                                 need-amt30_0
                                 (if (eq? limit-cust29_0 stop-cust_0)
                                   #f
                                   stop-cust_0))))
                           (cons
                            app_0
                            (custodian-memory-limits limit-cust29_0))))
                        (if (eq? stop-cust_0 limit-cust29_0)
                          (let ((old-limit_0
                                 (custodian-immediate-limit limit-cust29_0)))
                            (if (let ((or-part_0 (not old-limit_0)))
                                  (if or-part_0
                                    or-part_0
                                    (> old-limit_0 need-amt30_0)))
                              (set-custodian-immediate-limit!
                               limit-cust29_0
                               need-amt30_0)
                              (void)))
                          (void))
                        (check-limit-custodian limit-cust29_0)))
                    (end-atomic/no-interrupts))
                  (void))))))))
    (|#%name|
     custodian-limit-memory
     (case-lambda
      ((limit-cust_0 need-amt_0)
       (begin
         (custodian-limit-memory_0 limit-cust_0 need-amt_0 unsafe-undefined)))
      ((limit-cust_0 need-amt_0 stop-cust28_0)
       (custodian-limit-memory_0 limit-cust_0 need-amt_0 stop-cust28_0))))))
(define custodians-with-limits (make-hasheq))
(define check-limit-custodian
  (lambda (limit-cust_0)
    (if (pair? (custodian-memory-limits limit-cust_0))
      (begin
        (|#%app| host:disable-interrupts)
        (|#%app| host:mutex-acquire memory-limit-lock)
        (if (if (custodian-gc-roots limit-cust_0)
              (positive? (hash-count (custodian-gc-roots limit-cust_0)))
              #f)
          (begin
            (hash-set! custodians-with-limits limit-cust_0 #t)
            (set! compute-memory-sizes (max compute-memory-sizes 1)))
          (hash-remove! custodians-with-limits limit-cust_0))
        (|#%app| host:mutex-release memory-limit-lock)
        (|#%app| host:enable-interrupts))
      (void))))
(define remove-limit-custodian!
  (lambda (c_0)
    (begin
      (if (if (custodian-gc-roots c_0)
            (positive? (hash-count (custodian-gc-roots c_0)))
            #f)
        (internal-error "remove-limit-custodian!: roots table is not empty")
        (void))
      (check-limit-custodian c_0))))
(define 1/make-custodian-box
  (|#%name|
   make-custodian-box
   (lambda (c_0 v_0)
     (begin
       (begin
         (if (1/custodian? c_0)
           (void)
           (raise-argument-error 'make-custodian-box "custodian?" c_0))
         (let ((b_0 (custodian-box1.1 v_0 (custodian-get-shutdown-sema c_0))))
           (begin
             (if (let ((temp76_0 (lambda (b_1) (set-custodian-box-v! b_1 #f))))
                   (do-custodian-register.1 #f #t #f #t c_0 b_0 temp76_0))
               (void)
               (begin-unsafe
                (raise-arguments-error
                 'make-custodian-box
                 "the custodian has been shut down"
                 "custodian"
                 c_0)))
             b_0)))))))
(define 1/custodian-box-value
  (|#%name|
   custodian-box-value
   (lambda (cb_0)
     (begin
       (begin
         (if (1/custodian-box? cb_0)
           (void)
           (raise-argument-error 'custodian-box-value "custodian-box?" cb_0))
         (custodian-box-v cb_0))))))
(define raise-custodian-is-shut-down
  (lambda (who_0 c_0)
    (raise-arguments-error
     who_0
     "the custodian has been shut down"
     "custodian"
     c_0)))
(define thread-engine-for-roots (lambda (t_0) #f))
(define set-thread-engine-for-roots!
  (lambda (thread-engine_0) (set! thread-engine-for-roots thread-engine_0)))
(define futures-sync-for-custodian-shutdown (lambda () (void)))
(define future-scheduler-add-thread-custodian-mapping!
  (lambda (s_0 ht_0) (void)))
(define set-custodian-future-callbacks!
  (lambda (sync-shutdown_0 add-custodian-mapping_0)
    (begin
      (set! futures-sync-for-custodian-shutdown sync-shutdown_0)
      (set! future-scheduler-add-thread-custodian-mapping!
        add-custodian-mapping_0))))
(define memory-limit-lock (|#%app| host:make-mutex))
(define compute-memory-sizes 0)
(define computed-memory-sizes? #f)
(define effect_2498
  (begin
    (void
     (|#%app|
      set-reachable-size-increments-callback!
      (lambda (call-with-size-increments_0)
        (if (zero? compute-memory-sizes)
          (|#%app|
           call-with-size-increments_0
           null
           null
           (lambda (sizes_0 custs_0) (void)))
          (|#%app|
           host:call-with-current-continuation-roots
           (lambda (k-roots_0)
             (let ((custodian-future-threads_0 (make-hasheq)))
               (begin
                 (let ((app_0 future-scheduler-add-thread-custodian-mapping!))
                   (|#%app|
                    app_0
                    (place-future-scheduler initial-place)
                    custodian-future-threads_0))
                 (call-with-values
                  (lambda ()
                    (letrec*
                     ((c-loop_0
                       (|#%name|
                        c-loop
                        (lambda (c_0 pl_0 accum-roots_0 accum-custs_0)
                          (begin
                            (begin
                              (set-custodian-memory-use! c_0 0)
                              (let ((gc-roots_0 (custodian-gc-roots c_0)))
                                (let ((roots_0
                                       (if gc-roots_0
                                         (hash-keys gc-roots_0)
                                         null)))
                                  (let ((host-regs_0
                                         (let ((pl_1 (custodian-place c_0)))
                                           (if (eq? (place-custodian pl_1) c_0)
                                             (list pl_1)
                                             null))))
                                    (letrec*
                                     ((loop_0
                                       (|#%name|
                                        loop
                                        (lambda (roots_1
                                                 local-accum-roots_0
                                                 accum-roots_1
                                                 accum-custs_1)
                                          (begin
                                            (if (null? roots_1)
                                              (let ((local-custs_0
                                                     (reverse$1
                                                      (begin
                                                        (letrec*
                                                         ((for-loop_0
                                                           (|#%name|
                                                            for-loop
                                                            (lambda (fold-var_0
                                                                     lst_0)
                                                              (begin
                                                                (if (pair?
                                                                     lst_0)
                                                                  (let ((root_0
                                                                         (unsafe-car
                                                                          lst_0)))
                                                                    (let ((rest_0
                                                                           (unsafe-cdr
                                                                            lst_0)))
                                                                      (let ((fold-var_1
                                                                             (cons
                                                                              c_0
                                                                              fold-var_0)))
                                                                        (let ((fold-var_2
                                                                               (values
                                                                                fold-var_1)))
                                                                          (for-loop_0
                                                                           fold-var_2
                                                                           rest_0)))))
                                                                  fold-var_0))))))
                                                         (for-loop_0
                                                          null
                                                          local-accum-roots_0))))))
                                                (let ((app_0
                                                       (append
                                                        (reverse$1
                                                         local-accum-roots_0)
                                                        accum-roots_1)))
                                                  (values
                                                   app_0
                                                   (append
                                                    local-custs_0
                                                    accum-custs_1))))
                                              (if (1/custodian? (car roots_1))
                                                (call-with-values
                                                 (lambda ()
                                                   (c-loop_0
                                                    (car roots_1)
                                                    pl_0
                                                    accum-roots_1
                                                    accum-custs_1))
                                                 (case-lambda
                                                  ((new-roots_0 new-custs_0)
                                                   (loop_0
                                                    (cdr roots_1)
                                                    local-accum-roots_0
                                                    new-roots_0
                                                    new-custs_0))
                                                  (args
                                                   (raise-binding-result-arity-error
                                                    2
                                                    args))))
                                                (if (1/place? (car roots_1))
                                                  (let ((pl_1 (car roots_1)))
                                                    (let ((c_1
                                                           (place-custodian
                                                            pl_1)))
                                                      (begin
                                                        (let ((app_0
                                                               future-scheduler-add-thread-custodian-mapping!))
                                                          (|#%app|
                                                           app_0
                                                           (place-future-scheduler
                                                            pl_1)
                                                           custodian-future-threads_0))
                                                        (call-with-values
                                                         (lambda ()
                                                           (c-loop_0
                                                            c_1
                                                            pl_1
                                                            accum-roots_1
                                                            accum-custs_1))
                                                         (case-lambda
                                                          ((new-roots_0
                                                            new-custs_0)
                                                           (loop_0
                                                            (cdr roots_1)
                                                            local-accum-roots_0
                                                            new-roots_0
                                                            new-custs_0))
                                                          (args
                                                           (raise-binding-result-arity-error
                                                            2
                                                            args)))))))
                                                  (let ((root_0 (car roots_1)))
                                                    (let ((new-local-roots_0
                                                           (cons
                                                            root_0
                                                            local-accum-roots_0)))
                                                      (let ((more-local-roots_0
                                                             (if (eq?
                                                                  root_0
                                                                  (place-current-thread
                                                                   pl_0))
                                                               (let ((more-local-roots_0
                                                                      (cons
                                                                       (place-host-thread
                                                                        pl_0)
                                                                       new-local-roots_0)))
                                                                 (if (eq?
                                                                      pl_0
                                                                      (unsafe-place-local-ref
                                                                       cell.1$2))
                                                                   (append
                                                                    k-roots_0
                                                                    more-local-roots_0)
                                                                   more-local-roots_0))
                                                               new-local-roots_0)))
                                                        (let ((even-more-local-roots_0
                                                               (let ((c2_0
                                                                      (|#%app|
                                                                       thread-engine-for-roots
                                                                       root_0)))
                                                                 (if c2_0
                                                                   (append
                                                                    (|#%app|
                                                                     engine-roots
                                                                     c2_0)
                                                                    more-local-roots_0)
                                                                   more-local-roots_0))))
                                                          (loop_0
                                                           (cdr roots_1)
                                                           even-more-local-roots_0
                                                           accum-roots_1
                                                           accum-custs_1)))))))))))))
                                     (loop_0
                                      roots_0
                                      (cons c_0 host-regs_0)
                                      accum-roots_0
                                      accum-custs_0)))))))))))
                     (c-loop_0
                      initial-place-root-custodian
                      initial-place
                      null
                      null)))
                  (case-lambda
                   ((roots_0 custs_0)
                    (|#%app|
                     call-with-size-increments_0
                     roots_0
                     custs_0
                     (lambda (sizes_0 custs_1)
                       (begin
                         (begin
                           (letrec*
                            ((for-loop_0
                              (|#%name|
                               for-loop
                               (lambda (lst_0 lst_1)
                                 (begin
                                   (if (if (pair? lst_0) (pair? lst_1) #f)
                                     (let ((size_0 (unsafe-car lst_0)))
                                       (let ((rest_0 (unsafe-cdr lst_0)))
                                         (let ((c_0 (unsafe-car lst_1)))
                                           (let ((rest_1 (unsafe-cdr lst_1)))
                                             (begin
                                               (set-custodian-memory-use!
                                                c_0
                                                (+
                                                 size_0
                                                 (custodian-memory-use c_0)))
                                               (for-loop_0 rest_0 rest_1))))))
                                     (values)))))))
                            (for-loop_0 sizes_0 custs_1)))
                         (let ((any-limits?_0
                                (letrec*
                                 ((c-loop_0
                                   (|#%name|
                                    c-loop
                                    (lambda (c_0)
                                      (begin
                                        (let ((gc-roots_0
                                               (custodian-gc-roots c_0)))
                                          (let ((roots_1
                                                 (let ((app_0
                                                        (hash-ref
                                                         custodian-future-threads_0
                                                         c_0
                                                         null)))
                                                   (append
                                                    app_0
                                                    (if gc-roots_0
                                                      (hash-keys gc-roots_0)
                                                      null)))))
                                            (let ((any-limits?_0
                                                   (begin
                                                     (letrec*
                                                      ((for-loop_0
                                                        (|#%name|
                                                         for-loop
                                                         (lambda (any-limits?_0
                                                                  lst_0)
                                                           (begin
                                                             (if (pair? lst_0)
                                                               (let ((root_0
                                                                      (unsafe-car
                                                                       lst_0)))
                                                                 (let ((rest_0
                                                                        (unsafe-cdr
                                                                         lst_0)))
                                                                   (let ((any-limits?_1
                                                                          (if (let ((or-part_0
                                                                                     (1/custodian?
                                                                                      root_0)))
                                                                                (if or-part_0
                                                                                  or-part_0
                                                                                  (1/place?
                                                                                   root_0)))
                                                                            (let ((any-limits?_1
                                                                                   (let ((next-c_0
                                                                                          (if (1/custodian?
                                                                                               root_0)
                                                                                            root_0
                                                                                            (place-custodian
                                                                                             root_0))))
                                                                                     (let ((root-any-limits?_0
                                                                                            (c-loop_0
                                                                                             next-c_0)))
                                                                                       (begin
                                                                                         (set-custodian-memory-use!
                                                                                          c_0
                                                                                          (let ((app_0
                                                                                                 (custodian-memory-use
                                                                                                  next-c_0)))
                                                                                            (+
                                                                                             app_0
                                                                                             (custodian-memory-use
                                                                                              c_0))))
                                                                                         (if root-any-limits?_0
                                                                                           root-any-limits?_0
                                                                                           any-limits?_0))))))
                                                                              (values
                                                                               any-limits?_1))
                                                                            any-limits?_0)))
                                                                     (for-loop_0
                                                                      any-limits?_1
                                                                      rest_0))))
                                                               any-limits?_0))))))
                                                      (for-loop_0
                                                       #f
                                                       roots_1)))))
                                              (let ((use_0
                                                     (custodian-memory-use
                                                      c_0)))
                                                (let ((old-limits_0
                                                       (custodian-memory-limits
                                                        c_0)))
                                                  (let ((new-limits_0
                                                         (reverse$1
                                                          (begin
                                                            (letrec*
                                                             ((for-loop_0
                                                               (|#%name|
                                                                for-loop
                                                                (lambda (fold-var_0
                                                                         lst_0)
                                                                  (begin
                                                                    (if (pair?
                                                                         lst_0)
                                                                      (let ((limit_0
                                                                             (unsafe-car
                                                                              lst_0)))
                                                                        (let ((rest_0
                                                                               (unsafe-cdr
                                                                                lst_0)))
                                                                          (let ((fold-var_1
                                                                                 (if (if (<=
                                                                                          (car
                                                                                           limit_0)
                                                                                          use_0)
                                                                                       (begin
                                                                                         (queue-custodian-shutdown!
                                                                                          (let ((or-part_0
                                                                                                 (cdr
                                                                                                  limit_0)))
                                                                                            (if or-part_0
                                                                                              or-part_0
                                                                                              c_0)))
                                                                                         #f)
                                                                                       #t)
                                                                                   (let ((fold-var_1
                                                                                          (cons
                                                                                           limit_0
                                                                                           fold-var_0)))
                                                                                     (values
                                                                                      fold-var_1))
                                                                                   fold-var_0)))
                                                                            (for-loop_0
                                                                             fold-var_1
                                                                             rest_0))))
                                                                      fold-var_0))))))
                                                             (for-loop_0
                                                              null
                                                              old-limits_0))))))
                                                    (begin
                                                      (set-custodian-memory-limits!
                                                       c_0
                                                       new-limits_0)
                                                      (if (if (pair?
                                                               old-limits_0)
                                                            (let ((or-part_0
                                                                   (null?
                                                                    new-limits_0)))
                                                              (if or-part_0
                                                                or-part_0
                                                                (let ((or-part_1
                                                                       (not
                                                                        (custodian-gc-roots
                                                                         c_0))))
                                                                  (if or-part_1
                                                                    or-part_1
                                                                    (zero?
                                                                     (hash-count
                                                                      (custodian-gc-roots
                                                                       c_0)))))))
                                                            #f)
                                                        (hash-remove!
                                                         custodians-with-limits
                                                         c_0)
                                                        (void))
                                                      (if any-limits?_0
                                                        any-limits?_0
                                                        (pair?
                                                         new-limits_0))))))))))))))
                                 (c-loop_0 initial-place-root-custodian))))
                           (begin
                             (if any-limits?_0
                               (void)
                               (set! compute-memory-sizes
                                 (sub1 compute-memory-sizes)))
                             (set! computed-memory-sizes? #t)))))))
                   (args (raise-binding-result-arity-error 2 args))))))))))))
    (void)))
(define effect_3119
  (begin
    (void
     (|#%app|
      set-custodian-memory-use-proc!
      (lambda (c_0 all_0)
        (begin
          (if (1/custodian? c_0)
            (void)
            (raise-argument-error
             'current-memory-use
             "(or/c #f 'cumulative custodian?)"
             c_0))
          (if (eq? c_0 initial-place-root-custodian)
            all_0
            (begin
              (if (begin
                    (start-atomic/no-interrupts)
                    (begin0
                      (begin
                        (|#%app| host:mutex-acquire memory-limit-lock)
                        (if (zero? compute-memory-sizes)
                          (begin
                            (set! computed-memory-sizes? #f)
                            (set! compute-memory-sizes 2)
                            (|#%app| host:mutex-release memory-limit-lock)
                            #t)
                          (let ((done?_0 computed-memory-sizes?))
                            (begin
                              (|#%app| host:mutex-release memory-limit-lock)
                              (not done?_0)))))
                      (end-atomic/no-interrupts)))
                (collect-garbage)
                (void))
              (custodian-memory-use c_0)))))))
    (void)))
(define custodian-check-immediate-limit
  (lambda (mref_0 n_0)
    (letrec*
     ((loop_0
       (|#%name|
        loop
        (lambda (mref_1)
          (begin
            (if mref_1
              (let ((c_0 (custodian-reference->custodian mref_1)))
                (if c_0
                  (let ((limit_0 (custodian-immediate-limit c_0)))
                    (begin
                      (if (if limit_0 (>= n_0 limit_0) #f)
                        (raise
                         (|#%app|
                          exn:fail:out-of-memory
                          "out of memory"
                          (current-continuation-marks)))
                        (void))
                      (loop_0 (custodian-parent-reference c_0))))
                  (void)))
              (void)))))))
     (loop_0 mref_0))))
(define struct:thread
  (make-record-type-descriptor*
   'thread
   struct:node
   (|#%nongenerative-uid| thread)
   #f
   #f
   24
   16777082))
(define effect_2967
  (struct-type-install-properties!
   struct:thread
   '(thread)
   24
   0
   struct:node
   (let ((app_0 (cons prop:authentic #t)))
     (let ((app_1 (cons prop:object-name 0)))
       (let ((app_2
              (cons
               1/prop:evt
               (lambda (t_0)
                 (wrap-evt7.1
                  (|#%app| get-thread-dead-evt t_0)
                  (lambda (v_0) t_0))))))
         (let ((app_3
                (cons
                 prop:waiter
                 (let ((temp28_0
                        (lambda (t_0 i-cb_0)
                          (|#%app| thread-deschedule! t_0 #f i-cb_0))))
                   (let ((temp29_0
                          (lambda (t_0 v_0)
                            (begin (|#%app| thread-reschedule! t_0) v_0))))
                     (make-waiter-methods.1 temp29_0 temp28_0))))))
           (list
            app_0
            app_1
            app_2
            app_3
            (cons host:prop:unsafe-authentic-override #t))))))
   (current-inspector)
   #f
   '(0 2 7)
   #f
   'thread))
(define thread1.1
  (|#%name|
   thread
   (record-constructor
    (make-record-constructor-descriptor struct:thread #f #f))))
(define 1/thread? (|#%name| thread? (record-predicate struct:thread)))
(define thread-name (|#%name| thread-name (record-accessor struct:thread 0)))
(define thread-engine
  (|#%name| thread-engine (record-accessor struct:thread 1)))
(define thread-parent
  (|#%name| thread-parent (record-accessor struct:thread 2)))
(define thread-sleeping
  (|#%name| thread-sleeping (record-accessor struct:thread 3)))
(define thread-sched-info
  (|#%name| thread-sched-info (record-accessor struct:thread 4)))
(define thread-custodian-references
  (|#%name| thread-custodian-references (record-accessor struct:thread 5)))
(define thread-transitive-resumes
  (|#%name| thread-transitive-resumes (record-accessor struct:thread 6)))
(define thread-suspend-to-kill?
  (|#%name| thread-suspend-to-kill? (record-accessor struct:thread 7)))
(define thread-kill-callbacks
  (|#%name| thread-kill-callbacks (record-accessor struct:thread 8)))
(define thread-suspend+resume-callbacks
  (|#%name| thread-suspend+resume-callbacks (record-accessor struct:thread 9)))
(define thread-descheduled?
  (|#%name| thread-descheduled? (record-accessor struct:thread 10)))
(define thread-interrupt-callback
  (|#%name| thread-interrupt-callback (record-accessor struct:thread 11)))
(define thread-dead-sema
  (|#%name| thread-dead-sema (record-accessor struct:thread 12)))
(define 1/thread-dead-evt
  (|#%name| thread-dead-evt (record-accessor struct:thread 13)))
(define thread-suspended-box
  (|#%name| thread-suspended-box (record-accessor struct:thread 14)))
(define thread-suspended-evt
  (|#%name| thread-suspended-evt (record-accessor struct:thread 15)))
(define thread-resumed-evt
  (|#%name| thread-resumed-evt (record-accessor struct:thread 16)))
(define thread-pending-break
  (|#%name| thread-pending-break (record-accessor struct:thread 17)))
(define thread-ignore-break-cells
  (|#%name| thread-ignore-break-cells (record-accessor struct:thread 18)))
(define thread-forward-break-to
  (|#%name| thread-forward-break-to (record-accessor struct:thread 19)))
(define thread-mailbox
  (|#%name| thread-mailbox (record-accessor struct:thread 20)))
(define thread-mailbox-wakeup
  (|#%name| thread-mailbox-wakeup (record-accessor struct:thread 21)))
(define thread-cpu-time
  (|#%name| thread-cpu-time (record-accessor struct:thread 22)))
(define thread-future
  (|#%name| thread-future (record-accessor struct:thread 23)))
(define set-thread-engine!
  (|#%name| set-thread-engine! (record-mutator struct:thread 1)))
(define set-thread-sleeping!
  (|#%name| set-thread-sleeping! (record-mutator struct:thread 3)))
(define set-thread-sched-info!
  (|#%name| set-thread-sched-info! (record-mutator struct:thread 4)))
(define set-thread-custodian-references!
  (|#%name| set-thread-custodian-references! (record-mutator struct:thread 5)))
(define set-thread-transitive-resumes!
  (|#%name| set-thread-transitive-resumes! (record-mutator struct:thread 6)))
(define set-thread-kill-callbacks!
  (|#%name| set-thread-kill-callbacks! (record-mutator struct:thread 8)))
(define set-thread-suspend+resume-callbacks!
  (|#%name|
   set-thread-suspend+resume-callbacks!
   (record-mutator struct:thread 9)))
(define set-thread-descheduled?!
  (|#%name| set-thread-descheduled?! (record-mutator struct:thread 10)))
(define set-thread-interrupt-callback!
  (|#%name| set-thread-interrupt-callback! (record-mutator struct:thread 11)))
(define set-thread-dead-sema!
  (|#%name| set-thread-dead-sema! (record-mutator struct:thread 12)))
(define set-thread-dead-evt!
  (|#%name| set-thread-dead-evt! (record-mutator struct:thread 13)))
(define set-thread-suspended-box!
  (|#%name| set-thread-suspended-box! (record-mutator struct:thread 14)))
(define set-thread-suspended-evt!
  (|#%name| set-thread-suspended-evt! (record-mutator struct:thread 15)))
(define set-thread-resumed-evt!
  (|#%name| set-thread-resumed-evt! (record-mutator struct:thread 16)))
(define set-thread-pending-break!
  (|#%name| set-thread-pending-break! (record-mutator struct:thread 17)))
(define set-thread-ignore-break-cells!
  (|#%name| set-thread-ignore-break-cells! (record-mutator struct:thread 18)))
(define set-thread-forward-break-to!
  (|#%name| set-thread-forward-break-to! (record-mutator struct:thread 19)))
(define set-thread-mailbox!
  (|#%name| set-thread-mailbox! (record-mutator struct:thread 20)))
(define set-thread-mailbox-wakeup!
  (|#%name| set-thread-mailbox-wakeup! (record-mutator struct:thread 21)))
(define set-thread-cpu-time!
  (|#%name| set-thread-cpu-time! (record-mutator struct:thread 22)))
(define set-thread-future!
  (|#%name| set-thread-future! (record-mutator struct:thread 23)))
(define cell.1$1 (unsafe-make-place-local #f))
(define 1/current-thread
  (|#%name|
   current-thread
   (lambda () (begin (begin (future-barrier) (current-thread/in-atomic))))))
(define do-make-thread.1
  (|#%name|
   do-make-thread
   (lambda (at-root?3_0
            custodian2_0
            initial?4_0
            suspend-to-kill?5_0
            who10_0
            proc11_0)
     (begin
       (let ((c_0
              (if (eq? custodian2_0 unsafe-undefined)
                (1/current-custodian)
                custodian2_0)))
         (begin
           (if (if (procedure? proc11_0)
                 (procedure-arity-includes? proc11_0 0)
                 #f)
             (void)
             (raise-argument-error
              who10_0
              "(procedure-arity-includes/c 0)"
              proc11_0))
           (let ((p_0
                  (if (if at-root?3_0 at-root?3_0 initial?4_0)
                    (unsafe-place-local-ref cell.1)
                    (1/current-thread-group))))
             (let ((e_0
                    (|#%app|
                     make-engine
                     proc11_0
                     (default-continuation-prompt-tag)
                     #f
                     (if (if initial?4_0 initial?4_0 at-root?3_0)
                       break-enabled-default-cell
                       (current-break-enabled-cell))
                     at-root?3_0)))
               (let ((t_0
                      (let ((app_0 (object-name proc11_0)))
                        (thread1.1
                         'none
                         'none
                         app_0
                         e_0
                         p_0
                         #f
                         #f
                         null
                         null
                         suspend-to-kill?5_0
                         null
                         null
                         #f
                         #f
                         #f
                         #f
                         #f
                         #f
                         #f
                         #f
                         #f
                         #f
                         (make-queue)
                         void
                         0
                         #f))))
                 (begin
                   (|#%app|
                    (begin
                      (start-atomic)
                      (begin0
                        (let ((cref_0
                               (if c_0
                                 (custodian-register-thread
                                  c_0
                                  t_0
                                  remove-thread-custodian)
                                 #f)))
                          (if (let ((or-part_0 (not c_0)))
                                (if or-part_0 or-part_0 cref_0))
                            (begin
                              (set-thread-custodian-references!
                               t_0
                               (list cref_0))
                              (thread-group-add! p_0 t_0)
                              void)
                            (lambda ()
                              (begin-unsafe
                               (raise-arguments-error
                                who10_0
                                "the custodian has been shut down"
                                "custodian"
                                c_0)))))
                        (end-atomic))))
                   t_0))))))))))
(define make-thread
  (|#%name|
   thread
   (lambda (proc_0)
     (begin (do-make-thread.1 #f unsafe-undefined #f #f 'thread proc_0)))))
(define 1/thread/suspend-to-kill
  (|#%name|
   thread/suspend-to-kill
   (lambda (proc_0)
     (begin
       (do-make-thread.1
        #f
        unsafe-undefined
        #f
        #t
        'thread/suspend-to-kill
        proc_0)))))
(define make-initial-thread
  (lambda (thunk_0)
    (let ((t_0 (do-make-thread.1 #f unsafe-undefined #t #f 'thread thunk_0)))
      (begin (unsafe-place-local-set! cell.1$1 t_0) t_0))))
(define 1/unsafe-thread-at-root
  (|#%name|
   unsafe-thread-at-root
   (lambda (proc_0)
     (begin
       (let ((root-custodian41_0 (unsafe-place-local-ref cell.1$6)))
         (do-make-thread.1
          #t
          root-custodian41_0
          #f
          #f
          'unsafe-thread-at-root
          proc_0))))))
(define thread-suspended?
  (lambda (t_0)
    (let ((b_0 (thread-suspended-box t_0)))
      (if b_0 (if (unbox b_0) #t #f) #f))))
(define set-thread-suspended?!
  (lambda (t_0 suspended?_0)
    (let ((b_0
           (let ((or-part_0 (thread-suspended-box t_0)))
             (if or-part_0
               or-part_0
               (let ((b_0 (box #f)))
                 (begin (set-thread-suspended-box! t_0 b_0) b_0))))))
      (set-box! b_0 (if suspended?_0 t_0 #f)))))
(define 1/thread-running?
  (|#%name|
   thread-running?
   (lambda (t_0)
     (begin
       (begin
         (if (1/thread? t_0)
           (void)
           (raise-argument-error 'thread-running? "thread?" t_0))
         (if (not (eq? 'done (thread-engine t_0)))
           (not (thread-suspended? t_0))
           #f))))))
(define 1/thread-dead?
  (|#%name|
   thread-dead?
   (lambda (t_0)
     (begin
       (begin
         (if (1/thread? t_0)
           (void)
           (raise-argument-error 'thread-dead? "thread?" t_0))
         (eq? 'done (thread-engine t_0)))))))
(define thread-dead!
  (lambda (t_0)
    (begin
      (set-thread-engine! t_0 'done)
      (run-interrupt-callback t_0)
      (if (thread-dead-sema t_0)
        (semaphore-post-all (thread-dead-sema t_0))
        (void))
      (if (thread-descheduled? t_0)
        (void)
        (begin
          (thread-group-remove! (thread-parent t_0) t_0)
          (|#%app| thread-unscheduled-for-work-tracking! t_0)))
      (remove-from-sleeping-threads! t_0)
      (run-kill-callbacks! t_0)
      (set-thread-suspend+resume-callbacks! t_0 null)
      (if (thread-forward-break-to t_0)
        (do-break-thread (thread-forward-break-to t_0) 'break #f)
        (void))
      (let ((lst_0 (thread-custodian-references t_0)))
        (begin
          (letrec*
           ((for-loop_0
             (|#%name|
              for-loop
              (lambda (lst_1)
                (begin
                  (if (pair? lst_1)
                    (let ((cr_0 (unsafe-car lst_1)))
                      (let ((rest_0 (unsafe-cdr lst_1)))
                        (begin
                          (1/unsafe-custodian-unregister t_0 cr_0)
                          (for-loop_0 rest_0))))
                    (values)))))))
           (for-loop_0 lst_0))))
      (void)
      (set-thread-custodian-references! t_0 null)
      (set-thread-mailbox! t_0 #f)
      (set-thread-mailbox-wakeup! t_0 void))))
(define thread-push-kill-callback!
  (lambda (cb_0)
    (let ((t_0 (current-thread/in-atomic)))
      (set-thread-kill-callbacks!
       t_0
       (cons cb_0 (thread-kill-callbacks t_0))))))
(define thread-pop-kill-callback!
  (lambda ()
    (let ((t_0 (current-thread/in-atomic)))
      (set-thread-kill-callbacks! t_0 (cdr (thread-kill-callbacks t_0))))))
(define 1/kill-thread
  (|#%name|
   kill-thread
   (lambda (t_0)
     (begin
       (begin
         (if (1/thread? t_0)
           (void)
           (raise-argument-error 'kill-thread "thread?" t_0))
         (if (let ((lst_0 (thread-custodian-references t_0)))
               (begin
                 (letrec*
                  ((for-loop_0
                    (|#%name|
                     for-loop
                     (lambda (result_0 lst_1)
                       (begin
                         (if (pair? lst_1)
                           (let ((cr_0 (unsafe-car lst_1)))
                             (let ((rest_0 (unsafe-cdr lst_1)))
                               (let ((result_1
                                      (let ((result_1
                                             (custodian-manages-reference?
                                              (1/current-custodian)
                                              cr_0)))
                                        (values result_1))))
                                 (if (if (not
                                          (let ((x_0 (list cr_0)))
                                            (not result_1)))
                                       #t
                                       #f)
                                   (for-loop_0 result_1 rest_0)
                                   result_1))))
                           result_0))))))
                  (for-loop_0 #t lst_0))))
           (void)
           (raise-arguments-error
            'kill-thread
            "the current custodian does not solely manage the specified thread"
            "thread"
            t_0))
         (if (thread-suspend-to-kill? t_0)
           (|#%app|
            (begin
              (start-atomic)
              (begin0 (do-thread-suspend t_0) (end-atomic))))
           (begin
             (start-atomic)
             (do-kill-thread t_0)
             (end-atomic)
             (if (eq? t_0 (current-thread/in-atomic))
               (begin
                 (if (eq? t_0 (unsafe-place-local-ref cell.1$1))
                   (force-exit 0)
                   (void))
                 (engine-block))
               (void))
             (begin-unsafe (|#%app| 1/check-for-break)))))))))
(define do-kill-thread
  (lambda (t_0) (if (1/thread-dead? t_0) (void) (thread-dead! t_0))))
(define remove-thread-custodian
  (lambda (t_0 c_0)
    (let ((new-crs_0
           (reverse$1
            (let ((lst_0 (thread-custodian-references t_0)))
              (begin
                (letrec*
                 ((for-loop_0
                   (|#%name|
                    for-loop
                    (lambda (fold-var_0 lst_1)
                      (begin
                        (if (pair? lst_1)
                          (let ((cref_0 (unsafe-car lst_1)))
                            (let ((rest_0 (unsafe-cdr lst_1)))
                              (let ((fold-var_1
                                     (if (custodian-manages-reference?
                                          c_0
                                          cref_0)
                                       fold-var_0
                                       (let ((fold-var_1
                                              (cons cref_0 fold-var_0)))
                                         (values fold-var_1)))))
                                (for-loop_0 fold-var_1 rest_0))))
                          fold-var_0))))))
                 (for-loop_0 null lst_0)))))))
      (begin
        (set-thread-custodian-references! t_0 new-crs_0)
        (if (null? new-crs_0)
          (if (thread-suspend-to-kill? t_0)
            (do-thread-suspend t_0)
            (do-kill-thread t_0))
          (void))))))
(define thread-representative-custodian
  (lambda (t_0)
    (begin
      (start-atomic)
      (begin0
        (let ((cs_0 (thread-custodian-references t_0)))
          (if (pair? cs_0) (custodian-reference->custodian (car cs_0)) #f))
        (end-atomic)))))
(define run-kill-callbacks!
  (lambda (t_0)
    (begin
      (let ((lst_0 (thread-kill-callbacks t_0)))
        (begin
          (letrec*
           ((for-loop_0
             (|#%name|
              for-loop
              (lambda (lst_1)
                (begin
                  (if (pair? lst_1)
                    (let ((cb_0 (unsafe-car lst_1)))
                      (let ((rest_0 (unsafe-cdr lst_1)))
                        (begin (|#%app| cb_0) (for-loop_0 rest_0))))
                    (values)))))))
           (for-loop_0 lst_0))))
      (void)
      (set-thread-kill-callbacks! t_0 null))))
(define check-for-break-after-kill (lambda () (|#%app| 1/check-for-break)))
(define effect_2294
  (begin
    (void
     (let ((proc_0
            (lambda ()
              (let ((t_0 (1/current-thread)))
                (if t_0
                  (begin
                    (if (let ((or-part_0 (1/thread-dead? t_0)))
                          (if or-part_0
                            or-part_0
                            (null? (thread-custodian-references t_0))))
                      (engine-block)
                      (void))
                    (begin-unsafe (|#%app| 1/check-for-break)))
                  (void))))))
       (begin-unsafe (set! post-shutdown-action proc_0))))
    (void)))
(define 1/thread-wait
  (|#%name|
   thread-wait
   (lambda (t_0)
     (begin
       (begin
         (if (1/thread? t_0)
           (void)
           (raise-argument-error 'thread-wait "thread?" t_0))
         (1/semaphore-wait (|#%app| get-thread-dead-sema t_0)))))))
(define struct:dead-evt
  (make-record-type-descriptor*
   'thread-dead-evt
   #f
   (|#%nongenerative-uid| thread-dead-evt)
   #f
   #f
   1
   0))
(define effect_2406
  (struct-type-install-properties!
   struct:dead-evt
   '(thread-dead-evt)
   1
   0
   #f
   (list
    (cons
     1/prop:evt
     (lambda (tde_0)
       (wrap-evt7.1 (dead-evt-sema tde_0) (lambda (s_0) tde_0)))))
   (current-inspector)
   #f
   '(0)
   #f
   'dead-evt))
(define dead-evt13.1
  (|#%name|
   dead-evt
   (record-constructor
    (make-record-constructor-descriptor struct:dead-evt #f #f))))
(define dead-evt?_2047
  (|#%name| thread-dead-evt? (record-predicate struct:dead-evt)))
(define dead-evt?
  (|#%name|
   thread-dead-evt?
   (lambda (v)
     (if (dead-evt?_2047 v)
       #t
       ($value
        (if (impersonator? v) (dead-evt?_2047 (impersonator-val v)) #f))))))
(define dead-evt-sema_2516
  (|#%name| thread-dead-evt-sema (record-accessor struct:dead-evt 0)))
(define dead-evt-sema
  (|#%name|
   thread-dead-evt-sema
   (lambda (s)
     (if (dead-evt?_2047 s)
       (dead-evt-sema_2516 s)
       ($value
        (impersonate-ref
         dead-evt-sema_2516
         struct:dead-evt
         0
         s
         'thread-dead-evt
         'sema))))))
(define thread-dead-evt? (lambda (v_0) (dead-evt? v_0)))
(define get-thread-dead-evt
  (|#%name|
   thread-dead-evt
   (lambda (t_0)
     (begin
       (begin
         (if (1/thread? t_0)
           (void)
           (raise-argument-error 'thread-dead-evt "thread?" t_0))
         (start-atomic)
         (begin0
           (if (1/thread-dead-evt t_0)
             (void)
             (set-thread-dead-evt!
              t_0
              (dead-evt13.1 (get-thread-dead-sema t_0))))
           (end-atomic))
         (1/thread-dead-evt t_0))))))
(define get-thread-dead-sema
  (lambda (t_0)
    (begin
      (start-atomic)
      (begin0
        (if (thread-dead-sema t_0)
          (void)
          (begin
            (set-thread-dead-sema! t_0 (1/make-semaphore 0))
            (if (eq? 'done (thread-engine t_0))
              (semaphore-post-all (thread-dead-sema t_0))
              (void))))
        (end-atomic))
      (thread-dead-sema t_0))))
(define remove-from-sleeping-threads!
  (lambda (t_0)
    (let ((sleeping_0 (thread-sleeping t_0)))
      (if sleeping_0
        (begin
          (set-thread-sleeping! t_0 #f)
          (begin-unsafe
           (|#%app| (sandman-do-remove-thread! the-sandman) t_0 sleeping_0)))
        (void)))))
(define add-to-sleeping-threads!
  (lambda (t_0 ext-events_0)
    (let ((sleeping_0
           (begin-unsafe
            (|#%app| (sandman-do-add-thread! the-sandman) t_0 ext-events_0))))
      (set-thread-sleeping! t_0 sleeping_0))))
(define force-atomic-timeout-callback void)
(define set-force-atomic-timeout-callback!
  (lambda (proc_0) (set! force-atomic-timeout-callback proc_0)))
(define do-thread-deschedule!
  (lambda (t_0 timeout-at_0)
    (begin
      (if (thread-descheduled? t_0)
        (internal-error "tried to deschedule a descheduled thread")
        (void))
      (set-thread-descheduled?! t_0 #t)
      (thread-group-remove! (thread-parent t_0) t_0)
      (|#%app| thread-unscheduled-for-work-tracking! t_0)
      (if timeout-at_0
        (add-to-sleeping-threads!
         t_0
         (begin-unsafe
          (|#%app| (sandman-do-merge-timeout the-sandman) #f timeout-at_0)))
        (void))
      (if (eq? t_0 (current-thread/in-atomic))
        (|#%app| thread-did-work!)
        (void))
      (lambda ()
        (if (eq? t_0 (1/current-thread))
          (begin
            (letrec*
             ((loop_0
               (|#%name|
                loop
                (lambda ()
                  (begin
                    (if (positive? (current-atomic))
                      (if (|#%app| force-atomic-timeout-callback)
                        (loop_0)
                        (internal-error
                         "attempt to deschedule the current thread in atomic mode"))
                      (void)))))))
             (loop_0))
            (engine-block))
          (void))))))
(define thread-deschedule!
  (lambda (t_0 timeout-at_0 interrupt-callback_0)
    (let ((retry-callback_0 #f))
      (begin
        (start-atomic)
        (begin0
          (begin
            (set-thread-interrupt-callback!
             t_0
             (lambda ()
               (set! retry-callback_0 (|#%app| interrupt-callback_0))))
            (let ((finish_0 (do-thread-deschedule! t_0 timeout-at_0)))
              (lambda ()
                (begin
                  (|#%app| finish_0)
                  (if retry-callback_0 (|#%app| retry-callback_0) (void))))))
          (end-atomic))))))
(define thread-reschedule!
  (lambda (t_0)
    (begin
      (if (1/thread-dead? t_0)
        (internal-error "tried to reschedule a dead thread")
        (void))
      (if (thread-descheduled? t_0)
        (void)
        (internal-error "tried to reschedule a scheduled thread"))
      (set-thread-descheduled?! t_0 #f)
      (set-thread-interrupt-callback! t_0 #f)
      (remove-from-sleeping-threads! t_0)
      (thread-group-add! (thread-parent t_0) t_0))))
(define 1/thread-suspend
  (|#%name|
   thread-suspend
   (lambda (t_0)
     (begin
       (begin
         (if (1/thread? t_0)
           (void)
           (raise-argument-error 'thread-suspend "thread?" t_0))
         (|#%app|
          (begin
            (start-atomic)
            (begin0 (do-thread-suspend t_0) (end-atomic)))))))))
(define do-thread-suspend
  (lambda (t_0)
    (if (1/thread-dead? t_0)
      void
      (begin
        (if (thread-suspended? t_0)
          (void)
          (begin
            (set-thread-suspended?! t_0 #t)
            (begin
              (run-interrupt-callback t_0)
              (begin
                (run-suspend/resume-callbacks t_0 car)
                (let ((suspended-evt_0 (thread-suspended-evt t_0)))
                  (if suspended-evt_0
                    (begin
                      (set-suspend-resume-evt-thread! suspended-evt_0 t_0)
                      (semaphore-post-all
                       (suspend-resume-evt-sema suspended-evt_0))
                      (set-thread-suspended-evt! t_0 #f))
                    (void)))))))
        (if (not (thread-descheduled? t_0))
          (do-thread-deschedule! t_0 #f)
          (begin (remove-from-sleeping-threads! t_0) void))))))
(define 1/thread-resume
  (let ((thread-resume_0
         (|#%name|
          thread-resume
          (lambda (t15_0 benefactor14_0)
            (begin
              (begin
                (if (1/thread? t15_0)
                  (void)
                  (raise-argument-error 'thread-resume "thread?" t15_0))
                (if (let ((or-part_0 (not benefactor14_0)))
                      (if or-part_0
                        or-part_0
                        (let ((or-part_1 (1/thread? benefactor14_0)))
                          (if or-part_1
                            or-part_1
                            (1/custodian? benefactor14_0)))))
                  (void)
                  (raise-argument-error
                   'thread-resume
                   "(or/c #f thread? custodian?)"
                   benefactor14_0))
                (if (if (1/custodian? benefactor14_0)
                      (1/custodian-shut-down? benefactor14_0)
                      #f)
                  (begin-unsafe
                   (raise-arguments-error
                    'thread-resume
                    "the custodian has been shut down"
                    "custodian"
                    benefactor14_0))
                  (void))
                (start-atomic)
                (begin0
                  (do-thread-resume t15_0 benefactor14_0)
                  (end-atomic))))))))
    (|#%name|
     thread-resume
     (case-lambda
      ((t_0) (begin (thread-resume_0 t_0 #f)))
      ((t_0 benefactor14_0) (thread-resume_0 t_0 benefactor14_0))))))
(define do-thread-resume
  (lambda (t_0 benefactor_0)
    (if (1/thread-dead? t_0)
      (void)
      (begin
        (if (1/thread? benefactor_0)
          (begin
            (let ((lst_0 (thread-custodian-references benefactor_0)))
              (begin
                (letrec*
                 ((for-loop_0
                   (|#%name|
                    for-loop
                    (lambda (lst_1)
                      (begin
                        (if (pair? lst_1)
                          (let ((cr_0 (unsafe-car lst_1)))
                            (let ((rest_0 (unsafe-cdr lst_1)))
                              (begin
                                (add-custodian-to-thread!
                                 t_0
                                 (custodian-reference->custodian cr_0))
                                (for-loop_0 rest_0))))
                          (values)))))))
                 (for-loop_0 lst_0))))
            (void)
            (add-transitive-resume-to-thread! benefactor_0 t_0))
          (if (1/custodian? benefactor_0)
            (add-custodian-to-thread! t_0 benefactor_0)
            (void)))
        (if (if (thread-suspended? t_0)
              (pair? (thread-custodian-references t_0))
              #f)
          (let ((resumed-evt_0 (thread-resumed-evt t_0)))
            (begin
              (if resumed-evt_0
                (begin
                  (set-suspend-resume-evt-thread! resumed-evt_0 t_0)
                  (semaphore-post-all (suspend-resume-evt-sema resumed-evt_0))
                  (set-thread-resumed-evt! t_0 #f))
                (void))
              (set-thread-suspended?! t_0 #f)
              (run-suspend/resume-callbacks t_0 cdr)
              (thread-reschedule! t_0)
              (do-resume-transitive-resumes t_0 #f)))
          (void))))))
(define add-custodian-to-thread!
  (lambda (t_0 c_0)
    (letrec*
     ((loop_0
       (|#%name|
        loop
        (lambda (crs_0 accum_0)
          (begin
            (if (null? crs_0)
              (let ((new-crs_0
                     (cons
                      (1/unsafe-custodian-register
                       c_0
                       t_0
                       remove-thread-custodian
                       #f
                       #t)
                      accum_0)))
                (begin
                  (set-thread-custodian-references! t_0 new-crs_0)
                  (do-resume-transitive-resumes t_0 c_0)))
              (let ((old-c_0 (custodian-reference->custodian (car crs_0))))
                (if (let ((or-part_0 (eq? c_0 old-c_0)))
                      (if or-part_0
                        or-part_0
                        (custodian-subordinate? c_0 old-c_0)))
                  (void)
                  (if (custodian-subordinate? old-c_0 c_0)
                    (loop_0 (cdr crs_0) accum_0)
                    (let ((app_0 (cdr crs_0)))
                      (loop_0 app_0 (cons (car crs_0) accum_0))))))))))))
     (loop_0 (thread-custodian-references t_0) null))))
(define struct:transitive-resume
  (make-record-type-descriptor*
   'transitive-resume
   #f
   (|#%nongenerative-uid| transitive-resume)
   #f
   #f
   2
   0))
(define effect_2379
  (struct-type-install-properties!
   struct:transitive-resume
   '(transitive-resume)
   2
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(0 1)
   #f
   'transitive-resume))
(define transitive-resume16.1
  (|#%name|
   transitive-resume
   (record-constructor
    (make-record-constructor-descriptor struct:transitive-resume #f #f))))
(define transitive-resume?
  (|#%name| transitive-resume? (record-predicate struct:transitive-resume)))
(define transitive-resume-weak-box
  (|#%name|
   transitive-resume-weak-box
   (record-accessor struct:transitive-resume 0)))
(define transitive-resume-box
  (|#%name|
   transitive-resume-box
   (record-accessor struct:transitive-resume 1)))
(define add-transitive-resume-to-thread!
  (lambda (t_0 b-t_0)
    (let ((new-l_0
           (letrec*
            ((loop_0
              (|#%name|
               loop
               (lambda (l_0)
                 (begin
                   (if (null? l_0)
                     (begin
                       (set-thread-suspended?! b-t_0 (thread-suspended? b-t_0))
                       (list
                        (let ((app_0 (make-weak-box b-t_0)))
                          (transitive-resume16.1
                           app_0
                           (thread-suspended-box b-t_0)))))
                     (let ((o-t_0
                            (weak-box-value
                             (transitive-resume-weak-box (car l_0)))))
                       (if (not o-t_0)
                         (loop_0 (cdr l_0))
                         (if (1/thread-dead? o-t_0)
                           (loop_0 (cdr l_0))
                           (if (eq? b-t_0 o-t_0)
                             l_0
                             (let ((app_0 (car l_0)))
                               (cons app_0 (loop_0 (cdr l_0))))))))))))))
            (loop_0 (thread-transitive-resumes t_0)))))
      (set-thread-transitive-resumes! t_0 new-l_0))))
(define do-resume-transitive-resumes
  (lambda (t_0 c_0)
    (begin
      (let ((lst_0 (thread-transitive-resumes t_0)))
        (begin
          (letrec*
           ((for-loop_0
             (|#%name|
              for-loop
              (lambda (lst_1)
                (begin
                  (if (pair? lst_1)
                    (let ((tr_0 (unsafe-car lst_1)))
                      (let ((rest_0 (unsafe-cdr lst_1)))
                        (begin
                          (let ((b-t_0
                                 (weak-box-value
                                  (transitive-resume-weak-box tr_0))))
                            (if b-t_0 (do-thread-resume b-t_0 c_0) (void)))
                          (for-loop_0 rest_0))))
                    (values)))))))
           (for-loop_0 lst_0))))
      (void))))
(define thread-push-suspend+resume-callbacks!
  (lambda (s-cb_0 r-cb_0)
    (let ((t_0 (current-thread/in-atomic)))
      (set-thread-suspend+resume-callbacks!
       t_0
       (let ((app_0 (cons s-cb_0 r-cb_0)))
         (cons app_0 (thread-suspend+resume-callbacks t_0)))))))
(define thread-pop-suspend+resume-callbacks!
  (lambda ()
    (let ((t_0 (current-thread/in-atomic)))
      (set-thread-suspend+resume-callbacks!
       t_0
       (cdr (thread-suspend+resume-callbacks t_0))))))
(define run-suspend/resume-callbacks
  (lambda (t_0 sel_0)
    (begin
      (let ((lst_0 (thread-suspend+resume-callbacks t_0)))
        (begin
          (letrec*
           ((for-loop_0
             (|#%name|
              for-loop
              (lambda (lst_1)
                (begin
                  (if (pair? lst_1)
                    (let ((cbs_0 (unsafe-car lst_1)))
                      (let ((rest_0 (unsafe-cdr lst_1)))
                        (begin
                          (|#%app| (|#%app| sel_0 cbs_0))
                          (for-loop_0 rest_0))))
                    (values)))))))
           (for-loop_0 lst_0))))
      (void))))
(define run-interrupt-callback
  (lambda (t_0)
    (let ((interrupt-callback_0 (thread-interrupt-callback t_0)))
      (if interrupt-callback_0
        (begin
          (set-thread-interrupt-callback! t_0 #f)
          (|#%app| interrupt-callback_0))
        (void)))))
(define struct:suspend-resume-evt
  (make-record-type-descriptor*
   'suspend-resume-evt
   #f
   (|#%nongenerative-uid| suspend-resume-evt)
   #f
   #f
   2
   2))
(define effect_2856
  (struct-type-install-properties!
   struct:suspend-resume-evt
   '(suspend-resume-evt)
   2
   0
   #f
   (list
    (cons
     1/prop:evt
     (lambda (se_0)
       (wrap-evt7.1
        (suspend-resume-evt-sema se_0)
        (lambda (s_0) (suspend-resume-evt-thread se_0))))))
   (current-inspector)
   #f
   '(0)
   #f
   'suspend-resume-evt))
(define suspend-resume-evt17.1
  (|#%name|
   suspend-resume-evt
   (record-constructor
    (make-record-constructor-descriptor struct:suspend-resume-evt #f #f))))
(define suspend-resume-evt?_2231
  (|#%name| suspend-resume-evt? (record-predicate struct:suspend-resume-evt)))
(define suspend-resume-evt?
  (|#%name|
   suspend-resume-evt?
   (lambda (v)
     (if (suspend-resume-evt?_2231 v)
       #t
       ($value
        (if (impersonator? v)
          (suspend-resume-evt?_2231 (impersonator-val v))
          #f))))))
(define suspend-resume-evt-sema_2220
  (|#%name|
   suspend-resume-evt-sema
   (record-accessor struct:suspend-resume-evt 0)))
(define suspend-resume-evt-sema
  (|#%name|
   suspend-resume-evt-sema
   (lambda (s)
     (if (suspend-resume-evt?_2231 s)
       (suspend-resume-evt-sema_2220 s)
       ($value
        (impersonate-ref
         suspend-resume-evt-sema_2220
         struct:suspend-resume-evt
         0
         s
         'suspend-resume-evt
         'sema))))))
(define suspend-resume-evt-thread_2163
  (|#%name|
   suspend-resume-evt-thread
   (record-accessor struct:suspend-resume-evt 1)))
(define suspend-resume-evt-thread
  (|#%name|
   suspend-resume-evt-thread
   (lambda (s)
     (if (suspend-resume-evt?_2231 s)
       (suspend-resume-evt-thread_2163 s)
       ($value
        (impersonate-ref
         suspend-resume-evt-thread_2163
         struct:suspend-resume-evt
         1
         s
         'suspend-resume-evt
         'thread))))))
(define set-suspend-resume-evt-thread!_3086
  (|#%name|
   set-suspend-resume-evt-thread!
   (record-mutator struct:suspend-resume-evt 1)))
(define set-suspend-resume-evt-thread!
  (|#%name|
   set-suspend-resume-evt-thread!
   (lambda (s v)
     (if (suspend-resume-evt?_2231 s)
       (set-suspend-resume-evt-thread!_3086 s v)
       ($value
        (impersonate-set!
         set-suspend-resume-evt-thread!_3086
         struct:suspend-resume-evt
         1
         1
         s
         v
         'suspend-resume-evt
         'thread))))))
(define struct:suspend-evt
  (make-record-type-descriptor*
   'thread-suspend-evt
   struct:suspend-resume-evt
   (|#%nongenerative-uid| thread-suspend-evt)
   #f
   #f
   0
   0))
(define effect_2484
  (struct-type-install-properties!
   struct:suspend-evt
   '(thread-suspend-evt)
   0
   0
   struct:suspend-resume-evt
   null
   (current-inspector)
   #f
   '()
   #f
   'suspend-evt))
(define suspend-evt18.1
  (|#%name|
   suspend-evt
   (record-constructor
    (make-record-constructor-descriptor struct:suspend-evt #f #f))))
(define suspend-evt?_3224
  (|#%name| thread-suspend-evt? (record-predicate struct:suspend-evt)))
(define suspend-evt?
  (|#%name|
   thread-suspend-evt?
   (lambda (v)
     (if (suspend-evt?_3224 v)
       #t
       ($value
        (if (impersonator? v) (suspend-evt?_3224 (impersonator-val v)) #f))))))
(define struct:resume-evt
  (make-record-type-descriptor*
   'thread-resume-evt
   struct:suspend-resume-evt
   (|#%nongenerative-uid| thread-resume-evt)
   #f
   #f
   0
   0))
(define effect_2390
  (struct-type-install-properties!
   struct:resume-evt
   '(thread-resume-evt)
   0
   0
   struct:suspend-resume-evt
   null
   (current-inspector)
   #f
   '()
   #f
   'resume-evt))
(define resume-evt19.1
  (|#%name|
   resume-evt
   (record-constructor
    (make-record-constructor-descriptor struct:resume-evt #f #f))))
(define resume-evt?_2037
  (|#%name| thread-resume-evt? (record-predicate struct:resume-evt)))
(define resume-evt?
  (|#%name|
   thread-resume-evt?
   (lambda (v)
     (if (resume-evt?_2037 v)
       #t
       ($value
        (if (impersonator? v) (resume-evt?_2037 (impersonator-val v)) #f))))))
(define 1/thread-resume-evt
  (|#%name|
   thread-resume-evt
   (lambda (t_0)
     (begin
       (begin
         (if (1/thread? t_0)
           (void)
           (raise-argument-error 'thread-resume-evt "thread?" t_0))
         (start-atomic)
         (begin0
           (if (1/thread-dead? t_0)
             (resume-evt19.1 the-never-evt #f)
             (if (thread-suspended? t_0)
               (let ((or-part_0 (thread-resumed-evt t_0)))
                 (if or-part_0
                   or-part_0
                   (let ((r_0 (resume-evt19.1 (1/make-semaphore) #f)))
                     (begin (set-thread-resumed-evt! t_0 r_0) r_0))))
               (resume-evt19.1 the-always-evt t_0)))
           (end-atomic)))))))
(define 1/thread-suspend-evt
  (|#%name|
   thread-suspend-evt
   (lambda (t_0)
     (begin
       (begin
         (if (1/thread? t_0)
           (void)
           (raise-argument-error 'thread-suspend-evt "thread?" t_0))
         (start-atomic)
         (begin0
           (if (1/thread-dead? t_0)
             (suspend-evt18.1 the-never-evt #f)
             (if (thread-suspended? t_0)
               (suspend-evt18.1 the-always-evt t_0)
               (let ((or-part_0 (thread-suspended-evt t_0)))
                 (if or-part_0
                   or-part_0
                   (let ((s_0 (suspend-evt18.1 (1/make-semaphore) #f)))
                     (begin (set-thread-suspended-evt! t_0 s_0) s_0))))))
           (end-atomic)))))))
(define thread-yield
  (lambda (sched-info_0)
    (begin
      (start-atomic)
      (begin0
        (begin
          (if (let ((or-part_0 (not sched-info_0)))
                (if or-part_0
                  or-part_0
                  (schedule-info-did-work? sched-info_0)))
            (|#%app| thread-did-work!)
            (thread-did-no-work!))
          (set-thread-sched-info! (current-thread/in-atomic) sched-info_0))
        (end-atomic))
      (engine-block))))
(define 1/sleep
  (let ((sleep_0
         (|#%name|
          sleep
          (lambda (secs20_0)
            (begin
              (begin
                (if (if (real? secs20_0) (>= secs20_0 0) #f)
                  (void)
                  (raise-argument-error 'sleep "(>=/c 0)" secs20_0))
                (if (if (zero? secs20_0) (zero? (current-atomic)) #f)
                  (thread-yield #f)
                  (let ((until-msecs_0
                         (let ((app_0 (* secs20_0 1000.0)))
                           (+ app_0 (current-inexact-milliseconds)))))
                    (letrec*
                     ((loop_0
                       (|#%name|
                        loop
                        (lambda ()
                          (begin
                            (|#%app|
                             (thread-deschedule!
                              (1/current-thread)
                              until-msecs_0
                              (lambda () (lambda () (loop_0))))))))))
                     (loop_0))))))))))
    (|#%name|
     sleep
     (case-lambda (() (begin (sleep_0 0))) ((secs20_0) (sleep_0 secs20_0))))))
(define cell.2$1 (unsafe-make-place-local hash2610))
(define thread-did-no-work!
  (lambda ()
    (unsafe-place-local-set!
     cell.2$1
     (hash-set (unsafe-place-local-ref cell.2$1) (1/current-thread) #t))))
(define thread-did-work!
  (lambda () (unsafe-place-local-set! cell.2$1 hash2610)))
(define thread-unscheduled-for-work-tracking!
  (lambda (t_0)
    (unsafe-place-local-set!
     cell.2$1
     (hash-remove (unsafe-place-local-ref cell.2$1) t_0))))
(define break-enabled-default-cell (make-thread-cell #t))
(define current-breakable-atomic (make-pthread-parameter 0))
(define current-break-enabled-cell
  (lambda ()
    (continuation-mark-set-first
     #f
     break-enabled-key
     break-enabled-default-cell
     (unsafe-root-continuation-prompt-tag))))
(define 1/break-enabled
  (|#%name|
   break-enabled
   (case-lambda
    (() (begin (thread-cell-ref (current-break-enabled-cell))))
    ((on?_0)
     (begin
       (thread-cell-set! (current-break-enabled-cell) on?_0)
       (if on?_0 (1/check-for-break) (void)))))))
(define 1/check-for-break
  (|#%name|
   check-for-break
   (lambda ()
     (begin
       (if (current-future$1)
         (void)
         (let ((t_0 (1/current-thread)))
           (if (if t_0 (thread-pending-break t_0) #f)
             (|#%app|
              (begin
                (start-atomic)
                (begin0
                  (if (if (thread-pending-break t_0)
                        (if (let ((app_0 (add1 (current-breakable-atomic))))
                              (>= app_0 (current-atomic)))
                          (if (1/break-enabled)
                            (not
                             (let ((app_0 thread-ignore-break-cell?))
                               (|#%app|
                                app_0
                                t_0
                                (current-break-enabled-cell))))
                            #f)
                          #f)
                        #f)
                    (let ((exn:break*_0
                           (let ((tmp_0 (thread-pending-break t_0)))
                             (if (eq? tmp_0 'hang-up)
                               exn:break:hang-up/non-engine
                               (if (eq? tmp_0 'terminate)
                                 exn:break:terminate/non-engine
                                 exn:break/non-engine)))))
                      (begin
                        (set-thread-pending-break! t_0 #f)
                        (lambda ()
                          (call-with-escape-continuation
                           (lambda (k_0)
                             (raise
                              (|#%app|
                               exn:break*_0
                               "user break"
                               (current-continuation-marks)
                               k_0)))))))
                    void)
                  (end-atomic))))
             (void))))))))
(define effect_2492
  (begin
    (void (|#%app| set-break-enabled-transition-hook! 1/check-for-break))
    (void)))
(define 1/break-thread
  (let ((break-thread_0
         (|#%name|
          break-thread
          (lambda (t22_0 kind21_0)
            (begin
              (begin
                (if (1/thread? t22_0)
                  (void)
                  (raise-argument-error 'break-thread "thread?" t22_0))
                (if (let ((or-part_0 (not kind21_0)))
                      (if or-part_0
                        or-part_0
                        (let ((or-part_1 (eq? kind21_0 'hang-up)))
                          (if or-part_1 or-part_1 (eq? kind21_0 'terminate)))))
                  (void)
                  (raise-argument-error
                   'break-thread
                   "(or/c #f 'hang-up 'terminate)"
                   kind21_0))
                (let ((app_0 (if kind21_0 kind21_0 'break)))
                  (do-break-thread t22_0 app_0 (1/current-thread)))))))))
    (|#%name|
     break-thread
     (case-lambda
      ((t_0) (begin (break-thread_0 t_0 #f)))
      ((t_0 kind21_0) (break-thread_0 t_0 kind21_0))))))
(define do-break-thread
  (lambda (t_0 kind_0 check-t_0)
    (begin
      (|#%app|
       (begin
         (start-atomic)
         (begin0
           (let ((c1_0 (thread-forward-break-to t_0)))
             (if c1_0
               (lambda () (do-break-thread c1_0 kind_0 check-t_0))
               (begin
                 (if (if (thread-pending-break t_0)
                       (break>? kind_0 (thread-pending-break t_0))
                       #f)
                   (set-thread-pending-break! t_0 kind_0)
                   (void))
                 (if (thread-pending-break t_0)
                   (void)
                   (begin
                     (set-thread-pending-break! t_0 kind_0)
                     (thread-did-work!)
                     (run-suspend/resume-callbacks t_0 car)
                     (run-suspend/resume-callbacks t_0 cdr)
                     (if (thread-descheduled? t_0)
                       (if (thread-suspended? t_0)
                         (void)
                         (begin
                           (run-interrupt-callback t_0)
                           (thread-reschedule! t_0)))
                       (void))))
                 void)))
           (end-atomic))))
      (if (eq? t_0 check-t_0)
        (begin
          (1/check-for-break)
          (if (in-atomic-mode?)
            (add-end-atomic-callback! 1/check-for-break)
            (void)))
        (void)))))
(define break>?
  (lambda (k1_0 k2_0)
    (if (eq? k1_0 'break)
      #f
      (if (eq? k1_0 'hang-up) (eq? k2_0 'break) (not (eq? k2_0 'terminate))))))
(define break-max
  (lambda (k1_0 k2_0)
    (if (not (if k1_0 k2_0 #f))
      (if k1_0 k1_0 k2_0)
      (if (break>? k1_0 k2_0) k1_0 k2_0))))
(define effect_2939
  (begin
    (void
     (|#%app|
      set-ctl-c-handler!
      (lambda (kind_0)
        (do-break-thread (unsafe-place-local-ref cell.1$1) kind_0 #f))))
    (void)))
(define thread-ignore-break-cell?
  (lambda (t_0 bc_0)
    (let ((ignore_0 (thread-ignore-break-cells t_0)))
      (let ((or-part_0 (eq? ignore_0 bc_0)))
        (if or-part_0
          or-part_0
          (if (hash? ignore_0) (hash-ref ignore_0 bc_0 #f) #f))))))
(define thread-ignore-break-cell!
  (lambda (t_0 bc_0)
    (let ((ignore_0 (thread-ignore-break-cells t_0)))
      (set-thread-ignore-break-cells!
       t_0
       (if (not ignore_0)
         bc_0
         (if (hash? ignore_0)
           (hash-set ignore_0 bc_0 #t)
           (hasheq ignore_0 #t bc_0 #t)))))))
(define thread-remove-ignored-break-cell!
  (lambda (t_0 bc_0)
    (begin
      (start-atomic)
      (if (thread-ignore-break-cell? t_0 bc_0)
        (let ((ignore_0 (thread-ignore-break-cells t_0)))
          (set-thread-ignore-break-cells!
           t_0
           (if (eq? ignore_0 bc_0) #f (hash-remove ignore_0 bc_0))))
        (void))
      (end-atomic))))
(define enqueue-mail!
  (lambda (thd_0 v_0) (queue-add! (thread-mailbox thd_0) v_0)))
(define dequeue-mail!
  (lambda (thd_0)
    (let ((mbx_0 (thread-mailbox thd_0)))
      (if (begin-unsafe (not (queue-start mbx_0)))
        (internal-error "No Mail!\n")
        (queue-remove! mbx_0)))))
(define is-mail?
  (lambda (thd_0)
    (not
     (let ((q_0 (thread-mailbox thd_0)))
       (begin-unsafe (not (queue-start q_0)))))))
(define push-mail!
  (lambda (thd_0 v_0) (queue-add-front! (thread-mailbox thd_0) v_0)))
(define 1/thread-send
  (let ((thread-send_0
         (|#%name|
          thread-send
          (lambda (thd24_0 v25_0 fail-thunk23_0)
            (begin
              (let ((fail-thunk_0
                     (if (eq? fail-thunk23_0 unsafe-undefined)
                       (|#%name|
                        fail-thunk
                        (lambda ()
                          (begin
                            (raise-arguments-error
                             'thread-send
                             "target thread is not running"))))
                       fail-thunk23_0)))
                (begin
                  (if (1/thread? thd24_0)
                    (void)
                    (raise-argument-error 'thread-send "thread?" thd24_0))
                  (if (let ((or-part_0 (not fail-thunk_0)))
                        (if or-part_0
                          or-part_0
                          (if (procedure? fail-thunk_0)
                            (procedure-arity-includes? fail-thunk_0 0)
                            #f)))
                    (void)
                    (raise-argument-error
                     'thread-send
                     "(or/c (procedure-arity-includes/c 0) #f)"
                     fail-thunk_0))
                  (|#%app|
                   (begin
                     (start-atomic)
                     (begin0
                       (if (not (1/thread-dead? thd24_0))
                         (begin
                           (begin-unsafe
                            (queue-add! (thread-mailbox thd24_0) v25_0))
                           (let ((wakeup_0 (thread-mailbox-wakeup thd24_0)))
                             (begin
                               (set-thread-mailbox-wakeup! thd24_0 void)
                               (|#%app| wakeup_0)
                               void)))
                         (if fail-thunk_0 fail-thunk_0 (lambda () #f)))
                       (end-atomic)))))))))))
    (|#%name|
     thread-send
     (case-lambda
      ((thd_0 v_0) (begin (thread-send_0 thd_0 v_0 unsafe-undefined)))
      ((thd_0 v_0 fail-thunk23_0) (thread-send_0 thd_0 v_0 fail-thunk23_0))))))
(define 1/thread-receive
  (|#%name|
   thread-receive
   (lambda ()
     (begin
       (|#%app|
        (begin
          (start-atomic)
          (begin0
            (let ((t_0 (current-thread/in-atomic)))
              (if (is-mail? t_0)
                (let ((v_0 (dequeue-mail! t_0))) (lambda () v_0))
                (begin
                  (set-thread-mailbox-wakeup!
                   t_0
                   (lambda () (thread-reschedule! t_0)))
                  (let ((do-yield_0
                         (thread-deschedule!
                          t_0
                          #f
                          (lambda ()
                            (begin
                              (set-thread-mailbox-wakeup! t_0 void)
                              void)))))
                    (lambda ()
                      (begin (|#%app| do-yield_0) (1/thread-receive)))))))
            (end-atomic))))))))
(define 1/thread-try-receive
  (|#%name|
   thread-try-receive
   (lambda ()
     (begin
       (begin
         (start-atomic)
         (begin0
           (let ((t_0 (current-thread/in-atomic)))
             (if (is-mail? t_0) (dequeue-mail! t_0) #f))
           (end-atomic)))))))
(define 1/thread-rewind-receive
  (|#%name|
   thread-rewind-receive
   (lambda (lst_0)
     (begin
       (begin
         (if (list? lst_0)
           (void)
           (raise-argument-error 'thread-rewind-receive "list?" lst_0))
         (start-atomic)
         (begin0
           (let ((t_0 (current-thread/in-atomic)))
             (for-each_2380
              (lambda (msg_0)
                (begin-unsafe (queue-add-front! (thread-mailbox t_0) msg_0)))
              lst_0))
           (end-atomic)))))))
(define struct:thread-receiver-evt
  (make-record-type-descriptor*
   'thread-receive-evt
   #f
   (|#%nongenerative-uid| thread-receive-evt)
   #f
   #f
   0
   0))
(define effect_2597
  (struct-type-install-properties!
   struct:thread-receiver-evt
   '(thread-receive-evt)
   0
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1
      (lambda (self_0 poll-ctx_0)
        (let ((t_0 (current-thread/in-atomic)))
          (if (is-mail? t_0)
            (values (list self_0) #f)
            (if (poll-ctx-poll? poll-ctx_0)
              (values #f self_0)
              (let ((select-proc_0 (poll-ctx-select-proc poll-ctx_0)))
                (let ((receive_0
                       (|#%name|
                        receive
                        (lambda ()
                          (begin
                            (if (is-mail? t_0)
                              (|#%app| select-proc_0)
                              (void)))))))
                  (let ((add-wakeup-callback!_0
                         (|#%name|
                          add-wakeup-callback!
                          (lambda ()
                            (begin
                              (let ((wakeup_0 (thread-mailbox-wakeup t_0)))
                                (set-thread-mailbox-wakeup!
                                 t_0
                                 (lambda ()
                                   (begin
                                     (|#%app| wakeup_0)
                                     (|#%app| receive_0))))))))))
                    (begin
                      (add-wakeup-callback!_0)
                      (values
                       #f
                       (control-state-evt9.1
                        the-async-evt
                        (lambda (v_0) self_0)
                        (lambda () (set-thread-mailbox-wakeup! t_0 void))
                        (lambda () (set! receive_0 void))
                        (lambda () (add-wakeup-callback!_0)))))))))))))))
   (current-inspector)
   #f
   '()
   #f
   'thread-receiver-evt))
(define thread-receiver-evt26.1
  (|#%name|
   thread-receiver-evt
   (record-constructor
    (make-record-constructor-descriptor struct:thread-receiver-evt #f #f))))
(define thread-receiver-evt?_2591
  (|#%name| thread-receive-evt? (record-predicate struct:thread-receiver-evt)))
(define thread-receiver-evt?
  (|#%name|
   thread-receive-evt?
   (lambda (v)
     (if (thread-receiver-evt?_2591 v)
       #t
       ($value
        (if (impersonator? v)
          (thread-receiver-evt?_2591 (impersonator-val v))
          #f))))))
(define 1/thread-receive-evt
  (|#%name| thread-receive-evt (lambda () (begin (thread-receiver-evt26.1)))))
(define effect_2328
  (begin
    (void
     (|#%app|
      set-immediate-allocation-check-proc!
      (lambda (n_0)
        (let ((t_0 (1/current-thread)))
          (if t_0
            (let ((mrefs_0 (thread-custodian-references t_0)))
              (if (null? mrefs_0)
                (void)
                (custodian-check-immediate-limit (car mrefs_0) n_0)))
            (void))))))
    (void)))
(define effect_2829
  (begin
    (void
     (let ((thread-engine_0
            (lambda (v_0)
              (if (1/thread? v_0)
                (let ((e_0 (thread-engine v_0)))
                  (if (not (eq? e_0 'done))
                    (if (not (eq? e_0 'running)) e_0 #f)
                    #f))
                #f))))
       (begin-unsafe (set! thread-engine-for-roots thread-engine_0))))
    (void)))
(define struct:channel
  (make-record-type-descriptor*
   'channel
   #f
   (|#%nongenerative-uid| channel)
   #f
   #f
   2
   0))
(define effect_2021
  (struct-type-install-properties!
   struct:channel
   '(channel)
   2
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1
      (lambda (ch_0 poll-ctx_0) (channel-get/poll ch_0 poll-ctx_0)))))
   (current-inspector)
   #f
   '(0 1)
   #f
   'channel))
(define channel1.1
  (|#%name|
   channel
   (record-constructor
    (make-record-constructor-descriptor struct:channel #f #f))))
(define 1/channel?_2784 (|#%name| channel? (record-predicate struct:channel)))
(define 1/channel?
  (|#%name|
   channel?
   (lambda (v)
     (if (1/channel?_2784 v)
       #t
       ($value
        (if (impersonator? v) (1/channel?_2784 (impersonator-val v)) #f))))))
(define channel-get-queue_2731
  (|#%name| channel-get-queue (record-accessor struct:channel 0)))
(define channel-get-queue
  (|#%name|
   channel-get-queue
   (lambda (s)
     (if (1/channel?_2784 s)
       (channel-get-queue_2731 s)
       ($value
        (impersonate-ref
         channel-get-queue_2731
         struct:channel
         0
         s
         'channel
         'get-queue))))))
(define channel-put-queue_2027
  (|#%name| channel-put-queue (record-accessor struct:channel 1)))
(define channel-put-queue
  (|#%name|
   channel-put-queue
   (lambda (s)
     (if (1/channel?_2784 s)
       (channel-put-queue_2027 s)
       ($value
        (impersonate-ref
         channel-put-queue_2027
         struct:channel
         1
         s
         'channel
         'put-queue))))))
(define struct:channel-put-evt*
  (make-record-type-descriptor*
   'channel-put-evt
   #f
   (|#%nongenerative-uid| channel-put-evt)
   #f
   #f
   2
   0))
(define effect_2566
  (struct-type-install-properties!
   struct:channel-put-evt*
   '(channel-put-evt)
   2
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1
      (lambda (cp_0 poll-ctx_0)
        (let ((app_0 (channel-put-evt*-ch cp_0)))
          (channel-put/poll
           app_0
           (channel-put-evt*-v cp_0)
           cp_0
           poll-ctx_0))))))
   (current-inspector)
   #f
   '(0 1)
   #f
   'channel-put-evt*))
(define channel-put-evt*2.1
  (|#%name|
   channel-put-evt*
   (record-constructor
    (make-record-constructor-descriptor struct:channel-put-evt* #f #f))))
(define channel-put-evt*?_2795
  (|#%name| channel-put-evt? (record-predicate struct:channel-put-evt*)))
(define channel-put-evt*?
  (|#%name|
   channel-put-evt?
   (lambda (v)
     (if (channel-put-evt*?_2795 v)
       #t
       ($value
        (if (impersonator? v)
          (channel-put-evt*?_2795 (impersonator-val v))
          #f))))))
(define channel-put-evt*-ch_2175
  (|#%name| channel-put-evt-ch (record-accessor struct:channel-put-evt* 0)))
(define channel-put-evt*-ch
  (|#%name|
   channel-put-evt-ch
   (lambda (s)
     (if (channel-put-evt*?_2795 s)
       (channel-put-evt*-ch_2175 s)
       ($value
        (impersonate-ref
         channel-put-evt*-ch_2175
         struct:channel-put-evt*
         0
         s
         'channel-put-evt
         'ch))))))
(define channel-put-evt*-v_2260
  (|#%name| channel-put-evt-v (record-accessor struct:channel-put-evt* 1)))
(define channel-put-evt*-v
  (|#%name|
   channel-put-evt-v
   (lambda (s)
     (if (channel-put-evt*?_2795 s)
       (channel-put-evt*-v_2260 s)
       ($value
        (impersonate-ref
         channel-put-evt*-v_2260
         struct:channel-put-evt*
         1
         s
         'channel-put-evt
         'v))))))
(define struct:channel-select-waiter
  (make-record-type-descriptor*
   'channel-select-waiter
   struct:select-waiter
   (|#%nongenerative-uid| channel-select-waiter)
   #f
   #f
   1
   0))
(define effect_2402
  (struct-type-install-properties!
   struct:channel-select-waiter
   '(channel-select-waiter)
   1
   0
   struct:select-waiter
   null
   (current-inspector)
   #f
   '(0)
   #f
   'channel-select-waiter))
(define channel-select-waiter3.1
  (|#%name|
   channel-select-waiter
   (record-constructor
    (make-record-constructor-descriptor struct:channel-select-waiter #f #f))))
(define channel-select-waiter?_2334
  (|#%name|
   channel-select-waiter?
   (record-predicate struct:channel-select-waiter)))
(define channel-select-waiter?
  (|#%name|
   channel-select-waiter?
   (lambda (v)
     (if (channel-select-waiter?_2334 v)
       #t
       ($value
        (if (impersonator? v)
          (channel-select-waiter?_2334 (impersonator-val v))
          #f))))))
(define channel-select-waiter-thread_2342
  (|#%name|
   channel-select-waiter-thread
   (record-accessor struct:channel-select-waiter 0)))
(define channel-select-waiter-thread
  (|#%name|
   channel-select-waiter-thread
   (lambda (s)
     (if (channel-select-waiter?_2334 s)
       (channel-select-waiter-thread_2342 s)
       ($value
        (impersonate-ref
         channel-select-waiter-thread_2342
         struct:channel-select-waiter
         0
         s
         'channel-select-waiter
         'thread))))))
(define 1/make-channel
  (|#%name|
   make-channel
   (lambda ()
     (begin (let ((app_0 (make-queue))) (channel1.1 app_0 (make-queue)))))))
(define channel-get
  (lambda (ch_0)
    (begin
      (if (1/channel? ch_0)
        (void)
        (raise-argument-error 'channel-get "channel?" ch_0))
      (if (|#%app| evt-impersonator? ch_0)
        (|#%app| sync-on-channel ch_0)
        (let ((b_0 (box #f)))
          (begin
            (letrec*
             ((receive_0
               (|#%name|
                receive
                (lambda ()
                  (begin
                    (|#%app|
                     (begin
                       (start-atomic)
                       (begin0
                         (let ((pw+v_0
                                (queue-remove! (channel-put-queue ch_0))))
                           (let ((gw_0 (current-thread/in-atomic)))
                             (if (not pw+v_0)
                               (let ((gq_0 (channel-get-queue ch_0)))
                                 (let ((n_0 (queue-add! gq_0 (cons gw_0 b_0))))
                                   (let ((interrupt-cb_0
                                          (lambda ()
                                            (begin
                                              (queue-remove-node! gq_0 n_0)
                                              (lambda () (receive_0))))))
                                     (begin-unsafe
                                      (|#%app|
                                       (waiter-methods-suspend
                                        (waiter-ref gw_0))
                                       gw_0
                                       interrupt-cb_0)))))
                               (begin
                                 (set-box! b_0 (cdr pw+v_0))
                                 (let ((w_0 (car pw+v_0)))
                                   (begin-unsafe
                                    (|#%app|
                                     (waiter-methods-resume (waiter-ref w_0))
                                     w_0
                                     (void))))
                                 void))))
                         (end-atomic)))))))))
             (receive_0))
            (unbox b_0)))))))
(define channel-get/poll
  (lambda (ch_0 poll-ctx_0)
    (let ((pq_0 (channel-put-queue ch_0)))
      (let ((pw+v_0 (queue-fremove! pq_0 not-matching-select-waiter)))
        (if pw+v_0
          (begin
            (let ((w_0 (car pw+v_0)))
              (begin-unsafe
               (|#%app| (waiter-methods-resume (waiter-ref w_0)) w_0 (void))))
            (values (list (cdr pw+v_0)) #f))
          (if (poll-ctx-poll? poll-ctx_0)
            (values #f ch_0)
            (let ((b_0 (box #f)))
              (let ((gq_0 (channel-get-queue ch_0)))
                (let ((gw_0
                       (channel-select-waiter3.1
                        (poll-ctx-select-proc poll-ctx_0)
                        (current-thread/in-atomic))))
                  (let ((n_0 (queue-add! gq_0 (cons gw_0 b_0))))
                    (values
                     #f
                     (control-state-evt9.1
                      the-async-evt
                      (lambda (v_0) (unbox b_0))
                      (lambda () (queue-remove-node! gq_0 n_0))
                      void
                      (lambda ()
                        (let ((pw+v_1
                               (queue-fremove!
                                pq_0
                                not-matching-select-waiter)))
                          (if pw+v_1
                            (begin
                              (let ((w_0 (car pw+v_1)))
                                (begin-unsafe
                                 (|#%app|
                                  (waiter-methods-resume (waiter-ref w_0))
                                  w_0
                                  (void))))
                              (set-box! b_0 (cdr pw+v_1))
                              (values #t #t))
                            (begin
                              (set! n_0 (queue-add! gq_0 (cons gw_0 b_0)))
                              (values #f #f)))))))))))))))))
(define channel-put
  (lambda (ch_0 v_0)
    (begin
      (if (1/channel? ch_0)
        (void)
        (raise-argument-error 'channel-put "channel?" ch_0))
      (if (|#%app| channel-put-impersonator? ch_0)
        (channel-impersonator-put ch_0 v_0 channel-put)
        (|#%app|
         (begin
           (start-atomic)
           (begin0
             (let ((gw+b_0 (queue-remove! (channel-get-queue ch_0))))
               (let ((pw_0 (current-thread/in-atomic)))
                 (if (not gw+b_0)
                   (let ((pq_0 (channel-put-queue ch_0)))
                     (let ((n_0 (queue-add! pq_0 (cons pw_0 v_0))))
                       (let ((interrupt-cb_0
                              (lambda ()
                                (begin
                                  (queue-remove-node! pq_0 n_0)
                                  (lambda () (channel-put ch_0 v_0))))))
                         (begin-unsafe
                          (|#%app|
                           (waiter-methods-suspend (waiter-ref pw_0))
                           pw_0
                           interrupt-cb_0)))))
                   (begin
                     (set-box! (cdr gw+b_0) v_0)
                     (let ((w_0 (car gw+b_0)))
                       (begin-unsafe
                        (|#%app|
                         (waiter-methods-resume (waiter-ref w_0))
                         w_0
                         v_0)))
                     void))))
             (end-atomic))))))))
(define channel-put/poll
  (lambda (ch_0 v_0 self_0 poll-ctx_0)
    (let ((gq_0 (channel-get-queue ch_0)))
      (let ((gw+b_0 (queue-fremove! gq_0 not-matching-select-waiter)))
        (if gw+b_0
          (begin
            (set-box! (cdr gw+b_0) v_0)
            (let ((w_0 (car gw+b_0)))
              (begin-unsafe
               (|#%app| (waiter-methods-resume (waiter-ref w_0)) w_0 v_0)))
            (values (list self_0) #f))
          (if (poll-ctx-poll? poll-ctx_0)
            (values #f self_0)
            (let ((pq_0 (channel-put-queue ch_0)))
              (let ((pw_0
                     (channel-select-waiter3.1
                      (poll-ctx-select-proc poll-ctx_0)
                      (current-thread/in-atomic))))
                (let ((n_0 (queue-add! pq_0 (cons pw_0 v_0))))
                  (values
                   #f
                   (control-state-evt9.1
                    the-async-evt
                    (lambda (v_1) self_0)
                    (lambda () (queue-remove-node! pq_0 n_0))
                    void
                    (lambda ()
                      (let ((gw+b_1
                             (queue-fremove! gq_0 not-matching-select-waiter)))
                        (if gw+b_1
                          (begin
                            (set-box! (cdr gw+b_1) v_0)
                            (let ((w_0 (car gw+b_1)))
                              (begin-unsafe
                               (|#%app|
                                (waiter-methods-resume (waiter-ref w_0))
                                w_0
                                v_0)))
                            (values self_0 #t))
                          (begin
                            (set! n_0 (queue-add! pq_0 (cons pw_0 v_0)))
                            (values #f #f))))))))))))))))
(define 1/channel-put-evt
  (|#%name|
   channel-put-evt
   (lambda (ch_0 v_0)
     (begin
       (begin
         (if (1/channel? ch_0)
           (void)
           (raise-argument-error 'channel-put-evt "channel?" ch_0))
         (if (|#%app| channel-put-impersonator? ch_0)
           (channel-impersonator-put ch_0 v_0 1/channel-put-evt)
           (channel-put-evt*2.1 ch_0 v_0)))))))
(define 1/channel-put-evt?
  (|#%name| channel-put-evt? (lambda (v_0) (begin (channel-put-evt*? v_0)))))
(define channel-impersonator-put
  (lambda (ch_0 v_0 channel-put_0)
    (let ((ch+put-proc_0 (|#%app| channel-put-impersonator-ref ch_0)))
      (let ((old-ch_0 (car ch+put-proc_0)))
        (let ((new-v_0 (|#%app| (cdr ch+put-proc_0) old-ch_0 v_0)))
          (|#%app| channel-put_0 old-ch_0 new-v_0))))))
(define channel-put-do
  (lambda (v_0)
    (let ((app_0 (channel-put-evt*-ch v_0)))
      (channel-put app_0 (channel-put-evt*-v v_0)))))
(define not-matching-select-waiter
  (lambda (w+b/v_0)
    (let ((w_0 (car w+b/v_0)))
      (let ((or-part_0 (not (channel-select-waiter? w_0))))
        (if or-part_0
          or-part_0
          (not
           (let ((app_0 (current-thread/in-atomic)))
             (eq? app_0 (channel-select-waiter-thread w_0)))))))))
(define sync-on-channel #f)
(define set-sync-on-channel! (lambda (sync_0) (set! sync-on-channel sync_0)))
(define-values
 (impersonator-prop:channel-put
  channel-put-impersonator?
  channel-put-impersonator-ref)
 (make-impersonator-property 'channel-put-impersonator))
(define 1/chaperone-evt
  (|#%name|
   chaperone-evt
   (lambda (evt_0 proc_0 . args_0)
     (begin
       (begin
         (if (1/evt? evt_0)
           (void)
           (raise-argument-error 'chaperone-evt "evt?" evt_0))
         (if (if (procedure? proc_0) (procedure-arity-includes? proc_0 1) #f)
           (void)
           (raise-argument-error
            proc_0
            "(procedure-arity-includes/c 1)"
            proc_0))
         (do-chaperone-evt
          'chaperone-evt
          "evt"
          #t
          evt_0
          proc_0
          args_0
          (lambda (v_0)
            (if (1/evt? v_0)
              (void)
              (raise-result-error 'chaperone-evt "evt?" v_0)))))))))
(define do-chaperone-evt
  (lambda (who_0 what_0 chaperone?_0 evt_0 proc_0 args_0 check-evt_0)
    (begin
      (check-impersonator-properties who_0 args_0)
      (apply
       chaperone-struct
       evt_0
       (if (primary-evt? evt_0)
         primary-evt-ref
         (if (secondary-evt? evt_0)
           secondary-evt-ref
           (internal-error "unrecognized evt to impersonate")))
       (lambda (evt_1 v_0) v_0)
       impersonator-prop:evt
       (lambda (also-evt_0)
         (call-with-values
          (lambda () (|#%app| proc_0 evt_0))
          (case-lambda
           ((new-evt_0 wrap_0)
            (begin
              (if chaperone?_0
                (check-chaperone-of what_0 new-evt_0 evt_0)
                (void))
              (|#%app| check-evt_0 new-evt_0)
              (if (if (procedure? wrap_0)
                    (procedure-arity-includes? wrap_0 1)
                    #f)
                (void)
                (raise-result-error
                 who_0
                 "(procedure-arity-includes/c 1)"
                 wrap_0))
              (handle-evt8.1
               new-evt_0
               (lambda rs_0
                 (call-with-values
                  (lambda () (apply wrap_0 rs_0))
                  (lambda new-rs_0
                    (begin
                      (if (let ((app_0 (length rs_0)))
                            (= app_0 (length new-rs_0)))
                        (void)
                        (raise
                         (let ((app_0
                                (let ((app_0
                                       (if chaperone?_0
                                         "chaperone"
                                         "impersonator")))
                                  (let ((app_1 (number->string (length rs_0))))
                                    (string-append
                                     what_0
                                     " "
                                     app_0
                                     ": result wrapper returned wrong number of values\n"
                                     "  expected count: "
                                     app_1
                                     "\n"
                                     "  returned count: "
                                     (number->string (length new-rs_0)))))))
                           (|#%app|
                            exn:fail:contract:arity
                            app_0
                            (current-continuation-marks)))))
                      (if chaperone?_0
                        (begin
                          (begin
                            (letrec*
                             ((for-loop_0
                               (|#%name|
                                for-loop
                                (lambda (lst_0 lst_1)
                                  (begin
                                    (if (if (pair? lst_0) (pair? lst_1) #f)
                                      (let ((r_0 (unsafe-car lst_0)))
                                        (let ((rest_0 (unsafe-cdr lst_0)))
                                          (let ((new-r_0 (unsafe-car lst_1)))
                                            (let ((rest_1 (unsafe-cdr lst_1)))
                                              (begin
                                                (check-chaperone-of
                                                 what_0
                                                 new-r_0
                                                 r_0)
                                                (for-loop_0 rest_0 rest_1))))))
                                      (values)))))))
                             (for-loop_0 rs_0 new-rs_0)))
                          (void))
                        (void))
                      (apply values new-rs_0))))))))
           (args_1
            (raise
             (let ((app_0
                    (let ((app_0 (if chaperone?_0 "chaperone" "impersonator")))
                      (string-append
                       what_0
                       " "
                       app_0
                       ": returned wrong number of values\n"
                       "  expected count: 2\n"
                       "  returned count: "
                       (number->string (length args_1))))))
               (|#%app|
                exn:fail:contract:arity
                app_0
                (current-continuation-marks))))))))
       args_0))))
(define 1/chaperone-channel
  (|#%name|
   chaperone-channel
   (lambda (ch_0 get-proc_0 put-proc_0 . args_0)
     (begin
       (do-impersonate-channel
        'chaperone-channel
        #t
        ch_0
        get-proc_0
        put-proc_0
        args_0)))))
(define 1/impersonate-channel
  (|#%name|
   impersonate-channel
   (lambda (ch_0 get-proc_0 put-proc_0 . args_0)
     (begin
       (do-impersonate-channel
        'impersonate-channel
        #f
        ch_0
        get-proc_0
        put-proc_0
        args_0)))))
(define do-impersonate-channel
  (lambda (who_0 chaperone?_0 ch_0 get-proc_0 put-proc_0 args_0)
    (begin
      (if (1/channel? ch_0)
        (void)
        (raise-argument-error who_0 "channel?" ch_0))
      (if (if (procedure? get-proc_0)
            (procedure-arity-includes? get-proc_0 1)
            #f)
        (void)
        (raise-argument-error
         who_0
         "(procedure-arity-includes/c 1)"
         get-proc_0))
      (if (if (procedure? put-proc_0)
            (procedure-arity-includes? put-proc_0 2)
            #f)
        (void)
        (raise-argument-error
         who_0
         "(procedure-arity-includes/c 2)"
         put-proc_0))
      (do-chaperone-evt
       who_0
       "channel"
       chaperone?_0
       ch_0
       get-proc_0
       (list*
        impersonator-prop:channel-put
        (cons
         ch_0
         (lambda (ch_1 v_0)
           (let ((new-v_0 (|#%app| put-proc_0 ch_1 v_0)))
             (begin
               (if chaperone?_0
                 (check-chaperone-of "channel" new-v_0 v_0)
                 (void))
               new-v_0))))
        args_0)
       (lambda (v_0)
         (if (1/channel? v_0)
           (void)
           (raise-result-error who_0 "channel?" v_0)))))))
(define check-chaperone-of
  (lambda (what_0 new-r_0 r_0)
    (if (chaperone-of? new-r_0 r_0)
      (void)
      (let ((app_0 (string->symbol (string-append what_0 " chaperone"))))
        (raise-arguments-error
         app_0
         (string-append
          "non-chaperone result;\n"
          " received a value that is not a chaperone of the original value\n")
         "value"
         r_0
         "non-chaperone value"
         new-r_0)))))
(define check-impersonator-properties
  (lambda (who_0 args_0)
    (letrec*
     ((loop_0
       (|#%name|
        loop
        (lambda (args_1)
          (begin
            (if (null? args_1)
              (void)
              (begin
                (if (impersonator-property? (car args_1))
                  (void)
                  (raise-argument-error
                   who_0
                   "impersonator-property?"
                   (car args_1)))
                (if (null? args_1)
                  (raise-arguments-error
                   who_0
                   "missing an argument after an impersonator-property argument"
                   "impersonator property"
                   (car args_1))
                  (loop_0 (cddr args_1))))))))))
     (loop_0 args_0))))
(define struct:syncing
  (make-record-type-descriptor*
   'syncing
   #f
   (|#%nongenerative-uid| syncing)
   #f
   #f
   5
   31))
(define effect_2287
  (struct-type-install-properties!
   struct:syncing
   '(syncing)
   5
   0
   #f
   null
   (current-inspector)
   #f
   '()
   #f
   'syncing))
(define syncing1.1
  (|#%name|
   syncing
   (record-constructor
    (make-record-constructor-descriptor struct:syncing #f #f))))
(define syncing?_2448 (|#%name| syncing? (record-predicate struct:syncing)))
(define syncing?
  (|#%name|
   syncing?
   (lambda (v)
     (if (syncing?_2448 v)
       #t
       ($value
        (if (impersonator? v) (syncing?_2448 (impersonator-val v)) #f))))))
(define syncing-selected_2375
  (|#%name| syncing-selected (record-accessor struct:syncing 0)))
(define syncing-selected
  (|#%name|
   syncing-selected
   (lambda (s)
     (if (syncing?_2448 s)
       (syncing-selected_2375 s)
       ($value
        (impersonate-ref
         syncing-selected_2375
         struct:syncing
         0
         s
         'syncing
         'selected))))))
(define syncing-syncers_2564
  (|#%name| syncing-syncers (record-accessor struct:syncing 1)))
(define syncing-syncers
  (|#%name|
   syncing-syncers
   (lambda (s)
     (if (syncing?_2448 s)
       (syncing-syncers_2564 s)
       ($value
        (impersonate-ref
         syncing-syncers_2564
         struct:syncing
         1
         s
         'syncing
         'syncers))))))
(define syncing-wakeup_2421
  (|#%name| syncing-wakeup (record-accessor struct:syncing 2)))
(define syncing-wakeup
  (|#%name|
   syncing-wakeup
   (lambda (s)
     (if (syncing?_2448 s)
       (syncing-wakeup_2421 s)
       ($value
        (impersonate-ref
         syncing-wakeup_2421
         struct:syncing
         2
         s
         'syncing
         'wakeup))))))
(define syncing-disable-break_2747
  (|#%name| syncing-disable-break (record-accessor struct:syncing 3)))
(define syncing-disable-break
  (|#%name|
   syncing-disable-break
   (lambda (s)
     (if (syncing?_2448 s)
       (syncing-disable-break_2747 s)
       ($value
        (impersonate-ref
         syncing-disable-break_2747
         struct:syncing
         3
         s
         'syncing
         'disable-break))))))
(define syncing-need-retry?_2343
  (|#%name| syncing-need-retry? (record-accessor struct:syncing 4)))
(define syncing-need-retry?
  (|#%name|
   syncing-need-retry?
   (lambda (s)
     (if (syncing?_2448 s)
       (syncing-need-retry?_2343 s)
       ($value
        (impersonate-ref
         syncing-need-retry?_2343
         struct:syncing
         4
         s
         'syncing
         'need-retry?))))))
(define set-syncing-selected!_2425
  (|#%name| set-syncing-selected! (record-mutator struct:syncing 0)))
(define set-syncing-selected!
  (|#%name|
   set-syncing-selected!
   (lambda (s v)
     (if (syncing?_2448 s)
       (set-syncing-selected!_2425 s v)
       ($value
        (impersonate-set!
         set-syncing-selected!_2425
         struct:syncing
         0
         0
         s
         v
         'syncing
         'selected))))))
(define set-syncing-syncers!_2653
  (|#%name| set-syncing-syncers! (record-mutator struct:syncing 1)))
(define set-syncing-syncers!
  (|#%name|
   set-syncing-syncers!
   (lambda (s v)
     (if (syncing?_2448 s)
       (set-syncing-syncers!_2653 s v)
       ($value
        (impersonate-set!
         set-syncing-syncers!_2653
         struct:syncing
         1
         1
         s
         v
         'syncing
         'syncers))))))
(define set-syncing-wakeup!_2835
  (|#%name| set-syncing-wakeup! (record-mutator struct:syncing 2)))
(define set-syncing-wakeup!
  (|#%name|
   set-syncing-wakeup!
   (lambda (s v)
     (if (syncing?_2448 s)
       (set-syncing-wakeup!_2835 s v)
       ($value
        (impersonate-set!
         set-syncing-wakeup!_2835
         struct:syncing
         2
         2
         s
         v
         'syncing
         'wakeup))))))
(define set-syncing-disable-break!_2672
  (|#%name| set-syncing-disable-break! (record-mutator struct:syncing 3)))
(define set-syncing-disable-break!
  (|#%name|
   set-syncing-disable-break!
   (lambda (s v)
     (if (syncing?_2448 s)
       (set-syncing-disable-break!_2672 s v)
       ($value
        (impersonate-set!
         set-syncing-disable-break!_2672
         struct:syncing
         3
         3
         s
         v
         'syncing
         'disable-break))))))
(define set-syncing-need-retry?!_2470
  (|#%name| set-syncing-need-retry?! (record-mutator struct:syncing 4)))
(define set-syncing-need-retry?!
  (|#%name|
   set-syncing-need-retry?!
   (lambda (s v)
     (if (syncing?_2448 s)
       (set-syncing-need-retry?!_2470 s v)
       ($value
        (impersonate-set!
         set-syncing-need-retry?!_2470
         struct:syncing
         4
         4
         s
         v
         'syncing
         'need-retry?))))))
(define struct:syncer
  (make-record-type-descriptor*
   'syncer
   #f
   (|#%nongenerative-uid| syncer)
   #f
   #f
   9
   511))
(define effect_2172
  (struct-type-install-properties!
   struct:syncer
   '(syncer)
   9
   0
   #f
   null
   #f
   #f
   '()
   #f
   'syncer))
(define syncer2.1
  (|#%name|
   syncer
   (record-constructor
    (make-record-constructor-descriptor struct:syncer #f #f))))
(define syncer?_2220 (|#%name| syncer? (record-predicate struct:syncer)))
(define syncer?
  (|#%name|
   syncer?
   (lambda (v)
     (if (syncer?_2220 v)
       #t
       ($value
        (if (impersonator? v) (syncer?_2220 (impersonator-val v)) #f))))))
(define syncer-evt_2678
  (|#%name| syncer-evt (record-accessor struct:syncer 0)))
(define syncer-evt
  (|#%name|
   syncer-evt
   (lambda (s)
     (if (syncer?_2220 s)
       (syncer-evt_2678 s)
       ($value
        (impersonate-ref syncer-evt_2678 struct:syncer 0 s 'syncer 'evt))))))
(define syncer-wraps_2709
  (|#%name| syncer-wraps (record-accessor struct:syncer 1)))
(define syncer-wraps
  (|#%name|
   syncer-wraps
   (lambda (s)
     (if (syncer?_2220 s)
       (syncer-wraps_2709 s)
       ($value
        (impersonate-ref
         syncer-wraps_2709
         struct:syncer
         1
         s
         'syncer
         'wraps))))))
(define syncer-commits_2120
  (|#%name| syncer-commits (record-accessor struct:syncer 2)))
(define syncer-commits
  (|#%name|
   syncer-commits
   (lambda (s)
     (if (syncer?_2220 s)
       (syncer-commits_2120 s)
       ($value
        (impersonate-ref
         syncer-commits_2120
         struct:syncer
         2
         s
         'syncer
         'commits))))))
(define syncer-interrupted?_2265
  (|#%name| syncer-interrupted? (record-accessor struct:syncer 3)))
(define syncer-interrupted?
  (|#%name|
   syncer-interrupted?
   (lambda (s)
     (if (syncer?_2220 s)
       (syncer-interrupted?_2265 s)
       ($value
        (impersonate-ref
         syncer-interrupted?_2265
         struct:syncer
         3
         s
         'syncer
         'interrupted?))))))
(define syncer-interrupt_2546
  (|#%name| syncer-interrupt (record-accessor struct:syncer 4)))
(define syncer-interrupt
  (|#%name|
   syncer-interrupt
   (lambda (s)
     (if (syncer?_2220 s)
       (syncer-interrupt_2546 s)
       ($value
        (impersonate-ref
         syncer-interrupt_2546
         struct:syncer
         4
         s
         'syncer
         'interrupt))))))
(define syncer-abandons_1792
  (|#%name| syncer-abandons (record-accessor struct:syncer 5)))
(define syncer-abandons
  (|#%name|
   syncer-abandons
   (lambda (s)
     (if (syncer?_2220 s)
       (syncer-abandons_1792 s)
       ($value
        (impersonate-ref
         syncer-abandons_1792
         struct:syncer
         5
         s
         'syncer
         'abandons))))))
(define syncer-retry_2570
  (|#%name| syncer-retry (record-accessor struct:syncer 6)))
(define syncer-retry
  (|#%name|
   syncer-retry
   (lambda (s)
     (if (syncer?_2220 s)
       (syncer-retry_2570 s)
       ($value
        (impersonate-ref
         syncer-retry_2570
         struct:syncer
         6
         s
         'syncer
         'retry))))))
(define syncer-prev_3082
  (|#%name| syncer-prev (record-accessor struct:syncer 7)))
(define syncer-prev
  (|#%name|
   syncer-prev
   (lambda (s)
     (if (syncer?_2220 s)
       (syncer-prev_3082 s)
       ($value
        (impersonate-ref syncer-prev_3082 struct:syncer 7 s 'syncer 'prev))))))
(define syncer-next_2299
  (|#%name| syncer-next (record-accessor struct:syncer 8)))
(define syncer-next
  (|#%name|
   syncer-next
   (lambda (s)
     (if (syncer?_2220 s)
       (syncer-next_2299 s)
       ($value
        (impersonate-ref syncer-next_2299 struct:syncer 8 s 'syncer 'next))))))
(define set-syncer-evt!_2722
  (|#%name| set-syncer-evt! (record-mutator struct:syncer 0)))
(define set-syncer-evt!
  (|#%name|
   set-syncer-evt!
   (lambda (s v)
     (if (syncer?_2220 s)
       (set-syncer-evt!_2722 s v)
       ($value
        (impersonate-set!
         set-syncer-evt!_2722
         struct:syncer
         0
         0
         s
         v
         'syncer
         'evt))))))
(define set-syncer-wraps!_2694
  (|#%name| set-syncer-wraps! (record-mutator struct:syncer 1)))
(define set-syncer-wraps!
  (|#%name|
   set-syncer-wraps!
   (lambda (s v)
     (if (syncer?_2220 s)
       (set-syncer-wraps!_2694 s v)
       ($value
        (impersonate-set!
         set-syncer-wraps!_2694
         struct:syncer
         1
         1
         s
         v
         'syncer
         'wraps))))))
(define set-syncer-commits!_2064
  (|#%name| set-syncer-commits! (record-mutator struct:syncer 2)))
(define set-syncer-commits!
  (|#%name|
   set-syncer-commits!
   (lambda (s v)
     (if (syncer?_2220 s)
       (set-syncer-commits!_2064 s v)
       ($value
        (impersonate-set!
         set-syncer-commits!_2064
         struct:syncer
         2
         2
         s
         v
         'syncer
         'commits))))))
(define set-syncer-interrupted?!_1948
  (|#%name| set-syncer-interrupted?! (record-mutator struct:syncer 3)))
(define set-syncer-interrupted?!
  (|#%name|
   set-syncer-interrupted?!
   (lambda (s v)
     (if (syncer?_2220 s)
       (set-syncer-interrupted?!_1948 s v)
       ($value
        (impersonate-set!
         set-syncer-interrupted?!_1948
         struct:syncer
         3
         3
         s
         v
         'syncer
         'interrupted?))))))
(define set-syncer-interrupt!_2374
  (|#%name| set-syncer-interrupt! (record-mutator struct:syncer 4)))
(define set-syncer-interrupt!
  (|#%name|
   set-syncer-interrupt!
   (lambda (s v)
     (if (syncer?_2220 s)
       (set-syncer-interrupt!_2374 s v)
       ($value
        (impersonate-set!
         set-syncer-interrupt!_2374
         struct:syncer
         4
         4
         s
         v
         'syncer
         'interrupt))))))
(define set-syncer-abandons!_1983
  (|#%name| set-syncer-abandons! (record-mutator struct:syncer 5)))
(define set-syncer-abandons!
  (|#%name|
   set-syncer-abandons!
   (lambda (s v)
     (if (syncer?_2220 s)
       (set-syncer-abandons!_1983 s v)
       ($value
        (impersonate-set!
         set-syncer-abandons!_1983
         struct:syncer
         5
         5
         s
         v
         'syncer
         'abandons))))))
(define set-syncer-retry!_2583
  (|#%name| set-syncer-retry! (record-mutator struct:syncer 6)))
(define set-syncer-retry!
  (|#%name|
   set-syncer-retry!
   (lambda (s v)
     (if (syncer?_2220 s)
       (set-syncer-retry!_2583 s v)
       ($value
        (impersonate-set!
         set-syncer-retry!_2583
         struct:syncer
         6
         6
         s
         v
         'syncer
         'retry))))))
(define set-syncer-prev!_2849
  (|#%name| set-syncer-prev! (record-mutator struct:syncer 7)))
(define set-syncer-prev!
  (|#%name|
   set-syncer-prev!
   (lambda (s v)
     (if (syncer?_2220 s)
       (set-syncer-prev!_2849 s v)
       ($value
        (impersonate-set!
         set-syncer-prev!_2849
         struct:syncer
         7
         7
         s
         v
         'syncer
         'prev))))))
(define set-syncer-next!_2406
  (|#%name| set-syncer-next! (record-mutator struct:syncer 8)))
(define set-syncer-next!
  (|#%name|
   set-syncer-next!
   (lambda (s v)
     (if (syncer?_2220 s)
       (set-syncer-next!_2406 s v)
       ($value
        (impersonate-set!
         set-syncer-next!_2406
         struct:syncer
         8
         8
         s
         v
         'syncer
         'next))))))
(define make-syncer
  (lambda (evt_0 wraps_0 prev_0)
    (syncer2.1 evt_0 wraps_0 null #f #f null #f prev_0 #f)))
(define none-syncer
  (begin-unsafe (syncer2.1 #f null null #f #f null #f #f #f)))
(define make-syncing.1
  (|#%name|
   make-syncing
   (lambda (disable-break3_0 syncers5_0)
     (begin (syncing1.1 #f syncers5_0 void disable-break3_0 #f)))))
(define sync-atomic-poll-evt?
  (lambda (evt_0)
    (let ((or-part_0 (begin-unsafe (channel-put-evt*? evt_0))))
      (if or-part_0
        or-part_0
        (let ((or-part_1 (1/channel? evt_0)))
          (if or-part_1
            or-part_1
            (let ((or-part_2 (1/semaphore? evt_0)))
              (if or-part_2
                or-part_2
                (let ((or-part_3 (1/semaphore-peek-evt? evt_0)))
                  (if or-part_3
                    or-part_3
                    (let ((or-part_4 (eq? the-always-evt evt_0)))
                      (if or-part_4
                        or-part_4
                        (eq? the-never-evt evt_0)))))))))))))
(define do-sync.1
  (|#%name|
   do-sync
   (lambda (enable-break?7_0 who9_0 timeout10_0 args11_0)
     (begin
       (begin
         (if (let ((or-part_0 (not timeout10_0)))
               (if or-part_0
                 or-part_0
                 (let ((or-part_1
                        (if (real? timeout10_0) (>= timeout10_0 0) #f)))
                   (if or-part_1
                     or-part_1
                     (if (procedure? timeout10_0)
                       (procedure-arity-includes? timeout10_0 0)
                       #f)))))
           (void)
           (raise-argument-error
            who9_0
            "(or/c #f (and/c real? (not/c negative?)) (-> any))"
            timeout10_0))
         (let ((local-break-cell_0
                (if enable-break?7_0 (make-thread-cell #t) #f)))
           (let ((s_0
                  (let ((temp41_0
                         (let ((app_0 random-rotate))
                           (|#%app|
                            app_0
                            (|#%app| evts->syncers who9_0 args11_0)))))
                    (let ((temp42_0
                           (if local-break-cell_0
                             (let ((t_0 (1/current-thread)))
                               (|#%name|
                                temp42
                                (lambda ()
                                  (begin
                                    (thread-ignore-break-cell!
                                     t_0
                                     local-break-cell_0)))))
                             #f)))
                      (let ((temp41_1 temp41_0))
                        (make-syncing.1 temp42_0 temp41_1))))))
             (begin
               (if (let ((or-part_0
                          (if (real? timeout10_0) (zero? timeout10_0) #f)))
                     (if or-part_0 or-part_0 (procedure? timeout10_0)))
                 (begin
                   (start-atomic)
                   (call-pre-poll-external-callbacks)
                   (end-atomic))
                 (void))
               (let ((go_0
                      (|#%name|
                       go
                       (lambda (thunk-result?38_0)
                         (begin
                           (dynamic-wind
                            (lambda ()
                              (begin
                                (start-atomic)
                                (thread-push-kill-callback!
                                 (lambda () (|#%app| syncing-abandon! s_0)))
                                (thread-push-suspend+resume-callbacks!
                                 (lambda () (|#%app| syncing-interrupt! s_0))
                                 (lambda ()
                                   (|#%app| syncing-queue-retry! s_0)))
                                (end-atomic)))
                            (lambda ()
                              (begin
                                (if enable-break?7_0
                                  (1/check-for-break)
                                  (void))
                                (if (let ((or-part_0
                                           (if (real? timeout10_0)
                                             (zero? timeout10_0)
                                             #f)))
                                      (if or-part_0
                                        or-part_0
                                        (procedure? timeout10_0)))
                                  (letrec*
                                   ((poll-loop_0
                                     (|#%name|
                                      poll-loop
                                      (lambda ()
                                        (begin
                                          (let ((temp44_0
                                                 (if thunk-result?38_0
                                                   (|#%name|
                                                    temp44
                                                    (lambda (thunk_0)
                                                      (begin thunk_0)))
                                                   #f)))
                                            (let ((temp45_0
                                                   (lambda (sched-info_0
                                                            polled-all?_0
                                                            no-wrappers?_0)
                                                     (if (not polled-all?_0)
                                                       (poll-loop_0)
                                                       (if (procedure?
                                                            timeout10_0)
                                                         (if thunk-result?38_0
                                                           timeout10_0
                                                           (|#%app|
                                                            timeout10_0))
                                                         (if thunk-result?38_0
                                                           (lambda () #f)
                                                           #f))))))
                                              (let ((temp44_1 temp44_0))
                                                (|#%app|
                                                 sync-poll.1
                                                 #f
                                                 #t
                                                 temp45_0
                                                 #f
                                                 #t
                                                 unsafe-undefined
                                                 temp44_1
                                                 s_0)))))))))
                                   (poll-loop_0))
                                  (let ((timeout-at_0
                                         (if timeout10_0
                                           (let ((app_0 (* timeout10_0 1000)))
                                             (+
                                              app_0
                                              (current-inexact-milliseconds)))
                                           #f)))
                                    (letrec*
                                     ((loop_0
                                       (|#%name|
                                        loop
                                        (lambda (did-work?_0 polled-all?_0)
                                          (begin
                                            (if (if polled-all?_0
                                                  (if timeout10_0
                                                    (<=
                                                     timeout-at_0
                                                     (current-inexact-milliseconds))
                                                    #f)
                                                  #f)
                                              (begin
                                                (start-atomic)
                                                (if (syncing-selected s_0)
                                                  (begin
                                                    (end-atomic)
                                                    (loop_0 #f #f))
                                                  (begin
                                                    (|#%app|
                                                     syncing-done!
                                                     s_0
                                                     none-syncer)
                                                    (end-atomic)
                                                    (if thunk-result?38_0
                                                      (lambda () #f)
                                                      #f))))
                                              (if (if (|#%app|
                                                       all-asynchronous?
                                                       s_0)
                                                    (if (not
                                                         (syncing-selected
                                                          s_0))
                                                      (not
                                                       (syncing-need-retry?
                                                        s_0))
                                                      #f)
                                                    #f)
                                                (begin
                                                  (|#%app|
                                                   suspend-syncing-thread
                                                   s_0
                                                   timeout-at_0)
                                                  (set-syncing-wakeup!
                                                   s_0
                                                   void)
                                                  (loop_0 #f #t))
                                                (let ((temp48_0
                                                       (if thunk-result?38_0
                                                         (|#%name|
                                                          temp48
                                                          (lambda (thunk_0)
                                                            (begin thunk_0)))
                                                         #f)))
                                                  (let ((temp50_0
                                                         (lambda (sched-info_0
                                                                  now-polled-all?_0
                                                                  no-wrappers?_0)
                                                           (begin
                                                             (if timeout-at_0
                                                               (schedule-info-add-timeout-at!
                                                                sched-info_0
                                                                timeout-at_0)
                                                               (void))
                                                             (thread-yield
                                                              sched-info_0)
                                                             (loop_0
                                                              #f
                                                              (if polled-all?_0
                                                                polled-all?_0
                                                                now-polled-all?_0))))))
                                                    (let ((temp48_1 temp48_0))
                                                      (|#%app|
                                                       sync-poll.1
                                                       did-work?_0
                                                       #t
                                                       temp50_0
                                                       #f
                                                       #f
                                                       unsafe-undefined
                                                       temp48_1
                                                       s_0)))))))))))
                                     (loop_0 #t #f))))))
                            (lambda ()
                              (begin
                                (start-atomic)
                                (thread-pop-suspend+resume-callbacks!)
                                (thread-pop-kill-callback!)
                                (|#%app| syncing-abandon! s_0)
                                (end-atomic)))))))))
                 (if enable-break?7_0
                   (let ((thunk_0
                          (with-continuation-mark*
                           push-authentic
                           break-enabled-key
                           local-break-cell_0
                           (go_0 #t))))
                     (begin
                       (thread-remove-ignored-break-cell!
                        (current-thread/in-atomic)
                        local-break-cell_0)
                       (1/check-for-break)
                       (|#%app| thunk_0)))
                   (let ((temp52_0
                          (lambda (sched-info_0 polled-all?_0 no-wrappers?_0)
                            (if polled-all?_0
                              (if (if (real? timeout10_0)
                                    (zero? timeout10_0)
                                    #f)
                                #f
                                (if (procedure? timeout10_0)
                                  (|#%app| timeout10_0)
                                  (if no-wrappers?_0
                                    (go_0 #f)
                                    (|#%app| (go_0 #t)))))
                              (|#%app| (go_0 #t))))))
                     (|#%app|
                      sync-poll.1
                      #f
                      #t
                      temp52_0
                      #t
                      #t
                      unsafe-undefined
                      #f
                      s_0))))))))))))
(define 1/sync
  (|#%name|
   sync
   (case-lambda
    ((evt_0)
     (begin
       (if (|#%app| evt-impersonator? evt_0)
         (let ((temp58_0 (list evt_0))) (do-sync.1 #f 'sync #f temp58_0))
         (if (1/semaphore? evt_0)
           (begin (1/semaphore-wait evt_0) evt_0)
           (if (1/channel? evt_0)
             (channel-get evt_0)
             (if (begin-unsafe (channel-put-evt*? evt_0))
               (begin (channel-put-do evt_0) evt_0)
               (let ((temp61_0 (list evt_0)))
                 (do-sync.1 #f 'sync #f temp61_0))))))))
    (args_0
     (let ((simpler-args_0 (simplify-evts args_0)))
       (if (if (pair? simpler-args_0) (null? (cdr simpler-args_0)) #f)
         (1/sync (car simpler-args_0))
         (do-sync.1 #f 'sync #f simpler-args_0)))))))
(define 1/sync/timeout
  (|#%name|
   sync/timeout
   (case-lambda
    ((timeout_0 evt_0)
     (begin
       (if (|#%app| evt-impersonator? evt_0)
         (let ((temp67_0 (list evt_0)))
           (do-sync.1 #f 'sync/timeout timeout_0 temp67_0))
         (if (if (eqv? timeout_0 0) (1/semaphore? evt_0) #f)
           (if (1/semaphore-try-wait? evt_0) evt_0 #f)
           (if (not timeout_0)
             (if (1/semaphore? evt_0)
               (begin (1/semaphore-wait evt_0) evt_0)
               (if (1/channel? evt_0)
                 (channel-get evt_0)
                 (if (begin-unsafe (channel-put-evt*? evt_0))
                   (begin (channel-put-do evt_0) evt_0)
                   (let ((temp70_0 (list evt_0)))
                     (do-sync.1 #f 'sync/timeout #f temp70_0)))))
             (let ((temp73_0 (list evt_0)))
               (do-sync.1 #f 'sync/timeout timeout_0 temp73_0)))))))
    ((timeout_0 . args_0)
     (let ((simpler-args_0 (simplify-evts args_0)))
       (if (if (pair? simpler-args_0) (null? (cdr simpler-args_0)) #f)
         (1/sync/timeout timeout_0 (car simpler-args_0))
         (do-sync.1 #f 'sync/timeout timeout_0 simpler-args_0)))))))
(define simplify-evts
  (lambda (args_0)
    (if (null? args_0)
      args_0
      (let ((arg_0 (car args_0)))
        (if (eq? the-never-evt arg_0)
          (simplify-evts (cdr args_0))
          (if (if (choice-evt? arg_0)
                (< (length (choice-evt-evts arg_0)) 3)
                #f)
            (simplify-evts
             (let ((app_0 (choice-evt-evts arg_0)))
               (append app_0 (cdr args_0))))
            (cons arg_0 (simplify-evts (cdr args_0)))))))))
(define 1/sync/enable-break
  (|#%name|
   sync/enable-break
   (lambda args_0 (begin (do-sync.1 #t 'sync/enable-break #f args_0)))))
(define 1/sync/timeout/enable-break
  (|#%name|
   sync/timeout/enable-break
   (lambda (timeout_0 . args_0)
     (begin (do-sync.1 #t 'sync/timeout/enable-break timeout_0 args_0)))))
(define effect_2689
  (begin (void (begin-unsafe (set! sync-on-channel 1/sync))) (void)))
(define evts->syncers
  (let ((evts->syncers_0
         (|#%name|
          evts->syncers
          (lambda (who16_0 evts17_0 wraps13_0 commits14_0 abandons15_0)
            (begin
              (call-with-values
               (lambda ()
                 (|#%app| cross-commits-and-abandons commits14_0 abandons15_0))
               (case-lambda
                ((extended-commits_0 guarded-abandons_0)
                 (letrec*
                  ((loop_0
                    (|#%name|
                     loop
                     (lambda (evts_0 first_0 last_0)
                       (begin
                         (if (null? evts_0)
                           first_0
                           (let ((arg_0 (car evts_0)))
                             (begin
                               (if who16_0
                                 (if (1/evt? arg_0)
                                   (void)
                                   (raise-argument-error who16_0 "evt?" arg_0))
                                 (void))
                               (if (choice-evt? arg_0)
                                 (loop_0
                                  (let ((app_0 (choice-evt-evts arg_0)))
                                    (append app_0 (cdr evts_0)))
                                  first_0
                                  last_0)
                                 (let ((sr_0
                                        (begin-unsafe
                                         (syncer2.1
                                          arg_0
                                          wraps13_0
                                          null
                                          #f
                                          #f
                                          null
                                          #f
                                          last_0
                                          #f))))
                                   (begin
                                     (if (if (null? extended-commits_0)
                                           (null? guarded-abandons_0)
                                           #f)
                                       (void)
                                       (begin
                                         (set-syncer-commits!
                                          sr_0
                                          extended-commits_0)
                                         (set-syncer-abandons!
                                          sr_0
                                          guarded-abandons_0)))
                                     (if last_0
                                       (set-syncer-next! last_0 sr_0)
                                       (void))
                                     (let ((app_0 (cdr evts_0)))
                                       (loop_0
                                        app_0
                                        (if first_0 first_0 sr_0)
                                        sr_0)))))))))))))
                  (loop_0 evts17_0 #f #f)))
                (args (raise-binding-result-arity-error 2 args)))))))))
    (case-lambda
     ((who_0 evts_0) (evts->syncers_0 who_0 evts_0 null null null))
     ((who_0 evts_0 wraps_0 commits_0 abandons15_0)
      (evts->syncers_0 who_0 evts_0 wraps_0 commits_0 abandons15_0))
     ((who_0 evts_0 wraps_0 commits14_0)
      (evts->syncers_0 who_0 evts_0 wraps_0 commits14_0 null))
     ((who_0 evts_0 wraps13_0)
      (evts->syncers_0 who_0 evts_0 wraps13_0 null null)))))
(define cross-commits-and-abandons
  (lambda (commits_0 abandons_0)
    (if (if (null? commits_0) (null? abandons_0) #f)
      (values null null)
      (let ((selected?_0 #f))
        (values
         (list
          (lambda ()
            (begin
              (set! selected?_0 #t)
              (let ((lst_0 commits_0))
                (begin
                  (letrec*
                   ((for-loop_0
                     (|#%name|
                      for-loop
                      (lambda (lst_1)
                        (begin
                          (if (pair? lst_1)
                            (let ((commit_0 (unsafe-car lst_1)))
                              (let ((rest_0 (unsafe-cdr lst_1)))
                                (begin
                                  (|#%app| commit_0)
                                  (for-loop_0 rest_0))))
                            (values)))))))
                   (for-loop_0 lst_0))))
              (void)
              (set! commits_0 null))))
         (list
          (lambda ()
            (begin
              (if selected?_0
                (void)
                (begin
                  (let ((lst_0 abandons_0))
                    (begin
                      (letrec*
                       ((for-loop_0
                         (|#%name|
                          for-loop
                          (lambda (lst_1)
                            (begin
                              (if (pair? lst_1)
                                (let ((abandon_0 (unsafe-car lst_1)))
                                  (let ((rest_0 (unsafe-cdr lst_1)))
                                    (begin
                                      (|#%app| abandon_0)
                                      (for-loop_0 rest_0))))
                                (values)))))))
                       (for-loop_0 lst_0))))
                  (void)))
              (set! abandons_0 null)))))))))
(define syncer-remove!
  (lambda (sr_0 s_0)
    (begin
      (if (syncer-prev sr_0)
        (let ((app_0 (syncer-prev sr_0)))
          (set-syncer-next! app_0 (syncer-next sr_0)))
        (set-syncing-syncers! s_0 (syncer-next sr_0)))
      (if (syncer-next sr_0)
        (let ((app_0 (syncer-next sr_0)))
          (set-syncer-prev! app_0 (syncer-prev sr_0)))
        (void)))))
(define syncer-replace!
  (lambda (sr_0 new-syncers_0 s_0)
    (begin
      (let ((prev_0 (syncer-prev sr_0)))
        (begin
          (set-syncer-prev! new-syncers_0 prev_0)
          (if prev_0
            (set-syncer-next! prev_0 new-syncers_0)
            (set-syncing-syncers! s_0 new-syncers_0))))
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda (new-syncers_1)
            (begin
              (let ((c1_0 (syncer-next new-syncers_1)))
                (if c1_0
                  (loop_0 c1_0)
                  (let ((next_0 (syncer-next sr_0)))
                    (begin
                      (set-syncer-next! new-syncers_1 next_0)
                      (if next_0
                        (set-syncer-prev! next_0 new-syncers_1)
                        (void)))))))))))
       (loop_0 new-syncers_0)))))
(define MAX-SYNC-TRIES-ON-ONE-EVT 10)
(define sync-poll.1
  (|#%name|
   sync-poll
   (lambda (did-work?23_0
            done-after-poll?22_0
            fail-k18_0
            fast-only?21_0
            just-poll?20_0
            schedule-info24_0
            success-k19_0
            s32_0)
     (begin
       (let ((sched-info_0
              (if (eq? schedule-info24_0 unsafe-undefined)
                (make-schedule-info.1 did-work?23_0)
                schedule-info24_0)))
         (letrec*
          ((loop_0
            (|#%name|
             loop
             (lambda (sr_0 retries_0 polled-all-so-far?_0 no-wrappers?_0)
               (begin
                 (begin
                   (start-atomic)
                   (if (syncing-need-retry? s32_0)
                     (syncing-retry! s32_0)
                     (void))
                   (let ((c2_0 (syncing-selected s32_0)))
                     (if c2_0
                       (make-result
                        c2_0
                        (list (syncer-evt c2_0))
                        success-k19_0)
                       (if (not sr_0)
                         (begin
                           (if (if just-poll?20_0
                                 (if done-after-poll?22_0
                                   (if polled-all-so-far?_0
                                     (not fast-only?21_0)
                                     #f)
                                   #f)
                                 #f)
                             (syncing-done! s32_0 none-syncer)
                             (void))
                           (end-atomic)
                           (|#%app|
                            fail-k18_0
                            sched-info_0
                            polled-all-so-far?_0
                            no-wrappers?_0))
                         (if (= retries_0 10)
                           (begin
                             (begin-unsafe
                              (set-schedule-info-did-work?! sched-info_0 #t))
                             (end-atomic)
                             (loop_0 (syncer-next sr_0) 0 #f #f))
                           (if (let ((app_0 nested-sync-evt?))
                                 (|#%app| app_0 (syncer-evt sr_0)))
                             (begin
                               (end-atomic)
                               (call-with-values
                                (lambda ()
                                  (let ((app_0 poll-nested-sync))
                                    (|#%app|
                                     app_0
                                     (syncer-evt sr_0)
                                     just-poll?20_0
                                     fast-only?21_0
                                     sched-info_0)))
                                (case-lambda
                                 ((same?_0 new-evt_0)
                                  (if same?_0
                                    (loop_0
                                     (syncer-next sr_0)
                                     0
                                     polled-all-so-far?_0
                                     no-wrappers?_0)
                                    (begin
                                      (start-atomic)
                                      (if (syncing-selected s32_0)
                                        (void)
                                        (set-syncer-evt! sr_0 new-evt_0))
                                      (end-atomic)
                                      (if fast-only?21_0
                                        (|#%app| fail-k18_0 sched-info_0 #f #f)
                                        (loop_0
                                         sr_0
                                         (add1 retries_0)
                                         polled-all-so-far?_0
                                         no-wrappers?_0)))))
                                 (args
                                  (raise-binding-result-arity-error 2 args)))))
                             (let ((ctx_0
                                    (poll-ctx3.1
                                     just-poll?20_0
                                     (lambda () (syncing-done! s32_0 sr_0))
                                     sched-info_0
                                     #f)))
                               (call-with-values
                                (lambda () (evt-poll (syncer-evt sr_0) ctx_0))
                                (case-lambda
                                 ((results_0 new-evt_0)
                                  (if results_0
                                    (begin
                                      (syncing-done! s32_0 sr_0)
                                      (make-result
                                       sr_0
                                       results_0
                                       success-k19_0))
                                    (if (delayed-poll? new-evt_0)
                                      (begin
                                        (end-atomic)
                                        (if fast-only?21_0
                                          (|#%app|
                                           fail-k18_0
                                           sched-info_0
                                           #f
                                           #f)
                                          (let ((new-evt_1
                                                 (|#%app|
                                                  (delayed-poll-resume
                                                   new-evt_0))))
                                            (begin
                                              (start-atomic)
                                              (if (syncing-selected s32_0)
                                                (void)
                                                (set-syncer-evt!
                                                 sr_0
                                                 new-evt_1))
                                              (end-atomic)
                                              (loop_0
                                               sr_0
                                               (add1 retries_0)
                                               polled-all-so-far?_0
                                               #f)))))
                                      (if (choice-evt? new-evt_0)
                                        (begin
                                          (if (let ((or-part_0
                                                     (syncer-interrupt sr_0)))
                                                (if or-part_0
                                                  or-part_0
                                                  (syncer-retry sr_0)))
                                            (begin
                                              (end-atomic)
                                              (internal-error
                                               "choice event discovered after interrupt/retry callback"))
                                            (void))
                                          (let ((new-syncers_0
                                                 (let ((app_0 random-rotate))
                                                   (|#%app|
                                                    app_0
                                                    (let ((app_1
                                                           evts->syncers))
                                                      (let ((app_2
                                                             (choice-evt-evts
                                                              new-evt_0)))
                                                        (let ((app_3
                                                               (syncer-wraps
                                                                sr_0)))
                                                          (let ((app_4
                                                                 (syncer-commits
                                                                  sr_0)))
                                                            (|#%app|
                                                             app_1
                                                             #f
                                                             app_2
                                                             app_3
                                                             app_4
                                                             (syncer-abandons
                                                              sr_0))))))))))
                                            (if (not new-syncers_0)
                                              (begin
                                                (syncer-remove! sr_0 s32_0)
                                                (end-atomic)
                                                (loop_0
                                                 (syncer-next sr_0)
                                                 0
                                                 polled-all-so-far?_0
                                                 no-wrappers?_0))
                                              (begin
                                                (syncer-replace!
                                                 sr_0
                                                 new-syncers_0
                                                 s32_0)
                                                (end-atomic)
                                                (loop_0
                                                 new-syncers_0
                                                 (add1 retries_0)
                                                 polled-all-so-far?_0
                                                 no-wrappers?_0)))))
                                        (if (wrap-evt? new-evt_0)
                                          (begin
                                            (set-syncer-wraps!
                                             sr_0
                                             (let ((app_0
                                                    (wrap-evt-wrap new-evt_0)))
                                               (cons
                                                app_0
                                                (let ((l_0
                                                       (syncer-wraps sr_0)))
                                                  (if (if (null? l_0)
                                                        (not
                                                         (handle-evt?$1
                                                          new-evt_0))
                                                        #f)
                                                    (list values)
                                                    l_0)))))
                                            (let ((inner-new-evt_0
                                                   (wrap-evt-evt new-evt_0)))
                                              (begin
                                                (set-syncer-evt!
                                                 sr_0
                                                 inner-new-evt_0)
                                                (if (eq?
                                                     inner-new-evt_0
                                                     the-always-evt)
                                                  (begin
                                                    (syncing-done! s32_0 sr_0)
                                                    (make-result
                                                     sr_0
                                                     (list the-always-evt)
                                                     success-k19_0))
                                                  (begin
                                                    (end-atomic)
                                                    (loop_0
                                                     sr_0
                                                     (add1 retries_0)
                                                     polled-all-so-far?_0
                                                     #f))))))
                                          (if (control-state-evt? new-evt_0)
                                            (let ((wrap-proc_0
                                                   (control-state-evt-wrap-proc
                                                    new-evt_0)))
                                              (let ((interrupt-proc_0
                                                     (control-state-evt-interrupt-proc
                                                      new-evt_0)))
                                                (let ((abandon-proc_0
                                                       (control-state-evt-abandon-proc
                                                        new-evt_0)))
                                                  (let ((retry-proc_0
                                                         (control-state-evt-retry-proc
                                                          new-evt_0)))
                                                    (begin
                                                      (if (eq?
                                                           wrap-proc_0
                                                           values)
                                                        (void)
                                                        (set-syncer-wraps!
                                                         sr_0
                                                         (cons
                                                          wrap-proc_0
                                                          (syncer-wraps
                                                           sr_0))))
                                                      (if (eq?
                                                           interrupt-proc_0
                                                           void)
                                                        (void)
                                                        (if (eq?
                                                             interrupt-proc_0
                                                             'reset)
                                                          (set-syncer-interrupt!
                                                           sr_0
                                                           #f)
                                                          (begin
                                                            (if (syncer-interrupt
                                                                 sr_0)
                                                              (internal-error
                                                               "syncer already has an interrupt callback")
                                                              (void))
                                                            (set-syncer-interrupt!
                                                             sr_0
                                                             interrupt-proc_0))))
                                                      (if (eq?
                                                           abandon-proc_0
                                                           void)
                                                        (void)
                                                        (set-syncer-abandons!
                                                         sr_0
                                                         (cons
                                                          abandon-proc_0
                                                          (syncer-abandons
                                                           sr_0))))
                                                      (if (eq?
                                                           retry-proc_0
                                                           void)
                                                        (void)
                                                        (if (eq?
                                                             retry-proc_0
                                                             'reset)
                                                          (set-syncer-retry!
                                                           sr_0
                                                           #f)
                                                          (begin
                                                            (if (syncer-retry
                                                                 sr_0)
                                                              (internal-error
                                                               "syncer already has an retry callback")
                                                              (void))
                                                            (set-syncer-retry!
                                                             sr_0
                                                             retry-proc_0))))
                                                      (set-syncer-evt!
                                                       sr_0
                                                       (control-state-evt-evt
                                                        new-evt_0))
                                                      (end-atomic)
                                                      (if (if fast-only?21_0
                                                            (not
                                                             (if (eq?
                                                                  interrupt-proc_0
                                                                  void)
                                                               (if (eq?
                                                                    abandon-proc_0
                                                                    void)
                                                                 (eq?
                                                                  retry-proc_0
                                                                  void)
                                                                 #f)
                                                               #f))
                                                            #f)
                                                        (|#%app|
                                                         fail-k18_0
                                                         sched-info_0
                                                         #f
                                                         #f)
                                                        (loop_0
                                                         sr_0
                                                         (add1 retries_0)
                                                         polled-all-so-far?_0
                                                         no-wrappers?_0)))))))
                                            (if (poll-guard-evt? new-evt_0)
                                              (begin
                                                (end-atomic)
                                                (if fast-only?21_0
                                                  (|#%app|
                                                   fail-k18_0
                                                   sched-info_0
                                                   #f
                                                   #f)
                                                  (let ((generated_0
                                                         (call-with-continuation-barrier
                                                          (lambda ()
                                                            (|#%app|
                                                             (poll-guard-evt-proc
                                                              new-evt_0)
                                                             just-poll?20_0)))))
                                                    (begin
                                                      (set-syncer-evt!
                                                       sr_0
                                                       (if (1/evt? generated_0)
                                                         generated_0
                                                         (wrap-evt7.1
                                                          the-always-evt
                                                          (lambda (a_0)
                                                            generated_0))))
                                                      (loop_0
                                                       sr_0
                                                       (add1 retries_0)
                                                       polled-all-so-far?_0
                                                       #f)))))
                                              (if (if (never-evt? new-evt_0)
                                                    (if (not
                                                         (|#%app|
                                                          evt-impersonator?
                                                          new-evt_0))
                                                      (if (not
                                                           (syncer-interrupt
                                                            sr_0))
                                                        (if (null?
                                                             (syncer-commits
                                                              sr_0))
                                                          (null?
                                                           (syncer-abandons
                                                            sr_0))
                                                          #f)
                                                        #f)
                                                      #f)
                                                    #f)
                                                (begin
                                                  (syncer-remove! sr_0 s32_0)
                                                  (end-atomic)
                                                  (loop_0
                                                   (syncer-next sr_0)
                                                   0
                                                   polled-all-so-far?_0
                                                   no-wrappers?_0))
                                                (if (if (eq?
                                                         new-evt_0
                                                         (syncer-evt sr_0))
                                                      (not
                                                       (poll-ctx-incomplete?
                                                        ctx_0))
                                                      #f)
                                                  (begin
                                                    (end-atomic)
                                                    (loop_0
                                                     (syncer-next sr_0)
                                                     0
                                                     polled-all-so-far?_0
                                                     no-wrappers?_0))
                                                  (begin
                                                    (set-syncer-evt!
                                                     sr_0
                                                     new-evt_0)
                                                    (end-atomic)
                                                    (loop_0
                                                     sr_0
                                                     (add1 retries_0)
                                                     polled-all-so-far?_0
                                                     no-wrappers?_0)))))))))))
                                 (args
                                  (raise-binding-result-arity-error
                                   2
                                   args))))))))))))))))
          (loop_0 (syncing-syncers s32_0) 0 #t #t)))))))
(define make-result
  (lambda (sr_0 results_0 success-k_0)
    (let ((wraps_0 (syncer-wraps sr_0)))
      (begin
        (end-atomic)
        (letrec*
         ((loop_0
           (|#%name|
            loop
            (lambda (wraps_1 results_1)
              (begin
                (if (null? wraps_1)
                  (if success-k_0
                    (|#%app| success-k_0 (lambda () (apply values results_1)))
                    (apply values results_1))
                  (if (null? (cdr wraps_1))
                    (let ((proc_0 (car wraps_1)))
                      (if success-k_0
                        (|#%app|
                         success-k_0
                         (lambda () (apply proc_0 results_1)))
                        (apply proc_0 results_1)))
                    (let ((app_0 (cdr wraps_1)))
                      (loop_0
                       app_0
                       (call-with-values
                        (lambda () (apply (car wraps_1) results_1))
                        list))))))))))
         (loop_0 wraps_0 results_0))))))
(define syncing-done!
  (lambda (s_0 selected-sr_0)
    (begin
      (set-syncing-selected! s_0 selected-sr_0)
      (let ((lst_0 (syncer-commits selected-sr_0)))
        (begin
          (letrec*
           ((for-loop_0
             (|#%name|
              for-loop
              (lambda (lst_1)
                (begin
                  (if (pair? lst_1)
                    (let ((callback_0 (unsafe-car lst_1)))
                      (let ((rest_0 (unsafe-cdr lst_1)))
                        (begin (|#%app| callback_0) (for-loop_0 rest_0))))
                    (values)))))))
           (for-loop_0 lst_0))))
      (void)
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda (sr_0)
            (begin
              (if sr_0
                (begin
                  (if (eq? sr_0 selected-sr_0)
                    (void)
                    (begin
                      (if (syncer-interrupted? sr_0)
                        (void)
                        (let ((interrupt_0 (syncer-interrupt sr_0)))
                          (if interrupt_0 (|#%app| interrupt_0) (void))))
                      (let ((lst_0 (syncer-abandons sr_0)))
                        (begin
                          (letrec*
                           ((for-loop_0
                             (|#%name|
                              for-loop
                              (lambda (lst_1)
                                (begin
                                  (if (pair? lst_1)
                                    (let ((abandon_0 (unsafe-car lst_1)))
                                      (let ((rest_0 (unsafe-cdr lst_1)))
                                        (begin
                                          (|#%app| abandon_0)
                                          (for-loop_0 rest_0))))
                                    (values)))))))
                           (for-loop_0 lst_0))))
                      (void)))
                  (loop_0 (syncer-next sr_0)))
                (void)))))))
       (loop_0 (syncing-syncers s_0)))
      (if (syncing-disable-break s_0)
        (|#%app| (syncing-disable-break s_0))
        (void))
      (|#%app| (syncing-wakeup s_0)))))
(define syncing-abandon!
  (lambda (s_0)
    (if (syncing-selected s_0) (void) (syncing-done! s_0 none-syncer))))
(define syncing-interrupt!
  (lambda (s_0)
    (letrec*
     ((loop_0
       (|#%name|
        loop
        (lambda (sr_0)
          (begin
            (if sr_0
              (begin
                (if (syncer-interrupted? sr_0)
                  (void)
                  (begin
                    (set-syncer-interrupted?! sr_0 #t)
                    (let ((interrupt_0 (syncer-interrupt sr_0)))
                      (if interrupt_0 (|#%app| interrupt_0) (void)))))
                (loop_0 (syncer-next sr_0)))
              (void)))))))
     (loop_0 (syncing-syncers s_0)))))
(define syncing-retry!
  (lambda (s_0)
    (begin
      (set-syncing-need-retry?! s_0 #f)
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda (sr_0)
            (begin
              (if (if sr_0 (not (syncing-selected s_0)) #f)
                (begin
                  (if (syncer-interrupted? sr_0)
                    (begin
                      (set-syncer-interrupted?! sr_0 #f)
                      (let ((retry_0 (syncer-retry sr_0)))
                        (if retry_0
                          (call-with-values
                           (lambda () (|#%app| retry_0))
                           (case-lambda
                            ((result_0 ready?_0)
                             (if ready?_0
                               (begin
                                 (set-syncer-wraps!
                                  sr_0
                                  (cons
                                   (lambda args_0 result_0)
                                   (syncer-wraps sr_0)))
                                 (syncing-done! s_0 sr_0))
                               (void)))
                            (args (raise-binding-result-arity-error 2 args))))
                          (void))))
                    (void))
                  (loop_0 (syncer-next sr_0)))
                (void)))))))
       (loop_0 (syncing-syncers s_0))))))
(define syncing-queue-retry! (lambda (s_0) (set-syncing-need-retry?! s_0 #t)))
(define all-asynchronous?
  (lambda (s_0)
    (begin
      (start-atomic)
      (begin0
        (letrec*
         ((loop_0
           (|#%name|
            loop
            (lambda (sr_0)
              (begin
                (if (not sr_0)
                  #t
                  (let ((e_0 (syncer-evt sr_0)))
                    (if (let ((or-part_0 (async-evt? e_0)))
                          (if or-part_0
                            or-part_0
                            (let ((or-part_1 (never-evt? e_0)))
                              (if or-part_1
                                or-part_1
                                (if (|#%app| nested-sync-evt? e_0)
                                  (let ((s_1 (|#%app| nested-sync-evt-s e_0)))
                                    (if (not (syncing-selected s_1))
                                      (all-asynchronous? s_1)
                                      #f))
                                  #f)))))
                      (if (not (|#%app| evt-impersonator? e_0))
                        (loop_0 (syncer-next sr_0))
                        #f)
                      #f))))))))
         (loop_0 (syncing-syncers s_0)))
        (end-atomic)))))
(define nested-syncings
  (lambda (s_0 orig-s_0)
    (letrec*
     ((loop_0
       (|#%name|
        loop
        (lambda (sr_0)
          (begin
            (if (not sr_0)
              null
              (let ((e_0 (syncer-evt sr_0)))
                (if (|#%app| nested-sync-evt? e_0)
                  (let ((s_1 (|#%app| nested-sync-evt-s e_0)))
                    (begin
                      (set-syncing-wakeup!
                       s_1
                       (lambda () (|#%app| (syncing-wakeup orig-s_0))))
                      (let ((app_0 (nested-syncings s_1 orig-s_0)))
                        (append
                         app_0
                         (cons s_1 (loop_0 (syncer-next sr_0)))))))
                  (loop_0 (syncer-next sr_0))))))))))
     (loop_0 (syncing-syncers s_0)))))
(define suspend-syncing-thread
  (lambda (s_0 timeout-at_0)
    (|#%app|
     (begin
       (start-atomic)
       (begin0
         (letrec*
          ((retry_0
            (|#%name|
             retry
             (lambda ()
               (begin
                 (let ((nss_0 (nested-syncings s_0 s_0)))
                   (if (let ((or-part_0 (syncing-selected s_0)))
                         (if or-part_0
                           or-part_0
                           (begin
                             (letrec*
                              ((for-loop_0
                                (|#%name|
                                 for-loop
                                 (lambda (result_0 lst_0)
                                   (begin
                                     (if (pair? lst_0)
                                       (let ((ns_0 (unsafe-car lst_0)))
                                         (let ((rest_0 (unsafe-cdr lst_0)))
                                           (let ((result_1
                                                  (let ((result_1
                                                         (syncing-selected
                                                          ns_0)))
                                                    (values result_1))))
                                             (if (if (not
                                                      (let ((x_0 (list ns_0)))
                                                        result_1))
                                                   #t
                                                   #f)
                                               (for-loop_0 result_1 rest_0)
                                               result_1))))
                                       result_0))))))
                              (for-loop_0 #f nss_0)))))
                     void
                     (let ((t_0 (current-thread/in-atomic)))
                       (begin
                         (set-syncing-wakeup!
                          s_0
                          (lambda ()
                            (begin
                              (set-syncing-wakeup! s_0 void)
                              (if (thread-descheduled? t_0)
                                (thread-reschedule! t_0)
                                (void)))))
                         (thread-deschedule!
                          t_0
                          timeout-at_0
                          (lambda ()
                            (begin
                              (set-syncing-wakeup! s_0 void)
                              (if (syncing-selected s_0)
                                (void)
                                (syncing-interrupt! s_0))
                              (lambda ()
                                (|#%app|
                                 (begin
                                   (start-atomic)
                                   (begin0
                                     (begin
                                       (if (syncing-selected s_0)
                                         (void)
                                         (syncing-retry! s_0))
                                       (retry_0))
                                     (end-atomic)))))))))))))))))
          (retry_0))
         (end-atomic))))))
(define struct:replacing-evt
  (make-record-type-descriptor* 'evt #f (|#%nongenerative-uid| evt) #f #f 1 0))
(define effect_2315
  (struct-type-install-properties!
   struct:replacing-evt
   '(evt)
   1
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1
      (lambda (self_0 poll-ctx_0) (|#%app| (replacing-evt-guard self_0))))))
   (current-inspector)
   #f
   '(0)
   #f
   'replacing-evt))
(define replacing-evt34.1
  (|#%name|
   replacing-evt
   (record-constructor
    (make-record-constructor-descriptor struct:replacing-evt #f #f))))
(define replacing-evt?_2294
  (|#%name| evt? (record-predicate struct:replacing-evt)))
(define replacing-evt?
  (|#%name|
   evt?
   (lambda (v)
     (if (replacing-evt?_2294 v)
       #t
       ($value
        (if (impersonator? v)
          (replacing-evt?_2294 (impersonator-val v))
          #f))))))
(define replacing-evt-guard_2603
  (|#%name| evt-guard (record-accessor struct:replacing-evt 0)))
(define replacing-evt-guard
  (|#%name|
   evt-guard
   (lambda (s)
     (if (replacing-evt?_2294 s)
       (replacing-evt-guard_2603 s)
       ($value
        (impersonate-ref
         replacing-evt-guard_2603
         struct:replacing-evt
         0
         s
         'evt
         'guard))))))
(define struct:nested-sync-evt
  (make-record-type-descriptor* 'evt #f (|#%nongenerative-uid| evt) #f #f 3 0))
(define effect_2607
  (struct-type-install-properties!
   struct:nested-sync-evt
   '(evt)
   3
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1 (lambda (self_0 poll-ctx_0) (values #f self_0)))))
   (current-inspector)
   #f
   '(0 1 2)
   #f
   'nested-sync-evt))
(define nested-sync-evt35.1
  (|#%name|
   nested-sync-evt
   (record-constructor
    (make-record-constructor-descriptor struct:nested-sync-evt #f #f))))
(define nested-sync-evt?_2757
  (|#%name| evt? (record-predicate struct:nested-sync-evt)))
(define nested-sync-evt?
  (|#%name|
   evt?
   (lambda (v)
     (if (nested-sync-evt?_2757 v)
       #t
       ($value
        (if (impersonator? v)
          (nested-sync-evt?_2757 (impersonator-val v))
          #f))))))
(define nested-sync-evt-s_2315
  (|#%name| evt-s (record-accessor struct:nested-sync-evt 0)))
(define nested-sync-evt-s
  (|#%name|
   evt-s
   (lambda (s)
     (if (nested-sync-evt?_2757 s)
       (nested-sync-evt-s_2315 s)
       ($value
        (impersonate-ref
         nested-sync-evt-s_2315
         struct:nested-sync-evt
         0
         s
         'evt
         's))))))
(define nested-sync-evt-next_2802
  (|#%name| evt-next (record-accessor struct:nested-sync-evt 1)))
(define nested-sync-evt-next
  (|#%name|
   evt-next
   (lambda (s)
     (if (nested-sync-evt?_2757 s)
       (nested-sync-evt-next_2802 s)
       ($value
        (impersonate-ref
         nested-sync-evt-next_2802
         struct:nested-sync-evt
         1
         s
         'evt
         'next))))))
(define nested-sync-evt-orig-evt_2620
  (|#%name| evt-orig-evt (record-accessor struct:nested-sync-evt 2)))
(define nested-sync-evt-orig-evt
  (|#%name|
   evt-orig-evt
   (lambda (s)
     (if (nested-sync-evt?_2757 s)
       (nested-sync-evt-orig-evt_2620 s)
       ($value
        (impersonate-ref
         nested-sync-evt-orig-evt_2620
         struct:nested-sync-evt
         2
         s
         'evt
         'orig-evt))))))
(define 1/replace-evt
  (|#%name|
   replace-evt
   (lambda (evt_0 next_0)
     (begin
       (begin
         (if (1/evt? evt_0)
           (void)
           (raise-argument-error 'replace-evt "evt?" evt_0))
         (begin
           (if (procedure? next_0)
             (void)
             (raise-argument-error 'replace-evt "procedure?" next_0))
           (letrec*
            ((orig-evt_0
              (replacing-evt34.1
               (lambda ()
                 (let ((s_0
                        (let ((temp89_0
                               (let ((app_0 evts->syncers))
                                 (|#%app| app_0 'replace-evt (list evt_0)))))
                          (make-syncing.1 #f temp89_0))))
                   (values
                    #f
                    (control-state-evt9.1
                     (nested-sync-evt35.1 s_0 next_0 orig-evt_0)
                     values
                     (lambda () (syncing-interrupt! s_0))
                     (lambda () (syncing-abandon! s_0))
                     (lambda () (syncing-retry! s_0)))))))))
            orig-evt_0)))))))
(define poll-nested-sync
  (lambda (ns_0 just-poll?_0 fast-only?_0 sched-info_0)
    (let ((temp90_0 (|#%app| nested-sync-evt-s ns_0)))
      (let ((temp91_0
             (lambda (sched-info_1 polled-all?_0 no-wrappers?_0)
               (values polled-all?_0 ns_0))))
        (let ((temp92_0
               (lambda (thunk_0)
                 (let ((next_0 (nested-sync-evt-next ns_0)))
                   (let ((orig-evt_0 (nested-sync-evt-orig-evt ns_0)))
                     (values
                      #f
                      (control-state-evt9.1
                       (poll-guard-evt10.1
                        (lambda (poll?_0)
                          (let ((r_0 (|#%call-with-values| thunk_0 next_0)))
                            (if (1/evt? r_0)
                              r_0
                              (wrap-evt7.1
                               the-always-evt
                               (lambda (v_0) orig-evt_0))))))
                       values
                       'reset
                       void
                       'reset)))))))
          (let ((temp91_1 temp91_0) (temp90_1 temp90_0))
            (sync-poll.1
             #f
             #f
             temp91_1
             fast-only?_0
             just-poll?_0
             sched-info_0
             temp92_0
             temp90_1)))))))
(define 1/current-evt-pseudo-random-generator
  (make-parameter
   (make-pseudo-random-generator)
   (lambda (v_0)
     (begin
       (if (pseudo-random-generator? v_0)
         (void)
         (raise-argument-error
          'current-evt-pseudo-random-generator
          "pseudo-random-generator?"
          v_0))
       v_0))
   'current-evt-pseudo-random-generator))
(define init-sync-place!
  (lambda ()
    (1/current-evt-pseudo-random-generator (make-pseudo-random-generator))))
(define random-rotate
  (lambda (first-sr_0)
    (let ((n_0
           (letrec*
            ((loop_0
              (|#%name|
               loop
               (lambda (sr_0 n_0)
                 (begin
                   (if (not sr_0)
                     n_0
                     (let ((app_0 (syncer-next sr_0)))
                       (loop_0 app_0 (add1 n_0)))))))))
            (loop_0 first-sr_0 0))))
      (if (<= n_0 1)
        first-sr_0
        (let ((m_0 (-random n_0 (1/current-evt-pseudo-random-generator))))
          (if (zero? m_0)
            first-sr_0
            (letrec*
             ((loop_0
               (|#%name|
                loop
                (lambda (sr_0 m_1)
                  (begin
                    (if (zero? m_1)
                      (let ((new-first-sr_0 (syncer-next sr_0)))
                        (begin
                          (set-syncer-next! sr_0 #f)
                          (set-syncer-prev! new-first-sr_0 #f)
                          (letrec*
                           ((loop_1
                             (|#%name|
                              loop
                              (lambda (next-sr_0)
                                (begin
                                  (let ((next-next-sr_0
                                         (syncer-next next-sr_0)))
                                    (if (not next-next-sr_0)
                                      (begin
                                        (set-syncer-next! next-sr_0 first-sr_0)
                                        (set-syncer-prev! first-sr_0 next-sr_0)
                                        new-first-sr_0)
                                      (loop_1 next-next-sr_0))))))))
                           (loop_1 new-first-sr_0))))
                      (let ((app_0 (syncer-next sr_0)))
                        (loop_0 app_0 (sub1 m_1)))))))))
             (loop_0 first-sr_0 (sub1 m_0)))))))))
(define cell.1$5 (unsafe-make-place-local (1/make-semaphore)))
(define cell.2$3
  (unsafe-make-place-local
   (wrap-evt7.1 (unsafe-place-local-ref cell.1$5) void)))
(define struct:system-idle-evt
  (make-record-type-descriptor*
   'system-idle-evt
   #f
   (|#%nongenerative-uid| system-idle-evt)
   #f
   #f
   0
   0))
(define effect_2854
  (struct-type-install-properties!
   struct:system-idle-evt
   '(system-idle-evt)
   0
   0
   #f
   (list (cons 1/prop:evt (lambda (i_0) (unsafe-place-local-ref cell.2$3))))
   (current-inspector)
   #f
   '()
   #f
   'system-idle-evt))
(define system-idle-evt1.1
  (|#%name|
   system-idle-evt
   (record-constructor
    (make-record-constructor-descriptor struct:system-idle-evt #f #f))))
(define system-idle-evt?_2250
  (|#%name| system-idle-evt? (record-predicate struct:system-idle-evt)))
(define system-idle-evt?
  (|#%name|
   system-idle-evt?
   (lambda (v)
     (if (system-idle-evt?_2250 v)
       #t
       ($value
        (if (impersonator? v)
          (system-idle-evt?_2250 (impersonator-val v))
          #f))))))
(define the-idle-evt (system-idle-evt1.1))
(define get-system-idle-evt
  (|#%name| system-idle-evt (lambda () (begin the-idle-evt))))
(define any-idle-waiters?
  (lambda ()
    (let ((s_0 (unsafe-place-local-ref cell.1$5)))
      (begin-unsafe (not (begin-unsafe (not (queue-start s_0))))))))
(define post-idle
  (lambda ()
    (if (let ((s_0 (unsafe-place-local-ref cell.1$5)))
          (begin-unsafe (not (begin-unsafe (not (queue-start s_0))))))
      (begin (semaphore-post/atomic (unsafe-place-local-ref cell.1$5)) #t)
      #f)))
(define init-system-idle-evt!
  (lambda ()
    (begin
      (unsafe-place-local-set! cell.1$5 (1/make-semaphore))
      (unsafe-place-local-set!
       cell.2$3
       (wrap-evt7.1 (unsafe-place-local-ref cell.1$5) void)))))
(define TICKS 100000)
(define set-schedule-quantum! (lambda (n_0) (set! TICKS n_0)))
(define struct:future*
  (make-record-type-descriptor*
   'future
   #f
   (|#%nongenerative-uid| future)
   #f
   #f
   10
   1016))
(define effect_3020
  (struct-type-install-properties!
   struct:future*
   '(future)
   10
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(0 1 2)
   #f
   'future*))
(define future*1.1
  (|#%name|
   future*
   (record-constructor
    (make-record-constructor-descriptor struct:future* #f #f))))
(define future*? (|#%name| future? (record-predicate struct:future*)))
(define future*-id (|#%name| future-id (record-accessor struct:future* 0)))
(define future*-lock (|#%name| future-lock (record-accessor struct:future* 1)))
(define future*-custodian
  (|#%name| future-custodian (record-accessor struct:future* 2)))
(define future*-would-be?
  (|#%name| future-would-be? (record-accessor struct:future* 3)))
(define future*-thunk
  (|#%name| future-thunk (record-accessor struct:future* 4)))
(define future*-prev (|#%name| future-prev (record-accessor struct:future* 5)))
(define future*-next (|#%name| future-next (record-accessor struct:future* 6)))
(define future*-results
  (|#%name| future-results (record-accessor struct:future* 7)))
(define future*-state
  (|#%name| future-state (record-accessor struct:future* 8)))
(define future*-dependents
  (|#%name| future-dependents (record-accessor struct:future* 9)))
(define set-future*-would-be?!
  (|#%name| set-future-would-be?! (record-mutator struct:future* 3)))
(define set-future*-thunk!
  (|#%name| set-future-thunk! (record-mutator struct:future* 4)))
(define set-future*-prev!
  (|#%name| set-future-prev! (record-mutator struct:future* 5)))
(define set-future*-next!
  (|#%name| set-future-next! (record-mutator struct:future* 6)))
(define set-future*-results!
  (|#%name| set-future-results! (record-mutator struct:future* 7)))
(define set-future*-state!
  (|#%name| set-future-state! (record-mutator struct:future* 8)))
(define set-future*-dependents!
  (|#%name| set-future-dependents! (record-mutator struct:future* 9)))
(define currently-running-future-key (gensym 'future))
(define currently-running-future
  (lambda ()
    (continuation-mark-set-first
     #f
     currently-running-future-key
     #f
     (unsafe-root-continuation-prompt-tag))))
(define ID (box 1))
(define get-next-id
  (lambda ()
    (let ((id_0 (unbox ID)))
      (if (unsafe-box*-cas! ID id_0 (+ 1 id_0)) id_0 (get-next-id)))))
(define make-lock (lambda () (box 0)))
(define start-future-uninterrupted
  (lambda ()
    (if (current-future$1)
      (current-atomic (add1 (current-atomic)))
      (start-atomic))))
(define end-future-uninterrupted
  (lambda ()
    (if (current-future$1)
      (current-atomic (sub1 (current-atomic)))
      (end-atomic))))
(define lock-acquire
  (lambda (lock_0)
    (begin
      (start-future-uninterrupted)
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda ()
            (begin
              (if (unsafe-box*-cas! lock_0 0 1)
                (memory-order-acquire)
                (loop_0)))))))
       (loop_0)))))
(define lock-release
  (lambda (lock_0)
    (if (unsafe-box*-cas! lock_0 1 0)
      (begin (memory-order-release) (end-future-uninterrupted))
      (if (eq? (unbox lock_0) 0)
        (internal-error "lock release failed!")
        (lock-release lock_0)))))
(define struct:future-event
  (make-record-type-descriptor*
   'future-event
   #f
   (structure-type-lookup-prefab-uid 'future-event #f 6 0 #f '(0 1 2 3 4 5))
   #f
   #f
   6
   63))
(define effect_2966
  (struct-type-install-properties!
   struct:future-event
   '(future-event)
   6
   0
   #f
   null
   'prefab
   #f
   '(0 1 2 3 4 5)
   #f
   'future-event))
(define future-event1.1
  (|#%name|
   future-event
   (record-constructor
    (make-record-constructor-descriptor struct:future-event #f #f))))
(define future-event?_2528
  (|#%name| future-event? (record-predicate struct:future-event)))
(define future-event?
  (|#%name|
   future-event?
   (lambda (v)
     (if (future-event?_2528 v)
       #t
       ($value
        (if (impersonator? v)
          (future-event?_2528 (impersonator-val v))
          #f))))))
(define future-event-future-id_2042
  (|#%name| future-event-future-id (record-accessor struct:future-event 0)))
(define future-event-future-id
  (|#%name|
   future-event-future-id
   (lambda (s)
     (if (future-event?_2528 s)
       (future-event-future-id_2042 s)
       ($value
        (impersonate-ref
         future-event-future-id_2042
         struct:future-event
         0
         s
         'future-event
         'future-id))))))
(define future-event-proc-id_3059
  (|#%name| future-event-proc-id (record-accessor struct:future-event 1)))
(define future-event-proc-id
  (|#%name|
   future-event-proc-id
   (lambda (s)
     (if (future-event?_2528 s)
       (future-event-proc-id_3059 s)
       ($value
        (impersonate-ref
         future-event-proc-id_3059
         struct:future-event
         1
         s
         'future-event
         'proc-id))))))
(define future-event-action_1972
  (|#%name| future-event-action (record-accessor struct:future-event 2)))
(define future-event-action
  (|#%name|
   future-event-action
   (lambda (s)
     (if (future-event?_2528 s)
       (future-event-action_1972 s)
       ($value
        (impersonate-ref
         future-event-action_1972
         struct:future-event
         2
         s
         'future-event
         'action))))))
(define future-event-time_3029
  (|#%name| future-event-time (record-accessor struct:future-event 3)))
(define future-event-time
  (|#%name|
   future-event-time
   (lambda (s)
     (if (future-event?_2528 s)
       (future-event-time_3029 s)
       ($value
        (impersonate-ref
         future-event-time_3029
         struct:future-event
         3
         s
         'future-event
         'time))))))
(define future-event-prim-name_2605
  (|#%name| future-event-prim-name (record-accessor struct:future-event 4)))
(define future-event-prim-name
  (|#%name|
   future-event-prim-name
   (lambda (s)
     (if (future-event?_2528 s)
       (future-event-prim-name_2605 s)
       ($value
        (impersonate-ref
         future-event-prim-name_2605
         struct:future-event
         4
         s
         'future-event
         'prim-name))))))
(define future-event-user-data_2504
  (|#%name| future-event-user-data (record-accessor struct:future-event 5)))
(define future-event-user-data
  (|#%name|
   future-event-user-data
   (lambda (s)
     (if (future-event?_2528 s)
       (future-event-user-data_2504 s)
       ($value
        (impersonate-ref
         future-event-user-data_2504
         struct:future-event
         5
         s
         'future-event
         'user-data))))))
(define cell.1$4 (unsafe-make-place-local (box null)))
(define init-future-logging-place!
  (lambda () (unsafe-place-local-set! cell.1$4 (box null))))
(define log-future.1
  (|#%name|
   log-future
   (lambda (data3_0 prim-name2_0 action7_0 future-id6_0)
     (begin
       (let ((c1_0 (current-future$1)))
         (if c1_0
           (let ((e_0
                  (let ((app_0
                         (if future-id6_0 future-id6_0 (future*-id c1_0))))
                    (let ((app_1 (|#%app| get-pthread-id)))
                      (future-event1.1
                       app_0
                       app_1
                       action7_0
                       (current-inexact-milliseconds)
                       prim-name2_0
                       data3_0)))))
             (letrec*
              ((loop_0
                (|#%name|
                 loop
                 (lambda ()
                   (begin
                     (let ((old-events_0
                            (unbox (unsafe-place-local-ref cell.1$4))))
                       (if (unsafe-box*-cas!
                            (unsafe-place-local-ref cell.1$4)
                            old-events_0
                            (cons e_0 old-events_0))
                         (void)
                         (loop_0))))))))
              (loop_0)))
           (if (begin-unsafe (|#%app| logging-future-events?))
             (begin
               (flush-future-log)
               (let ((id_0
                      (if future-id6_0
                        future-id6_0
                        (let ((f_0 (currently-running-future)))
                          (if f_0 (future*-id f_0) #f)))))
                 (log-future-event*
                  (future-event1.1
                   id_0
                   0
                   action7_0
                   (current-inexact-milliseconds)
                   prim-name2_0
                   data3_0))))
             (void))))))))
(define logging-futures? (lambda () (|#%app| logging-future-events?)))
(define flush-future-log
  (lambda ()
    (let ((new-events_0 (unsafe-unbox* (unsafe-place-local-ref cell.1$4))))
      (if (null? new-events_0)
        (void)
        (if (unsafe-box*-cas!
             (unsafe-place-local-ref cell.1$4)
             new-events_0
             null)
          (if (begin-unsafe (|#%app| logging-future-events?))
            (begin
              (let ((lst_0 (reverse$1 new-events_0)))
                (begin
                  (letrec*
                   ((for-loop_0
                     (|#%name|
                      for-loop
                      (lambda (lst_1)
                        (begin
                          (if (pair? lst_1)
                            (let ((e_0 (unsafe-car lst_1)))
                              (let ((rest_0 (unsafe-cdr lst_1)))
                                (begin
                                  (log-future-event* e_0)
                                  (for-loop_0 rest_0))))
                            (values)))))))
                   (for-loop_0 lst_0))))
              (void))
            (void))
          (flush-future-log))))))
(define log-future-event*
  (lambda (e_0)
    (let ((proc-id_0 (future-event-proc-id e_0)))
      (let ((action_0 (future-event-action e_0)))
        (let ((msg_0
               (let ((app_0
                      (let ((id_0 (future-event-future-id e_0)))
                        (if id_0
                          (number->string (future-event-future-id e_0))
                          "-1"))))
                 (let ((app_1 (number->string proc-id_0)))
                   (let ((app_2
                          (if (if (eqv? proc-id_0 0)
                                (let ((or-part_0 (eq? action_0 'block)))
                                  (if or-part_0
                                    or-part_0
                                    (eq? action_0 'sync)))
                                #f)
                            (let ((app_2
                                   (if (eq? action_0 'block)
                                     "HANDLING: "
                                     "synchronizing: ")))
                              (string-append
                               app_2
                               (symbol->string
                                (let ((or-part_0 (future-event-prim-name e_0)))
                                  (if or-part_0 or-part_0 '|[unknown]|)))))
                            (action->string action_0))))
                     (string-append
                      "id "
                      app_0
                      ", process "
                      app_1
                      ": "
                      app_2
                      "; time: "
                      (number->string (future-event-time e_0))))))))
          (|#%app| log-future-event msg_0 e_0))))))
(define action->string
  (lambda (a_0)
    (if (eq? a_0 'create)
      "created"
      (if (eq? a_0 'complete)
        "completed"
        (if (eq? a_0 'start-work)
          "started work"
          (if (eq? a_0 'end-work)
            "ended work"
            (if (eq? a_0 'block)
              "BLOCKING on process 0"
              (if (eq? a_0 'sync)
                "synchronizing with process 0"
                (if (eq? a_0 'touch)
                  "touching future: touch"
                  (if (eq? a_0 'result)
                    "result determined"
                    (if (eq? a_0 'suspend)
                      "suspended"
                      (if (eq? a_0 'touch-pause)
                        "paused for touch"
                        (if (eq? a_0 'touch-resume)
                          "resumed for touch"
                          "[unknown action]")))))))))))))
(define logging-future-events? (lambda () #f))
(define log-future-event (lambda (msg_0 e_0) (void)))
(define install-future-logging-procs!
  (lambda (logging?_0 log_0)
    (begin
      (set! logging-future-events? logging?_0)
      (set! log-future-event log_0))))
(define init-future-place! (lambda () (init-future-logging-place!)))
(define 1/futures-enabled?
  (|#%name| futures-enabled? (lambda () (begin (|#%app| threaded?)))))
(define struct:future-evt
  (make-record-type-descriptor*
   'future-evt
   #f
   (|#%nongenerative-uid| future-evt)
   #f
   #f
   1
   0))
(define effect_2519
  (struct-type-install-properties!
   struct:future-evt
   '(future-evt)
   1
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1
      (lambda (fe_0 poll-ctx_0)
        (let ((f_0 (future-evt-future fe_0)))
          (begin
            (lock-acquire (future*-lock f_0))
            (let ((s_0 (future*-state f_0)))
              (begin
                (lock-release (future*-lock f_0))
                (if (let ((or-part_0 (eq? s_0 'running)))
                      (if or-part_0 or-part_0 (eq? s_0 'fsema)))
                  (values #f fe_0)
                  (values '(#t) #f))))))))))
   (current-inspector)
   #f
   '(0)
   #f
   'future-evt))
(define future-evt1.1
  (|#%name|
   future-evt
   (record-constructor
    (make-record-constructor-descriptor struct:future-evt #f #f))))
(define future-evt?_2974
  (|#%name| future-evt? (record-predicate struct:future-evt)))
(define future-evt?
  (|#%name|
   future-evt?
   (lambda (v)
     (if (future-evt?_2974 v)
       #t
       ($value
        (if (impersonator? v) (future-evt?_2974 (impersonator-val v)) #f))))))
(define future-evt-future_3037
  (|#%name| future-evt-future (record-accessor struct:future-evt 0)))
(define future-evt-future
  (|#%name|
   future-evt-future
   (lambda (s)
     (if (future-evt?_2974 s)
       (future-evt-future_3037 s)
       ($value
        (impersonate-ref
         future-evt-future_3037
         struct:future-evt
         0
         s
         'future-evt
         'future))))))
(define create-future
  (lambda (thunk_0 cust_0 would-be?_0)
    (let ((id_0 (get-next-id)))
      (begin
        (log-future.1 id_0 #f 'create #f)
        (future*1.1
         id_0
         (make-lock)
         cust_0
         would-be?_0
         thunk_0
         #f
         #f
         #f
         #f
         hash2610)))))
(define 1/future? (|#%name| future? (lambda (v_0) (begin (future*? v_0)))))
(define future-scheduler-prompt-tag
  (make-continuation-prompt-tag 'future-scheduler))
(define future-start-prompt-tag (make-continuation-prompt-tag 'future-star))
(define current-future-prompt
  (lambda ()
    (if (current-future$1)
      future-scheduler-prompt-tag
      (internal-error "not running in a future"))))
(define run-future.1
  (|#%name|
   run-future
   (lambda (was-blocked?2_0 f4_0)
     (begin
       (begin
         (set-future*-state! f4_0 'running)
         (let ((thunk_0 (future*-thunk f4_0)))
           (begin
             (set-future*-thunk! f4_0 #f)
             (begin
               (lock-release (future*-lock f4_0))
               (begin
                 (if was-blocked?2_0
                   (if (begin-unsafe (|#%app| logging-future-events?))
                     (begin
                       (let ((temp17_0 (future*-id f4_0)))
                         (let ((temp18_0
                                (|#%app|
                                 continuation-current-primitive
                                 thunk_0
                                 '(unsafe-start-atomic))))
                           (let ((temp17_1 temp17_0))
                             (log-future.1 #f temp18_0 'block temp17_1))))
                       (let ((temp20_0 (future*-id f4_0)))
                         (log-future.1 #f #f 'result temp20_0)))
                     (void))
                   (void))
                 (begin
                   (if (eq? (future*-would-be? f4_0) 'blocked)
                     (void)
                     (let ((temp22_0 (future*-id f4_0)))
                       (log-future.1 #f #f 'start-work temp22_0)))
                   (let ((finish!_0
                          (|#%name|
                           finish!
                           (lambda (results_0 state_0)
                             (begin
                               (begin
                                 (start-future-uninterrupted)
                                 (begin
                                   (lock-acquire (future*-lock f4_0))
                                   (begin
                                     (set-future*-results! f4_0 results_0)
                                     (begin
                                       (set-future*-state! f4_0 state_0)
                                       (let ((deps_0
                                              (future*-dependents f4_0)))
                                         (begin
                                           (set-future*-dependents!
                                            f4_0
                                            hash2610)
                                           (lock-release (future*-lock f4_0))
                                           (future-notify-dependents deps_0)
                                           (end-future-uninterrupted)
                                           (let ((temp24_0 (future*-id f4_0)))
                                             (log-future.1
                                              #f
                                              #f
                                              'complete
                                              temp24_0)))))))))))))
                     (if (current-future$1)
                       (call-with-values
                        (lambda ()
                          (call-with-continuation-prompt
                           (lambda ()
                             (begin
                               (current-atomic (sub1 (current-atomic)))
                               (|#%app| thunk_0)))
                           future-start-prompt-tag
                           (lambda args_0 (void))))
                        (lambda results_0 (finish!_0 results_0 'done)))
                       (if (eq? (future*-would-be? f4_0) #t)
                         (call-with-values
                          (lambda ()
                            (call-with-continuation-prompt
                             (lambda ()
                               (begin
                                 (current-future$1 f4_0)
                                 (begin0
                                   (|#%app| thunk_0)
                                   (current-future$1 #f))))
                             future-start-prompt-tag
                             (lambda args_0
                               (begin
                                 (set-future*-would-be?! f4_0 'blocked)
                                 (1/touch f4_0)))))
                          (lambda results_0
                            (if (eq? (future*-state f4_0) 'running)
                              (begin
                                (finish!_0 results_0 'done)
                                (let ((temp26_0 (future*-id f4_0)))
                                  (log-future.1 #f #f 'end-work temp26_0)))
                              (void))))
                         (dynamic-wind
                          (lambda () (void))
                          (lambda ()
                            (with-continuation-mark*
                             general
                             currently-running-future-key
                             f4_0
                             (|#%call-with-values|
                              thunk_0
                              (lambda results_0 (finish!_0 results_0 'done)))))
                          (lambda ()
                            (begin
                              (if (eq? (future*-state f4_0) 'done)
                                (void)
                                (finish!_0 #f 'aborted))
                              (let ((temp28_0 (future*-id f4_0)))
                                (log-future.1
                                 #f
                                 #f
                                 'end-work
                                 temp28_0))))))))))))))))))
(define 1/future
  (|#%name|
   future
   (lambda (thunk_0)
     (begin
       (begin
         (if (if (procedure? thunk_0) (procedure-arity-includes? thunk_0 0) #f)
           (void)
           (raise-argument-error
            'future
            "(procedure-arity-includes/c 0)"
            thunk_0))
         (if (not (1/futures-enabled?))
           (1/would-be-future thunk_0)
           (let ((me-f_0 (current-future$1)))
             (let ((cust_0 (future-custodian me-f_0)))
               (let ((f_0 (create-future thunk_0 cust_0 #f)))
                 (begin
                   (if cust_0
                     (begin
                       (if me-f_0
                         (void)
                         (begin
                           (maybe-start-scheduler)
                           (set-custodian-sync-futures?! cust_0 #t)))
                       (schedule-future!.1 #f f_0))
                     (void))
                   f_0))))))))))
(define 1/would-be-future
  (|#%name|
   would-be-future
   (lambda (thunk_0)
     (begin
       (begin
         (if (if (procedure? thunk_0) (procedure-arity-includes? thunk_0 0) #f)
           (void)
           (raise-argument-error
            'would-be-future
            "(procedure-arity-includes/c 0)"
            thunk_0))
         (|#%app| ensure-place-wakeup-handle)
         (create-future thunk_0 (future-custodian (current-future$1)) #t))))))
(define future-custodian
  (lambda (me-f_0)
    (if me-f_0
      (future*-custodian me-f_0)
      (thread-representative-custodian (current-thread/in-atomic)))))
(define lock-acquire-both
  (lambda (f_0)
    (let ((me-f_0 (current-future$1)))
      (if (let ((or-part_0 (not me-f_0)))
            (if or-part_0 or-part_0 (eq? me-f_0 f_0)))
        (lock-acquire (future*-lock f_0))
        (if (< (future*-id me-f_0) (future*-id f_0))
          (begin
            (lock-acquire (future*-lock me-f_0))
            (lock-acquire (future*-lock f_0)))
          (begin
            (lock-acquire (future*-lock f_0))
            (lock-acquire (future*-lock me-f_0))))))))
(define lock-release-both
  (lambda (f_0)
    (begin (lock-release-current) (lock-release (future*-lock f_0)))))
(define lock-release-current
  (lambda ()
    (let ((me-f_0 (current-future$1)))
      (if me-f_0 (lock-release (future*-lock me-f_0)) (void)))))
(define 1/touch
  (|#%name|
   touch
   (lambda (f_0)
     (begin
       (begin
         (if (future*? f_0)
           (void)
           (raise-argument-error 'touch "future*?" f_0))
         (begin
           (lock-acquire-both f_0)
           (let ((s_0 (future*-state f_0)))
             (if (eq? s_0 'done)
               (begin
                 (lock-release-both f_0)
                 (apply values (future*-results f_0)))
               (if (eq? s_0 'aborted)
                 (begin
                   (lock-release-both f_0)
                   (raise
                    (|#%app|
                     exn:fail
                     "touch: future previously aborted"
                     (current-continuation-marks))))
                 (if (eq? s_0 'blocked)
                   (if (current-future$1)
                     (dependent-on-future f_0)
                     (begin
                       (run-future.1 #t f_0)
                       (apply values (future*-results f_0))))
                   (if (eq? s_0 #f)
                     (if (current-future$1)
                       (dependent-on-future f_0)
                       (if (future*-would-be? f_0)
                         (begin
                           (lock-release-current)
                           (run-future.1 #f f_0)
                           (apply values (future*-results f_0)))
                         (begin
                           (lock-release (future*-lock f_0))
                           (if (try-deschedule-future? f_0)
                             (begin
                               (run-future.1 #f f_0)
                               (apply values (future*-results f_0)))
                             (1/touch f_0)))))
                     (if (eq? s_0 'running)
                       (if (current-future$1)
                         (dependent-on-future f_0)
                         (begin
                           (set-future*-dependents!
                            f_0
                            (hash-set (future*-dependents f_0) 'place #t))
                           (lock-release (future*-lock f_0))
                           (let ((temp38_0 (future*-id f_0)))
                             (log-future.1 #f #f 'touch-pause temp38_0))
                           (1/sync (future-evt1.1 f_0))
                           (let ((temp40_0 (future*-id f_0)))
                             (log-future.1 #f #f 'touch-resume temp40_0))
                           (1/touch f_0)))
                       (if (begin-unsafe (future*? s_0))
                         (if (current-future$1)
                           (dependent-on-future f_0)
                           (begin
                             (lock-release (future*-lock f_0))
                             (1/touch s_0)
                             (1/touch f_0)))
                         (if (let ((or-part_0 (box? s_0)))
                               (if or-part_0 or-part_0 (eq? s_0 'fsema)))
                           (if (current-future$1)
                             (dependent-on-future f_0)
                             (begin
                               (lock-release (future*-lock f_0))
                               (let ((temp42_0 (future*-id f_0)))
                                 (log-future.1 #f #f 'touch-pause temp42_0))
                               (1/sync (future-evt1.1 f_0))
                               (let ((temp44_0 (future*-id f_0)))
                                 (log-future.1 #f #f 'touch-resume temp44_0))
                               (1/touch f_0)))
                           (begin
                             (lock-release (future*-lock f_0))
                             (internal-error
                              "unrecognized future state"))))))))))))))))
(define dependent-on-future
  (lambda (f_0)
    (let ((me-f_0 (current-future$1)))
      (begin
        (set-future*-dependents!
         f_0
         (hash-set (future*-dependents f_0) me-f_0 #t))
        (set-future*-state! me-f_0 f_0)
        (on-transition-to-unfinished)
        (if (eq? me-f_0 f_0) (void) (lock-release (future*-lock f_0)))
        (future-suspend f_0)
        (1/touch f_0)))))
(define future-block
  (lambda ()
    (let ((me-f_0 (current-future$1)))
      (begin
        (if (future*-would-be? me-f_0)
          (void)
          (let ((temp46_0 (future*-id me-f_0)))
            (log-future.1 #f #f 'block temp46_0)))
        (lock-acquire (future*-lock me-f_0))
        (set-future*-state! me-f_0 'blocked)
        (on-transition-to-unfinished)
        (future-suspend)))))
(define future-suspend
  (let ((future-suspend_0
         (|#%name|
          future-suspend
          (lambda (touching-f6_0)
            (begin
              (let ((me-f_0 (current-future$1)))
                (call-with-composable-continuation
                 (lambda (k_0)
                   (begin
                     (set-future*-thunk! me-f_0 k_0)
                     (lock-release (future*-lock me-f_0))
                     (if touching-f6_0
                       (let ((temp48_0 (future*-id me-f_0)))
                         (let ((temp49_0 (future*-id touching-f6_0)))
                           (log-future.1 temp49_0 #f 'touch temp48_0)))
                       (void))
                     (if (future*-would-be? me-f_0)
                       (void)
                       (let ((temp51_0 (future*-id me-f_0)))
                         (log-future.1 #f #f 'suspend temp51_0)))
                     (if (future*-would-be? me-f_0)
                       (begin
                         (current-future$1 #f)
                         (abort-current-continuation
                          future-start-prompt-tag
                          (void)))
                       (abort-current-continuation
                        future-scheduler-prompt-tag
                        (void)))))
                 future-start-prompt-tag)))))))
    (case-lambda
     (() (future-suspend_0 #f))
     ((touching-f6_0) (future-suspend_0 touching-f6_0)))))
(define future-sync
  (lambda (who_0 thunk_0)
    (let ((me-f_0 (current-future$1)))
      (if (future*-would-be? me-f_0)
        (begin
          (current-future$1 #f)
          (let ((temp53_0 (future*-id me-f_0)))
            (log-future.1 #f who_0 'sync temp53_0))
          (let ((v_0 (|#%app| thunk_0)))
            (begin
              (let ((temp56_0 (future*-id me-f_0)))
                (log-future.1 #f #f 'result temp56_0))
              (current-future$1 me-f_0)
              v_0)))
        (begin
          (engine-block)
          (|#%app|
           host:call-as-asynchronous-callback
           (lambda ()
             (begin
               (let ((temp58_0 (future*-id me-f_0)))
                 (log-future.1 #f who_0 'sync temp58_0))
               (let ((v_0 (|#%app| thunk_0)))
                 (begin
                   (let ((temp61_0 (future*-id me-f_0)))
                     (log-future.1 #f #f 'result temp61_0))
                   v_0))))))))))
(define pthread-count 1)
(define set-processor-count! (lambda (n_0) (set! pthread-count n_0)))
(define struct:scheduler
  (make-record-type-descriptor*
   'scheduler
   #f
   (|#%nongenerative-uid| scheduler)
   #f
   #f
   6
   7))
(define effect_2452
  (struct-type-install-properties!
   struct:scheduler
   '(scheduler)
   6
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(3 4 5)
   #f
   'scheduler))
(define scheduler7.1
  (|#%name|
   scheduler
   (record-constructor
    (make-record-constructor-descriptor struct:scheduler #f #f))))
(define scheduler? (|#%name| scheduler? (record-predicate struct:scheduler)))
(define scheduler-workers
  (|#%name| scheduler-workers (record-accessor struct:scheduler 0)))
(define scheduler-futures-head
  (|#%name| scheduler-futures-head (record-accessor struct:scheduler 1)))
(define scheduler-futures-tail
  (|#%name| scheduler-futures-tail (record-accessor struct:scheduler 2)))
(define scheduler-mutex
  (|#%name| scheduler-mutex (record-accessor struct:scheduler 3)))
(define scheduler-cond
  (|#%name| scheduler-cond (record-accessor struct:scheduler 4)))
(define scheduler-ping-cond
  (|#%name| scheduler-ping-cond (record-accessor struct:scheduler 5)))
(define set-scheduler-workers!
  (|#%name| set-scheduler-workers! (record-mutator struct:scheduler 0)))
(define set-scheduler-futures-head!
  (|#%name| set-scheduler-futures-head! (record-mutator struct:scheduler 1)))
(define set-scheduler-futures-tail!
  (|#%name| set-scheduler-futures-tail! (record-mutator struct:scheduler 2)))
(define struct:worker
  (make-record-type-descriptor*
   'worker
   #f
   (|#%nongenerative-uid| worker)
   #f
   #f
   5
   26))
(define effect_2639
  (struct-type-install-properties!
   struct:worker
   '(worker)
   5
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(0 2)
   #f
   'worker))
(define worker8.1
  (|#%name|
   worker
   (record-constructor
    (make-record-constructor-descriptor struct:worker #f #f))))
(define worker? (|#%name| worker? (record-predicate struct:worker)))
(define worker-id (|#%name| worker-id (record-accessor struct:worker 0)))
(define worker-pthread
  (|#%name| worker-pthread (record-accessor struct:worker 1)))
(define worker-current-future-box
  (|#%name| worker-current-future-box (record-accessor struct:worker 2)))
(define worker-die? (|#%name| worker-die? (record-accessor struct:worker 3)))
(define worker-ping (|#%name| worker-ping (record-accessor struct:worker 4)))
(define set-worker-pthread!
  (|#%name| set-worker-pthread! (record-mutator struct:worker 1)))
(define set-worker-die?!
  (|#%name| set-worker-die?! (record-mutator struct:worker 3)))
(define set-worker-ping!
  (|#%name| set-worker-ping! (record-mutator struct:worker 4)))
(define current-scheduler
  (case-lambda
   (() (place-future-scheduler (unsafe-place-local-ref cell.1$2)))
   ((s_0)
    (set-place-future-scheduler! (unsafe-place-local-ref cell.1$2) s_0))))
(define make-worker (lambda (id_0) (worker8.1 id_0 #f (box #f) #f (box #f))))
(define maybe-start-scheduler
  (lambda ()
    (begin
      (start-atomic)
      (begin0
        (if (current-scheduler)
          (void)
          (begin
            (|#%app| ensure-place-wakeup-handle)
            (let ((s_0
                   (let ((app_0 (|#%app| host:make-mutex)))
                     (let ((app_1 (|#%app| host:make-condition)))
                       (scheduler7.1
                        '()
                        #f
                        #f
                        app_0
                        app_1
                        (|#%app| host:make-condition))))))
              (begin
                (current-scheduler s_0)
                (let ((workers_0
                       (reverse$1
                        (let ((end_0 (add1 pthread-count)))
                          (begin
                            (letrec*
                             ((for-loop_0
                               (|#%name|
                                for-loop
                                (lambda (fold-var_0 pos_0)
                                  (begin
                                    (if (< pos_0 end_0)
                                      (let ((fold-var_1
                                             (let ((fold-var_1
                                                    (cons
                                                     (let ((w_0
                                                            (make-worker
                                                             pos_0)))
                                                       (begin
                                                         (start-worker w_0)
                                                         w_0))
                                                     fold-var_0)))
                                               (values fold-var_1))))
                                        (for-loop_0 fold-var_1 (+ pos_0 1)))
                                      fold-var_0))))))
                             (for-loop_0 null 1)))))))
                  (set-scheduler-workers! s_0 workers_0))))))
        (end-atomic)))))
(define kill-future-scheduler
  (lambda ()
    (let ((s_0 (current-scheduler)))
      (if s_0
        (begin
          (|#%app| host:mutex-acquire (scheduler-mutex s_0))
          (let ((lst_0 (scheduler-workers s_0)))
            (begin
              (letrec*
               ((for-loop_0
                 (|#%name|
                  for-loop
                  (lambda (lst_1)
                    (begin
                      (if (pair? lst_1)
                        (let ((w_0 (unsafe-car lst_1)))
                          (let ((rest_0 (unsafe-cdr lst_1)))
                            (begin
                              (set-worker-die?! w_0 #t)
                              (for-loop_0 rest_0))))
                        (values)))))))
               (for-loop_0 lst_0))))
          (void)
          (|#%app| host:mutex-release (scheduler-mutex s_0))
          (futures-sync-for-shutdown)
          (current-scheduler #f))
        (void)))))
(define schedule-future!.1
  (|#%name|
   schedule-future!
   (lambda (front?9_0 f11_0)
     (begin
       (let ((s_0 (current-scheduler)))
         (begin
           (|#%app| host:mutex-acquire (scheduler-mutex s_0))
           (let ((old_0
                  (if front?9_0
                    (scheduler-futures-head s_0)
                    (scheduler-futures-tail s_0))))
             (begin
               (if (not old_0)
                 (begin
                   (set-scheduler-futures-head! s_0 f11_0)
                   (set-scheduler-futures-tail! s_0 f11_0))
                 (if front?9_0
                   (begin
                     (set-future*-next! f11_0 old_0)
                     (set-future*-prev! old_0 f11_0)
                     (set-scheduler-futures-head! s_0 f11_0))
                   (begin
                     (set-future*-prev! f11_0 old_0)
                     (set-future*-next! old_0 f11_0)
                     (set-scheduler-futures-tail! s_0 f11_0))))
               (|#%app| host:condition-signal (scheduler-cond s_0))
               (|#%app| host:mutex-release (scheduler-mutex s_0))))))))))
(define deschedule-future
  (lambda (f_0)
    (let ((s_0 (current-scheduler)))
      (if (let ((or-part_0 (future*-prev f_0)))
            (if or-part_0 or-part_0 (future*-next f_0)))
        (begin
          (if (future*-prev f_0)
            (let ((app_0 (future*-prev f_0)))
              (set-future*-next! app_0 (future*-next f_0)))
            (set-scheduler-futures-head! s_0 (future*-next f_0)))
          (if (future*-next f_0)
            (let ((app_0 (future*-next f_0)))
              (set-future*-prev! app_0 (future*-prev f_0)))
            (set-scheduler-futures-tail! s_0 (future*-prev f_0)))
          (set-future*-prev! f_0 #f)
          (set-future*-next! f_0 #f))
        (if (eq? f_0 (scheduler-futures-head s_0))
          (begin
            (set-scheduler-futures-head! s_0 #f)
            (set-scheduler-futures-tail! s_0 #f))
          (internal-error "future is not in queue"))))))
(define try-deschedule-future?
  (lambda (f_0)
    (let ((s_0 (current-scheduler)))
      (begin
        (|#%app| host:mutex-acquire (scheduler-mutex s_0))
        (let ((ok?_0
               (if (if (not (future*-prev f_0))
                     (if (not (future*-next f_0))
                       (not (eq? f_0 (scheduler-futures-head s_0)))
                       #f)
                     #f)
                 #f
                 (begin
                   (deschedule-future f_0)
                   (lock-acquire (future*-lock f_0))
                   #t))))
          (begin (|#%app| host:mutex-release (scheduler-mutex s_0)) ok?_0))))))
(define future-notify-dependents
  (lambda (deps_0)
    (begin
      (begin
        (letrec*
         ((for-loop_0
           (|#%name|
            for-loop
            (lambda (i_0)
              (begin
                (if i_0
                  (let ((f_0 (hash-iterate-key deps_0 i_0)))
                    (begin
                      (if (eq? f_0 'place)
                        (|#%app| wakeup-this-place)
                        (future-notify-dependent f_0))
                      (for-loop_0 (hash-iterate-next deps_0 i_0))))
                  (values)))))))
         (for-loop_0 (hash-iterate-first deps_0))))
      (void))))
(define future-notify-dependent
  (lambda (f_0)
    (begin
      (let ((lock_0 (future*-lock f_0)))
        (begin
          (lock-acquire lock_0)
          (begin0 (set-future*-state! f_0 #f) (lock-release lock_0))))
      (on-transition-to-unfinished)
      (if (future*-would-be? f_0)
        (|#%app| wakeup-this-place)
        (schedule-future!.1 #t f_0)))))
(define start-worker
  (lambda (w_0)
    (let ((s_0 (current-scheduler)))
      (let ((th_0
             (|#%app|
              fork-pthread
              (lambda ()
                (begin
                  (current-future$1 'worker)
                  (|#%app| host:mutex-acquire (scheduler-mutex s_0))
                  (letrec*
                   ((loop_0
                     (|#%name|
                      loop
                      (lambda ()
                        (begin
                          (begin
                            (check-in w_0)
                            (if (worker-die? w_0)
                              (|#%app|
                               host:mutex-release
                               (scheduler-mutex s_0))
                              (let ((c1_0 (scheduler-futures-head s_0)))
                                (if c1_0
                                  (begin
                                    (deschedule-future c1_0)
                                    (|#%app|
                                     host:mutex-release
                                     (scheduler-mutex s_0))
                                    (lock-acquire (future*-lock c1_0))
                                    (maybe-run-future-in-worker c1_0 w_0)
                                    (|#%app|
                                     host:mutex-acquire
                                     (scheduler-mutex s_0))
                                    (loop_0))
                                  (begin
                                    (|#%app|
                                     host:condition-wait
                                     (scheduler-cond s_0)
                                     (scheduler-mutex s_0))
                                    (loop_0)))))))))))
                   (loop_0)))))))
        (set-worker-pthread! w_0 th_0)))))
(define maybe-run-future-in-worker
  (lambda (f_0 w_0)
    (if (custodian-shut-down?/other-pthread (future*-custodian f_0))
      (begin
        (set-future*-state! f_0 'blocked)
        (on-transition-to-unfinished)
        (lock-release (future*-lock f_0)))
      (run-future-in-worker f_0 w_0))))
(define run-future-in-worker
  (lambda (f_0 w_0)
    (begin
      (current-future$1 f_0)
      (begin
        (set-box! (worker-current-future-box w_0) f_0)
        (let ((e_0
               (|#%app|
                make-engine
                (lambda () (run-future.1 #f f_0))
                future-scheduler-prompt-tag
                void
                break-enabled-default-cell
                #t)))
          (begin
            (current-atomic (add1 (current-atomic)))
            (|#%app|
             call-with-engine-completion
             (lambda (done_0)
               (letrec*
                ((loop_0
                  (|#%name|
                   loop
                   (lambda (e_1)
                     (begin
                       (|#%app|
                        e_1
                        TICKS
                        (lambda ()
                          (begin
                            (if (if (zero? (current-atomic))
                                  (worker-pinged? w_0)
                                  #f)
                              (begin
                                (|#%app|
                                 host:mutex-acquire
                                 (scheduler-mutex (current-scheduler)))
                                (check-in w_0)
                                (|#%app|
                                 host:mutex-release
                                 (scheduler-mutex (current-scheduler))))
                              (void))
                            (if (if (let ((or-part_0
                                           (custodian-shut-down?/other-pthread
                                            (future*-custodian f_0))))
                                      (if or-part_0
                                        or-part_0
                                        (worker-die? w_0)))
                                  (zero? (current-atomic))
                                  #f)
                              (begin
                                (lock-acquire (future*-lock f_0))
                                (set-future*-state! f_0 #f)
                                (on-transition-to-unfinished)
                                (future-suspend))
                              (void))))
                        (lambda (e_2 results_0 leftover-ticks_0)
                          (if e_2 (loop_0 e_2) (|#%app| done_0 (void))))))))))
                (loop_0 e_0))))
            (let ((temp67_0 (future*-id f_0)))
              (log-future.1 #f #f 'end-work temp67_0))
            (current-future$1 'worker)
            (set-box! (worker-current-future-box w_0) #f)))))))
(define futures-sync-for-shutdown
  (lambda ()
    (let ((s_0 (current-scheduler)))
      (begin
        (|#%app| host:mutex-acquire (scheduler-mutex s_0))
        (let ((lst_0 (scheduler-workers s_0)))
          (begin
            (letrec*
             ((for-loop_0
               (|#%name|
                for-loop
                (lambda (lst_1)
                  (begin
                    (if (pair? lst_1)
                      (let ((w_0 (unsafe-car lst_1)))
                        (let ((rest_0 (unsafe-cdr lst_1)))
                          (begin
                            (letrec*
                             ((retry_0
                               (|#%name|
                                retry
                                (lambda ()
                                  (begin
                                    (if (unsafe-box*-cas!
                                         (worker-ping w_0)
                                         #f
                                         #t)
                                      (void)
                                      (retry_0)))))))
                             (retry_0))
                            (for-loop_0 rest_0))))
                      (values)))))))
             (for-loop_0 lst_0))))
        (void)
        (|#%app| host:condition-broadcast (scheduler-cond s_0))
        (drain-async-callbacks (scheduler-mutex s_0))
        (letrec*
         ((loop_0
           (|#%name|
            loop
            (lambda ()
              (begin
                (if (let ((lst_0 (scheduler-workers s_0)))
                      (begin
                        (letrec*
                         ((for-loop_0
                           (|#%name|
                            for-loop
                            (lambda (result_0 lst_1)
                              (begin
                                (if (pair? lst_1)
                                  (let ((w_0 (unsafe-car lst_1)))
                                    (let ((rest_0 (unsafe-cdr lst_1)))
                                      (let ((result_1
                                             (let ((result_1
                                                    (unbox (worker-ping w_0))))
                                               (values result_1))))
                                        (if (if (not
                                                 (let ((x_0 (list w_0)))
                                                   result_1))
                                              #t
                                              #f)
                                          (for-loop_0 result_1 rest_0)
                                          result_1))))
                                  result_0))))))
                         (for-loop_0 #f lst_0))))
                  (begin
                    (|#%app|
                     host:condition-wait
                     (scheduler-ping-cond s_0)
                     (scheduler-mutex s_0))
                    (loop_0))
                  (void)))))))
         (loop_0))
        (|#%app| host:mutex-release (scheduler-mutex s_0))))))
(define worker-pinged?
  (lambda (w_0)
    (if (unsafe-box*-cas! (worker-ping w_0) #t #t)
      #t
      (if (unsafe-box*-cas! (worker-ping w_0) #f #f)
        #f
        (worker-pinged? w_0)))))
(define check-in
  (lambda (w_0)
    (if (unbox (worker-ping w_0))
      (begin
        (set-box! (worker-ping w_0) #f)
        (|#%app|
         host:condition-broadcast
         (scheduler-ping-cond (current-scheduler))))
      (void))))
(define drain-async-callbacks
  (lambda (m_0)
    (begin
      (|#%app| host:mutex-release m_0)
      (let ((callbacks_0 (|#%app| host:poll-async-callbacks)))
        (begin
          (begin
            (letrec*
             ((for-loop_0
               (|#%name|
                for-loop
                (lambda (lst_0)
                  (begin
                    (if (pair? lst_0)
                      (let ((callback_0 (unsafe-car lst_0)))
                        (let ((rest_0 (unsafe-cdr lst_0)))
                          (begin (|#%app| callback_0) (for-loop_0 rest_0))))
                      (values)))))))
             (for-loop_0 callbacks_0)))
          (void)
          (|#%app| host:mutex-acquire m_0))))))
(define scheduler-add-thread-custodian-mapping!
  (lambda (s_0 ht_0)
    (if s_0
      (begin
        (let ((lst_0 (scheduler-workers s_0)))
          (begin
            (letrec*
             ((for-loop_0
               (|#%name|
                for-loop
                (lambda (lst_1)
                  (begin
                    (if (pair? lst_1)
                      (let ((w_0 (unsafe-car lst_1)))
                        (let ((rest_0 (unsafe-cdr lst_1)))
                          (begin
                            (let ((f_0
                                   (unbox (worker-current-future-box w_0))))
                              (if f_0
                                (let ((c_0 (future*-custodian f_0)))
                                  (if c_0
                                    (hash-set!
                                     ht_0
                                     c_0
                                     (let ((app_0 (worker-pthread w_0)))
                                       (cons app_0 (hash-ref ht_0 c_0 null))))
                                    (void)))
                                (void)))
                            (for-loop_0 rest_0))))
                      (values)))))))
             (for-loop_0 lst_0))))
        (void))
      (void))))
(define 1/reset-future-logs-for-tracing!
  (|#%name|
   reset-future-logs-for-tracing!
   (lambda ()
     (begin (begin (start-atomic) (begin0 (flush-future-log) (end-atomic)))))))
(define 1/mark-future-trace-end!
  (|#%name|
   mark-future-trace-end!
   (lambda () (begin (log-future.1 #f #f 'stop-trace #f)))))
(define on-transition-to-unfinished
  (lambda ()
    (let ((me-f_0 (current-future$1)))
      (if (if me-f_0 (not (future*-would-be? me-f_0)) #f)
        (|#%app| wakeup-this-place)
        (void)))))
(define wakeup-this-place (lambda () (void)))
(define ensure-place-wakeup-handle (lambda () (void)))
(define set-place-future-procs!
  (lambda (wakeup_0 ensure_0)
    (begin
      (set! wakeup-this-place wakeup_0)
      (set! ensure-place-wakeup-handle ensure_0))))
(define effect_1869
  (begin
    (void (begin-unsafe (set! future-block-for-atomic future-block)))
    (void)))
(define effect_2841
  (begin
    (void
     (set-custodian-future-callbacks!
      futures-sync-for-shutdown
      scheduler-add-thread-custodian-mapping!))
    (void)))
(define call-in-main-thread
  (lambda (thunk_0)
    (call-in-new-main-thread
     (lambda ()
       (begin
         (set-place-host-roots!
          initial-place
          (|#%app| host:current-place-roots))
         (|#%app| thunk_0))))))
(define call-in-another-main-thread
  (lambda (c_0 thunk_0)
    (begin
      (make-another-initial-thread-group)
      (set-root-custodian! c_0)
      (init-system-idle-evt!)
      (begin-unsafe (init-future-logging-place!))
      (init-schedule-counters!)
      (init-sync-place!)
      (call-in-new-main-thread thunk_0))))
(define call-in-new-main-thread
  (lambda (thunk_0)
    (begin
      (make-initial-thread thunk_0)
      (|#%app|
       call-with-engine-completion
       (lambda (done_0) (poll-and-select-thread! 0))))))
(define cell.1$3 (unsafe-make-place-local 0))
(define cell.2$2 (unsafe-make-place-local 0))
(define cell.3 (unsafe-make-place-local 0))
(define init-schedule-counters!
  (lambda ()
    (begin
      (unsafe-place-local-set! cell.1$3 0)
      (unsafe-place-local-set! cell.2$2 0)
      (unsafe-place-local-set! cell.3 0))))
(define poll-and-select-thread!
  (let ((poll-and-select-thread!_0
         (|#%name|
          poll-and-select-thread!
          (lambda (leftover-ticks2_0 pending-callbacks1_0)
            (begin
              (let ((callbacks_0
                     (if (null? pending-callbacks1_0)
                       (|#%app| host:poll-async-callbacks)
                       pending-callbacks1_0)))
                (let ((poll-now?_0 (<= leftover-ticks2_0 0)))
                  (begin
                    (|#%app| host:poll-will-executors)
                    (poll-custodian-will-executor)
                    (if poll-now?_0 (check-external-events) (void))
                    (call-pre-poll-external-callbacks)
                    (|#%app| check-place-activity callbacks_0)
                    (if (check-queued-custodian-shutdown)
                      (if (1/thread-dead? (unsafe-place-local-ref cell.1$1))
                        (force-exit 0)
                        (void))
                      (void))
                    (flush-future-log)
                    (if (all-threads-poll-done?)
                      (if (not (null? callbacks_0))
                        (begin
                          (let ((temp4_0 (lambda () (void))))
                            (do-make-thread.1 #t #f #f #f 'callbacks temp4_0))
                          (poll-and-select-thread! TICKS callbacks_0))
                        (if (if (not poll-now?_0) (check-external-events) #f)
                          (poll-and-select-thread! TICKS callbacks_0)
                          (if (try-post-idle)
                            (select-thread! leftover-ticks2_0 callbacks_0)
                            (begin
                              (process-sleep)
                              (poll-and-select-thread! 0 callbacks_0)))))
                      (select-thread!
                       (if poll-now?_0 TICKS leftover-ticks2_0)
                       callbacks_0))))))))))
    (case-lambda
     ((leftover-ticks_0) (poll-and-select-thread!_0 leftover-ticks_0 null))
     ((leftover-ticks_0 pending-callbacks1_0)
      (poll-and-select-thread!_0 leftover-ticks_0 pending-callbacks1_0)))))
(define select-thread!
  (lambda (leftover-ticks_0 callbacks_0)
    (letrec*
     ((loop_0
       (|#%name|
        loop
        (lambda (g_0 callbacks_1 none-k_0)
          (begin
            (let ((child_0 (thread-group-next! g_0)))
              (if (not child_0)
                (|#%app| none-k_0 callbacks_1)
                (if (1/thread? child_0)
                  (swap-in-thread child_0 leftover-ticks_0 callbacks_1)
                  (loop_0
                   child_0
                   callbacks_1
                   (lambda (callbacks_2)
                     (loop_0 g_0 none-k_0 callbacks_2)))))))))))
     (loop_0 (unsafe-place-local-ref cell.1) callbacks_0 maybe-done))))
(define swap-in-thread
  (lambda (t_0 leftover-ticks_0 callbacks_0)
    (begin
      (current-thread/in-atomic t_0)
      (let ((e_0 (thread-engine t_0)))
        (begin
          (set-thread-sched-info! t_0 #f)
          (current-future$1 (thread-future t_0))
          (set-place-current-thread! (unsafe-place-local-ref cell.1$2) t_0)
          (unsafe-place-local-set!
           cell.3
           (add1 (unsafe-place-local-ref cell.3)))
          (run-callbacks-in-engine e_0 callbacks_0 t_0 leftover-ticks_0))))))
(define current-thread-now-running!
  (lambda () (set-thread-engine! (current-thread/in-atomic) 'running)))
(define swap-in-engine
  (lambda (e_0 t_0 leftover-ticks_0)
    (letrec*
     ((loop_0
       (|#%name|
        loop
        (lambda (e_1 prefix_0)
          (begin
            (|#%app|
             e_1
             TICKS
             prefix_0
             (lambda (e_2 results_0 remaining-ticks_0)
               (if (not e_2)
                 (begin
                   (accum-cpu-time! t_0 #t)
                   (set-thread-future! t_0 #f)
                   (current-thread/in-atomic #f)
                   (set-place-current-thread!
                    (unsafe-place-local-ref cell.1$2)
                    #f)
                   (current-future$1 #f)
                   (if (zero? (current-atomic))
                     (void)
                     (internal-error "terminated in atomic mode!"))
                   (flush-end-atomic-callbacks!)
                   (thread-dead! t_0)
                   (if (eq? (unsafe-place-local-ref cell.1$1) t_0)
                     (force-exit 0)
                     (void))
                   (thread-did-work!)
                   (poll-and-select-thread!
                    (- leftover-ticks_0 (- TICKS remaining-ticks_0))))
                 (if (zero? (current-atomic))
                   (begin
                     (if (1/thread-dead? (unsafe-place-local-ref cell.1$1))
                       (force-exit 0)
                       (void))
                     (let ((new-leftover-ticks_0
                            (- leftover-ticks_0 (- TICKS remaining-ticks_0))))
                       (begin
                         (accum-cpu-time! t_0 (<= new-leftover-ticks_0 0))
                         (set-thread-future! t_0 (current-future$1))
                         (current-future$1 #f)
                         (set-place-current-thread!
                          (unsafe-place-local-ref cell.1$2)
                          #f)
                         (if (eq? (thread-engine t_0) 'done)
                           (void)
                           (set-thread-engine! t_0 e_2))
                         (current-thread/in-atomic #f)
                         (poll-and-select-thread! new-leftover-ticks_0))))
                   (begin
                     (add-end-atomic-callback! engine-timeout)
                     (loop_0 e_2 check-for-atomic-timeout)))))))))))
     (loop_0 e_0 check-break-prefix))))
(define check-break-prefix
  (lambda ()
    (begin
      (current-thread-now-running!)
      (1/check-for-break)
      (check-for-atomic-timeout))))
(define check-for-atomic-timeout
  (lambda ()
    (if (unsafe-place-local-ref cell.4)
      (if (positive? (current-atomic))
        (|#%app| (unsafe-place-local-ref cell.4) #f)
        (void))
      (void))))
(define maybe-done
  (lambda (callbacks_0)
    (if (pair? callbacks_0)
      (begin
        (do-make-thread.1 #f #f #f #f 'scheduler-make-thread void)
        (poll-and-select-thread! 0 callbacks_0))
      (if (if (not (sandman-any-sleepers?)) (not (any-idle-waiters?)) #f)
        (if (1/thread-running? (unsafe-place-local-ref cell.1$1))
          (begin (process-sleep) (poll-and-select-thread! 0))
          (void))
        (poll-and-select-thread! 0)))))
(define check-external-events
  (lambda ()
    (let ((did?_0 #f))
      (begin
        (let ((thread-wakeup_0
               (lambda (t_0)
                 (begin
                   (if t_0 (thread-reschedule! t_0) (void))
                   (set! did?_0 #t)))))
          (begin-unsafe
           (|#%app| (sandman-do-poll the-sandman) thread-wakeup_0)))
        (if did?_0 (thread-did-work!) (void))
        did?_0))))
(define run-callbacks-in-engine
  (lambda (e_0 callbacks_0 t_0 leftover-ticks_0)
    (if (null? callbacks_0)
      (swap-in-engine e_0 t_0 leftover-ticks_0)
      (let ((done?_0 #f))
        (letrec*
         ((loop_0
           (|#%name|
            loop
            (lambda (e_1 callbacks_1)
              (begin
                (let ((app_0 TICKS))
                  (|#%app|
                   e_1
                   app_0
                   (if (pair? callbacks_1)
                     (lambda ()
                       (begin
                         (current-thread-now-running!)
                         (run-callbacks callbacks_1)
                         (set! done?_0 #t)
                         (engine-block)))
                     void)
                   (lambda (e_2 result_0 remaining_0)
                     (begin
                       (if e_2
                         (void)
                         (internal-error
                          "thread ended while it should run callbacks atomically"))
                       (if done?_0
                         (swap-in-engine e_2 t_0 leftover-ticks_0)
                         (loop_0 e_2 null)))))))))))
         (loop_0 e_0 callbacks_0))))))
(define run-callbacks
  (lambda (callbacks_0)
    (begin
      (start-atomic)
      (begin
        (letrec*
         ((for-loop_0
           (|#%name|
            for-loop
            (lambda (lst_0)
              (begin
                (if (pair? lst_0)
                  (let ((callback_0 (unsafe-car lst_0)))
                    (let ((rest_0 (unsafe-cdr lst_0)))
                      (begin (|#%app| callback_0) (for-loop_0 rest_0))))
                  (values)))))))
         (for-loop_0 callbacks_0)))
      (void)
      (end-atomic))))
(define all-threads-poll-done?
  (lambda ()
    (=
     (hash-count (unsafe-place-local-ref cell.2$1))
     (unsafe-place-local-ref cell.2))))
(define process-sleep
  (lambda ()
    (let ((ts_0
           (thread-group-all-threads (unsafe-place-local-ref cell.1) null)))
      (let ((sleeping-exts_0 (sandman-sleepers-external-events)))
        (let ((exts_0
               (begin
                 (letrec*
                  ((for-loop_0
                    (|#%name|
                     for-loop
                     (lambda (exts_0 lst_0)
                       (begin
                         (if (pair? lst_0)
                           (let ((t_0 (unsafe-car lst_0)))
                             (let ((rest_0 (unsafe-cdr lst_0)))
                               (let ((exts_1
                                      (let ((exts_1
                                             (let ((sched-info_0
                                                    (thread-sched-info t_0)))
                                               (let ((t-exts_0
                                                      (if sched-info_0
                                                        (schedule-info-exts
                                                         sched-info_0)
                                                        #f)))
                                                 (begin-unsafe
                                                  (|#%app|
                                                   (sandman-do-merge-external-event-sets
                                                    the-sandman)
                                                   exts_0
                                                   t-exts_0))))))
                                        (values exts_1))))
                                 (for-loop_0 exts_1 rest_0))))
                           exts_0))))))
                  (for-loop_0 sleeping-exts_0 ts_0)))))
          (begin
            (begin-unsafe (|#%app| (sandman-do-sleep the-sandman) exts_0))
            (thread-did-work!)))))))
(define try-post-idle
  (lambda () (if (post-idle) (begin (thread-did-work!) #t) #f)))
(define accum-cpu-time!
  (lambda (t_0 timeout?_0)
    (if (not timeout?_0)
      (let ((n_0 (unsafe-place-local-ref cell.2$2)))
        (begin
          (unsafe-place-local-set! cell.2$2 (add1 n_0))
          (if (= n_0 100) (accum-cpu-time! t_0 #t) (void))))
      (let ((start_0 (unsafe-place-local-ref cell.1$3)))
        (let ((now_0 (|#%app| current-process-milliseconds$1)))
          (begin
            (unsafe-place-local-set! cell.1$3 now_0)
            (unsafe-place-local-set! cell.2$2 0)
            (set-thread-cpu-time!
             t_0
             (let ((app_0 (thread-cpu-time t_0)))
               (+ app_0 (- now_0 start_0))))))))))
(define cell.4 (unsafe-make-place-local #f))
(define set-atomic-timeout-callback!
  (lambda (cb_0)
    (begin0
      (unsafe-place-local-ref cell.4)
      (unsafe-place-local-set! cell.4 cb_0))))
(define effect_2769
  (begin
    (void
     (let ((proc_0
            (lambda ()
              (if (unsafe-place-local-ref cell.4)
                (begin (|#%app| (unsafe-place-local-ref cell.4) #t) #t)
                #f))))
       (begin-unsafe (set! force-atomic-timeout-callback proc_0))))
    (void)))
(define check-place-activity void)
(define set-check-place-activity!
  (lambda (proc_0) (set! check-place-activity proc_0)))
(define struct:alarm-evt
  (make-record-type-descriptor*
   'alarm-evt
   #f
   (|#%nongenerative-uid| alarm-evt)
   #f
   #f
   1
   0))
(define effect_2783
  (struct-type-install-properties!
   struct:alarm-evt
   '(alarm-evt)
   1
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1
      (lambda (e_0 ctx_0)
        (let ((msecs_0 (alarm-evt-msecs e_0)))
          (if (>= (current-inexact-milliseconds) msecs_0)
            (values (list e_0) #f)
            (begin
              (schedule-info-add-timeout-at!
               (poll-ctx-sched-info ctx_0)
               msecs_0)
              (values #f e_0))))))))
   (current-inspector)
   #f
   '(0)
   #f
   'alarm-evt))
(define alarm-evt1.1
  (|#%name|
   alarm-evt
   (record-constructor
    (make-record-constructor-descriptor struct:alarm-evt #f #f))))
(define alarm-evt?_2440
  (|#%name| alarm-evt? (record-predicate struct:alarm-evt)))
(define alarm-evt?
  (|#%name|
   alarm-evt?
   (lambda (v)
     (if (alarm-evt?_2440 v)
       #t
       ($value
        (if (impersonator? v) (alarm-evt?_2440 (impersonator-val v)) #f))))))
(define alarm-evt-msecs_2883
  (|#%name| alarm-evt-msecs (record-accessor struct:alarm-evt 0)))
(define alarm-evt-msecs
  (|#%name|
   alarm-evt-msecs
   (lambda (s)
     (if (alarm-evt?_2440 s)
       (alarm-evt-msecs_2883 s)
       ($value
        (impersonate-ref
         alarm-evt-msecs_2883
         struct:alarm-evt
         0
         s
         'alarm-evt
         'msecs))))))
(define create-alarm-evt
  (lambda (msecs_0)
    (begin
      (if (real? msecs_0)
        (void)
        (raise-argument-error 'create-alarm-evt "real?" msecs_0))
      (alarm-evt1.1 msecs_0))))
(define 1/call-in-nested-thread
  (let ((call-in-nested-thread_0
         (|#%name|
          call-in-nested-thread
          (lambda (thunk2_0 cust1_0)
            (begin
              (let ((cust_0
                     (if (eq? cust1_0 unsafe-undefined)
                       (1/current-custodian)
                       cust1_0)))
                (begin
                  (if (if (procedure? thunk2_0)
                        (procedure-arity-includes? thunk2_0 0)
                        #f)
                    (void)
                    (raise-argument-error
                     'call-in-nested-thread
                     "(procedure-arity-includes/c 0)"
                     thunk2_0))
                  (begin
                    (if (1/custodian? cust_0)
                      (void)
                      (raise-argument-error
                       'call-in-nested-thread
                       "custodian?"
                       cust_0))
                    (let ((init-break-cell_0 (current-break-enabled-cell)))
                      (let ((result_0 #f))
                        (let ((result-kind_0 #f))
                          (let ((ready-sema_0 (1/make-semaphore)))
                            (let ((t_0 unsafe-undefined))
                              (set! t_0
                                (with-continuation-mark*
                                 push-authentic
                                 break-enabled-key
                                 (make-thread-cell #f)
                                 (let ((temp5_0
                                        (lambda ()
                                          (begin
                                            (1/semaphore-wait ready-sema_0)
                                            (let ((with-handlers-predicate7_0
                                                   (|#%name|
                                                    with-handlers-predicate7
                                                    (lambda (x_0)
                                                      (begin #t)))))
                                              (let ((with-handlers-handler8_0
                                                     (|#%name|
                                                      with-handlers-handler8
                                                      (lambda (x_0)
                                                        (begin
                                                          (begin
                                                            (set! result-kind_0
                                                              'exn)
                                                            (set! result_0
                                                              x_0)))))))
                                                (let ((bpz_0
                                                       (continuation-mark-set-first
                                                        #f
                                                        break-enabled-key)))
                                                  (call-handled-body
                                                   bpz_0
                                                   (lambda (e_0)
                                                     (select-handler/no-breaks
                                                      e_0
                                                      bpz_0
                                                      (list
                                                       (cons
                                                        with-handlers-predicate7_0
                                                        with-handlers-handler8_0))))
                                                   (lambda ()
                                                     (with-continuation-mark*
                                                      authentic
                                                      break-enabled-key
                                                      init-break-cell_0
                                                      (begin
                                                        (set! result_0
                                                          (call-with-continuation-barrier
                                                           (lambda ()
                                                             (call-with-values
                                                              (lambda ()
                                                                (call-with-continuation-prompt
                                                                 thunk2_0
                                                                 (default-continuation-prompt-tag)
                                                                 (lambda (thunk_0)
                                                                   (abort-current-continuation
                                                                    (default-continuation-prompt-tag)
                                                                    thunk_0))))
                                                              list))))
                                                        (begin
                                                          (start-atomic)
                                                          (begin0
                                                            (begin
                                                              (set! result-kind_0
                                                                'value)
                                                              (thread-dead!
                                                               (check-not-unsafe-undefined
                                                                t_0
                                                                't_79)))
                                                            (end-atomic)))
                                                        (engine-block))))))))))))
                                   (do-make-thread.1
                                    #f
                                    cust_0
                                    #f
                                    #f
                                    'call-in-nested-thread
                                    temp5_0))))
                              (begin
                                (start-atomic)
                                (begin
                                  (begin0
                                    (let ((app_0 (current-thread/in-atomic)))
                                      (set-thread-forward-break-to! app_0 t_0))
                                    (end-atomic))
                                  (begin
                                    (1/semaphore-post ready-sema_0)
                                    (let ((pending-break_0
                                           (letrec*
                                            ((loop_0
                                              (|#%name|
                                               loop
                                               (lambda (t_1 pending-break_0)
                                                 (begin
                                                   (begin
                                                     (1/thread-wait t_1)
                                                     (let ((next-pending-break_0
                                                            (break-max
                                                             pending-break_0
                                                             (thread-pending-break
                                                              t_1))))
                                                       (let ((sub-t_0
                                                              (thread-forward-break-to
                                                               t_1)))
                                                         (if sub-t_0
                                                           (loop_0
                                                            sub-t_0
                                                            next-pending-break_0)
                                                           next-pending-break_0)))))))))
                                            (loop_0 t_0 #f))))
                                      (begin
                                        (start-atomic)
                                        (begin0
                                          (set-thread-forward-break-to!
                                           (current-thread/in-atomic)
                                           #f)
                                          (end-atomic))
                                        (with-continuation-mark*
                                         push-authentic
                                         break-enabled-key
                                         (make-thread-cell #f)
                                         (begin
                                           (if pending-break_0
                                             (let ((app_0 (1/current-thread)))
                                               (1/break-thread
                                                app_0
                                                (if (eq?
                                                     pending-break_0
                                                     'break)
                                                  #f
                                                  pending-break_0)))
                                             (void))
                                           (if (eq? result-kind_0 'exn)
                                             (raise result_0)
                                             (void))
                                           (if (eq? result-kind_0 'value)
                                             (void)
                                             (raise
                                              (|#%app|
                                               exn:fail
                                               "call-in-nested-thread: the thread was killed, or it exited via the default error escape handler"
                                               (current-continuation-marks))))))
                                        (1/check-for-break)
                                        (apply
                                         values
                                         result_0)))))))))))))))))))
    (|#%name|
     call-in-nested-thread
     (case-lambda
      ((thunk_0) (begin (call-in-nested-thread_0 thunk_0 unsafe-undefined)))
      ((thunk_0 cust1_0) (call-in-nested-thread_0 thunk_0 cust1_0))))))
(define 1/continuation-marks
  (let ((continuation-marks_0
         (|#%name|
          continuation-marks
          (lambda (k2_0 prompt-tag1_0)
            (begin
              (let ((prompt-tag_0
                     (if (eq? prompt-tag1_0 unsafe-undefined)
                       (default-continuation-prompt-tag)
                       prompt-tag1_0)))
                (begin
                  (if (let ((or-part_0 (not k2_0)))
                        (if or-part_0
                          or-part_0
                          (let ((or-part_1 (continuation? k2_0)))
                            (if or-part_1 or-part_1 (1/thread? k2_0)))))
                    (void)
                    (raise-argument-error
                     'continuation-marks
                     "(or/c continuation? thread? #f)"
                     k2_0))
                  (if (continuation-prompt-tag? prompt-tag_0)
                    (void)
                    (raise-argument-error
                     'continuation-marks
                     "continuation-prompt-tag?"
                     prompt-tag_0))
                  (if (1/thread? k2_0)
                    (let ((e_0 (thread-engine k2_0)))
                      (if (eq? e_0 'done)
                        (|#%app| host:continuation-marks #f prompt-tag_0)
                        (if (eq? e_0 'running)
                          (current-continuation-marks)
                          (|#%app| host:continuation-marks e_0 prompt-tag_0))))
                    (|#%app| host:continuation-marks k2_0 prompt-tag_0)))))))))
    (|#%name|
     continuation-marks
     (case-lambda
      ((k_0) (begin (continuation-marks_0 k_0 unsafe-undefined)))
      ((k_0 prompt-tag1_0) (continuation-marks_0 k_0 prompt-tag1_0))))))
(define 1/current-future
  (|#%name|
   current-future
   (lambda ()
     (begin
       (let ((or-part_0 (current-future$1)))
         (if or-part_0 or-part_0 (currently-running-future)))))))
(define 1/choice-evt
  (|#%name|
   choice-evt
   (lambda args_0
     (begin
       (begin
         (begin
           (letrec*
            ((for-loop_0
              (|#%name|
               for-loop
               (lambda (lst_0)
                 (begin
                   (if (pair? lst_0)
                     (let ((arg_0 (unsafe-car lst_0)))
                       (let ((rest_0 (unsafe-cdr lst_0)))
                         (begin
                           (if (1/evt? arg_0)
                             (void)
                             (raise-argument-error 'choice-evt "evt?" arg_0))
                           (for-loop_0 rest_0))))
                     (values)))))))
            (for-loop_0 args_0)))
         (void)
         (choice-evt11.1 args_0))))))
(define 1/wrap-evt
  (|#%name|
   wrap-evt
   (lambda (evt_0 proc_0)
     (begin
       (begin
         (if (1/evt? evt_0)
           (void)
           (raise-argument-error 'wrap-evt "evt?" evt_0))
         (if (procedure? proc_0)
           (void)
           (raise-argument-error 'wrap-evt "procedure?" proc_0))
         (wrap-evt7.1 evt_0 proc_0))))))
(define 1/handle-evt
  (|#%name|
   handle-evt
   (lambda (evt_0 proc_0)
     (begin
       (begin
         (if (1/evt? evt_0)
           (void)
           (raise-argument-error 'handle-evt "evt?" evt_0))
         (if (procedure? proc_0)
           (void)
           (raise-argument-error 'handle-evt "procedure?" proc_0))
         (handle-evt8.1 evt_0 proc_0))))))
(define 1/handle-evt?
  (|#%name|
   handle-evt?
   (lambda (evt_0)
     (begin
       (begin
         (if (1/evt? evt_0)
           (void)
           (raise-argument-error 'handle-evt? "evt?" evt_0))
         (letrec*
          ((loop_0
            (|#%name|
             loop
             (lambda (evt_1)
               (begin
                 (let ((or-part_0 (handle-evt?$1 evt_1)))
                   (if or-part_0
                     or-part_0
                     (if (choice-evt? evt_1)
                       (let ((lst_0 (choice-evt-evts evt_1)))
                         (begin
                           (letrec*
                            ((for-loop_0
                              (|#%name|
                               for-loop
                               (lambda (result_0 lst_1)
                                 (begin
                                   (if (pair? lst_1)
                                     (let ((evt_2 (unsafe-car lst_1)))
                                       (let ((rest_0 (unsafe-cdr lst_1)))
                                         (let ((result_1
                                                (let ((result_1
                                                       (loop_0 evt_2)))
                                                  (values result_1))))
                                           (if (if (not
                                                    (let ((x_0 (list evt_2)))
                                                      result_1))
                                                 #t
                                                 #f)
                                             (for-loop_0 result_1 rest_0)
                                             result_1))))
                                     result_0))))))
                            (for-loop_0 #f lst_0))))
                       #f))))))))
          (loop_0 evt_0)))))))
(define guard-evt
  (lambda (proc_0)
    (begin
      (if (if (procedure? proc_0) (procedure-arity-includes? proc_0 0) #f)
        (void)
        (raise-argument-error
         'guard-evt
         "(procedure-arity-includes/c 0)"
         proc_0))
      (poll-guard-evt10.1 (lambda (poll?_0) (|#%app| proc_0))))))
(define 1/poll-guard-evt
  (|#%name|
   poll-guard-evt
   (lambda (proc_0)
     (begin
       (begin
         (if (if (procedure? proc_0) (procedure-arity-includes? proc_0 1) #f)
           (void)
           (raise-argument-error
            'poll-guard-evt
            "(procedure-arity-includes/c 1)"
            proc_0))
         (poll-guard-evt10.1 proc_0))))))
(define 1/nack-guard-evt
  (|#%name|
   nack-guard-evt
   (lambda (proc_0)
     (begin
       (begin
         (if (if (procedure? proc_0) (procedure-arity-includes? proc_0 1) #f)
           (void)
           (raise-argument-error
            'nack-guard-evt
            "(procedure-arity-includes/c 1)"
            proc_0))
         (poll-guard-evt10.1
          (lambda (poll?_0)
            (let ((s_0 (1/make-semaphore)))
              (control-state-evt9.1
               (poll-guard-evt10.1
                (lambda (poll?_1)
                  (let ((v_0
                         (|#%app|
                          proc_0
                          (1/wrap-evt (semaphore-peek-evt2.1 s_0) void))))
                    (if (1/evt? v_0)
                      v_0
                      (1/wrap-evt the-always-evt (lambda () v_0))))))
               values
               void
               (lambda () (1/semaphore-post s_0))
               void)))))))))
(define 1/semaphore-peek-evt
  (|#%name|
   semaphore-peek-evt
   (lambda (s_0)
     (begin
       (begin
         (if (1/semaphore? s_0)
           (void)
           (raise-argument-error 'semaphore-peek-evt "semaphore?" s_0))
         (semaphore-peek-evt2.1 s_0))))))
(define 1/semaphore-wait/enable-break
  (|#%name|
   semaphore-wait/enable-break
   (lambda (s_0)
     (begin
       (begin
         (if (1/semaphore? s_0)
           (void)
           (raise-argument-error
            'semaphore-wait/enable-break
            "semaphore?"
            s_0))
         (1/sync/enable-break s_0)
         (void))))))
(define do-call-with-semaphore.1
  (|#%name|
   do-call-with-semaphore
   (lambda (enable-break?1_0 who3_0 s4_0 proc5_0 try-fail6_0 args7_0)
     (begin
       (begin
         (if (1/semaphore? s4_0)
           (void)
           (raise-argument-error who3_0 "semaphore?" s4_0))
         (begin
           (if (procedure? proc5_0)
             (void)
             (raise-argument-error who3_0 "procedure?" proc5_0))
           (begin
             (if (let ((or-part_0 (not try-fail6_0)))
                   (if or-part_0
                     or-part_0
                     (if (procedure? try-fail6_0)
                       (procedure-arity-includes? try-fail6_0 0)
                       #f)))
               (void)
               (raise-argument-error
                who3_0
                "(or/c (procedure-arity-includes/c 0) #f)"
                try-fail6_0))
             (let ((breaks-on?_0
                    (if enable-break?1_0 enable-break?1_0 (break-enabled))))
               (let ((results_0 #t))
                 (begin
                   (dynamic-wind
                    (lambda ()
                      (if try-fail6_0
                        (set! results_0 (1/semaphore-try-wait? s4_0))
                        (if breaks-on?_0
                          (1/semaphore-wait/enable-break s4_0)
                          (1/semaphore-wait s4_0))))
                    (lambda ()
                      (if results_0
                        (call-with-continuation-barrier
                         (lambda ()
                           (set! results_0
                             (call-with-values
                              (lambda () (apply proc5_0 args7_0))
                              list))))
                        (void)))
                    (lambda () (if results_0 (1/semaphore-post s4_0) (void))))
                   (if results_0
                     (apply values results_0)
                     (|#%app| try-fail6_0))))))))))))
(define 1/call-with-semaphore
  (let ((call-with-semaphore_0
         (|#%name|
          call-with-semaphore
          (lambda (s10_0 proc11_0 try-fail9_0 new-rest_0)
            (begin
              (do-call-with-semaphore.1
               #f
               'call-with-semaphore
               s10_0
               proc11_0
               try-fail9_0
               new-rest_0))))))
    (|#%name|
     call-with-semaphore
     (case-lambda
      ((s_0 proc_0) (begin (call-with-semaphore_0 s_0 proc_0 #f null)))
      ((s_0 proc_0 try-fail9_0 . args_0)
       (call-with-semaphore_0 s_0 proc_0 try-fail9_0 args_0))))))
(define 1/call-with-semaphore/enable-break
  (let ((call-with-semaphore/enable-break_0
         (|#%name|
          call-with-semaphore/enable-break
          (lambda (s13_0 proc14_0 try-fail12_0 new-rest_0)
            (begin
              (do-call-with-semaphore.1
               #t
               'call-with-semaphore/enable-break
               s13_0
               proc14_0
               try-fail12_0
               new-rest_0))))))
    (|#%name|
     call-with-semaphore/enable-break
     (case-lambda
      ((s_0 proc_0)
       (begin (call-with-semaphore/enable-break_0 s_0 proc_0 #f null)))
      ((s_0 proc_0 try-fail12_0 . args_0)
       (call-with-semaphore/enable-break_0 s_0 proc_0 try-fail12_0 args_0))))))
(define struct:will-executor
  (make-record-type-descriptor*
   'will-executor
   #f
   (|#%nongenerative-uid| will-executor)
   #f
   #f
   2
   0))
(define effect_2531
  (struct-type-install-properties!
   struct:will-executor
   '(will-executor)
   2
   0
   #f
   (list
    (cons prop:authentic #t)
    (cons
     1/prop:evt
     (lambda (we_0)
       (wrap-evt7.1
        (semaphore-peek-evt2.1 (will-executor-sema we_0))
        (lambda (v_0) we_0))))
    (cons host:prop:unsafe-authentic-override #t))
   (current-inspector)
   #f
   '(0 1)
   #f
   'will-executor))
(define will-executor1.1
  (|#%name|
   will-executor
   (record-constructor
    (make-record-constructor-descriptor struct:will-executor #f #f))))
(define 1/will-executor?
  (|#%name| will-executor? (record-predicate struct:will-executor)))
(define will-executor-host-we
  (|#%name| will-executor-host-we (record-accessor struct:will-executor 0)))
(define will-executor-sema
  (|#%name| will-executor-sema (record-accessor struct:will-executor 1)))
(define do-make-will-executor
  (lambda (host:make-will-executor_0)
    (let ((sema_0 (1/make-semaphore)))
      (will-executor1.1
       (|#%app|
        host:make-will-executor_0
        (lambda () (semaphore-post/atomic sema_0)))
       sema_0))))
(define 1/make-will-executor
  (|#%name|
   make-will-executor
   (lambda () (begin (do-make-will-executor host:make-will-executor)))))
(define 1/make-late-will-executor
  (|#%name|
   make-late-will-executor
   (lambda () (begin (do-make-will-executor host:make-late-will-executor)))))
(define 1/will-register
  (|#%name|
   will-register
   (lambda (we_0 v_0 proc_0)
     (begin
       (begin
         (if (1/will-executor? we_0)
           (void)
           (raise-argument-error 'will-register "will-executor?" we_0))
         (if (if (procedure? proc_0) (procedure-arity-includes? proc_0 1) #f)
           (void)
           (raise-argument-error
            'will-register
            "(procedure-arity-includes/c 1)"
            proc_0))
         (|#%app|
          host:will-register
          (will-executor-host-we we_0)
          v_0
          proc_0))))))
(define do-will-execute
  (lambda (who_0 we_0 fail-k_0)
    (begin
      (if (1/will-executor? we_0)
        (void)
        (raise-argument-error who_0 "will-executor?" we_0))
      (|#%app|
       (begin
         (start-atomic)
         (begin0
           (let ((c1_0
                  (|#%app|
                   host:will-try-execute
                   (will-executor-host-we we_0))))
             (if c1_0
               (begin
                 (semaphore-wait/atomic (will-executor-sema we_0))
                 (lambda ()
                   (let ((app_0 (car c1_0))) (|#%app| app_0 (cdr c1_0)))))
               fail-k_0))
           (end-atomic)))))))
(define 1/will-execute
  (|#%name|
   will-execute
   (lambda (we_0)
     (begin
       (do-will-execute
        'will-execute
        we_0
        (lambda () (begin (1/sync we_0) (1/will-execute we_0))))))))
(define 1/will-try-execute
  (let ((will-try-execute_0
         (|#%name|
          will-try-execute
          (lambda (we3_0 default2_0)
            (begin
              (do-will-execute
               'will-try-execute
               we3_0
               (lambda () default2_0)))))))
    (|#%name|
     will-try-execute
     (case-lambda
      ((we_0) (begin (will-try-execute_0 we_0 #f)))
      ((we_0 default2_0) (will-try-execute_0 we_0 default2_0))))))
(define 1/unsafe-start-breakable-atomic
  (|#%name|
   unsafe-start-breakable-atomic
   (lambda ()
     (begin
       (begin
         (start-atomic)
         (current-breakable-atomic (fx+ (current-breakable-atomic) 1)))))))
(define 1/unsafe-end-breakable-atomic
  (|#%name|
   unsafe-end-breakable-atomic
   (lambda ()
     (begin
       (begin
         (current-breakable-atomic (fx- (current-breakable-atomic) 1))
         (end-atomic))))))
(define 1/unsafe-start-atomic
  (|#%name| unsafe-start-atomic (lambda () (begin (start-atomic)))))
(define 1/unsafe-end-atomic
  (|#%name| unsafe-end-atomic (lambda () (begin (end-atomic)))))
(define 1/unsafe-in-atomic?
  (|#%name|
   unsafe-in-atomic?
   (lambda () (begin (positive? (current-atomic))))))
(define 1/unsafe-set-on-atomic-timeout!
  (|#%name|
   unsafe-set-on-atomic-timeout!
   (lambda (proc_0) (begin (set-atomic-timeout-callback! proc_0)))))
(define 1/current-process-milliseconds
  (let ((current-process-milliseconds_0
         (|#%name|
          current-process-milliseconds
          (lambda (scope1_0)
            (begin
              (if (not scope1_0)
                (|#%app| current-process-milliseconds$1)
                (if (1/thread? scope1_0)
                  (thread-cpu-time scope1_0)
                  (if (eq? scope1_0 'subprocesses)
                    (|#%app| get-subprocesses-time)
                    (raise-argument-error
                     'current-process-milliseconds
                     "(or/c #f thread? 'subprocesses)"
                     scope1_0)))))))))
    (|#%name|
     current-process-milliseconds
     (case-lambda
      (() (begin (current-process-milliseconds_0 #f)))
      ((scope1_0) (current-process-milliseconds_0 scope1_0))))))
(define get-subprocesses-time (lambda () 0))
(define set-get-subprocesses-time!
  (lambda (f_0) (set! get-subprocesses-time f_0)))
(define |#%thread-instance|
  (hasheq
   'thread
   make-thread
   'thread-suspend-evt
   1/thread-suspend-evt
   'thread-dead-evt
   get-thread-dead-evt
   'current-thread
   1/current-thread
   'thread-resume
   1/thread-resume
   'make-semaphore
   1/make-semaphore
   'semaphore-post
   1/semaphore-post
   'semaphore-post-all
   semaphore-post-all
   'semaphore-wait
   1/semaphore-wait
   'semaphore-peek-evt
   semaphore-peek-evt2.1
   'make-channel
   1/make-channel
   'channel-put-evt
   1/channel-put-evt
   'wrap-evt
   wrap-evt7.1
   'handle-evt
   handle-evt8.1
   'always-evt
   the-always-evt
   'choice-evt
   choice-evt11.1
   'sync
   1/sync
   'sync/timeout
   1/sync/timeout
   'evt?
   1/evt?
   'sync-atomic-poll-evt?
   sync-atomic-poll-evt?
   'prop:evt
   1/prop:evt
   'prop:secondary-evt
   prop:secondary-evt
   'poller
   poller2.1
   'poller-evt
   poller-evt13.1
   'poll-ctx-poll?
   poll-ctx-poll?
   'poll-ctx-select-proc
   poll-ctx-select-proc
   'poll-ctx-sched-info
   poll-ctx-sched-info
   'set-poll-ctx-incomplete?!
   set-poll-ctx-incomplete?!
   'control-state-evt
   control-state-evt9.1
   'async-evt
   the-async-evt
   'current-sandman
   current-sandman
   'schedule-info-current-exts
   schedule-info-current-exts
   'schedule-info-did-work!
   schedule-info-did-work!
   'unsafe-start-atomic
   1/unsafe-start-atomic
   'unsafe-end-atomic
   1/unsafe-end-atomic
   'start-atomic/no-interrupts
   start-atomic/no-interrupts
   'end-atomic/no-interrupts
   end-atomic/no-interrupts
   'in-atomic-mode?
   in-atomic-mode?
   'current-custodian
   1/current-custodian
   'custodian-shut-down?
   1/custodian-shut-down?
   'current-plumber
   1/current-plumber
   'plumber-add-flush!
   1/plumber-add-flush!
   'plumber-flush-handle-remove!
   1/plumber-flush-handle-remove!
   'unsafe-custodian-register
   1/unsafe-custodian-register
   'unsafe-custodian-unregister
   1/unsafe-custodian-unregister
   'unsafe-make-custodian-at-root
   1/unsafe-make-custodian-at-root
   'thread-push-kill-callback!
   thread-push-kill-callback!
   'thread-pop-kill-callback!
   thread-pop-kill-callback!
   'unsafe-add-pre-poll-callback!
   unsafe-add-pre-poll-callback!
   'set-get-subprocesses-time!
   set-get-subprocesses-time!
   'prop:place-message
   prop:place-message))
(define 1/vector-set-performance-stats!
  (let ((vector-set-performance-stats!_0
         (|#%name|
          vector-set-performance-stats!
          (lambda (vec2_0 thd1_0)
            (begin
              (begin
                (if (if (vector? vec2_0) (not (immutable? vec2_0)) #f)
                  (void)
                  (raise-argument-error
                   'vector-set-performance-stats!
                   "(and/c vector? (not/c immutable?))"
                   vec2_0))
                (begin
                  (if (let ((or-part_0 (not thd1_0)))
                        (if or-part_0 or-part_0 (1/thread? thd1_0)))
                    (void)
                    (raise-argument-error
                     'vector-set-performance-stats!
                     "(or/c thread? #f)"
                     thd1_0))
                  (let ((maybe-set!_0
                         (|#%name|
                          maybe-set!
                          (lambda (i_0 v_0)
                            (begin
                              (if (< i_0 (vector-length vec2_0))
                                (vector-set! vec2_0 i_0 v_0)
                                (void)))))))
                    (if (not thd1_0)
                      (begin
                        (maybe-set!_0 0 (1/current-process-milliseconds))
                        (maybe-set!_0 1 (current-milliseconds))
                        (maybe-set!_0 2 (current-gc-milliseconds))
                        (maybe-set!_0 3 0)
                        (maybe-set!_0 4 (unsafe-place-local-ref cell.3))
                        (maybe-set!_0 5 0)
                        (maybe-set!_0 6 0)
                        (maybe-set!_0 7 0)
                        (maybe-set!_0 8 0)
                        (maybe-set!_0 9 0)
                        (maybe-set!_0 10 0)
                        (maybe-set!_0 11 0)
                        (void))
                      (begin
                        (maybe-set!_0 0 (1/thread-running? thd1_0))
                        (maybe-set!_0 1 (1/thread-dead? thd1_0))
                        (maybe-set!_0 2 #f)
                        (maybe-set!_0 3 #f)
                        (void)))))))))))
    (|#%name|
     vector-set-performance-stats!
     (case-lambda
      ((vec_0) (begin (vector-set-performance-stats!_0 vec_0 #f)))
      ((vec_0 thd1_0) (vector-set-performance-stats!_0 vec_0 thd1_0))))))
(define 1/current-thread-initial-stack-size
  (make-parameter
   64
   (lambda (v_0)
     (begin
       (if (exact-positive-integer? v_0)
         (void)
         (raise-argument-error
          'current-thread-initial-stack-size
          "exact-positive-integer?"
          v_0))
       v_0))
   'current-thread-initial-stack-size))
(define struct:place-event
  (make-record-type-descriptor*
   'place-event
   #f
   (structure-type-lookup-prefab-uid 'place-event #f 4 0 #f '(0 1 2 3))
   #f
   #f
   4
   15))
(define effect_2427
  (struct-type-install-properties!
   struct:place-event
   '(place-event)
   4
   0
   #f
   null
   'prefab
   #f
   '(0 1 2 3)
   #f
   'place-event))
(define place-event1.1
  (|#%name|
   place-event
   (record-constructor
    (make-record-constructor-descriptor struct:place-event #f #f))))
(define place-event?_2380
  (|#%name| place-event? (record-predicate struct:place-event)))
(define place-event?
  (|#%name|
   place-event?
   (lambda (v)
     (if (place-event?_2380 v)
       #t
       ($value
        (if (impersonator? v) (place-event?_2380 (impersonator-val v)) #f))))))
(define place-event-id_2098
  (|#%name| place-event-id (record-accessor struct:place-event 0)))
(define place-event-id
  (|#%name|
   place-event-id
   (lambda (s)
     (if (place-event?_2380 s)
       (place-event-id_2098 s)
       ($value
        (impersonate-ref
         place-event-id_2098
         struct:place-event
         0
         s
         'place-event
         'id))))))
(define place-event-action_2875
  (|#%name| place-event-action (record-accessor struct:place-event 1)))
(define place-event-action
  (|#%name|
   place-event-action
   (lambda (s)
     (if (place-event?_2380 s)
       (place-event-action_2875 s)
       ($value
        (impersonate-ref
         place-event-action_2875
         struct:place-event
         1
         s
         'place-event
         'action))))))
(define place-event-data_2440
  (|#%name| place-event-data (record-accessor struct:place-event 2)))
(define place-event-data
  (|#%name|
   place-event-data
   (lambda (s)
     (if (place-event?_2380 s)
       (place-event-data_2440 s)
       ($value
        (impersonate-ref
         place-event-data_2440
         struct:place-event
         2
         s
         'place-event
         'data))))))
(define place-event-time_2569
  (|#%name| place-event-time (record-accessor struct:place-event 3)))
(define place-event-time
  (|#%name|
   place-event-time
   (lambda (s)
     (if (place-event?_2380 s)
       (place-event-time_2569 s)
       ($value
        (impersonate-ref
         place-event-time_2569
         struct:place-event
         3
         s
         'place-event
         'time))))))
(define log-place.1
  (|#%name|
   log-place
   (lambda (action2_0 data3_0 msg6_0)
     (begin
       (let ((action_0
              (if (eq? action2_0 unsafe-undefined)
                (string->symbol msg6_0)
                action2_0)))
         (if (|#%app| logging-place-events?)
           (let ((id_0 (place-id (unsafe-place-local-ref cell.1$2))))
             (let ((app_0 log-place-event))
               (let ((app_1
                      (let ((app_1 (number->string id_0)))
                        (string-append
                         "id "
                         app_1
                         ": "
                         msg6_0
                         (if data3_0
                           (string-append " " (number->string data3_0))
                           "")))))
                 (|#%app|
                  app_0
                  app_1
                  (place-event1.1
                   id_0
                   action_0
                   data3_0
                   (current-inexact-milliseconds))))))
           (void)))))))
(define logging-place-events? (lambda () #f))
(define log-place-event (lambda (msg_0 e_0) (void)))
(define install-place-logging-procs!
  (lambda (logging?_0 log_0)
    (begin
      (set! logging-place-events? logging?_0)
      (set! log-place-event log_0))))
(define 1/dynamic-place
  (|#%name|
   dynamic-place
   (lambda (path_0 sym_0 in_0 out_0 err_0)
     (begin
       (begin
         (if (eq? initial-place (unsafe-place-local-ref cell.1$2))
           (begin (start-atomic) (begin0 (ensure-wakeup-handle!) (end-atomic)))
           (void))
         (let ((orig-cust_0 (create-custodian #f)))
           (let ((lock_0 (|#%app| host:make-mutex)))
             (let ((started_0 (|#%app| host:make-condition)))
               (call-with-values
                (lambda () (1/place-channel))
                (case-lambda
                 ((place-pch_0 child-pch_0)
                  (let ((orig-plumber_0 (1/make-plumber)))
                    (let ((current-place15_0
                           (unsafe-place-local-ref cell.1$2)))
                      (let ((new-place_0
                             (make-place.1
                              current-place15_0
                              place-pch_0
                              lock_0
                              orig-cust_0)))
                        (begin
                          (set-custodian-place! orig-cust_0 new-place_0)
                          (let ((done-waiting_0
                                 (place-done-waiting new-place_0)))
                            (let ((default-exit_0
                                   (|#%name|
                                    default-exit
                                    (lambda (explicit?7_0 v9_0)
                                      (begin
                                        (begin
                                          (let ((temp17_0
                                                 (if explicit?7_0
                                                   "exit (via `exit`)"
                                                   "exit")))
                                            (log-place.1
                                             unsafe-undefined
                                             #f
                                             temp17_0))
                                          (let ((flush-failed?_0 #f))
                                            (begin
                                              (plumber-flush-all/wrap
                                               orig-plumber_0
                                               (lambda (proc_0 h_0)
                                                 (call-with-continuation-prompt
                                                  (lambda ()
                                                    (|#%app| proc_0 h_0))
                                                  (default-continuation-prompt-tag)
                                                  (lambda (thunk_0)
                                                    (begin
                                                      (set! flush-failed?_0 #t)
                                                      (call-with-continuation-prompt
                                                       thunk_0))))))
                                              (start-atomic)
                                              (begin0
                                                (begin
                                                  (|#%app|
                                                   host:mutex-acquire
                                                   lock_0)
                                                  (set-place-queued-result!
                                                   new-place_0
                                                   (if flush-failed?_0
                                                     1
                                                     (if (byte? v9_0) v9_0 0)))
                                                  (place-has-activity!
                                                   new-place_0)
                                                  (|#%app|
                                                   host:mutex-release
                                                   lock_0))
                                                (end-atomic))
                                              (engine-block)))))))))
                              (begin
                                (start-atomic)
                                (let ((cref_0
                                       (custodian-register-place
                                        (1/current-custodian)
                                        new-place_0
                                        shutdown-place)))
                                  (begin
                                    (if cref_0
                                      (void)
                                      (begin
                                        (end-atomic)
                                        (let ((c_0 (1/current-custodian)))
                                          (begin-unsafe
                                           (raise-arguments-error
                                            'dynamic-place
                                            "the custodian has been shut down"
                                            "custodian"
                                            c_0)))))
                                    (begin
                                      (set-place-custodian-ref!
                                       new-place_0
                                       cref_0)
                                      (call-with-values
                                       (lambda ()
                                         (|#%app|
                                          make-place-ports+fds
                                          in_0
                                          out_0
                                          err_0))
                                       (case-lambda
                                        ((parent-in_0
                                          parent-out_0
                                          parent-err_0
                                          child-in-fd_0
                                          child-out-fd_0
                                          child-err-fd_0)
                                         (begin
                                           (|#%app| host:mutex-acquire lock_0)
                                           (let ((host-thread_0
                                                  (|#%app|
                                                   host:fork-place
                                                   (lambda ()
                                                     (call-in-another-main-thread
                                                      orig-cust_0
                                                      (lambda ()
                                                        (begin
                                                          (unsafe-place-local-set!
                                                           cell.1$2
                                                           new-place_0)
                                                          (begin
                                                            (set-place-id!
                                                             new-place_0
                                                             (|#%app|
                                                              get-pthread-id))
                                                            (begin
                                                              (set-place-host-roots!
                                                               new-place_0
                                                               (|#%app|
                                                                host:current-place-roots))
                                                              (begin
                                                                (1/current-thread-group
                                                                 (unsafe-place-local-ref
                                                                  cell.1))
                                                                (begin
                                                                  (1/current-custodian
                                                                   orig-cust_0)
                                                                  (begin
                                                                    (1/current-plumber
                                                                     orig-plumber_0)
                                                                    (begin
                                                                      (1/exit-handler
                                                                       (lambda (v_0)
                                                                         (default-exit_0
                                                                          #t
                                                                          v_0)))
                                                                      (begin
                                                                        (current-pseudo-random-generator
                                                                         (make-pseudo-random-generator))
                                                                        (begin
                                                                          (1/current-evt-pseudo-random-generator
                                                                           (make-pseudo-random-generator))
                                                                          (let ((finish_0
                                                                                 (|#%app|
                                                                                  host:start-place
                                                                                  child-pch_0
                                                                                  path_0
                                                                                  sym_0
                                                                                  child-in-fd_0
                                                                                  child-out-fd_0
                                                                                  child-err-fd_0
                                                                                  orig-cust_0
                                                                                  orig-plumber_0)))
                                                                            (call-with-continuation-prompt
                                                                             (lambda ()
                                                                               (begin
                                                                                 (|#%app|
                                                                                  host:mutex-acquire
                                                                                  lock_0)
                                                                                 (set-place-wakeup-handle!
                                                                                  new-place_0
                                                                                  (sandman-get-wakeup-handle))
                                                                                 (|#%app|
                                                                                  host:condition-signal
                                                                                  started_0)
                                                                                 (|#%app|
                                                                                  host:mutex-release
                                                                                  lock_0)
                                                                                 (let ((temp20_0
                                                                                        "enter"))
                                                                                   (log-place.1
                                                                                    unsafe-undefined
                                                                                    #f
                                                                                    temp20_0))
                                                                                 (|#%app|
                                                                                  finish_0)
                                                                                 (default-exit_0
                                                                                  #f
                                                                                  0)))
                                                                             (default-continuation-prompt-tag)
                                                                             (lambda (thunk_0)
                                                                               (begin
                                                                                 (call-with-continuation-prompt
                                                                                  thunk_0)
                                                                                 (default-exit_0
                                                                                  #f
                                                                                  1)))))))))))))))))
                                                   (lambda (result_0)
                                                     (begin
                                                       (do-custodian-shutdown-all
                                                        orig-cust_0)
                                                       (let ((lst_0
                                                              (place-post-shutdown
                                                               new-place_0)))
                                                         (begin
                                                           (letrec*
                                                            ((for-loop_0
                                                              (|#%name|
                                                               for-loop
                                                               (lambda (lst_1)
                                                                 (begin
                                                                   (if (pair?
                                                                        lst_1)
                                                                     (let ((proc_0
                                                                            (unsafe-car
                                                                             lst_1)))
                                                                       (let ((rest_0
                                                                              (unsafe-cdr
                                                                               lst_1)))
                                                                         (begin
                                                                           (|#%app|
                                                                            proc_0)
                                                                           (for-loop_0
                                                                            rest_0))))
                                                                     (values)))))))
                                                            (for-loop_0
                                                             lst_0))))
                                                       (void)
                                                       (kill-future-scheduler)
                                                       (|#%app|
                                                        host:mutex-acquire
                                                        lock_0)
                                                       (set-place-result!
                                                        new-place_0
                                                        result_0)
                                                       (|#%app|
                                                        host:mutex-release
                                                        lock_0)
                                                       (begin
                                                         (letrec*
                                                          ((for-loop_0
                                                            (|#%name|
                                                             for-loop
                                                             (lambda (i_0)
                                                               (begin
                                                                 (if i_0
                                                                   (let ((pl_0
                                                                          (hash-iterate-key
                                                                           done-waiting_0
                                                                           i_0)))
                                                                     (begin
                                                                       (wakeup-waiting
                                                                        pl_0)
                                                                       (for-loop_0
                                                                        (hash-iterate-next
                                                                         done-waiting_0
                                                                         i_0))))
                                                                   (values)))))))
                                                          (for-loop_0
                                                           (hash-iterate-first
                                                            done-waiting_0))))
                                                       (void)
                                                       (hash-clear!
                                                        done-waiting_0))))))
                                             (begin
                                               (set-place-host-thread!
                                                new-place_0
                                                host-thread_0)
                                               (|#%app|
                                                host:condition-wait
                                                started_0
                                                lock_0)
                                               (|#%app|
                                                host:mutex-release
                                                lock_0)
                                               (end-atomic)
                                               (let ((temp11_0 "create"))
                                                 (let ((temp12_0
                                                        (place-id
                                                         new-place_0)))
                                                   (let ((temp11_1 temp11_0))
                                                     (log-place.1
                                                      unsafe-undefined
                                                      temp12_0
                                                      temp11_1))))
                                               (values
                                                new-place_0
                                                parent-in_0
                                                parent-out_0
                                                parent-err_0)))))
                                        (args
                                         (raise-binding-result-arity-error
                                          6
                                          args)))))))))))))))
                 (args (raise-binding-result-arity-error 2 args))))))))))))
(define 1/place-break
  (let ((place-break_0
         (|#%name|
          place-break
          (lambda (p2_0 kind1_0)
            (begin
              (begin
                (if (1/place? p2_0)
                  (void)
                  (raise-argument-error 'place-break "place?" p2_0))
                (if (let ((or-part_0 (not kind1_0)))
                      (if or-part_0
                        or-part_0
                        (let ((or-part_1 (eq? kind1_0 'hang-up)))
                          (if or-part_1 or-part_1 (eq? kind1_0 'terminate)))))
                  (void)
                  (raise-argument-error
                   'place-break
                   "(or/c #f 'hang-up 'terminate)"
                   kind1_0))
                (start-atomic)
                (begin0
                  (begin
                    (|#%app| host:mutex-acquire (place-lock p2_0))
                    (let ((pending-break_0 (place-pending-break p2_0)))
                      (begin
                        (if (let ((or-part_0 (not pending-break_0)))
                              (if or-part_0
                                or-part_0
                                (break>?
                                 (if kind1_0 kind1_0 'break)
                                 pending-break_0)))
                          (begin
                            (set-place-pending-break!
                             p2_0
                             (if kind1_0 kind1_0 'break))
                            (place-has-activity! p2_0))
                          (void))
                        (|#%app| host:mutex-release (place-lock p2_0)))))
                  (end-atomic))))))))
    (|#%name|
     place-break
     (case-lambda
      ((p_0) (begin (place-break_0 p_0 #f)))
      ((p_0 kind1_0) (place-break_0 p_0 kind1_0))))))
(define place-has-activity!
  (lambda (p_0)
    (begin
      (set-box! (place-activity-canary p_0) #t)
      (let ((h_0 (place-wakeup-handle p_0)))
        (begin-unsafe (|#%app| (sandman-do-wakeup the-sandman) h_0))))))
(define effect_2163
  (begin
    (void
     (let ((proc_0
            (lambda (callbacks_0)
              (let ((p_0 (unsafe-place-local-ref cell.1$2)))
                (if (unbox (place-activity-canary p_0))
                  (begin
                    (set-box! (place-activity-canary p_0) #f)
                    (begin
                      (|#%app| host:mutex-acquire (place-lock p_0))
                      (let ((queued-result_0 (place-queued-result p_0)))
                        (let ((break_0 (place-pending-break p_0)))
                          (let ((dequeue-semas_0 (place-dequeue-semas p_0)))
                            (begin
                              (if break_0
                                (set-place-pending-break! p_0 #f)
                                (void))
                              (if (pair? dequeue-semas_0)
                                (set-place-dequeue-semas! p_0 null)
                                (void))
                              (|#%app| host:mutex-release (place-lock p_0))
                              (if queued-result_0
                                (begin
                                  (|#%app|
                                   host:post-as-asynchronous-callback
                                   (lambda ()
                                     (begin
                                       (begin
                                         (letrec*
                                          ((for-loop_0
                                            (|#%name|
                                             for-loop
                                             (lambda (lst_0)
                                               (begin
                                                 (if (pair? lst_0)
                                                   (let ((callback_0
                                                          (unsafe-car lst_0)))
                                                     (let ((rest_0
                                                            (unsafe-cdr
                                                             lst_0)))
                                                       (begin
                                                         (|#%app| callback_0)
                                                         (for-loop_0 rest_0))))
                                                   (values)))))))
                                          (for-loop_0 callbacks_0)))
                                       (void))))
                                  (force-exit queued-result_0))
                                (void))
                              (begin
                                (letrec*
                                 ((for-loop_0
                                   (|#%name|
                                    for-loop
                                    (lambda (lst_0)
                                      (begin
                                        (if (pair? lst_0)
                                          (let ((s_0 (unsafe-car lst_0)))
                                            (let ((rest_0 (unsafe-cdr lst_0)))
                                              (begin
                                                (begin
                                                  (thread-did-work!)
                                                  (semaphore-post-all/atomic
                                                   s_0))
                                                (for-loop_0 rest_0))))
                                          (values)))))))
                                 (for-loop_0 dequeue-semas_0)))
                              (void)
                              (if break_0
                                (begin
                                  (thread-did-work!)
                                  (do-break-thread
                                   (unsafe-place-local-ref cell.1$1)
                                   break_0
                                   #f))
                                (void))))))))
                  (void))))))
       (begin-unsafe (set! check-place-activity proc_0))))
    (void)))
(define do-place-kill
  (lambda (p_0)
    (begin
      (|#%app| host:mutex-acquire (place-lock p_0))
      (if (let ((or-part_0 (place-result p_0)))
            (if or-part_0 or-part_0 (place-queued-result p_0)))
        (void)
        (begin (set-place-queued-result! p_0 1) (place-has-activity! p_0)))
      (|#%app| host:mutex-release (place-lock p_0)))))
(define 1/place-kill
  (|#%name|
   place-kill
   (lambda (p_0)
     (begin
       (begin
         (if (1/place? p_0)
           (void)
           (raise-argument-error 'place-kill "place?" p_0))
         (start-atomic)
         (begin0 (do-place-kill p_0) (end-atomic))
         (1/place-wait p_0)
         (void))))))
(define 1/place-wait
  (|#%name|
   place-wait
   (lambda (p_0)
     (begin
       (begin
         (if (1/place? p_0)
           (void)
           (raise-argument-error 'place-wait "place?" p_0))
         (let ((result_0 (1/sync (place-done-evt3.1 p_0 #t))))
           (let ((vec_0 (place-pumpers p_0)))
             (begin
               (if vec_0
                 (begin
                   (call-with-values
                    (lambda ()
                      (begin
                        (check-vector vec_0)
                        (values vec_0 (unsafe-vector-length vec_0))))
                    (case-lambda
                     ((vec_1 len_0)
                      (begin
                        #f
                        (letrec*
                         ((for-loop_0
                           (|#%name|
                            for-loop
                            (lambda (pos_0)
                              (begin
                                (if (unsafe-fx< pos_0 len_0)
                                  (let ((s_0 (unsafe-vector-ref vec_1 pos_0)))
                                    (begin
                                      (if (1/thread? s_0)
                                        (1/thread-wait s_0)
                                        (void))
                                      (for-loop_0 (unsafe-fx+ 1 pos_0))))
                                  (values)))))))
                         (for-loop_0 0))))
                     (args (raise-binding-result-arity-error 2 args))))
                   (void)
                   (set-place-pumpers! p_0 #f))
                 (void))
               (begin
                 (if (place-host-thread p_0)
                   (if (begin
                         (start-atomic)
                         (begin0
                           (if (place-host-thread p_0)
                             (begin (set-place-host-thread! p_0 #f) #t)
                             #f)
                           (end-atomic)))
                     (let ((temp26_0 "reap"))
                       (let ((temp27_0 (place-id p_0)))
                         (let ((temp26_1 temp26_0))
                           (log-place.1 unsafe-undefined temp27_0 temp26_1))))
                     (void))
                   (void))
                 (let ((cref_0 (place-custodian-ref p_0)))
                   (begin
                     (if cref_0
                       (begin
                         (1/unsafe-custodian-unregister p_0 cref_0)
                         (set-place-custodian-ref! p_0 #f))
                       (void))
                     result_0)))))))))))
(define shutdown-place
  (lambda (p_0 c_0)
    (begin
      (do-place-kill p_0)
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda ()
            (begin
              (begin
                (|#%app| host:mutex-acquire (place-lock p_0))
                (let ((result_0 (place-result p_0)))
                  (begin
                    (if result_0
                      (void)
                      (hash-set!
                       (place-done-waiting p_0)
                       (unsafe-place-local-ref cell.1$2)
                       #t))
                    (|#%app| host:mutex-release (place-lock p_0))
                    (if result_0
                      (void)
                      (begin
                        (begin-unsafe
                         (|#%app| (sandman-do-sleep the-sandman) #f))
                        (loop_0)))))))))))
       (loop_0)))))
(define struct:place-done-evt
  (make-record-type-descriptor*
   'place-dead-evt
   #f
   (|#%nongenerative-uid| place-dead-evt)
   #f
   #f
   2
   0))
(define effect_2098
  (struct-type-install-properties!
   struct:place-done-evt
   '(place-dead-evt)
   2
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1
      (lambda (self_0 poll-ctx_0)
        (begin
          (ensure-wakeup-handle!)
          (let ((p_0 (place-done-evt-p self_0)))
            (begin
              (|#%app| host:mutex-acquire (place-lock p_0))
              (let ((result_0 (place-result p_0)))
                (begin
                  (if result_0
                    (void)
                    (hash-set!
                     (place-done-waiting p_0)
                     (unsafe-place-local-ref cell.1$2)
                     #t))
                  (|#%app| host:mutex-release (place-lock p_0))
                  (if result_0
                    (if (place-done-evt-get-result? self_0)
                      (values (list result_0) #f)
                      (values (list self_0) #f))
                    (values #f self_0)))))))))))
   (current-inspector)
   #f
   '(0 1)
   #f
   'place-done-evt))
(define place-done-evt3.1
  (|#%name|
   place-done-evt
   (record-constructor
    (make-record-constructor-descriptor struct:place-done-evt #f #f))))
(define place-done-evt?_2138
  (|#%name| place-dead-evt? (record-predicate struct:place-done-evt)))
(define place-done-evt?
  (|#%name|
   place-dead-evt?
   (lambda (v)
     (if (place-done-evt?_2138 v)
       #t
       ($value
        (if (impersonator? v)
          (place-done-evt?_2138 (impersonator-val v))
          #f))))))
(define place-done-evt-p_2323
  (|#%name| place-dead-evt-p (record-accessor struct:place-done-evt 0)))
(define place-done-evt-p
  (|#%name|
   place-dead-evt-p
   (lambda (s)
     (if (place-done-evt?_2138 s)
       (place-done-evt-p_2323 s)
       ($value
        (impersonate-ref
         place-done-evt-p_2323
         struct:place-done-evt
         0
         s
         'place-dead-evt
         'p))))))
(define place-done-evt-get-result?_2601
  (|#%name|
   place-dead-evt-get-result?
   (record-accessor struct:place-done-evt 1)))
(define place-done-evt-get-result?
  (|#%name|
   place-dead-evt-get-result?
   (lambda (s)
     (if (place-done-evt?_2138 s)
       (place-done-evt-get-result?_2601 s)
       ($value
        (impersonate-ref
         place-done-evt-get-result?_2601
         struct:place-done-evt
         1
         s
         'place-dead-evt
         'get-result?))))))
(define 1/place-dead-evt
  (|#%name|
   place-dead-evt
   (lambda (p_0)
     (begin
       (begin
         (if (1/place? p_0)
           (void)
           (raise-argument-error 'place-dead-evt "place?" p_0))
         (place-done-evt3.1 p_0 #f))))))
(define struct:message-queue
  (make-record-type-descriptor*
   'message-queue
   #f
   (|#%nongenerative-uid| message-queue)
   #f
   #f
   6
   22))
(define effect_2499
  (struct-type-install-properties!
   struct:message-queue
   '(message-queue)
   6
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(0 3 5)
   #f
   'message-queue))
(define message-queue4.1
  (|#%name|
   message-queue
   (record-constructor
    (make-record-constructor-descriptor struct:message-queue #f #f))))
(define message-queue?
  (|#%name| message-queue? (record-predicate struct:message-queue)))
(define message-queue-lock
  (|#%name| message-queue-lock (record-accessor struct:message-queue 0)))
(define message-queue-q
  (|#%name| message-queue-q (record-accessor struct:message-queue 1)))
(define message-queue-rev-q
  (|#%name| message-queue-rev-q (record-accessor struct:message-queue 2)))
(define message-queue-out-key-box
  (|#%name|
   message-queue-out-key-box
   (record-accessor struct:message-queue 3)))
(define message-queue-waiters
  (|#%name| message-queue-waiters (record-accessor struct:message-queue 4)))
(define message-queue-in-key-box
  (|#%name| message-queue-in-key-box (record-accessor struct:message-queue 5)))
(define set-message-queue-q!
  (|#%name| set-message-queue-q! (record-mutator struct:message-queue 1)))
(define set-message-queue-rev-q!
  (|#%name| set-message-queue-rev-q! (record-mutator struct:message-queue 2)))
(define set-message-queue-waiters!
  (|#%name|
   set-message-queue-waiters!
   (record-mutator struct:message-queue 4)))
(define make-message-queue
  (lambda ()
    (let ((app_0 (|#%app| host:make-mutex)))
      (message-queue4.1 app_0 '() '() (box #f) hash2725 (box #f)))))
(define enqueue!
  (lambda (mq_0 msg_0 wk_0)
    (let ((lock_0 (message-queue-lock mq_0)))
      (begin
        (start-atomic)
        (begin0
          (begin
            (|#%app| host:mutex-acquire lock_0)
            (begin
              (set-message-queue-rev-q!
               mq_0
               (cons msg_0 (message-queue-rev-q mq_0)))
              (let ((waiters_0 (message-queue-waiters mq_0)))
                (begin
                  (set-message-queue-waiters! mq_0 hash2725)
                  (set-box! (message-queue-out-key-box mq_0) wk_0)
                  (set-box! (message-queue-in-key-box mq_0) #f)
                  (|#%app| host:mutex-release lock_0)
                  (begin
                    (letrec*
                     ((for-loop_0
                       (|#%name|
                        for-loop
                        (lambda (i_0)
                          (begin
                            (if i_0
                              (call-with-values
                               (lambda ()
                                 (hash-iterate-key+value waiters_0 i_0))
                               (case-lambda
                                ((pl_0 s_0)
                                 (begin
                                   (begin
                                     (|#%app|
                                      host:mutex-acquire
                                      (place-lock pl_0))
                                     (set-place-dequeue-semas!
                                      pl_0
                                      (cons s_0 (place-dequeue-semas pl_0)))
                                     (place-has-activity! pl_0)
                                     (|#%app|
                                      host:mutex-release
                                      (place-lock pl_0))
                                     (wakeup-waiting pl_0))
                                   (for-loop_0
                                    (hash-iterate-next waiters_0 i_0))))
                                (args
                                 (raise-binding-result-arity-error 2 args))))
                              (values)))))))
                     (for-loop_0 (hash-iterate-first waiters_0))))
                  (void)))))
          (end-atomic))))))
(define dequeue!
  (lambda (mq_0 rk_0 success-k_0 fail-k_0)
    (begin
      (ensure-wakeup-handle!)
      (let ((lock_0 (message-queue-lock mq_0)))
        (begin
          (|#%app| host:mutex-acquire lock_0)
          (begin
            (if (if (null? (message-queue-q mq_0))
                  (not (null? (message-queue-rev-q mq_0)))
                  #f)
              (begin
                (set-message-queue-q!
                 mq_0
                 (reverse$1 (message-queue-rev-q mq_0)))
                (set-message-queue-rev-q! mq_0 null))
              (void))
            (let ((q_0 (message-queue-q mq_0)))
              (if (null? q_0)
                (let ((waiters_0 (message-queue-waiters mq_0)))
                  (let ((c1_0
                         (hash-ref
                          waiters_0
                          (unsafe-place-local-ref cell.1$2)
                          #f)))
                    (if c1_0
                      (begin
                        (|#%app| host:mutex-release lock_0)
                        (|#%app| fail-k_0 c1_0))
                      (let ((s_0 (1/make-semaphore)))
                        (begin
                          (set-message-queue-waiters!
                           mq_0
                           (hash-set
                            waiters_0
                            (unsafe-place-local-ref cell.1$2)
                            s_0))
                          (set-box! (message-queue-in-key-box mq_0) rk_0)
                          (|#%app| host:mutex-release lock_0)
                          (|#%app| fail-k_0 s_0))))))
                (let ((new-q_0 (cdr q_0)))
                  (begin
                    (set-message-queue-q! mq_0 new-q_0)
                    (if (null? new-q_0)
                      (set-box! (message-queue-out-key-box mq_0) #f)
                      (void))
                    (|#%app| host:mutex-release lock_0)
                    (|#%app| success-k_0 (car q_0))))))))))))
(define struct:pchannel
  (make-record-type-descriptor*
   'place-channel
   #f
   (|#%nongenerative-uid| place-channel)
   #f
   #f
   6
   0))
(define effect_2960
  (struct-type-install-properties!
   struct:pchannel
   '(place-channel)
   6
   0
   #f
   (list
    (cons prop:place-message (lambda (self_0) (lambda () (lambda () self_0))))
    (cons
     1/prop:evt
     (poller2.1
      (lambda (self_0 poll-ctx_0)
        (let ((in-mq_0 (ephemeron-value (pchannel-in-mq-e self_0))))
          (if in-mq_0
            (dequeue!
             in-mq_0
             (pchannel-reader-key self_0)
             (lambda (v_0)
               (values
                #f
                (wrap-evt7.1
                 the-always-evt
                 (lambda (a_0) (un-message-ize v_0)))))
             (lambda (sema_0)
               (values #f (1/replace-evt sema_0 (lambda (s_0) self_0)))))
            (values #f the-never-evt)))))))
   (current-inspector)
   #f
   '(0 1 2 3 4 5)
   #f
   'pchannel))
(define pchannel5.1
  (|#%name|
   pchannel
   (record-constructor
    (make-record-constructor-descriptor struct:pchannel #f #f))))
(define pchannel?_3032
  (|#%name| place-channel? (record-predicate struct:pchannel)))
(define pchannel?
  (|#%name|
   place-channel?
   (lambda (v)
     (if (pchannel?_3032 v)
       #t
       ($value
        (if (impersonator? v) (pchannel?_3032 (impersonator-val v)) #f))))))
(define pchannel-in-mq-e_2571
  (|#%name| place-channel-in-mq-e (record-accessor struct:pchannel 0)))
(define pchannel-in-mq-e
  (|#%name|
   place-channel-in-mq-e
   (lambda (s)
     (if (pchannel?_3032 s)
       (pchannel-in-mq-e_2571 s)
       ($value
        (impersonate-ref
         pchannel-in-mq-e_2571
         struct:pchannel
         0
         s
         'place-channel
         'in-mq-e))))))
(define pchannel-out-mq-e_2865
  (|#%name| place-channel-out-mq-e (record-accessor struct:pchannel 1)))
(define pchannel-out-mq-e
  (|#%name|
   place-channel-out-mq-e
   (lambda (s)
     (if (pchannel?_3032 s)
       (pchannel-out-mq-e_2865 s)
       ($value
        (impersonate-ref
         pchannel-out-mq-e_2865
         struct:pchannel
         1
         s
         'place-channel
         'out-mq-e))))))
(define pchannel-reader-key_2908
  (|#%name| place-channel-reader-key (record-accessor struct:pchannel 2)))
(define pchannel-reader-key
  (|#%name|
   place-channel-reader-key
   (lambda (s)
     (if (pchannel?_3032 s)
       (pchannel-reader-key_2908 s)
       ($value
        (impersonate-ref
         pchannel-reader-key_2908
         struct:pchannel
         2
         s
         'place-channel
         'reader-key))))))
(define pchannel-writer-key_2620
  (|#%name| place-channel-writer-key (record-accessor struct:pchannel 3)))
(define pchannel-writer-key
  (|#%name|
   place-channel-writer-key
   (lambda (s)
     (if (pchannel?_3032 s)
       (pchannel-writer-key_2620 s)
       ($value
        (impersonate-ref
         pchannel-writer-key_2620
         struct:pchannel
         3
         s
         'place-channel
         'writer-key))))))
(define pchannel-in-key-box_2626
  (|#%name| place-channel-in-key-box (record-accessor struct:pchannel 4)))
(define pchannel-in-key-box
  (|#%name|
   place-channel-in-key-box
   (lambda (s)
     (if (pchannel?_3032 s)
       (pchannel-in-key-box_2626 s)
       ($value
        (impersonate-ref
         pchannel-in-key-box_2626
         struct:pchannel
         4
         s
         'place-channel
         'in-key-box))))))
(define pchannel-out-key-box_2914
  (|#%name| place-channel-out-key-box (record-accessor struct:pchannel 5)))
(define pchannel-out-key-box
  (|#%name|
   place-channel-out-key-box
   (lambda (s)
     (if (pchannel?_3032 s)
       (pchannel-out-key-box_2914 s)
       ($value
        (impersonate-ref
         pchannel-out-key-box_2914
         struct:pchannel
         5
         s
         'place-channel
         'out-key-box))))))
(define 1/place-channel?
  (|#%name|
   place-channel?
   (lambda (v_0)
     (begin
       (let ((or-part_0 (pchannel? v_0)))
         (if or-part_0 or-part_0 (1/place? v_0)))))))
(define unwrap-place-channel
  (lambda (in-pch_0) (if (1/place? in-pch_0) (place-pch in-pch_0) in-pch_0)))
(define 1/place-channel
  (|#%name|
   place-channel
   (lambda ()
     (begin
       (let ((mq1_0 (make-message-queue)))
         (let ((mq2_0 (make-message-queue)))
           (let ((rk1_0 (gensym 'read)))
             (let ((wk1_0 (gensym 'write)))
               (let ((rk2_0 (gensym 'read)))
                 (let ((wk2_0 (gensym 'write)))
                   (values
                    (pchannel5.1
                     (make-ephemeron wk1_0 mq1_0)
                     (make-ephemeron rk2_0 mq2_0)
                     rk1_0
                     wk2_0
                     (message-queue-out-key-box mq1_0)
                     (message-queue-in-key-box mq2_0))
                    (pchannel5.1
                     (make-ephemeron wk2_0 mq2_0)
                     (make-ephemeron rk1_0 mq1_0)
                     rk2_0
                     wk1_0
                     (message-queue-out-key-box mq2_0)
                     (message-queue-in-key-box mq1_0)))))))))))))
(define 1/place-channel-get
  (|#%name|
   place-channel-get
   (lambda (in-pch_0)
     (begin
       (begin
         (if (1/place-channel? in-pch_0)
           (void)
           (raise-argument-error 'place-channel-get "place-channel?" in-pch_0))
         (let ((pch_0 (unwrap-place-channel in-pch_0)))
           (let ((in-mq_0 (ephemeron-value (pchannel-in-mq-e pch_0))))
             (if in-mq_0
               (begin
                 (start-atomic)
                 (dequeue!
                  in-mq_0
                  (pchannel-reader-key pch_0)
                  (lambda (v_0)
                    (begin
                      (end-atomic)
                      (let ((temp33_0 "get message"))
                        (log-place.1 'get #f temp33_0))
                      (un-message-ize v_0)))
                  (lambda (sema_0)
                    (begin
                      (end-atomic)
                      (1/semaphore-wait sema_0)
                      (1/place-channel-get pch_0)))))
               (1/sync the-never-evt)))))))))
(define 1/place-channel-put
  (|#%name|
   place-channel-put
   (lambda (in-pch_0 in-v_0)
     (begin
       (begin
         (if (1/place-channel? in-pch_0)
           (void)
           (raise-argument-error 'place-channel-put "place-channel?" in-pch_0))
         (let ((v_0
                (if (place-message-allowed-direct? in-v_0)
                  in-v_0
                  (message-ize
                   in-v_0
                   (lambda ()
                     (raise-argument-error
                      'place-channel-put
                      "place-message-allowed?"
                      in-v_0))))))
           (begin
             (let ((temp36_0 "put message")) (log-place.1 'put #f temp36_0))
             (let ((pch_0 (unwrap-place-channel in-pch_0)))
               (let ((out-mq_0 (ephemeron-value (pchannel-out-mq-e pch_0))))
                 (if out-mq_0
                   (enqueue! out-mq_0 v_0 (pchannel-writer-key pch_0))
                   (void)))))))))))
(define ensure-wakeup-handle!
  (lambda ()
    (if (place-wakeup-handle (unsafe-place-local-ref cell.1$2))
      (void)
      (set-place-wakeup-handle!
       (unsafe-place-local-ref cell.1$2)
       (sandman-get-wakeup-handle)))))
(define wakeup-waiting
  (lambda (pl_0)
    (begin
      (|#%app| host:mutex-acquire (place-lock pl_0))
      (if (place-result pl_0)
        (void)
        (let ((h_0 (place-wakeup-handle pl_0)))
          (begin-unsafe (|#%app| (sandman-do-wakeup the-sandman) h_0))))
      (|#%app| host:mutex-release (place-lock pl_0)))))
(define wakeup-initial-place
  (lambda ()
    (let ((h_0 (place-wakeup-handle initial-place)))
      (begin-unsafe (|#%app| (sandman-do-wakeup the-sandman) h_0)))))
(define make-place-ports+fds
  (lambda (in_0 out_0 err_0) (values #f #f #f in_0 out_0 err_0)))
(define set-make-place-ports+fds!
  (lambda (proc_0) (set! make-place-ports+fds proc_0)))
(define 1/place-pumper-threads
  (|#%name|
   place-pumper-threads
   (lambda (p_0 vec_0) (begin (set-place-pumpers! p_0 vec_0)))))
(define effect_2262
  (begin
    (void
     (set-place-custodian-procs!
      (lambda ()
        (begin
          (start-atomic)
          (begin0 (ensure-wakeup-handle!) (end-atomic))
          (unsafe-place-local-ref cell.1$2)))
      (lambda () (wakeup-initial-place))
      (lambda (pl_0) (wakeup-waiting pl_0))))
    (void)))
(define effect_2833
  (begin
    (void
     (set-place-future-procs!
      (lambda () (place-has-activity! (unsafe-place-local-ref cell.1$2)))
      (lambda () (ensure-wakeup-handle!))))
    (void)))
(define struct:fsemaphore
  (make-record-type-descriptor*
   'fsemaphore
   #f
   (|#%nongenerative-uid| fsemaphore)
   #f
   #f
   4
   13))
(define effect_2715
  (struct-type-install-properties!
   struct:fsemaphore
   '(fsemaphore)
   4
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(1)
   #f
   'fsemaphore))
(define fsemaphore1.1
  (|#%name|
   fsemaphore
   (record-constructor
    (make-record-constructor-descriptor struct:fsemaphore #f #f))))
(define 1/fsemaphore?
  (|#%name| fsemaphore? (record-predicate struct:fsemaphore)))
(define fsemaphore-c
  (|#%name| fsemaphore-c (record-accessor struct:fsemaphore 0)))
(define fsemaphore-lock
  (|#%name| fsemaphore-lock (record-accessor struct:fsemaphore 1)))
(define fsemaphore-dependents
  (|#%name| fsemaphore-dependents (record-accessor struct:fsemaphore 2)))
(define fsemaphore-dep-box
  (|#%name| fsemaphore-dep-box (record-accessor struct:fsemaphore 3)))
(define set-fsemaphore-c!
  (|#%name| set-fsemaphore-c! (record-mutator struct:fsemaphore 0)))
(define set-fsemaphore-dependents!
  (|#%name| set-fsemaphore-dependents! (record-mutator struct:fsemaphore 2)))
(define set-fsemaphore-dep-box!
  (|#%name| set-fsemaphore-dep-box! (record-mutator struct:fsemaphore 3)))
(define struct:fsemaphore-box-evt
  (make-record-type-descriptor*
   'fsemaphore-box-evt
   #f
   (|#%nongenerative-uid| fsemaphore-box-evt)
   #f
   #f
   1
   0))
(define effect_2250
  (struct-type-install-properties!
   struct:fsemaphore-box-evt
   '(fsemaphore-box-evt)
   1
   0
   #f
   (list
    (cons
     1/prop:evt
     (poller2.1
      (lambda (fsb_0 poll-ctx_0)
        (let ((b_0 (fsemaphore-box-evt-b fsb_0)))
          (if (unbox b_0) (values '(#t) #f) (values #f fsb_0)))))))
   (current-inspector)
   #f
   '(0)
   #f
   'fsemaphore-box-evt))
(define fsemaphore-box-evt2.1
  (|#%name|
   fsemaphore-box-evt
   (record-constructor
    (make-record-constructor-descriptor struct:fsemaphore-box-evt #f #f))))
(define fsemaphore-box-evt?_2344
  (|#%name| fsemaphore-box-evt? (record-predicate struct:fsemaphore-box-evt)))
(define fsemaphore-box-evt?
  (|#%name|
   fsemaphore-box-evt?
   (lambda (v)
     (if (fsemaphore-box-evt?_2344 v)
       #t
       ($value
        (if (impersonator? v)
          (fsemaphore-box-evt?_2344 (impersonator-val v))
          #f))))))
(define fsemaphore-box-evt-b_2517
  (|#%name|
   fsemaphore-box-evt-b
   (record-accessor struct:fsemaphore-box-evt 0)))
(define fsemaphore-box-evt-b
  (|#%name|
   fsemaphore-box-evt-b
   (lambda (s)
     (if (fsemaphore-box-evt?_2344 s)
       (fsemaphore-box-evt-b_2517 s)
       ($value
        (impersonate-ref
         fsemaphore-box-evt-b_2517
         struct:fsemaphore-box-evt
         0
         s
         'fsemaphore-box-evt
         'b))))))
(define 1/make-fsemaphore
  (|#%name|
   make-fsemaphore
   (lambda (init_0)
     (begin
       (begin
         (if (exact-nonnegative-integer? init_0)
           (void)
           (raise-argument-error
            'make-fsemaphore
            "exact-nonnegative-integer?"
            init_0))
         (fsemaphore1.1 init_0 (make-lock) hash2610 #f))))))
(define 1/fsemaphore-post
  (|#%name|
   fsemaphore-post
   (lambda (fs_0)
     (begin
       (begin
         (if (1/fsemaphore? fs_0)
           (void)
           (raise-argument-error 'fsemaphore-post "fsemaphore?" fs_0))
         (let ((lock_0 (fsemaphore-lock fs_0)))
           (begin
             (lock-acquire lock_0)
             (begin0
               (let ((c_0 (fsemaphore-c fs_0)))
                 (if (zero? c_0)
                   (let ((b_0 (fsemaphore-dep-box fs_0)))
                     (let ((deps_0 (fsemaphore-dependents fs_0)))
                       (if (not (hash-empty? deps_0))
                         (let ((f_0
                                (hash-iterate-key
                                 deps_0
                                 (hash-iterate-first deps_0))))
                           (begin
                             (set-fsemaphore-dependents!
                              fs_0
                              (hash-remove deps_0 f_0))
                             (future-notify-dependent f_0)))
                         (begin
                           (set-fsemaphore-c! fs_0 1)
                           (if b_0
                             (begin
                               (set-fsemaphore-dep-box! fs_0 #f)
                               (set-box! b_0 #t)
                               (|#%app| wakeup-this-place))
                             (void))))))
                   (set-fsemaphore-c! fs_0 (add1 c_0))))
               (lock-release lock_0)))))))))
(define 1/fsemaphore-wait
  (|#%name|
   fsemaphore-wait
   (lambda (fs_0)
     (begin
       (begin
         (if (1/fsemaphore? fs_0)
           (void)
           (raise-argument-error 'fsemaphore-wait "fsemaphore?" fs_0))
         (begin
           (lock-acquire (fsemaphore-lock fs_0))
           (let ((c_0 (fsemaphore-c fs_0)))
             (if (zero? c_0)
               (let ((me-f_0 (current-future$1)))
                 (if me-f_0
                   (begin
                     (lock-acquire (future*-lock me-f_0))
                     (set-fsemaphore-dependents!
                      fs_0
                      (hash-set (fsemaphore-dependents fs_0) me-f_0 #t))
                     (set-future*-state! me-f_0 'fsema)
                     (lock-release (fsemaphore-lock fs_0))
                     (future-suspend)
                     (void))
                   (let ((dep-box_0
                          (let ((or-part_0 (fsemaphore-dep-box fs_0)))
                            (if or-part_0
                              or-part_0
                              (let ((b_0 (box #f)))
                                (begin
                                  (set-fsemaphore-dep-box! fs_0 b_0)
                                  b_0))))))
                     (begin
                       (lock-release (fsemaphore-lock fs_0))
                       (1/sync (fsemaphore-box-evt2.1 dep-box_0))
                       (1/fsemaphore-wait fs_0)))))
               (begin
                 (set-fsemaphore-c! fs_0 (sub1 c_0))
                 (lock-release (fsemaphore-lock fs_0)))))))))))
(define 1/fsemaphore-try-wait?
  (|#%name|
   fsemaphore-try-wait?
   (lambda (fs_0)
     (begin
       (begin
         (if (1/fsemaphore? fs_0)
           (void)
           (raise-argument-error 'fsemaphore-try-wait? "fsemaphore?" fs_0))
         (let ((lock_0 (fsemaphore-lock fs_0)))
           (begin
             (lock-acquire lock_0)
             (begin0
               (let ((c_0 (fsemaphore-c fs_0)))
                 (if (zero? c_0)
                   #f
                   (begin (set-fsemaphore-c! fs_0 (sub1 c_0)) #t)))
               (lock-release lock_0)))))))))
(define 1/fsemaphore-count
  (|#%name|
   fsemaphore-count
   (lambda (fs_0)
     (begin
       (begin
         (if (1/fsemaphore? fs_0)
           (void)
           (raise-argument-error 'fsemaphore-count "fsemaphore?" fs_0))
         (let ((lock_0 (fsemaphore-lock fs_0)))
           (begin
             (lock-acquire lock_0)
             (begin0 (fsemaphore-c fs_0) (lock-release lock_0)))))))))
(define 1/unsafe-os-thread-enabled?
  (|#%name| unsafe-os-thread-enabled? (lambda () (begin (|#%app| threaded?)))))
(define 1/unsafe-call-in-os-thread
  (|#%name|
   unsafe-call-in-os-thread
   (lambda (proc_0)
     (begin
       (begin
         (if (if (procedure? proc_0) (procedure-arity-includes? proc_0 0) #f)
           (void)
           (raise-argument-error
            'unsafe-call-in-os-thread
            "(procedure-arity-includes/c 0)"
            proc_0))
         (if threaded? (void) (raise-unsupported 'unsafe-call-in-os-thread))
         (|#%app|
          fork-pthread
          (lambda () (begin (start-atomic) (|#%app| proc_0))))
         (void))))))
(define struct:os-semaphore
  (make-record-type-descriptor*
   'os-semaphore
   #f
   (|#%nongenerative-uid| os-semaphore)
   #f
   #f
   3
   1))
(define effect_2314
  (struct-type-install-properties!
   struct:os-semaphore
   '(os-semaphore)
   3
   0
   #f
   (list (cons prop:authentic #t))
   (current-inspector)
   #f
   '(1 2)
   #f
   'os-semaphore))
(define os-semaphore1.1
  (|#%name|
   os-semaphore
   (record-constructor
    (make-record-constructor-descriptor struct:os-semaphore #f #f))))
(define os-semaphore?
  (|#%name| os-semaphore? (record-predicate struct:os-semaphore)))
(define os-semaphore-count
  (|#%name| os-semaphore-count (record-accessor struct:os-semaphore 0)))
(define os-semaphore-mutex
  (|#%name| os-semaphore-mutex (record-accessor struct:os-semaphore 1)))
(define os-semaphore-condition
  (|#%name| os-semaphore-condition (record-accessor struct:os-semaphore 2)))
(define set-os-semaphore-count!
  (|#%name| set-os-semaphore-count! (record-mutator struct:os-semaphore 0)))
(define 1/unsafe-make-os-semaphore
  (|#%name|
   unsafe-make-os-semaphore
   (lambda ()
     (begin
       (begin
         (if threaded? (void) (raise-unsupported 'unsafe-make-os-semaphore))
         (let ((app_0 (|#%app| host:make-mutex)))
           (os-semaphore1.1 0 app_0 (|#%app| host:make-condition))))))))
(define 1/unsafe-os-semaphore-post
  (|#%name|
   unsafe-os-semaphore-post
   (lambda (s_0)
     (begin
       (begin
         (if (os-semaphore? s_0)
           (void)
           (raise-argument-error
            'unsafe-os-semaphore-post
            "os-semaphore?"
            s_0))
         (|#%app| host:mutex-acquire (os-semaphore-mutex s_0))
         (if (zero? (os-semaphore-count s_0))
           (|#%app| host:condition-signal (os-semaphore-condition s_0))
           (void))
         (set-os-semaphore-count! s_0 (add1 (os-semaphore-count s_0)))
         (|#%app| host:mutex-release (os-semaphore-mutex s_0)))))))
(define 1/unsafe-os-semaphore-wait
  (|#%name|
   unsafe-os-semaphore-wait
   (lambda (s_0)
     (begin
       (begin
         (if (os-semaphore? s_0)
           (void)
           (raise-argument-error
            'unsafe-os-semaphore-wait
            "os-semaphore?"
            s_0))
         (|#%app| host:mutex-acquire (os-semaphore-mutex s_0))
         (letrec*
          ((loop_0
            (|#%name|
             loop
             (lambda ()
               (begin
                 (if (zero? (os-semaphore-count s_0))
                   (begin
                     (|#%app|
                      host:condition-wait
                      (os-semaphore-condition s_0)
                      (os-semaphore-mutex s_0))
                     (loop_0))
                   (set-os-semaphore-count!
                    s_0
                    (sub1 (os-semaphore-count s_0)))))))))
          (loop_0))
         (|#%app| host:mutex-release (os-semaphore-mutex s_0)))))))
(define raise-unsupported
  (lambda (who_0)
    (raise
     (let ((app_0
            (string-append
             (symbol->string who_0)
             ": unsupported on this platform")))
       (|#%app| exn:fail:unsupported app_0 (current-continuation-marks))))))
