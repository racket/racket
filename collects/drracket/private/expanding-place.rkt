#lang racket/base
(require racket/place
         racket/port
         racket/list
         "eval-helpers.rkt"
         compiler/cm
         syntax/readerr)
(provide start)

(struct exn-info (str src-vecs exn-stack missing-mods) #:prefab)

(struct job (cust response-pc working-thd stop-watching-abnormal-termination))

;; key : any (used by equal? for comparision, but back in the main place)
(struct handler (key proc))
(define handlers '())

(define module-language-parallel-lock-client
  'uninitialized-module-language-parallel-lock-client)

(define old-registry-chan (make-channel))

(define expanding-place-logger (make-logger 
                                'drracket-background-compilation
                                (current-logger)))
(define-syntax-rule
  (ep-log-info expr)
  (when (log-level? expanding-place-logger 'info)
    (log-message expanding-place-logger
                 'info
                 expr
                 (current-continuation-marks))))

(define (start p)
  ;; get the module-language-compile-lock in the initial message
  (set! module-language-parallel-lock-client
        (compile-lock->parallel-lock-client
         (place-channel-get p)
         (current-custodian)))
  
  ;; get the handlers in a second message
  (set! handlers (for/list ([lst (place-channel-get p)])
                   (define file (list-ref lst 0))
                   (define id (list-ref lst 1))
                   (handler lst (dynamic-require file id))))
  (let loop ([current-job #f]
             ;; the old-registry argument holds on to the namespace-module-registry
             ;; from a previous run in order to keep entries in the bytecode cache
             [old-registry #f])
    (sync
     (handle-evt 
      old-registry-chan
      (λ (reg) (loop current-job reg)))
     (handle-evt
      p
      (λ (message)
        (cond
          [(eq? message 'abort)
           (when current-job (abort-job current-job))
           (loop #f old-registry)]
          [(vector? message)
           (when current-job (abort-job current-job))
           (define program-as-string (vector-ref message 0))
           (define path (vector-ref message 1))
           (define response-pc (vector-ref message 2))
           (define settings (vector-ref message 3))
           (define pc-status-expanding-place (vector-ref message 4))
           (define currently-open-files (vector-ref message 5))
           (loop (new-job program-as-string path response-pc settings pc-status-expanding-place)
                 old-registry)]))))))

(define (abort-job job)
  (when (log-level? expanding-place-logger 'info)
    (define stack (continuation-mark-set->context
                   (continuation-marks 
                    (job-working-thd job))))
    (ep-log-info (format "expanding-place.rkt: kill; worker-thd stack (size ~a) dead? ~a:" 
                         (length stack)
                         (thread-dead? (job-working-thd job))))
    (for ([x (in-list stack)])
      (ep-log-info (format "  ~s" x))))
  ((job-stop-watching-abnormal-termination job))
  (custodian-shutdown-all (job-cust job)))

(struct exn:access exn:fail ())

(define (new-job program-as-string path response-pc settings pc-status-expanding-place)
  (define cust (make-custodian))
  (define exn-chan (make-channel))
  (define extra-exns-chan (make-channel))
  (define result-chan (make-channel))
  (define normal-termination (make-channel))
  (define abnormal-termination (make-channel))
  (define the-source (or path "unsaved editor"))
  (define orig-cust (current-custodian))
  (define (stop-watching-abnormal-termination) 
    (channel-put normal-termination #t))
  
  (define working-thd
    (parameterize ([current-custodian cust])
      (thread
       (λ ()
         (ep-log-info "expanding-place.rkt: 01 starting thread")
         (define sema (make-semaphore 0))
         (ep-log-info "expanding-place.rkt: 02 setting basic parameters")
         (set-basic-parameters/no-gui)
         
         (define loaded-paths '())
         (define original-path (make-parameter #f))
         (current-load/use-compiled
          (let ([ol (current-load/use-compiled)])
            (λ (path mod-name)
              (parameterize ([original-path path])
                (ol path mod-name)))))
         (current-load
          (let ([cl (current-load)])
            (λ (path mod-name)
              (set! loaded-paths
                    (cons (or (current-module-declare-source)
                              (original-path)
                              path)
                          loaded-paths))
              (cl path mod-name))))
         
         (ep-log-info "expanding-place.rkt: 03 setting module language parameters")
         (set-module-language-parameters settings
                                         module-language-parallel-lock-client
                                         null
                                         #:use-use-current-security-guard? #t)
         (ep-log-info "expanding-place.rkt: 04 setting directories")
         (let ([init-dir (get-init-dir path)])
           (current-directory init-dir)
           (current-directory-for-user init-dir))
         (current-load-relative-directory #f)
         (define sp (open-input-string program-as-string))
         (port-count-lines! sp)
         (ep-log-info "expanding-place.rkt: 05 installing security guard")
         (install-security-guard) ;; must come after the call to set-module-language-parameters
         (ep-log-info "expanding-place.rkt: 06 setting uncaught-exception-handler")
         (error-display-handler
          (let ([e-d-h (error-display-handler)])
            (λ (msg exn)
              (channel-put extra-exns-chan exn)
              (e-d-h msg exn))))
         (uncaught-exception-handler
          (λ (exn)
            (parameterize ([current-custodian orig-cust])
              (thread
               (λ ()
                 (stop-watching-abnormal-termination)
                 (semaphore-post sema)
                 (channel-put exn-chan (list exn loaded-paths)))))
            (semaphore-wait sema)
            ((error-escape-handler))))
         (ep-log-info "expanding-place.rkt: 07 starting read-syntax")
         (define stx
           (parameterize ([read-accept-reader #t])
             (read-syntax the-source sp)))
         (ep-log-info "expanding-place.rkt: 08 read")
         (when (eof-object? stx) 
           (define-values (line col pos) (port-next-location sp))
           (raise-read-eof-error "no program to process"
                                 the-source
                                 1 0 1 pos))
         (define-values (name lang transformed-stx)
           (transform-module path
                             (namespace-syntax-introduce stx)
                             raise-hopeless-syntax-error))
         (ep-log-info "expanding-place.rkt: 09 starting expansion")
         (define log-io? (log-level? expanding-place-logger 'warning))
         (define-values (in out) (if log-io? 
                                     (make-pipe)
                                     (values #f (open-output-nowhere))))
         (define io-sema (make-semaphore 0))
         (when log-io?
           (thread (λ () (catch-and-log in io-sema))))
         (define expanded 
           (parameterize ([current-output-port out]
                          [current-error-port out])
             (expand transformed-stx)))
         (when log-io?
           (close-output-port out)
           (semaphore-wait io-sema))
         (channel-put old-registry-chan 
                      (namespace-module-registry (current-namespace)))
         (place-channel-put pc-status-expanding-place 'finished-expansion)
         (ep-log-info "expanding-place.rkt: 10 expanded")
         (define handler-results
           (for/list ([handler (in-list handlers)])
             (list (handler-key handler)
                   ((handler-proc handler) expanded
                                           path
                                           the-source
                                           orig-cust))))
         (ep-log-info "expanding-place.rkt: 11 handlers finished")
         
         (parameterize ([current-custodian orig-cust])
           (thread
            (λ ()
              (stop-watching-abnormal-termination)
              (semaphore-post sema)
              (channel-put result-chan (list handler-results loaded-paths)))))
         (semaphore-wait sema)
         (ep-log-info "expanding-place.rkt: 12 finished")))))
  
  (thread
   (λ ()
     (let loop ([watch-dead? #t])
       (sync 
        (handle-evt 
         normal-termination
         (λ (x) (loop #f)))
        (if watch-dead?
            (handle-evt 
             (thread-dead-evt working-thd)
             (λ (x) 
               (ep-log-info "expanding-place.rkt: abnormal termination")
               (channel-put abnormal-termination #t)
               (loop #f)))
            never-evt)))))
  
  (thread
   (λ ()
     (let loop ([extra-exns '()])
       (sync
        (handle-evt
         abnormal-termination
         (λ (val) 
           (place-channel-put pc-status-expanding-place
                              'abnormal-termination)
           (place-channel-put 
            response-pc
            (vector 'abnormal-termination 
                    ;; note: this message is actually ignored: a string 
                    ;; constant is used back in the drracket place
                    "Expansion thread terminated unexpectedly"
                    '()
                    
                    ;; give up on dep paths in this case:
                    '()))))
        (handle-evt
         result-chan
         (λ (val+loaded-paths)
           (place-channel-put response-pc (vector 'handler-results 
                                                  (list-ref val+loaded-paths 0)
                                                  (list-ref val+loaded-paths 1)))))
        (handle-evt extra-exns-chan (λ (exn) (loop (cons exn extra-exns))))
        (handle-evt
         exn-chan
         (λ (exn+loaded-paths)
           (place-channel-put pc-status-expanding-place 'exn-raised)
           (define main-exn (list-ref exn+loaded-paths 0))
           (define exn-type
             (cond
               [(exn:access? main-exn)
                'access-violation]
               [(and (exn:fail:read? main-exn)
                     (andmap (λ (srcloc) (equal? (srcloc-source srcloc) the-source))
                             (exn:fail:read-srclocs main-exn)))
                'reader-in-defs-error]
               [(and (exn? main-exn)
                     (regexp-match #rx"expand: unbound identifier" (exn-message main-exn)))
                'exn:variable]
               [else 'exn]))
           
           (define (format-srcloc srcloc)
             (define pos
               (cond
                 [(and (srcloc-line srcloc)
                       (srcloc-column srcloc))
                  (format ":~a:~a" (srcloc-line srcloc) (srcloc-column srcloc))]
                 [(srcloc-line srcloc)
                  (format ":~a" (srcloc-line srcloc))]
                 [(srcloc-position srcloc)
                  (format "::~a" (srcloc-position srcloc))]
                 [else ""]))
             (format "~a~a" (srcloc-source srcloc) pos))
             
           (define exn-infos
             (for/list ([an-exn (in-list (cons main-exn extra-exns))])
               (exn-info 
                (trim-message
                 (if (exn? an-exn) 
                     (regexp-replace* #rx"[ \t]*\n[ \t]*" (exn-message an-exn) " ") 
                     (format "uncaught exn: ~s" an-exn)))
                (if (exn:srclocs? an-exn)
                    (sort
                     (for/list ([srcloc ((exn:srclocs-accessor an-exn) an-exn)]
                                #:when (and (srcloc? srcloc)
                                            (equal? the-source (srcloc-source srcloc))
                                            (srcloc-position srcloc)
                                            (srcloc-span srcloc)))
                       (vector (srcloc-position srcloc)
                               (srcloc-span srcloc)))
                     <
                     #:key (λ (x) (vector-ref x 0)))
                    '())
                (if (exn? an-exn)
                    (let ([ctxt 
                           (continuation-mark-set->context
                            (exn-continuation-marks an-exn))])
                      (for/list ([ctxt-elem (if (< (length ctxt) 100)
                                                ctxt
                                                (take ctxt 100))])
                        (define name (car ctxt-elem))
                        (define loc (cdr ctxt-elem))
                        (cond
                          [(not name) (format-srcloc loc)]
                          [(not loc) (format "~a" name)]
                          [else (format "~a:~a" (format-srcloc loc) name)])))
                    '())
                (and (exn:missing-module? an-exn)
                     ((exn:missing-module-accessor an-exn) an-exn)))))
           (place-channel-put 
            response-pc
            (vector 
             exn-type
             exn-infos
             (list-ref exn+loaded-paths 1)))))))))
  
  (job cust response-pc working-thd stop-watching-abnormal-termination))

(define (catch-and-log port sema)
  (let loop ()
    (sync
     (handle-evt (read-line-evt port 'linefeed)
                 (λ (l)
                   (cond
                     [(eof-object? l)
                      (semaphore-post sema)]
                     [else
                      (log-warning (format "online comp io: ~a" l))
                      (loop)]))))))

(define (raise-hopeless-syntax-error . args)
  (apply raise-syntax-error '|Module Language| args))

(define (install-security-guard)
  (current-security-guard
   (make-security-guard
    (current-security-guard)
    (λ (prim path whats)
      (when (or (member 'write whats)
                (member 'execute whats)
                (member 'delete whats))
        (raise (exn:access (format "~a: forbidden ~a access to ~a" prim whats path)
                           (current-continuation-marks)))))
    (λ (prim target port what)
      (raise (exn:access (format "~a: forbidden ~a access to ~a:~a" prim what target port)
                         (current-continuation-marks))))
    (λ (prim path1 path2)
      (raise (exn:access (format "~a: forbidden to link ~a to ~a" prim path1 path2)
                         (current-continuation-marks)))))))

;; trim-message : string -> string[200 chars max]
(define (trim-message str)
  (cond
    [(<= (string-length str) 200)
     str]
    [else
     (define prefix-len 99)
     (define suffix-len 98)
     (define middle "...")
     
     ;; (+ prefix-len suffix-len (string-length middle)) must be 200 (or less)
     (string-append (substring str 0 prefix-len)
                    middle
                    (substring str (- (string-length str) suffix-len) (string-length str)))]))
