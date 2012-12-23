#lang racket/base
(require racket/place
         racket/port
         "eval-helpers.rkt"
         compiler/cm)
(provide start)

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
  (define result-chan (make-channel))
  (define normal-termination (make-channel))
  (define abnormal-termination (make-channel))
  (define the-source (or path "unsaved editor"))
  (define orig-cust (current-custodian))
  
  (define working-thd
    (parameterize ([current-custodian cust])
      (thread
       (λ ()
         (ep-log-info "expanding-place.rkt: 01 starting thread")
         (define sema (make-semaphore 0))
         (ep-log-info "expanding-place.rkt: 02 setting basic parameters")
         (set-basic-parameters/no-gui)
         (ep-log-info "expanding-place.rkt: 03 setting module language parameters")
         (set-module-language-parameters settings
                                         module-language-parallel-lock-client
                                         #:use-use-current-security-guard? #t)
         (ep-log-info "expanding-place.rkt: 04 setting directories")
         (let ([init-dir (get-init-dir path)])
           (current-directory init-dir))
         (current-load-relative-directory #f)
         (define sp (open-input-string program-as-string))
         (port-count-lines! sp)
         (ep-log-info "expanding-place.rkt: 05 installing security guard")
         (install-security-guard) ;; must come after the call to set-module-language-parameters
         (ep-log-info "expanding-place.rkt: 06 setting uncaught-exception-handler")
         (uncaught-exception-handler
          (λ (exn)
            (parameterize ([current-custodian orig-cust])
              (thread
               (λ ()
                 (stop-watching-abnormal-termination)
                 (semaphore-post sema)
                 (channel-put exn-chan exn))))
            (semaphore-wait sema)
            ((error-escape-handler))))
         (ep-log-info "expanding-place.rkt: 07 starting read-syntax")
         (define stx
           (parameterize ([read-accept-reader #t])
             (read-syntax the-source sp)))
         (ep-log-info "expanding-place.rkt: 08 read")
         (when (syntax? stx) ;; could be eof
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
           (define original-path (make-parameter #f))
           (define loaded-paths '())
           (define expanded 
             (parameterize ([current-output-port out]
                            [current-error-port out]
                            [current-load/use-compiled
                             (let ([ol (current-load/use-compiled)])
                               (λ (path mod-name)
                                 (parameterize ([original-path path])
                                   (ol path mod-name))))]
                            [current-load
                             (let ([cl (current-load)])
                               (λ (path mod-name)
                                 (set! loaded-paths
                                       (cons (or (current-module-declare-source)
                                                 (original-path)
                                                 path)
                                             loaded-paths))
                                 (cl path mod-name)))])
               (expand transformed-stx)))
           (when log-io?
             (close-output-port out)
             (semaphore-wait io-sema))
           (channel-put old-registry-chan 
                        (namespace-module-registry (current-namespace)))
           (place-channel-put pc-status-expanding-place (void))
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
           (ep-log-info "expanding-place.rkt: 12 finished"))))))
  
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
     (sync
      (handle-evt
       abnormal-termination
       (λ (val) 
         (place-channel-put 
          response-pc
          (vector 'abnormal-termination 
                  ;; note: this message is actually ignored: a string 
                  ;; constant is used back in the drracket place
                  "Expansion thread terminated unexpectedly"
                  '()))))
      (handle-evt
       result-chan
       (λ (val+loaded-files)
         (place-channel-put response-pc (vector 'handler-results 
                                                (list-ref val+loaded-files 0)
                                                (list-ref val+loaded-files 1)))))
      (handle-evt
       exn-chan
       (λ (exn)
         (place-channel-put 
          response-pc
          (vector 
           (cond
             [(exn:access? exn)
              'access-violation]
             [(and (exn:fail:read? exn)
                   (andmap (λ (srcloc) (equal? (srcloc-source srcloc) the-source))
                           (exn:fail:read-srclocs exn)))
              'reader-in-defs-error]
             [(and (exn? exn)
                   (regexp-match #rx"expand: unbound identifier" (exn-message exn)))
              'exn:variable]
             [else 'exn])
           (trim-message 
            (if (exn? exn) 
                (regexp-replace* #rx"[ \t]*\n[ \t]*" (exn-message exn) " ") 
                (format "uncaught exn: ~s" exn)))
           (if (exn:srclocs? exn)
               (sort
                (for/list ([srcloc ((exn:srclocs-accessor exn) exn)]
                           #:when (and (srcloc? srcloc)
                                       (equal? the-source (srcloc-source srcloc))
                                       (srcloc-position srcloc)
                                       (srcloc-span srcloc)))
                  (vector (srcloc-position srcloc)
                          (srcloc-span srcloc)))
                <
                #:key (λ (x) (vector-ref x 0)))
               '()))))))))
  
  (define (stop-watching-abnormal-termination) 
    (channel-put normal-termination #t))
    
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
