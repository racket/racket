#lang racket/base
(require racket/place
         "eval-helpers.rkt"
         compiler/cm)
(provide start)

(struct job (cust response-pc))

;; key : any (used by equal? for comparision, but back in the main place)
(struct handler (key proc))
(define handlers '())

(define module-language-parallel-lock-client
  'uninitialized-module-language-parallel-lock-client)

(define (start p)
  ;; get the module-language-compile-lock in the initial message
  (set! module-language-parallel-lock-client
        (compile-lock->parallel-lock-client
         (place-channel-get p)))
  
  ;; get the handlers in a second message
  (set! handlers (for/list ([lst (place-channel-get p)])
                   (define file (list-ref lst 0))
                   (define id (list-ref lst 1))
                   (handler lst (dynamic-require file id))))
  (let loop ([current-job #f])
    (sync
     (handle-evt
      p
      (λ (message)
        (cond
          [(eq? message 'abort)
           (when current-job (abort-job current-job))
           (loop #f)]
          [(vector? message)
           (when current-job (abort-job current-job))
           (define program-as-string (vector-ref message 0))
           (define path (vector-ref message 1))
           (define response-pc (vector-ref message 2))
           (define settings (vector-ref message 3))
           (loop (new-job program-as-string path response-pc settings))]))))))

(define (abort-job job)
  (custodian-shutdown-all (job-cust job))
  (place-channel-put (job-response-pc job) #f))

(struct exn:access exn:fail ())

(define (new-job program-as-string path response-pc settings)
  (define cust (make-custodian))
  (define exn-chan (make-channel))
  (define result-chan (make-channel))
  (define normal-termination (make-channel))
  (define abnormal-termination (make-channel))
  (define the-source (or path "unsaved editor"))
  (define orig-cust (current-custodian))
  
  (define thd
    (parameterize ([current-custodian cust])
      (thread
       (λ ()
         (log-info "expanding-place.rkt: starting thread")
         (define sema (make-semaphore 0))
         (set-basic-parameters/no-gui)
         (set-module-language-parameters settings
                                         module-language-parallel-lock-client
                                         #:use-use-current-security-guard? #t)
         (when path
           (let-values ([(base name dir?) (split-path path)])
             (current-directory base)
             (current-load-relative-directory base)))
         (define sp (open-input-string program-as-string))
         (port-count-lines! sp)
         (install-security-guard) ;; must come after the call to set-module-language-parameters
         (uncaught-exception-handler
          (λ (exn)
            (parameterize ([current-custodian orig-cust])
              (thread
               (λ ()
                 (channel-put normal-termination #t)
                 (semaphore-post sema)
                 (channel-put exn-chan exn))))
            (semaphore-wait sema)
            ((error-escape-handler))))
         (log-info "expanding-place.rkt: starting read-syntax")
         (define stx
           (parameterize ([read-accept-reader #t])
             (read-syntax the-source sp)))
         (log-info "expanding-place.rkt: read")
         (when (syntax? stx) ;; could be eof
           (define-values (name lang transformed-stx)
             (transform-module path
                               (namespace-syntax-introduce stx)
                               raise-hopeless-syntax-error))
           (log-info "expanding-place.rkt: starting expansion")
           (define expanded (expand transformed-stx))
           (log-info "expanding-place.rkt: expanded")
           (define handler-results
             (for/list ([handler (in-list handlers)])
               (list (handler-key handler)
                     ((handler-proc handler) expanded
                                             path
                                             the-source))))
           (log-info "expanding-place.rkt: handlers finished")
           (parameterize ([current-custodian orig-cust])
             (thread
              (λ ()
                (channel-put normal-termination #t)
                (semaphore-post sema)
                (channel-put result-chan handler-results))))
           (semaphore-wait sema))))))
  
  (thread
   (λ ()
     (sync 
      (handle-evt 
       normal-termination
       (λ (x) (void)))
      (handle-evt 
       (thread-dead-evt thd)
       (λ (x) (channel-put abnormal-termination #t))))))
  
  (thread
   (λ ()
     (sync
      (handle-evt
       abnormal-termination
       (λ (val) 
         (place-channel-put 
          response-pc
          (vector 'abnormal-termination))))
      (handle-evt
       result-chan
       (λ (val)
         (place-channel-put response-pc (vector 'handler-results val))))
      (handle-evt
       exn-chan
       (λ (exn)
         (place-channel-put 
          response-pc
          (cond
            [(exn:access? exn)
             (vector 'access-violation (exn-message exn))]
            [(and (exn:fail:read? exn)
                  (andmap (λ (srcloc) (equal? (srcloc-source srcloc) the-source))
                          (exn:fail:read-srclocs exn)))
             ;; figure the syntax colorer can deal 
             ;; with these better than we can
             (vector 'no-errors)]
            [else
             (vector 
              'exn
              (trim-message 
               (if (exn? exn) 
                   (regexp-replace* #rx"[ \t]*\n[ \t]*" (exn-message exn) " ") 
                   (format "uncaught exn: ~s" exn)))
              (if (exn:srclocs? exn)
                  (sort
                   (filter
                    values
                    (for/list ([srcloc ((exn:srclocs-accessor exn) exn)])
                      (and (srcloc? srcloc)
                           (equal? the-source (srcloc-source srcloc))
                           (srcloc-position srcloc)
                           (srcloc-span srcloc)
                           (vector (srcloc-position srcloc)
                                   (srcloc-span srcloc)))))
                   <
                   #:key (λ (x) (vector-ref x 0)))
                  '()))])))))))
  (job cust response-pc))

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
