(module debugger-model mzscheme
  (require (lib "unit.ss")
           (lib "mred.ss" "mred")
           (lib "debugger-annotate.ss" "stepper/private")
           (lib "marks.ss" "stepper/private")
           "mztake-structs.ss"
           "private/load-annotator.ss"
           "private/more-useful-code.ss")
  
  (provide debugger-model@)
  
  (define debugger-model@
    (unit
      (import receive-result
              process)
      (export run)
      
      
      (define run-semaphore (debug-process-run-semaphore process))
      (define ev (make-eventspace))
      
      
      (define ((break client) mark-set kind final-mark)
        (let ([mark-list (continuation-mark-set->list mark-set debug-key)])
          (parameterize ([current-eventspace ev])
            (queue-callback (lambda () (receive-result (make-normal-breakpoint-info (cons final-mark mark-list) client)))))
          (semaphore-wait run-semaphore)))
      
      
      (define ((err-display-handler source) message exn)
        (thread (lambda () (receive-result (make-error-breakpoint-info (list source exn))))))
      
      
      (define (annotate-module-with-error-handler stx err-hndlr)
        (syntax-case stx (module #%plain-module-begin)
          [(module name req (#%plain-module-begin body ...))
           #`(module name req (#%plain-module-begin
                               (error-display-handler #,err-hndlr)
                               body ...))]))
      
      
      ; Return run functions
      (define (run)
        (parameterize ([error-display-handler (err-display-handler "Trying to load client code...")])
          (let* ([clients (debug-process-clients process)]
                 
                 [all-used-module-paths (map (lambda (c) (debug-client-modpath c))
                                             clients)]
                 
                 [path->client (lambda (path)
                                 (car (filter (lambda (c) (equal? (debug-client-modpath c) path))
                                              clients)))]
                 
                 [annotate-module? (lambda (fn m)
                                     (memf (lambda (sym) (eq? sym fn))
                                           all-used-module-paths))]
                 
                 [annotator (lambda (fn m stx)
                              (printf "annotating: ~a~n~n" fn)
                              (let* ([client (path->client fn)]
                                     [breakpoints (hash-keys (debug-client-tracepoints client))]
                                     [stx (annotate (expand stx) breakpoints fn (break client))])
                                ; add an error handler so anything that goes wrong points to the correct module
                                (annotate-module-with-error-handler stx (err-display-handler fn))))]
                 
                 ;TODO hack
                 [_ (print "hack -- main-mod problem")]
                 [main-mod (first all-used-module-paths)])
            
            (parameterize ([current-custodian (debug-process-custodian process)]
                           [current-namespace (make-namespace-with-mred)]
                           [error-display-handler (err-display-handler (format "Loading module ~a..." main-mod))])
              (require/annotations `(file ,main-mod) annotate-module? annotator))))))))