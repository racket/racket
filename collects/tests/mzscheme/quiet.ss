
(namespace-variable-value 'quiet-load #f
  (lambda ()
    (namespace-set-variable-value! 'quiet-load
      (let ([argv (current-command-line-arguments)])
        (if (= 1 (vector-length argv)) (vector-ref argv 0) "all.ss")))))

(namespace-variable-value 'real-error-port #f
  (lambda ()
    (let ([err  (current-error-port)]
          [exit (exit-handler)]
          [errh (uncaught-exception-handler)]
          [esch (error-escape-handler)]
          [cust (current-custodian)]
          [orig-thread (current-thread)])
      (namespace-set-variable-value! 'real-error-port err)
      ;; we're loading this for the first time:
      ;; -- make real errors show
      ;;    (can't override current-exception-handler alone, since the escape
      ;;     handler is overridden to avoid running off, so use the first to
      ;;     save the data and the second to show it)
      (let ([last-error #f])
        (uncaught-exception-handler (lambda (e) 
                                      (when (eq? (current-thread) orig-thread)
                                        (set! last-error e))
                                      (errh e))))
      ;; -- set up a timeout
      (thread (lambda ()
                (sleep 600)
                (fprintf err "\n\nTIMEOUT -- ABORTING!\n")
                (exit 3)
                ;; in case the above didn't work for some reason
                (sleep 60)
                (custodian-shutdown-all cust))))))

(let ([p (make-output-port
          'quiet always-evt (lambda (str s e nonblock? breakable?) (- e s))
          void)])
  (call-with-continuation-prompt
   (lambda ()
     (parameterize ([current-output-port p] [current-error-port p])
       (load-relative quiet-load)))
   (default-continuation-prompt-tag)
   (lambda ()
     (when last-error
       (fprintf err "ERROR: ~a\n"
                (if (exn? last-error) (exn-message last-error) last-error))
       (exit 2))))
  (report-errs #t))
