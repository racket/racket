
(namespace-variable-value 'quiet-load #f
  (lambda ()
    (namespace-set-variable-value! 'quiet-load
      (let ([argv (current-command-line-arguments)])
        (if (= 1 (vector-length argv)) (vector-ref argv 0) "all.rktl")))))

(define timeout-thread #f)

(namespace-variable-value 'real-output-port #f
  (lambda ()
    (let ([outp (current-output-port)]
          [errp (current-error-port)]
          [exit (exit-handler)]
          [errh (uncaught-exception-handler)]
          [esch (error-escape-handler)]
          [cust (current-custodian)]
          [orig-thread (current-thread)])
      (namespace-set-variable-value! 'real-output-port outp)
      (namespace-set-variable-value! 'real-error-port  errp)
      (namespace-set-variable-value! 'last-error #f)
      ;; we're loading this for the first time:
      ;; make real errors show by remembering the exn
      ;; value, and then printing it on abort.
      (uncaught-exception-handler (lambda (e) 
                                    (when (eq? (current-thread) orig-thread)
                                      (set! last-error e))
                                    (errh e)))
      ;; -- set up a timeout
      (set! timeout-thread
            (thread
             (lambda ()
               (sleep 1200)
               (fprintf errp "\n\n~aTIMEOUT -- ABORTING!\n" Section-prefix)
               (exit 3)
               ;; in case the above didn't work for some reason
               (sleep 60)
               (custodian-shutdown-all cust)))))))

(let ([p (make-output-port
          'quiet always-evt (lambda (str s e nonblock? breakable?) (- e s))
          void)])
  (call-with-continuation-prompt
   (lambda ()
     (parameterize ([current-output-port p] [current-error-port p])
       (load-relative quiet-load))
     (kill-thread timeout-thread))
   (default-continuation-prompt-tag)
   (lambda (thunk)
     (when last-error
       (fprintf real-error-port "~aERROR: ~a\n"
                Section-prefix
                (if (exn? last-error) (exn-message last-error) last-error))
       (exit 2))))
  (report-errs #t))
