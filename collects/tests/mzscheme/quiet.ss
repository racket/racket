
(namespace-variable-value 'quiet-load #f
  (lambda ()
    (namespace-set-variable-value! 'quiet-load
      (let ([argv (current-command-line-arguments)])
        (if (= 1 (vector-length argv)) (vector-ref argv 0) "all.ss")))))

(namespace-variable-value 'real-error-port #f
  (lambda ()
    (let ([err  (current-error-port)]
          [exit (exit-handler)]
          [errh (current-exception-handler)]
          [esch (error-escape-handler)]
          [cust (current-custodian)])
      (namespace-set-variable-value! 'real-error-port err)
      ;; we're loading this for the first time:
      ;; -- make real errors show
      ;;    (can't override current-exception-handler alone, since the escape
      ;;     handler is overridden to avoid running off, so use the first to
      ;;     save the data and the second to show it)
      (let ([last-error #f])
        (current-exception-handler (lambda (e) (set! last-error e) (errh e)))
        (error-escape-handler
         (lambda ()
           (fprintf err "ERROR: ~a\n"
                    (if (exn? last-error) (exn-message last-error) last-error))
           (exit 2))))
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
  (parameterize ([current-output-port p] [current-error-port p])
    (load-relative quiet-load))
  (report-errs #t))
