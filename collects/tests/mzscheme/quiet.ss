
(namespace-variable-value 'quiet-load #f
  (lambda ()
    (namespace-set-variable-value! 'quiet-load
      (let ([argv (current-command-line-arguments)])
        (if (= 1 (vector-length argv)) (vector-ref argv 0) "all.ss")))))

(namespace-variable-value 'real-error-port #f
  (lambda ()
    (let ([e (current-error-port)] [ex (exit-handler)] [c (current-custodian)])
      (namespace-set-variable-value! 'real-error-port e)
      ;; we're loading this for the first time -- set up a timeout
      (thread (lambda ()
                (sleep 600) (fprintf e "\n\nTIMEOUT -- ABORTING!\n") (ex 2)
                ;; in case the above didn't work for some reason
                (sleep 60) (custodian-shutdown-all c))))))

(let ([p (make-output-port
          'quiet always-evt (lambda (str s e nonblock? breakable?) (- e s))
          void)])
  (parameterize ([current-output-port p] [current-error-port p])
    (load-relative quiet-load))
  (report-errs #t))
