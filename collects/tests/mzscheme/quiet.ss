
(namespace-variable-value 'quiet-load #f
  (lambda ()
    (namespace-set-variable-value! 'quiet-load
      (let ([argv (current-command-line-arguments)])
        (if (= 1 (vector-length argv)) (vector-ref argv 0) "all.ss")))))

(namespace-variable-value 'real-error-port #f
  (lambda ()
    (namespace-set-variable-value! 'real-error-port (current-error-port))))

(let ([p (make-output-port
          'quiet always-evt (lambda (str s e nonblock? breakable?) (- e s))
          void)])
  (parameterize ([current-output-port p] [current-error-port p])
    (load-relative quiet-load))
  (report-errs #t))
