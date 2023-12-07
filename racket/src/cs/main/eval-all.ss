
(define (eval-all i)
  (let loop ()
    (define expr (read i))
    (unless (eof-object? expr)
      (call-with-values (lambda ()
                          (call-with-continuation-prompt
                           (lambda ()
                             (eval `(|#%top-interaction| . ,expr)))
                           (default-continuation-prompt-tag)
                           (lambda (proc)
                             ;; continue escape to set error status:
                             (abort-current-continuation (default-continuation-prompt-tag) proc))))
        (lambda vals
          (for-each (lambda (v)
                      (|#%app| (current-print) v)
                      (flush-output))
                    vals)))
      (loop))))
