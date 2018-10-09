#lang racket

(provide main)

(define (main mode)
  (define out
    (place in
           (for/list ((n (in-naturals)))
             (place-channel-get in))))

  (define i
    (case mode
      [("tcp")
       (define PORT 12346)
       (define listener (tcp-listen PORT 100 #t))
       (define-values (i o) (tcp-connect "127.0.0.1" PORT))
       i]
      [else
       (current-input-port)]))
  (for ((n (in-naturals)))
    (printf "sending port ~a\n" n)
    (place-channel-put out i)))

(module+ test
  (require racket/system
           compiler/find-exe)

  (define (check mode rx)
    (define s (open-output-bytes))
    (define r
      (parameterize ([current-error-port s]
                     [current-output-port (open-output-bytes)])
        (system* (find-exe)
                 "-tm"
                 (variable-reference->module-source
                  (#%variable-reference))
                 mode)))
    (when r
      (error "expected process to exit with failure"))
    (unless (regexp-match? rx (get-output-bytes s))
      (error "output did not match expected pattern: "
             (get-output-bytes s))))

  (check "tcp" "socket dup failed|error during dup of file descriptor")
  (check "stdio" "port dup failed|error during dup of file descriptor"))

