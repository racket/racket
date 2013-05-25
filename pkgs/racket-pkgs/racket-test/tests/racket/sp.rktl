
(load-relative "testing.rktl")

(require mzlib/process)

(Section 'subprocess)

(define self
  (parameterize ([current-directory (find-system-path 'orig-dir)])
    (find-executable-path (find-system-path 'exec-file) #f)))

(unless (eq? 'windows (system-type))
  (let ([try
         (lambda (post-shutdown?)
           (let ([l (parameterize ([subprocess-group-enabled (not post-shutdown?)])
                      (process* self
                                "-e"
                                (format "(define l (process* \"~a\" \"-e\" \"(let loop () (loop))\"))" self)
                                "-e"
                                "(displayln (list-ref l 2))"
                                "-e"
                                "(flush-output)"
                                "-e"
                                "(let loop () (loop))"))]
                 [running? (lambda (sub-pid)
                             (regexp-match?
                              (format "(?m:^ *~a(?=[^0-9]))" sub-pid)
                              (let ([s (open-output-string)])
                                (parameterize ([current-output-port s]
                                               [current-input-port (open-input-string "")])
                                  (system (format "ps x")))
                                (get-output-string s))))])
             (let ([sub-pid (read (car l))])
               (test 'running (list-ref l 4) 'status)
               (test #t running? sub-pid)
               ((list-ref l 4) 'kill)
               ((list-ref l 4) 'wait)
               (test 'done-error (list-ref l 4) 'status)
               (test post-shutdown? running? sub-pid)
               (when post-shutdown?
                 (parameterize ([current-input-port (open-input-string "")])
                   (system (format "kill ~a" sub-pid)))))))])
    (try #t)
    (try #f)))
