(module logger mzscheme
  (require "config.ss" (lib "date.ss") (lib "port.ss"))

  (provide current-session)
  (define current-session (make-parameter #f))

  ;; A convenient function to print log lines (which really just assembles a
  ;; string to print in one shot, and flushes the output)
  (provide log-line)
  (define (log-line fmt . args)
    (let ([line (format "~a\n" (apply format fmt args))])
      (display line (current-error-port))))

  (define (prefix)
    (parameterize ([date-display-format 'iso-8601])
      (format "[~a|~a] "
              (or (current-session) '-)
              (date->string (seconds->date (current-seconds)) #t))))

  (define (combine-outputs o1 o2)
    (let-values ([(i o) (make-pipe)])
      (thread
       (lambda ()
         (let loop ()
           (let ([line (read-bytes-line i)])
             (if (eof-object? line)
               (begin (close-output-port o1) (close-output-port o2))
               (begin (write-bytes line o1) (newline o1) (flush-output o1)
                      (write-bytes line o2) (newline o2) (flush-output o2)
                      (loop)))))))
      o))

  ;; Implement a logger by making the current-error-port show prefix tags and
  ;; output the line on the output port
  (define (make-logger-port out log)
    (if (and (not out) (not log))
      ;; /dev/null-like output port
      (make-output-port 'nowhere
                        always-evt
                        (lambda (buf start end imm? break?) (- end start))
                        void)
      (let ([prompt? #t]
            [sema (make-semaphore 1)]
            [outp (cond [(not log) out]
                        [(not out) log]
                        [else (combine-outputs out log)])])
        (make-output-port
         'logger-output
         outp
         (lambda (buf start end imm? break?)
           (dynamic-wind
             (lambda () (semaphore-wait sema))
             (lambda ()
               (if (= start end)
                 (begin (flush-output outp) 0)
                 (let ([nl (regexp-match-positions #rx#"\n" buf start end)])
                   ;; may be problematic if this hangs...
                   (when prompt? (display (prefix) outp) (set! prompt? #f))
                   (if (not nl)
                     (write-bytes-avail* buf outp start end)
                     (let* ([nl (cdar nl)]
                            [l  (write-bytes-avail* buf outp start nl)])
                       (when (= l (- nl start))
                         ;; pre-newline part written
                         (flush-output outp) (set! prompt? #t))
                       l)))))
             (lambda () (semaphore-post sema))))
         (lambda () (close-output-port outp))))))

  ;; Install this wrapper as the current error port
  (provide install-logger-port)
  (define (install-logger-port)
    (current-error-port
     (make-logger-port
      (and (get-conf 'log-output) (current-output-port))
      (cond [(get-conf 'log-file) => (lambda (f) (open-output-file f 'append))]
            [else #f])))))
