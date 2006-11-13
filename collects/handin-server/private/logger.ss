(module logger mzscheme
  (require (lib "date.ss"))

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

  ;; Implement a logger by capturing current-error-port and printing a prefix,
  ;; provide a function to install this port
  (define (make-logger-port stderr)
    (define prompt? #t)
    (define sema (make-semaphore 1))
    (make-output-port
     'logger-output
     stderr
     (lambda (buf start end imm? break?)
       (dynamic-wind
         (lambda () (semaphore-wait sema))
         (lambda ()
           (if (= start end)
             (begin (flush-output stderr) 0)
             (let ([nl (regexp-match-positions #rx#"\n" buf start end)])
               ;; may be problematic if this hangs...
               (when prompt? (display (prefix) stderr) (set! prompt? #f))
               (if (not nl)
                 (write-bytes-avail* buf stderr start end)
                 (let* ([nl (cdar nl)]
                        [l  (write-bytes-avail* buf stderr start nl)])
                   (when (= l (- nl start))
                     ;; pre-newline part written
                     (flush-output stderr) (set! prompt? #t))
                   l)))))
        (lambda () (semaphore-post sema))))
     (lambda () (close-output-port stderr))))

  ;; Install this wrapper on the current error port
  (provide install-logger-port)
  (define (install-logger-port)
    (current-error-port (make-logger-port (current-error-port)))))
