#lang racket/base

(require "config.rkt" racket/date racket/port)

(provide current-session)
(define current-session (make-parameter #f))

;; A convenient function to print log lines (which really just assembles a
;; string to print in one shot, and flushes the output)
(provide log-line)
(define (log-line fmt . args)
  (define line (format "~a\n" (apply format fmt args)))
  (display line (current-error-port)))

(define (prefix)
  (parameterize ([date-display-format 'iso-8601])
    (format "[~a|~a] "
            (or (current-session) '-)
            (date->string (seconds->date (current-seconds)) #t))))

;; Implement a logger by making the current-error-port show prefix tags and
;; output the line on the output port
(define (make-logger-port out log)
  (if (and (not out) (not log))
    (open-output-nowhere)
    (let ([prompt? #t]
          [sema (make-semaphore 1)]
          [outps (filter values (list out log))])
      (make-output-port
       'logger-output
       (car outps)
       (lambda (buf start end imm? break?)
         (dynamic-wind
           (lambda () (semaphore-wait sema))
           (lambda ()
             (if (= start end)
               (begin (for-each flush-output outps) 0)
               (let ([nl (regexp-match-positions #rx#"\n" buf start end)])
                 ;; may be problematic if this hangs...
                 (when prompt?
                   (let ([pfx (prefix)])
                     (for ([p (in-list outps)]) (display pfx p)))
                   (set! prompt? #f))
                 (let* ([nl  (and nl (cdar nl))]
                        [end (or nl end)]
                        [ls (for/list ([p (in-list outps)])
                              (write-bytes-avail* buf p start end))]
                        [l (car ls)])
                   (when (and (pair? (cdr ls))
                              (not (equal? (car ls) (cadr ls))))
                     (display "WARNING: incomplete write to log file\n"
                              (car outps)))
                   (when (= l (- end start))
                     (map flush-output outps)
                     (when nl (set! prompt? #t)))
                   l))))
           (lambda () (semaphore-post sema))))
       (lambda () (map close-output-port outps))))))

;; Install this wrapper as the current error port
(provide install-logger-port)
(define (install-logger-port)
  (current-error-port
   (make-logger-port
    (and (get-conf 'log-output) (current-output-port))
    (cond [(get-conf 'log-file)
           => (lambda (f) (open-output-file f #:exists 'append))]
          [else #f]))))
