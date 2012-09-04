#lang racket/base

(require racket/sandbox racket/port racket/class)

(provide run-inside-optimization-coach-sandbox
         make-file-predicate)

(define (log-output in done-chan)
  (let loop ()
    (sync (handle-evt
           (read-line-evt in 'linefeed)
           (lambda (line)
             (cond [(eof-object? line) (channel-put done-chan 'done)]
                   [else
                    (log-warning
                     (format "Optimization Coach Program Output: ~a" line))
                    (loop)]))))))

(define (run-inside-optimization-coach-sandbox this thunk)
  (call-with-trusted-sandbox-configuration
   (lambda ()
     (define port-name  (send this get-port-name))
     ;; If the sandboxed program produces any output, log it as `warning'.
     ;; Mimics what check-syntax does.
     (define log-output? (log-level? (current-logger) 'warning))
     (define-values (log-in log-out)
       (if log-output? (make-pipe) (values #f (open-output-nowhere))))
     (define log-done-chan (make-channel))
     (when log-output? (thread (lambda () (log-output log-in log-done-chan))))
     ;; Set up the environment.
     (begin0
         (parameterize
             ([current-namespace (make-base-namespace)]
              [current-load-relative-directory
               (if (path-string? port-name)
                   (let-values ([(base name _) (split-path port-name)])
                     base)
                   (current-load-relative-directory))]
              [read-accept-reader #t]
              [current-output-port log-out]
              [current-error-port  log-out])
           (thunk))
       (when log-output?
         (close-output-port log-out)
         (sync log-done-chan))))))

;; Returns a predicate that, given a path, returns whether it corresponds
;; to the right file.
(define (make-file-predicate this)
  (define portname (send this get-port-name))
  (define unsaved-file?
    (and (symbol? portname)
         (regexp-match #rx"^unsaved-editor" (symbol->string portname))))
  (define good-portname-cache #f)
  (lambda (path) ; (or/c path? #f)
    (cond [(and good-portname-cache ; cache is populated
                (equal? path good-portname-cache))
           #t]
          [good-portname-cache ; cache is populated, but we have the wrong file
           #f]
          [unsaved-file?
           ;; we assume that any log entry without a filename comes from
           ;; the unsaved editor
           (not path)]
          ;; no cache, ask directly
          [(send this port-name-matches? path)
           (set! good-portname-cache path) ; populate cache
           #t]
          [else ; different file
           #f])))
