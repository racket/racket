#lang racket/base
(require "config.rkt")

(provide reader-error
         bad-syntax-error
         catch-and-reraise-as-reader)

(define (reader-error in config
                      #:continuation-marks [continuation-marks (current-continuation-marks)]
                      #:due-to [due-to #\x]
                      #:who [who (if (read-config-for-syntax? config)
                                     'read-syntax
                                     'read)]
                      #:end-pos [end-pos #f]
                      str . args)
  (define msg (format "~a: ~a" who (apply format str args)))
  (define srcloc (and in (port+config->srcloc in config
                                              #:end-pos end-pos)))
  (raise
   ((cond
     [(eof-object? due-to) exn:fail:read:eof]
     [(not (char? due-to)) exn:fail:read:non-char]
     [else exn:fail:read])
    (let ([s (and (error-print-source-location)
                  srcloc
                  (srcloc->string srcloc))])
      (if s
          (string-append s ": " msg)
          msg))
    continuation-marks
    (if srcloc
        (list srcloc)
        null))))

(define (bad-syntax-error in config str #:due-to [due-to #\x])
  (reader-error in config #:due-to due-to "bad syntax `~a`" str))


(define-syntax-rule (catch-and-reraise-as-reader in config expr)
  (catch-and-reraise-as-reader/proc in config (lambda () expr)))

(define (catch-and-reraise-as-reader/proc in config thunk)
  (with-handlers ([exn:fail? (lambda (exn)
                               (reader-error in config
                                             "~a"
                                             (let ([s (exn-message exn)])
                                               (regexp-replace "^[a-z-]*: " s ""))
                                             #:continuation-marks (exn-continuation-marks exn)))])
    (thunk)))
