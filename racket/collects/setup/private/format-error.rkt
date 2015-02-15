#lang racket/base
(require compiler/cm
         setup/path-to-relative)

(provide format-error)

(define (format-error exn
                      #:long? [long? #t]
                      #:to-string? [to-string? #f]
                      #:cache [pkg-path-cache #f])
  (let loop ([to-string? to-string?])
    (cond
     [to-string?
      (define sp (open-output-string))
      (parameterize ([current-error-port sp])
        (loop #f))
      (regexp-replace #rx"\n$" (get-output-string sp) "")]
     [long?
      ((make-compilation-context-error-display-handler
        (lambda (str exn)
          ((error-display-handler)
           str
           exn)))
       (exn-message exn)
       exn)]
     [else
      (eprintf "~a\n" (exn-message exn))
      (define path (continuation-mark-set-first (exn-continuation-marks exn)
                                                managed-compiled-context-key))
      (when (path-string? path)
        (eprintf "  compiling: ~a"
                 (path->relative-string/setup path #:cache pkg-path-cache)))])))
