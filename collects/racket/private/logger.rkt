
(module logger '#%kernel
  (#%require "small-scheme.rkt" "define.rkt"
             (for-syntax '#%kernel "stx.rkt" "small-scheme.rkt" "stxcase-scheme.rkt"))

  (#%provide log-fatal log-error log-warning log-info log-debug
             define-logger)

  (define-for-syntax (make-define-log mode X-logger-stx)
    (lambda (stx)
      (with-syntax ([X-logger X-logger-stx]
                    [mode mode])
        (syntax-case stx ()
          [(_ str-expr) 
           #'(let ([l X-logger])
               (when (log-level? l 'mode)
                 (log-message l 'mode str-expr (current-continuation-marks))))]
          [(_ str-expr arg ...)
           #'(let ([l X-logger])
               (when (log-level? l 'mode)
                 (log-message l 'mode (format str-expr arg ...) (current-continuation-marks))))]))))

  (define-syntax log-fatal (make-define-log 'fatal #'(current-logger)))
  (define-syntax log-error (make-define-log 'error #'(current-logger)))
  (define-syntax log-warning (make-define-log 'warning #'(current-logger)))
  (define-syntax log-info (make-define-log 'info #'(current-logger)))
  (define-syntax log-debug (make-define-log 'debug #'(current-logger)))

  (define (check-logger who)
    (lambda (v)
      (unless (logger? v)
        (raise-argument-error who "logger?" v))
      v))

  (define-syntax (define-logger stx)
    (syntax-case stx ()
      [(_ X)
       (let* ([X #'X]
              [mk (lambda (mode)
                    (datum->syntax X (string->symbol (format "log-~a-~a" (syntax-e X) mode)) X))])
         (unless (identifier? X)
           (raise-syntax-error #f "not an identifier" stx X))
         (with-syntax ([log-X-fatal (mk 'fatal)]
                       [log-X-error (mk 'error)]
                       [log-X-warning (mk 'warning)]
                       [log-X-info (mk 'info)]
                       [log-X-debug (mk 'debug)]
                       [X-logger 
                        (datum->syntax X (string->symbol (format "~a-logger" (syntax-e X))) X)]
                       [X X])
           #'(begin
               (define X-logger (make-logger 'X (current-logger)))
               (define-syntax log-X-fatal (make-define-log 'fatal #'X-logger))
               (define-syntax log-X-error (make-define-log 'error #'X-logger))
               (define-syntax log-X-warning (make-define-log 'warning #'X-logger))
               (define-syntax log-X-info (make-define-log 'info #'X-logger))
               (define-syntax log-X-debug (make-define-log 'debug #'X-logger)))))])))
