
(module logger '#%kernel
  (#%require "small-scheme.rkt" "define.rkt"
             (for-syntax '#%kernel "stx.rkt" "small-scheme.rkt" "stxcase-scheme.rkt"))

  (#%provide log-fatal log-error log-warning log-info log-debug
             define-logger)

  (define-for-syntax (make-define-log mode X-logger-stx name)
    (lambda (stx)
      (with-syntax ([X-logger X-logger-stx]
                    [mode mode]
                    [name name])
        (syntax-case stx ()
          [(_ str-expr) 
           #'(let ([l X-logger])
               (when (log-level? l 'mode name)
                 (log-message l 'mode str-expr (current-continuation-marks))))]
          [(_ str-expr arg ...)
           #'(let ([l X-logger])
               (when (log-level? l 'mode name)
                 (log-message l 'mode (format str-expr arg ...) (current-continuation-marks))))]))))

  (define-syntax log-fatal (make-define-log 'fatal #'(current-logger) #'(logger-name l)))
  (define-syntax log-error (make-define-log 'error #'(current-logger) #'(logger-name l)))
  (define-syntax log-warning (make-define-log 'warning #'(current-logger) #'(logger-name l)))
  (define-syntax log-info (make-define-log 'info #'(current-logger) #'(logger-name l)))
  (define-syntax log-debug (make-define-log 'debug #'(current-logger) #'(logger-name l)))

  (define (check-logger-or-false who v)
    (unless (or (not v) (logger? v))
      (raise-argument-error who "(or/c logger? #f)" v))
    v)

  (define-syntax (define-logger stx)
    (syntax-case stx ()
      [(d-l X)
       (syntax/loc stx
         (d-l X #:parent (current-logger)))]
      [(d-l X #:parent parent)
       (let* ([X #'X]
              [logger-local-introduced (syntax-local-introduce X)]
              [logger-name-size (string-length (symbol->string (syntax-e X)))]
              [mk-binder (lambda (id starting-point)
                           (vector (syntax-local-introduce id)
                                   starting-point logger-name-size 0.5 0.5
                                   logger-local-introduced
                                   0 logger-name-size 0.5 0.5))]
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
           (syntax-property
            #'(begin
                (define X-logger (make-logger 'X (check-logger-or-false 'd-l parent)))
                (define-syntax log-X-fatal (make-define-log 'fatal #'X-logger #''X))
                (define-syntax log-X-error (make-define-log 'error #'X-logger #''X))
                (define-syntax log-X-warning (make-define-log 'warning #'X-logger #''X))
                (define-syntax log-X-info (make-define-log 'info #'X-logger #''X))
                (define-syntax log-X-debug (make-define-log 'debug #'X-logger #''X)))
            'sub-range-binders
            (map
             mk-binder
             (list #'X-logger #'log-X-fatal #'log-X-error #'log-X-warning #'log-X-info #'log-X-debug)
             (list 0 4 4 4 4 4)))))])))
