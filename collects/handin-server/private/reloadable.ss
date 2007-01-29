(module reloadable mzscheme

  (require (lib "moddep.ss" "syntax") "logger.ss")

  (provide reload-module)
  (define (reload-module modspec path)
    ;; the path argument is not needed (could use resolve-module-path here),
    ;; but its always known when this function is called
    (let* ([name ((current-module-name-resolver) modspec #f #f)]
           [name (symbol->string name)]
           [name (if (eq? #\, (string-ref name 0))
                   (substring name 1)
                   (error 'reload-module
                          "unexpected module name for ~e: ~e" modspec name))]
           [prefix (let-values ([(base name dir?) (split-path name)])
                     (string->symbol (format ",~a" base)))])
      (log-line "(re)loading module from ~a" modspec)
      (parameterize ([current-module-name-prefix prefix]
                     [compile-enforce-module-constants #f])
        (load/use-compiled path))))

  ;; pulls out a value from a module, reloading the module if its source file
  ;; was modified
  (provide auto-reload-value)
  (define module-times (make-hash-table 'equal))
  (define (auto-reload-value modspec valname)
    (let* ([path (resolve-module-path modspec #f)]
           [last (hash-table-get module-times path #f)]
           [cur  (file-or-directory-modify-seconds path)])
      (unless (equal? cur last)
        (hash-table-put! module-times path cur)
        (reload-module modspec path))
      (dynamic-require modspec valname)))

  (define poll-freq 2000.0) ; poll at most once every two seconds

  ;; pulls out a procedure from a module, and returns a wrapped procedure that
  ;; automatically reloads the module if the file was changed whenever the
  ;; procedure is used
  (provide auto-reload-procedure)
  (define (auto-reload-procedure modspec procname)
    (let ([path (resolve-module-path modspec #f)] [date #f] [proc #f] [poll #f])
      (define (reload)
        (unless (and proc (< (- (current-inexact-milliseconds) poll) poll-freq))
          (set! poll (current-inexact-milliseconds))
          (let ([cur (file-or-directory-modify-seconds path)])
            (unless (equal? cur date)
              (set! date cur)
              (reload-module modspec path)
              (set! proc (dynamic-require modspec procname))))))
      (reload)
      (lambda xs (reload) (apply proc xs))))

  )
