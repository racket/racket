(module load '#%kernel
  (#%require "qq-and-or.rkt"
             "more-scheme.rkt"
             "define-et-al.rkt"
             "executable-path.rkt")

  (#%provide load/use-compiled
             embedded-load)

  (define-values (load/use-compiled)
    (lambda (f) ((current-load/use-compiled) f #f)))

  ;; used for the -k command-line argument:
  (define-values (embedded-load)
    (lambda (start end str)
      (let* ([s (if str
                    str
                    (let* ([sp (find-system-path 'exec-file)] 
                           [exe (find-executable-path sp #f)]
                           [start (or (string->number start) 0)]
                           [end (or (string->number end) 0)])
                      (with-input-from-file exe 
                        (lambda ()
                          (file-position (current-input-port) start)
                          (read-bytes (max 0 (- end start)))))))]
             [p (open-input-bytes s)])
        (let loop ()
          (let ([e (parameterize ([read-accept-compiled #t]
                                  [read-accept-reader #t]
                                  [read-accept-lang #t]
                                  [read-on-demand-source #t])
                     (read p))])
            (unless (eof-object? e)
              ((current-eval) e)
              (loop))))))))
