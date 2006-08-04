(module run-scribble mzscheme

  (require (lib "cmdline.ss"))

  (define exe-name 'scribble) ; for errors

  (define (error* msg . args)
    (apply raise-user-error exe-name msg args))

  (define formats
    `([sexpr ,(lambda (v)
                ((dynamic-require '(lib "pretty.ss") 'pretty-print) v))]))
  (define default-format 'sexpr)

  (define (format->renderer format)
    (cond [(assq format formats)
           => (lambda (f)
                (let ([f (cadr f)])
                  (cond [(procedure? f) f]
                        [else (error 'format->renderer
                                     "internal error: ~s" f)])))]
          [else (error* "unknown format ~e (use -L for a list of formats)"
                        format)]))

  (provide render-file)
  (define (render-file input output format)
    (unless (file-exists? input)
      (error* "cannot find input file: ~e" input))
    (let* ([contents (dynamic-require `(file ,input) 'contents)]
           [renderer (format->renderer format)]
           [render   (lambda () (renderer contents))])
      (if output (with-output-to-file output render 'truncate) (render))))

  (provide main)
  (define (main args)
    (define *output-name #f)
    (define *format #f)
    (command-line (car args) (cdr args)
      [once-each
       [("-o" "--output") output-name "output name (sometimes a directory)"
        (set! *output-name output-name)]
       [("-f" "--format") format "output format (implies suffix)"
                                 "(use -L to list available formats)"
        (set! *format (string->symbol format))]
       [("-L" "--list-formats") "show available output-formats"
        (printf "Available formats:\n")
        (for-each (lambda (f) (printf "  ~a\n" (car f))) formats)
        (printf "The default is ~a\n" default-format)
        (exit)]]
      [args (input-file)
        (let* ([fmt (cond [*format *format]
                          [(and *output-name
                                (regexp-match #rx"[.]([^.]+)$" *output-name))
                           => (lambda (m) (string->symbol (cadr m)))]
                          [else default-format])]
               [output (or *output-name (path-replace-suffix
                                         input-file (symbol->string fmt)))]
               [output (and (not (equal? "-" output)) output)])
          (render-file input-file output fmt))]))

  (main (cons (symbol->string exe-name)
              (vector->list (current-command-line-arguments))))

)
