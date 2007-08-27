(module installer mzscheme
  (provide installer)

  (require (lib "match.ss")
           (lib "file.ss")
           (lib "list.ss")
           (lib "dirs.ss" "setup")
           "servlets/home.ss")

  (define (installer path)
    (create-index-file))

  (define index-file "hdindex")

  ;; assume that "help" is in the main doc directory
  (define dest-dir (build-path (find-doc-dir) "help"))

  (define (create-index-file)
    (gen-index servlet-dir)
    (set! index (append index (generate-index-for-static-pages)))
    (with-output-to-file (build-path dest-dir index-file)
      (lambda ()
        (printf "(\n")
        (for-each (lambda (x) (printf "~s\n" x)) index)
        (printf ")\n"))
      'truncate))

  (define servlet-dir (normalize-path
                       (build-path (collection-path "help") "servlets")))
  (define exploded-servlet-dir-len (length (explode-path servlet-dir)))

  (unless (directory-exists? dest-dir)
    (make-directory* dest-dir))
  (current-directory dest-dir)

  (define (get-servlet-files dir)
    (let* ([all-files
            (map (lambda (f) (build-path dir f))
                 (directory-list dir))]
           [servlet-files
            (filter (lambda (s)
                      (regexp-match #rx#"[.]ss$" (path->bytes s)))
                    all-files)]
           [dirs
            (filter directory-exists? all-files)])
      (apply append servlet-files
             (map get-servlet-files dirs))))

  ; path is absolute, and has the servlet dir as a prefix
  (define (relativize-and-slashify path)
    (let* ([exp-path (explode-path path)]
           [prefix-len (sub1 exploded-servlet-dir-len)]
           [relative-exp-path
            (let loop ([p exp-path] [n 0])
              ; leave off prefix up to servlet dir
              (if (>= n prefix-len)
                p
                (loop (cdr p) (add1 n))))])
      (fold-into-web-path relative-exp-path)))

  ; (listof string) -> string
  ; result is forward-slashed web path
  ;  e.g. ("foo" "bar") -> "foo/bar"
  (define (fold-into-web-path lst)
    (apply string-append
           (cdr (apply append (map (lambda (x) (list "/" (path->string x)))
                                   lst)))))

  (define index '())

  (define (add-index-entry! val file name title)
    (set! index
          (cons (list val
                      (string-append "/" (relativize-and-slashify file))
                      name
                      title)
                index)))

  (define (gen-index dir)
    (let* ([all-files (directory-list)]
           [servlet-files (get-servlet-files dir)])
      (for-each
       (lambda (file)
         (let ([port (open-input-file file)]
               [title-value #f])
           (let loop ()
             (let ([sexp (with-handlers ([exn:fail:read?
                                          (lambda (x)
                                            (fprintf (current-error-port)
                                                     "couldn't read ~a: ~a\n"
                                                     file
                                                     (if (exn? x)
                                                         (exn-message x)
                                                         (format "~s" x)))
                                            #f)])
                           (read port))])
               (unless (eof-object? sexp)
                 (let loop ([exp sexp])
                   (match exp
                     [`(title ,(? string? title))
                      (set! title-value title)]
                     [`(a ((name ,(? string? name)) (value ,(? string? value))))
                      (add-index-entry! value file name
                                        (or title-value (path->string file)))]
                     [_ (when (pair? exp)
                          (begin (loop (car exp))
                                 (loop (cdr exp))))]))
                 (loop))))))
       servlet-files))))

