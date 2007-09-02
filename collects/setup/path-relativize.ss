(module path-relativize mzscheme

  (provide make-relativize)

  (define (make-relativize find-main-dir
                           tag
                           to-rel-name
                           from-rel-name)
                           
    ;; Historical note: this module is based on the old "plthome.ss"
    
    ;; The `path->main-collects-relative' and
    ;; `main-collects-relative->path' functions are used to store paths
    ;; that are relative to the main "collects" directory, such as in
    ;; .dep files.  This means that if the plt tree is moved, .dep files
    ;; still work.  It is generally fine if
    ;; `path->main-collects-relative' misses some usages, as long as it
    ;; works when we prepare a distribution tree.  Otherwise, things
    ;; will continue to work fine and .dep files will just contain
    ;; absolute path names.  These functions work on .dep elements:
    ;; either a pathname or a pair with a pathname in its cdr; the
    ;; `path->main-collects-relative' pathname will itself be a pair.
    
    ;; We need to compare paths to find when something is in the plt
    ;; tree -- this does some basic "normalization" that should work
    ;; fine: getting rid of `.' and `..', collapsing multiple
    ;; `/'s to one `/', and converting '/'s to '\'s under Windows.
    (define (simplify-path* bytes)
      (path->bytes (simplify-path (bytes->path bytes))))
    
    (define main-collects-dir/
      (delay (let ([dir (find-main-dir)])
               (and dir (simplify-path* (path->bytes (path->directory-path dir)))))))

    (define (maybe-cdr-op fname f)
      (lambda (x)
        (cond [(and (pair? x) (not (eq? tag (car x))))
               (cons (car x) (f (cdr x)))]
              [else (f x)])))

    ;; path->main-collects-relative* : path-or-bytes -> datum-containing-bytes-or-path
    (define (path->main-collects-relative* path)
      (let* ([path (cond [(bytes? path) path]
                         [(path?  path) (path->bytes path)]
                         [else (error 'path->main-collects-relative
                                      "expecting a byte-string, got ~e" path)])]
             [path* (simplify-path* path)]
             [main-collects-dir/ (force main-collects-dir/)]
             [mcd-len (bytes-length main-collects-dir/)])
        (cond [(and path*
                    mcd-len
                    (> (bytes-length path*) mcd-len)
                    (equal? (subbytes path* 0 mcd-len)
                            main-collects-dir/))
               (cons tag (subbytes path* mcd-len))]
              [(equal? path* main-collects-dir/) (cons tag #"")]
              [else path])))

    ;; main-collects-relative->path* : datum-containing-bytes-or-path -> path
    (define (main-collects-relative->path* path)
      (cond [(and (pair? path)
                  (eq? tag (car path))
                  (bytes? (cdr path)))
             (let ([dir (or (find-main-dir)
                            ;; No main "collects"/"doc"/whatever? Use original working directory:
                            (find-system-path 'orig-dir))])
               (if (equal? (cdr path) #"")
                   dir
                   (build-path dir (bytes->path (cdr path)))))]
            [(bytes? path) (bytes->path path)]
            [else path]))

    (values
      (maybe-cdr-op to-rel-name path->main-collects-relative*)
      (maybe-cdr-op from-rel-name main-collects-relative->path*))))
