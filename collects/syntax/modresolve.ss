
(module modresolve mzscheme
  (require (lib "list.ss")
           (lib "contract.ss")
           (lib "resolver.ss" "planet")
           "private/modhelp.ss")

  (define (force-relto relto dir?)
    (cond [(path-string? relto)
           (if dir?
             (let-values ([(base n d?) (split-path relto)])
               (if (eq? base 'relative)
                 (or (current-load-relative-directory) (current-directory))
                 base))
             relto)]
          [(pair? relto) relto]
          [(not dir?)
           (error 'resolve-module-path-index
                  "can't resolve \"self\" with non-path relative-to: ~e" relto)]
          [(procedure? relto) (relto)]
          [else (current-directory)]))

  (define (resolve-module-path s relto)
    ;; relto should be a complete path, #f, or procedure that returns a
    ;; complete path
    (define (get-dir) (force-relto relto #t))
    (cond [(string? s)
           ;; Parse Unix-style relative path string
           (apply build-path (get-dir) (explode-relpath-string s))]
          [(and (or (not (pair? s)) (not (list? s))) (not (path? s)))
           #f]
          [(or (path? s) (eq? (car s) 'file))
           (let ([p (if (path? s) s (cadr s))])
             (path->complete-path
              p (let ([d (get-dir)])
                  (if (path-string? d)
                    d
                    (or (current-load-relative-directory)
                        (current-directory))))))]
          [(eq? (car s) 'lib)
           (let* ([cols (let ([len (length s)])
                          (if (= len 2) (list "mzlib") (cddr s)))]
                  [p (apply collection-path cols)])
             (build-path p (cadr s)))]
          [(eq? (car s) 'planet)
           (let ([module-id
                  (and (path? relto)
                       (string->symbol
                        (string-append
                         "," (path->string (path->complete-path relto)))))])
             (let-values ([(path pkg)
                           (get-planet-module-path/pkg s module-id #f)])
               path))]
          [else #f]))

  (define (resolve-module-path-index mpi relto)
    ;; relto must be a complete path
    (let-values ([(path base) (module-path-index-split mpi)])
      (if path
        (resolve-module-path path (resolve-possible-module-path-index base relto))
        (force-relto relto #f))))

  (define (resolve-possible-module-path-index base relto)
    (cond [(module-path-index? base)
           (resolve-module-path-index base relto)]
          [(symbol? base)
           (let ([s (symbol->string base)])
             (if (and ((string-length s) . > . 0)
                      (char=? #\, (string-ref s 0)))
               (substring s 1 (string-length s))
               relto))]
          [relto (if (procedure? relto) (relto) relto)]
          [else #f]))

  (define rel-to-path-string/thunk/#f
    (or/c path-string? (-> path-string?) false/c))

  (provide/contract
   [resolve-module-path (module-path-v? rel-to-path-string/thunk/#f
                         . -> . path?)]
   [resolve-module-path-index ((or/c symbol? module-path-index?)
                               rel-to-path-string/thunk/#f
                               . -> . path?)]))
