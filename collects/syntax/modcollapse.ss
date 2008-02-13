
(module modcollapse mzscheme
  (require (lib "list.ss")
           (lib "contract.ss")
           "private/modhelp.ss")

  (define (collapse-module-path s relto-mp)
    ;; relto-mp should be a relative path, '(lib relative-path collection),
    ;;   or '(file path) or a thunk that produces one of those

    ;; Used for 'lib paths, so it's always Unix-style
    (define (attach-to-relative-path-string elements relto)
      (let ([elem-str
             (substring
              (apply string-append
                     (map (lambda (i)
                            (string-append
                             "/"
                             (cond [(bytes? i) (bytes->string/locale i)]
                                   [(path? i) (path->string i)]
                                   [(eq? i 'up) ".."]
                                   [else i])))
                          (filter (lambda (x) (not (eq? x 'same)))
                                  elements)))
              1)])
        (if (or (regexp-match #rx"^[.]/+[^/]*" relto)
                (not (regexp-match #rx"/" relto)))
          elem-str
          (let ([m (regexp-match #rx"^(.*/)/*[^/]*$" relto)])
            (string-append (cadr m) elem-str)))))

    (define (add-main s)
      (if (regexp-match #rx"[.][^/]*$" s)
          s
          (string-append s "/main.ss")))

    (define (combine-relative-elements elements)

      ;; Used for 'file paths, so it's platform specific:
      (define (attach-to-relative-path relto)
        (apply build-path
               (let-values ([(base n d?) (split-path relto)])
                 (if (eq? base 'relative) 'same base))
               (map (lambda (i) (if (bytes? i) (bytes->path i) i))
                    elements)))

      (when (procedure? relto-mp) (set! relto-mp (relto-mp)))
      (cond
        [(or (path? relto-mp) (and (string? relto-mp) (ormap path? elements)))
         (apply build-path
                (let-values ([(base name dir?) (split-path relto-mp)])
                  (if (eq? base 'relative) 'same base))
                (map (lambda (x) (if (bytes? x) (bytes->path x) x))
                     elements))]
        [(string? relto-mp)
         (bytes->string/locale
          (apply
           bytes-append
           (cond [(regexp-match #rx#"^(.*)/[^/]*$"
                                (string->bytes/locale relto-mp))
                  => cadr]
                 [else #"."])
           (map (lambda (e)
                  (cond [(eq? e 'same) #"/."]
                        [(eq? e 'up) #"/.."]
                        [else (bytes-append
                               #"/" (if (path? e) (path->bytes e) e))]))
                elements)))]
        [(eq? (car relto-mp) 'file)
         (let ([path ((if (ormap path? elements) values path->string)
                      (attach-to-relative-path (cadr relto-mp)))])
           (if (path? path) path `(file ,path)))]
        [(eq? (car relto-mp) 'lib)
         (let ([relto-mp (if (null? (cddr relto-mp))
                             ;; old style => add 'mzlib
                             ;; new style => add main.ss or split
                             (let ([m (regexp-match-positions #rx"[/]" (cadr relto-mp))])
                               (if m
                                   ;; new style: split
                                   `(lib ,(substring (cadr relto-mp) (cdar m))
                                         ,(substring (cadr relto-mp) 0 (caar m)))
                                   (if (regexp-match? #rx"[.]" (cadr relto-mp))
                                       ;; old style:
                                       `(lib ,(cadr relto-mp) "mzlib")
                                       ;; new style, add "main.ss":
                                       `(lib "main.ss" ,(cadr relto-mp)))))
                             ;; already has at least two parts:
                             relto-mp)])
           (let ([path (attach-to-relative-path-string
                        elements (apply string-append
                                        (append
                                         (map (lambda (s)
                                                (string-append s "/"))
                                              (cddr relto-mp))
                                         (list (cadr relto-mp)))))])
             (let ([simpler (let loop ([s (regexp-replace* #px"(?<![.])[.]/" path "")])
                              (let ([s2 (regexp-replace #rx"([^/.]*)/[.][.]/" s "")])
                                (if (equal? s s2)
                                    s
                                    (loop s2))))])
               (let ([m (regexp-match #rx"^(.*)/([^/]*)$" simpler)])
                 (if m
                     `(lib ,(caddr m) ,(cadr m))
                     (error 'combine-relative-elements
                            "relative path escapes collection: ~s relative to ~s"
                            elements relto-mp))))))]
         [(eq? (car relto-mp) 'planet)
         (let ([pathstr (attach-to-relative-path-string
                         elements (cadr relto-mp))])
           `(planet ,pathstr ,(caddr relto-mp)))]
        [else (error 'combine-relative-elements
                     "don't know how to deal with: ~s" relto-mp)]))

    (cond [(string? s)
           ;; Parse Unix-style relative path string
           (combine-relative-elements (explode-relpath-string s))]
          [(symbol? s)
           ;; Convert to `lib' form:
           `(lib ,(symbol->string s))]
          [(and (or (not (pair? s)) (not (list? s))) (not (path? s)))
           #f]
          [(or (path? s) (eq? (car s) 'file))
           (let ([p (if (path? s) s (cadr s))])
             (if (absolute-path? p)
               s
               (let loop ([p p] [elements null])
                 (let-values ([(base name dir?) (split-path p)])
                   (cond [(eq? base 'relative)
                          (combine-relative-elements (cons name elements))]
                         [else (loop base (cons name elements))])))))]
          [(eq? (car s) 'lib) s]
          [(eq? (car s) 'planet) s]
          [(eq? (car s) 'quote) s]
          [else #f]))

  (define (collapse-module-path-index mpi relto-mp)
    (let-values ([(path base) (module-path-index-split mpi)])
      (if path
          (collapse-module-path
           path
           (if base
               (collapse-module-path-index base relto-mp)
               relto-mp))
          relto-mp)))

  (define simple-rel-to-module-path-v/c
    (or/c (and/c module-path?
                 (or/c
                  (cons/c (symbols 'lib) any/c)
                  (cons/c (symbols 'file) any/c)
                  (cons/c (symbols 'planet) any/c)
                  (cons/c (symbols 'quote) any/c)))
          path?))

  (define rel-to-module-path-v/c
    (or/c simple-rel-to-module-path-v/c 
          path?
          (-> simple-rel-to-module-path-v/c)))

  (provide/contract
   [collapse-module-path ((or/c module-path? path?) rel-to-module-path-v/c
                          . -> . simple-rel-to-module-path-v/c)]
   [collapse-module-path-index ((or/c symbol? module-path-index?)
                                rel-to-module-path-v/c
                                . -> . simple-rel-to-module-path-v/c)]))
