
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
         (let ([path (attach-to-relative-path-string
                      elements (cadr relto-mp))])
           `(lib ,path ,(caddr relto-mp)))]
        [(eq? (car relto-mp) 'planet)
         (let ([pathstr (attach-to-relative-path-string
                         elements (cadr relto-mp))])
           `(planet ,pathstr ,(caddr relto-mp)))]
        [else (error 'combine-relative-elements
                     "don't know how to deal with: ~s" relto-mp)]))

    (cond [(string? s)
           ;; Parse Unix-style relative path string
           (combine-relative-elements (explode-relpath-string s))]
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
          [(eq? (car s) 'lib)
           (let ([cols (let ([len (length s)])
                         (if (= len 2) (list "mzlib") (cddr s)))])
             `(lib ,(attach-to-relative-path-string
                     (append (cdr cols) (list (cadr s)))
                     ".")
                   ,(car cols)))]
          [(eq? (car s) 'planet)
           (let ([cols (cdddr s)])
             `(planet
               ,(attach-to-relative-path-string
                 (append cols (list (cadr s)))
                 ".")
               ,(caddr s)))]
          [else #f]))

  (define (collapse-module-path-index mpi relto-mp)
    (let-values ([(path base) (module-path-index-split mpi)])
      (if path
        (collapse-module-path
         path
         (cond
           [(symbol? base)
            (let ([s (symbol->string base)])
              (if (and ((string-length s) . > . 0)
                       (char=? #\, (string-ref s 0)))
                `(file ,(substring s 1))
                relto-mp))]
           [(module-path-index? base)
            (collapse-module-path-index base relto-mp)]
           [else relto-mp]))
        relto-mp)))

  (define simple-rel-to-module-path-v/c
    (or/c
     (list/c (symbols 'lib) module-path-v-string? module-path-v-string?)
     (list/c (symbols 'file) (and/c string? path-string?))
     ;; not quite specific enough of a contract -- it should also spell out
     ;; what's allowed in the package spec
     (cons/c (symbols 'planet)
             (cons/c string? (cons/c (listof any/c) (listof string?))))
     path-string?))

  (define rel-to-module-path-v/c
    (or/c simple-rel-to-module-path-v/c (-> simple-rel-to-module-path-v/c)))

  (provide/contract
   [collapse-module-path (module-path-v? rel-to-module-path-v/c
                          . -> . simple-rel-to-module-path-v/c)]
   [collapse-module-path-index ((or/c symbol? module-path-index?)
                                rel-to-module-path-v/c
                                . -> . simple-rel-to-module-path-v/c)]))
