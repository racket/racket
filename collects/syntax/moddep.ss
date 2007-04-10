
(module moddep mzscheme
  (require (lib "port.ss")
           (lib "list.ss")
           (lib "string.ss")
           (lib "contract.ss")
           (lib "kw.ss")
           (lib "resolver.ss" "planet"))

  (provide moddep-current-open-input-file)
  (define moddep-current-open-input-file
    (make-parameter open-input-file))

  (provide with-module-reading-parameterization)
  (define (with-module-reading-parameterization thunk)
    (parameterize ([read-case-sensitive #t]
                   [read-square-bracket-as-paren #t]
                   [read-curly-brace-as-paren #t]
                   [read-accept-box #t]
                   [read-accept-compiled #t]
                   [read-accept-bar-quote #t]
                   [read-accept-graph #t]
                   [read-decimal-as-inexact #t]
                   [read-accept-dot #t]
                   [read-accept-quasiquote #t]
                   [read-accept-reader #t]
                   [current-readtable #f])
      (thunk)))

  (define (raise-wrong-module-name filename expected-name name)
    (error 'load-handler
           "expected a `module' declaration for `~a' in ~s, found: ~a"
           expected-name filename name))

  (define (check-module-form exp expected-module filename)
    (cond [(or (eof-object? exp) (eof-object? (syntax-e exp)))
           (and filename
                (error 'load-handler
                       "expected a `module' declaration for `~a' in ~s, but found end-of-file"
                       expected-module filename))]
          [(compiled-module-expression? (syntax-e exp))
           (if (eq? (module-compiled-name (syntax-e exp)) expected-module)
             ;; It's fine:
             exp
             ;; Wrong name:
             (and filename (raise-wrong-module-name
                            filename expected-module
                            (module-compiled-name (syntax-e exp)))))]
          [(and (syntax? exp)
                (syntax-case exp ()
                  [(mod nm . _)
                   (and (eq? (syntax-e #'mod) 'module) (identifier? #'nm))]
                  [_else #f]))
           ;; It's ok; need to install a specific `module' binding:
           (with-syntax ([(mod nm . _) exp])
             (unless (eq? (syntax-e #'nm) expected-module)
               (raise-wrong-module-name filename expected-module
                                        (syntax-e #'nm)))
             (datum->syntax-object exp
                                   (cons #'module (cdr (syntax-e exp)))
                                   exp
                                   exp))]
          [else
           (and filename
                (error 'load-handler
                       "expected a `module' declaration for `~a' in ~s, but found something else"
                       expected-module filename))]))

  (define (resolve s)
    (if (complete-path? s)
      s
      (let ([d (current-load-relative-directory)])
        (if d (path->complete-path s d) s))))

  (define (date>=? a bm)
    (and a (let ([am (with-handlers ([exn:fail:filesystem? (lambda (x) #f)])
                       (file-or-directory-modify-seconds a))])
             (or (and (not bm) am) (and am bm (>= am bm))))))

  (define (read-one path src?)
    (let ([p ((moddep-current-open-input-file) path)])
      (when src? (port-count-lines! p) (strip-shell-command-start p))
      (dynamic-wind
        void
        (lambda ()
          (let ([v (with-module-reading-parameterization
                    (lambda () (read-syntax path p)))])
            (when (eof-object? v)
              (error 'read-one
                     "empty file; expected a module declaration in: ~a" path))
            (let* ([name (let-values ([(base name dir?) (split-path path)])
                           (string->symbol
                            (bytes->string/utf-8
                             (path->bytes (path-replace-suffix name #""))
                             #\?)))]
                   [v (check-module-form v name path)])
              (unless (eof-object? (read p))
                (error 'read-one
                       "file has more than one expression; expected a module declaration only in: ~a"
                       path))
              (if (and (syntax? v) (compiled-expression? (syntax-e v)))
                (syntax-e v)
                v))))
        (lambda () (close-input-port p)))))

  (define-struct (exn:get-module-code exn) (path))

  (define/kw (get-module-code path
               #:optional
               [sub-path "compiled"] [compiler compile] [extension-handler #f])
    (unless (path-string? path)
      (raise-type-error 'get-module-code "path or string (sans nul)" path))
    (let*-values ([(path) (resolve path)]
                  [(base file dir?) (split-path path)]
                  [(base) (if (eq? base 'relative) 'same base)]
                  [(mode) (use-compiled-file-paths)])
      (let* ([get-so (lambda (file)
                       (build-path
                        base sub-path "native"
                        (system-library-subpath)
                        (path-replace-suffix file (system-type 'so-suffix))))]
             [zo (build-path base sub-path (path-replace-suffix file #".zo"))]
             [so (get-so file)]
             [_loader-so (get-so (string->path "_loader.ss"))]
             [path-d (with-handlers ([exn:fail:filesystem? (lambda (x) #f)])
                       (file-or-directory-modify-seconds path))]
             [with-dir (lambda (t)
                         (parameterize ([current-load-relative-directory
                                         (if (path? base)
                                           base
                                           (current-directory))])
                           (t)))])
        (cond
          ;; Use .zo, if it's new enough
          [(date>=? zo path-d) (read-one zo #f)]
          ;; Otherwise, use source if it exists
          [path-d (with-dir (lambda () (compiler (read-one path #t))))]
          ;; No source --- maybe there's an .so?
          [(and (not path-d) (date>=? so path-d))
           (if extension-handler
             (extension-handler so #f)
             (raise (make-exn:get-module-code
                     (format "get-module-code: cannot use extension file; ~e" so)
                     (current-continuation-marks)
                     so)))]
          ;; Or maybe even a _loader.so?
          [(and (not path-d)
                (date>=? _loader-so path-d)
                (let ([getter (load-extension _loader-so)])
                  (let-values ([(loader modname)
                                (getter (string->symbol
                                         (bytes->string/latin-1
                                          (path->bytes
                                           (path-replace-suffix file #"")))))])
                    loader)))
           => (lambda (loader)
                (if extension-handler
                  (extension-handler loader #t)
                  (raise (make-exn:get-module-code
                          (format "get-module-code: cannot use _loader file: ~e"
                                  _loader-so)
                          (current-continuation-marks)
                          loader))))]
          ;; Report a not-there error
          [else (raise (make-exn:get-module-code
                        (format "get-module-code: no such file: ~e" path)
                        (current-continuation-marks)
                        #f))]))))

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

  (define (explode-relpath-string p)
    (map (lambda (p)
           (cond [(assoc p '((#"." . same) (#".." . up))) => cdr]
                 [else (bytes->path p)]))
         (regexp-split #rx#"/+" (string->bytes/utf-8 p))))

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

  (define (show-import-tree module-path)
    (let loop ([path (resolve-module-path module-path #f)][indent ""][fs ""])
      (printf "~a~a~a\n" indent path fs)
      (let ([code (get-module-code path)])
        (let-values ([(imports fs-imports ft-imports)
                      (module-compiled-imports code)])
          (define ((mk-loop fs) i)
            (unless (symbol? i)
              (loop (resolve-module-path-index i path)
                    (format " ~a" indent)
                    fs)))
          (for-each (mk-loop "") imports)
          (for-each (mk-loop " [for-syntax]") fs-imports)
          (for-each (mk-loop " [for-template]") ft-imports)))))

  (define (module-path-v-string? v)
    (and (regexp-match? #rx"^[-a-zA-Z0-9./]+$" v)
         (not (regexp-match? #rx"^/" v))
         (not (regexp-match? #rx"/$" v))))

  (define (module-path-v? v)
    (cond [(path? v) #t]
          [(string? v) (module-path-v-string? v)]
          [(pair? v)
           (case (car v)
             [(file) (and (pair? (cdr v))
                          (path-string? (cadr v))
                          (null? (cddr v)))]
             [(lib) (and (pair? (cdr v))
                         (list? (cdr v))
                         (andmap module-path-v-string? (cdr v)))]
             [(planet) #t]
             [else #f])]
          [else #f]))

  (define rel-to-path-string/thunk/#f
    (or/c path-string? (-> path-string?) false/c))

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
   [check-module-form ((or/c syntax? eof-object?) symbol? (or/c string? path? false/c) . -> . any)]

   [get-module-code ([path-string?]
                     [(and/c path-string? relative-path?)
                      (any/c . -> . any)
                      (or/c false/c (path? boolean? . -> . any))]
                     . opt-> .
                     any)])

  (provide exn:get-module-code
           exn:get-module-code?
           exn:get-module-code-path
           make-exn:get-module-code)

  (provide/contract
   [resolve-module-path (module-path-v? rel-to-path-string/thunk/#f
                         . -> . path?)]
   [resolve-module-path-index ((or/c symbol? module-path-index?)
                               rel-to-path-string/thunk/#f
                               . -> . path?)]

   [collapse-module-path (module-path-v? rel-to-module-path-v/c
                          . -> . simple-rel-to-module-path-v/c)]
   [collapse-module-path-index ((or/c symbol? module-path-index?)
                                rel-to-module-path-v/c
                                . -> . simple-rel-to-module-path-v/c)])

  (provide show-import-tree))
