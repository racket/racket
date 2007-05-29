
(module modcode mzscheme
  (require (lib "port.ss")
           (lib "kw.ss")
           (lib "contract.ss")
           "modread.ss")

  (provide moddep-current-open-input-file
           exn:get-module-code
           exn:get-module-code?
           exn:get-module-code-path
           make-exn:get-module-code
           get-module-code)
  #;
  ;; Contracts don't yet play well with keyword arguments:
  (provide/contract
   [get-module-code ([path-string?]
                     [(and/c path-string? relative-path?)
                      (any/c . -> . any)
                      (or/c false/c (path? boolean? . -> . any))
		      any/c]
                     . opt-> .
                     any)])



  (define moddep-current-open-input-file
    (make-parameter open-input-file))

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
      (when src? (port-count-lines! p))
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
               [sub-path "compiled"] [compiler compile] [extension-handler #f] 
               #:key
               [choose (lambda (src zo so) #f)])
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
                           (t)))]
             [prefer (choose path zo so)])
        (cond
          ;; Use .zo, if it's new enough
          [(or (eq? prefer 'zo)
               (and (not prefer)
                    (date>=? zo path-d)))
           (read-one zo #f)]
          ;; Maybe there's an .so? Use it only if we don't prefer source.
          [(or (eq? prefer 'so)
               (and (not prefer)
                    (or (not path-d)
                        (date>=? so path-d))))
           (if extension-handler
             (extension-handler so #f)
             (raise (make-exn:get-module-code
                     (format "get-module-code: cannot use extension file; ~e" so)
                     (current-continuation-marks)
                     so)))]
          ;; Use source if it exists
          [(or (eq? prefer 'src)
               path-d)
           (with-dir (lambda () (compiler (read-one path #t))))]
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
                        #f))])))))
